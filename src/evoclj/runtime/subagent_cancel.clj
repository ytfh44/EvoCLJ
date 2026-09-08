(ns evoclj.runtime.subagent-cancel
  "Subagent cancellation, cascade revocation, and structured-concurrency enforcement (S4).

  Decoupled from runtime/subagent to break the scheduler <-> subagent circular
  dependency: scheduler statically requires this namespace for terminal-step
  child cleanup, while subagent statically requires scheduler for session runs."
  (:require [evoclj.capability.mint :as mint]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.event :as event]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.work :as work-store])
  (:import (java.util UUID)))

(defn- db-spec [db]
  (cond
    (string? db) db
    (map? db) (or (:db db) db)
    (instance? evoclj.store.session_store.SessionStore db)
    (.-db ^evoclj.store.session_store.SessionStore db)
    :else (try
            (.-db ^Object db)
            (catch Exception _ db))))

(declare work-subtree-session-ids)

(defn get-parent-session-id
  "Return the parent session id (UUID) for `child-session-id`, or nil.
  Uses Work graph (works.parent_work_id) - the single durable spawn truth."
  [db child-session-id]
  (let [spec (db-spec db)
        cid (types/session-id child-session-id)
        sid (str cid)]
    (when-let [row (first (sqlite/query spec
                                        ["SELECT w2.session_id AS parent_session_id
                                          FROM works w1 JOIN works w2 ON w1.parent_work_id = w2.id
                                          WHERE w1.session_id = ?
                                            AND w1.parent_work_id IS NOT NULL
                                          ORDER BY w1.created_at, w1.id
                                          LIMIT 1"
                                         sid]))]
      (types/session-id (:parent_session_id row)))))

(defn child-session-ids
  "All child session ids spawned from `parent-session-id` via Work graph."
  [db parent-session-id]
  (let [spec (db-spec db)
        pid (str (types/session-id parent-session-id))
        rows (try
               (sqlite/query spec
                             ["SELECT w1.session_id AS child_session_id
                               FROM works w1 JOIN works w2 ON w1.parent_work_id = w2.id
                               WHERE w2.session_id = ?
                               ORDER BY w1.created_at, w1.id"
                              pid])
               (catch Exception _ []))]
    (->> rows
         (map :child_session_id)
         (map types/session-id)
         distinct
         vec)))

(defn list-descendants
  "Return all descendant session ids (UUIDs) transitively spawned from
  `root-id` via Work graph (works.parent_work_id)."
  [db root-id]
  (let [root-id (types/session-id root-id)
        spec (db-spec db)
        works (try (work-store/list-works spec root-id) (catch Exception _ []))
        descendant-work-ids (try
                              (mapcat #(work-store/work-descendants spec (:work/id %)) works)
                              (catch Exception _ []))]
    (->> descendant-work-ids
         (map #(try (work-store/fetch-work spec %) (catch Exception _ nil)))
         (keep :work/session-id)
         (remove #(= root-id %))
         distinct
         vec)))

(defn- work-subtree-session-ids
  "Session ids owning `work-id` and all its transitive Work descendants."
  [db work-id]
  (let [spec (db-spec db)
        ids (into [work-id] (try (work-store/work-descendants spec work-id)
                                 (catch Exception _ [])))]
    (into []
          (comp (map (fn [wid] (try (work-store/fetch-work spec wid) (catch Exception _ nil))))
                (filter some?)
                (map :work/session-id)
                (filter some?)
                (distinct))
          ids)))

(defn- cancel-targets
  "Transitive session cancel targets for `child-id`: `child-id` itself,
  followed by every session reached by Work graph descendants."
  [db child-id]
  (let [spec (db-spec db)
        works (try (work-store/list-works spec child-id) (catch Exception _ []))
        work-kids (mapcat #(try (work-subtree-session-ids spec (:work/id %))
                                (catch Exception _ []))
                          works)]
    (vec (distinct (concat [child-id] work-kids)))))

(defn- resolve-cancel-session!
  "Accept a session id or a first-class Work id; Work ids resolve to
  their owning session (the cancel path is Work-addressable)."
  [db id]
  (let [uuid (try (types/session-id id) (catch Exception _ id))
        w (try (work-store/fetch-work (db-spec db) uuid) (catch Exception _ nil))]
    (if w (:work/session-id w) uuid)))

(defn- target-cap-ids
  "Revocable capability row ids (strings) plus in-memory leases for `session-id`."
  [db session-id]
  (let [spec (db-spec db)
        sid (str (types/session-id session-id))
        all-db-rows (try
                      (sqlite/query spec
                                    ["SELECT id, revoked, subject_session_id, principal_id FROM capabilities"])
                      (catch Exception _ []))
        revocable-rows (filter (fn [r]
                                 (and (or (zero? (long (or (:revoked r) 0)))
                                          (false? (:revoked r)))
                                      (or (= (str (:subject_session_id r)) sid)
                                          (= (str (:principal_id r)) sid))))
                               all-db-rows)
        all-db-ids (mapv :id revocable-rows)
        all-leases (try
                     (let [reg @(requiring-resolve 'evoclj.runtime.subagent/subagent-lease-registry)]
                       (get @reg (types/session-id session-id) []))
                     (catch Exception _ []))]
    {:all-db-ids all-db-ids
     :leases (vec all-leases)}))

(defn- tombstone-memory-leases!
  "Apply the in-memory half of durable-first revocation AFTER the DB transaction commits."
  [cap-ids leases]
  (try
    (let [reg @(requiring-resolve 'evoclj.runtime.subagent/subagent-lease-registry)]
      (doseq [id cap-ids]
        (let [cap-id (try (UUID/fromString (str id)) (catch Exception _ id))]
          (mint/revoke-lease! reg cap-id)))
      (doseq [l leases]
        (when-let [cap-id (:cap/id l)]
          (mint/revoke-lease! reg cap-id))))
    (catch Exception _ nil)))

(defn- cancel-subtree-tx!
  "Atomically cancel `targets` in ONE BEGIN IMMEDIATE transaction on a single connection."
  [db direct-id parent-id targets reason]
  (let [spec (db-spec db)
        link-parent (into {} (map (fn [tid] [tid (try (get-parent-session-id db tid)
                                                     (catch Exception _ nil))])
                                  targets))
        parent-set (vec (distinct (filter some? (cons parent-id (vals link-parent)))))
        sessions (into {} (map (fn [tid] [tid (session/get-session db tid)])
                               (distinct (concat targets parent-set))))
        caps (mapv #(target-cap-ids db %) targets)
        cap-ids (vec (distinct (mapcat :all-db-ids caps)))
        leases (vec (distinct (mapcat :leases caps)))
        work-ids (vec (mapcat (fn [tid]
                                (try (mapv :work/id (work-store/list-works spec tid))
                                     (catch Exception _ [])))
                              targets))]
    (sqlite/with-write-tx [conn spec]
      ;; 1. durable revoke first: every capability row, WHERE revoked = 0
      (let [now (str (java.time.Instant/now))]
        (doseq [id cap-ids]
          (sqlite/insert-raw! conn "UPDATE capabilities SET revoked = 1, revoked_at = ? WHERE id = ? AND revoked = 0"
                               [now (str id)])))
      ;; 2. CAS every target Work to :cancelled (non-terminal only)
      (let [now (str (java.time.Instant/now))]
        (doseq [wid work-ids]
          (sqlite/insert-raw! conn "UPDATE works SET state = 'cancelled', updated_at = ? WHERE id = ? AND state IN ('queued','running','waiting')"
                               [now (str wid)])))
      ;; 3. cancel events: :session/cancelled per target + :subagent/cancelled
      ;;    on the immediate parent chain, sequenced inside the same tx
      (doseq [tid targets]
        (let [sess (get sessions tid)
              last-id (:id (last (sqlite/query-raw! conn "SELECT id FROM events WHERE session_id = ? ORDER BY event_seq"
                                                    [(str tid)])))
              edge-reason (if (= tid direct-id) reason :parent-cancel)
              edge-parent (if (= tid direct-id) parent-id (get link-parent tid))]
          (when (and sess last-id)
            (let [req {:session/id tid
                       :generation/id (:generation/id sess)
                       :phenotype/id (:phenotype/id sess)
                       :event/type :session/cancelled
                       :prev/event-id last-id
                       :payload-ref nil
                       :metadata {:reason edge-reason}}]
              (event/append-event-on-conn! conn req)))
          (when (and edge-parent (get sessions edge-parent))
            (let [psess (get sessions edge-parent)
                  plast-id (:id (last (sqlite/query-raw! conn "SELECT id FROM events WHERE session_id = ? ORDER BY event_seq"
                                                         [(str edge-parent)])))]
              (when (and psess plast-id)
                (let [req {:session/id edge-parent
                           :generation/id (:generation/id psess)
                           :phenotype/id (:phenotype/id psess)
                           :event/type :subagent/cancelled
                           :prev/event-id plast-id
                           :payload-ref nil
                           :metadata {:child/session-id tid
                                      :reason edge-reason}}]
                  (event/append-event-on-conn! conn req))))))))
    {:cap-ids cap-ids :leases leases}))

(defn- session-work-state
  "The session's sole execution Work state, or nil when it has no Work yet
  (W2: Work is the sole durable lifecycle - a Session carries no runtime
  state of its own)."
  [db session-id]
  (some-> (last (work-store/list-works (db-spec db) session-id)) :work/state))

(defn cancel-subagent!
  "Cancel a single child subagent session `child-session-id` spawned from
  `parent-session-id`. Cascade: also cancels all transitive descendants."
  [db parent-session-id child-session-id reason]
  (when (nil? db)
    (throw (err/error "cancel-subagent! requires a db/store handle"
                      {:error/type :store/session-invalid})))
  (let [child-id (resolve-cancel-session! db child-session-id)
        parent-id (when parent-session-id
                    (resolve-cancel-session! db parent-session-id))
        child (session/get-session db child-id)]
    (when-not child
      (throw (err/error (str "child session not found: " child-id)
                        {:error/type :subagent/not-found
                         :session/id child-id})))
    (when (and parent-session-id parent-id (not (session/get-session db parent-id)))
      (throw (err/error (str "parent session not found: " parent-id)
                        {:error/type :store/session-not-found
                         :session/id parent-id})))
    (if (= :cancelled (session-work-state db child-id))
      {:cancelled [] :already-cancelled? true :child/session-id child-id}
      (let [targets (cancel-targets db child-id)
            {:keys [cap-ids leases]} (cancel-subtree-tx! db child-id parent-id targets
                                                         (or reason :user-request))]
        (tombstone-memory-leases! cap-ids leases)
        {:cancelled targets :already-cancelled? false :child/session-id child-id}))))

(defn cancel-subagent-tree!
  "Cascade-cancel the entire subtree rooted at `root-session-id`."
  [db root-session-id reason]
  (when (nil? db)
    (throw (err/error "cancel-subagent-tree! requires a db/store handle"
                      {:error/type :store/session-invalid})))
  (let [root-id (resolve-cancel-session! db root-session-id)
        root (session/get-session db root-id)]
    (when-not root
      (throw (err/error (str "root session not found: " root-id)
                        {:error/type :subagent/not-found
                         :session/id root-id})))
    (if (= :cancelled (session-work-state db root-id))
      {:cancelled [] :already-cancelled? true :root/session-id root-id}
      (let [targets (cancel-targets db root-id)
            parent-id (get-parent-session-id db root-id)
            {:keys [cap-ids leases]} (cancel-subtree-tx! db root-id parent-id targets
                                                         (or reason :user-request))]
        (tombstone-memory-leases! cap-ids leases)
        {:cancelled targets :already-cancelled? false :root/session-id root-id}))))

(defn cancel-non-terminal-children!
  "Structured-concurrency enforcement: cancel every live child of
  `session-id` so children never outlive their parent's terminal step.
  Direct children are selected only from Work.parent_work_id; cancellation
  itself cascades through the same Work graph."
  ([db session-id]
   (cancel-non-terminal-children! db session-id :parent-cancel cancel-subagent!))
  ([db session-id reason]
   (cancel-non-terminal-children! db session-id reason cancel-subagent!))
  ([db session-id reason cancel-fn]
   (when db
     (let [sid (types/session-id session-id)
           cancel-fn (or cancel-fn cancel-subagent!)
           direct-children (child-session-ids db sid)
           live? (fn [cid]
                   (try
                     (boolean
                      (some #(contains? #{:queued :running :waiting} (:work/state %))
                            (mapcat #(work-store/list-works (db-spec db) %)
                                    (cons cid (list-descendants db cid)))))
                     (catch Exception _ false)))]
       {:cancelled
        (vec
         (distinct
          (reduce (fn [acc cid]
                    (try
                      (if (live? cid)
                        (into acc (:cancelled
                                   (cancel-fn db sid cid (or reason :parent-cancel))))
                        acc)
                      (catch Exception _ acc)))
                  []
                  direct-children)))}))))
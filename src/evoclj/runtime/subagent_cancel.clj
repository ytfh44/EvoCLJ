(ns evoclj.runtime.subagent-cancel
  "Subagent cancellation, cascade revocation, and structured-concurrency enforcement (S4).

  Decoupled from runtime/subagent to break the scheduler <-> subagent circular
  dependency: scheduler statically requires this namespace for terminal-step
  child cleanup, while subagent statically requires scheduler for session runs."
  (:require [clojure.java.jdbc :as jdbc]
            [evoclj.capability.mint :as mint]
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

(defn get-parent-session-id
  "Return the parent session id (UUID) for `child-session-id`, or nil.
  `db` is a sqlite spec or SessionStore handle."
  [db child-session-id]
  (let [spec (db-spec db)
        sid (str (types/session-id child-session-id))]
    (or (when-let [row (first (sqlite/query spec
                                            ["SELECT session_id FROM events
                                              WHERE event_type = 'subagent/spawned'
                                                AND payload LIKE ?
                                              LIMIT 1"
                                             (str "%" sid "%")]))]
          (types/session-id (:session_id row)))
        (when-let [row (first (sqlite/query spec
                                            ["SELECT w2.session_id AS parent_session_id
                                              FROM works w1 JOIN works w2 ON w1.parent_work_id = w2.id
                                              WHERE w1.session_id = ?
                                              LIMIT 1"
                                             sid]))]
          (types/session-id (:parent_session_id row)))
        (when-let [row (first (try (sqlite/query spec
                                                 ["SELECT parent_session_id FROM subagent_links WHERE child_session_id = ?"
                                                  sid])
                                   (catch Exception _ nil)))]
          (types/session-id (:parent_session_id row))))))

(defn child-session-ids
  "All child session ids spawned from `parent-session-id`."
  [db parent-session-id]
  (let [spec (db-spec db)
        pid (str (types/session-id parent-session-id))
        work-kids (try
                    (mapv #(types/session-id (:child_session_id %))
                          (sqlite/query spec
                                        ["SELECT w1.session_id AS child_session_id
                                          FROM works w1 JOIN works w2 ON w1.parent_work_id = w2.id
                                          WHERE w2.session_id = ?
                                          ORDER BY w1.created_at" pid]))
                    (catch Exception _ []))
        event-kids (try
                     (let [rows (sqlite/query spec
                                              ["SELECT payload FROM events
                                                WHERE session_id = ?
                                                  AND event_type = 'subagent/spawned'
                                                ORDER BY event_seq" pid])]
                       (into []
                             (comp (map (fn [r]
                                          (try (let [m (clojure.edn/read-string (:payload r))]
                                                 (or (get-in m [:metadata :child/session-id])
                                                     (:child/session-id m)))
                                               (catch Exception _ nil))))
                                   (filter some?)
                                   (map types/session-id))
                             rows))
                     (catch Exception _ []))
        link-kids (try
                    (mapv #(types/session-id (:child_session_id %))
                          (sqlite/query spec
                                        ["SELECT child_session_id FROM subagent_links WHERE parent_session_id = ? ORDER BY created_at"
                                         pid]))
                    (catch Exception _ []))]
    (into [] (distinct (concat work-kids event-kids link-kids)))))

(defn list-descendants
  "Return all descendant session ids (UUIDs) transitively spawned from
  `root-id` via subagent_links (BFS, not including `root-id`)."
  [db root-id]
  (try
    (session/list-descendants db root-id)
    (catch Exception _ [])))

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
  followed by every session reached by work-graph descendants and session descendants."
  [db child-id]
  (let [works (try (work-store/list-works db child-id) (catch Exception _ []))
        work-kids (mapcat #(try (work-subtree-session-ids db (:work/id %))
                                (catch Exception _ []))
                          works)
        link-kids (try (list-descendants db child-id)
                       (catch Exception _ []))]
    (vec (distinct (concat [child-id] work-kids link-kids)))))

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
                                (try (mapv :work/id (work-store/list-works db tid))
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
      ;; 3. cancel events: :session/cancelled per target + :subagent/cancelled on parent chain
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
  "The session's sole execution Work state, or nil when it has no Work yet."
  [db session-id]
  (some-> (last (work-store/list-works db session-id)) :work/state))

(defn cancel-subagent!
  "Cancel a single child subagent session `child-session-id` spawned from
  `parent-session-id`. Cascade: also cancels all transitive descendants."
  [db parent-session-id child-session-id reason]
  (when (nil? db)
    (throw (ex-info "cancel-subagent! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [child-id (resolve-cancel-session! db child-session-id)
        parent-id (when parent-session-id
                    (resolve-cancel-session! db parent-session-id))
        child (session/get-session db child-id)]
    (when-not child
      (throw (ex-info (str "child session not found: " child-id)
                      {:error/type :subagent/not-found
                       :session/id child-id})))
    (when (and parent-session-id parent-id (not (session/get-session db parent-id)))
      (throw (ex-info (str "parent session not found: " parent-id)
                      {:error/type :store/session-not-found
                       :session/id parent-id})))
    (if (= :cancelled (session-work-state db child-id))
      {:cancelled [] :already-cancelled? true :child/session-id child-id}
      (let [targets (cancel-targets db child-id)
            {:keys [cap-ids leases]} (cancel-subtree-tx! db child-id parent-id targets (or reason :user-request))]
        (tombstone-memory-leases! cap-ids leases)
        {:cancelled targets :already-cancelled? false :child/session-id child-id}))))

(defn cancel-subagent-tree!
  "Cascade-cancel the entire subtree rooted at `root-session-id`."
  [db root-session-id reason]
  (when (nil? db)
    (throw (ex-info "cancel-subagent-tree! requires a db/store handle" {:error/type :store/session-invalid})))
  (let [root-id (resolve-cancel-session! db root-session-id)
        root (session/get-session db root-id)]
    (when-not root
      (throw (ex-info (str "root session not found: " root-id)
                      {:error/type :subagent/not-found
                       :session/id root-id})))
    (if (= :cancelled (session-work-state db root-id))
      {:cancelled [] :already-cancelled? true :root/session-id root-id}
      (let [targets (cancel-targets db root-id)
            parent-id (get-parent-session-id db root-id)
            {:keys [cap-ids leases]} (cancel-subtree-tx! db root-id parent-id targets (or reason :user-request))]
        (tombstone-memory-leases! cap-ids leases)
        {:cancelled targets :already-cancelled? false :root/session-id root-id}))))

(defn cancel-non-terminal-children!
  "Structured-concurrency enforcement: cancel every live child of
  `session-id` so children never outlive their parent's terminal step."
  ([db session-id] (cancel-non-terminal-children! db session-id :parent-cancel cancel-subagent!))
  ([db session-id reason] (cancel-non-terminal-children! db session-id reason cancel-subagent!))
  ([db session-id reason cancel-fn]
   (when db
     (let [sid (try (types/session-id session-id) (catch Exception _ session-id))
           link-kids (try (child-session-ids db sid) (catch Exception _ []))
           works (try (work-store/list-works db sid) (catch Exception _ []))
           work-kids (distinct (mapcat #(try (work-subtree-session-ids db (:work/id %))
                                             (catch Exception _ []))
                                       works))
           live? (fn [cid]
                   (try
                     (boolean (some #(contains? #{:queued :running :waiting} (:work/state %))
                                    (mapcat #(try (work-store/list-works db %) (catch Exception _ []))
                                            (distinct (cons cid (try (list-descendants db cid)
                                                                     (catch Exception _ [])))))))
                     (catch Exception _ false)))]
       {:cancelled (vec (distinct (reduce (fn [acc cid]
                                            (try
                                              (if (live? cid)
                                                (into acc (:cancelled (cancel-fn db sid cid (or reason :parent-cancel))))
                                                acc)
                                              (catch Exception _ acc)))
                                          []
                                          (distinct (concat link-kids work-kids)))))}))))

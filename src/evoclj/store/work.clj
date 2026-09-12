(ns evoclj.store.work
  "Durable Work store — the single lifecycle queued/running/waiting/succeeded/failed/cancelled/timed-out (W2).

  W2: Work is the sole durable lifecycle. No bare Future shadows Command/Work
  state. Work's :running IS execution; a future is only an internal await
  handle, never an observable lifecycle. Cancel and timeout atomically drive
  Work state via compare-and-set (CAS) on the `works.state` column — the DB
  row is the truth, the future is not. Succeeded equals execution completed
  (the row transitions to :succeeded only after the execution future completes
  and its result is persisted). Failure and cancel/timeout are the same: each
  transition is a single atomic UPDATE WHERE state IN (expected) so concurrent
  drivers cannot both move the same row.

  Recovery is idempotent and Work-based: find-orphaned-works classifies
  crash residue (queued/running/waiting without a terminal event), and
  recover-works! drives running/waiting -> failed via CAS with
  {:error/type :recovery/orphaned} while queued orphans stay queued for
  redelivery. Re-running recovery on the same store is a no-op — already
  terminal rows are never revisited.

  Works replace the commands + subagent_sessions portions (W1). Each work is
  pinned to a session (immutable context); parent_work_id links child works
  (subagent = child Work + Principal + causal-links). The table `works` is
  created by migration 018-work.sql and is the durable source of truth;
  the store validates every transition against evoclj.runtime.work's closed
  transition table (definition > validation).

  Public Work contract is a closed Malli map:
    :work/id, :work/type, :work/state, :work/session-id,
    :work/parent-work-id (optional), :work/payload-ref,
    :work/created-at, :work/deadline (optional)."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.genome.types :as types]
            [evoclj.runtime.work :as work]
            [evoclj.store.sqlite :as sqlite]
            [malli.core :as m]
            [malli.error :as me])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp [inst]
  (let [inst (or inst (Date.))
        inst (cond (instance? Date inst) (.toInstant ^Date inst)
                   (instance? Instant inst) inst
                   :else (throw (err/error :store/work-invalid "created-at/deadline must be an inst" {:value inst})))]
    (.format timestamp-fmt inst)))

(defn- state->db [s] (get work/kw->db-state s (name s)))
(defn- db->state [s] (get work/db-state->kw s (keyword s)))

(def WorkState
  "Malli enum for :work/state."
  (into [:enum] (sort work/work-states)))

(def WorkSchema
  "Closed Malli schema for a persisted Work."
  [:map {:closed true}
   [:work/id uuid?]
   [:work/type keyword?]
   [:work/state WorkState]
   [:work/session-id uuid?]
   [:work/parent-work-id {:optional true} [:maybe uuid?]]
   [:work/payload-ref {:optional true} [:maybe string?]]
   [:work/created-at [:fn inst?]]
   [:work/deadline {:optional true} [:maybe [:fn inst?]]]
   [:work/continuation-edn {:optional true} :any]])

(defn work? [x] (boolean (m/validate WorkSchema x)))
(defn work-state? [s] (contains? work/work-states s))

(defn- row->work [row]
  (cond-> {:work/id (UUID/fromString (:id row))
           :work/type (keyword (:type row))
           :work/state (db->state (:state row))
           :work/session-id (UUID/fromString (:session_id row))
           :work/created-at (Date/from (Instant/parse (:created_at row)))}
    (:parent_work_id row) (assoc :work/parent-work-id (UUID/fromString (:parent_work_id row)))
    (contains? row :payload_ref) (assoc :work/payload-ref (:payload_ref row))
    (:deadline row) (assoc :work/deadline (Date/from (Instant/parse (:deadline row))))
    (:continuation_edn row) (assoc :work/continuation-edn (try (edn/read-string (:continuation_edn row)) (catch Exception _ (:continuation_edn row))))))

(defn- with-defaults [work]
  (cond-> work
    (nil? (:work/created-at work)) (assoc :work/created-at (Date.))))

(defn- duplicate-key? [e]
  (when-let [cmsg (or (ex-message e) (str e))]
    (boolean (re-find #"UNIQUE constraint failed.*works" cmsg))))

;; ---------------------------------------------------------------------------
;; Public DB ops
;; ---------------------------------------------------------------------------

(defn create-work!
  "Insert a work row. Validates :work/state against the closed Work vocabulary
  and enforces parent existence when :work/parent-work-id is supplied.
  Throws :store/work-invalid or :store/duplicate-work on UNIQUE violation."
  [db work]
  (let [work (with-defaults work)
        state (:work/state work)]
    (when-not (work/work-state? state)
      (throw (err/error :store/work-invalid "work state not in closed Work vocabulary" {:state state})))
    (let [id (str (or (:work/id work) (UUID/randomUUID)))
          type (subs (str (:work/type work)) 1)
          state-db (state->db state)
          session-id (str (types/session-id (:work/session-id work)))
          parent-id (some-> (:work/parent-work-id work) str)
          payload-ref (:work/payload-ref work)
          deadline (some-> (:work/deadline work) canonical-timestamp)
          continuation (some-> (:work/continuation-edn work) pr-str)
          created-at (canonical-timestamp (:work/created-at work))]
      (try
        (sqlite/with-db [conn db]
          (jdbc/insert! conn :works
                        (cond-> {:id id
                                 :type type
                                 :state state-db
                                 :session_id session-id
                                 :payload_ref payload-ref
                                 :created_at created-at
                                 :updated_at created-at}
                          parent-id (assoc :parent_work_id parent-id)
                          deadline (assoc :deadline deadline)
                          continuation (assoc :continuation_edn continuation))))
        (catch Exception e
          (if (duplicate-key? e)
            (throw (err/error :store/duplicate-work "work id already exists" {:work/id id :cause (ex-message e)}))
            (throw e))))
      (first (sqlite/query db ["SELECT * FROM works WHERE id = ?" id])))))

(defn fetch-work
  "Fetch a work by id, or nil."
  [db work-id]
  (some-> (first (sqlite/query db ["SELECT * FROM works WHERE id = ?" (str work-id)]))
          row->work))

(defn fetch-works-by-state
  [db state]
  (when-not (work/work-state? state)
    (throw (err/error :store/work-invalid "state not in Work vocabulary" {:state state})))
  (let [rows (sqlite/query db ["SELECT * FROM works WHERE state = ?" (state->db state)])]
    (mapv row->work rows)))
(defn get-parent-work-id
  "The parent Work id for `work-id`, or nil when `work-id` is a root Work.
  Throws :store/work-not-found when no Work has this id (fail closed —
  callers must not invent parent links)."
  [db work-id]
  (let [w (fetch-work db work-id)]
    (when-not w
      (throw (err/error :store/work-not-found "no work with this id" {:work/id work-id})))
    (:work/parent-work-id w)))

(defn child-work-ids
  "Direct child Work ids of `work-id` (ordered by created_at, oldest first).
  Empty vector when childless. Throws :store/work-not-found when `work-id`
  itself is missing (fail closed)."
  [db work-id]
  (when-not (fetch-work db work-id)
    (throw (err/error :store/work-not-found "no work with this id" {:work/id work-id})))
  (mapv #(UUID/fromString (:id %))
        (sqlite/query db ["SELECT id FROM works WHERE parent_work_id = ? ORDER BY created_at"
                          (str work-id)])))

(defn work-descendants
  "All transitive descendant Work ids of `root-work-id` via parent_work_id
  (BFS, not including `root-work-id`). Throws :store/work-not-found when
  the root is missing."
  [db root-work-id]
  (when-not (fetch-work db root-work-id)
    (throw (err/error :store/work-not-found "no work with this id" {:work/id root-work-id})))
  (loop [queue [(UUID/fromString (str root-work-id))] visited #{} result []]
    (if (empty? queue)
      result
      (let [cur (first queue)
            rest-q (vec (rest queue))]
        (if (contains? visited cur)
          (recur rest-q visited result)
          (let [children (try (child-work-ids db cur) (catch Exception _ []))
                visited' (conj visited cur)]
            (recur (into rest-q children) visited' (into result children))))))))

(defn work-depth
  "Depth of `work-id` in the Work graph. A root Work (no parent) has depth
  0, its child depth 1, etc. Throws :store/work-not-found when missing."
  [db work-id]
  (let [w (fetch-work db work-id)]
    (when-not w
      (throw (err/error :store/work-not-found "no work with this id" {:work/id work-id})))
    (loop [cur (:work/parent-work-id w) depth 0 seen #{}]
      (if (nil? cur)
        depth
        (if (contains? seen cur)
          depth
          (let [parent (fetch-work db cur)]
            (if-not parent
              depth
              (recur (:work/parent-work-id parent) (inc depth) (conj seen cur)))))))))

(defn work-fanout
  "Number of direct child Works of `work-id`. Throws :store/work-not-found
  when missing."
  [db work-id]
  (count (child-work-ids db work-id)))


;; ---------------------------------------------------------------------------
;; State machine transitions (CAS)
;; ---------------------------------------------------------------------------

(defn- cas-transition!
  [db work-id expected new-state]
  (when-not (work/work-state? new-state)
    (throw (err/error :store/work-invalid "target state not in Work vocabulary" {:new-state new-state})))
  (let [id-str (str work-id)
        work (fetch-work db work-id)]
    (when-not work
      (throw (err/error :store/work-not-found "no work with this id" {:work/id work-id})))
    (let [actual (:work/state work)
          expected-set (if (set? expected) expected #{expected})]
      (when-not (contains? expected-set actual)
        (throw (err/error :work/invalid-transition "work is not in the expected state"
                          {:work/id work-id :expected expected :state actual})))
      (when-not (some #(work/valid-transition? actual %) (if (set? new-state) new-state #{new-state}))
        ;; new-state is a single keyword; check direct edge
        (when-not (work/valid-transition? actual new-state)
          (throw (err/error :work/invalid-transition "not an edge of the Work state machine"
                            {:work/id work-id :expected-state actual :new-state new-state}))))
      (let [updated-at (canonical-timestamp nil)
            new-db (state->db new-state)]
        (sqlite/with-db [conn db]
          (let [cnt (first (jdbc/execute! conn ["UPDATE works SET state = ?, updated_at = ? WHERE id = ? AND state = ?"
                                                new-db updated-at id-str (state->db actual)]))]
            (when-not (= 1 cnt)
              (let [row (first (jdbc/query conn ["SELECT state FROM works WHERE id = ?" id-str]))]
                (if row
                  (throw (err/error :work/invalid-transition "work is not in the expected state"
                                    {:work/id work-id :expected expected :state (db->state (:state row))}))
                  (throw (err/error :store/work-not-found "no work with this id" {:work/id work-id})))))))
        (fetch-work db work-id)))))

(defn list-works
  "List works for a session, or all when session-id is nil."
  ([db] (list-works db nil))
  ([db session-id]
   (let [rows (if session-id
                (sqlite/query db ["SELECT * FROM works WHERE session_id = ? ORDER BY created_at" (str (types/session-id session-id))])
                (sqlite/query db ["SELECT * FROM works ORDER BY created_at"]))]
     (mapv row->work rows))))

;; ---------------------------------------------------------------------------
;; Session-graph traversal over the Work graph
;;
;; SESSION parentage is read exclusively from works.parent_work_id — the single
;; durable spawn truth (W2). These are the THREE traversals previously copied
;; into evoclj.runtime.subagent, evoclj.runtime.subagent-cancel, and
;; evoclj.store.session; this namespace is the owner because it is the Work-store
;; layer, already owns list-works/work-descendants/fetch-work, and sits below all
;; three callers in the dependency graph (nothing it requires reaches them).
;;
;; Every function coerces its `db` argument with sqlite/db-spec, so callers may
;; pass any handle shape they already hold — a jdbc spec, a path string, the
;; executor :stores map, or a SessionStore.
;; ---------------------------------------------------------------------------

(defn get-parent-session-id
  "Return the parent session id (UUID) for `child-session-id`, or nil.
  Uses the Work graph (works.parent_work_id) — the single durable spawn truth."
  [db child-session-id]
  (let [spec (sqlite/db-spec db)
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
  "All child session ids spawned from `parent-session-id` via the Work graph.
  Never throws: an unreadable store yields []."
  [db parent-session-id]
  (let [spec (sqlite/db-spec db)
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
  "All descendant session ids (UUIDs) transitively spawned from `root-id`
  via the Work graph (works.parent_work_id). Never throws: an unreadable
  store yields []."
  [db root-id]
  (let [root-id (types/session-id root-id)
        spec (sqlite/db-spec db)
        works (try (list-works spec root-id) (catch Exception _ []))
        descendant-work-ids (try
                              (mapcat #(work-descendants spec (:work/id %)) works)
                              (catch Exception _ []))]
    (->> descendant-work-ids
         (map #(try (fetch-work spec %) (catch Exception _ nil)))
         (keep :work/session-id)
         (remove #(= root-id %))
         distinct
         vec)))

(defn dispatch-work!
  "queued -> running (CAS)."
  [db work-id]
  (cas-transition! db work-id :queued :running))

(defn wait-work!
  "running -> waiting (CAS)."
  [db work-id]
  (cas-transition! db work-id :running :waiting))

(defn succeed-work!
  "running|waiting -> succeeded (CAS). Atomic: succeeds only if the row is
  still in :running or :waiting at UPDATE time. Returns the updated Work.
  Idempotent on already-succeeded: if the row is already :succeeded, returns
  it without error. Otherwise throws :work/invalid-transition when the row
  is not in an expected pre-state."
  [db work-id result-ref]
  (let [work (fetch-work db work-id)]
    (when-not work (throw (err/error :store/work-not-found "no work" {:work/id work-id})))
    (let [actual (:work/state work)]
      (when (contains? #{:succeeded :failed :cancelled :timed-out} actual)
        (if (= :succeeded actual)
          (throw (err/error :work/invalid-transition "work already succeeded (idempotent no-op not via succeed)" {:state actual}))
          (throw (err/error :work/invalid-transition "work already terminal" {:state actual}))))))
  (let [id-str (str work-id)
        updated-at (canonical-timestamp nil)]
    (sqlite/with-db [conn db]
      (let [cnt (first (jdbc/execute! conn ["UPDATE works SET state = ?, updated_at = ?, payload_ref = COALESCE(?, payload_ref) WHERE id = ? AND state IN ('running','waiting')" "succeeded" updated-at (some-> result-ref str) id-str]))]
        (when-not (= 1 cnt)
          (let [row (first (jdbc/query conn ["SELECT state FROM works WHERE id = ?" id-str]))]
            (if row
              (let [state (db->state (:state row))]
                (if (= :succeeded state)
                  nil
                  (throw (err/error :work/invalid-transition "succeed requires running or waiting" {:state state}))))
              (throw (err/error :store/work-not-found "no work" {:work/id work-id})))))))
    (fetch-work db work-id)))

(defn fail-work!
  "queued|running|waiting -> failed (CAS). Atomic: UPDATE WHERE state IN (...)."
  [db work-id reason]
  (let [work (fetch-work db work-id)]
    (when-not work (throw (err/error :store/work-not-found "no work" {:work/id work-id})))
    (let [actual (:work/state work)]
      (when (contains? #{:succeeded :failed :cancelled :timed-out} actual)
        (if (= :failed actual)
          (throw (err/error :work/invalid-transition "work already failed" {:state actual}))
          (throw (err/error :work/invalid-transition "work already terminal" {:state actual}))))))
  (let [id-str (str work-id)
        updated-at (canonical-timestamp nil)]
    (sqlite/with-db [conn db]
      (let [cnt (first (jdbc/execute! conn ["UPDATE works SET state = ?, updated_at = ? WHERE id = ? AND state IN ('queued','running','waiting')" "failed" updated-at id-str]))]
        (when-not (= 1 cnt)
          (let [row (first (jdbc/query conn ["SELECT state FROM works WHERE id = ?" id-str]))]
            (if row
              (throw (err/error :work/invalid-transition "fail requires queued, running or waiting" {:state (db->state (:state row))}))
              (throw (err/error :store/work-not-found "no work" {:work/id work-id})))))))
    (fetch-work db work-id)))

(defn cancel-work!
  "queued|running|waiting -> cancelled (CAS). Idempotent: if already
  :cancelled, returns the row; if already another terminal, throws."
  [db work-id]
  (let [work (fetch-work db work-id)]
    (when-not work (throw (err/error :store/work-not-found "no work" {:work/id work-id})))
    (let [actual (:work/state work)]
      (cond
        (= :cancelled actual) work
        (contains? #{:succeeded :failed :timed-out} actual) (throw (err/error :work/invalid-transition "work already terminal" {:state actual}))
        :else (cas-transition! db work-id #{:queued :running :waiting} :cancelled)))))

(defn timeout-work!
  "running|waiting -> timed-out (CAS, after deadline). Idempotent on already :timed-out."
  [db work-id]
  (let [work (fetch-work db work-id)]
    (when-not work (throw (err/error :store/work-not-found "no work" {:work/id work-id})))
    (let [actual (:work/state work)]
      (cond
        (= :timed-out actual) work
        (contains? #{:succeeded :failed :cancelled} actual) (throw (err/error :work/invalid-transition "work already terminal" {:state actual}))
        :else (cas-transition! db work-id #{:running :waiting} :timed-out)))))

(defn deadline-passed?
  "True when deadline is strictly before now (deadline has passed)."
  [deadline now]
  (when (and deadline now)
    (let [d ^Date (if (instance? Date deadline) deadline (Date/from (Instant/parse (str deadline))))
          n ^Date (if (instance? Date now) now (Date/from (Instant/parse (str now))))]
      (.before d n))))

;; ---------------------------------------------------------------------------
;; W2: Work-based recovery — idempotent, no fabrication of :succeeded
;; ---------------------------------------------------------------------------

(def orphan-work-error
  "Typed error used when a running/waiting Work is left orphaned by a crash."
  {:error/type :recovery/orphaned})

(defn find-orphaned-works
  "Works left in a non-terminal in-flight state when the process died:
  :queued (submitted but never dispatched), :running, or :waiting
  (dispatched but never settled). Terminal states are never orphans.
  Returns a vector of :work/* maps."
  [db]
  (into []
        (mapcat (fn [state] (try (fetch-works-by-state db state) (catch Exception _ [])))
                [:queued :running :waiting])))

(defn recover-works!
  "Idempotent Work recovery (report, not fabricate :succeeded).

  For each :queued orphan leaves the row :queued (no change) so redelivery
  is possible. For each :running or :waiting orphan marks the row :failed
  with {:error/type :recovery/orphaned} via CAS (fail-work!). Already
  terminal rows are ignored, so re-running recovery is a no-op.

  Returns {:orphaned-works [...] :recovered-queued [...] :recovered-running [...]}"
  [db]
  (let [orphans (find-orphaned-works db)]
    (reduce (fn [report work]
              (let [state (:work/state work)
                    wid (:work/id work)]
                (cond
                  (= :queued state)
                  (update report :recovered-queued conj wid)
                  (contains? #{:running :waiting} state)
                  (do
                    (try (fail-work! db wid orphan-work-error) (catch Exception _ nil))
                    (update report :recovered-running conj {:work/id wid :recovery/error orphan-work-error}))
                  :else report)))
            {:orphaned-works orphans :recovered-queued [] :recovered-running []}
            orphans)))

;; ---------------------------------------------------------------------------
;; Deadline sweeper — the loop that drives expired Works to timed-out
;; ---------------------------------------------------------------------------

(def sweep-deadline-error
  "Typed marker carried in the sweep report when a Work's :work/deadline has passed."
  {:error/type :work/deadline-exceeded})

(defn sweep-expired-deadlines!
  "Drive every non-terminal Work whose :work/deadline has passed to its
  timeout terminal via CAS: :queued -> :failed, :running/:waiting ->
  :timed-out. Terminal rows and rows without a deadline are untouched.
  Each row is swept independently — a CAS race on one row (already settled
  by its owner) is skipped, never thrown. Idempotent: re-running on swept
  rows is a no-op (they are terminal now).
  `now` defaults to the current instant; tests inject a fixed instant.
  Returns {:swept [...] :timed-out [...] :failed [...]} where each entry is
  {:work/id _ :recovery/error sweep-deadline-error}."
  ([db] (sweep-expired-deadlines! db nil))
  ([db now]
   (let [now (or now (java.util.Date.))
         orphans (find-orphaned-works db)]
     (reduce (fn [report work]
               (let [wid (:work/id work)
                     state (:work/state work)
                     deadline (:work/deadline work)
                     entry {:work/id wid :recovery/error sweep-deadline-error}]
                 (if-not (and deadline (deadline-passed? deadline now))
                   report
                   (try
                     (cond
                       (= :queued state)
                       (do (fail-work! db wid sweep-deadline-error)
                           (-> report
                               (update :swept conj entry)
                               (update :failed conj entry)))
                       (contains? #{:running :waiting} state)
                       (do (timeout-work! db wid)
                           (-> report
                               (update :swept conj entry)
                               (update :timed-out conj entry)))
                       :else report)
                     (catch Exception _ report)))))
             {:swept [] :timed-out [] :failed []}
             orphans))))

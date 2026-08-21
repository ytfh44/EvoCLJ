(ns evoclj.runtime.episode
  "component — materialize Episode records from completed sessions.

  materialize-episode! turns a TERMINAL session into an immutable
  Episode record: one SQLite row in the component `episodes` table that
  REFERENCES the session's causal trace and its CAS artifacts instead
  of copying any payload (Global Constraint 21 — the episode row
  stores task_ref, the first/last event ids, and small outcome/usage
  EDN maps; the bodies live in the filesystem CAS and the append-only
  event log). The returned map is the Episode contract of
  'Detailed Public Data Contracts':

      {:episode/id uuid?
       :session/id uuid?
       :generation/id stable-id?
       :genome/id GenomeId
       :resolution/id ResolutionId
       :task-ref ArtifactId
       :trace {:first-event int? :last-event int?}
       :outcome map?
       :usage map?}

  WHERE THE TASK PAYLOAD LIVES: run-session! (component) persists the
  task input as a CAS artifact on the :session/started event's
  :payload-ref. materialize-episode! READS that ref as the episode's
  :task-ref — the scheduler is the only writer of the task artifact;
  this namespace never writes a payload, only the referencing row, and
  it verifies the reference resolves in the CAS before committing the
  episode (:episode/task-artifact-missing if it does not).

  PINNED GENERATION (Global Constraint 2): the episode's
  :generation/id is the session row's pinned generation, read from the
  store at materialization time. Even when the CURRENT pointer has
  moved to a newer generation, the session's pin never changes, so the
  episode always names the generation the session actually ran under.

  TERMINAL SESSIONS ONLY: :created/:resolving/:running/:waiting
  sessions are not evidence yet and are rejected with
  :episode/not-terminal. EVERY terminal state becomes an episode —
  :completed, :failed, and :budget-exhausted are all evidence
  (failures are evidence, not discarded traces). The :outcome map
  carries the terminal :status (the session's persisted state) and a
  nil :score (v0 has no scoring; later tasks attach one).

  IDEMPOTENT: materializing the same session twice returns the same
  :episode/id and never duplicates the row.

  `store` is the executor's :stores map, exactly as the component
  scheduler defines it: {:sqlite <migrated db> :cas <CAS root>}. Both
  handles arrive open; this namespace opens and closes nothing.

  Error contract (Global Constraint 22 — plain serializable data):
  :episode/store-invalid (:reason :not-a-map :sqlite-missing
  :cas-missing), :episode/session-not-found,
  :episode/not-terminal (:session/state), :episode/task-ref-missing,
  :episode/task-artifact-missing (:task-ref), :episode/invalid
  (contract violation on the read-back row)."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

;; --- the Episode contract (Detailed Public Data Contracts) -----------------

(def EpisodeSchema
  "The public Episode contract map returned by materialize-episode!."
  [:map {:closed true}
   [:episode/id uuid?]
   [:session/id uuid?]
   [:generation/id string?]
   [:genome/id [:fn types/genome-id?]]
   [:resolution/id [:fn types/resolution-id?]]
   [:task-ref [:fn types/artifact-id?]]
   [:trace [:map {:closed true}
            [:first-event int?]
            [:last-event int?]]]
   [:outcome map?]
   [:usage map?]])

(defn- episode-error
  "A typed :episode/* ExceptionInfo carrying the distinguishing
  context."
  [type message data]
  (err/error type message data))

(defn- validate-episode!
  [e]
  (when-let [expl (m/explain EpisodeSchema e)]
    (throw (episode-error :episode/invalid
                          "episode does not satisfy the Episode contract"
                          {:errors (me/humanize expl)})))
  e)

;; --- store trust boundary ----------------------------------------------------

(defn- validate-store!
  "Validate the store trust boundary: the executor :stores map
  {:sqlite <db> :cas <CAS root>} exactly as the component scheduler
  defines it."
  [store]
  (when-not (map? store)
    (throw (episode-error :episode/store-invalid
                          "store must be the executor :stores map {:sqlite ... :cas ...}"
                          {:reason :not-a-map :value (err/sanitize store)})))
  (when-not (contains? store :sqlite)
    (throw (episode-error :episode/store-invalid
                          "store must carry the :sqlite handle"
                          {:reason :sqlite-missing})))
  (when-not (contains? store :cas)
    (throw (episode-error :episode/store-invalid
                          "store must carry the :cas handle"
                          {:reason :cas-missing})))
  store)

;; --- row mapping --------------------------------------------------------------

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  "Canonical ISO-8601 UTC string for a timestamp value (a
  java.util.Date, a java.time.Instant, or an ISO-8601 string);
  nil means now."
  [ts]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               (string? ts) (Instant/parse ts)
               :else (throw (episode-error :episode/invalid
                                           "timestamp must be an inst, Instant, or ISO-8601 string"
                                           {:timestamp ts})))]
    (.format timestamp-fmt inst)))

(defn- row->episode
  "Convert an episodes DB row into the public Episode contract map."
  [row]
  {:episode/id (UUID/fromString (:id row))
   :session/id (UUID/fromString (:session_id row))
   :generation/id (:generation_id row)
   :genome/id (:genome_id row)
   :resolution/id (:resolution_id row)
   :task-ref (:task_ref row)
   :trace {:first-event (:first_event_id row)
           :last-event (:last_event_id row)}
   :outcome (edn/read-string (:outcome row))
   :usage (edn/read-string (:usage row))})

;; --- the materialization ------------------------------------------------------

(defn materialize-episode!
  "Materialize the immutable Episode record for a TERMINAL session
  (component) and return the Episode contract map.

  Reads the session's PINNED generation/genome/resolution from the
  store's session row (never assumes CURRENT — Global Constraint 2),
  bounds the trace by the session's root :session/created event and
  its terminal event, and takes the episode's :task-ref from the
  :session/started event's :payload-ref — the CAS artifact the component scheduler already persisted for the task input. The episode row
  stores ONLY references (task_ref, first/last event ids, small
  outcome/usage EDN); no payload body is ever copied into it (Global
  Constraint 21). The task artifact is verified to resolve in the CAS
  before the row commits.

  Terminal sessions only: :created/:resolving/:running/:waiting
  sessions are rejected with :episode/not-terminal. :completed,
  :failed, and :budget-exhausted all become episodes — failures are
  evidence, not discarded traces. Materializing the same session
  twice is idempotent: the existing episode is returned and no row is
  duplicated.

  Typed errors: :episode/store-invalid, :episode/session-not-found,
  :episode/not-terminal, :episode/task-ref-missing,
  :episode/task-artifact-missing, :episode/invalid."
  [store session-id]
  (validate-store! store)
  (let [db (:sqlite store)
        sid (types/session-id session-id)
        s (session/get-session db sid)]
    (when-not s
      (throw (episode-error :episode/session-not-found
                            "no session with this id"
                            {:session/id sid})))
    (when-not (contains? session/terminal-states (:state s))
      (throw (episode-error :episode/not-terminal
                            "only terminal sessions become episodes"
                            {:session/id sid
                             :session/state (:state s)})))
    (let [existing (first (sqlite/query db
                                        ["SELECT * FROM episodes WHERE session_id = ?"
                                         (str sid)]))]
      (if existing
        ;; idempotent: the episode for this session already exists
        (validate-episode! (row->episode existing))
        (let [events (event/events-for-session db sid)
              root (first events)
              terminal (last events)
              started (some #(when (= :session/started (:event/type %)) %) events)
              task-ref (:payload-ref started)]
          (when-not root
            (throw (episode-error :episode/task-ref-missing
                                  "the session has no causal trace to bound"
                                  {:session/id sid})))
          (when-not task-ref
            (throw (episode-error :episode/task-ref-missing
                                  "the session's :session/started event carries no task artifact"
                                  {:session/id sid})))
          ;; the episode points at a REAL artifact, never a dangling id
          (when-not (cas/exists? (:cas store) task-ref)
            (throw (episode-error :episode/task-artifact-missing
                                  "the session's task artifact is missing from the CAS"
                                  {:session/id sid :task-ref task-ref})))
          (let [eid (UUID/randomUUID)
                outcome {:status (:state s) :score nil}
                ts (canonical-timestamp nil)]
            (sqlite/with-db [conn db]
              (jdbc/insert! conn :episodes
                            {:id (str eid)
                             :session_id (str sid)
                             :generation_id (:generation/id s)
                             :genome_id (:genome/id s)
                             :resolution_id (:resolution/id s)
                             :task_ref task-ref
                             :first_event_id (:event/id root)
                             :last_event_id (:event/id terminal)
                             :outcome (pr-str outcome)
                             :usage (pr-str {})
                             :created_at ts}))
            (validate-episode!
             (row->episode
              (first (sqlite/query db
                                   ["SELECT * FROM episodes WHERE id = ?"
                                    (str eid)]))))))))))

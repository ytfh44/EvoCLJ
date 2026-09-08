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

  TERMINAL WORK ONLY: :queued/:running/:waiting Works are not evidence yet
  and are rejected with :episode/not-terminal. EVERY terminal Work becomes
  an episode — :succeeded maps to :completed; :failed, :cancelled, and
  :timed-out are all evidence (failures are evidence, not discarded traces).
  The :outcome map carries the terminal Work status and a nil :score (v0
  has no scoring; later tasks attach one).

  IDEMPOTENT: materializing the same session twice returns the same
  :episode/id and never duplicates the row.

  `store` is the executor's :stores map, exactly as the component
  scheduler defines it: {:sqlite <migrated db> :cas <CAS root>}. Both
  handles arrive open; this namespace opens and closes nothing.

  Error contract (Global Constraint 22 — plain serializable data):
  :episode/store-invalid (:reason :not-a-map :sqlite-missing
  :cas-missing), :episode/session-not-found,
  :episode/not-terminal (:work/state), :episode/task-ref-missing,
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
            [evoclj.store.work :as work-store]
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

(def ^:private work-terminal-state->outcome
  "Work terminal state -> the Episode :outcome :status keyword
  (W2: the episode's outcome is derived from the session's terminal Work,
  never from a Session transition)."
  {:succeeded :completed
   :failed :failed
   :cancelled :cancelled
   :timed-out :budget-exhausted})

(defn- session-terminal-work
  "The session's terminal Work, or nil when no Work has reached a terminal
  state (W2: a terminal Work is the sole durable proof the session has run
  to a conclusion)."
  [db session-id]
  (some #(when (contains? (set (keys work-terminal-state->outcome)) (:work/state %)) %)
        (work-store/list-works db session-id)))

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

  Terminal Work only: a session with no terminal Work is rejected
  with :episode/not-terminal (W2: Work terminal state is the sole durable
  proof of completion). A terminal Work becomes an episode — its :outcome
  :status maps :succeeded→:completed, :failed→:failed, :cancelled→:cancelled,
  :timed-out→:budget-exhausted; failures are evidence, not discarded traces.
  Materializing the same session twice is idempotent: the existing episode
  is returned and no row is duplicated.

  Typed errors: :episode/store-invalid, :episode/session-not-found,
  :episode/not-terminal, :episode/task-ref-missing,
  :episode/task-artifact-missing, :episode/invalid."
  [store session-id]
  (validate-store! store)
  (let [db (:sqlite store)
        sid (types/session-id session-id)
        s (session/get-session db sid)
        tw (session-terminal-work db sid)]
    (when-not s
      (throw (episode-error :episode/session-not-found
                            "no session with this id"
                            {:session/id sid})))
    (when-not tw
      (throw (episode-error :episode/not-terminal
                            "only terminal Work becomes an episode"
                            {:session/id sid
                             :work/state nil})))
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
                outcome {:status (get work-terminal-state->outcome (:work/state tw)) :score nil}
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

(defn- replay-ineligible!
  [message data]
  (throw (episode-error :episode/replay-ineligible message data)))

(defn- verified-cas
  [handle]
  (if (map? handle)
    (assoc handle :verify true)
    (cas/->cas handle {:verify true})))

(defn- verified-edn-artifact!
  [cas-handle ref label]
  (when-not (string? ref)
    (replay-ineligible! (str label " is missing") {:artifact/ref ref :label label}))
  (try
    (edn/read-string (String. ^bytes (cas/get-bytes cas-handle ref)
                              java.nio.charset.StandardCharsets/UTF_8))
    (catch clojure.lang.ExceptionInfo e
      (replay-ineligible! (str label " failed CAS verification")
                          {:artifact/ref ref
                           :label label
                           :cause (err/error-data e)}))
    (catch Throwable t
      (replay-ineligible! (str label " is not valid EDN")
                          {:artifact/ref ref :label label
                           :cause (err/sanitize t)}))))

(defn- event-terminal?
  [e]
  (contains? #{:session/completed :session/failed :session/budget-exhausted}
             (:event/type e)))

(defn- required-replay-evidence
  [events]
  (doseq [e events
          :when (contains? #{:intent/proposed :intent/authorized
                             :provider/call-started :provider/call-completed
                             :intent/denied :intent/failed}
                            (:event/type e))]
    (when-not (get-in e [:metadata :replay/evidence-ref])
      (replay-ineligible! "session is missing replay evidence metadata"
                          {:event/id (:event/id e)
                           :event/type (:event/type e)
                           :reason :evidence-missing})))
  true)

(defn- evidence-complete?
  [evidence]
  (and (map? evidence)
       (= 1 (:replay/version evidence))
       (map? (:raw-intent evidence))
       (map? (:normalized-request evidence))
       (map? (:descriptor evidence))
       (= 1 (:canonicalization/version evidence))))

(defn- replay-trace
  [events evidence-by-ref cas-handle]
  (let [starts (filter #(= :provider/call-started (:event/type %)) events)
        completed (filter #(= :provider/call-completed (:event/type %)) events)]
    (mapv
     (fn [started]
       (let [sid (:intent/id (:metadata started))
             done (first (filter #(= sid (:intent/id (:metadata %))) completed))
             ref (get-in started [:metadata :replay/evidence-ref])
             evidence (get evidence-by-ref ref)
             result-ref (or (get-in done [:metadata :replay/provider-result-ref])
                            (:payload-ref done))]
         (when-not done
           (replay-ineligible! "provider call has no completed result"
                               {:event/id (:event/id started) :intent/id sid}))
         (when-not (evidence-complete? evidence)
           (replay-ineligible! "provider call has incomplete replay evidence"
                               {:event/id (:event/id started) :intent/id sid
                                :evidence/ref ref}))
         (when-not (= result-ref (:payload-ref done))
           (replay-ineligible! "provider result reference does not match event payload"
                               {:event/id (:event/id done)
                                :expected (:payload-ref done)
                                :actual result-ref}))
         {:intent/id sid
          :intent (:raw-intent evidence)
          :normalized-request (:normalized-request evidence)
          :descriptor (:descriptor evidence)
          :response (verified-edn-artifact! cas-handle result-ref "provider result")
          :evidence-ref ref
          :provider-result-ref result-ref}))
     starts)))

(defn load-replay-episode!
  "Load a historical Episode without mutating its session, events, or episode row.

  The full event chain and every referenced CAS body are verified before the
  Episode bounds are sliced. Sessions written before replay evidence capture,
  or sessions with incomplete provider evidence, fail closed as
  :episode/replay-ineligible."
  [store session-id]
  (validate-store! store)
  (let [sid (types/session-id session-id)
        db (:sqlite store)
        cas-handle (verified-cas (:cas store))
        chain (event/verify-event-chain db sid)]
    (when-not (:valid? chain)
      (replay-ineligible! "event chain failed verification"
                          {:session/id sid :reason (:reason chain)
                           :event/seq (:event/seq chain)}))
    (let [s (session/get-session db sid)
          row (first (sqlite/query db
                                   ["SELECT * FROM episodes WHERE session_id = ?"
                                    (str sid)]) )
          all-events (event/events-for-session db sid)]
      (when-not s
        (replay-ineligible! "historical session does not exist" {:session/id sid}))
      (when-not row
        (replay-ineligible! "historical session has no materialized Episode"
                            {:session/id sid :reason :episode-missing}))
      (let [episode (validate-episode! (row->episode row))
            first-id (get-in episode [:trace :first-event])
            last-id (get-in episode [:trace :last-event])
            events (vec (filter #(and (<= first-id (:event/id %))
                                      (<= (:event/id %) last-id)) all-events))
            started (first (filter #(= :session/started (:event/type %)) events))
            terminal (last (filter event-terminal? events))
            refs (->> events
                      (mapcat (fn [e]
                                (keep identity
                                      [(:payload-ref e)
                                       (get-in e [:metadata :replay/evidence-ref])
                                       (get-in e [:metadata :replay/provider-result-ref])
                                       (get-in e [:metadata :error/artifact-ref])
                                       (get-in e [:metadata :output/ref])])))
                      set)]
        (when (or (nil? first-id) (nil? last-id) (empty? events)
                  (not= first-id (:event/id (first events)))
                  (not= last-id (:event/id (last events)))
                  (nil? started) (nil? terminal))
          (replay-ineligible! "Episode bounds do not identify a complete terminal trace"
                              {:session/id sid :first-event first-id :last-event last-id}))
        (when-not (:payload-ref started)
          (replay-ineligible! "session task input has no CAS reference"
                              {:session/id sid :event/id (:event/id started)}))
        (required-replay-evidence events)
        ;; Touch every referenced body with verification enabled before parsing
        ;; any of the selected artifacts. This catches tampering even for an
        ;; otherwise unused failure payload.
        (doseq [ref refs]
          (cas/get-bytes cas-handle ref))
        (let [evidence-by-ref
              (into {} (for [ref (set (keep #(get-in % [:metadata :replay/evidence-ref]) events))]
                         [ref (verified-edn-artifact! cas-handle ref "replay evidence")]))
              task-input (verified-edn-artifact! cas-handle (:payload-ref started) "session task input")
              trace (replay-trace events evidence-by-ref cas-handle)
              terminal-output (when (:payload-ref terminal)
                                (verified-edn-artifact! cas-handle (:payload-ref terminal)
                                                        "terminal output") )]
          {:eligible? true
           :episode episode
           :session s
           :events events
           :task-input task-input
           :terminal-output terminal-output
           :terminal/event terminal
           :trace trace
           :provenance {:event-chain chain
                        :cas/verified? true
                        :episode/id (:episode/id episode)
                        :session/id sid
                        :event-range [first-id last-id]
                        :evidence-refs (set (keys evidence-by-ref))}})))))

(defn load-replay-episode
  "Return a typed eligibility result instead of throwing for old/incomplete data."
  [store session-id]
  (try
    (load-replay-episode! store session-id)
    (catch clojure.lang.ExceptionInfo e
      (if (= :episode/replay-ineligible (:error/type (ex-data e)))
        {:eligible? false
         :error/type :episode/replay-ineligible
         :error/message (ex-message e)
         :error/data (err/error-data e)}
        (throw e)))))

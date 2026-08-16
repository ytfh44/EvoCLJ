(ns evoclj.runtime.scheduler-stress-test
  "Task R4 — scheduler concurrency semantics + stress test.

  N sessions × M events run CONCURRENTLY (real host threads behind a
  barrier) over ONE shared store: a single migrated sqlite db, a
  single CAS root, and a single provider registry / broker context.
  Each session runs on its OWN executor — its own phenotype with its
  OWN isolated SCI runtime — because evoclj.sci.execute documents a
  SCI runtime as NOT thread-safe (\"it belongs to one
  Phenotype/session\"): the scheduler's concurrency model is parallel
  sessions, isolated runtimes, one serializing store. The store is the
  single contention point, and that is exactly where corruption would
  appear (see docs/scheduler.md).

  The stress topology is a pure chain of `tool-count` :fixture/echo
  :tool nodes into :emit, so every session run persists the SAME
  exact event sequence of length M = 5 + 6×tool-count:

    1 :session/created root (appended by the host)          (1)
    1 :session/started                                       (1)
    per :tool node: :node/started :node/completed
                    :intent/proposed :intent/authorized
                    :provider/call-started :provider/call-completed
                                                             (6T)
    :node/emit :node/started + :node/completed               (2)
    1 :session/completed                                      (1)

  After every session completes the test proves, for EVERY session:

  - hash chain valid:  verify-event-chain passes with :events = M;
  - no lost events:    the persisted event-type sequence is EXACTLY
                       the expected pattern, per-session :event/seq is
                       exactly 1..M with no gaps or duplicates, the
                       total across all sessions is N × M, and the
                       shared provider executed exactly once per tool
                       node per session;
  - no cross-session leakage: every event row carries its own
    session id and the pinned generation/phenotype identity; every
    non-root :cause/event-id resolves to an event id in the SAME
    session's own log; global event ids are unique; and each
    session's final outputs artifact contains only that session's own
    task text (the fixture echo provider returns its input, so any
    cross-session value swap would be visible in the outputs).

  DETERMINISM: the fixture is fully deterministic apart from the
  :created-at timestamps (and the event hashes derived from them,
  which verify-event-chain re-derives losslessly). A second test runs
  the entire N×M scenario twice and compares structural fingerprints
  (per-session event-type sequences, per-session counts, global
  totals) — equal on every run."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.compiler.topology :as topology]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util.concurrent CountDownLatch)))

;; --- shared fixture identity -------------------------------------------------

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private genome-id (str "sha256:" hex64))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype-id (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private generation-id "generation-1")

;; --- temp stores --------------------------------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-scheduler-stress-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-scheduler-stress-cas-"
                                     (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifacts)."
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup!
  []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

;; --- the shared store ----------------------------------------------------------

(defn- echo-lease
  "A valid CapabilityLease granting this phenotype's exact id the
  :fixture/echo :invoke action for the next minute."
  []
  (let [now (java.util.Date.)]
    {:cap/id (random-uuid)
     :subject {:phenotype/id phenotype-id}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 1000}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- shared-store
  "ONE migrated sqlite db, ONE CAS root, ONE provider registry (with a
  shared :executions counter), ONE usage atom and ONE lease set — the
  single store every concurrent session appends to and executes
  through. The generation row every session is pinned to is seeded
  here."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 0
                     :created_at "2025-01-01T00:00:00Z"}))
    (let [executions (atom 0)
          reg (registry/create-registry)]
      (registry/register! reg (fixture/echo-provider
                               {:execution-count executions}))
      {:db db
       :cas-root (temp-cas-dir)
       :registry reg
       :executions executions
       :usage (atom {})
       :leases [(echo-lease)]})))

(defn- session-executor
  "A per-session executor over the SHARED store: its OWN phenotype
  (its OWN isolated SCI runtime — the documented concurrency model;
  no session shares a runtime with a concurrently running session),
  but the shared sqlite db, CAS root, registry, leases, and usage
  atom. Every session carries the SAME pinned identity (same logical
  genome/resolution/phenotype — separate instances of one Phenotype).
  Returns the executor map."
  [shared compiled]
  {:phenotype (phenotype/instantiate
               compiled
               {:stores {:sqlite :poison :cas {:root :poison}}
                :providers {:registry (:registry shared)}
                :capabilities {:leases (:leases shared)
                               :usage (:usage shared)}
                :program-sources {}})
   :stores {:sqlite (:db shared)
            :cas (cas/->cas (:cas-root shared))}
   :dispatch (dispatch/make-broker-context
              {:registry (:registry shared)
               :leases (:leases shared)
               :usage (:usage shared)})})

(defn- create-pinned-session
  "create-session! pinned to the fixture identity, then append the
  :session/created root event (the host's job — the scheduler anchors
  its causal chain on it). Returns the session id."
  [executor]
  (let [db (:sqlite (:stores executor))
        sid (:session/id
             (session/create-session!
              db
              {:genome/id genome-id
               :resolution/id resolution-id
               :phenotype/id phenotype-id
               :generation/id generation-id}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id generation-id
                          :phenotype/id phenotype-id
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

(defn- artifact-edn
  "Read a CAS artifact back as EDN data."
  [executor artifact-id]
  (edn/read-string
   (String. (cas/get-bytes (:cas (:stores executor)) artifact-id)
            StandardCharsets/UTF_8)))

;; --- the stress topology --------------------------------------------------------

(defn- tool-chain-topology
  "A pure chain of `tool-count` :fixture/echo :tool nodes into :emit —
  no :sci nodes, so a run needs no SCI invocation and the fixture
  stays entirely within the scheduler + broker + store. A run persists
  exactly 5 + 6×tool-count events (see the namespace docstring)."
  [tool-count]
  (let [tools (for [i (range tool-count)]
                {:node/id (keyword (str "node/tool-" i))
                 :node/type :tool
                 :tool :fixture/echo
                 :next (if (= i (dec tool-count))
                         :node/emit
                         (keyword (str "node/tool-" (inc i))))})]
    {:graph/id :graph/stress
     :entry :node/tool-0
     :nodes (into {:node/emit {:node/type :emit}}
                  (map (fn [t] [(:node/id t) t]) tools))
     :limits {:max-steps (+ tool-count 2)}}))

(defn- compiled-genome
  "A minimal CompiledGenome value carrying the stress topology —
  constructed directly, exactly as the Task 6.3 scheduler tests do."
  [fixture-topology]
  {:compiled/genome-id genome-id
   :compiled/resolution-id resolution-id
   :compiled/phenotype-id phenotype-id
   :abi {}
   :manifest {}
   :topology (topology/compile-topology fixture-topology)
   :programs (sorted-map)})

(defn- expected-event-types
  "The EXACT per-session event-type sequence a run of a
  tool-count-node chain must persist (see the namespace docstring)."
  [tool-count]
  (into [:session/created :session/started]
        (concat (mapcat (fn [_]
                          [:node/started :node/completed
                           :intent/proposed :intent/authorized
                           :provider/call-started :provider/call-completed])
                        (range tool-count))
                [:node/started :node/completed
                 :session/completed])))

(defn- run-stress-scenario
  "Launch `n` sessions concurrently (behind a barrier) over ONE shared
  store, each running the tool-count-node chain with its own distinct
  task text, and return the outcomes in launch order:
  {:task-index <launch index>
   :executor <per-session executor>
   :session/id <uuid>
   :result <run-session! result map>}. A session that does not
  complete within 120s is reported as :timed-out."
  [n tool-count]
  (let [shared (shared-store)
        barrier (CountDownLatch. 1)
        futures (mapv
                 (fn [i]
                   (future
                     (.await barrier)
                     (let [executor (session-executor shared
                                                      (compiled-genome
                                                       (tool-chain-topology tool-count)))
                           task {:op :echo :text (str "s" i)}
                           sid (create-pinned-session executor)]
                       {:task-index i
                        :executor executor
                        :session/id sid
                        :result (scheduler/run-session! executor sid task)})))
                 (range n))]
    (.countDown barrier)
    {:shared shared
     :outcomes (mapv #(deref % 120000 :timed-out) futures)}))

(defn- stress-fingerprint
  "The deterministic structural fingerprint of one full N×M stress
  run: per session (in launch order) the persisted event-type
  sequence and the verified event count, plus the global totals and
  the total provider executions. Wall-clock timestamps and the
  hashes derived from them are deliberately excluded — everything the
  scheduler semantics promise is compared."
  [n tool-count]
  (let [m (+ 5 (* 6 tool-count))
        {:keys [shared outcomes]} (run-stress-scenario n tool-count)
        db (:db shared)
        per-session (mapv (fn [o]
                            (let [sid (:session/id o)]
                              {:event/types (mapv :event/type
                                                  (event/events-for-session db sid))
                               :verified (:events (event/verify-event-chain db sid))
                               :status (:status (:result o))}))
                          outcomes)]
    {:per-session per-session
     :total-events (reduce + 0 (map :verified per-session))
     :provider-executions @(:executions shared)}))

;; ============================================================================
;; N sessions × M events interleaved — no corruption
;; ============================================================================

(deftest n-sessions-times-m-events-interleaved-proves-no-corruption
  (let [n 8
        tool-count 5
        m (+ 5 (* 6 tool-count))            ; 35 events per session
        {:keys [shared outcomes]} (run-stress-scenario n tool-count)
        db (:db shared)
        expected (expected-event-types tool-count)]
    (testing "every session completed with the exact per-session event count (nothing lost, nothing duplicated)"
      (is (= n (count outcomes)))
      (doseq [{:keys [result]} outcomes]
        (is (= :completed (:status result)))
        (is (= m (inc (:event/count result))))))
    (testing "the append-only hash chain verifies for EVERY session"
      (doseq [o outcomes]
        (let [sid (:session/id o)
              v (event/verify-event-chain db sid)]
          (is (:valid? v) (pr-str v))
          (is (= m (:events v))))))
    (testing "each session's log is the EXACT expected event sequence (deterministic order, no lost events, no intra-session interleaving)"
      (doseq [o outcomes]
        (is (= expected
               (mapv :event/type (event/events-for-session db (:session/id o)))))))
    (testing "per-session :event/seq is exactly 1..M with no gaps and no duplicates"
      (doseq [o outcomes]
        (is (= (range 1 (inc m))
               (mapv :event/seq (event/events-for-session db (:session/id o)))))))
    (testing "no cross-session leakage: every row belongs to its own session, carries the pinned identity, and every cause resolves inside the same session"
      (let [by-session (into {}
                             (map (fn [o]
                                    [(:session/id o)
                                     (event/events-for-session db (:session/id o))]))
                             outcomes)
            all-ids (into #{} (mapcat (fn [[_ evs]] (map :event/id evs))) by-session)]
        (is (= n (count by-session)) "the sessions are distinct")
        (is (= (* n m) (count all-ids)) "global event ids are unique across sessions")
        (doseq [[sid evs] by-session]
          (let [own-ids (into #{} (map :event/id) evs)]
            (is (every? #(= sid (:session/id %)) evs)
                (str "every event of " sid " belongs to " sid))
            (is (every? #(= generation-id (:generation/id %)) evs)
                (str "every event of " sid " carries the pinned generation"))
            (is (every? #(= phenotype-id (:phenotype/id %)) evs)
                (str "every event of " sid " carries the pinned phenotype"))
            (is (every? (fn [e]
                          (let [c (:cause/event-id e)]
                            (or (nil? c) (contains? own-ids c))))
                        evs)
                (str "every cause of " sid " resolves inside " sid))))))
    (testing "the session rows stayed pinned to the fixture identity (Global Constraint 2)"
      (doseq [o outcomes]
        (let [s (session/get-session db (:session/id o))]
          (is (= genome-id (:genome/id s)))
          (is (= resolution-id (:resolution/id s)))
          (is (= phenotype-id (:phenotype/id s)))
          (is (= :completed (:state s))))))
    (testing "each session's final outputs contain EXACTLY that session's own task text (no cross-session value leakage)"
      (let [by-session (into {} (map (fn [{:keys [task-index executor result]}]
                                       [(str "s" task-index)
                                        (map :text (artifact-edn executor (:output-ref result)))])
                                     outcomes))
            own-texts (into #{} (map (fn [i] (str "s" i))) (range n))]
        (is (= n (count by-session)))
        (doseq [[task-text texts] by-session]
          (is (= (repeat tool-count task-text) texts)
              (str "the outputs of " task-text " hold only its own echo value"))
          (is (every? own-texts texts)
              (str "every output of " task-text " is one of the session task texts")))
        (is (every? (fn [[task-text texts]]
                      (every? #(= task-text %) texts))
                    by-session)
            "no session's outputs contain another session's text")))
    (testing "the shared provider executed exactly once per tool node per session (no lost, no duplicated effects)"
      (is (= (* n tool-count) @(:executions shared))))
    (testing "the total persisted events across ALL sessions is exactly N × M"
      (is (= (* n m)
             (reduce + 0
                     (map (fn [o]
                            (count (event/events-for-session db (:session/id o))))
                          outcomes)))))))

;; ============================================================================
;; the scenario is deterministic across runs
;; ============================================================================

(deftest stress-scenario-is-deterministic-across-runs
  (let [n 4
        tool-count 4
        m (+ 5 (* 6 tool-count))            ; 29 events per session
        f1 (stress-fingerprint n tool-count)
        f2 (stress-fingerprint n tool-count)]
    (testing "two full concurrent runs of the same fixture produce identical structure"
      (is (= f1 f2)))
    (testing "and that structure is exactly the expected one"
      (is (= n (count (:per-session f1))))
      (is (every? #(= m (:verified %)) (:per-session f1)))
      (is (= (* n m) (:total-events f1)))
      (is (= (* n tool-count) (:provider-executions f1))))))

(ns evoclj.runtime.scheduler-test
  "component tests for the deterministic scheduler and step budget.

  run-session! executes the phenotype's compiled topology (the
  executor map's :phenotype :compiled :topology) against a session the
  test pinned to the SAME genome/resolution/phenotype ids, walking
  nodes from :entry, stepping each node's handler, dispatching every
  emitted intent through evoclj.intent.dispatch! (the broker), feeding
  the provider results back into the session's accumulated outputs,
  and persisting EVERY transition through
  evoclj.store.event/append-event! and
  evoclj.store.session/transition-session!.

  The four normative scenarios, in the task's numbered order:

  - Step 1: the fixture graph sci → tool → emit runs in the EXACT
    logical event order (17 events including the :session/created
    root: session started → node/router stepped → its intent
    dispatched (proposed → authorized → provider-call-started →
    provider-call-completed) → node/tool stepped → its intent
    dispatched → node/emit completes → session completed).
  - Step 2: the SAME graph under :limits {:max-steps 2} halts before
    the third node as :session/budget-exhausted.
  - Step 3: every step's node-start/node-result/intent/result events
    are appended (and causally chained) BEFORE the scheduler advances
    to the next node — proven by the exact per-type sequence
    allocation.
  - Step 4: an unhandled node failure (:failed transition) fails the
    session and preserves the error payload as a CAS artifact whose
    ref rides in the :session/failed event metadata.

  Plus one supplementary test covering the taxonomy's other dispatch
  outcomes (:intent/denied — a denied intent never reaches a provider
  and the session continues to completion).

  FIXTURE DESIGN: the scheduler tests construct the CompiledGenome
  directly (pure data, topology validated through
  evoclj.compiler.topology/compile-topology) instead of going through
  evoclj.compiler.core/compile-genome, because the seed bundle's
  minimal-valid topology (:llm → :sci → :emit) is not executable in
  v0 (:llm is not-implemented). The fixture graphs here are
  sci → tool → emit (Step 1/2), boom → emit (Step 4), and tool → emit
  (the denied-intent test). The scheduler reads only the pinned
  identity (:compiled/genome-id, :compiled/resolution-id,
  :compiled/phenotype-id) and the compiled :topology from the
  phenotype, so a directly constructed CompiledGenome is a faithful
  test double.

  The executor map shape (normative for component, designed here):

    {:phenotype <Phenotype from evoclj.runtime.phenotype/instantiate>
     :stores {:sqlite <migrated db> :cas <CAS root>}
     :dispatch <broker context from evoclj.intent.dispatch/make-broker-context>}

  The test builds it directly; Integrant assembly is component"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
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
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixture identity ------------------------------------------------

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private genome-id (str "sha256:" hex64))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype-id (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private generation-id "generation-1")

;; --- temp stores ------------------------------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-scheduler-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-scheduler-cas-"
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

(defn- fresh-db
  "A migrated database spec backed by a fresh temp file, seeded with
  the generation row sessions are pinned to."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (doseq [artifact-id [genome-id resolution-id phenotype-id]]
        (jdbc/execute!
         conn
         ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
           VALUES (?, 'application/octet-stream', 0, datetime('now'))"
          artifact-id]))
      (jdbc/execute!
       conn
       ["INSERT OR IGNORE INTO genomes (id, created_at)
        VALUES (?, datetime('now'))"
        genome-id])
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 0
                     :created_at "2025-01-01T00:00:00Z"}))
    db))

;; --- fixture topologies and programs ---------------------------------------

(defn- chain-topology
  "The component fixture graph: :sci router → :tool → :emit."
  [limits]
  {:graph/id :graph/scheduler-fixture
   :entry :node/router
   :nodes
   {:node/router {:node/type :sci :program :program/route :next :node/tool}
    :node/tool {:node/type :tool :tool :fixture/echo :next :node/emit}
    :node/emit {:node/type :emit}}
   :limits limits})

(defn- boom-topology
  "A graph whose :entry :sci node runs a program that always throws."
  []
  {:graph/id :graph/boom
   :entry :node/boom
   :nodes
   {:node/boom {:node/type :sci :program :program/boom :next :node/emit}
    :node/emit {:node/type :emit}}
   :limits {:max-steps 64}})

(defn- tool-only-topology
  "A :tool entry node straight into :emit."
  []
  {:graph/id :graph/tool-only
   :entry :node/tool
   :nodes
   {:node/tool {:node/type :tool :tool :fixture/echo :next :node/emit}
    :node/emit {:node/type :emit}}
   :limits {:max-steps 64}})

(defn- compiled-genome
  "A minimal CompiledGenome value carrying a custom executable
  topology — constructed directly (see the namespace docstring)."
  [fixture-topology]
  {:compiled/genome-id genome-id
   :compiled/resolution-id resolution-id
   :compiled/phenotype-id phenotype-id
   :abi {}
   :manifest {}
   :topology (topology/compile-topology fixture-topology)
   :programs (into (sorted-map)
                   {:program/route {:program/id :program/route
                                    :entry 'agent.route/run}
                    :program/boom {:program/id :program/boom
                                   :entry 'test.boom/run}})})

(defn- program-sources
  []
  {:program/route (slurp (io/resource
                          "fixtures/genomes/minimal-valid/programs/route.clj"))
   :program/boom (str "(ns test.boom)\n"
                      "(defn run [x] (throw (ex-info \"boom\" {:error/type :test/boom})))")})

(defn- echo-lease
  "A valid CapabilityLease granting this phenotype's exact id the
  :fixture/echo :invoke action for the next minute."
  []
  (let [now (java.util.Date.)]
    {:cap/id (random-uuid)
     :subject {:phenotype/id phenotype-id}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 10}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- build-executor
  "Build the executor map for a fixture topology: a live phenotype
  (with :program/route and :program/boom loaded into its isolated SCI
  runtime), the opened sqlite + cas stores, and a broker context. With
  `grant-lease?` true the broker carries one :fixture/echo lease for
  the phenotype; with false it carries none (every tool call is
  denied). Returns {:executor <executor map> :executions <atom>} where
  :executions counts real provider executions."
  ([fixture-topology] (build-executor fixture-topology true))
  ([fixture-topology grant-lease?]
   (let [executions (atom 0)
         reg (registry/create-registry)
         _ (registry/register! reg (fixture/echo-provider
                                    {:execution-count executions}))
         usage (atom {})
         leases (when grant-lease? [(echo-lease)])
         compiled (compiled-genome fixture-topology)
         ph (phenotype/instantiate
             compiled
             {:stores {:sqlite :poison :cas {:root :poison}}
              :providers {:registry reg}
              :capabilities {:leases (or leases []) :usage usage}
              :program-sources (program-sources)})]
     {:executor {:phenotype ph
                 :stores {:sqlite (fresh-db) :cas (cas/->cas (temp-cas-dir))}
                 :dispatch (dispatch/make-broker-context
                            {:registry reg
                             :leases (or leases [])
                             :usage usage})}
      :executions executions})))

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

;; ============================================================================
;; Step 1 — the fixture graph runs in the EXACT logical event order
;; ============================================================================

(deftest step-1-fixture-graph-runs-in-exact-logical-event-order
  (let [{:keys [executor executions]} (build-executor (chain-topology {:max-steps 64}))
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:op :echo :text "abc"})
        events (event/events-for-session (:sqlite (:stores executor)) sid)
        by-type (group-by :event/type events)]
    (testing "run-session! returns a completed session with the final outputs artifact"
      (is (= :completed (:status result)))
      (is (= sid (:session/id result)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:output-ref result)))
      (is (nil? (:error/artifact-ref result)))
      (is (nil? (:episode/id result)))
      (is (= 16 (:event/count result))))
    (testing "the persisted events follow the EXACT logical order (Step 1 + Step 3)"
      (is (= [:session/created :session/started
              :node/started :node/completed
              :intent/proposed :intent/authorized
              :provider/call-started :provider/call-completed
              :node/started :node/completed
              :intent/proposed :intent/authorized
              :provider/call-started :provider/call-completed
              :node/started :node/completed
              :session/completed]
             (mapv :event/type events))))
    (testing "the :session/started event persists the task input as a CAS artifact"
      (is (= {:op :echo :text "abc"}
             (artifact-edn executor
                           (get-in events [1 :payload-ref])))))
    (testing "node and intent events carry attribution (Global Constraint 20)"
      (is (= #{:node/router :node/tool :node/emit}
             (set (map #(get-in % [:metadata :node/id]) (:node/started by-type)))))
      (is (= 2 (count (:intent/proposed by-type))))
      (is (every? uuid? (map #(get-in % [:metadata :intent/id]) (:intent/proposed by-type))))
      (is (= 2 (count (:provider/call-completed by-type)))))
    (testing "every authorized intent really reached the provider once"
      (is (= 2 @executions)))
    (testing "the session ended :completed and the output artifact holds the accumulated outputs"
      (is (= :completed (:state (session/get-session (:sqlite (:stores executor)) sid))))
      (is (= [{:action {:intent/type :intent/tool-call
                        :payload {:tool/id :fixture/echo :args {:text "abc"}}}}
              {:text "abc"}
              {:text "abc"}]
             (artifact-edn executor (:output-ref result)))))
    (testing "the append-only chain verifies end to end"
      (is (:valid? (event/verify-event-chain (:sqlite (:stores executor)) sid))))))

;; ============================================================================
;; Step 2 — :limits {:max-steps N} halts an overlong run as :budget-exhausted
;; ============================================================================

(deftest step-2-max-steps-budget-halts-overlong-runs
  (let [{:keys [executor executions]} (build-executor (chain-topology {:max-steps 2}))
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:op :echo :text "abc"})
        events (event/events-for-session (:sqlite (:stores executor)) sid)
        budget-event (last events)]
    (testing "the run halts as :budget-exhausted before the third node is ever stepped"
      (is (= :budget-exhausted (:status result)))
      (is (= :budget-exhausted
             (:state (session/get-session (:sqlite (:stores executor)) sid))))
      (is (= 2 (count (filter #(= :node/started (:event/type %)) events)))))
    (testing "the trace is the completed first two steps, then :session/budget-exhausted"
      (is (= [:session/created :session/started
              :node/started :node/completed
              :intent/proposed :intent/authorized
              :provider/call-started :provider/call-completed
              :node/started :node/completed
              :intent/proposed :intent/authorized
              :provider/call-started :provider/call-completed
              :session/budget-exhausted]
             (mapv :event/type events))))
    (testing "the budget event records the limit, the steps consumed, and the outputs so far"
      (is (= {:max-steps 2} (get-in budget-event [:metadata :limits])))
      (is (= 2 (get-in budget-event [:metadata :steps])))
      (is (= (:output-ref result) (:payload-ref budget-event)))
      (is (= 14 (:event/count result))))
    (testing "the outputs accumulated before the halt are preserved as evidence"
      (is (= [{:action {:intent/type :intent/tool-call
                        :payload {:tool/id :fixture/echo :args {:text "abc"}}}}
              {:text "abc"}
              {:text "abc"}]
             (artifact-edn executor (:output-ref result)))))
    (testing "the two authorized intents that did run reached the provider"
      (is (= 2 @executions)))))

;; ============================================================================
;; Step 3 — each step's events are appended BEFORE the scheduler advances
;; ============================================================================

(deftest step-3-each-steps-events-are-persisted-before-advancing
  (let [{:keys [executor]} (build-executor (chain-topology {:max-steps 64}))
        sid (create-pinned-session executor)
        _ (scheduler/run-session! executor sid {:op :echo :text "abc"})
        events (event/events-for-session (:sqlite (:stores executor)) sid)
        seqs (fn [type] (mapv :event/seq (filter #(= type (:event/type %)) events)))]
    (testing "every event is causally chained to the previous one (a linear, append-before-advance log)"
      (doseq [[prev cur] (partition 2 1 events)]
        (is (= (:event/id prev) (:cause/event-id cur))
            (str "event " (:event/seq cur) " must cause " (:event/seq prev)))))
    (testing "each step's full block of events strictly precedes the next step's :node/started"
      ;; step 1 block = seqs 3-8, step 2 block = seqs 9-14, step 3 block = seqs 15-16
      (is (= [3 9 15] (seqs :node/started)))
      (is (= [4 10 16] (seqs :node/completed)))
      (is (= [5 11] (seqs :intent/proposed)))
      (is (= [6 12] (seqs :intent/authorized)))
      (is (= [7 13] (seqs :provider/call-started)))
      (is (= [8 14] (seqs :provider/call-completed)))
      (is (= [17] (seqs :session/completed)))
      ;; the last result event of step N (seq 8/14) precedes the next :node/started
      (is (< 8 (second (seqs :node/started))))
      (is (< 14 (nth (seqs :node/started) 2))))))

;; ============================================================================
;; Step 4 — unhandled node failure fails the session and preserves the artifact
;; ============================================================================

(deftest step-4-unhandled-node-failure-fails-the-session-with-the-error-artifact
  (let [{:keys [executor executions]} (build-executor (boom-topology))
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:op :echo :text "abc"})
        events (event/events-for-session (:sqlite (:stores executor)) sid)
        session-failed (last events)
        node-failed (second (drop 2 events))
        error-ref (get-in session-failed [:metadata :error/artifact-ref])]
    (testing "the session fails and no provider ever ran (the failure precedes any intent)"
      (is (= :failed (:status result)))
      (is (= :failed (:state (session/get-session (:sqlite (:stores executor)) sid))))
      (is (= 0 @executions)))
    (testing "the trace is node/started → node/failed → session/failed"
      (is (= [:session/created :session/started
              :node/started :node/failed :session/failed]
             (mapv :event/type events))))
    (testing "the node/failed event names the node and the error type"
      (is (= :node/boom (get-in node-failed [:metadata :node/id])))
      (is (= :test/boom (get-in node-failed [:metadata :error/type]))))
    (testing "the error payload survives as a CAS artifact referenced in the event metadata"
      (is (re-matches #"^sha256:[0-9a-f]{64}$" error-ref))
      (is (= error-ref (:error/artifact-ref result)))
      (is (= error-ref (:payload-ref session-failed)))
      (is (= error-ref (:payload-ref node-failed)))
      (is (cas/exists? (:cas (:stores executor)) error-ref))
      (let [error-map (artifact-edn executor error-ref)]
        (is (= :test/boom (:error/type error-map)))
        (is (string? (:error/message error-map)))))))

;; ============================================================================
;; supplementary — denied and failed intents are persisted; the session continues
;; ============================================================================

(deftest denied-and-failed-intents-are-persisted-and-the-session-continues
  (let [{:keys [executor executions]} (build-executor (tool-only-topology) false)
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:text "abc"})
        events (event/events-for-session (:sqlite (:stores executor)) sid)
        denied (nth events 5)]
    (testing "a denied intent never reaches a provider; the session still completes"
      (is (= :completed (:status result)))
      (is (= 0 @executions))
      (is (= [:session/created :session/started
              :node/started :node/completed
              :intent/proposed :intent/denied
              :node/started :node/completed
              :session/completed]
             (mapv :event/type events))))
    (testing "the :intent/denied event records the broker's reason"
      (is (= :capability/denied (get-in denied [:metadata :error/type])))
      (is (= :capability/missing (get-in denied [:metadata :reason])))
      (is (uuid? (get-in denied [:metadata :intent/id]))))
    (testing "with nothing fed back, the session completes with empty outputs"
      (is (nil? (:output-ref result))))))

;; ============================================================================
;; supplementary — a dispatch failure (unknown tool) is persisted as
;; :intent/failed and the session continues (closes the component review
;; gap: the non-denial dispatch-error branch was untested)
;; ============================================================================

(deftest unknown-tool-intent-fails-dispatch-and-the-session-continues
  (let [topology {:graph/id :graph/ghost-tool
                  :entry :node/tool
                  :nodes
                  {:node/tool {:node/type :tool :tool :fixture/ghost :next :node/emit}
                   :node/emit {:node/type :emit}}
                  :limits {:max-steps 64}}
        {:keys [executor executions]} (build-executor topology)
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:text "abc"})
        events (event/events-for-session (:sqlite (:stores executor)) sid)
        failed (nth events 5)]
    (testing "the session completes; the provider never ran"
      (is (= :completed (:status result)))
      (is (= 0 @executions))
      (is (= [:session/created :session/started
              :node/started :node/completed
              :intent/proposed :intent/failed
              :node/started :node/completed
              :session/completed]
             (mapv :event/type events))))
    (testing "the :intent/failed event records the typed dispatch error"
      (is (= :provider/not-found (get-in failed [:metadata :error/type])))
      (is (uuid? (get-in failed [:metadata :intent/id]))))
    (testing "the dispatch error record survives as a CAS artifact"
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:payload-ref failed)))
      (is (cas/exists? (:cas (:stores executor)) (:payload-ref failed))))))

(deftest declared-capability-without-grant-fails-before-session-start
  (let [{:keys [executor]} (build-executor (tool-only-topology) false)
        executor (update-in executor [:phenotype :compiled]
                            assoc :requested-capabilities #{:tool/call})
        sid (create-pinned-session executor)
        e (try
            (scheduler/run-session! executor sid {:text "abc"})
            nil
            (catch clojure.lang.ExceptionInfo e e))]
    (testing "the runtime enforces Requested ⊆ Granted before execution"
      (is (= :capability/lattice-invalid (:error/type (ex-data e))))
      (is (= :requested-not-granted (:reason (ex-data e)))))
    (testing "the failed preflight leaves the session in :created"
      (is (= :created
             (:state (session/get-session
                      (:sqlite (:stores executor)) sid)))))))

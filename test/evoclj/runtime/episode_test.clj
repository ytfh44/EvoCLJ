(ns evoclj.runtime.episode-test
  "component tests for materialize-episode!.

  materialize-episode! turns a TERMINAL session (the component/6.4
  scheduler leaves sessions :completed, :failed, or :budget-exhausted)
  into an immutable Episode record: one SQLite row in the component
  `episodes` table that REFERENCES the session's causal trace and its
  CAS artifacts instead of copying any payload (Global Constraint 21).

  The three normative scenarios, in the task's numbered order:

  - Step 1: completed AND failed sessions both become episodes —
    failures are evidence, not discarded traces (a budget-exhausted
    session is evidence too: it is a third terminal outcome).
  - Step 2: the episode references the FULL trace range (the root
    :session/created event through the terminal event) and artifacts
    rather than copying every payload — the episode row carries no
    body bytes and the :task-ref resolves in the CAS to the exact
    task input the scheduler persisted on the :session/started event.
  - Step 3: the episode's :generation/id is the session's PINNED
    generation read from the store's session row, even when the
    CURRENT pointer moves to a newer generation before
    materialization.

  Plus the error contract: a :created session and an unknown session
  are rejected before anything is written.

  FIXTURE DESIGN: identical to the scheduler tests (evoclj.runtime.
  scheduler-test) — the executor map is built directly with a custom
  executable topology, so the sessions here are REAL runs whose
  events were persisted by run-session!. The store passed to
  materialize-episode! is the executor's :stores map
  {:sqlite <migrated db> :cas <CAS root>}."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.compiler.topology :as topology]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.episode :as episode]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.store.artifact :as artifact]
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
  (let [p (str (Files/createTempFile "evoclj-episode-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-episode-cas-"
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
    (doseq [[artifact-id media-type]
            [[genome-id "application/octet-stream"]
             [resolution-id "application/edn"]
             [phenotype-id "application/edn"]]]
      (artifact/ensure-artifact! db artifact-id media-type 0))
    (artifact/ensure-genome! db genome-id)
    (sqlite/with-db [conn db]
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
  {:graph/id :graph/episode-fixture
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

(defn- compiled-genome
  "A minimal CompiledGenome value carrying a custom executable
  topology — constructed directly (see the scheduler-test docstring)."
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
     :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id phenotype-id}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 10}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- build-executor
  "Build the executor map for a fixture topology: a live phenotype
  (with :program/route and :program/boom loaded into its isolated SCI
  runtime), the opened sqlite + cas stores, and a broker context
  carrying one :fixture/echo lease for the phenotype."
  [fixture-topology]
  (let [executions (atom 0)
        reg (registry/create-registry)
        _ (registry/register! reg (fixture/echo-provider
                                    {:execution-count executions}))
        usage (atom {})
        leases [(echo-lease)]
        compiled (compiled-genome fixture-topology)
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases leases :usage usage}
             :program-sources (program-sources)})]
    {:executor {:phenotype ph
                :stores {:sqlite (fresh-db) :cas (cas/->cas (temp-cas-dir))}
                :dispatch (dispatch/make-broker-context
                           {:registry reg
                            :leases leases
                            :usage usage})}
     :executions executions}))

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

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil
  when nothing is thrown."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))

;; ============================================================================
;; Step 1 — completed AND failed sessions both become episodes; failures are
;; evidence, not discarded traces
;; ============================================================================

(deftest step-1-completed-sessions-become-episodes
  (let [{:keys [executor]} (build-executor (chain-topology {:max-steps 64}))
        sid (create-pinned-session executor)
        _ (scheduler/run-session! executor sid {:op :echo :text "abc"})
        store (:stores executor)
        ep (episode/materialize-episode! store sid)
        events (event/events-for-session (:sqlite store) sid)
        root (first events)
        terminal (last events)]
    (testing "the episode records the session's full pinned identity"
      (is (uuid? (:episode/id ep)))
      (is (= sid (:session/id ep)))
      (is (= generation-id (:generation/id ep)))
      (is (= genome-id (:genome/id ep)))
      (is (= resolution-id (:resolution/id ep))))
    (testing "the episode references the trace range and the task artifact"
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:task-ref ep)))
      (is (= {:first-event (:event/id root)
              :last-event (:event/id terminal)}
             (:trace ep))))
    (testing "a completed session's episode carries the completed outcome"
      (is (= {:status :completed :score nil} (:outcome ep)))
      (is (= {} (:usage ep))))
    (testing "the episode row is durable and queryable"
      (let [rows (sqlite/query (:sqlite store)
                               ["SELECT * FROM episodes WHERE session_id = ?"
                                (str sid)])]
        (is (= 1 (count rows)))
        (is (= (str (:episode/id ep)) (:id (first rows))))))
    (testing "materializing twice is idempotent — the same episode, one row"
      (is (= (:episode/id ep)
             (:episode/id (episode/materialize-episode! store sid))))
      (is (= 1 (count (sqlite/query (:sqlite store)
                                    ["SELECT * FROM episodes WHERE session_id = ?"
                                     (str sid)])))))))

(deftest step-1-failed-sessions-become-episodes
  (let [{:keys [executor]} (build-executor (boom-topology))
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:op :echo :text "abc"})
        store (:stores executor)
        ep (episode/materialize-episode! store sid)
        events (event/events-for-session (:sqlite store) sid)]
    (testing "the fixture run really failed"
      (is (= :failed (:status result))))
    (testing "a failed session is evidence, not a discarded trace"
      (is (uuid? (:episode/id ep)))
      (is (= sid (:session/id ep)))
      (is (= :failed (:status (:outcome ep))))
      (is (= nil (:score (:outcome ep))))
      (is (= {:first-event (:event/id (first events))
              :last-event (:event/id (last events))}
             (:trace ep)))
      (is (= generation-id (:generation/id ep)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:task-ref ep))))))

(deftest step-1-budget-exhausted-sessions-become-episodes
  (let [{:keys [executor]} (build-executor (chain-topology {:max-steps 2}))
        sid (create-pinned-session executor)
        result (scheduler/run-session! executor sid {:op :echo :text "abc"})
        store (:stores executor)
        ep (episode/materialize-episode! store sid)]
    (testing "a budget-exhausted session is a terminal outcome and becomes an episode"
      (is (= :budget-exhausted (:status result)))
      (is (= :budget-exhausted (:status (:outcome ep))))
      (is (= sid (:session/id ep)))
      (is (= generation-id (:generation/id ep))))))

(deftest non-terminal-and-unknown-sessions-are-rejected
  (let [{:keys [executor]} (build-executor (chain-topology {:max-steps 64}))
        sid (create-pinned-session executor) ;; :created — never run
        store (:stores executor)]
    (testing "a :created session is not evidence yet"
      (is (= :episode/not-terminal
             (thrown-error-type #(episode/materialize-episode! store sid)))))
    (testing "an unknown session id is rejected before anything is written"
      (is (= :episode/session-not-found
             (thrown-error-type
              #(episode/materialize-episode! store (random-uuid))))))
    (testing "a malformed store is rejected"
      (is (= :episode/store-invalid
             (thrown-error-type #(episode/materialize-episode! {} sid))))
      (is (= :episode/store-invalid
             (thrown-error-type #(episode/materialize-episode! nil sid)))))))

;; ============================================================================
;; Step 2 — the episode references the full trace range and artifacts rather
;; than copying every payload
;; ============================================================================

(deftest step-2-episode-references-trace-range-and-artifacts-not-payload-copies
  (let [{:keys [executor]} (build-executor (chain-topology {:max-steps 64}))
        sid (create-pinned-session executor)
        task-input {:op :echo :text "abc"}
        _ (scheduler/run-session! executor sid task-input)
        store (:stores executor)
        ep (episode/materialize-episode! store sid)
        events (event/events-for-session (:sqlite store) sid)
        row (first (sqlite/query (:sqlite store)
                                 ["SELECT * FROM episodes WHERE session_id = ?"
                                  (str sid)]))]
    (testing "the trace range spans the ENTIRE causal chain: root → terminal"
      (is (= 17 (count events)))
      (is (= :session/created (:event/type (first events))))
      (is (= :session/completed (:event/type (last events))))
      (is (= (:event/id (first events)) (get-in ep [:trace :first-event])))
      (is (= (:event/id (last events)) (get-in ep [:trace :last-event]))))
    (testing "the episode map carries only the contract keys — no payload keys"
      (is (= #{:episode/id :session/id :generation/id :genome/id
               :resolution/id :task-ref :trace :outcome :usage}
             (set (keys ep))))
      (is (every? int? [(:first-event (:trace ep)) (:last-event (:trace ep))])))
    (testing "the episode ROW contains no body bytes: the task input and the
              accumulated outputs never appear anywhere in the row"
      (let [row-values (map str [(:id row) (:session_id row)
                                 (:generation_id row) (:genome_id row)
                                 (:resolution_id row) (:task_ref row)
                                 (:first_event_id row) (:last_event_id row)
                                 (:outcome row) (:usage row) (:created_at row)])
            joined (str/join " " row-values)
            outputs [{:action {:intent/type :intent/tool-call
                               :payload {:tool/id :fixture/echo
                                         :args {:text "abc"}}}}
                     {:text "abc"} {:text "abc"}]]
        (is (not (str/includes? joined (pr-str task-input))))
        (is (not (str/includes? joined (pr-str outputs))))))
    (testing "the task artifact is a RESOLVABLE reference, not a copy"
      (is (cas/exists? (:cas store) (:task-ref ep)))
      (is (= task-input (artifact-edn executor (:task-ref ep)))))
    (testing "the traced events reference CAS artifacts for their payloads
              (outputs and errors live in the CAS, never in the episode row)"
      (is (every? #(or (nil? (:payload-ref %))
                       (cas/exists? (:cas store) (:payload-ref %)))
                  events)))))

;; ============================================================================
;; Step 3 — the episode's generation id is the session's PINNED generation,
;; even if the CURRENT pointer changes before materialization
;; ============================================================================

(deftest step-3-episode-uses-the-pinned-generation-after-current-moves
  (let [{:keys [executor]} (build-executor (chain-topology {:max-steps 64}))
        sid (create-pinned-session executor)
        db (:sqlite (:stores executor))
        _ (scheduler/run-session! executor sid {:op :echo :text "abc"})
        ;; CURRENT moves on AFTER the session was pinned: a NEW generation
        ;; becomes the current pointer (the seed generation was pinned
        ;; with :current 0, so the new row is the only current row)
        _ (sqlite/with-db [conn db]
            (jdbc/insert! conn :generations
                          {:id "generation-2"
                           :genome_id genome-id
                           :resolution_id resolution-id
                           :parent_id generation-id
                           :state "active"
                           :current 1
                           :created_at "2025-02-01T00:00:00Z"}))
        _ (is (= 1 (count (sqlite/query db
                                        ["SELECT id FROM generations
                                          WHERE current = 1"]))))
        _ (is (= "generation-2"
                 (:id (first (sqlite/query db
                                           ["SELECT id FROM generations
                                             WHERE current = 1"])))))
        ep (episode/materialize-episode! (:stores executor) sid)
        row (first (sqlite/query db
                                 ["SELECT * FROM episodes WHERE session_id = ?"
                                  (str sid)]))]
    (testing "the episode names the generation the session RAN under, not the
              new CURRENT pointer"
      (is (= generation-id (:generation/id ep)))
      (is (= "generation-1" (:generation_id row))))
    (testing "the session's own pin is unchanged"
      (is (= generation-id
             (:generation/id (session/get-session db sid)))))))

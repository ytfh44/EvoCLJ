(ns evoclj.runtime.e2e-seed-test
  "Task 6.6 — the REAL seed Genome runs end to end (Milestone 6 exit).

  This test drives the whole pipeline through the REAL
  genomes/seed bundle — no test-double CompiledGenome, no inline
  topology:

    load genomes/seed
      → compile with the fixture provider catalog (Resolution)
      → instantiate the Phenotype (isolated SCI runtime)
      → create a pinned session (store)
      → run-session! (scheduler) with a capability lease for
        :fixture/echo granted to the phenotype
      → broker dispatch (the route program's :intent/tool-call)
      → completed session
      → materialize the Episode

  SEED TOPOLOGY (the preferred executable chain, documented in
  genomes/seed/topology.edn): :node/router (:sci, :program/route) →
  :node/tool → :node/emit. The route program (the M3/M4 contract) is
  the seed's decision-maker: for {:op :echo :text \"abc\"} it EMITS the
  typed :intent/tool-call for :fixture/echo, which the scheduler
  dispatches through the broker — that is the session's ONE authorized
  tool call and ONE provider result. The :tool node then requests
  :fixture/non-idempotent — a REGISTERED provider OUTSIDE the granted
  lease — so the broker scope-denies it (:intent/denied
  :capability/scope-denied): the seed demonstrates Global Constraint 9
  (a visible tool never grants resource authority) inside the same
  run, while the granted :fixture/echo call completes exactly once.

  STEP 4 INVARIANTS (normative):
    1. Genome hash unchanged — reloading genomes/seed yields the same
       :genome/id.
    2. Session pin unchanged — the store row's pinned
       (genome, resolution, phenotype) ids never move.
    3. Exactly one :intent/authorized event.
    4. Exactly one :provider/call-completed event (the echo provider
       really ran once; the ungranted tool never reached a provider).
    5. Exactly one completed Episode row.

  STEP 5: the store is reopened from disk (the sqlite store is
  per-operation connections and the CAS is a directory, so \"close +
  reopen\" rebuilds fresh handles over the same paths, exactly like the
  Task 5.5 recovery test) and the episode + trace remain queryable.

  The route program contract's other branches ({:op :finish :value v}
  and anything-else → finish) are asserted against the real loaded
  program via the phenotype's isolated SCI runtime."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.compiler.core :as core]
            [evoclj.genome.load :as load]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.episode :as episode]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.sci.execute :as execute]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- the seed genome --------------------------------------------------------

(defn- seed-root
  "The real seed Genome bundle directory at the repo root
  (genomes/seed). The test runs with the repo root as the working
  directory (clojure -M:test)."
  []
  (let [p (.toPath (io/file "genomes/seed"))]
    (when-not (Files/isDirectory p (make-array LinkOption 0))
      (throw (ex-info "genomes/seed bundle not found (run from the repo root)"
                      {:path (str p)})))
    p))

(defn- route-descriptor
  "The seed route program descriptor (Task 2.3 choice (a): an in-memory
  descriptor list riding on the loaded-genome value under :programs)."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- seed-loaded-genome
  "The REAL genomes/seed bundle loaded from disk with its program
  registry attached."
  []
  (assoc (load/load-genome (seed-root))
         :programs [(route-descriptor)]))

(defn- fixture-catalog
  "The on-disk provider catalog fixture (Task 2.1 Resolution)."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- program-sources
  "Decode every compiled program's source text from the immutable
  loaded bundle :files (the CompiledGenome carries only :source/digest
  references, Global Constraint 22). The compiled :programs map is the
  authoritative program-id -> descriptor source: each descriptor's
  :file is looked up in the loaded bundle."
  [loaded-genome compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (get-in loaded-genome
                                         [:files (:file descriptor) :bytes]))
                        StandardCharsets/UTF_8)]))
        (:programs compiled)))

;; --- temp stores (test temp dirs only) -------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-e2e-seed-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-e2e-seed-cas-"
                                     (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifact trees)."
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

;; --- store / executor assembly (the only \"glue\" this test needs) --------

(def ^:private generation-id "generation-1")

(defn- fresh-db
  "A migrated database backed by a fresh temp file, seeded with the
  generation row sessions are pinned to (current = 1: the seed
  generation IS the CURRENT pointer, Database Invariant 6). Returns
  [db-spec db-path]."
  [genome-id resolution-id]
  (let [path (temp-db-path)
        db (sqlite/spec path)]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    [db path]))

(defn- echo-lease
  "A valid CapabilityLease granting THIS phenotype's exact id the
  :fixture/echo :invoke action for the next minute. Only :fixture/echo
  is granted: :fixture/non-idempotent stays visible-but-ungranted."
  [phenotype-id]
  (let [now (java.util.Date.)]
    {:cap/id (random-uuid)
     :subject {:phenotype/id phenotype-id}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 10}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- build-executor
  "Assemble the Task 6.3 executor map from the REAL seed genome:

    {:phenotype <instantiated Phenotype>
     :stores {:sqlite <migrated db> :cas <CAS root>}
     :dispatch <broker context>}

  The runtime provider REGISTRY registers both fixture providers
  (:fixture/echo and :fixture/non-idempotent); the broker carries ONE
  lease (for :fixture/echo). Returns {:executor ... :executions ...
  :db-path ... :cas-root ...} where :executions counts real provider
  executions and :db-path/:cas-root are the on-disk handles the
  restart step reopens."
  []
  (let [loaded (seed-loaded-genome)
        compiled (core/compile-genome loaded (fixture-catalog))
        genome-id (:compiled/genome-id compiled)
        resolution-id (:compiled/resolution-id compiled)
        phenotype-id (:compiled/phenotype-id compiled)
        executions (atom 0)
        reg (registry/create-registry)
        _ (registry/register! reg (fixture/echo-provider
                                   {:execution-count executions}))
        _ (registry/register! reg (fixture/non-idempotent-provider))
        usage (atom {})
        lease (echo-lease phenotype-id)
        [db db-path] (fresh-db genome-id resolution-id)
        cas-root (temp-cas-dir)
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases [lease] :usage usage}
             :program-sources (program-sources loaded compiled)})]
    {:executor {:phenotype ph
                :stores {:sqlite db :cas (cas/->cas cas-root)}
                :dispatch (dispatch/make-broker-context
                           {:registry reg
                            :leases [lease]
                            :usage usage})}
     :executions executions
     :db-path db-path
     :cas-root cas-root
     :compiled compiled
     :phenotype ph
     :lease lease}))

(defn- create-pinned-session
  "create-session! pinned to the seed's compiled identity, then append
  the :session/created root event (the host's job — the scheduler
  anchors its causal chain on it). Returns the session id."
  [executor compiled]
  (let [db (:sqlite (:stores executor))
        sid (:session/id
             (session/create-session!
              db
              {:genome/id (:compiled/genome-id compiled)
               :resolution/id (:compiled/resolution-id compiled)
               :phenotype/id (:compiled/phenotype-id compiled)
               :generation/id generation-id}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id generation-id
                          :phenotype/id (:compiled/phenotype-id compiled)
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

(defn- artifact-edn
  "Read a CAS artifact back as EDN data."
  [store artifact-id]
  (edn/read-string
   (String. (cas/get-bytes (:cas store) artifact-id)
            StandardCharsets/UTF_8)))

;; ============================================================================
;; Step 1/4/5 — the whole pipeline: load → compile → instantiate → session →
;; scheduler → broker → event store → episode, then restart
;; ============================================================================

(deftest seed-genome-runs-end-to-end-with-the-normative-invariants
  (let [{:keys [executor executions db-path cas-root compiled lease]}
        (build-executor)
        db (:sqlite (:stores executor))
        store (:stores executor)]
    (testing "Step 4 invariant 1 — the Genome hash is unchanged on reload"
      (let [g1 (seed-loaded-genome)
            g2 (seed-loaded-genome)]
        (is (re-matches #"^sha256:[0-9a-f]{64}$" (:genome/id g1)))
        (is (= (:genome/id g1) (:genome/id g2))
            "reloading genomes/seed must yield the same content address")
        (is (= (:genome/id g1) (:compiled/genome-id compiled))
            "the compiled genome names the loaded bundle's address")))
    (testing "the phenotype instantiated from the real seed carries its ids"
      (is (= (:compiled/phenotype-id compiled) (:phenotype/id (:phenotype executor))))
      (is (= :node/router (get-in compiled [:topology :entry])))
      (let [files (set (keys (:files (seed-loaded-genome))))]
        (is (contains? files "programs/route.clj"))
        (is (every? files ["manifest.edn" "topology.edn" "models.edn"
                           "memory.edn" "evolution.edn"]))
        (is (= 6 (count files)))))
    (let [sid (create-pinned-session executor compiled)
          result (scheduler/run-session! executor sid {:op :echo :text "abc"})
          events (event/events-for-session db sid)
          by-type (group-by :event/type events)]
      (testing "the session completes with the accumulated outputs artifact"
        (is (= :completed (:status result)))
        (is (= sid (:session/id result)))
        (is (re-matches #"^sha256:[0-9a-f]{64}$" (:output-ref result)))
        (is (nil? (:error/artifact-ref result)))
        (is (nil? (:episode/id result)))
        (is (= 14 (:event/count result))))
      (testing "Step 4 invariant 3 — exactly one intent/authorized event"
        (is (= 1 (count (:intent/authorized by-type))))
        (let [auth (get-in (first (:intent/authorized by-type))
                           [:metadata :authorization])]
          (is (= :allow (:decision auth)))
          (is (uuid? (:lease-id auth)))
          (is (= (:cap/id lease) (:lease-id auth))
              "the authorization names the host's granted :fixture/echo lease")))
      (testing "Step 4 invariant 4 — exactly one provider call-completed event"
        (is (= 1 (count (:provider/call-completed by-type))))
        (is (= 1 @executions)
            "the granted :fixture/echo provider really ran exactly once"))
      (testing "the ungranted :tool node request is scope-denied (Global Constraint 9)"
        (let [proposed-seq (:intent/proposed by-type)
              denied (first (:intent/denied by-type))
              denied-proposed (first (filter #(= (get-in denied
                                                     [:metadata :intent/id])
                                                 (get-in %
                                                        [:metadata :intent/id]))
                                             proposed-seq))]
          (is (= 1 (count (:intent/denied by-type))))
          (is (= :capability/scope-denied (get-in denied [:metadata :reason])))
          (is (= :intent/tool-call (get-in denied [:metadata :intent/type])))
          (is (= denied-proposed (second proposed-seq))
              "the denied request is the session's SECOND tool call (the :tool node's)")
          (is (= :node/tool (get-in denied-proposed [:metadata :node/id]))
              "the denied request is the :tool node's (ungranted) tool call"))
        (is (= 1 @executions)
            "a denied intent never reaches a provider"))
      (testing "the persisted trace is the seed's exact executable chain"
        (is (= [:session/created :session/started
                :node/started :node/completed
                :intent/proposed :intent/authorized
                :provider/call-started :provider/call-completed
                :node/started :node/completed
                :intent/proposed :intent/denied
                :node/started :node/completed
                :session/completed]
               (mapv :event/type events))))
      (testing "the outputs artifact holds the router's decision and the echo result"
        (is (= [{:action {:intent/type :intent/tool-call
                          :payload {:tool/id :fixture/echo
                                    :args {:text "abc"}}}}
                {:text "abc"}]
               (artifact-edn store (:output-ref result)))))
      (testing "the :session/started event persists the task input as a CAS artifact"
        (is (= {:op :echo :text "abc"}
               (artifact-edn store (get-in events [1 :payload-ref])))))
      (testing "Step 4 invariant 2 — the session pin is unchanged in the store row"
        (let [row (first (sqlite/query db
                                       ["SELECT genome_id, resolution_id, phenotype_id, generation_id
                                         FROM sessions WHERE id = ?" (str sid)]))]
          (is (= (:compiled/genome-id compiled) (:genome_id row)))
          (is (= (:compiled/resolution-id compiled) (:resolution_id row)))
          (is (= (:compiled/phenotype-id compiled) (:phenotype_id row)))
          (is (= generation-id (:generation_id row)))))
      (testing "the append-only chain verifies end to end"
        (is (:valid? (event/verify-event-chain db sid))))
      (testing "Step 4 invariant 5 — exactly one completed Episode"
        (let [ep (episode/materialize-episode! store sid)]
          (is (uuid? (:episode/id ep)))
          (is (= sid (:session/id ep)))
          (is (= generation-id (:generation/id ep)))
          (is (= (:compiled/genome-id compiled) (:genome/id ep)))
          (is (= (:compiled/resolution-id compiled) (:resolution/id ep)))
          (is (= {:status :completed :score nil} (:outcome ep)))
          (is (= {:first-event (:event/id (first events))
                  :last-event (:event/id (last events))}
                 (:trace ep)))
          (is (= {:op :echo :text "abc"}
                 (artifact-edn store (:task-ref ep))))
          (is (= 1 (count (sqlite/query db
                                        ["SELECT * FROM episodes WHERE session_id = ?"
                                         (str sid)]))))
          (testing "materializing twice is idempotent — one row, one episode"
            (is (= (:episode/id ep)
                   (:episode/id (episode/materialize-episode! store sid))))
            (is (= 1 (count (sqlite/query db
                                          ["SELECT * FROM episodes WHERE session_id = ?"
                                           (str sid)])))))))
      (testing "Step 5 — close and REOPEN the store from disk; the episode and
                trace remain queryable"
        (let [reopened-db (sqlite/spec db-path)
              _ (is (= {:status :noop :version 1}
                       (migrate/migrate! reopened-db)))
              reopened-cas (cas/->cas cas-root)
              reopened-store {:sqlite reopened-db :cas reopened-cas}
              ep (episode/materialize-episode! reopened-store sid)
              reopened-events (event/events-for-session reopened-db sid)]
          (is (uuid? (:episode/id ep)))
          (is (= sid (:session/id ep)))
          (is (= (:compiled/genome-id compiled) (:genome/id ep)))
          (is (= 1 (count (sqlite/query reopened-db
                                        ["SELECT * FROM episodes WHERE session_id = ?"
                                         (str sid)]))))
          (is (= 15 (count reopened-events))
              "the full causal trace survives the restart")
          (is (= :session/completed (:event/type (last reopened-events))))
          (is (= 1 (count (filter #(= :provider/call-completed (:event/type %))
                                  reopened-events))))
          (is (:valid? (event/verify-event-chain reopened-db sid))
              "the append-only chain re-verifies after the restart")
          (testing "the session row and its pin survive the restart"
            (let [s (session/get-session reopened-db sid)]
              (is (= :completed (:state s)))
              (is (= (:compiled/genome-id compiled) (:genome/id s)))
              (is (= (:compiled/resolution-id compiled) (:resolution/id s)))
              (is (= (:compiled/phenotype-id compiled) (:phenotype/id s))))))))))

;; ============================================================================
;; The route program contract (the rest of the M3/M4 decision table)
;; ============================================================================

(deftest seed-route-program-contract-decisions
  (let [{:keys [phenotype]} (build-executor)
        invoke (fn [input]
                 (select-keys
                  (execute/invoke! (:sci-runtime phenotype)
                                   :program/route input nil)
                  [:status :value]))]
    (testing "{:op :echo :text t} routes to the :fixture/echo tool call"
      (is (= {:status :ok
              :value {:action {:intent/type :intent/tool-call
                               :payload {:tool/id :fixture/echo
                                         :args {:text "abc"}}}}}
             (invoke {:op :echo :text "abc"}))))
    (testing "{:op :finish :value v} routes to the finish intent"
      (is (= {:status :ok
              :value {:action {:intent/type :intent/finish
                               :payload {:value "done"}}}}
             (invoke {:op :finish :value "done"}))))
    (testing "anything else routes to finish with the whole input"
      (let [input {:unexpected :payload :n 1}]
        (is (= {:status :ok
                :value {:action {:intent/type :intent/finish
                                 :payload {:value input}}}}
               (invoke input)))))
    (testing "invoke! reports deterministic usage metadata (SCI contract)"
      (let [res (execute/invoke! (:sci-runtime phenotype)
                                 :program/route {:op :echo :text "x"} nil)]
        (is (= :ok (:status res)))
        (is (map? (:usage res)))
        (is (pos-int? (get-in res [:usage :steps])))))))

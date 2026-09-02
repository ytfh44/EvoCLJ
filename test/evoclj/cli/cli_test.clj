(ns evoclj.cli.cli-test
  "component — the CLI read/execute commands, tested in-process.

  Every test drives the CLI through its PUBLIC entry points
  (evoclj.cli.main/execute and evoclj.cli.main/run) with an args
  vector and a temp --state-dir, exactly as an operator would invoke
  `clojure -M -m evoclj.cli.main ...` (Step 1-4 of the task).

  The fixture store is provisioned the way a real host deployment
  provisions it — through the public subsystem APIs and host
  bookkeeping, never through the CLI:

    - the SQLite store is migrated and seeded with the generation-1
      row (current = 1, Database Invariant 6);
    - the G1 Genome (test/fixtures/evolution-e2e/route-a) is stored in
      the CAS under its content address (Invariant 7) AND copied into
      <state-dir>/genomes/<id-as-dash> (the CLI's genome-store
      convention);
    - sessions/candidates/evaluations are created through the public
      store/evolution APIs.

  Step 2's guarantees are asserted two ways: BY CONSTRUCTION (the cli
  namespaces contain no SQL write statements, use no raw JDBC, and
  never depend on the promotion CURRENT machinery) and BEHAVIORALLY
  (the cli evolve/promote commands call
  evolution.core/propose-candidates! / promotion.promote/promote! —
  with-redefs record the calls — and the CURRENT pointer moves only
  as a consequence)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.cli.main :as main]
            [evoclj.cli.session :as cli-session]
            [evoclj.compiler.core :as compiler]
            [evoclj.eval.replay :as replay]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.evolution.core :as evolution-core]
            [evoclj.store.candidate-store :as candidate-store]
            [evoclj.genome.load :as load]
            [evoclj.genome.path :as gpath]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.promotion.current :as current]
            [evoclj.promotion.promote :as promote]
            [evoclj.runtime.episode :as episode]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.existence :as existence]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; ============================================================================
;; fixture identity
;; ============================================================================

(def ^:private generation-id "generation-1")

(defn- route-a-root [] (str (io/file "test" "fixtures" "evolution-e2e" "route-a")))
(defn- route-b-root [] (str (io/file "test" "fixtures" "evolution-e2e" "route-b")))
(defn- selection-root [] (str (io/file "test" "fixtures" "evolution-e2e" "selection")))

(defn- route-descriptor []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- fixture-catalog []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- fake-sha [] (str "sha256:" (apply str (repeat 64 "0"))))

(defn- dash [id] (str/replace id ":" "-"))

;; ============================================================================
;; temp state-dir plumbing
;; ============================================================================

(def ^:private temp-paths (atom []))

(defn- temp-dir
  [prefix]
  (let [d (str (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (swap! temp-paths conj d)
    d))

(defn- delete-tree!
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup! []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- copy-tree!
  "Recursively copy the bundle at `src` to `dest` (the CLI's genome
  store must hold REAL files; load-genome rejects symlinks)."
  [src dest]
  (let [from (Paths/get src (make-array String 0))
        to (Paths/get dest (make-array String 0))]
    (with-open [stream (Files/walk from (make-array FileVisitOption 0))]
      (doseq [p (iterator-seq (.iterator stream))]
        (let [rel (.relativize from p)
              target (.resolve to rel)]
          (when (Files/isDirectory p (make-array LinkOption 0))
            (Files/createDirectories target (make-array FileAttribute 0)))
          (when (Files/isRegularFile p (make-array LinkOption 0))
            (Files/createDirectories (.getParent target) (make-array FileAttribute 0))
            (Files/copy p target (make-array java.nio.file.CopyOption 0))))))))

(defn- genome-index-bytes
  "The canonical index bytes whose SHA-256 is the genome's content
  address (the exact serialization of evoclj.genome.hash/tree-digest)."
  [loaded]
  (apply str
         (map (fn [[p {:keys [digest]}]]
                (str p "\u0000" digest "\n"))
              (sort-by (fn [[p _]] p) gpath/bytewise-compare (:files loaded)))))

(defn- program-sources
  [loaded compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (get-in loaded [:files (:file descriptor) :bytes]))
                        StandardCharsets/UTF_8)]))
        (:programs compiled)))

(defn- compile-route-a
  "G1 loaded + compiled with the route program registry."
  []
  (let [loaded (assoc (load/load-genome (route-a-root))
                      :programs [(route-descriptor)])]
    {:loaded loaded
     :compiled (compiler/compile-genome loaded (fixture-catalog))}))

(defn- compile-bundle
  [bundle-root]
  (let [loaded (assoc (load/load-genome bundle-root)
                      :programs [(route-descriptor)])]
    {:loaded loaded
     :compiled (compiler/compile-genome loaded (fixture-catalog))}))

(defn- provision!
  "Create a temp state dir provisioned like a real host deployment:
  migrated db, the generation-1 row (current = 1), G1's canonical body
  in the CAS, and the G1 bundle at <state-dir>/genomes/<id-as-dash>.
  Returns a context map."
  []
  (let [dir (temp-dir "evoclj-cli-state-")
        _ (Files/createDirectories (Paths/get (str dir "/db") (make-array String 0))
                                   (make-array FileAttribute 0))
        db-path (str dir "/db/evoclj.db")
        db (sqlite/spec db-path)
        _ (migrate/migrate! db)
        {:keys [loaded compiled]} (compile-route-a)
        genome-id (:compiled/genome-id compiled)
        resolution-id (:compiled/resolution-id compiled)
        cas-root (str dir "/cas")
        cas-store (cas/->cas cas-root)]
    (sqlite/with-db [conn db]
      (doseq [artifact-id [genome-id resolution-id (:compiled/phenotype-id compiled)]]
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
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    (cas/put-bytes! cas-store
                    (.getBytes (genome-index-bytes loaded) StandardCharsets/UTF_8)
                    {})
    (copy-tree! (route-a-root) (str dir "/genomes/" (dash genome-id)))
    {:state-dir dir :db db :db-path db-path :cas-root cas-root
     :cas-store cas-store :loaded loaded :compiled compiled
     :genome-id genome-id :resolution-id resolution-id}))

(defn- store-genome-body!
  "Store a loaded Genome's canonical body in the CAS under its content
  address (Database Invariant 7 — required for lineage/promotion
  integrity checks)."
  [cas-store loaded]
  (:artifact/id
   (cas/put-bytes! cas-store
                   (.getBytes (genome-index-bytes loaded) StandardCharsets/UTF_8)
                   {})))

(defn- proof [id]
  (#'existence/unsafe-verified-digest id))

(defn- proof-candidate [candidate]
  (update candidate :candidate/genome-id proof))

(defn- proof-mutation [mutation]
  (cond-> mutation
    (:parent/genome-id mutation) (update :parent/genome-id proof)
    (:evidence/id mutation) (update :evidence/id proof)))

(defn- read-artifact
  [store artifact-id]
  (edn/read-string
   (String. (cas/get-bytes (:cas store) artifact-id) StandardCharsets/UTF_8)))

;; ============================================================================
;; a real G1 session through the public runtime APIs (fixture for the
;; read-only replay/events/capability commands)
;; ============================================================================

(defn- echo-lease
  [phenotype-id]
  (let [now (java.util.Date.)]
    {:cap/id (random-uuid)
     :principal {:principal/type :session :session/id #uuid "00000000-0000-4000-a000-000000000000"}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 100}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- run-fixture-session!
  "Run one G1 session through the REAL pipeline using only public APIs
  and return {:session/id ... :result ...}. Used to build the fixture
  store the read-only commands read."
  [ctx task]
  (let [{:keys [db cas-store compiled loaded]} ctx
        reg (registry/create-registry)
        _ (registry/register! reg (fixture/echo-provider {}))
        _ (registry/register! reg (fixture/non-idempotent-provider {}))
        usage (atom {})
        lease (echo-lease (:compiled/phenotype-id compiled))
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases [lease] :usage usage}
             :program-sources (program-sources loaded compiled)})
        executor {:phenotype ph
                  :stores {:sqlite db :cas cas-store}
                  :dispatch (dispatch/make-broker-context
                             {:registry reg :leases [lease] :usage usage})}
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
                          :prev/event-id nil
                          :payload-ref nil
                          :metadata {}})
    (let [result (scheduler/run-session! executor sid task)
          ep (episode/materialize-episode! {:sqlite db :cas cas-store} sid)]
      {:session/id sid :result result :executor executor})))

(defn- replay-case-from-session
  "Build the G4 replay case from a REAL recorded G1 session: the task
  input, the recorded route decision, and the recorded provider
  response read back from the CAS."
  [ctx sid task expected-output]
  (let [store {:sqlite (:db ctx) :cas (:cas-store ctx)}
        events (event/events-for-session (:db ctx) sid)
        completed (first (filter #(= :provider/call-completed (:event/type %))
                                 events))
        response (read-artifact store (:payload-ref completed))
        decision (first expected-output)
        payload (get-in decision [:action :payload])]
    (replay/build-replay-case
     {:episode/id (random-uuid)
      :outcome {:status :completed}}
     [{:intent/type :intent/tool-call
       :effect :read
       :payload payload
       :response response}]
     {:case/id :replay/a
      :task-input task
      :expected-output expected-output
      :mode :fixture})))

;; ============================================================================
;; evolution fixture: the deterministic mutator (mirrors the component
;; e2e fixture) + the hidden selection cases
;; ============================================================================

(defn- deterministic-uuid [s]
  (UUID/nameUUIDFromBytes (.getBytes s StandardCharsets/UTF_8)))

(defn- echo-b-provider
  []
  (reify proto/Provider
    (describe [_]
      {:tool/id :fixture/echo-b
       :effect :pure
       :input-schema [:map [:text :string]]
       :output-schema [:map [:text :string]]
       :required-action :invoke
       :retry {:safe? true}})
    (normalize-request [_ intent]
      (let [args (get-in intent [:payload :args])]
        (when-not (map? args)
          (throw (ex-info "tool-call payload must carry an :args map"
                          {:error/type :provider/input-invalid})))
        {:tool/id :fixture/echo-b
         :resource {:kind :tool :id :fixture/echo-b}
         :args args}))
    (execute-request! [_ authorized-request]
      {:text (get-in authorized-request [:args :text])})))

(defn- g2-case-form
  []
  (list 'case 'op
        :echo-a {:action (list 'tool-call-intent :fixture/echo
                               {:text (list 'get 'input :text)})}
        :echo-b {:action (list 'tool-call-intent :fixture/echo-b
                               {:text (list 'get 'input :text)})}
        {:action (list 'finish-intent 'input)}))

(defn- route-replacement-op
  [parent-genome form]
  {:op :replace-form
   :file "programs/route.clj"
   :selector ['case]
   :expect/hash (get-in parent-genome [:files "programs/route.clj" :digest])
   :form form})

(defn- delta-mutation
  [parent diagnosis hypothesis form suffix]
  (let [content {:parent/genome-id (:genome/id parent)
                 :hypothesis/id (:hypothesis/id hypothesis)
                 :evidence/id (:evidence/id diagnosis)
                 :risk :program
                 :ops [(route-replacement-op parent form)]
                 :expected-effect {:primary-metric :task/success
                                   :direction :increase}}]
    (assoc content
           :mutation/id (deterministic-uuid (pr-str [content suffix])))))

(defn- propose-deltas
  [ctx]
  (when-let [hypothesis (some #(when (= :task/success (:pattern %)) %)
                              (:hypotheses (:diagnosis ctx)))]
    (let [parent (:parent-genome ctx)
          diagnosis (:diagnosis ctx)]
      [(delta-mutation parent diagnosis hypothesis (g2-case-form) "g2")])))

(defn- recording-mutator
  [captured]
  (reify evolution-core/Mutator
    (propose-mutations [_ context]
      (reset! captured context)
      (propose-deltas context))))

(defn- selection-cases
  "The hidden Selection case bodies from the fixture files (the
  evaluator-only surface; the CLI ships none — hosts inject them)."
  []
  (into {}
        (map (fn [f]
               (let [c (edn/read-string (slurp f))]
                 [(:case/id c) c])))
        [(io/file (selection-root) "sel-a.edn")
         (io/file (selection-root) "sel-b.edn")]))

(defn- eval-overrides
  "The :overrides seam the CLI's build-config accepts: the evaluator's
  hidden cases and fixture providers (host-injected, exactly as a real
  deployment injects them)."
  [replay-case]
  {:eval/system
   {:selection/cases (selection-cases)
    :selection/fixtures
    {:fixture/echo (fn [_seed] (fixture/echo-provider {}))
     :fixture/echo-b (fn [_seed] (echo-b-provider))}
    :replay/cases {:replay/a replay-case}
    :replay/fixtures
    {:fixture/echo (fn [] (fixture/echo-provider {}))
     :fixture/echo-b (fn [] (echo-b-provider))}}})

(defn- task-file!
  "Write an EDN task input to a temp file and return its path."
  [task]
  (let [f (java.io.File/createTempFile "evoclj-cli-task-" ".edn")]
    (swap! temp-paths conj (.getPath f))
    (spit f (pr-str task))
    (.getPath f)))

;; ============================================================================
;; STEP 1 — read-only commands against fixture stores
;; ============================================================================

(deftest cli-read-only-commands-against-fixture-store
  (let [ctx (provision!)
        dir (:state-dir ctx)
        g1-id (:genome-id ctx)
        task {:op :echo-a :text "hi"}
        expected [{:action {:intent/type :intent/tool-call
                            :payload {:tool/id :fixture/echo
                                      :args {:text "hi"}}}}
                  {:text "hi"}]
        run (run-fixture-session! ctx task)
        sid (:session/id run)]
    (testing "genome validate <path> — the real G1 bundle validates"
      (let [{:keys [exit data]} (main/execute ["genome" "validate" (route-a-root)]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (true? (:valid? data)))
        (is (= g1-id (:genome/id data)))
        (is (= 6 (:files data)))))
    (testing "genome inspect <path> and <id> — both resolve"
      (let [{:keys [exit data]} (main/execute ["genome" "inspect" (route-a-root)]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= g1-id (:genome/id data)))
        (is (contains? (:files data) "programs/route.clj"))
        (is (not (contains? (get-in data [:files "programs/route.clj"]) :bytes))
            "inspect strips the raw payload bytes"))
      (let [{:keys [exit data]} (main/execute ["genome" "inspect" g1-id]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= g1-id (:genome/id data)))
        (is (= "programs/route.clj"
               (get-in data [:files "programs/route.clj" :path])))))
    (testing "genome diff <left> <right> — route-a vs route-b, and identical"
      (let [{:keys [exit data]} (main/execute ["genome" "diff" (route-a-root)
                                               (route-b-root)]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= g1-id (:left/genome-id data)))
        (is (false? (:identical? data)))
        (is (= ["manifest.edn" "programs/route.clj" "topology.edn"]
               (mapv :path (:changed data))))
        (let [{:keys [exit data]} (main/execute ["genome" "diff" (route-a-root)
                                                 (route-a-root)]
                                                {:state-dir dir})]
          (is (= 0 exit))
          (is (true? (:identical? data)))))
      (testing "diff by content address too"
        (let [{:keys [exit data]} (main/execute ["genome" "diff" g1-id g1-id]
                                                {:state-dir dir})]
          (is (= 0 exit))
          (is (true? (:identical? data))))))
    (testing "lineage <generation-id> — the seed generation"
      (let [{:keys [exit data]} (main/execute ["lineage" generation-id]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= generation-id (get-in data [:generation :generation/id])))
        (is (= g1-id (get-in data [:generation :genome/id])))
        (is (nil? (get-in data [:mutation])) "the seed has no mutation")
        (is (empty? (:children data)))))
    (testing "replay --session <uuid> — episode + task + output from the store"
      (let [{:keys [exit data]} (main/execute ["replay" "--session" (str sid)]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= sid (:session/id data)))
        (is (= task (:task-input data)))
        (is (= expected (:output data)))
        (is (= :completed (get-in data [:episode :outcome :status])))))
    (testing "events --session <uuid> — the full causal trace"
      (let [{:keys [exit data]} (main/execute ["events" "--session" (str sid)]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= sid (:session/id data)))
        (is (= :session/created (get-in data [:events 0 :event/type])))
        (is (= :session/completed (get-in data [:events (dec (count (:events data)))
                                                :event/type])))))
    (testing "capability inspect --session <uuid> — attributable capability facts"
      (let [{:keys [exit data]} (main/execute ["capability" "inspect"
                                               "--session" (str sid)]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= sid (:session/id data)))
        (is (= 1 (count (:capabilities/authorized data))))
        (is (= :allow (get-in data [:capabilities/authorized 0 :decision])))
        (is (empty? (:capabilities/denied data)))
        (is (= (:genome-id ctx) (get-in data [:session :genome/id])))))
    (testing "candidate list / candidate inspect — an empty fixture lists []"
      (let [{:keys [exit data]} (main/execute ["candidate" "list"]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= [] (:candidates data))))
      (let [{:keys [exit data]} (main/execute ["candidate" "inspect"
                                               (str (random-uuid))]
                                              {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/candidate-not-found (:error/type data)))))))

;; ============================================================================
;; STEP 2 — mutating commands call the public Promotion/Evolution APIs and
;; cannot directly update SQL current-pointer rows
;; ============================================================================

(deftest cli-namespaces-never-write-sql
  (testing "by construction: the cli namespaces contain no SQL writes,
            no raw JDBC, and no dependency on the CURRENT machinery"
    (doseq [file ["evoclj/cli/main.clj" "evoclj/cli/genome.clj"
                  "evoclj/cli/session.clj" "evoclj/cli/evolution.clj"
                  "evoclj/cli/promotion.clj"]]
      (let [src (slurp (io/resource file))]
        (is (not (re-find #"(?i)(?:insert|update|delete)\s+(?:into|set|from)\b" src))
            (str file " contains no SQL write statements"))
        (is (not (re-find #"clojure\.java\.jdbc|java\.sql" src))
            (str file " uses no raw JDBC"))))
    (doseq [sym '[evoclj.cli.main evoclj.cli.genome evoclj.cli.session
                  evoclj.cli.evolution evoclj.cli.promotion]]
      (require sym)
      (is (nil? (get (ns-aliases sym) 'current))
          (str sym " never depends on the promotion CURRENT machinery"))
      (is (nil? (get (ns-aliases sym) 'promotion-current))
          (str sym " never depends on the promotion CURRENT machinery")))))

(deftest cli-evolve-promote-call-the-public-apis
  (let [ctx (provision!)
        dir (:state-dir ctx)
        captured (atom nil)]
    (testing "evolve --generation current calls evolution.core/propose-candidates!"
      (let [calls (atom [])]
        (with-redefs [evolution-core/propose-candidates!
                      (fn [system request]
                        (swap! calls conj {:system system :request request})
                        [])]
          (let [{:keys [exit data]} (main/execute
                                     ["evolve" "--generation" "current"]
                                     {:state-dir dir
                                      :overrides {:evolution/system
                                                  {:mutator (fn [_] [])}}})]
            (is (= 0 exit))
            (is (= [] (:candidates data)))
            (is (= 1 (count @calls)))
            (is (= generation-id (get-in @calls [0 :request :generation/id])))
            (is (fn? (get-in @calls [0 :system :genome-loader]))
                "the evolution-system the CLI hands over carries a generation loader")))))
    (testing "a real evolve with a no-op mutator proposes nothing and the
              CURRENT pointer does not move (evolution never activates)"
      (let [{:keys [exit data]} (main/execute ["evolve" "--generation" "current"]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= [] (:candidates data)))
        (is (= generation-id (:id (current/current-generation (:db ctx))))
            "the CURRENT pointer still names generation-1 after the cycle")))
    (testing "promote <candidate-id> --evaluation <id> calls
              promotion.promote/promote! with the candidate's lineage"
      ;; a real :evaluation-pending candidate + finalized evaluation,
      ;; fabricated through the public APIs + host bookkeeping (the
      ;; test, not the CLI, owns the SQL fixture writes)
      (let [route-b (assoc (load/load-genome (route-b-root))
                           :programs [(route-descriptor)])
            g2-id (:genome/id route-b)
            _ (store-genome-body! (:cas-store ctx) route-b)
            _ (copy-tree! (route-b-root)
                          (str dir "/candidates/" (dash g2-id)))
            pack-ref (:artifact/id
                      (cas/put-bytes! (:cas-store ctx)
                                      (.getBytes (pr-str {:generation/id generation-id
                                                          :summary {:selected 1}})
                                                 StandardCharsets/UTF_8)
                                      {}))
            _ (sqlite/with-db [conn (:db ctx)]
                (doseq [artifact-id [g2-id pack-ref]]
                  (jdbc/execute!
                   conn
                   ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at)
                     VALUES (?, 'application/octet-stream', 0, datetime('now'))"
                    artifact-id]))
                (jdbc/execute!
                 conn
                 ["INSERT OR IGNORE INTO genomes (id, created_at)
                  VALUES (?, datetime('now'))"
                  g2-id]))
            mutation {:mutation/id (random-uuid)
                      :parent/genome-id (:genome-id ctx)
                      :hypothesis/id (random-uuid)
                      :evidence/id pack-ref
                      :risk :program
                      :ops [{:op :replace-form
                             :file "programs/route.clj"
                             :selector ['case]
                             :expect/hash "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                             :form []}]
                      :expected-effect {:primary-metric :task/success
                                        :direction :increase}}
            proposed (candidate/create-candidate
                      {:parent/generation-id generation-id
                       :parent/genome-id (:genome-id ctx)
                       :candidate/genome-id g2-id
                       :mutation/id (:mutation/id mutation)
                       :evidence/id pack-ref
                       :risk :program})
            candidate-store-handle (candidate-store/make-candidate-store (:db ctx))
            materialized (candidate/materialize-candidate!
                          candidate-store-handle
                          (proof-candidate proposed)
                          (proof-mutation mutation))
            pending (candidate/mark-evaluation-pending!
                     candidate-store-handle
                     (:candidate/id materialized))
            cand-id (:candidate/id pending)
            eval-id (random-uuid)]
        (sqlite/with-db [conn (:db ctx)]
          (jdbc/execute! conn ["UPDATE candidates SET state = 'eligible' WHERE id = ?"
                               (str cand-id)])
          (jdbc/insert! conn :eval_runs
                        {:id (str eval-id)
                         :candidate_id (str cand-id)
                         :parent_generation_id generation-id
                         :profile_id ":default-v1"
                         :gates "[]"
                         :paired_results_ref nil
                         :summary (pr-str {:hard {} :utility {}
                                           :cost {} :complexity {}})
                         :eligibility (pr-str {:eligible? true :reasons []})
                         :status "finalized"
                         :created_at "2025-01-02T00:00:00Z"}))
        (let [calls (atom [])]
          (with-redefs [promote/promote!
                        (fn [system request]
                          (swap! calls conj {:system system :request request})
                          {:status :promoted :from generation-id :to "generation-2"})]
            (let [{:keys [exit data]} (main/execute
                                       ["promote" (str cand-id)
                                        "--evaluation" (str eval-id)]
                                       {:state-dir dir})]
              (is (= 0 exit))
              (is (= :promoted (:status data)))
              (is (= 1 (count @calls)))
              (is (= cand-id (get-in @calls [0 :request :candidate-id])))
              (is (= eval-id (get-in @calls [0 :request :evaluation-id])))
              (is (= generation-id
                     (get-in @calls [0 :request :expected-parent-generation])))
              (is (uuid? (get-in @calls [0 :system :event/session-id]))
                  "the CLI anchors the promotion to a real operator session"))))
        (testing "a REAL promote moves CURRENT only through promotion/promote!"
          (let [{:keys [exit data]} (main/execute
                                     ["promote" (str cand-id)
                                      "--evaluation" (str eval-id)]
                                     {:state-dir dir})]
            (is (= 0 exit))
            (is (= :promoted (:status data)))
            (is (= generation-id (:from data)))
            (is (not= generation-id (:to data)))
            (is (= (:to data) (:id (current/current-generation (:db ctx))))
                "CURRENT now names the promoted generation")
            (is (= "promoted"
                   (:state (first (sqlite/query (:db ctx)
                                                ["SELECT state FROM candidates
                                                  WHERE id = ?" (str cand-id)]))))
                "the winning candidate is :promoted")
            (is (= 1 (count (sqlite/query (:db ctx)
                                          ["SELECT * FROM promotions"])))
                "exactly the winning promotion has a promotions row")))))))

;; ============================================================================
;; STEP 3 — machine-readable EDN by default, optional human renderer
;; ============================================================================

(deftest cli-emits-edn-by-default-and-pretty-on-demand
  (let [ctx (provision!)
        dir (:state-dir ctx)]
    (testing "default output round-trips through clojure.edn/read-string"
      (let [out (java.io.StringWriter.)]
        (binding [*out* out]
          (is (= 0 (main/run ["genome" "validate" (route-a-root)]
                             {:state-dir dir}))))
        (let [data (edn/read-string (str out))]
          (is (map? data))
          (is (= (:genome-id ctx) (:genome/id data)))
          (is (true? (:valid? data))))))
    (testing "--pretty renders a human form instead"
      (let [out (java.io.StringWriter.)]
        (binding [*out* out]
          (is (= 0 (main/run ["genome" "validate" (route-a-root) "--pretty"]
                             {:state-dir dir}))))
        (let [s (str out)]
          (is (str/includes? s "genome/id"))
          (is (str/includes? s "valid?")))))))

;; ============================================================================
;; STEP 4 — non-zero exit + :error/type on typed failures
;; ============================================================================

(deftest cli-typed-failures-exit-nonzero-with-error-type
  (let [ctx (provision!)
        dir (:state-dir ctx)]
    (testing "an unknown command exits 1 with :cli/unknown-command"
      (let [{:keys [exit data]} (main/execute ["frobnicate"] {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/unknown-command (:error/type data)))))
    (testing "a missing positional exits 1 with :cli/usage-invalid"
      (let [{:keys [exit data]} (main/execute ["genome" "diff" (route-a-root)]
                                              {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/usage-invalid (:error/type data)))))
    (testing "genome validate on a missing bundle exits 1 with the loader's typed error"
      (let [{:keys [exit data]} (main/execute ["genome" "validate" "/no/such/bundle"]
                                              {:state-dir dir})]
        (is (= 1 exit))
        (is (= :genome/root-invalid (:error/type data)))))
    (testing "run without --task exits 1 with :cli/usage-invalid"
      (let [{:keys [exit data]} (main/execute ["run" "--genome" "current"]
                                              {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/usage-invalid (:error/type data)))))
    (testing "run with a missing task file exits 1 with :cli/task-file-missing"
      (let [{:keys [exit data]} (main/execute ["run" "--genome" "current"
                                               "--task" "/no/such/task.edn"]
                                              {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/task-file-missing (:error/type data)))))
    (testing "promote of a nonexistent candidate exits 1 with the public API's typed error"
      (let [{:keys [exit data]} (main/execute ["promote" (str (random-uuid))
                                               "--evaluation" (str (random-uuid))]
                                              {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/candidate-not-found (:error/type data)))))
    (testing "rollback to a nonexistent generation exits 1 with :promotion/generation-not-found"
      (let [{:keys [exit data]} (main/execute ["rollback" "--to" "generation-999"
                                               "--reason" ":cli-test"]
                                              {:state-dir dir})]
        (is (= 1 exit))
        (is (= :promotion/generation-not-found (:error/type data)))))
    (testing "lineage of a nonexistent generation exits 1 with :lineage/generation-not-found"
      (let [{:keys [exit data]} (main/execute ["lineage" "generation-999"]
                                              {:state-dir dir})]
        (is (= 1 exit))
        (is (= :lineage/generation-not-found (:error/type data)))))
    (testing "evolve --generation on an unknown generation exits 1 with the public API's typed error"
      (let [{:keys [exit data]} (main/execute ["evolve" "--generation" "generation-999"]
                                              {:state-dir dir})]
        (is (= 1 exit))
        (is (= :evolution/generation-not-found (:error/type data)))))))

(deftest cli-context-commands-are-registered
  (let [dir (temp-dir "evoclj-cli-context-")]
    (testing "context compress without input exits 1 with :cli/usage-invalid"
      (let [{:keys [exit data]} (main/execute ["context" "compress"] {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/usage-invalid (:error/type data)))))
    (testing "context recompress without input exits 1 with :cli/usage-invalid"
      (let [{:keys [exit data]} (main/execute ["context" "recompress"] {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/usage-invalid (:error/type data)))))
    (testing "context loop without input exits 1 with :cli/usage-invalid"
      (let [{:keys [exit data]} (main/execute ["context" "loop"] {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/usage-invalid (:error/type data)))))
    (testing "context inspect without input exits 1 with :cli/usage-invalid"
      (let [{:keys [exit data]} (main/execute ["context" "inspect"] {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/usage-invalid (:error/type data)))))
    (testing "an unknown context subcommand exits 1 with :cli/unknown-command"
      (let [{:keys [exit data]} (main/execute ["context" "foo"] {:state-dir dir})]
        (is (= 1 exit))
        (is (= :cli/unknown-command (:error/type data)))))))

;; ===========================================================================
;; STEP 2 (end to end) — evolve → eval → promote → rollback through the CLI
;; ============================================================================

(deftest cli-evolve-eval-promote-rollback-end-to-end
  (let [ctx (provision!)
        dir (:state-dir ctx)
        task {:op :echo-a :text "hi"}
        expected [{:action {:intent/type :intent/tool-call
                            :payload {:tool/id :fixture/echo
                                      :args {:text "hi"}}}}
                  {:text "hi"}]
        captured (atom nil)
        eval-id-atom (atom nil)
        ;; the Evolution set: ONE success (class A) + TWO failures
        ;; (class B fails under the A-for-everything router) — the
        ;; pack that makes the deterministic :task/success hypothesis fire
        run (run-fixture-session! ctx task)
        sid (:session/id run)
        _ (run-fixture-session! ctx {:op :echo-b :text "bo"})
        _ (run-fixture-session! ctx {:op :echo-b :text "go"})
        replay-case (replay-case-from-session ctx sid task expected)
        evolve-overrides {:evolution/system
                          ;; the host wraps a plain fn into the Mutator
                          ;; adapter (kernel.build-mutator)
                          {:mutator (fn [ctx]
                                      (reset! captured ctx)
                                      (propose-deltas ctx))}}]
    (testing "STEP 2 — evolve --generation current through the CLI materializes
              the deterministic candidate (public Evolution API)"
      (let [{:keys [exit data]} (main/execute
                                 ["evolve" "--generation" "current"]
                                 {:state-dir dir :overrides evolve-overrides})]
        (is (= 0 exit))
        (let [candidates (:candidates data)]
          (is (= 1 (count candidates)))
          (let [g2 (first candidates)]
            (is (= :evaluation-pending (:state g2)))
            (is (= (:genome-id ctx) (:parent/genome-id g2)))
            (is (not= (:genome-id ctx) (:candidate/genome-id g2)))
            (is (= :program (:risk g2))))
          (is (= generation-id (:id (current/current-generation (:db ctx))))
              "the CURRENT pointer is untouched by evolution")
          (is (some? @captured)
              "the mutator received the closed orchestration context")
          ;; host bookkeeping (the test, not the CLI): store the
          ;; finalized candidate Genome body in the CAS — Database
          ;; Invariant 7 — so lineage (strict) and promotion (integrity
          ;; refusal) can verify it
          (doseq [c candidates]
            (store-genome-body!
             (:cas-store ctx)
             (load/load-genome
              (str dir "/candidates/" (dash (:candidate/genome-id c)))))))))
    (testing "candidate list / candidate inspect read the persisted records"
      (let [{:keys [exit data]} (main/execute ["candidate" "list"]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= 1 (count (:candidates data))))
        (is (= :evaluation-pending (get-in data [:candidates 0 :state]))))
      (let [g2-id (get-in (main/execute ["candidate" "list"] {:state-dir dir})
                          [:data :candidates 0 :candidate/id])
            {:keys [exit data]} (main/execute ["candidate" "inspect" (str g2-id)]
                                              {:state-dir dir})]
        (is (= 0 exit))
        (is (= g2-id (:candidate/id data)))
        (is (= generation-id (:parent/generation-id data)))))
    (testing "STEP 2 — eval <candidate-id> --profile default-v1 through the CLI
              produces the finalized evaluation"
      (let [g2-id (get-in (main/execute ["candidate" "list"] {:state-dir dir})
                          [:data :candidates 0 :candidate/id])
            {:keys [exit data]} (main/execute
                                 ["eval" (str g2-id) "--profile" "default-v1"]
                                 {:state-dir dir :overrides (eval-overrides replay-case)})]
        (is (= 0 exit))
        (let [evaluation (:evaluation data)]
          (is (uuid? (:evaluation/id evaluation)))
          (is (= g2-id (:candidate/id evaluation)))
          (is (= :default-v1 (:profile/id evaluation)))
          (is (every? #(= :pass (:status %)) (:gates evaluation))
              "all seven phases pass")
          (is (true? (get-in evaluation [:eligibility :eligible?])))
          (is (= 1.0 (get-in evaluation [:summary :utility :task/success :candidate])))
          (reset! eval-id-atom (:evaluation/id evaluation)))))
    (testing "STEP 2 — promote <candidate-id> --evaluation <id> through the CLI
              CAS-moves CURRENT to the candidate's generation"
      (let [g2-id (get-in (main/execute ["candidate" "list"] {:state-dir dir})
                          [:data :candidates 0 :candidate/id])
            eval-id @eval-id-atom
            {:keys [exit data]} (main/execute
                                 ["promote" (str g2-id) "--evaluation" (str eval-id)]
                                 {:state-dir dir})]
        (is (= 0 exit))
        (is (= :promoted (:status data)))
        (is (= generation-id (:from data)))
        (let [to (:to data)]
          (is (not= generation-id to))
          (is (= to (:id (current/current-generation (:db ctx))))
              "CURRENT now names the promoted generation")
          (testing "lineage explains G1 + evidence + mutation + evaluation → G2"
            (let [{:keys [exit data]} (main/execute ["lineage" generation-id]
                                                    {:state-dir dir})]
              (is (= 0 exit))
              (is (= 1 (count (:children data))))
              (let [child (first (:children data))]
                (is (= to (get-in child [:generation :generation/id])))
                (is (= g2-id (get-in child [:promotion :candidate/id])))
                (is (= :promoted (get-in child [:promotion :decision])))
                (is (= eval-id (get-in child [:evaluation :evaluation/id])))
                (is (true? (get-in child [:evaluation :eligibility :eligible?])))
                (is (= :program (get-in child [:mutation :risk])))))))
        (testing "STEP 2 — rollback --to generation-1 --reason <kw> through the
                  CLI moves CURRENT back (selection-only)"
          (let [{:keys [exit data]} (main/execute
                                     ["rollback" "--to" generation-id
                                      "--reason" ":cli-regression-test"]
                                     {:state-dir dir})]
            (is (= 0 exit))
            (is (= :rolled-back (:status data)))
            (is (= generation-id (:to data)))
            (is (= generation-id (:id (current/current-generation (:db ctx))))
                "CURRENT is back on generation-1")))))
    (testing "run --genome current --task <edn-file> --tool <tool-id> through
              the CLI executes a session pinned to the current generation"
      (let [task-file (task-file! task)
            {:keys [exit data]} (main/execute
                                 ["run" "--genome" "current"
                                  "--task" task-file
                                  "--tool" ":fixture/echo"]
                                 {:state-dir dir})]
        (is (= 0 exit))
        (is (= :completed (:status data)))
        (is (uuid? (:session/id data)))
        (is (uuid? (:episode data)))
        (is (= 10 (:events data)) "the CLI run appended the full route-a trace")
        (testing "replay the CLI-run session from the store"
          (let [{:keys [exit] :as rd} (main/execute
                                       ["replay" "--session" (str (:session/id data))]
                                       {:state-dir dir})
                rdata (:data rd)]
            (is (= 0 exit))
            (is (= (:session/id data) (:session/id rdata)))
            (is (= task (:task-input rdata)))
            (is (= expected (:output rdata)))))))))

;; ============================================================================
;; STEP 5 (component) — validated config in CLI startup (foundation F5)
;;
;; build-config must route the CLI's config through evoclj.config/load-config
;; (validated merge + defaults), resolve-profile, and config-value; the
;; EVOCLJ_* env overrides must win over the config file; a malformed envelope
;; must surface as the typed :config/invalid exit (never a stack trace); and
;; the :overrides deep-merge seam (host/test injection) must keep working.
;; ============================================================================

(defn- write-config-file!
  "Write `m` as EDN to a temp file (registered for cleanup) and return
  its path."
  [m]
  (let [f (java.io.File/createTempFile "evoclj-cli-config-" ".edn")]
    (swap! temp-paths conj (.getPath f))
    (spit f (pr-str m))
    (.getPath f)))

(deftest cli-config-envelope-feeds-the-host
  (let [dir (temp-dir "evoclj-cli-config-")]
    (testing "no config source — the base evolution budget-profile cap stays"
      (let [cfg (cli-session/build-config {:state-dir dir})]
        (is (= {:max-candidates 3 :max-cost 0.0 :max-tokens 0}
               (get-in cfg [:evolution/system :budget-profile])))))
    (testing ":config opts as a map validates through load-config and feeds
              the :config/budget section into the host budget-profile"
      (let [cfg (cli-session/build-config {:state-dir dir
                                       :config {:config/budget {:max-candidates 7}}})]
        (is (= 7 (get-in cfg [:evolution/system :budget-profile :max-candidates])))
        (is (= {:max-candidates 7 :max-cost 0.0 :max-tokens 0}
               (get-in cfg [:evolution/system :budget-profile])))))
    (testing ":config opts as an EDN string parses through load-config"
      (let [cfg (cli-session/build-config {:state-dir dir
                                       :config (pr-str {:config/budget {:max-candidates 2}})})]
        (is (= 2 (get-in cfg [:evolution/system :budget-profile :max-candidates])))))
    (testing "host :overrides deep-merge on top of the validated envelope
              (cases/fixtures/mutator injection still works)"
      (let [cfg (cli-session/build-config
                 {:state-dir dir
                  :config {:config/budget {:max-candidates 7}}
                  :overrides {:evolution/system {:mutator :none}
                              :eval/system {:selection/cases {:case/a 1}}}})]
        (is (= 7 (get-in cfg [:evolution/system :budget-profile :max-candidates])))
        (is (= :none (get-in cfg [:evolution/system :mutator])))
        (is (= {:case/a 1} (get-in cfg [:eval/system :selection/cases])))))))

(deftest cli-config-env-overrides-win-over-file
  (let [dir (temp-dir "evoclj-cli-config-")
        file (write-config-file! {:config/budget {:max-candidates 5}})
        file-with-profile
        (write-config-file!
         {:config/budget {:max-candidates 5}
          :config/profiles {:ops {:config/budget {:max-candidates 4}}}})]
    (testing "EVOCLJ_CONFIG env names the config file"
      (let [cfg (cli-session/build-config {:state-dir dir
                                       :env {"EVOCLJ_CONFIG" file}})]
        (is (= 5 (get-in cfg [:evolution/system :budget-profile :max-candidates])))))
    (testing "EVOCLJ_PROFILE env selects a profile over the file's base values"
      (let [cfg (cli-session/build-config {:state-dir dir
                                       :env {"EVOCLJ_CONFIG" file-with-profile
                                             "EVOCLJ_PROFILE" "ops"}})]
        (is (= 4 (get-in cfg [:evolution/system :budget-profile :max-candidates])))))
    (testing ":config/profile opts selects a profile the same way"
      (let [cfg (cli-session/build-config {:state-dir dir
                                       :env {"EVOCLJ_CONFIG" file-with-profile}
                                       :config/profile :ops})]
        (is (= 4 (get-in cfg [:evolution/system :budget-profile :max-candidates])))))
    (testing "an EVOCLJ_* scalar override wins over the file"
      (let [cfg (cli-session/build-config {:state-dir dir
                                       :env {"EVOCLJ_CONFIG" file
                                             "EVOCLJ_BUDGET_MAX_CANDIDATES" "9"}})]
        (is (= 9 (get-in cfg [:evolution/system :budget-profile :max-candidates])))))))

(deftest cli-invalid-config-exits-typed-not-stack-trace
  (let [ctx (provision!)
        dir (:state-dir ctx)]
    (testing "an unknown top-level config key exits 1 with :config/invalid"
      (let [{:keys [exit data]} (main/execute ["recovery"]
                                              {:state-dir dir
                                               :config {:config/bogus {}}})]
        (is (= 1 exit))
        (is (= :config/invalid (:error/type data)))
        (is (string? (:message data)))))
    (testing "a non-map config section exits 1 with :config/invalid"
      (let [{:keys [exit data]} (main/execute ["recovery"]
                                              {:state-dir dir
                                               :config {:config/budget 42}})]
        (is (= 1 exit))
        (is (= :config/invalid (:error/type data)))))
    (testing "a config file named by EVOCLJ_CONFIG with unparseable EDN exits 1
              with :config/invalid"
      (let [bad (java.io.File/createTempFile "evoclj-cli-config-bad-" ".edn")]
        (swap! temp-paths conj (.getPath bad))
        (spit bad "{:config/budget {:max-candidates")
        (let [{:keys [exit data]} (main/execute ["recovery"]
                                                {:state-dir dir
                                                 :env {"EVOCLJ_CONFIG" (.getPath bad)}})]
          (is (= 1 exit))
          (is (= :config/invalid (:error/type data))))))
    (testing "a missing EVOCLJ_CONFIG file exits 1 with :config/invalid"
      (let [{:keys [exit data]} (main/execute ["recovery"]
                                              {:state-dir dir
                                               :env {"EVOCLJ_CONFIG"
                                                     (str dir "/no-such-config.edn")}})]
        (is (= 1 exit))
        (is (= :config/invalid (:error/type data)))))
    (testing "an unknown profile exits 1 with :config/profile-not-found"
      (let [{:keys [exit data]} (main/execute ["recovery"]
                                              {:state-dir dir
                                               :config {:config/profiles {:a {}}}
                                               :config/profile :nope})]
        (is (= 1 exit))
        (is (= :config/profile-not-found (:error/type data)))))))

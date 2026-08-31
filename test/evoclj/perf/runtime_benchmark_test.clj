(ns evoclj.perf.runtime-benchmark-test
  "component — benchmark fixtures and broad regression ceilings.

  This namespace MEASURES the runtime's hot paths on the REAL seed
  bundle and the REAL production modules (no test doubles):

    - Genome load + hash time            (evoclj.genome.load)
    - compile time                       (evoclj.compiler.core)
    - SCI invocation overhead            (evoclj.sci.execute)
    - broker authorization overhead      (evoclj.capability.broker)
    - append-event throughput            (evoclj.store.event)
    - CAS small/large artifact throughput(evoclj.store.cas)
    - seed end-to-end task latency       (scheduler + broker + store)
    - candidate evaluation orchestration (evoclj.eval.core)

  Every deftest carries ^:perf metadata and asserts ONLY broad
  pathological ceilings (10-60x the recorded baseline, see
  docs/performance-baseline.md) so a healthy host can never trip them
  through normal variance — they exist to catch pathological
  regressions, not to win microbenchmarks. The measured numbers are
  PRINTED by each test and recorded in docs/performance-baseline.md.

  RUN: it runs with the full suite (`clojure -M:test`) and must pass
  there. To run JUST this namespace:
      clojure -M:test -n evoclj.perf.runtime-benchmark-test
  To exclude the perf namespace from a correctness run, filter on
  :perf metadata (`clojure -M:test -e :perf`; the pinned cognitect
  test-runner v0.5.1 has no --focus option). The namespace is fully
  self-contained: it
  touches only its own temp dirs and the read-only genomes/seed
  bundle, so it can never affect the correctness suite.

  All timings use System/nanoTime and report the best of several
  runs (the min is robust against GC pauses / host noise)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.capability.broker :as broker]
            [evoclj.compiler.core :as core]
            [evoclj.eval.core :as eval-core]
            [evoclj.eval.replay :as replay]
            [evoclj.eval.static :as static]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.genome.load :as load]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.episode :as episode]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.sci.execute :as execute]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.candidate-store :as candidate-store]
            [evoclj.store.existence :as existence]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; ============================================================================
;; timing helpers
;; ============================================================================

(defn- ms
  "Run (f) once; return [elapsed-ms result]."
  [f]
  (let [t0 (System/nanoTime)
        r (f)
        t1 (System/nanoTime)]
    [(/ (double (- t1 t0)) 1e6) r]))

(defn- best-of
  "Run (f) n times; return {:best-ms min :mean-ms mean :samples n}."
  [n f]
  (let [times (repeatedly n #(first (ms f)))]
    {:best-ms (apply min times)
     :mean-ms (/ (reduce + times) (double n))
     :samples n}))

(defn- report
  "Print one measurement line: label + best/mean ms."
  [label {:keys [best-ms mean-ms samples]}]
  (println (format "  [perf] %-42s best %10.1f ms   mean %10.1f ms   (n=%d)"
                   label best-ms mean-ms samples)))

(defn- throughput
  "Throughput of N operations over elapsed-ms, in ops/second."
  [n elapsed-ms]
  (* 1000.0 (/ n elapsed-ms)))

;; ============================================================================
;; shared temp-path lifecycle
;; ============================================================================

(def ^:private temp-paths (atom []))

(defn- temp-path!
  "Create a temp path (directory unless `file?`) and register it for
  cleanup."
  ([prefix] (temp-path! prefix false))
  ([prefix file?]
   (let [p (if file?
             (str (Files/createTempFile prefix "" (make-array FileAttribute 0)))
             (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))]
     (swap! temp-paths conj p)
     p)))

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

(use-fixtures :each
  (fn [f]
    ;; the G3 suite registry is kernel-side and shared; every test
    ;; starts with an empty registry so no suite leaks across tests
    (static/clear-suites!)
    (f)
    (cleanup!)))

;; ============================================================================
;; the REAL seed bundle fixtures (component style, read-only inputs)
;; ============================================================================

(def ^:private generation-id "generation-1")

(defn- seed-root
  "The real genomes/seed bundle at the repo root."
  []
  (let [p (.toPath (io/file "genomes/seed"))]
    (when-not (Files/isDirectory p (make-array LinkOption 0))
      (throw (ex-info "genomes/seed bundle not found (run from the repo root)"
                      {:path (str p)})))
    p))

(defn- route-descriptor
  "The seed route program descriptor (an in-memory descriptor list
  riding on the loaded-genome value under :programs)."
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
  "The on-disk provider catalog fixture (component Resolution)."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- program-sources
  "Decode every compiled program's source text from the immutable
  loaded bundle :files (the CompiledGenome carries only
  :source/digest references, Global Constraint 22)."
  [loaded-genome compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (get-in loaded-genome
                                         [:files (:file descriptor) :bytes]))
                        StandardCharsets/UTF_8)]))
        (:programs compiled)))

(defn- echo-lease
  "A valid CapabilityLease granting the phenotype the :fixture/echo
  :invoke action for the next minute."
  [phenotype-id]
  (let [now (java.util.Date.)]
    {:cap/id (random-uuid)
     :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id phenotype-id}
     :resource {:kind :tool :id :fixture/echo}
     :actions #{:invoke}
     :constraints {:max-calls 1000}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- model-lease
  "A valid CapabilityLease granting the phenotype :model/call."
  [phenotype-id]
  (let [now (java.util.Date.)]
    {:cap/id (random-uuid)
     :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id phenotype-id}
     :resource {:kind :model :id "lmstudio/*"}
     :actions #{:invoke}
     :constraints {:max-calls 1000}
     :issued-at now
     :expires-at (java.util.Date. (+ (.getTime now) 60000))}))

(defn- fresh-db
  [genome-id resolution-id phenotype-id]
  (let [path (temp-path! "evoclj-bench-" true)
        db (sqlite/spec path)]
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
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    [db path]))

(defn- build-executor
  "Assemble the component executor map from the REAL seed genome:

    {:phenotype <instantiated Phenotype>
     :stores {:sqlite <migrated db> :cas <CAS root>}
     :dispatch <broker context>}

  Both fixture providers are registered; the broker carries ONE lease
  (for :fixture/echo). Returns the executor plus the on-disk handles."
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
        leases [(echo-lease phenotype-id) (model-lease phenotype-id)]
        [db db-path] (fresh-db genome-id resolution-id phenotype-id)
        cas-root (temp-path! "evoclj-bench-cas-")
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases leases :usage usage}
             :program-sources (program-sources loaded compiled)})]
    {:executor {:phenotype ph
                :stores {:sqlite db :cas (cas/->cas cas-root)}
                :dispatch (dispatch/make-broker-context
                           {:registry reg
                            :leases leases
                            :usage usage})}
     :executions executions
     :db-path db-path
     :cas-root cas-root
     :compiled compiled
     :phenotype ph
     :leases leases
     :registry reg}))

(defn- create-pinned-session
  "create-session! pinned to the compiled identity, then append the
  :session/created root event. Returns the session id."
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

;; ============================================================================
;; candidate-evaluation fixtures (component minimal evaluator, self-contained)
;; ============================================================================

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private parent-genome-id (str "sha256:" hex64))
(def ^:private candidate-genome-id
  (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private evidence-id (str "sha256:" (apply str (repeat 64 "e"))))
(def ^:private file-hash (str "sha256:" (apply str (repeat 64 "f"))))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "r"))))

(defn- uuid [n]
  (UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- write-file!
  "Write `content` as UTF-8 to `path`, creating parent directories."
  [path content]
  (let [p (Paths/get path (make-array String 0))
        parent (.getParent p)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

(defn- route-source
  "A route program: {:op :echo :text t} emits a :fixture/echo tool-call
  with (transform t); anything else finishes."
  [transform-expr]
  (str "(ns agent.route)\n"
       "(defn- transform [text] " transform-expr ")\n"
       "(defn run [input]\n"
       "  (let [op (get input :op)]\n"
       "    (case op\n"
       "      :echo {:action {:intent/type :intent/tool-call\n"
       "                      :payload {:tool/id :fixture/echo\n"
       "                                :args {:text (transform (get input :text))}}}}\n"
       "      {:action {:intent/type :intent/finish :payload {:value input}}})))\n"))

(defn- bundle!
  "Build a genome bundle in a fresh temp dir and return its path
  string. Topology: :sci router → :emit."
  [transform-expr]
  (let [dir (temp-path! "bench-bundle-")]
    (write-file! (str dir "/manifest.edn")
                 (pr-str {:genome/format 1
                          :agent/id :main
                          :agent/entry :graph/main
                          :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
                          :modules {:topology "topology.edn"
                                    :models "models.edn"
                                    :memory "memory.edn"
                                    :evolution "evolution.edn"}
                          :capabilities/requested #{:tool/call}
                          :evolution {:max-risk :behavioral
                                      :mutable #{:parameters :prompts
                                                 :skills :programs}}
                          :metadata {:name "bench-fixture"
                                     :description "benchmark bundle"}}))
    (write-file! (str dir "/topology.edn")
                 (pr-str {:graph/id :graph/core
                          :entry :node/router
                          :nodes {:node/router {:node/type :sci
                                                :program :program/route
                                                :next :node/emit}
                                  :node/emit {:node/type :emit}}
                          :limits {:max-steps 64}}))
    (write-file! (str dir "/models.edn")
                 "{:models {:planner {:alias :reasoning/high}}}")
    (write-file! (str dir "/memory.edn") "{:memory {}}")
    (write-file! (str dir "/evolution.edn") "{:evolution {}}")
    (write-file! (str dir "/programs/route.clj") (route-source transform-expr))
    dir))

(defn- seeded-echo-provider
  "A DETERMINISTIC fixture provider: echoes {:text (str text \"-fixed\")}."
  []
  (reify proto/Provider
    (describe [_]
      {:tool/id :fixture/echo
       :effect :pure
       :input-schema [:map [:text :string]]
       :output-schema [:map [:text :string]]
       :required-action :invoke})
    (normalize-request [_ intent]
      {:tool/id :fixture/echo
       :resource {:kind :tool :id :fixture/echo}
       :args (get-in intent [:payload :args])})
    (execute-request! [_ authorized-request]
      {:text (str (get-in authorized-request [:args :text]) "-fixed")})))

(defn- echo-decision [text]
  {:action {:intent/type :intent/tool-call
            :payload {:tool/id :fixture/echo :args {:text text}}}})

(defn- selection-case
  []
  {:case/id :sel/c1
   :task-input {:op :echo :text "hi"}
   :expected-output [(echo-decision "hi") {:text "hi-fixed"}]
   :tools #{:fixture/echo}})

(defn- replay-case
  []
  (replay/build-replay-case
   {:episode/id (UUID/randomUUID)
    :outcome {:status :completed}}
   [{:intent/type :intent/tool-call
     :effect :read
     :payload {:tool/id :fixture/echo :args {:text "hi"}}
     :response {:text "hi-fixed"}}]
   {:case/id :replay/c1
    :task-input {:op :echo :text "hi"}
    :expected-output [(echo-decision "hi") {:text "hi-fixed"}]
    :mode :fixture}))

(defn- test-profile
  []
  {:eval/profile-id :bench/v1
   :evolution-set {:source :evals/evolution}
   :selection-set {:source :evals/selection :visibility :kernel-only}
   :audit-set {:source :evals/audit :visibility :operator-only}
   :repetitions 1
   :promotion {:strategy :paired-comparison
               :min-delta 0.05
               :max-cost-regression 1.10
               :max-complexity-regression 1.25}})

(defn- fresh-store
  "A migrated temp database seeded with the parent generation row
  (current = 1) plus a temp CAS root. Returns {:sqlite ... :cas ...}."
  []
  (let [db-path (temp-path! "bench-eval-" true)
        db (sqlite/spec db-path)
        cas-root (temp-path! "bench-eval-cas-")]
    (migrate/migrate! db)
    (doseq [[artifact-id media-type]
            [[parent-genome-id "application/octet-stream"]
             [candidate-genome-id "application/octet-stream"]
             [evidence-id "application/edn"]
             [file-hash "application/edn"]
             [resolution-id "application/edn"]]]
      (artifact/ensure-artifact! db artifact-id media-type 0))
    (doseq [genome-id [parent-genome-id candidate-genome-id]]
      (artifact/ensure-genome! db genome-id))
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id parent-genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    {:sqlite db :cas (cas/->cas cas-root)}))

(defn- proof
  [artifact-id]
  (#'existence/unsafe-verified-digest artifact-id))

(defn- materialized-pending!
  "Materialize a fresh candidate from the fixture parent+mutation and
  transition it to :evaluation-pending. Returns the pending Candidate
  record."
  [store]
  (let [m {:mutation/id (uuid 1)
           :parent/genome-id parent-genome-id
           :hypothesis/id (uuid 2)
           :evidence/id evidence-id
           :risk :behavioral
           :ops [{:op :set-edn
                  :file "skills/debugging.edn"
                  :path [:workflow :before-edit]
                  :expect/hash file-hash
                  :value [:reproduce :localize]}]
           :expected-effect {:primary-metric :task/success
                             :direction :increase}}
        c (candidate/create-candidate
           {:parent/generation-id generation-id
            :parent/genome-id parent-genome-id
            :candidate/genome-id candidate-genome-id
            :mutation/id (uuid 1)
            :evidence/id evidence-id
            :risk :behavioral})
        handle (candidate-store/make-candidate-store (:sqlite store))
        m1 (candidate/materialize-candidate!
            handle
            (update c :candidate/genome-id proof)
            (-> m
                (update :parent/genome-id proof)
                (update :evidence/id proof)))]
    (candidate/mark-evaluation-pending! handle (:candidate/id m1))))

(defn- orchestrator-evaluator
  "A minimal valid evaluator value for evaluate-candidate!. The parent
  bundle's route TRANSFORMS the text (fails the selection oracle)
  while the candidate bundle is the identity transform (passes it)."
  [store pending]
  {:store store
   :provider/catalog (fixture-catalog)
   :kernel/abi {:kernel 1 :genome 1 :intent 1 :tool 1}
   :profiles {:bench/v1 (test-profile)}
   :genome/roots {generation-id (bundle! "(str text \"-parent\")")
                  (str (:candidate/id pending)) (bundle! "text")}
   :selection/cases {:sel/c1 (selection-case)}
   :selection/fixtures {:fixture/echo (fn [_seed] (seeded-echo-provider))}
   :replay/cases {:replay/c1 (replay-case)}
   :replay/fixtures {:fixture/echo (fn [] (seeded-echo-provider))}
   :programs (fn [_loaded] [(route-descriptor)])
   :measure/cost (fn [_root] 1000.0)})

;; ============================================================================
;; the benchmark deftests — broad pathological ceilings only
;; ============================================================================

(deftest ^:perf genome-load-and-hash-within-pathological-ceiling
  (let [{:keys [best-ms mean-ms]} (best-of 3 seed-loaded-genome)
        g1 (seed-loaded-genome)
        g2 (seed-loaded-genome)]
    (report "genome load+hash (seed bundle)" {:best-ms best-ms :mean-ms mean-ms :samples 3})
    (is (re-matches #"^sha256:[0-9a-f]{64}$" (:genome/id g1)))
    (is (= (:genome/id g1) (:genome/id g2))
        "reloading must yield the same content address (Global Constraint 6)")
    (is (<= best-ms 10000.0)
        "load+hash of the seed genome must complete in < 10s")))

(deftest ^:perf compile-time-within-pathological-ceiling
  (let [loaded (seed-loaded-genome)
        catalog (fixture-catalog)
        {:keys [best-ms mean-ms]} (best-of 3 #(core/compile-genome loaded catalog))
        compiled (core/compile-genome loaded catalog)]
    (report "compile seed genome" {:best-ms best-ms :mean-ms mean-ms :samples 3})
    (is (= (:compiled/genome-id compiled) (:genome/id loaded))
        "the compiled genome names the loaded bundle's address")
    (is (<= best-ms 10000.0)
        "compile must complete in < 10s")))

(deftest ^:perf sci-invocation-overhead-within-pathological-ceiling
  (let [{:keys [executor]} (build-executor)
        sci (:sci-runtime (:phenotype executor))
        n 500
        [elapsed _] (ms (fn [] (dotimes [i n]
                                 (execute/invoke! sci :program/route
                                                  {:op :finish :value i} nil))))
        per-inv (/ elapsed n)
        res (execute/invoke! sci :program/route {:op :finish :value 1} nil)]
    (println (format "  [perf] SCI: %d route-program invocations in %10.1f ms (%8.3f ms/inv)"
                     n elapsed per-inv))
    (is (= :ok (:status res)))
    (is (<= per-inv 20.0)
        "mean SCI invocation must be < 20ms (pathological only)")))

(deftest ^:perf broker-authorization-overhead-within-pathological-ceiling
  (let [sid #uuid "11111111-1111-4111-8111-111111111111"
        pid (str "sha256:" (apply str (repeat 64 "a")))
        it (intent/tool-call sid pid :node/tool 42
                             {:tool/id :fixture/echo :args {:text "hi"}}
                             {:wall-ms 1000})
        req {:tool/id :fixture/echo
             :resource {:kind :tool :id :fixture/echo}
             :args {:text "hi"}}
        now (java.util.Date. 1700001800000)
        lease {:cap/id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
               :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id pid}
               :resource {:kind :tool :id :fixture/echo}
               :actions #{:invoke}
               :constraints {:max-calls 1000}
               :issued-at (java.util.Date. 1700000000000)
               :expires-at (java.util.Date. 1700003600000)}
        n 1000
        [elapsed _] (ms (fn []
                          (dotimes [_ n]
                            (broker/authorize {:intent it
                                               :normalized-request req
                                               :leases [lease]
                                               :usage {}
                                               :now now}))))
        per-auth (/ elapsed n)
        decision (broker/authorize {:intent it :normalized-request req
                                    :leases [lease] :usage {} :now now})]
    (println (format "  [perf] broker authorize: %d calls in %10.1f ms (%8.3f ms/call)"
                     n elapsed per-auth))
    (is (= :allow (:decision decision)))
    (is (<= per-auth 10.0)
        "mean broker authorization must be < 10ms (pathological only)")))

(deftest ^:perf append-event-throughput-above-pathological-floor
  (let [{:keys [executor compiled]} (build-executor)
        db (:sqlite (:stores executor))
        sid (create-pinned-session executor compiled)
        root (event/append-event! db
                                  {:session/id sid
                                   :generation/id generation-id
                                   :phenotype/id (:compiled/phenotype-id compiled)
                                   :event/type :session/created
                                   :cause/event-id nil
                                   :payload-ref nil
                                   :metadata {}})
        n 200
        [elapsed _] (ms (fn []
                          (loop [i 0 cause (:event/id root)]
                            (when (< i n)
                              (let [e (event/append-event!
                                       db
                                       {:session/id sid
                                        :generation/id generation-id
                                        :phenotype/id (:compiled/phenotype-id compiled)
                                        :event/type :intent/proposed
                                        :cause/event-id cause
                                        :payload-ref nil
                                        :metadata {:i i}})]
                                (recur (inc i) (:event/id e)))))))
        rate (throughput n elapsed)]
    (println (format "  [perf] append-event: %d events in %10.1f ms (%8.1f events/s)"
                     n elapsed rate))
    (is (> rate 50.0)
        "append-event throughput must exceed 50 events/s (pathological floor)")))

(deftest ^:perf cas-small-and-large-throughput-within-pathological-ceiling
  (let [store (cas/->cas (temp-path! "bench-cas-"))
        small (fn [i] (byte-array (map #(unchecked-byte (bit-xor % i))
                                       (range 1024))))
        large (fn [i] (byte-array (map #(unchecked-byte (bit-xor % i))
                                       (range (* 1024 1024)))))
        n-small 200
        n-large 10
        [t-small _] (ms (fn [] (dotimes [i n-small]
                                 (cas/put-bytes! store (small i)
                                                 {:media-type "application/octet-stream"}))))
        [t-large _] (ms (fn [] (dotimes [i n-large]
                                 (cas/put-bytes! store (large i)
                                                 {:media-type "application/octet-stream"}))))
        art (cas/put-bytes! store (small 999) {:media-type "application/octet-stream"})
        readback (cas/get-bytes store (:artifact/id art))]
    (println (format "  [perf] CAS small: %d x 1KB puts in %10.1f ms" n-small t-small))
    (println (format "  [perf] CAS large: %d x 1MB puts in %10.1f ms" n-large t-large))
    (is (= (seq (small 999)) (seq readback)) "CAS round-trips the small artifact")
    (is (<= t-small 30000.0) "200 x 1KB CAS puts must complete in < 30s")
    (is (<= t-large 30000.0) "10 x 1MB CAS puts must complete in < 30s")))

(deftest ^:perf seed-end-to-end-task-latency-within-pathological-ceiling
  (let [[build-ms built] (ms build-executor)
        executor (:executor built)
        compiled (:compiled built)
        db (:sqlite (:stores executor))
        sid (create-pinned-session executor compiled)
        [run-ms result] (ms (fn []
                              (scheduler/run-session! executor sid
                                                      {:op :echo :text "abc"})))
        total (+ build-ms run-ms)]
    (println (format "  [perf] seed e2e: build %10.1f ms + run %10.1f ms = %10.1f ms total"
                     build-ms run-ms total))
    (is (= :completed (:status result)))
    (is (<= total 60000.0)
        "seed end-to-end task (excluding model network) must complete in < 60s")))

(deftest ^:perf candidate-evaluation-orchestration-within-pathological-ceiling
  (let [store (fresh-store)
        pending (materialized-pending! store)
        ev (orchestrator-evaluator store pending)
        [elapsed evaluation] (ms (fn []
                                   (eval-core/evaluate-candidate!
                                    ev (:candidate/id pending) :bench/v1)))]
    (println (format "  [perf] candidate evaluation orchestration: %10.1f ms" elapsed))
    (is (uuid? (:evaluation/id evaluation)))
    (is (true? (:eligible? (:eligibility evaluation))))
    (is (<= elapsed 60000.0)
        "candidate evaluation orchestration must complete in < 60s")))


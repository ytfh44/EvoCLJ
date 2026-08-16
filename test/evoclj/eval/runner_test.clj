(ns evoclj.eval.runner-test
  "Task A3 — Foundation F4: parallel candidate batch evaluation.

  evaluate-batch! (evoclj.eval.core) evaluates a BATCH of
  :evaluation-pending candidates under one profile in PARALLEL through
  evoclj.eval.workers/run-batch!, reusing the exact single-candidate
  pipeline (run-pipeline) per task — so every candidate keeps
  run-side!'s fresh-temp-store isolation (Global Constraints
  11/12/23) — with a per-task timeout, per-task error isolation, a
  bounded concurrency cap (default 4), and fully structured batch
  results (per-candidate status + eval refs).

  Coverage follows the foundation's own test philosophy
  (evoclj.eval.workers-test): the pool SEMANTICS — per-candidate error
  isolation, per-task timeout, concurrency cap, default concurrency,
  fail-fast validation — are exercised with fast INJECTED task runners
  (:task-runner test seam), exactly as workers-test never drives the
  real scheduler. The REAL pipeline path is exercised once end to end:
  a 4-candidate batch (two passing, one G2-failing, one whose genome
  root is unresolved) proves per-candidate results, a mix of
  pass/fail, error isolation inside the task, and zero cross-candidate
  corruption (each evaluation persists against its own candidate row
  and reads back by its own eval ref).

  candidate-batch-tasks (evoclj.eval.runner) — the eval layer's task
  contract for the pool — is asserted as a pure task-map builder."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.core :as eval-core]
            [evoclj.eval.replay :as replay]
            [evoclj.eval.runner :as runner]
            [evoclj.eval.static :as static]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.provider.protocol :as proto]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; --- shared fixture identity (Task 7.6 style, mirrors core_test) ------------

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private parent-genome-id
  "The parent Genome the candidates materialize from."
  (str "sha256:" hex64))

(def ^:private candidate-genome-id
  "The content-addressed candidate Genome (Task 7.4 patch output)."
  (str "sha256:" (apply str (repeat 64 "c"))))

(def ^:private evidence-id
  "ArtifactId of the frozen evidence pack the mutation answers."
  (str "sha256:" (apply str (repeat 64 "e"))))

(def ^:private file-hash
  "The :expect/hash preimage digest of the fixture op's target file."
  (str "sha256:" (apply str (repeat 64 "f"))))

(def ^:private resolution-id
  "A compiled ResolutionId for the seeded generation row."
  (str "sha256:" (apply str (repeat 64 "r"))))

(def ^:private generation-id
  "The parent generation's stable id (the seeded CURRENT row)."
  "generation-1")

(defn- uuid
  "A fixed, readable UUID for fixture ids."
  [n]
  (UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- mutation*
  "A schema-plausible Mutation IR fixture (Task 7.3 shape) carrying one
  :set-edn op; an optional override map wins. Varying :mutation/id
  keeps the (parent-genome-id, mutation-hash) dedup key distinct, so a
  batch can materialize several DIFFERENT pending candidates."
  [& [overrides]]
  (merge {:mutation/id (uuid 1)
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
         overrides))

(defn- candidate-request
  "A valid create-candidate request for the fixture parent+mutation;
  an optional override map wins."
  [& [overrides]]
  (merge {:parent/generation-id generation-id
          :parent/genome-id parent-genome-id
          :candidate/genome-id candidate-genome-id
          :mutation/id (uuid 1)
          :evidence/id evidence-id
          :risk :behavioral}
         overrides))

;; --- temp-path lifecycle -----------------------------------------------------

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
    ;; G3's suite registry is kernel-side and shared; every test starts
    ;; with an empty registry so no suite leaks across tests (the same
    ;; discipline as the Task 8.2 gates tests)
    (static/clear-suites!)
    (f)
    (cleanup!)))

(defn- write-file!
  "Write `content` as UTF-8 to `path`, creating parent directories."
  [path content]
  (let [p (Paths/get path (make-array String 0))
        parent (.getParent p)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

;; --- the candidate store -----------------------------------------------------

(defn- fresh-store
  "A migrated temp database seeded with the parent generation row
  (current = 1, Database Invariant 6) plus a temp CAS root. Returns
  the evaluator :store map {:sqlite ... :cas ...}."
  []
  (let [db-path (temp-path! "evoclj-batch-" true)
        db (sqlite/spec db-path)
        cas-root (temp-path! "evoclj-batch-cas-")]
    (migrate/migrate! db)
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

(defn- materialized-pending!
  "Materialize a FRESH candidate from the fixture parent+mutation and
  transition it to :evaluation-pending. Returns the pending Candidate
  record (its :candidate/id is a fresh random uuid). `n` varies the
  mutation's :evidence/id — the content the dedup rule
  (parent-genome-id, mutation-hash) hashes, which EXCLUDES
  :mutation/id — so each n yields a DISTINCT candidate."
  ([store] (materialized-pending! store 1))
  ([store n]
   (let [evidence (str "sha256:" (format "%064x" n))
         m (mutation* {:mutation/id (uuid n) :evidence/id evidence})
         c (candidate/create-candidate
            (candidate-request {:mutation/id (uuid n) :evidence/id evidence}))
         m1 (candidate/materialize-candidate! store c m)]
     (candidate/mark-evaluation-pending! store (:candidate/id m1)))))

;; --- genome bundles (Task 8.4 paired-fixture style) --------------------------

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
  string. The topology is :sci router → :emit; the router runs the
  route program built from `transform-expr` (default: identity)."
  ([transform-expr]
   (let [dir (temp-path! "runner-bundle-")]
     (write-file! (str dir "/manifest.edn")
                  (pr-str {:genome/format 1
                           :agent/id :main
                           :agent/entry :graph/main
                           :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
                           :modules {:topology "topology.edn"
                                     :models "models.edn"
                                     :memory "memory.edn"
                                     :evolution "evolution.edn"}
                           :capabilities/requested #{:model/call}
                           :evolution {:max-risk :behavioral
                                       :mutable #{:parameters :prompts
                                                  :skills :programs}}
                           :metadata {:name "runner-fixture"
                                      :description "batch test bundle"}}))
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
     dir)))

(defn- provider-catalog
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- route-descriptor
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- echo-decision
  [text]
  {:action {:intent/type :intent/tool-call
            :payload {:tool/id :fixture/echo :args {:text text}}}})

;; --- the deterministic echo fixture -------------------------------------------

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

;; --- selection and replay cases ----------------------------------------------

(defn- selection-case
  "A selection case exercising the echo fixture. The oracle expects the
  identity route's outputs: the echo decision plus the fixed suffix
  response — so an identity candidate scores 1.0 and a text-transforming
  parent scores 0.0."
  []
  {:case/id :sel/c1
   :task-input {:op :echo :text "hi"}
   :expected-output [(echo-decision "hi") {:text "hi-fixed"}]
   :tools #{:fixture/echo}})

(defn- replay-case
  "A :fixture-mode replay case over the candidate's recorded echo call.
  The candidate re-walk emits the same tool call, the replay provider
  serves the recorded response, and the accumulated outputs equal the
  oracle — a passing replay with no regressions."
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

;; --- the evaluation profile ---------------------------------------------------

(defn- test-profile
  "A Task 8.1 profile carrying the Task 8.5 thresholds (the paired
  comparison demands a real utility improvement, min-delta 0.05)."
  []
  {:eval/profile-id :test/v1
   :evolution-set {:source :evals/evolution}
   :selection-set {:source :evals/selection :visibility :kernel-only}
   :audit-set {:source :evals/audit :visibility :operator-only}
   :repetitions 1
   :promotion {:strategy :paired-comparison
               :min-delta 0.05
               :max-cost-regression 1.10
               :max-complexity-regression 1.25}})

;; --- the batch evaluator ------------------------------------------------------

(defn- batch-evaluator
  "A minimal valid evaluator for evaluate-batch!: `parent-bundle` is
  the parent side root (shared by EVERY candidate — the whole batch
  runs under one parent generation), `candidate-roots` maps each
  candidate id string -> its bundle root (a candidate with NO entry
  fails closed inside its own task with :eval/genome-unresolved). An
  optional override map wins."
  [store parent-bundle candidate-roots & [overrides]]
  (merge {:store store
          :provider/catalog (provider-catalog)
          :kernel/abi {:kernel 1 :genome 1 :intent 1 :tool 1}
          :profiles {:test/v1 (test-profile)}
          :genome/roots (assoc candidate-roots generation-id parent-bundle)
          :selection/cases {:sel/c1 (selection-case)}
          :selection/fixtures {:fixture/echo (fn [_seed] (seeded-echo-provider))}
          :replay/cases {:replay/c1 (replay-case)}
          :replay/fixtures {:fixture/echo (fn [] (seeded-echo-provider))}
          :programs (fn [_loaded] [(route-descriptor)])
          :measure/cost (fn [_root] 1000.0)}
         overrides))

;; --- helpers ------------------------------------------------------------------

(defn- fake-evaluation
  "A minimal Evaluation-shaped :task/result for the injected-runner
  tests (the batch passes task results through untouched)."
  [candidate-id]
  {:evaluation/id (UUID/randomUUID)
   :candidate/id candidate-id
   :eligibility {:eligible? true :reasons []}})

(defn- error-type
  "The :error/type of the FIRST typed error thrown by `thunk`, or fail
  the test if none is thrown."
  [thunk]
  (try
    (thunk)
    (is false "expected a typed error to be thrown")
    nil
    (catch clojure.lang.ExceptionInfo e
      (:error/type (ex-data e)))))

;; ============================================================================
;; candidate-batch-tasks — the eval layer's task contract for the pool
;; ============================================================================

(deftest candidate-batch-tasks-build-one-task-map-per-candidate
  (let [ids [(uuid 1) (uuid 2) (uuid 3)]
        tasks (runner/candidate-batch-tasks ids)]
    (testing "one task map per candidate, in order"
      (is (= 3 (count tasks)))
      (is (= ids (mapv :task/id tasks)))
      (is (= ids (mapv :candidate/id tasks))))
    (testing "each task carries the candidate id as its stable identity"
      (is (every? #(= (:task/id %) (:candidate/id %)) tasks)))))

;; ============================================================================
;; pool semantics with injected task runners (workers-test philosophy)
;; ============================================================================

(deftest batch-isolates-per-candidate-errors-and-shapes-results
  (let [store (fresh-store)
        c1 (materialized-pending! store 1)
        c2 (materialized-pending! store 2)
        c3 (materialized-pending! store 3)
        c4 (materialized-pending! store 4)
        ids (mapv :candidate/id [c1 c2 c3 c4])
        ev (batch-evaluator store (bundle! "text") {})
        runner-fn (fn [task]
                    (if (= (:candidate/id c2) (:candidate/id task))
                      (throw (ex-info "boom" {:error/type :test/boom
                                              :detail (:candidate/id task)}))
                      (fake-evaluation (:candidate/id task))))
        result (eval-core/evaluate-batch! ev ids :test/v1
                                          {:task-runner runner-fn})]
    (testing "completed entries carry one eval record per candidate, sorted by index"
      (is (= 3 (:ok (:batch/stats result))))
      (is (= [0 2 3] (mapv :task/index (:batch/completed result))))
      (is (= (remove #{(:candidate/id c2)} ids)
             (mapv :task/id (:batch/completed result))))
      (is (every? (fn [e]
                    (and (uuid? (get-in e [:task/result :evaluation/id]))
                         (= (:task/id e) (get-in e [:task/result :candidate/id]))))
                  (:batch/completed result))
          "each completed entry pairs the candidate id with ITS OWN eval ref"))
    (testing "the throwing candidate lands in :batch/failed with its type preserved"
      (is (= 1 (:failed (:batch/stats result))))
      (let [f (first (:batch/failed result))]
        (is (= 1 (:task/index f)))
        (is (= (:candidate/id c2) (:task/id f)))
        (is (= :test/boom (:error/type f)))
        (is (= "boom" (:error/message f)))
        (is (= {:detail (:candidate/id c2)} (:error/data f)))))
    (testing "stats reconcile; nothing was cancelled"
      (is (= 4 (:total (:batch/stats result))))
      (is (= 0 (:cancelled (:batch/stats result))))
      (is (= [] (:batch/cancelled result))))))

(deftest batch-timeout-aborts-only-the-stalled-task
  (let [store (fresh-store)
        c1 (materialized-pending! store 1)
        c2 (materialized-pending! store 2)
        c3 (materialized-pending! store 3)
        ids (mapv :candidate/id [c1 c2 c3])
        ev (batch-evaluator store (bundle! "text") {})
        runner-fn (fn [task]
                    (when (= (:candidate/id c1) (:candidate/id task))
                      (Thread/sleep 400))
                    (fake-evaluation (:candidate/id task)))
        start (System/nanoTime)
        result (eval-core/evaluate-batch! ev ids :test/v1
                                          {:task-runner runner-fn
                                           :timeout-ms 100})
        wall-ms (/ (- (System/nanoTime) start) 1000000)]
    (testing "only the stalled task times out (:eval/worker-timeout)"
      (is (= 1 (:failed (:batch/stats result))))
      (let [f (first (:batch/failed result))]
        (is (= (:candidate/id c1) (:task/id f)))
        (is (= :eval/worker-timeout (:error/type f)))))
    (testing "the other tasks complete normally"
      (is (= 2 (:ok (:batch/stats result))))
      (is (= (set (remove #{(:candidate/id c1)} ids))
             (set (mapv :task/id (:batch/completed result))))))
    (testing "the batch returns well under the serial 400ms sum (parallel)"
      (is (< wall-ms 300) (str "wall=" wall-ms "ms")))))

(deftest batch-respects-the-concurrency-cap
  (let [store (fresh-store)
        ids (mapv :candidate/id
                  (mapv #(materialized-pending! store %) [1 2 3 4 5]))
        ev (batch-evaluator store (bundle! "text") {})
        live (atom 0)
        max-live (atom 0)
        runner-fn (fn [_task]
                    (let [n (swap! live inc)]
                      (swap! max-live max n)
                      (try
                        (Thread/sleep 60)
                        (fake-evaluation (uuid 1))
                        (finally
                          (swap! live dec)))))
        start (System/nanoTime)
        result (eval-core/evaluate-batch! ev ids :test/v1
                                          {:task-runner runner-fn
                                           :concurrency 2})
        wall-ms (/ (- (System/nanoTime) start) 1000000)]
    (testing "never more than the configured concurrency run at once"
      (is (= 2 @max-live) (str "max concurrent = " @max-live)))
    (testing "all tasks complete"
      (is (= 5 (:ok (:batch/stats result))))
      (is (= 5 (:total (:batch/stats result)))))
    (testing "parallelism is real: far under the 300ms serial sum"
      (is (< wall-ms 280) (str "wall=" wall-ms "ms")))))

(deftest batch-default-concurrency-is-four
  (let [store (fresh-store)
        ids (mapv :candidate/id
                  (mapv #(materialized-pending! store %) [1 2 3 4]))
        ev (batch-evaluator store (bundle! "text") {})
        live (atom 0)
        max-live (atom 0)
        runner-fn (fn [_task]
                    (let [n (swap! live inc)]
                      (swap! max-live max n)
                      (try
                        (Thread/sleep 120)
                        (fake-evaluation (uuid 1))
                        (finally
                          (swap! live dec)))))
        result (eval-core/evaluate-batch! ev ids :test/v1
                                          {:task-runner runner-fn})]
    (testing "with no :concurrency the DEFAULT pool size is 4 (parallel, not serial)"
      (is (= 4 @max-live) (str "max concurrent = " @max-live)))
    (testing "all four complete"
      (is (= 4 (:ok (:batch/stats result)))))))

;; ============================================================================
;; fail-fast validation (validate before ANY task is submitted)
;; ============================================================================

(deftest batch-validates-up-front-before-any-task-is-submitted
  (let [store (fresh-store)
        pending (materialized-pending! store 1)
        ev (batch-evaluator store (bundle! "text") {})
        submitted (atom 0)
        runner-fn (fn [task]
                    (swap! submitted inc)
                    (fake-evaluation (:candidate/id task)))]
    (testing "non-sequential candidate ids are rejected"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"candidate-ids"
           (eval-core/evaluate-batch! ev {:candidate/id (:candidate/id pending)}
                                      :test/v1 {:task-runner runner-fn}))))
    (testing "an unknown candidate id fails closed"
      (is (= :eval/candidate-not-found
             (error-type #(eval-core/evaluate-batch!
                           ev [(uuid 99)] :test/v1
                           {:task-runner runner-fn})))))
    (testing "duplicate candidate ids are rejected"
      (is (= :eval/evaluator-invalid
             (error-type #(eval-core/evaluate-batch!
                           ev [(uuid 1) (uuid 1)] :test/v1
                           {:task-runner runner-fn})))))
    (testing "a candidate not :evaluation-pending fails closed"
      (let [evidence (str "sha256:" (format "%064x" 2))
            m (mutation* {:mutation/id (uuid 2) :evidence/id evidence})
            c (candidate/create-candidate
               (candidate-request {:mutation/id (uuid 2)
                                   :evidence/id evidence}))
            materialized (candidate/materialize-candidate! store c m)]
        (is (= :eval/candidate-state-invalid
               (error-type #(eval-core/evaluate-batch!
                             ev [(:candidate/id materialized)] :test/v1
                             {:task-runner runner-fn}))))))
    (testing "a failing validation never submits any task"
      (try
        (eval-core/evaluate-batch! ev [(uuid 99)] :test/v1
                                   {:task-runner runner-fn})
        (catch clojure.lang.ExceptionInfo _))
      (is (zero? @submitted)))))

;; ============================================================================
;; the REAL pipeline path — per-candidate results, pass/fail mix, isolation
;; ============================================================================

(deftest batch-runs-the-real-pipeline-with-per-candidate-isolation
  (let [store (fresh-store)
        pass-1 (materialized-pending! store 1)
        pass-2 (materialized-pending! store 2)
        fail-g2 (materialized-pending! store 3)
        unres (materialized-pending! store 4)
        ids (mapv :candidate/id [pass-1 pass-2 fail-g2 unres])
        ;; the G2-failing candidate: an eval-root file — it would modify
        ;; the evaluator that judges it (Global Constraint 12)
        failing-bundle (bundle! "text")
        _ (write-file! (str failing-bundle "/eval/tamper.edn") "{:x 1}")
        ev (batch-evaluator store (bundle! "(str text \"-parent\")")
                            {(str (:candidate/id pass-1)) (bundle! "text")
                             (str (:candidate/id pass-2)) (bundle! "text")
                             (str (:candidate/id fail-g2)) failing-bundle})
        ;; unres has NO :genome/roots entry -> fails closed inside its task
        result (eval-core/evaluate-batch! ev ids :test/v1 {:concurrency 1})]
    (testing "three candidates complete; the unresolved one fails inside its task"
      (is (= 3 (:ok (:batch/stats result))))
      (is (= 1 (:failed (:batch/stats result))))
      (is (= 0 (:cancelled (:batch/stats result))))
      (is (= 4 (:total (:batch/stats result)))))
    (testing "per-candidate results: each task/result is THAT candidate's evaluation"
      (is (= (set (remove #{(:candidate/id unres)} ids))
             (set (mapv :task/id (:batch/completed result)))))
      (doseq [e (:batch/completed result)]
        (let [eval-rec (:task/result e)]
          (is (= (:task/id e) (:candidate/id eval-rec)))
          (is (uuid? (:evaluation/id eval-rec)))
          (is (= :test/v1 (:profile/id eval-rec)))
          (is (instance? java.util.Date (:created-at eval-rec))))))
    (testing "the batch mixes pass and fail per candidate"
      (let [by-id (into {} (map (juxt :task/id :task/result))
                        (:batch/completed result))]
        (is (true? (:eligible? (:eligibility (by-id (:candidate/id pass-1))))))
        (is (true? (:eligible? (:eligibility (by-id (:candidate/id pass-2))))))
        (is (= [:pass :pass :pass :pass :pass :pass :pass]
               (mapv :status (:gates (by-id (:candidate/id pass-1))))))
        (is (false? (:eligible? (:eligibility (by-id (:candidate/id fail-g2))))))
        (is (= [:pass :pass :fail :not-run :not-run :not-run :not-run]
               (mapv :status (:gates (by-id (:candidate/id fail-g2))))))))
    (testing "no cross-candidate corruption: each eval persists against its OWN row"
      (doseq [e (:batch/completed result)]
        (let [ev-id (:evaluation/id (:task/result e))
              cid (:task/id e)]
          (is (= cid (:candidate/id (eval-core/find-evaluation ev ev-id))))
          (is (= 1 (count (eval-core/find-evaluations-by-candidate ev cid)))
              "exactly one eval_runs row per candidate")
          (is (= :evaluated (:state (candidate/find-candidate store cid)))))))
    (testing "passing candidates carry a durable paired-results eval ref"
      (doseq [cid [(:candidate/id pass-1) (:candidate/id pass-2)]]
        (let [eval-rec (:task/result
                        (first (filter #(= cid (:task/id %))
                                       (:batch/completed result))))]
          (is (re-matches #"^sha256:[0-9a-f]{64}$"
                          (:paired-results-ref eval-rec))))))
    (testing "the unresolved candidate was never evaluated: isolated, still pending"
      (let [f (first (:batch/failed result))]
        (is (= (:candidate/id unres) (:task/id f)))
        (is (= :eval/genome-unresolved (:error/type f)))
        (is (= :evaluation-pending
               (:state (candidate/find-candidate store (:candidate/id unres)))))))))

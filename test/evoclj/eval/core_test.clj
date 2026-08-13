(ns evoclj.eval.core-test
  "Task 8.7 — end-to-end candidate evaluation orchestration.

  evaluate-candidate! runs the NORMATIVE phase order (G0 parse → G1
  schema/ABI → G2 static policy → G3 deterministic tests → G4 replay →
  G5 paired hidden selection → G6 cost/complexity guardrails →
  eligibility summary) and produces an IMMUTABLE Evaluation record
  with an explicit :eligibility decision. A failed hard gate records
  later gates as :not-run (never implicit passes); a rerun creates a
  NEW evaluation id. The candidate transitions :evaluation-pending →
  :evaluated TRANSACTIONALLY with the finalized report: one SQL
  transaction inserts the eval_runs row and CAS-updates the candidates
  state, so a persistence failure rolls back BOTH.

  The four required scenarios, in the task's numbered order:

  - Step 1/2: one passing candidate (full pipeline → eligible? true)
    and one candidate failing at G2 (later gates :not-run, ineligible
    with explicit hard reasons).
  - Step 3: a finalized evaluation is immutable — the record equals
    the persisted read-back and a rerun of the pipeline creates a NEW
    evaluation id while the first row stays byte-identical.
  - Step 4: the candidate state transition is transactional — the
    success path lands on :evaluated, and a simulated persistence
    failure (injected inside the finalization transaction) leaves the
    candidate :evaluation-pending with no eval_runs row.
  - Step 5 (Milestone 8 exit): a candidate can be declared eligible?
    true/false with a complete independent evidence trail (evaluation
    record + gate results + paired results artifact), while CURRENT
    still cannot change — asserted behaviorally (the generations
    table is untouched) and by construction (no evoclj.promotion.*
    namespace exists on the classpath, no promotion alias or public
    function in evoclj.eval.core).

  FIXTURE DESIGN: the store is seeded exactly like the Task 7.6
  candidate tests (a migrated temp database with the seed generation
  row current = 1, a materialized candidate marked
  :evaluation-pending, a temp CAS root). Genome bundles are the
  Task 8.4 paired-fixture bundles (:sci router → :emit, one
  :fixture/echo tool). The parent bundle's route TRANSFORMS the text
  (so the re-evaluated parent fails the selection oracle) while the
  candidate bundle is the identity transform (so the candidate passes
  it) — a genuine utility improvement, not a fabricated summary."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.core :as eval-core]
            [evoclj.eval.replay :as replay]
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

;; --- shared fixture identity (Task 7.6 style) ---------------------------------

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private parent-genome-id
  "The parent Genome the candidate materializes from."
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
  :set-edn op; an optional override map wins."
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
  (let [db-path (temp-path! "evoclj-core-" true)
        db (sqlite/spec db-path)
        cas-root (temp-path! "evoclj-core-cas-")]
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
  "Materialize a fresh candidate from the fixture parent+mutation and
  transition it to :evaluation-pending. Returns the pending Candidate
  record."
  [store]
  (let [m (mutation*)
        c (candidate/create-candidate (candidate-request))
        m1 (candidate/materialize-candidate! store c m)]
    (candidate/mark-evaluation-pending! store (:candidate/id m1))))

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
   (let [dir (temp-path! "core-bundle-")]
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
                           :metadata {:name "core-fixture"
                                      :description "orchestration test bundle"}}))
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

;; --- the orchestrator evaluator ----------------------------------------------

(defn- orchestrator-evaluator
  "A minimal valid evaluator value for evaluate-candidate!. The parent
  bundle's route TRANSFORMS the text (the re-evaluated parent fails
  the selection oracle) while the candidate bundle is the identity
  transform — so the paired comparison shows a genuine utility
  improvement. An optional override map wins."
  [store pending parent-bundle candidate-bundle & [overrides]]
  (merge {:store store
          :provider/catalog (provider-catalog)
          :kernel/abi {:kernel 1 :genome 1 :intent 1 :tool 1}
          :profiles {:test/v1 (test-profile)}
          :genome/roots {generation-id parent-bundle
                         (str (:candidate/id pending)) candidate-bundle}
          :selection/cases {:sel/c1 (selection-case)}
          :selection/fixtures {:fixture/echo (fn [_seed] (seeded-echo-provider))}
          :replay/cases {:replay/c1 (replay-case)}
          :replay/fixtures {:fixture/echo (fn [] (seeded-echo-provider))}
          :programs (fn [_loaded] [(route-descriptor)])
          :measure/cost (fn [_root] 1000.0)}
         overrides))

;; ============================================================================
;; Step 1 — a passing candidate runs the full pipeline and is eligible
;; ============================================================================

(deftest passing-candidate-runs-full-pipeline-and-is-eligible
  (let [store (fresh-store)
        pending (materialized-pending! store)
        ev (orchestrator-evaluator store pending
                                   (bundle! "(str text \"-parent\")")
                                   (bundle! "text"))
        evaluation (eval-core/evaluate-candidate! ev (:candidate/id pending)
                                                  :test/v1)]
    (testing "the evaluation record carries the normative shape"
      (is (uuid? (:evaluation/id evaluation)))
      (is (= (:candidate/id pending) (:candidate/id evaluation)))
      (is (= generation-id (:parent/generation-id evaluation)))
      (is (= :test/v1 (:profile/id evaluation)))
      (is (instance? java.util.Date (:created-at evaluation))))
    (testing "all seven phases ran in the NORMATIVE order and passed"
      (is (= [:G0-parse :G1-schema-abi :G2-static-policy
              :G3-deterministic-suites :G4-replay
              :G5-paired-selection :G6-cost-complexity]
             (mapv :gate/id (:gates evaluation))))
      (is (every? #(= :pass (:status %)) (:gates evaluation))))
    (testing "the eligibility decision is explicit: eligible? true, empty reasons"
      (is (true? (:eligible? (:eligibility evaluation))))
      (is (= [] (:reasons (:eligibility evaluation)))))
    (testing "the summary keeps hard/utility/cost/complexity separate"
      (is (every? #(contains? (:summary evaluation) %)
                  [:hard :utility :cost :complexity]))
      (is (seq (get-in evaluation [:summary :utility :task/success])))
      (is (seq (get-in evaluation [:summary :cost])))
      (is (seq (get-in evaluation [:summary :complexity]))))
    (testing "the paired results artifact ref is a durable content address"
      (let [ref (:paired-results-ref evaluation)]
        (is (re-matches #"^sha256:[0-9a-f]{64}$" ref))
        (is (pos? (alength (cas/get-bytes (:cas store) ref)))
            "the raw paired observations are readable back from the CAS")))))

;; ============================================================================
;; Step 2 — a G2 failure records later gates :not-run (never implicit passes)
;; ============================================================================

(deftest g2-failure-records-later-gates-not-run
  (let [store (fresh-store)
        pending (materialized-pending! store)
        candidate-bundle (bundle! "text")]
    ;; an eval-root file: the candidate would modify the evaluator that
    ;; judges it (Global Constraint 12) — G2 rejects it
    (write-file! (str candidate-bundle "/eval/tamper.edn") "{:x 1}")
    (let [ev (orchestrator-evaluator store pending
                                     (bundle! "text") candidate-bundle)
          evaluation (eval-core/evaluate-candidate! ev (:candidate/id pending)
                                                    :test/v1)]
      (testing "the failing hard gate stops the pipeline; later gates are :not-run"
        (is (= [:G0-parse :G1-schema-abi :G2-static-policy
                :G3-deterministic-suites :G4-replay
                :G5-paired-selection :G6-cost-complexity]
               (mapv :gate/id (:gates evaluation))))
        (is (= [:pass :pass :fail :not-run :not-run :not-run :not-run]
               (mapv :status (:gates evaluation)))))
      (testing "no paired results were produced"
        (is (nil? (:paired-results-ref evaluation))))
      (testing "the eligibility decision is explicit: ineligible with hard reasons"
        (is (false? (:eligible? (:eligibility evaluation))))
        (let [hard (first (:reasons (:eligibility evaluation)))]
          (is (= :hard (:dimension hard)))
          (is (= :gates (:metric hard)))
          (is (= :G2-static-policy
                 (get-in hard [:detail :violations 0 :gate/id])))))
      (testing "the evaluation still completed: the candidate is :evaluated"
        (is (= :evaluated
               (:state (candidate/find-candidate store (:candidate/id pending)))))))))

;; ============================================================================
;; Step 3 — a finalized evaluation is immutable; reruns create new ids
;; ============================================================================

(deftest finalized-evaluation-is-immutable-and-reruns-create-new-ids
  (let [store (fresh-store)
        pending (materialized-pending! store)
        ev (orchestrator-evaluator store pending
                                   (bundle! "text") (bundle! "text"))
        e1 (eval-core/evaluate-candidate! ev (:candidate/id pending) :test/v1)]
    (testing "the returned record is byte-identical to the persisted read-back"
      (is (= e1 (eval-core/find-evaluation ev (:evaluation/id e1)))))
    (testing "the persisted row is finalized (never re-opened)"
      (is (= "finalized"
             (:status (first (sqlite/query
                              (:sqlite store)
                              ["SELECT status FROM eval_runs WHERE id = ?"
                               (str (:evaluation/id e1))]))))))
    (testing "a rerun of the pipeline creates a NEW evaluation id"
      ;; M8 defines no operator re-evaluation API; the test re-arms the
      ;; candidate for a second evaluation cycle by resetting the store
      ;; state — the FIRST evaluation row must stay byte-identical
      (sqlite/with-db [conn (:sqlite store)]
        (jdbc/execute! conn
                       ["UPDATE candidates SET state = 'evaluating' WHERE id = ?"
                        (str (:candidate/id pending))]))
      (let [e2 (eval-core/evaluate-candidate! ev (:candidate/id pending)
                                              :test/v1)]
        (is (not= (:evaluation/id e1) (:evaluation/id e2))
            "a rerun is a NEW evaluation, never a rewrite of the first")
        (is (= e1 (eval-core/find-evaluation ev (:evaluation/id e1)))
            "the first evaluation is unchanged — finalized evaluations are immutable")
        (is (= 2 (count (sqlite/query
                         (:sqlite store)
                         ["SELECT id FROM eval_runs WHERE candidate_id = ?"
                          (str (:candidate/id pending))]))))))))

;; ============================================================================
;; Step 4 — the candidate state transition is transactional
;; ============================================================================

(deftest candidate-transitions-to-evaluated-transactionally
  (let [store (fresh-store)
        pending (materialized-pending! store)
        ev (orchestrator-evaluator store pending
                                   (bundle! "text") (bundle! "text"))]
    (eval-core/evaluate-candidate! ev (:candidate/id pending) :test/v1)
    (testing "the report and the candidate state change together"
      (is (= 1 (count (sqlite/query
                       (:sqlite store)
                       ["SELECT id FROM eval_runs WHERE candidate_id = ?"
                        (str (:candidate/id pending))]))))
      (is (= :evaluated
             (:state (candidate/find-candidate store (:candidate/id pending))))))))

(deftest simulated-persistence-failure-rolls-back-atomically
  (let [store (fresh-store)
        pending (materialized-pending! store)
        ev (orchestrator-evaluator
            store pending (bundle! "text") (bundle! "text")
            {:finalize/before-candidate-update
             (fn [_evaluation]
               (throw (ex-info "injected persistence failure" {:injected true})))})]
    (is (thrown? Exception
                 (eval-core/evaluate-candidate! ev (:candidate/id pending)
                                                :test/v1)))
    (testing "the finalization transaction rolled back: no eval_runs row"
      (is (zero? (count (sqlite/query
                         (:sqlite store)
                         ["SELECT id FROM eval_runs WHERE candidate_id = ?"
                          (str (:candidate/id pending))])))))
    (testing "the candidate state is untouched — still :evaluation-pending"
      (is (= :evaluation-pending
             (:state (candidate/find-candidate store (:candidate/id pending))))))))

;; ============================================================================
;; Step 5 — Milestone 8 exit: eligibility with an evidence trail, no CURRENT
;;          change, no promotion API (asserted by construction)
;; ============================================================================

(deftest milestone-8-exit-eligible-true-with-evidence-trail-and-unchanged-current
  (let [store (fresh-store)
        pending (materialized-pending! store)
        ev (orchestrator-evaluator store pending
                                   (bundle! "(str text \"-parent\")")
                                   (bundle! "text"))
        evaluation (eval-core/evaluate-candidate! ev (:candidate/id pending)
                                                  :test/v1)]
    (testing "a candidate can be declared eligible? true with a complete evidence trail"
      (is (true? (:eligible? (:eligibility evaluation))))
      (is (seq (:gates evaluation)) "gate results are part of the record")
      (is (some? (:paired-results-ref evaluation))
          "the paired results artifact is part of the record")
      (is (seq (:summary evaluation)) "the summary is part of the record"))
    (testing "CURRENT still cannot change — evaluation writes no generation row"
      (is (= 1 (count (sqlite/query
                       (:sqlite store)
                       ["SELECT id FROM generations WHERE current = 1"])))
          "exactly one current generation, as before the evaluation")
      (is (= 1 (:current (first (sqlite/query
                                 (:sqlite store)
                                 ["SELECT current FROM generations WHERE id = ?"
                                  generation-id])))))
      (is (= 1 (count (sqlite/query (:sqlite store)
                                    ["SELECT id FROM generations"])))
          "evaluation never inserts or updates a generation row"))
    (testing "no promotion namespace or API exists, by construction"
      (is (not-any? #(str/starts-with? (str (ns-name %)) "evoclj.promotion")
                    (all-ns)))
      (is (not-any? #(re-find #"(?i)promot|current|activ" (name (key %)))
                    (ns-publics 'evoclj.eval.core)))
      (is (not-any? #(re-find #"(?i)promotion|current" (str %))
                    (map str (keys (ns-aliases 'evoclj.eval.core))))))))

(deftest milestone-8-exit-eligible-false-with-explicit-reasons
  (let [store (fresh-store)
        pending (materialized-pending! store)
        candidate-bundle (bundle! "text")]
    (write-file! (str candidate-bundle "/eval/tamper.edn") "{:x 1}")
    (let [ev (orchestrator-evaluator store pending
                                     (bundle! "text") candidate-bundle)
          evaluation (eval-core/evaluate-candidate! ev (:candidate/id pending)
                                                    :test/v1)]
      (testing "a candidate can be declared eligible? false with explicit reasons"
        (is (false? (:eligible? (:eligibility evaluation))))
        (is (seq (:reasons (:eligibility evaluation))))
        (is (seq (:gates evaluation)))
        (is (nil? (:paired-results-ref evaluation))
            "the evidence trail records exactly what ran: no paired artifact")
        (is (= :evaluated
               (:state (candidate/find-candidate store (:candidate/id pending))))
            "evaluation completed — the candidate is :evaluated, not promoted")))))

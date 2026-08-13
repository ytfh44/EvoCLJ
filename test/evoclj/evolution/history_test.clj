(ns evoclj.evolution.history-test
  "Task 7.7 tests — retained rejection evidence, no immediate
  oscillation.

  The recent-mutation-history store turns the durable lineage rows
  (mutations + candidates from Task 7.6; eval_runs + promotions from
  the Task 5.1 schema, written by the M8 evaluator and M9 promotion
  subsystems) into accepted/rejected mutation summaries with metric
  deltas and rejection reasons (Global Constraint 16: rejected
  mutations remain durable, queryable negative evidence). The four
  normative scenarios, in the task's numbered order:

  - Step 1: an unevaluated mutation reports :pending; once evaluator
    results exist (a FINALIZED eval run, and authoritatively a
    promotion decision row), the rejection reason and metric deltas
    persist and are surfaced. The evaluator/promotion rows are
    inserted directly as fixtures — history owns no decision write
    path (that is the M8/M9 subsystems' job).
  - Step 2: the similarity fingerprint is a deterministic structural
    digest over targeted files + op types + normalized selectors —
    never an LLM semantic judgment. Same file+op+selector with a
    different payload shares a fingerprint; a different file, op
    type, or selector differs; selectors are normalized (form scalar
    ≡ one-element vector, line-ending-normalized anchors, op order
    irrelevant).
  - Step 3: an EXACT repeat of a recently rejected mutation (same
    Task 7.6 :mutation/hash) is flagged :negative-evidence true to
    the Mutator; fingerprint-similar but content-distinct mutations
    are NOT flagged.
  - Step 4: history is evidence only — the public surface is exactly
    the evidence API, no banning/blocking/rejection vocabulary
    exists, and similar future mutations are neither filtered nor
    banned (proposal logic in Task 7.8 decides).

  FIXTURE DESIGN: mirroring the Task 7.6 candidate test, the parent
  generation row is seeded (current = 1) so the candidate lineage FK
  (Database Invariant 8) is exercised against a real row; mutations
  are materialized through evoclj.evolution.candidate so the history
  rows are written by the same path the orchestrator uses.
  Evaluator results are simulated by inserting eval_runs and
  promotions rows directly, exactly as the M8/M9 subsystems will."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.evolution.history :as history]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixture identity --------------------------------------------------

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

(def ^:private eval-summary
  "A Task 8.5-shaped evaluation summary (hard/utility/cost/complexity
  sections; each leaf is {:parent x :candidate y}). Chosen so every
  expected delta is exactly representable, and :integrity carries
  non-numeric :parent/:candidate values (no numeric delta)."
  {:hard {:safety {:parent 1.0 :candidate 1.0 :violations []}
          :integrity {:parent :pass :candidate :pass}}
   :utility {:task/success {:parent 0.5 :candidate 0.75}}
   :cost {:tokens/task {:parent 1200 :candidate 1260}}
   :complexity {:genome-bytes {:parent 18000 :candidate 18600}}})

(def ^:private expected-deltas
  "The metric deltas history must derive from eval-summary:
  candidate - parent per numeric leaf; non-numeric leaves omitted."
  {:hard {:safety 0.0}
   :utility {:task/success 0.25}
   :cost {:tokens/task 60}
   :complexity {:genome-bytes 600}})

(def ^:private rejection-reason
  "A rejection reason of the Task 8.5 eligibility shape."
  "utility regression exceeds the configured cost cap")

(defn- uuid
  "A fixed, readable UUID for fixture ids."
  [n]
  (java.util.UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- mutation*
  "A schema-plausible Mutation IR fixture (Task 7.3 shape) carrying one
  :set-edn op; an optional override map wins (including a wholesale
  :ops replacement)."
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

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil
  when nothing is thrown."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))

;; --- temp stores (test temp dirs only) ---------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-history-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-history-cas-"
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

(defn- fresh-store
  "A migrated temp database seeded with the parent generation row
  (current = 1, Database Invariant 6) plus a temp CAS root. Returns
  the executor :stores map {:sqlite ... :cas ...}."
  []
  (let [path (temp-db-path)
        db (sqlite/spec path)
        cas-root (temp-cas-dir)]
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

(defn- materialize!
  "Materialize a candidate for `m` (default fixture mutation) and
  return the candidate record. The candidate request inherits the
  mutation's :mutation/id and :parent/genome-id so the Task 7.6
  agreement checks hold for non-default parents; `parent-generation`
  overrides the request's :parent/generation-id (default
  generation-id) so the composite lineage FK stays consistent."
  [store & [m parent-generation]]
  (let [m (or m (mutation*))]
    (candidate/materialize-candidate!
     store
     (candidate/create-candidate
      (candidate-request {:mutation/id (:mutation/id m)
                          :parent/genome-id (:parent/genome-id m)
                          :parent/generation-id (or parent-generation
                                                    generation-id)}))
     m)))

(defn- insert-eval-run!
  "Insert an eval_runs row (M8 fixture): the evaluator result for one
  candidate. `n` names the row's uuid."
  [store candidate-id n {:keys [status summary eligibility]}]
  (sqlite/with-db [conn (:sqlite store)]
    (jdbc/insert! conn :eval_runs
                  {:id (str (uuid n))
                   :candidate_id (str candidate-id)
                   :parent_generation_id generation-id
                   :profile_id "default-v1"
                   :gates (pr-str [:g0-parse :g1-schema :g2-static-policy])
                   :paired_results_ref nil
                   :summary (pr-str summary)
                   :eligibility (pr-str eligibility)
                   :status (or status "finalized")
                   :created_at (str "2025-02-0" (inc n) "T00:00:00Z")})))

(defn- insert-promotion!
  "Insert a promotions row (M9 fixture): the authoritative decision for
  one candidate, referencing eval-run uuid `eval-n`. `n` names the
  row's uuid."
  [store candidate-id eval-n n decision reason]
  (sqlite/with-db [conn (:sqlite store)]
    (jdbc/insert! conn :promotions
                  {:id (str (uuid n))
                   :candidate_id (str candidate-id)
                   :evaluation_id (str (uuid eval-n))
                   :from_generation_id generation-id
                   :to_generation_id generation-id
                   :decision (name decision)
                   :reason (pr-str reason)
                   :created_at (str "2025-02-0" (inc n) "T00:00:00Z")})))

;; ============================================================================
;; Step 1 — rejection reason and metric deltas persist once evaluator results
;; exist; until then history reports :pending
;; ============================================================================

(deftest step-1-history-reports-pending-until-evaluator-results-exist
  (let [store (fresh-store)
        _ (materialize! store)
        [entry] (history/recent-mutation-history store [generation-id])]
    (testing "an unevaluated mutation reports :pending with no reason or deltas"
      (is (= 1 (count (history/recent-mutation-history store [generation-id]))))
      (is (= :pending (:state entry)))
      (is (nil? (:reason entry)))
      (is (nil? (:metric-deltas entry)))
      (is (false? (:negative-evidence entry))))
    (testing "a RUNNING (unfinalized) eval run is not a result yet — still :pending"
      (insert-eval-run! store (:candidate/id entry) 1
                        {:status "running"
                         :summary eval-summary
                         :eligibility {:eligible? false :reasons ["not yet finalized"]}})
      (let [[e2] (history/recent-mutation-history store [generation-id])]
        (is (= :pending (:state e2)))
        (is (nil? (:reason e2)))))
    (testing "once a FINALIZED eval run exists, the rejection reason and metric
              deltas persist and are surfaced"
      (insert-eval-run! store (:candidate/id entry) 2
                        {:summary eval-summary
                         :eligibility {:eligible? false :reasons [rejection-reason]}})
      (let [[e3] (history/recent-mutation-history store [generation-id])]
        (is (= :rejected (:state e3)))
        (is (= [rejection-reason] (:reason e3)))
        (is (= expected-deltas (:metric-deltas e3)))))))

(deftest step-1-eligible-evaluation-reports-accepted
  (let [store (fresh-store)
        c (materialize! store)
        _ (insert-eval-run! store (:candidate/id c) 1
                            {:summary eval-summary
                             :eligibility {:eligible? true :reasons []}})
        [entry] (history/recent-mutation-history store [generation-id])]
    (testing "a finalized eligible run reads :accepted with metric deltas and no reason"
      (is (= :accepted (:state entry)))
      (is (nil? (:reason entry)))
      (is (= expected-deltas (:metric-deltas entry))))))

(deftest step-1-a-promotion-decision-is-authoritative
  (let [store (fresh-store)
        c (materialize! store)
        _ (insert-eval-run! store (:candidate/id c) 1
                            {:summary eval-summary
                             :eligibility {:eligible? true :reasons []}})
        promotion-reason {:decision :rejected
                          :rationale "utility regression exceeds the cost cap"}
        _ (insert-promotion! store (:candidate/id c) 1 2 :rejected promotion-reason)
        [entry] (history/recent-mutation-history store [generation-id])]
    (testing "the promotion decision (rejected) wins over a permissive eligibility"
      (is (= :rejected (:state entry)))
      (is (= promotion-reason (:reason entry)))
      (is (= expected-deltas (:metric-deltas entry))
          "metric deltas still come from the eval run's summary"))
    (testing "a promoted decision reads :accepted with the promotion reason"
      (let [store2 (fresh-store)
            c2 (materialize! store2)
            _ (insert-eval-run! store2 (:candidate/id c2) 1
                                {:summary eval-summary
                                 :eligibility {:eligible? false :reasons ["utility regression"]}})
            _ (insert-promotion! store2 (:candidate/id c2) 1 2 :promoted
                                 {:decision :promoted :rationale "lexicographic comparison passed"})
            [entry2] (history/recent-mutation-history store2 [generation-id])]
        (is (= :accepted (:state entry2)))
        (is (= {:decision :promoted :rationale "lexicographic comparison passed"}
               (:reason entry2)))))))

;; ============================================================================
;; Step 2 — the similarity fingerprint: files + op types + normalized selectors
;; ============================================================================

(deftest step-2-fingerprint-identifies-the-structural-target
  (let [base (mutation*)]
    (testing "the fingerprint is a canonical sha256 digest"
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (history/mutation-fingerprint base)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$"
                      (history/op-fingerprint (first (:ops base))))))
    (testing "same file+op+selector with a DIFFERENT payload has the same fingerprint"
      (is (= (history/mutation-fingerprint base)
             (history/mutation-fingerprint
              (assoc-in base [:ops 0 :value] [:different :payload]))))
      (is (= (history/mutation-fingerprint base)
             (history/mutation-fingerprint
              (assoc-in base [:ops 0 :expect/hash]
                        (str "sha256:" (apply str (repeat 64 "0")))))))
      (is (= (history/op-fingerprint (first (:ops base)))
             (history/op-fingerprint
              {:op :set-edn :file "skills/debugging.edn"
               :path [:workflow :before-edit]
               :expect/hash file-hash :value 42}))))
    (testing "a different file, op type, or selector yields a different fingerprint"
      (is (not= (history/mutation-fingerprint base)
                (history/mutation-fingerprint
                 (assoc-in base [:ops 0 :file] "skills/other.edn"))))
      (is (not= (history/mutation-fingerprint base)
                (history/mutation-fingerprint
                 (assoc-in base [:ops 0]
                           {:op :delete-edn :file "skills/debugging.edn"
                            :path [:workflow :before-edit]
                            :expect/hash file-hash}))))
      (is (not= (history/mutation-fingerprint base)
                (history/mutation-fingerprint
                 (assoc-in base [:ops 0 :path] [:workflow :after-edit])))))))

(deftest step-2-normalized-selectors
  (testing "form selector scalar ≡ one-element vector (the schema admits both spellings)"
    (let [a (mutation* {:ops [{:op :replace-form
                               :file "programs/route.clj"
                               :selector :route
                               :form '(defn route [] :x)
                               :expect/hash file-hash}]})
          b (mutation* {:ops [{:op :replace-form
                               :file "programs/route.clj"
                               :selector [:route]
                               :form '(defn route [] :y)
                               :expect/hash file-hash}]})]
      (is (= (history/mutation-fingerprint a)
             (history/mutation-fingerprint b)))))
  (testing "ops in a different ORDER have the same fingerprint (the target set
            is order-insensitive)"
    (let [op-a (first (:ops (mutation*)))
          op-b {:op :insert-text :file "skills/debugging.edn"
                :position :before :anchor "localize" :text "x"}
          m1 (mutation* {:ops [op-a op-b]})
          m2 (mutation* {:ops [op-b op-a]})]
      (is (= (history/mutation-fingerprint m1)
             (history/mutation-fingerprint m2)))))
  (testing "text anchors are line-ending normalized"
    (let [a (mutation* {:ops [{:op :insert-text :file "skills/debugging.edn"
                               :position :before :anchor "a\r\nb" :text "x"}]})
          b (mutation* {:ops [{:op :insert-text :file "skills/debugging.edn"
                               :position :before :anchor "a\nb" :text "x"}]})]
      (is (= (history/mutation-fingerprint a)
             (history/mutation-fingerprint b)))))
  (testing "a genuinely different selector still differs"
    (is (not= (history/mutation-fingerprint
               (mutation* {:ops [{:op :insert-text :file "skills/debugging.edn"
                                  :position :before :anchor "localize" :text "x"}]}))
              (history/mutation-fingerprint
               (mutation* {:ops [{:op :insert-text :file "skills/debugging.edn"
                                  :position :before :anchor "reproduce" :text "x"}]}))))))

;; ============================================================================
;; Step 3 — an exact repeat of a recently rejected mutation is flagged to the
;; Mutator as negative evidence
;; ============================================================================

(deftest step-3-exact-repeats-of-rejected-mutations-are-flagged
  (let [store (fresh-store)
        original (mutation*)
        repeat (mutation* {:mutation/id (uuid 2)}) ; identical content, fresh uuid
        c1 (materialize! store original)
        _ (insert-eval-run! store (:candidate/id c1) 1
                            {:summary eval-summary
                             :eligibility {:eligible? false :reasons [rejection-reason]}})
        _ (materialize! store repeat)
        entries (history/recent-mutation-history store [generation-id])]
    (is (= 2 (count entries)) "both proposals stay durable — history is per proposal")
    (let [newest (first entries)
          oldest (second entries)]
      (is (= (uuid 2) (:mutation/id newest)))
      (is (= :rejected (:state newest)))
      (is (= (:mutation/hash newest) (:mutation/hash oldest))
          "exact repeat = the Task 7.6 content hash is identical")
      (is (true? (:negative-evidence newest))
          "the exact repeat is flagged to the Mutator as negative evidence")
      (is (false? (:negative-evidence oldest))
          "the original rejection is not itself a repeat"))))

(deftest step-3-fingerprint-similarity-is-evidence-not-a-repeat
  (let [store (fresh-store)
        original (mutation*)
        similar (mutation* {:mutation/id (uuid 3)
                            :ops [{:op :set-edn :file "skills/debugging.edn"
                                   :path [:workflow :before-edit]
                                   :expect/hash file-hash
                                   :value [:different :payload]}]})
        c1 (materialize! store original)
        _ (insert-eval-run! store (:candidate/id c1) 1
                            {:summary eval-summary
                             :eligibility {:eligible? false :reasons [rejection-reason]}})
        _ (materialize! store similar)
        entries (history/recent-mutation-history store [generation-id])]
    (is (= 2 (count entries)))
    (let [newest (first entries)
          oldest (second entries)]
      (is (= (uuid 3) (:mutation/id newest)))
      (is (= (:fingerprint newest) (:fingerprint oldest))
          "the similar mutation shares the structural fingerprint")
      (is (not= (:mutation/hash newest) (:mutation/hash oldest))
          "but it is NOT an exact content repeat")
      (is (false? (:negative-evidence newest))
          "fingerprint similarity alone is never auto-flagged — proposal logic decides")
      (is (some #(= (uuid 3) (:mutation/id %)) entries)
          "the similar mutation remains in history — nothing is banned or filtered"))))

(deftest step-3-repeats-of-pending-or-accepted-mutations-are-not-flagged
  (let [store (fresh-store)
        pending (mutation* {:mutation/id (uuid 4)
                            :ops [{:op :insert-text :file "skills/debugging.edn"
                                   :position :before :anchor "localize" :text "x"}]})
        repeat (assoc pending :mutation/id (uuid 5))
        _ (materialize! store pending)
        _ (materialize! store repeat)
        entries (history/recent-mutation-history store [generation-id])]
    (is (= 2 (count entries)))
    (is (false? (:negative-evidence (first entries)))
        "a repeat of a :pending mutation is not negative evidence")
    (let [store2 (fresh-store)
          accepted (mutation* {:mutation/id (uuid 6)
                               :ops [{:op :insert-text :file "skills/debugging.edn"
                                      :position :before :anchor "reproduce" :text "x"}]})
          repeat2 (assoc accepted :mutation/id (uuid 7))
          c (materialize! store2 accepted)
          _ (insert-eval-run! store2 (:candidate/id c) 1
                              {:summary eval-summary
                               :eligibility {:eligible? true :reasons []}})
          _ (materialize! store2 repeat2)
          entries2 (history/recent-mutation-history store2 [generation-id])]
      (is (= 2 (count entries2)))
      (is (= :accepted (:state (first entries2))))
      (is (false? (:negative-evidence (first entries2)))
          "a repeat of an :accepted mutation is not negative evidence"))))

;; ============================================================================
;; Step 4 — history is evidence only; no banning logic lives here
;; ============================================================================

(deftest step-4-history-is-evidence-only-no-banning-logic
  (let [publics (ns-publics 'evoclj.evolution.history)
        names (set (map (comp name key) publics))]
    (testing "the public surface is exactly the evidence API"
      (is (= #{"recent-mutation-history" "mutation-fingerprint" "op-fingerprint"}
             names)))
    (testing "no public function claims to ban, block, reject, or suppress
              future mutations"
      (is (not-any? #(re-find #"(?i)ban|block|reject|suppress|prevent|veto|filter|skip" %)
                    names)))
    (testing "no dependency on the Task 7.8 proposal logic or the Mutator"
      (is (not-any? #(re-find #"(?i)proposal|mutator" (str %))
                    (map str (keys (ns-aliases 'evoclj.evolution.history))))))))

;; ============================================================================
;; scoping and bounds
;; ============================================================================

(deftest history-is-scoped-to-the-lineage-and-bounded
  (let [store (fresh-store)
        other-genome (str "sha256:" (apply str (repeat 64 "b")))
        other-generation "generation-2"
        _ (sqlite/with-db [conn (:sqlite store)]
            (jdbc/insert! conn :generations
                          {:id other-generation
                           :genome_id other-genome
                           :resolution_id resolution-id
                           :parent_id generation-id
                           :state "active"
                           :current 0
                           :created_at "2025-01-02T00:00:00Z"}))
        _ (materialize! store (mutation* {:mutation/id (uuid 1)}))
        _ (materialize! store (mutation* {:mutation/id (uuid 5)
                                          :parent/genome-id other-genome})
                         other-generation)
        in-lineage (history/recent-mutation-history store [generation-id])
        both (history/recent-mutation-history store [generation-id other-generation])]
    (testing "only mutations whose candidate's parent generation is IN the
              lineage appear"
      (is (= 1 (count in-lineage)))
      (is (= (uuid 1) (:mutation/id (first in-lineage))))
      (is (= 2 (count both))))
    (testing "the window limit bounds the result, newest proposal first"
      (is (= 1 (count (history/recent-mutation-history
                       store [generation-id other-generation] {:limit 1}))))
      (is (= (uuid 5) (:mutation/id (first (history/recent-mutation-history
                                            store [generation-id other-generation]
                                            {:limit 1}))))))))

(deftest the-request-and-store-boundaries-are-validated
  (let [store (fresh-store)]
    (is (= :history/store-invalid
           (thrown-error-type #(history/recent-mutation-history nil [generation-id]))))
    (is (= :history/store-invalid
           (thrown-error-type #(history/recent-mutation-history {} [generation-id]))))
    (is (= :history/request-invalid
           (thrown-error-type #(history/recent-mutation-history store []))))
    (is (= :history/request-invalid
           (thrown-error-type #(history/recent-mutation-history store [generation-id]
                                                               {:limit 0}))))
    (is (= :history/request-invalid
           (thrown-error-type #(history/recent-mutation-history store [generation-id]
                                                               {:limit 1000000}))))
    (is (= :history/request-invalid
           (thrown-error-type #(history/recent-mutation-history store [generation-id]
                                                               {:bogus 1}))))))

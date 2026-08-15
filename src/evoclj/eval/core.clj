(ns evoclj.eval.core
  "Task 8.7 — end-to-end candidate evaluation orchestration.

  evaluate-candidate! runs the NORMATIVE phase order for one candidate
  under one profile and returns an IMMUTABLE Evaluation record:

      (evaluate-candidate! evaluator candidate-id profile-id)
      ;; => {:evaluation/id <uuid>
      ;;     :candidate/id <uuid>
      ;;     :parent/generation-id <stable-id>
      ;;     :profile/id <keyword>
      ;;     :gates [<gate result maps>]
      ;;     :paired-results-ref <ArtifactId or nil>
      ;;     :summary {:hard ... :utility ... :cost ... :complexity ...}
      ;;     :eligibility {:eligible? bool :reasons [<reason maps>]}
      ;;     :created-at <inst>}

  PHASE ORDER (normative, Task 8.7 — a failed HARD gate records later
  gates as :not-run, never implicit passes):

      G0 parse (evoclj.eval.gates/g0-parse — from scratch)
      → G1 schema/ABI (g1-schema-abi)
      → G2 static policy (g2-static-policy)
      → G3 deterministic suites (g3-deterministic-suites)
      → G4 replay (evoclj.eval.replay/run-replay!)
      → G5 paired hidden selection (evoclj.eval.paired/run-paired-selection!)
      → G6 cost/complexity guardrails (measured, compared lexicographically)
      → eligibility summary (evoclj.eval.compare/eligibility)

  G0–G3 run through gates/run-gates-until-hard-failure (all four are
  :hard? true; a non-pass stops the front). G4 is :fail when the
  replay report reports :hard-failure? (a critical regression); G5 is
  :fail when the paired run has a critical case the candidate did not
  win; G6 is :fail when a measured cost or (profile-declared)
  complexity regression exceeds the profile's guardrail. Every later
  phase after a non-pass is recorded as :not-run with the SAME gate
  shape — evidence that it did not silently pass.

  THE EVALUATOR VALUE (host-constructed, kernel-owned — Global
  Constraint 19; never agent-mutable):

      {:store {:sqlite <db> :cas <CAS>}                ; REQUIRED
       :provider/catalog <catalog map>                 ; REQUIRED
       :kernel/abi {:kernel n :genome n :intent n :tool n} ; REQUIRED
       :profiles {<profile-id keyword> <profile>}      ; REQUIRED
       :genome/roots {<side-id string> <bundle root>}  ; REQUIRED
       :replay/cases {<case-id> <replay case>}         ; REQUIRED
       :replay/fixtures {<tool-id> <provider | 0-ary fn>} ; REQUIRED
       :selection/fixtures {<tool-id> <fn | provider>} ; REQUIRED
       :selection/cases {<case-id> <selection case>}   ; OPTIONAL —
       ;   falls back to dataset/selection-loader over :dataset/roots
       :dataset/roots {<source> <root>}                ; OPTIONAL —
       ;   the physical dataset separation contract (Global Constraint
       ;   11): carried and validated, never mounted into any
       ;   workspace (Global Constraint 23)
       :model/registry <model registry atom>           ; OPTIONAL —
       ;   the kernel-owned model registry (result of
       ;   evoclj.provider.model-registry/build-model-registry); when
       ;   present the G5 runner injects it into the broker context and
       ;   grants a model lease so :llm topologies run with real
       ;   providers. Absent → an :llm genome fails closed with
       ;   :provider/not-found :reason :no-model-registry. Passed
       ;   through unchanged to evoclj.eval.runner/run-side!.
       :model/resource {:kind :model :id \"<provider>/*\"} ; OPTIONAL —
       ;   the model resource template the G5 model lease grants (the
       ;   prefix model leases authorize). Optional even when
       ;   :model/registry is present; default {:kind :model :id
       ;   \"*/*\"} (matches no concrete id — fail-closed). See
       ;   evoclj.eval.runner.
       :programs <resolver fn | vector>                ; OPTIONAL
       :equivalence/by-keyword <kw -> fn>              ; OPTIONAL
       :artifact/root <dir>                            ; OPTIONAL — G5
       :seed <string>                                  ; OPTIONAL — G5
       :workspace/root <dir>                           ; OPTIONAL — G3
       :measure/cost (fn [bundle-root] -> number)      ; OPTIONAL — G6;
       ;   when present it is the v0 cost instrument. When ABSENT but
       ;   the G5 paired result carries real model usage, the :cost
       ;   section is DERIVED from the aggregated :model-cost-units
       ;   (:provider-reported-cost fallback) of the parent and
       ;   candidate sides (Feature C). Only when NEITHER exists is
       ;   the :cost section EMPTY — no fabricated cost claims (v0 has
       ;   no scheduler token/latency telemetry contract, Global
       ;   Constraint 24)
       :store-details! (fn [details] -> ref)           ; OPTIONAL —
       ;   default: CAS content-hash artifact ref (Global
       ;   Constraint 21)
       :finalize/before-candidate-update (fn [evaluation]) ; OPTIONAL —
       ;   failure-injection hook documented under the transaction}

  :genome/roots maps the G5 side ids — (:parent/generation-id) and the
  candidate uuid STRING — to bundle roots. The evaluator only CONSUMES
  these roots (it re-loads and re-compiles from scratch, never a
  cached Mutator claim; Global Constraints 4, 6); the evolution layer
  stages candidate workspaces, the evaluator never writes to them
  (Global Constraint 23). :parent/capabilities for G2 is derived by
  RE-READING the parent bundle's manifest — never a host-supplied
  claim.

  EVALUATION SUMMARY (built from the gates + the paired results):
  every section stays separate (Global Constraint 14):

      {:hard {:gates  {:parent :pass :candidate <:pass|:fail>
                       :violations [<non-pass gate results>]}
              :replay {:parent :pass :candidate ... 
                       :violations [<critical regressions>]}
              :paired {:parent :pass :candidate ...
                       :violations [<critical paired losses>]}}
       :utility {:task/success {:parent rate :candidate rate}}  ; from G5
       :cost {:cost/units {:parent n :candidate n}}            ; :measure/cost,
       ;   else derived from parent/candidate model usage; else {}
       :complexity {:genome-bytes {...} :graph-nodes {...}}}   ; measured

  The eligibility decision is compare/eligibility over that summary —
  lexicographic: hard first, then utility, then cost, then complexity
  (Task 8.5; a hard failure short-circuits everything downstream).

  THE FINALIZATION TRANSACTION (documented; the plan's 'Evaluation
  finalization transaction'): all gate/case artifacts are already
  durable (gate details, the replay report, the paired case artifacts,
  and the raw paired observations are all CAS content-addressed BEFORE
  the SQL transaction opens). ONE SQL transaction then (1) inserts the
  eval_runs row (status 'finalized' — the evaluation is born
  finalized, never mutable), and (2) CAS-updates the candidate row
  state 'evaluating' → 'eligible' (the Task 7.6 machine's
  :evaluation-pending → :evaluated edge, realized at the row boundary
  per the documented vocabulary mapping in evoclj.evolution.candidate
  — 'evaluating' ↔ :evaluation-pending, 'eligible' ↔ :evaluated).
  Either step failing rolls BOTH back atomically: no eval_runs row,
  candidate still :evaluation-pending. The optional
  :finalize/before-candidate-update hook (a fn receiving the
  evaluation) runs INSIDE the transaction after the insert and before
  the state update; its only purpose in v0 is failure-injection
  testing — a throwing hook proves the transaction rolls back.

  IMMUTABILITY (Database Invariant 4): the eval_runs row is written
  once, status 'finalized', and no update path exists in this
  namespace; a rerun of the pipeline creates a NEW evaluation id. The
  persisted row stores references and EDN summaries — never duplicated
  payload bodies (Global Constraint 21).

  DEVIATIONS (documented, per Repo Convention 5):
  - evoclj.evolution.candidate is NOT edited (task file restriction).
    The :evaluation-pending → :evaluated edge is realized here by the
    SQL CAS inside the finalization transaction, with the same
    vocabulary mapping candidate.clj documents. The :invalid state
    (also named for M8 there) has no Task 5.1 schema value; this task
    resolves only the :evaluated leg — every completed evaluation
    (eligible or not) lands the candidate on :evaluated. Promotion
    (M9) decides :rejected/:canary/:promoted from the eligibility
    DATA.
  - statistics/promotion-checks (Task 8.6 Step 5) are NOT wired into
    eligibility: the Task 8.1 profile schema (closed) cannot carry
    :min-pairs/:max-candidate-failure-rate without an edit to
    evoclj.eval.profile, which this task must not touch. The checks
    remain pure, available data; wiring is trivially additive in a
    task that owns the profile schema.
  - No eval event rows (:eval/started etc.) are appended: evaluations
    run in fresh isolated sessions and have no parent session row; the
    eval_runs row is the durable eval record (YAGNI, Global
    Constraint 24).

  Error contract (Global Constraint 22 — plain serializable data):
  :eval/evaluator-invalid, :eval/profile-unknown,
  :eval/candidate-not-found, :eval/candidate-state-invalid,
  :eval/genome-unresolved, :eval/measure-invalid. Gate/phase
  exceptions become :fail/:error gate results with persisted details;
  the failure-injection hook's throw propagates (the transaction has
  already rolled back)."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.eval.compare :as compare]
            [evoclj.eval.dataset :as dataset]
            [evoclj.eval.gates :as gates]
            [evoclj.eval.metrics :as metrics]
            [evoclj.eval.paired :as paired]
            [evoclj.eval.profile :as profile]
            [evoclj.eval.replay :as replay]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.genome.load :as load]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

;; --- the normative phase order -------------------------------------------------

(def phase-ids
  "The NORMATIVE Task 8.7 phase order."
  [:G0-parse :G1-schema-abi :G2-static-policy
   :G3-deterministic-suites :G4-replay
   :G5-paired-selection :G6-cost-complexity])

;; --- the Evaluation contract ---------------------------------------------------

(def EvaluationSchema
  "The public Evaluation record contract (docs 'Detailed Public Data
  Contracts'). :summary and :eligibility are validated further by
  metrics/validate-summary! and the eligibility shape."
  [:map {:closed true}
   [:evaluation/id uuid?]
   [:candidate/id uuid?]
   [:parent/generation-id string?]
   [:profile/id keyword?]
   [:gates vector?]
   [:paired-results-ref [:or nil? [:fn types/artifact-id?]]]
   [:summary map?]
   [:eligibility [:map {:closed true}
                  [:eligible? boolean?]
                  [:reasons vector?]]]
   [:created-at [:fn inst?]]])

(defn- validate-evaluation!
  "Validate an Evaluation record against the contract; returns it
  unchanged or throws :eval/evaluator-invalid with humanized
  explanations."
  [evaluation]
  (when-let [expl (m/explain EvaluationSchema evaluation)]
    (throw (err/error :eval/evaluator-invalid
                      "evaluation does not satisfy the Evaluation contract"
                      {:errors (me/humanize expl)})))
  evaluation)

;; --- evaluator boundary validation ---------------------------------------------

(defn- evaluator-error
  [reason message value]
  (err/error :eval/evaluator-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- validate-evaluator!
  [evaluator]
  (when-not (map? evaluator)
    (throw (evaluator-error :not-a-map
                            "evaluator must be a map" evaluator)))
  (doseq [[k reason] [[:store :store-missing]
                      [:provider/catalog :catalog-missing]
                      [:kernel/abi :abi-missing]
                      [:profiles :profiles-missing]
                      [:genome/roots :genome-roots-missing]
                      [:replay/cases :replay-cases-missing]
                      [:replay/fixtures :replay-fixtures-missing]
                      [:selection/fixtures :selection-fixtures-missing]]]
    (when-not (contains? evaluator k)
      (throw (evaluator-error reason
                              (str "evaluator is missing the " k " key")
                              evaluator))))
  (let [store (:store evaluator)]
    (when-not (and (map? store) (contains? store :sqlite)
                   (contains? store :cas))
      (throw (evaluator-error :store-invalid
                              ":store must be the executor stores map {:sqlite ... :cas ...}"
                              store))))
  (when-not (map? (:profiles evaluator))
    (throw (evaluator-error :profiles-invalid
                            ":profiles must be a map of profile-id -> profile"
                            (:profiles evaluator))))
  (when-not (map? (:genome/roots evaluator))
    (throw (evaluator-error :genome-roots-invalid
                            ":genome/roots must be a map of side-id -> bundle root"
                            (:genome/roots evaluator))))
  (when-let [roots (:dataset/roots evaluator)]
    (when-not (map? roots)
      (throw (evaluator-error :dataset-roots-invalid
                              ":dataset/roots must be a source -> root map"
                              roots))))
  (when (and (not (contains? evaluator :selection/cases))
             (not (contains? evaluator :dataset/roots)))
    (throw (evaluator-error :selection-cases-missing
                            "evaluator must carry :selection/cases or :dataset/roots"
                            evaluator)))
  evaluator)

;; --- timestamps ----------------------------------------------------------------

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  "Canonical ISO-8601 UTC string for a Date/Instant/nil (now)."
  [ts]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               :else (throw (err/error :eval/evaluator-invalid
                                       "timestamp must be a Date, Instant, or nil"
                                       {:timestamp ts})))]
    (.format timestamp-fmt inst)))

;; --- CAS persistence (Global Constraint 21) -------------------------------------

(defn- put-artifact!
  "Persist `value` by content hash into the evaluator's CAS and return
  its canonical ArtifactId. The body is the pr-str — identical values
  always map to the same id, so re-persisting is idempotent."
  [evaluator value]
  (:artifact/id
   (cas/put-bytes! (:cas (:store evaluator))
                   (.getBytes (pr-str value) StandardCharsets/UTF_8)
                   {})))

(defn- details-store
  "The details persistence fn for this run: the evaluator's
  :store-details! when given, else a CAS content-hash artifact ref
  (the same convention the Task 8.2 gates default to, made durable in
  the CAS)."
  [evaluator]
  (or (:store-details! evaluator)
      (fn [details] (put-artifact! evaluator details))))

;; --- resolution helpers -----------------------------------------------------------

(defn- resolve-profile!
  "The profile registered under `profile-id`, validated against the
  Task 8.1 contract; an unknown id fails closed."
  [evaluator profile-id]
  (let [p (get (:profiles evaluator) profile-id)]
    (when-not p
      (throw (err/error :eval/profile-unknown
                        "no evaluation profile registered under this id"
                        {:profile/id profile-id
                         :known (vec (sort (keys (:profiles evaluator))))})))
    (profile/validate-profile! p)
    p))

(defn- resolve-root!
  "The bundle root registered under `side-id` (a string: the parent
  generation id or the candidate uuid string); unknown ids fail
  closed."
  [evaluator side-id]
  (or (get (:genome/roots evaluator) side-id)
      (throw (err/error :eval/genome-unresolved
                        "no genome bundle root registered for this side id"
                        {:side/id side-id
                         :known (vec (sort (keys (:genome/roots evaluator))))}))))

(defn- resolve-candidate!
  "The persisted Candidate record for `candidate-id`, required to be in
  :evaluation-pending (the machine's edge precondition)."
  [evaluator candidate-id]
  (let [c (candidate/find-candidate (:store evaluator) candidate-id)]
    (when-not c
      (throw (err/error :eval/candidate-not-found
                        "no candidate with this id"
                        {:candidate/id candidate-id})))
    (when-not (= :evaluation-pending (:state c))
      (throw (err/error :eval/candidate-state-invalid
                        "evaluate-candidate! accepts only :evaluation-pending candidates"
                        {:candidate/id (:candidate/id c)
                         :state (:state c)})))
    c))

(defn- parent-capabilities
  "The parent's requested capabilities, re-read from the parent
  bundle's manifest (never a host-supplied claim)."
  [parent-root]
  (set (:capabilities/requested (:manifest (load/load-genome parent-root)))))

(defn- selection-cases
  "The G5 selection cases: the evaluator's :selection/cases map, or —
  when absent — freshly loaded from the profile's selection source
  through the evaluator-only dataset/selection-loader (Global
  Constraint 11: selection bodies load only in evaluator code)."
  [evaluator profile]
  (if-let [cases (:selection/cases evaluator)]
    cases
    (let [roots (or (:dataset/roots evaluator) dataset/dataset-roots)]
      (into {} (map (fn [c] [(:case/id c) c]))
            ((dataset/selection-loader profile roots))))))

;; --- the gate / phase contexts -----------------------------------------------------

(defn- gate-context
  "The Task 8.2 gate context for the candidate bundle: the candidate
  root, the provider catalog, the kernel ABI, the parent's
  capabilities (re-read from the parent manifest), the program
  registry, the G3 staging parent, and the details store."
  [evaluator candidate-root parent-root]
  {:candidate/root candidate-root
   :provider/catalog (:provider/catalog evaluator)
   :kernel/abi (:kernel/abi evaluator)
   :parent/capabilities (parent-capabilities parent-root)
   :programs (:programs evaluator)
   :workspace/root (:workspace/root evaluator)
   :store-details! (details-store evaluator)})

(defn- replay-context
  "The Task 8.3 replay evaluator context (a subset of the orchestrator
  evaluator)."
  [evaluator]
  (select-keys evaluator
               [:provider/catalog :replay/cases :replay/fixtures
                :programs :equivalence/by-keyword]))

(defn- paired-context
  "The Task 8.4 paired-selection evaluator context (a subset of the
  orchestrator evaluator — the same :genome/roots and :programs, plus
  the optional :model/registry / :model/resource keys that switch on
  real model execution for :llm topologies)."
  [evaluator]
  (select-keys evaluator
               [:provider/catalog :selection/cases :selection/fixtures
                :programs :seed :equivalence/by-keyword :artifact/root
                :genome/roots :model/registry :model/resource]))

;; --- :not-run records (never implicit passes) ----------------------------------------

(defn- not-run-gate
  "The explicit :not-run record for a phase that a hard failure made
  unreachable — evidence that the phase did NOT silently pass."
  [gate-id]
  {:gate/id gate-id :status :not-run :hard? true
   :details-ref nil :duration-ms 0})

(defn- not-run-after
  "Every phase after the last recorded one, as explicit :not-run gate
  results in phase order."
  [gate-results]
  (->> (remove (set (map :gate/id gate-results)) phase-ids)
       (mapv not-run-gate)))

;; --- the phase envelope ---------------------------------------------------------------

(defn- elapsed
  [t0]
  (quot (- (System/nanoTime) t0) 1000000))

(defn- phase-gate
  "Run one phase's outcome-producing fn inside the normative gate
  result envelope. `status-fn` derives the gate :status from the
  report; a typed ExceptionInfo becomes a :fail gate with persisted
  error details and an unexpected throwable a :error gate — the same
  policy as the Task 8.2 gates, so a throwing phase can never
  masquerade as a pass. Returns {:gate <result map> :report <fn's
  return or nil>}."
  [store-details gate-id status-fn f]
  (let [t0 (System/nanoTime)
        gate (fn [status details report]
               {:gate {:gate/id gate-id
                       :status status
                       :hard? true
                       :details-ref (when details (store-details details))
                       :duration-ms (elapsed t0)}
                :report report})]
    (try
      (let [report (f)]
        (gate (status-fn report) report report))
      (catch clojure.lang.ExceptionInfo e
        (gate :fail (err/error-data e) nil))
      (catch Throwable t
        (gate :error {:error/message (.getMessage t)
                      :error/class (.getName (.getClass t))} nil)))))

;; --- the phases ---------------------------------------------------------------------

(defn- run-g4-phase!
  "G4 historical replay (Task 8.3): re-walk the candidate against the
  evaluator's replay cases. The gate fails on :hard-failure? (any
  critical regression)."
  [evaluator candidate-root]
  (phase-gate (details-store evaluator) :G4-replay
              #(if (:hard-failure? %) :fail :pass)
              (fn []
                (replay/run-replay! (replay-context evaluator)
                                    candidate-root
                                    (vec (keys (:replay/cases evaluator)))))))

(defn- paired-critical-violations
  "The G5 hard evidence: every pair whose case is :critical? and whose
  outcome the candidate did not win (:parent-wins or :both-failed).
  A critical case must be won, not merely tied or survived."
  [paired-result]
  (->> (:pairs paired-result)
       (filter #(get-in % [:case/outcome :critical?]))
       (filter #(contains? #{:parent-wins :both-failed}
                           (get-in % [:case/outcome :status])))
       (mapv (fn [p] (select-keys (:case/outcome p)
                                  [:case/id :repetition :status
                                   :score/parent :score/candidate])))))

(defn- raw-paired-observations
  "The RAW paired observations in the Task 8.6 statistics shape —
  the durable recomputable record the :paired-results-ref points at."
  [paired-result]
  (mapv (fn [p] {:parent (get-in p [:case/outcome :score/parent])
                 :candidate (get-in p [:case/outcome :score/candidate])})
        (:pairs paired-result)))

(defn- run-g5-phase!
  "G5 paired hidden selection (Task 8.4): re-evaluate the parent NOW
  against the candidate on the same selection cases, same derived
  seeds, same repetitions. The gate fails when a critical paired case
  was lost."
  [evaluator c profile case-set]
  (phase-gate (details-store evaluator) :G5-paired-selection
              #(if (seq (paired-critical-violations %)) :fail :pass)
              (fn []
                (paired/run-paired-selection!
                 (paired-context evaluator)
                 {:parent-generation (:parent/generation-id c)
                  :candidate-id (str (:candidate/id c))
                  :case-set case-set
                  :repetitions (or (:repetitions profile) 1)}))))

;; --- G6 measurement (cost/complexity guardrails) ---------------------------------------

(defn- bundle-bytes
  "The total on-disk bytes of every regular file in a bundle — the
  deterministic content-based genome-size measure."
  [root]
  (let [base (Paths/get root (make-array String 0))]
    (with-open [stream (Files/walk base (make-array FileVisitOption 0))]
      (reduce + 0
              (map (fn [^java.nio.file.Path p]
                     (if (Files/isRegularFile p (make-array LinkOption 0))
                       (Files/size p)
                       0))
                   (iterator-seq (.iterator stream)))))))

(defn- topology-nodes
  "The number of nodes in a bundle's compiled topology (read from the
  bundle's declared topology module)."
  [root]
  (let [loaded (load/load-genome root)
        topo-path (get-in loaded [:manifest :modules :topology])
        source (String. ^bytes
                        (byte-array (:bytes (get-in loaded [:files topo-path])))
                        StandardCharsets/UTF_8)]
    (count (:nodes (edn/read-string source)))))

(defn- usage-model-cost-units
  "The model cost units from one side's aggregated runtime.usage
  sample: the canonical :model-cost-units counter when present, else
  :provider-reported-cost (the accepted fallback counter key), else 0
  when the side carried no model cost."
  [usage]
  (or (:model-cost-units usage)
      (:provider-reported-cost usage)
      0))

(defn- cost-section
  "The :cost summary section. The evaluator's :measure/cost fn (root →
  number) is the v0 cost instrument; when ABSENT but the G5 paired
  result carries real model usage, the cost is DERIVED from the
  aggregated :model-cost-units (:provider-reported-cost fallback) of
  the parent and candidate sides — mirroring the :cost {:cost/units
  {:parent n :candidate n}} shape (Feature C). When neither
  :measure/cost nor model usage exists the section stays EMPTY — no
  fabricated cost claims (v0 has no scheduler token/latency telemetry
  contract; Global Constraint 24)."
  [evaluator paired-result parent-root candidate-root]
  (if-let [m (:measure/cost evaluator)]
    (let [parent (m parent-root)
          candidate (m candidate-root)]
      (when-not (and (number? parent) (number? candidate))
        (throw (err/error :eval/measure-invalid
                          ":measure/cost must return a number per bundle root"
                          {:parent parent :candidate candidate})))
      {:cost {:cost/units {:parent (double parent)
                           :candidate (double candidate)}}})
    (let [parent-usage (get-in paired-result [:parent :usage])
          candidate-usage (get-in paired-result [:candidate :usage])
          parent-cost? (contains? (or parent-usage {}) :model-cost-units)
          cand-cost? (contains? (or candidate-usage {}) :model-cost-units)
          parent-reported? (contains? (or parent-usage {}) :provider-reported-cost)
          cand-reported? (contains? (or candidate-usage {}) :provider-reported-cost)]
      (if (or parent-cost? cand-cost? parent-reported? cand-reported?)
        {:cost {:cost/units
                {:parent (double (usage-model-cost-units parent-usage))
                 :candidate (double (usage-model-cost-units candidate-usage))}}}
        {:cost {}}))))

(defn- complexity-section
  "The :complexity summary section: genome bytes and topology node
  count, measured deterministically from both bundle roots."
  [parent-root candidate-root]
  {:complexity {:genome-bytes {:parent (bundle-bytes parent-root)
                               :candidate (bundle-bytes candidate-root)}
                :graph-nodes {:parent (topology-nodes parent-root)
                              :candidate (topology-nodes candidate-root)}}})

(defn- guard-reasons
  "The G6 guardrail reasons over one numeric section: every metric
  whose candidate/parent ratio exceeds the profile's max. The
  complexity guard applies ONLY when the profile declares
  :max-complexity-regression (complexity is informational otherwise —
  the same rule as evoclj.eval.compare)."
  [dimension rule max-key section-max section]
  (into []
        (keep (fn [[metric entry]]
                (let [ratio (metrics/ratio entry)]
                  (when (> ratio section-max)
                    {:dimension dimension
                     :rule rule
                     :metric metric
                     :detail {:parent (:parent entry)
                              :candidate (:candidate entry)
                              :ratio ratio
                              max-key (double section-max)}}))))
        section))

(defn- g6-reasons
  "The combined cost/complexity guardrail reasons (the G6 gate status
  and the eligibility decision agree by construction — compare derives
  the SAME reasons from the SAME summary)."
  [profile cost complexity]
  (let [p (or (:promotion profile) {})
        d profile/default-promotion-thresholds
        max-cost (or (:max-cost-regression p) (:max-cost-regression d))
        max-cx (:max-complexity-regression p)
        cost-reasons (guard-reasons :cost :max-cost-regression
                                    :max-cost-regression max-cost
                                    (:cost cost))
        cx-reasons (when max-cx
                     (guard-reasons :complexity :max-complexity-regression
                                    :max-complexity-regression max-cx
                                    (:complexity complexity)))]
    (vec (concat cost-reasons cx-reasons))))

(defn- run-g6-phase!
  "G6 cost/complexity guardrails: measure both sections from the two
  bundle roots (the :cost section derives from the G5 paired result's
  model usage when no :measure/cost is injected) and compare each
  ratio against the profile's maxima. The gate fails when any measured
  regression exceeds its guardrail."
  [evaluator profile paired-result parent-root candidate-root]
  (let [cost (cost-section evaluator paired-result
                           parent-root candidate-root)
        complexity (complexity-section parent-root candidate-root)
        reasons (g6-reasons profile cost complexity)
        report {:cost (:cost cost)
                :complexity (:complexity complexity)
                :reasons reasons}]
    (phase-gate (details-store evaluator) :G6-cost-complexity
                #(if (seq (:reasons %)) :fail :pass)
                (fn [] report))))

;; --- the summary and the eligibility decision -------------------------------------------

(defn- hard-section
  "The :hard summary section from the gate results plus the G4/G5
  critical-case evidence. Every entry is the :violations form: pass =
  empty violations, fail = the violating evidence itself."
  [gate-results replay-report paired-result]
  (let [failed (filterv #(not= :pass (:status %)) gate-results)
        replay-critical (when replay-report
                          (filterv :critical? (:regressions replay-report)))
        paired-critical (when paired-result
                          (paired-critical-violations paired-result))]
    {:gates {:parent :pass
             :candidate (if (seq failed) :fail :pass)
             :violations failed}
     :replay {:parent :pass
              :candidate (if (seq replay-critical) :fail :pass)
              :violations (vec replay-critical)}
     :paired {:parent :pass
              :candidate (if (seq paired-critical) :fail :pass)
              :violations (or paired-critical [])}}))

(defn- build-summary
  "The NORMATIVE evaluation summary — every section separate (Global
  Constraint 14), built from the gates and the paired results.
  :utility comes from the G5 run (evoclj.eval.metrics/summarize-utility
  over the side pass rates); :cost/:complexity are the G6 measurements
  (empty :cost when the evaluator carries no :measure/cost)."
  [evaluator gate-results replay-report paired-result
   parent-root candidate-root profile]
  (metrics/validate-summary!
   {:hard (hard-section gate-results replay-report paired-result)
    :utility (if paired-result
               (:utility (metrics/summarize-utility paired-result))
               {})
    :cost (:cost (cost-section evaluator paired-result
                                parent-root candidate-root))
    :complexity (:complexity (complexity-section parent-root candidate-root))}))

(defn- eligibility
  "The eligibility decision: compare/eligibility over the summary —
  lexicographic, hard first, with explicit reason data (Task 8.5)."
  [summary profile]
  (compare/eligibility summary profile))

;; --- the Evaluation record and its finalization transaction ------------------------------

(defn- evaluation-record
  "Assemble the immutable Evaluation record."
  [c profile gate-results paired-ref summary elig]
  {:evaluation/id (UUID/randomUUID)
   :candidate/id (:candidate/id c)
   :parent/generation-id (:parent/generation-id c)
   :profile/id (:eval/profile-id profile)
   :gates (vec gate-results)
   :paired-results-ref paired-ref
   :summary summary
   :eligibility elig
   :created-at (Date.)})

(defn- persist-finalized!
  "THE FINALIZATION TRANSACTION (documented in the namespace docstring
  and in the plan's 'Evaluation finalization transaction'): one SQL
  transaction inserts the finalized eval_runs row and CAS-updates the
  candidate row 'evaluating' → 'eligible' (the machine edge
  :evaluation-pending → :evaluated at the row boundary). Either step
  failing — including the optional :finalize/before-candidate-update
  failure-injection hook — rolls BOTH back atomically. Returns the
  evaluation unchanged."
  [evaluator evaluation]
  (let [db (:sqlite (:store evaluator))
        cid (str (:candidate/id evaluation))
        ts (canonical-timestamp (:created-at evaluation))]
    (jdbc/with-db-transaction [conn (sqlite/spec db)]
      (sqlite/enable-foreign-keys! conn)
      (jdbc/insert! conn :eval_runs
                    {:id (str (:evaluation/id evaluation))
                     :candidate_id cid
                     :parent_generation_id (:parent/generation-id evaluation)
                     :profile_id (str (:profile/id evaluation))
                     :gates (pr-str (:gates evaluation))
                     :paired_results_ref (:paired-results-ref evaluation)
                     :summary (pr-str (:summary evaluation))
                     :eligibility (pr-str (:eligibility evaluation))
                     :status "finalized"
                     :created_at ts})
      ;; failure-injection hook (test-only): a throw here rolls the
      ;; transaction back — the report is not persisted and the
      ;; candidate state is untouched
      (when-let [hook (:finalize/before-candidate-update evaluator)]
        (hook evaluation))
      (let [n (first (jdbc/execute!
                      conn
                      ["UPDATE candidates SET state = 'eligible'
                        WHERE id = ? AND state = 'evaluating'"
                       cid]))]
        (when-not (= 1 n)
          (throw (err/error :eval/candidate-state-invalid
                            "candidate is not :evaluation-pending (persisted as 'evaluating')"
                            {:candidate/id cid}))))))
  evaluation)

(defn- finalize-evaluation!
  "Assemble the summary and eligibility, persist the paired results
  artifact, and commit the evaluation + candidate transition in one
  transaction. Returns the immutable Evaluation record."
  [evaluator c profile gate-results replay-report paired-result
   g6 parent-root candidate-root]
  (let [summary (build-summary evaluator gate-results replay-report
                               paired-result parent-root candidate-root
                               profile)
        elig (eligibility summary profile)
        paired-ref (when paired-result
                     (put-artifact! evaluator
                                    (raw-paired-observations paired-result)))
        evaluation (validate-evaluation!
                    (evaluation-record c profile gate-results paired-ref
                                       summary elig))]
    (persist-finalized! evaluator evaluation)
    evaluation))

;; --- reads -------------------------------------------------------------------------------

(defn- row->evaluation
  "Convert an eval_runs row into the public Evaluation record."
  [row]
  {:evaluation/id (UUID/fromString (:id row))
   :candidate/id (UUID/fromString (:candidate_id row))
   :parent/generation-id (:parent_generation_id row)
   :profile/id (keyword (subs (:profile_id row) 1))
   :gates (edn/read-string (:gates row))
   :paired-results-ref (:paired_results_ref row)
   :summary (edn/read-string (:summary row))
   :eligibility (edn/read-string (:eligibility row))
   :created-at (Date/from (Instant/parse (:created_at row)))})

(defn find-evaluation
  "The immutable Evaluation record for `evaluation-id`, or nil when no
  evaluation has that id. Read-only."
  [evaluator evaluation-id]
  (validate-evaluator! evaluator)
  (some-> (first (sqlite/query (:sqlite (:store evaluator))
                               ["SELECT * FROM eval_runs WHERE id = ?"
                                (str evaluation-id)]))
          row->evaluation
          validate-evaluation!))

(defn find-evaluations-by-candidate
  "Every finalized Evaluation record for a candidate, in creation
  order. Read-only."
  [evaluator candidate-id]
  (validate-evaluator! evaluator)
  (->> (sqlite/query (:sqlite (:store evaluator))
                     ["SELECT * FROM eval_runs WHERE candidate_id = ?
                       ORDER BY created_at ASC, id ASC"
                      (str candidate-id)])
       (mapv (fn [row] (validate-evaluation! (row->evaluation row))))))

;; --- the entry point ----------------------------------------------------------------------

(defn evaluate-candidate!
  "Run the NORMATIVE Task 8.7 phase order for one candidate under one
  profile and return the immutable Evaluation record.

  The candidate must be :evaluation-pending (persisted as 'evaluating'
  — the machine edge precondition). The pipeline runs G0–G3 through
  the hard-gate front door; a non-pass records every later phase as
  :not-run. Otherwise G4 replay, G5 paired hidden selection, and G6
  cost/complexity guardrails run in order, each non-pass also
  :not-run-ing the phases after it. The summary is built from the
  gates + paired results (every section separate) and the eligibility
  decision is lexicographic via evoclj.eval.compare/eligibility. The
  finalized report and the candidate's :evaluation-pending → :evaluated
  transition are committed in ONE SQL transaction.

  Canary and promotion remain Promotion's responsibility (Milestone 9)
  — this namespace only produces eligibility FACTS; it never changes
  CURRENT (no promotion/current dependency exists)."

  [evaluator candidate-id profile-id]
  (validate-evaluator! evaluator)
  (let [c (resolve-candidate! evaluator candidate-id)
        profile (resolve-profile! evaluator profile-id)
        parent-root (resolve-root! evaluator (:parent/generation-id c))
        candidate-root (resolve-root! evaluator (str candidate-id))
        case-set (vec (sort (keys (selection-cases evaluator profile))))
        _ (when (empty? case-set)
            (throw (err/error :eval/evaluator-invalid
                              "the profile's selection set resolves to no cases"
                              {:reason :no-selection-cases})))
        front (gates/run-gates-until-hard-failure
               (gate-context evaluator candidate-root parent-root))
        front-gates (:results front)]
    (if (:stopped? front)
      ;; a hard front gate failed: every later phase is :not-run
      (finalize-evaluation! evaluator c profile
                            (concat front-gates
                                    (not-run-after front-gates))
                            nil nil nil parent-root candidate-root)
      (let [g4 (run-g4-phase! evaluator candidate-root)]
        (if (not= :pass (:status (:gate g4)))
          (finalize-evaluation! evaluator c profile
                                (concat front-gates [(:gate g4)]
                                        (not-run-after
                                         (conj front-gates (:gate g4))))
                                (:report g4) nil nil
                                parent-root candidate-root)
          (let [g5 (run-g5-phase! evaluator c profile case-set)]
            (if (not= :pass (:status (:gate g5)))
              (finalize-evaluation! evaluator c profile
                                    (concat front-gates [(:gate g4) (:gate g5)]
                                            (not-run-after
                                             (conj front-gates (:gate g4)
                                                   (:gate g5))))
                                    (:report g4) (:report g5) nil
                                    parent-root candidate-root)
              (let [g6 (run-g6-phase! evaluator profile
                                      (:report g5)
                                      parent-root candidate-root)]
                (finalize-evaluation!
                 evaluator c profile
                 (concat front-gates [(:gate g4) (:gate g5) (:gate g6)])
                 (:report g4) (:report g5) (:report g6)
                 parent-root candidate-root)))))))))

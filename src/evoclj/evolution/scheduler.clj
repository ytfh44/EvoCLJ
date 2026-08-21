(ns evoclj.evolution.scheduler
  "Long-horizon evolution loop controller (component).

  `run-cycles!` makes the evolution runtime turn itself: it repeatedly
  executes ONE generation — evolve → eval → promote — through the
  EXISTING single-generation public APIs, appends a per-generation
  summary to `history`, and consults
  `evoclj.evolution.loop-policy/decide-continue?` to decide whether to
  produce another generation. The loop stops when the policy's decision
  keyword names a stop reason (`:stop-max-gen`, `:stop-regression`,
  `:stop-plateau`) or when a hard safety cap (`max-cycles`) is reached.

  `run-cycles!` does NOT re-implement evolve / eval / promote. The
  one-generation step is supplied as the `run-generation` argument — a
  fn of no args returning that generation's summary map
  `{:generation/id <str> :utility <double>}`. In production
  `run-generation` is built by `make-generation-runner`, which composes
  the public APIs `evolution.core/propose-candidates!`,
  `eval.core/evaluate-candidate!`, and `promotion.promote/promote!`. In
  tests a mock `run-generation` is injected, so the loop/policy
  integration is verified without running the heavy evaluate path.

  Per the loop-policy contract, each history entry also carries
  `:parent/utility`; `run-cycles!` fills `:parent/utility` from the
  previous generation's `:utility` (defaulting to 0.0 for the first
  generation) whenever the step omits it, so a one-generation step only
  needs to report the new generation's own `:utility` (the source for
  the loop-policy `:utility` is the candidate's evaluated utility, see
  `make-generation-runner`).

  The controller is controlled-only-by-composition: it performs no new
  side effects and introduces no global state — every IO it triggers
  lives inside the injected `run-generation` step."
  (:require [evoclj.evolution.core :as evolution]
             [evoclj.eval.core :as eval-core]
             [evoclj.eval.cost-guard :as cost-guard]
             [evoclj.eval.workers :as workers]
             [evoclj.evolution.loop-policy :as lp]
             [evoclj.evolution.pareto :as pareto]
             [evoclj.evolution.population :as population]
             [evoclj.promotion.promote :as promote]))

;; --- helpers ----------------------------------------------------------------

(defn- stop-decision?
  "True when a loop-policy `:decision` names a stop reason, i.e. its
  keyword (e.g. `:stop-max-gen`) starts with `stop`. `:continue` and any
  other keyword return false."
  [decision]
  (and (keyword? decision)
       (re-find #"^:stop" (str decision))))

(defn- summarize-generation
  "Populate one history summary from a one-generation step result `raw`:
  ensure `:generation/id`, coerce `:utility` to a double (default 0.0),
  and fill `:parent/utility` from the previous summary's `:utility`
  (default 0.0 for the first generation) when the step omitted it.

  Filling `:parent/utility` here keeps the loop-policy contract satisfied
  without requiring the one-generation step to know its parent's score —
  the parent of generation N is exactly generation N-1 in `history`."
  [raw history]
  (let [idx (inc (count history))]
    (-> raw
        (assoc :generation/id (or (:generation/id raw)
                                   (str "generation-" idx)))
        (assoc :utility (double (or (:utility raw) 0.0)))
        (assoc :parent/utility (or (:parent/utility raw)
                                   (:utility (peek history) 0.0))))))

;; --- the loop controller -----------------------------------------------------

(defn run-cycles!
  "The long-horizon evolution loop controller (component).

  `run-generation` — (fn [] -> summary-map). Executes ONE full generation
                     (evolve → eval → promote) starting from the current
                     CURRENT generation and returns that generation's
                     summary `{:generation/id <str> :utility <double>}`.
                     `:parent/utility` MAY be omitted; `run-cycles!` fills
                     it from the previous generation's `:utility` (0.0 for
                     the first — the seed has no scored parent).
  `loop-config`   — the loop-policy config map
                     `{:max-generations :plateau-window
                       :min-improvement :stop-on-regression?}`
                     (any key may be omitted; `decide-continue?` applies
                     its own defaults). A typical caller derives it from
                     `(:config/evolution-loop config)`.

  Optional keyword args:
    :max-cycles — hard safety cap on generations executed (default 1000).
                  Prevents a runaway loop when `loop-config` has no
                  effective stop; when hit, the loop stops with
                  `:final-decision :stop-max-cycles` (a documented guard,
                  not a loop-policy decision).
    :history    — optional initial `history` (prior generation summaries)
                  that seeds the policy. Each entry must carry `:utility`
                  (and `:parent/utility`); the first new generation's
                  `:parent/utility` is taken from the last seeded entry's
                  `:utility`.
    :cost-guard — optional cost hard-stop map `{:threshold <number>}`.
                  When present, cumulative cost (taken from each
                  generation summary's `:cost` key, defaulting to 0.0
                  when absent) is checked after each generation via
                  `evoclj.eval.cost-guard/should-stop?`. A strict
                  exceed triggers `:final-decision :stop-cost` and
                  returns immediately.

  Loop (per the spec): run one generation → build its summary → conj onto
  `history` → call `decide-continue?` → if the `:decision` starts with
  `:stop`, return the advisor map; otherwise iterate.

  Returns the advisor map:
    {:generations [summary ...]  ; newest LAST, each fully populated
     :final-decision <kw>        ; the loop-policy :decision (or
                                  ; :stop-max-cycles for the safety cap,
                                  ; :stop-cost for the budget hard-stop)
     :stop-reason <str>
     :cycles <int>}              ; number of generations executed

  The controller introduces no global state and no new side effects: it
  only calls the injected `run-generation` (which performs the IO) and
  the pure policy."
  [run-generation loop-config & {:keys [max-cycles history cost-guard]
                                 :or {max-cycles 1000}}]
  (loop [hist (vec history)
         cycles 0
         cumulative-cost 0.0]
    (if (>= cycles max-cycles)
      {:generations hist
       :final-decision :stop-max-cycles
       :stop-reason (str "reached hard safety cap max-cycles " max-cycles)
       :cycles cycles}
      (let [summary (summarize-generation (run-generation) hist)
            hist' (conj hist summary)
            step-cost (if (map? summary) (double (or (:cost summary) 0.0)) 0.0)
            cumulative-cost' (+ cumulative-cost step-cost)
            {:keys [decision reason]} (lp/decide-continue? hist' loop-config)]
        (if (stop-decision? decision)
          {:generations hist'
           :final-decision decision
           :stop-reason reason
           :cycles (inc cycles)}
          (if (and cost-guard
                   (:threshold cost-guard)
                   (= :stop (cost-guard/should-stop? cumulative-cost' (:threshold cost-guard))))
            {:generations hist'
             :final-decision :stop-cost
             :stop-reason (str "cumulative cost " cumulative-cost'
                               " exceeds threshold " (:threshold cost-guard))
             :cycles (inc cycles)}
            (recur hist' (inc cycles) cumulative-cost')))))))

;; --- production one-generation wiring (reuses the public APIs) ----------------

(defn make-generation-runner
  "Build the production `run-generation` fn for `run-cycles!` by composing
  the EXISTING single-generation public APIs — no evolve/eval/promote
  logic is re-implemented here:
    - `evolution.core/propose-candidates!` (evolve)
    - `eval.core/evaluate-candidate!`     (eval)
    - `promotion.promote/promote!`        (promote)

  `ctx` carries the host-constructed subsystem handles and the
  per-generation wiring:
    :evolution-system         — evolution-system map for
                                `propose-candidates!` (its :genome-loader/
                                :genome-root must resolve the CURRENT
                                generation's parent genome each call — the
                                host owns that wiring).
    :evaluator                — evaluator map for `evaluate-candidate!`.
    :promotion-system         — promotion-system map for `promote!` (carries
                                :store, :resolution/id, :event/session-id).
    :profile-id               — evaluation profile id keyword (default
                                :default-v1).
    :current-generation-id    — (fn [] -> <gen-id str> | nil) reads the
                                CURRENT pointer (a read; the CLI provides
                                this today via its private session helpers).
    :candidates-for-generation — (fn [gen-id] -> [candidate record])
                                lists the generation's candidates (the CLI
                                provides this today via its private
                                `candidates-for-generation` helper; the
                                scheduler accepts it as an injected read fn
                                to stay decoupled from the cli/session host
                                layer).
    :evidence-selector        — (optional) passed to `propose-candidates!`
                                (default {:recent 3 :include-successes 1
                                :include-failures 2 :include-high-cost 1}).
    :max-candidates           — (optional) passed to `propose-candidates!`
                                (default 3; the v0 cap remains 3).
    :no-promote?              — (optional) when true, skip the pointer move
                                (the would-be promotions are recorded as
                                `:skipped` in the step's report, mirroring
                                the `cycle` CLI command).
    :population               — (optional) a population map (S3-1); when
                                present, evaluated candidates are added after
                                eval and the mutator context receives
                                `:population` and `:breeding-candidates`.
    :pareto-archive           — (optional) a Pareto archive vector (S3-1);
                                when present, evaluated candidates' summaries
                                are added to the archive after eval.

  The runner reuses the SAME evolve→eval→promote shape as the `cycle` CLI
  command, but takes the subsystem maps directly instead of reconstructing
  them from CLI opts — it calls the public entry points only.

  Returned fn: (fn [] -> summary-map). It:
     1. reads the CURRENT generation id (throws `:scheduler/no-current`
        when none);
     2. `propose-candidates!` over it;
     3. evaluates every `:evaluation-pending` candidate of the generation
        under `:profile-id`;
     4. `promote!` every candidate whose evaluation `:eligible?` is true
        (skipping the pointer move when `:no-promote?`);
     5. returns `{:generation/id <new-or-current id> :utility <double>
        :cost <double>}` where `:utility` is the max candidate utility
        drawn from the evaluated summaries'
        `[:summary :utility :task/success :candidate]` rate (0.0 when no
        evaluation completed — a real score requires a completed eval).
        `:cost` is the sum of `:cost` across evaluated candidates (0.0
        when none report cost). `:parent/utility` is intentionally
        omitted: `run-cycles!` fills it from the previous generation's
        `:utility`."
  [ctx]
  (let [evolution-system (:evolution-system ctx)
        evaluator (:evaluator ctx)
        promotion-system (:promotion-system ctx)
        profile-id (or (:profile-id ctx) :default-v1)
        evidence-selector (or (:evidence-selector ctx)
                              {:recent 3 :include-successes 1
                               :include-failures 2 :include-high-cost 1})
        max-candidates (or (:max-candidates ctx) 3)
        no-promote? (boolean (:no-promote? ctx))
        current-id (:current-generation-id ctx)
        candidates-for-generation (:candidates-for-generation ctx)]
    (fn []
      (let [gen-id (current-id)]
        (when-not gen-id
          (throw (ex-info "no CURRENT generation to evolve"
                          {:error/type :scheduler/no-current})))
        ;; 1. EVOLVE — public API; the CURRENT pointer is never touched here.
        (evolution/propose-candidates!
         evolution-system
         {:generation/id gen-id
          :evidence-selector evidence-selector
          :max-candidates max-candidates})
        ;; 2. EVAL — every :evaluation-pending candidate of the generation.
        (let [pending (filter #(= :evaluation-pending (:state %))
                              (candidates-for-generation gen-id))
              transport (:worker-transport ctx)
              evals (if transport
                      (let [tasks (mapv (fn [c] {:task/id (:candidate/id c)}) pending)
                            batch-result (workers/run-batch-with-transport!
                                          transport tasks {:concurrency 4})]
                        (vec (concat
                              (mapv :task/result (:batch/completed batch-result))
                              (mapv (fn [entry]
                                      {:candidate/id (:task/id entry)
                                       :error (select-keys entry
                                                           [:error/type
                                                            :error/message
                                                            :error/data])})
                                    (:batch/failed batch-result)))))
                      (mapv (fn [c]
                              (try
                                (eval-core/evaluate-candidate!
                                 evaluator (:candidate/id c) profile-id)
                                (catch Throwable t
                                  {:candidate/id (:candidate/id c)
                                   :error (ex-data t)})))
                            pending))
              scored (filter :summary evals)
              ;; 3. PROMOTE — every eligible evaluation (or record a skip).
              passing (filterv #(get-in % [:eligibility :eligible?]) scored)
              promoted (if no-promote?
                         (mapv (fn [e]
                                 {:candidate/id (:candidate/id e)
                                  :status :skipped
                                  :reason :no-promote
                                  :eligible? true})
                               passing)
                         (when-let [ps (if (fn? promotion-system)
                                         (promotion-system)
                                         promotion-system)]
                           (mapv (fn [e]
                                   (try
                                     (promote/promote!
                                      ps
                                      {:candidate-id (:candidate/id e)
                                       :evaluation-id (:evaluation/id e)
                                       :expected-parent-generation
                                       (:parent/generation-id e)})
                                     (catch Throwable t
                                       {:candidate/id (:candidate/id e)
                                        :error (ex-data t)})))
                                 passing)))
              ;; --- S3-1: update pareto archive with evaluated candidates
              _ (when-let [archive (:pareto-archive ctx)]
                  (doseq [e scored]
                    (when-let [scores (:summary e)]
                      (pareto/add-candidate! archive scores))))
              ;; --- S3-1: update population with eval summaries and select
              ;;     breeding candidates for the NEXT generation's proposals
              _ (when-let [pop (:population ctx)]
                  (doseq [e scored]
                    (when-let [candidate-id (:candidate/id e)]
                      (population/add-candidate! pop
                                                 {:candidate/id candidate-id}
                                                 e)))
                  ;; the mutator context in the NEXT propose-candidates!
                  ;; call will see the updated :population and its
                  ;; :breeding-candidates via the system map
                  nil)
              utility (if (seq scored)
                        (apply max
                               (keep #(get-in % [:summary :utility
                                                 :task/success :candidate])
                                     scored))
                        0.0)
              cost (reduce + 0.0 (keep :cost scored))
              new-id (or (some #(when (= :promoted (:status %))
                                  (:to %))
                                promoted)
                         gen-id)]
          {:generation/id new-id
           :utility (double utility)
           :cost (double cost)})))))

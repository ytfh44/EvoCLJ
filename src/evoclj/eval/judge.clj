(ns evoclj.eval.judge
  "LLM-as-judge semantic equivalence for evaluation (feature V1).

  (llm-judge config) constructs a judge fn that decides whether a
  selection side's outputs are SEMANTICALLY equivalent to the expected
  output:

      (llm-judge config)              ;; => (fn [expected-output outputs] -> boolean)

  The returned fn is usable directly as a case :output/equiv? fn, or
  registered under :equivalence/by-keyword under (judge-keyword)
  (::equivalent/llm-judge => :equivalence/llm-judge) so a case can
  declare :output/equiv? :equivalence/llm-judge.

  A real model decides equivalence. For one judgement the judge builds a
  single bounded user message rendering the expected output and the side
  outputs, calls the host-injected :model-call closure EXACTLY ONCE, and
  parses the strict-JSON {\"equivalent\"} verdict. The model-call fn is the
  ONLY effect channel (Global Constraint 8): the judge never calls a
  provider directly — the host (evoclj.kernel.system) closes the
  :model-call closure over the kernel's capability broker and injects it
  here. This is a kernel-side component.

  CONFIG (a CLOSED map — any unknown key is rejected):

    :model-call  — a Clojure fn injected by the host with the SAME
                   contract as evoclj.evolution.llm-diagnostician:
                   (fn [model-id messages options]) -> the broker
                   dispatch result of exactly ONE model call:
                   {:result/status :ok
                    :value {:model/output {:text string?
                                           :reasoning (optional)}
                            :usage {...}}}
                   (the shape evoclj.intent.dispatch/dispatch! returns
                   for :intent/model-call). The fn either returns that
                   success result or throws ExceptionInfo with a stable
                   :error/type. REQUIRED.
    :model/id    — a string model identifier, passed through to
                   :model-call unchanged. REQUIRED.
    :temperature — optional number, default 0.0 (deterministic
                   judgement); passed through to the :model-call
                   options.
    :system-prompt — optional string; defaults to a built-in prompt that
                   instructs the model to decide semantic equivalence and
                   answer with STRICT JSON {\"equivalent\": true|false}.
    :max-tokens  — optional pos-int, default 1024.

  FAIL-LOUD POLICY: the judge NEVER guesses. A non-parseable response or
  a thrown model-call throws a typed error :eval/judge-failed (preserving
  the cause :error/type in the data). It never silently returns false —
  a judge outage must surface in the evaluation rather than flip every
  score to 0. Config contract violations throw :eval/judge-config-invalid.

  Error contract (Global Constraint 22 — plain serializable data):
    :eval/judge-config-invalid — bad constructor config (not a map,
                                  unknown key, missing required field,
                                  wrong type).
    :eval/judge-failed         — the :model-call execution failed (a
                                  thrown call or a non-ok dispatch result)
                                  OR the model text was not usable
                                  (non-JSON / invalid / missing :equivalent).

  component (Foundation F3): this namespace also owns the judge-verdict
  enrichment adapter. Per-case judge verdicts persist as versioned
  enrichment records attached to the :evaluation entity (entity-kind
  :evaluation, entity-id = the evaluation id, kind :judge-verdict,
  payload in the CAS): the pure verdict→enrichment mapping
  (verdict-record, verdict-payload) plus the isolated store adapter
  (persist-verdict-enrichment!) that NEVER fails the evaluation — a
  store failure is caught and returned as a :failed record.

  component (roadmap V2): this namespace also owns judge score
  aggregation. Per-case judge verdicts feed a utility summary
  (win/loss/equiv counts plus a per-case — category — breakdown)
  joined into the paired outcome. The pure surface: verdict-pair
  (join ONE parent verdict record and ONE candidate verdict record on
  the same case/repetition into a paired verdict), aggregate-verdicts
  (verdict list → summary record; counts correct; per-category
  breakdown stable; empty list → zeroed summary), and
  join-utility-summary (the pure join into the paired outcome —
  evoclj.eval.paired stays read-only). Aggregation fails loud with
  :eval/judge-summary-invalid — silently pairing or tallying
  misaligned verdicts would corrupt the win/loss counts.

  component (roadmap V5): judge configuration. The judge's model-call
  settings are configurable and validated: :temperature (optional
  number, default 0.0 — deterministic judgement), :system-prompt
  (optional string, built-in default), :max-tokens (optional pos-int,
  default 1024). Overrides flow into the :model-call options/messages;
  the host's evoclj.config envelope exposes the same three keys under
  :config/judge (JudgeSectionSchema), validated by validate-config!.

  component (T4c): the judge calibration harness.
  calibration-judgement runs ONE judge decision over ONE calibration
  pair (known-equivalent or known-different expected/actual outputs)
  into a plain per-pair record; agreement-stats is the pure agreement
  statistics fn (exact agree/disagree counts plus a stable per-label
  breakdown; zeroed on empty input); run-calibration is the harness —
  it runs any judge fn over a calibration fixture and reports the
  stats. In CI the harness runs the fixture judge (deterministic, no
  network), so the calibration fixture reports exact agreement; with a
  real model the same harness reports real agreement rates. Errors are
  typed :eval/judge-calibration-invalid."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.store.enrichment :as enrichment]
            [malli.core :as m]
            [malli.error :as me]))

;; --- the registered keyword --------------------------------------------------

(defn judge-keyword
  "The equivalence-registry keyword the judge is registered under:
  :equivalence/llm-judge. A selection case declares
  :output/equiv? :equivalence/llm-judge to opt into LLM judgement when
  the evaluator wires it into :equivalence/by-keyword."
  []
  :equivalence/llm-judge)

(defn merge-judge
  "Merge the judge fn into an equivalence registry map under
  :equivalence/llm-judge (overriding any existing entry for that
  keyword). Returns the merged registry."
  [registry judge-fn]
  (assoc registry (judge-keyword) judge-fn))

;; --- constructor config (closed map — Global Constraint 11 style) ------------

(def ^:private default-system-prompt
  (str/join
   "\n"
   ["You are an evaluation judge for EvoCLJ, a self-evolving agent runtime."
    "You compare an EXPECTED output with an ACTUAL output produced by an agent and"
    "decide whether the actual output is SEMANTICALLY EQUIVALENT to the expected one."
    ""
    "Semantic equivalence means: the actual output fulfils the expected intent,"
    "regardless of wording, formatting, or phrasing. Answer TRUE when the agent"
    "delivered what was asked (possibly with extra harmless detail). Answer FALSE"
    "only for genuinely wrong, missing, or contradicting output. Never guess: if"
    "the actual output is empty or absent, answer false."
    ""
    "Respond with STRICT JSON only: {\"equivalent\": true} or {\"equivalent\": false}."]))

(def LlmJudgeConfigSchema
  "The LLM judge's constructor config — a CLOSED map (any unknown key
  is rejected): :model-call (required fn), :model/id (required
  string), optional :temperature (number, default 0.0),
  :system-prompt (string, built-in default), and :max-tokens
  (pos-int, default 1024)."
  [:map {:closed true}
   [:model-call fn?]
   [:model/id string?]
   [:temperature {:optional true} number?]
   [:system-prompt {:optional true} string?]
   [:max-tokens {:optional true} pos-int?]])

(defn- throw-config-invalid!
  [expl]
  (throw (err/error :eval/judge-config-invalid
                    "llm-judge config does not satisfy the judge contract"
                    {:errors (me/humanize expl)})))

(defn- validate-config
  "Validate the judge config map. Returns it unchanged, or throws
  :eval/judge-config-invalid."
  [config]
  (if-let [expl (m/explain LlmJudgeConfigSchema config)]
    (throw-config-invalid! expl)
    config))

;; --- bounded user-message rendering -------------------------------------------

(def ^:private max-rendered-bytes
  "The hard cap on how many characters of expected/actual output are
  rendered into the judge's user message. Large outputs are truncated
  and the truncation is noted to the model."
  8000)

(defn- truncate-str
  "Truncate a string to at most max-rendered-bytes characters with an
  explicit ellipsis note when it was cut."
  [s]
  (if (and (string? s) (> (count s) max-rendered-bytes))
    (str (subs s 0 max-rendered-bytes) "\n...[truncated]")
    s))

(defn- render-value
  "Render one output value (any EDN-safe value) as a compact string for
  the judge prompt, with a bounded character cap."
  [x]
  (truncate-str (pr-str x)))

(defn- user-message
  "Build the bounded user message: the expected output and the actual
  outputs, each rendered and capped."
  [expected outputs]
  (str "EXPECTED output:\n"
       (render-value expected)
       "\n\nACTUAL output(s):\n"
       (if (seq outputs)
         (str/join "\n---\n" (map render-value outputs))
         "(no actual output)")))

;; --- robust JSON extraction ----------------------------------------------------

(defn- parse-model-json
  "Parse the model's output text into a keywordized map. Robust
  extractor: direct decode first; if that fails, locate the FIRST '{'
  and the LAST '}' and parse the substring between them (handles prose
  and code-fence wrapping). Returns the decoded map, or nil when
  neither attempt succeeds."
  [text]
  (when (string? text)
    (or (try (json/decode text true)
             (catch Exception _ nil))
        (let [first-brace (str/index-of text "{")
              last-brace (str/last-index-of text "}")]
          (when (and first-brace last-brace (< first-brace last-brace))
            (try (json/decode (subs text first-brace (inc last-brace)) true)
                 (catch Exception _ nil)))))))

(defn- verdict
  "Extract the boolean :equivalent verdict from the parsed JSON, or nil
  when absent or not a boolean."
  [parsed]
  (when (map? parsed)
    (let [e (:equivalent parsed)]
      (when (instance? Boolean e) e))))

;; --- the judge fn ---------------------------------------------------------------

(defn llm-judge
  "Construct the LLM-as-judge equivalence fn.

  (llm-judge config) => (fn [expected-output outputs] -> boolean)

  Config (CLOSED map — unknown keys rejected with
  :eval/judge-config-invalid):

    :model-call   — REQUIRED fn, the host-injected closure with the
                    same contract as
                    evoclj.evolution.llm-diagnostician: (fn
                    [model-id messages options]) -> the broker
                    dispatch result {:result/status :ok :value
                    {:model/output {:text ...} :usage ...}} or throws
                    ExceptionInfo with a stable :error/type.
    :model/id     — REQUIRED string model identifier.
    :temperature  — optional number, default 0.0 (deterministic
                    judgement); flows into the model-call options.
    :system-prompt — optional string; built-in default instructs
                    strict-JSON {\"equivalent\": true|false} answers.
    :max-tokens   — optional pos-int, default 1024.

  The returned fn is pure except for exactly ONE :model-call per
  judgement (the only effect channel, Global Constraint 8). FAIL-LOUD:
  any judgement failure (thrown call, non-ok dispatch, non-JSON
  response, missing/invalid :equivalent) throws :eval/judge-failed —
  the judge never silently returns false, so an outage surfaces in the
  evaluation instead of flipping every score to zero."
  [config]
  (let [v (validate-config config)
        model-call (:model-call v)
        model-id (:model/id v)
        temperature (or (:temperature v) 0.0)
        system-prompt (or (:system-prompt v) default-system-prompt)
        max-tokens (or (:max-tokens v) 1024)]
    (fn [expected outputs]
      (let [messages [{:role :system :content system-prompt}
                      {:role :user
                       :content (user-message expected outputs)}]
            result (try
                     (model-call model-id messages
                                 {:temperature temperature
                                  :max-tokens max-tokens})
                     (catch clojure.lang.ExceptionInfo e
                       (throw (err/error :eval/judge-failed
                                         "judge model call failed"
                                         {:error/type (:error/type (ex-data e))
                                          :cause (err/error-data e)})))
                     (catch Throwable t
                       (throw (err/error :eval/judge-failed
                                         "judge model call failed"
                                         {:error/type :eval/judge-failed
                                          :cause (err/error-data t)}))))
            _ (when (and (map? result)
                         (contains? result :result/status)
                         (not= :ok (:result/status result)))
                (throw (err/error :eval/judge-failed
                                  "judge model-call returned a non-ok dispatch result"
                                  {:result/status (:result/status result)
                                   :cause (dissoc result :value)})))
            value (:value result)
            text (get-in value [:model/output :text])]
        (when-not (string? text)
          (throw (err/error :eval/judge-failed
                            "judge model-call did not produce usable text"
                            {:reason :non-string-output
                             :value-type (some-> value class .getName)})))
        (let [parsed (parse-model-json text)
              v (verdict parsed)]
          (when-not (instance? Boolean v)
            (throw (err/error :eval/judge-failed
                              "judge output had no usable :equivalent verdict"
                              {:reason :no-verdict
                               :excerpt (subs text 0 (min 200 (count text)))})))
          v)))))

;; --- component — judge verdicts as enrichment records (Foundation F3) -----------

(defn verdict-record
  "ONE per-case judge verdict as a plain EDN map — the pure
  verdict→enrichment mapping (component):

      {:case/id <keyword>            ; the selection case id
       :repetition <pos-int>         ; the 1-based repetition
       :pair/seed <string>           ; the pair's derived fixture seed
       :expected-output <EDN>        ; the case oracle (kernel-side audit)
       :outputs <vector EDN>         ; the judged side's actual outputs
       :equivalent <boolean>         ; the judge's decision
       :score <1.0|0.0>}             ; derived: 1.0 iff equivalent

  Pure and deterministic: identical inputs always yield identical
  records, so a verdict batch round-trips byte-identically through the
  enrichment CAS (the payload body is pr-str EDN, Global Constraint
  21)."
  [case-id repetition pair-seed expected-output outputs equivalent]
  {:case/id case-id
   :repetition repetition
   :pair/seed pair-seed
   :expected-output expected-output
   :outputs (vec outputs)
   :equivalent (boolean equivalent)
   :score (if equivalent 1.0 0.0)})

(defn verdict-payload
  "The component enrichment :payload for a verdict batch: a plain EDN map
  {:verdicts <vector of verdict-record maps> :count n}. Pure — the
  exact map stored in the CAS via
  evoclj.store.enrichment/put-enrichment! (the row keeps only the
  content-address :payload-ref, never the body)."
  [verdicts]
  {:verdicts (vec verdicts)
   :count (count verdicts)})

(defn persist-verdict-enrichment!
  "component — the judge-verdict store adapter (Foundation F3). Persist a
  verdict batch as a VERSIONED enrichment record attached to the
  evaluation entity:

      (persist-verdict-enrichment! store evaluation-id verdicts)
      ;; => {:enrichment/status :ok :enrichment <Enrichment record>}
      ;; or {:enrichment/status :failed
      ;;     :error/type <keyword> :error/message <string>
      ;;     :error/data <map or nil>}

  The record is created with entity-kind :evaluation, entity-id =
  (str evaluation-id), kind :judge-verdict, :payload = (verdict-payload
  verdicts) (the body goes to the CAS; only its content-address
  :payload-ref lands in SQLite — Global Constraint 21), and :cause =
  the evaluation id as provenance. Each persisted batch allocates the
  next version (max+1) for the (entity-kind, entity-id, kind) triple
  inside the write transaction, so concurrent writers serialize.

  FAILURE ISOLATION (component acceptance): enrichment persistence must
  NEVER fail the evaluation. Every store failure — an invalid store
  map, a CAS write failure, a DB failure — is caught and RETURNED as a
  :failed record carrying the typed :error/type (preserved from an
  ExceptionInfo's :error/type, else :eval/judge-enrichment-failed)
  plus the serializable :error/message and :error/data. The function
  never throws."
  [store evaluation-id verdicts]
  (try
    (let [rec (enrichment/put-enrichment!
               store
               {:entity/kind :evaluation
                :entity/id (str evaluation-id)
                :kind :judge-verdict
                :payload (verdict-payload verdicts)
                :cause (str evaluation-id)})]
      {:enrichment/status :ok
       :enrichment rec})
    (catch clojure.lang.ExceptionInfo e
      {:enrichment/status :failed
       :error/type (:error/type (ex-data e))
       :error/message (.getMessage e)
       :error/data (err/error-data e)})
    (catch Throwable t
      {:enrichment/status :failed
       :error/type :eval/judge-enrichment-failed
       :error/message (.getMessage t)
       :error/data (err/error-data t)})))

;; --- component — judge score aggregation (roadmap V2) ---------------------------

(defn- summary-error
  "A typed :eval/judge-summary-invalid error for the aggregation
  boundary, with a :reason distinguishing the failure (Global
  Constraint 22 — plain serializable data)."
  [reason message value]
  (err/error :eval/judge-summary-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- side-verdict
  "The aggregation view of ONE side of a paired verdict: the judge's
  boolean decision and its derived score (1.0 iff equivalent — the
  same derivation as verdict-record)."
  [equivalent]
  {:equivalent (boolean equivalent)
   :score (if equivalent 1.0 0.0)})

(defn verdict-pair
  "Join ONE parent verdict record and ONE candidate verdict record on
  the same (case, repetition) into a paired verdict for aggregation
  (component):

      (verdict-pair parent-record candidate-record)
      ;; => {:case/id <kw> :repetition <pos-int> :pair/seed <string>
      ;;     :parent {:equivalent <bool> :score 1.0|0.0}
      ;;     :candidate {:equivalent <bool> :score 1.0|0.0}}

  Both inputs are the component verdict-record maps (the records the
  two sides persist as enrichments). The paired-verdict map carries
  the case context plus each side's decision and derived score, ready
  for aggregate-verdicts.

  FAIL-LOUD ON MISALIGNMENT: pairing verdicts from different
  :case/id or :repetition values (or differing :pair/seed when both
  sides carry one — the paired invariant requires both sides to
  observe the SAME fixture seed) throws :eval/judge-summary-invalid;
  silently pairing misaligned verdicts would corrupt the win/loss
  counts. A non-boolean :equivalent on either side fails the same
  way."
  [parent candidate]
  (when-not (and (map? parent) (map? candidate))
    (throw (summary-error :side-not-a-map
                          "both verdict sides must be verdict-record maps"
                          {:parent parent :candidate candidate})))
  (let [pe (:equivalent parent)
        ce (:equivalent candidate)]
    (when-not (and (instance? Boolean pe) (instance? Boolean ce))
      (throw (summary-error :non-boolean-equivalent
                            "both verdict sides must carry a boolean :equivalent"
                            {:parent/equivalent pe
                             :candidate/equivalent ce})))
    (when-not (= (:case/id parent) (:case/id candidate))
      (throw (summary-error :verdicts-misaligned
                            "paired verdicts must share the same :case/id"
                            {:parent-case (:case/id parent)
                             :candidate-case (:case/id candidate)})))
    (when-not (= (:repetition parent) (:repetition candidate))
      (throw (summary-error :verdicts-misaligned
                            "paired verdicts must share the same :repetition"
                            {:parent-repetition (:repetition parent)
                             :candidate-repetition (:repetition candidate)})))
    (let [ps (:pair/seed parent)
          cs (:pair/seed candidate)]
      (when (and ps cs (not= ps cs))
        (throw (summary-error :seed-mismatch
                              "paired verdicts must share the same :pair/seed"
                              {:parent-seed ps :candidate-seed cs}))))
    {:case/id (:case/id parent)
     :repetition (:repetition parent)
     :pair/seed (or (:pair/seed parent) (:pair/seed candidate))
     :parent (side-verdict pe)
     :candidate (side-verdict ce)}))

(defn- pair-outcome
  "The win/loss/equiv outcome of ONE paired verdict, in the paired
  runner's own vocabulary: :win when the candidate is strictly better
  (it delivered while the parent did not — paired :candidate-wins),
  :loss when the parent is strictly better (paired :parent-wins),
  :equiv when BOTH sides are equivalent (the paired :tie), and
  :both-failed when NEITHER side is equivalent (the paired
  :both-failed — a shared failure, never counted as a win or as an
  equiv of success)."
  [{:keys [parent candidate]}]
  (let [pe (:equivalent parent)
        ce (:equivalent candidate)]
    (cond
      (and ce (not pe)) :win
      (and pe (not ce)) :loss
      (and pe ce) :equiv
      :else :both-failed)))

(defn- zeroed-counts
  "A fresh zeroed tally: the win/loss/equiv/both-failed counts plus
  :total. Fresh per use — never shared or mutated."
  []
  {:win 0 :loss 0 :equiv 0 :both-failed 0 :total 0})

(defn- tally
  "Add one pair outcome to a counts map (increments the outcome key
  and the :total)."
  [counts outcome]
  (-> counts
      (update outcome inc)
      (update :total inc)))

(defn aggregate-verdicts
  "Aggregate paired judge verdicts into a utility summary (component):

      (aggregate-verdicts pairs)
      ;; => {:total 4 :win 1 :loss 1 :equiv 1 :both-failed 1
      ;;     :by-category {:sel/c1 {:total 2 :win 1 :loss 1
      ;;                             :equiv 0 :both-failed 0}
      ;;                   :sel/c2 {...}}}

  Input: a sequential collection of paired verdict maps built with
  verdict-pair — one per (case, repetition), each carrying the parent
  and candidate sides' boolean :equivalent. The summary counts, in
  the paired runner's own vocabulary:

    :win         — the candidate is strictly better (candidate
                   equivalent, parent not) — paired :candidate-wins.
    :loss        — the parent is strictly better (parent equivalent,
                   candidate not) — paired :parent-wins.
    :equiv       — BOTH sides equivalent — the paired :tie.
    :both-failed — BOTH sides NOT equivalent — the paired
                   :both-failed; a shared failure is never counted as
                   a win or as an equiv of success.
    :total       — win + loss + equiv + both-failed (= the input
                   length); every input pair is accounted for exactly
                   once, so the counts always sum to the total.

  :by-category is the per-case (category) breakdown — the same
  win/loss/equiv/both-failed/total tally per :case/id (the only
  grouping dimension the verdict records carry), keyed in a sorted
  map so the breakdown's iteration order is STABLE across runs:
  identical input always yields an identical summary.

  Pure and deterministic; the empty list yields a ZEROED summary (all
  counts 0, empty :by-category). Fail-loud on malformed input with
  :eval/judge-summary-invalid (:reason distinguishes :not-sequential,
  :pair-not-a-map, :pair-missing-case-id, :pair-missing-sides,
  :non-boolean-equivalent)."
  [pairs]
  (when-not (sequential? pairs)
    (throw (summary-error :not-sequential
                          "paired verdicts must be a sequential collection"
                          pairs)))
  (doseq [p pairs]
    (when-not (map? p)
      (throw (summary-error :pair-not-a-map
                            "each paired verdict must be a map"
                            p)))
    (when-not (keyword? (:case/id p))
      (throw (summary-error :pair-missing-case-id
                            "each paired verdict must carry a keyword :case/id"
                            p)))
    (let [parent (:parent p)
          candidate (:candidate p)]
      (when-not (and (map? parent) (map? candidate))
        (throw (summary-error :pair-missing-sides
                              "each paired verdict must carry :parent and :candidate sides"
                              p)))
      (when-not (and (instance? Boolean (:equivalent parent))
                     (instance? Boolean (:equivalent candidate)))
        (throw (summary-error :non-boolean-equivalent
                              "each side must carry a boolean :equivalent"
                              p)))))
  (let [totals (reduce tally (zeroed-counts) (map pair-outcome pairs))
        by-category (into (sorted-map)
                          (map (fn [[case-id case-pairs]]
                                 [case-id
                                  (reduce tally (zeroed-counts)
                                          (map pair-outcome case-pairs))]))
                          (group-by :case/id pairs))]
    (assoc totals :by-category by-category)))

(defn join-utility-summary
  "Join the judge-derived utility summary into a paired outcome record
  (component). evoclj.eval.paired stays read-only; this pure join is
  the wiring point for the orchestrator:

      (join-utility-summary paired-result summary)
      ;; => paired-result assoc'd with :utility/summary = summary

  The joined key is namespaced in the :utility namespace but is
  DISTINCT from the :utility section of the evaluation summary —
  evoclj.eval.metrics/summarize-utility derives the task/success rate
  from the paired scores, while this summary carries the judge's
  win/loss/equiv aggregation over the per-case verdicts. Pure and
  deterministic; a non-map paired outcome fails loud with
  :eval/judge-summary-invalid."
  [paired-result summary]
  (when-not (map? paired-result)
    (throw (summary-error :paired-result-not-a-map
                          "the paired outcome must be a map"
                          paired-result)))
  (assoc paired-result :utility/summary summary))

;; --- component — judge calibration harness ----------------------------------

(def ^:private calibration-labels
  "The binary ground-truth labels a calibration pair may carry: the
  judge's verdict either agrees with the label (:equivalent expects
  true, :different expects false) or it does not."
  #{:equivalent :different})

(defn- calibration-error
  "A typed :eval/judge-calibration-invalid error for the calibration
  boundary, with a :reason distinguishing the failure (Global
  Constraint 22 — plain serializable data)."
  [reason message value]
  (err/error :eval/judge-calibration-invalid message
             {:reason reason :value (err/sanitize value)}))

(defn- check-pair!
  "Validate one calibration PAIR (a fixture entry): a map with a
  keyword :calib/id, a binary :label, a present :expected-output, and
  a sequential :outputs. Throws :eval/judge-calibration-invalid on the
  first violation."
  [pair]
  (when-not (map? pair)
    (throw (calibration-error :pair-not-a-map
                              "calibration pairs must be maps"
                              pair)))
  (when-not (keyword? (:calib/id pair))
    (throw (calibration-error :pair-missing-id
                              "each calibration pair must carry a keyword :calib/id"
                              pair)))
  (when-not (keyword? (:label pair))
    (throw (calibration-error :pair-missing-label
                              "each calibration pair must carry a keyword :label"
                              pair)))
  (when-not (contains? calibration-labels (:label pair))
    (throw (calibration-error :unsupported-label
                              "calibration labels are binary: :equivalent or :different"
                              (:label pair))))
  (when-not (contains? pair :expected-output)
    (throw (calibration-error :pair-missing-expected
                              "each calibration pair must carry :expected-output"
                              pair)))
  (when-not (sequential? (:outputs pair))
    (throw (calibration-error :pair-outputs-not-sequential
                              "each calibration pair's :outputs must be sequential"
                              (:outputs pair))))
  pair)

(defn- check-record!
  "Validate ONE calibration RECORD (the per-pair outcome of running the
  judge): a map with a keyword :calib/id, a binary :label, and a
  boolean :judge/equivalent. Throws
  :eval/judge-calibration-invalid on the first violation."
  [record]
  (when-not (map? record)
    (throw (calibration-error :pair-not-a-map
                              "calibration records must be maps"
                              record)))
  (when-not (keyword? (:calib/id record))
    (throw (calibration-error :pair-missing-id
                              "each calibration record must carry a keyword :calib/id"
                              record)))
  (when-not (keyword? (:label record))
    (throw (calibration-error :pair-missing-label
                              "each calibration record must carry a keyword :label"
                              record)))
  (when-not (contains? calibration-labels (:label record))
    (throw (calibration-error :unsupported-label
                              "calibration labels are binary: :equivalent or :different"
                              (:label record))))
  (when-not (instance? Boolean (:judge/equivalent record))
    (throw (calibration-error :non-boolean-judge-equivalent
                              "each calibration record must carry a boolean :judge/equivalent"
                              (:judge/equivalent record))))
  record)

(defn- agrees?
  "Does the judge's boolean verdict agree with the binary ground-truth
  label? :equivalent expects true, :different expects false."
  [label judge-equivalent]
  (if (= :equivalent label)
    judge-equivalent
    (not judge-equivalent)))

(defn calibration-judgement
  "Run ONE judge decision over ONE calibration pair and produce the
  per-pair calibration record (component):

      (calibration-judgement judge-fn pair)
      ;; => {:calib/id <kw>
      ;;     :label <:equivalent|:different>
      ;;     :judge/equivalent <bool>
      ;;     :agree <bool>}

  The judge fn has the equivalence contract (fn [expected-output
  outputs] -> boolean) — the same shape llm-judge returns. The judge
  call is the ONLY effect channel (Global Constraint 8): with a real
  model this is one model call per pair; in CI the fixture judge is
  deterministic and network-free, so the fixture reports exact
  agreement. The record is plain serializable data (Global Constraint
  22). A malformed pair fails loud with
  :eval/judge-calibration-invalid BEFORE the judge runs."
  [judge-fn pair]
  (check-pair! pair)
  (let [label (:label pair)
        equivalent (judge-fn (:expected-output pair) (:outputs pair))]
    {:calib/id (:calib/id pair)
     :label label
     :judge/equivalent (boolean equivalent)
     :agree (agrees? label (boolean equivalent))}))

(defn agreement-stats
  "The pure calibration agreement statistics (component):

      (agreement-stats records)
      ;; => {:total 10 :agree 10 :disagree 0
      ;;     :by-label {:equivalent {:total 5 :agree 5 :disagree 0}
      ;;                :different {:total 5 :agree 5 :disagree 0}}}

  Input: the per-pair calibration records produced by
  calibration-judgement (or hand-built maps carrying :calib/id,
  :label, :judge/equivalent). Agreement is DERIVED from :label and
  :judge/equivalent — a stale :agree field on the input is never
  trusted. The summary counts:

    :total    — the input length; every record is accounted for once.
    :agree    — the judge's verdict matches the binary label
                (:equivalent expects true, :different expects false).
    :disagree — the judge's verdict contradicts the label.
    :by-label — the same total/agree/disagree tally per label, in a
                sorted map so the breakdown's iteration order is STABLE
                (identical input always yields an identical summary);
                both binary labels are always present (zeroed when
                absent).

  Pure and deterministic; the empty list yields a ZEROED summary.
  Fail-loud on malformed input with :eval/judge-calibration-invalid
  (:reason distinguishes :not-sequential, :pair-not-a-map,
  :pair-missing-id, :pair-missing-label, :unsupported-label,
  :non-boolean-judge-equivalent)."
  [records]
  (when-not (sequential? records)
    (throw (calibration-error :not-sequential
                              "calibration records must be a sequential collection"
                              records)))
  (doseq [r records]
    (check-record! r))
  (let [label-totals
        (reduce (fn [acc r]
                  (let [agree? (agrees? (:label r) (:judge/equivalent r))]
                    (-> acc
                        (update-in [(:label r) :total] inc)
                        (update-in [(:label r) (if agree? :agree :disagree)] inc))))
                {:equivalent {:total 0 :agree 0 :disagree 0}
                 :different {:total 0 :agree 0 :disagree 0}}
                records)
        agree (+ (:agree (:equivalent label-totals))
                 (:agree (:different label-totals)))
        disagree (+ (:disagree (:equivalent label-totals))
                    (:disagree (:different label-totals)))]
    {:total (+ agree disagree)
     :agree agree
     :disagree disagree
     :by-label (into (sorted-map) label-totals)}))

(defn run-calibration
  "The judge calibration HARNESS (component): run a judge fn over a
  calibration fixture and report agreement:

      (run-calibration judge-fn pairs)
      ;; => {:verdicts [<calibration-judgement records>]
      ;;     :stats <agreement-stats summary>}

  judge-fn is any equivalence fn (fn [expected-output outputs] ->
  boolean) — llm-judge for a real model, the fixture judge in CI. The
  verdicts are the per-pair records (one judge call per pair — the only
  effect channel, Global Constraint 8) and :stats is the pure
  agreement-stats summary over them. Pure except for the judge calls."
  [judge-fn pairs]
  (let [verdicts (mapv (fn [p] (calibration-judgement judge-fn p)) pairs)]
    {:verdicts verdicts
     :stats (agreement-stats verdicts)}))
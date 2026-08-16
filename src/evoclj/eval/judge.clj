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
    :system-prompt — optional string; defaults to a built-in prompt that
                   instructs the model to decide semantic equivalence and
                   answer with STRICT JSON {\"equivalent\": true|false}.
    :max-tokens  — optional pos-int, default 1024. :temperature is fixed
                   at 0.0 (deterministic judgement).

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

  Task A6 (Foundation F3): this namespace also owns the judge-verdict
  enrichment adapter. Per-case judge verdicts persist as versioned
  enrichment records attached to the :evaluation entity (entity-kind
  :evaluation, entity-id = the evaluation id, kind :judge-verdict,
  payload in the CAS): the pure verdict→enrichment mapping
  (verdict-record, verdict-payload) plus the isolated store adapter
  (persist-verdict-enrichment!) that NEVER fails the evaluation — a
  store failure is caught and returned as a :failed record."
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
  string), optional :system-prompt and :max-tokens."
  [:map {:closed true}
   [:model-call fn?]
   [:model/id string?]
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
        system-prompt (or (:system-prompt v) default-system-prompt)
        max-tokens (or (:max-tokens v) 1024)]
    (fn [expected outputs]
      (let [messages [{:role :system :content system-prompt}
                      {:role :user
                       :content (user-message expected outputs)}]
            result (try
                     (model-call model-id messages
                                 {:temperature 0.0 :max-tokens max-tokens})
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

;; --- Task A6 — judge verdicts as enrichment records (Foundation F3) -----------

(defn verdict-record
  "ONE per-case judge verdict as a plain EDN map — the pure
  verdict→enrichment mapping (Task A6):

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
  "The Task A6 enrichment :payload for a verdict batch: a plain EDN map
  {:verdicts <vector of verdict-record maps> :count n}. Pure — the
  exact map stored in the CAS via
  evoclj.store.enrichment/put-enrichment! (the row keeps only the
  content-address :payload-ref, never the body)."
  [verdicts]
  {:verdicts (vec verdicts)
   :count (count verdicts)})

(defn persist-verdict-enrichment!
  "Task A6 — the judge-verdict store adapter (Foundation F3). Persist a
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

  FAILURE ISOLATION (Task A6 acceptance): enrichment persistence must
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
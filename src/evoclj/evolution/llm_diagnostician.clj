(ns evoclj.evolution.llm-diagnostician
  "An LLM-driven Diagnostician adapter (component Step 3, feature 1).

  This adapter conforms to the Diagnostician protocol of
  evoclj.evolution.diagnose: it consumes EXACTLY ONE value — the frozen,
  content-addressed Evolution-set evidence pack produced by
  evoclj.evolution.evidence/build-evidence-pack — and returns a validated,
  content-addressed Diagnosis:

      {:diagnosis/id \"sha256:...\"
       :evidence/id \"sha256:...\"
       :hypotheses [{:hypothesis/id #uuid  ; deterministic name-based UUID
                     :pattern keyword?     ; e.g. :task/success, or new keywords
                     :claim string?
                     :support [...]
                     :counterevidence [...]
                     :target {...}
                     :expected-effect {...}
                     :confidence-band :low|:medium|:high}]}

  GLOBAL CONSTRAINT 11 (store isolation): this adapter holds NO store
  handle and NO Selection/Audit fixture handle. Everything it can see is
  exactly the pack it is handed — the episode summaries are plain EDN
  metadata (:episode/id :outcome :usage :trace) with no CAS excerpt
  dereference. The adapter NEVER calls a provider directly (Global
  Constraint 8): all external effects flow through the injected
  :model-call closure the host closes over the kernel's capability
  broker. The :model-call fn is the ONLY effect channel, and it is always
  injected by the host — never constructed here.

  The model is asked to output STRICT JSON (an object with a
  \"hypotheses\" array), but real models wrap output in prose or code
  fences, so the adapter tolerates noisy responses (a robust JSON
  extractor plus per-entry schema filtering) while still failing loud
  when nothing usable survives (see the LLM-NOISE TOLERANCE POLICY
  below).

  Determinism / content addressing is preserved: :hypothesis/id is a
  deterministic name-based UUID over the hypothesis body, and
  :diagnosis/id is the sha256 content hash of the diagnosis body, using
  the same canonical/digest conventions as evoclj.evolution.diagnose.
  The same (model output, pack) therefore always yields the same
  diagnosis for the same config; the model-call is the only
  nondeterministic input.

  LLM-NOISE TOLERANCE POLICY (fail-loud): the adapter never guesses at
  structurally broken model output. A response that cannot be parsed, or
  whose \"hypotheses\" array is entirely unusable, throws a typed error
  rather than silently returning an empty diagnosis, so the evolution
  loop does not mistake a model failure for an absence of hypotheses.
  Individually invalid hypothesis entries are skipped (noise — a model
  may emit a malformed entry among good ones), but a response that
  parses to nothing, or whose array is ALL noise, is a failure. Zero
  hypotheses is only ever returned honestly when the model itself
  produced an empty, structurally valid array.

  Error contract (Global Constraint 22 — plain serializable data):
    :diagnosis/config-invalid       — bad constructor config (closed
                                       map, missing required fields,
                                       wrong types)
    :diagnosis/llm-failed           — the :model-call execution failed
                                       (provider error or any Throwable)
    :diagnosis/llm-response-invalid — the model text was not usable
                                       (non-JSON, missing/unusable
                                       hypotheses, or all-noise)."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [evoclj.evolution.diagnose :refer [Diagnostician]]
            [evoclj.evolution.diagnosis-schema :as ds]
            [evoclj.evolution.evidence-schema :as es]
            [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err]
            [malli.core :as m]
            [malli.error :as me])
  (:import (java.nio.charset StandardCharsets)
           (java.util UUID)))

;; --- content addressing (same conventions as evoclj.evolution.diagnose) ---

(defn- utf8-bytes
  "UTF-8 bytes of a string."
  [s]
  (.getBytes ^String s StandardCharsets/UTF_8))

(defn- canonical
  "Deterministic EDN form for hashing — the same convention as
  evoclj.evolution.evidence/canonical and evoclj.evolution.diagnose:
  maps sorted by their pr-str key form, sets by their pr-str element
  form, collections realized eagerly. The :diagnosis/id is a pure
  function of logical content (Global Constraint 6)."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn- digest
  "Content hash (sha256:<64 hex>) of the canonical pr-str of `data`."
  [data]
  (hash/text-digest (pr-str (canonical data))))

(defn- deterministic-uuid
  "A deterministic name-based UUID (v3, nameUUIDFromBytes) over the
  canonical pr-str of `data`: identical logical content always maps to
  the same id, so a diagnosis is a pure function of its evidence pack
  and the model output it was derived from."
  [data]
  (UUID/nameUUIDFromBytes (utf8-bytes (pr-str (canonical data)))))

;; --- constructor config (closed map — Global Constraint 11) ---

(def ^:private default-system-prompt
  (str/join
   "\n"
   ["You are an evolution diagnostician for EvoCLJ, a self-evolving agent runtime."
    "You receive a frozen evidence pack describing a set of executed episodes (task outcomes,"
    "resource usage, trace bounds) and you propose structured, evidence-grounded hypotheses"
    "that a mutation step could act on."
    ""
    "Respond with STRICT JSON only — an object with a single top-level \"hypotheses\" array:"
    "{\"hypotheses\": ["
    "  {\"pattern\": \"<keyword, e.g. task/success or a new one>\","
    "   \"claim\": \"<one english sentence>\","
    "   \"support\": [{\"episode-id\": \"<episode id string>\", \"event-ids\": [<int>]}],"
    "   \"counterevidence\": [{\"episode-id\": \"<episode id string>\"}],"
    "   \"target-kind\": \"skill|workflow\", \"target-id\": \"<keyword>\","
    "   \"effect-metric\": \"<keyword e.g. task/success>\", \"effect-direction\": \"increase|decrease\","
    "   \"confidence-band\": \"low|medium|high\""
    "  }]"
    "}"
    ""
    "Cite ONLY episode ids and event ids that appear in the evidence pack. Never invent ids."]))

(def LlmDiagnosticianConfigSchema
  "The LLM adapter's constructor config — a CLOSED map (Global
  Constraint 11: no store handle, Selection/Audit fixture, or any other
  unknown key may be smuggled in). :model-call is a zero-dependency
  closure injected by the host that performs exactly one provider call
  through the kernel's capability broker."
  [:map {:closed true}
   [:model-call fn?]
   [:model/id string?]
   [:max-hypotheses {:optional true} pos-int?]
   [:confidence-band {:optional true} [:enum :low :medium :high]]
   [:system-prompt {:optional true} string?]])

(defn- throw-config-invalid!
  "Throw :diagnosis/config-invalid carrying the humanized Malli
  explanation."
  [expl]
  (throw (err/error :diagnosis/config-invalid
                    "llm-diagnostician config does not satisfy the diagnosis contract"
                    {:errors (me/humanize expl)})))

(defn- validate-llm-config
  "Validate an LLM adapter config map. Returns it unchanged, or throws
  :diagnosis/config-invalid. This is a private closed-schema check
  distinct from evoclj.evolution.diagnosis-schema/validate-config (the
  pattern adapter's config) — the LLM config carries different,
  model-call-specific required fields."
  [config]
  (if-let [expl (m/explain LlmDiagnosticianConfigSchema config)]
    (throw-config-invalid! expl)
    config))
;; --- evidence-pack rendering (bounded user message) ---

(def ^:private max-episodes-rendered
  "The hard cap on how many episode summaries are rendered into the user
  message. Large packs are truncated to keep the model context bounded.
  Truncation is explicitly noted to the model so it knows some episodes
  were omitted."
  40)

(defn- render-pack
  "Render a compact, bounded representation of the pack for the model:
  the :evidence/id, :generation/id, :cutoff-event-id, :summary, then up
  to max-episodes-rendered episode summaries (episode id (as a string),
  outcome status, usage, trace bounds). Episode ids are rendered as
  strings so the model can safely echo them back into its JSON. When the
  pack holds more episodes than the cap, a :truncated flag and a
  human-readable :note are included."
  [pack]
  (let [episodes (:episodes pack)
        rendered (mapv (fn [e]
                         {:episode/id (str (:episode/id e))
                          :outcome (:outcome e)
                          :usage (:usage e)
                          :trace (:trace e)})
                       (take max-episodes-rendered episodes))]
    {:evidence/id (:evidence/id pack)
     :generation/id (:generation/id pack)
     :cutoff-event-id (:cutoff-event-id pack)
     :summary (:summary pack)
     :episodes rendered
     :episode-count (count episodes)
     :truncated (> (count episodes) max-episodes-rendered)
     :note (when (> (count episodes) max-episodes-rendered)
             (str "Only the " max-episodes-rendered
                  " most recent episodes are shown; "
                  (- (count episodes) max-episodes-rendered)
                  " older episodes were omitted."))}))

(defn- user-message
  "Build the user-message string handed to the model: a compact EDN
  rendering of the pack (evidence id, generation, cutoff, summary, and
  bounded episode summaries)."
  [pack]
  (str "Analyze the following evolution evidence pack. "
       "Propose hypotheses per the system instructions.\n"
       (binding [*print-length* 1000]
         (pr-str (render-pack pack)))))

;; --- robust JSON / hypothesis extraction ---

(defn- to-keyword
  "Coerce a value to a keyword when it is a string or already a
  keyword; returns nil otherwise (so structurally wrong values are
  treated as missing rather than crashing)."
  [x]
  (cond
    (keyword? x) x
    (string? x) (keyword x)
    :else nil))

(defn- as-vector
  "Return `x` as a vector when it is sequential, else nil."
  [x]
  (when (sequential? x) (vec x)))

(defn- parse-model-json
  "Parse the model's output `text` into a keywordized object map.

  Robust extractor: first try a direct parse of the whole text as a
  JSON object; if that fails, locate the FIRST '{' and the LAST '}' and
  parse the substring between them (handles prose and code-fence
  wrapping — the first/last-brace model assumes the single JSON object
  is delimited by those two braces, which holds for the fenced replies
  we request). Returns the decoded keywordized map, or nil when neither
  attempt succeeds."
  [text]
  (when (string? text)
    (or (try (json/decode text true)
             (catch Exception _ nil))
        (let [first-brace (str/index-of text "{")
              last-brace (str/last-index-of text "}")]
          (when (and first-brace last-brace (< first-brace last-brace))
            (try (json/decode (subs text first-brace (inc last-brace)) true)
                 (catch Exception _ nil)))))))

(defn- coerce-uuid
  "Coerce a value to a java.util.UUID, or nil when it cannot be parsed
  as one."
  [x]
  (when x
    (try (java.util.UUID/fromString (str x))
         (catch Exception _ nil))))

(defn- ref-entry
  "Build one :support ref entry from a raw map: the episode id (via one
  of the accepted key spellings) coerced to a UUID, plus the event ids
  (a vector of positive ints). Returns nil when the episode id is
  absent or not a valid UUID (noise)."
  [m]
  (when (map? m)
    (let [eid (coerce-uuid (some m [:episode/id :episode-id]))]
      (when eid
        {:episode/id eid
         :event-ids (->> (or (:event-ids m) [])
                         (filter pos-int?)
                         (mapv identity))}))))

(defn- counter-ref-entry
  "Build one :counterevidence ref entry: the episode id (via one of the
  accepted key spellings) coerced to a UUID. Returns nil on a missing or
  invalid id."
  [m]
  (when (map? m)
    (let [eid (coerce-uuid (some m [:episode/id :episode-id]))]
      (when eid
        {:episode/id eid}))))

(defn- parse-raw-entry
  "Normalize one raw, keywordized model hypothesis entry into a
  candidate hypothesis map in the diagnosis-schema vocabulary, or nil
  when the entry is not structurally usable (LLM noise).

  The model is prompted to emit flat keys
  (pattern/claim/support/counterevidence, target-kind/target-id,
  effect-metric/effect-direction, confidence-band); this normalizes
  them into the schema's nested :target, :expected-effect and
  :support/:counterevidence shapes. Episode ids are coerced to UUIDs; a
  non-UUID id yields an empty ref so the entry is dropped by schema
  validation. Missing pattern/claim/target/effect-metric all yield
  nil so the entry is skipped as noise."
  [raw]
  (when (map? raw)
    (let [target-kind (to-keyword (:target-kind raw))
          target-id (to-keyword (:target-id raw))
          effect-metric (to-keyword (:effect-metric raw))
          pattern (to-keyword (:pattern raw))
          claim (:claim raw)]
      (when (and pattern claim target-kind target-id effect-metric)
        {:pattern pattern
         :claim claim
         :support (vec (keep ref-entry (as-vector (:support raw))))
         :counterevidence (vec (keep counter-ref-entry (as-vector (:counterevidence raw))))
         :target {:kind target-kind :id target-id}
         :expected-effect {:metric effect-metric
                           :direction (if (= (to-keyword (:effect-direction raw)) :decrease)
                                        :decrease
                                        :increase)}
         :confidence-band (or (to-keyword (:confidence-band raw)) :medium)}))))

(defn- finalize-hypothesis
  "Compute the deterministic name-based :hypothesis/id over the
  hypothesis content (everything except the id), then validate with
  ds/validate-hypothesis. Returns the validated hypothesis, or nil
  when the candidate fails schema validation (skipped as LLM noise)."
  [candidate]
  (let [with-id (assoc candidate :hypothesis/id (deterministic-uuid candidate))]
    (try
      (ds/validate-hypothesis with-id)
      (catch clojure.lang.ExceptionInfo _ nil))))
;; --- the LLM adapter ---

(defn- output-text
  "Extract the :text string from the provider result :value (a map
  {:model/output {:text ... :reasoning ...} :usage ...}), or nil when
  the shape is not as the model-call contract promises (a typed
  :diagnosis/llm-response-invalid failure follows)."
  [value]
  (when (map? value)
    (let [mo (:model/output value)]
      (when (map? mo)
        (let [t (:text mo)]
          (when (string? t) t))))))

(defrecord LlmDiagnostician [config]
  Diagnostician
  (diagnose [_ evidence-pack]
    ;; 1. validate the frozen evidence pack at the trust boundary
    (es/validate-pack evidence-pack)
    (let [model-call (:model-call config)
          model-id (:model/id config)
          max-hypotheses (:max-hypotheses config)
          confidence-band (:confidence-band config)
          system-prompt (:system-prompt config)
          ;; 2. bounded user message
          messages [{:role :system :content (or system-prompt default-system-prompt)}
                    {:role :user :content (user-message evidence-pack)}]]
      ;; 3. exactly ONE model call through the injected :model-call closure
      (let [result (try
                     (model-call model-id messages
                                 {:temperature 0.2 :max-tokens 4096})
                     (catch clojure.lang.ExceptionInfo e
                       (throw (err/error :diagnosis/llm-failed
                                         "model call failed during diagnosis"
                                         {:error/type (:error/type (ex-data e))
                                          :cause (err/error-data e)})))
                     (catch Throwable t
                       (throw (err/error :diagnosis/llm-failed
                                         "model call failed during diagnosis"
                                         {:error/type :diagnosis/llm-failed
                                          :cause (err/error-data t)}))))
            ;; the host fn contract: a dispatch result with :result/status
            ;; :ok, or a thrown typed error. A non-ok status is a
            ;; defensive guard, not a substitute for the contract.
            _ (when (and (map? result)
                         (contains? result :result/status)
                         (not= :ok (:result/status result)))
                (throw (err/error :diagnosis/llm-failed
                                  "model-call returned a non-ok dispatch result"
                                  {:result/status (:result/status result)
                                   :cause (dissoc result :value)})))
            value (:value result)
            text (output-text value)]
        (when-not (string? text)
          (throw (err/error :diagnosis/llm-response-invalid
                            "model-call did not produce usable text output"
                            {:reason :non-string-output
                             :value-type (some-> value class .getName)})))
        ;; 4. parse as JSON (robust extractor)
        (let [parsed (parse-model-json text)]
          (when-not parsed
            (throw (err/error :diagnosis/llm-response-invalid
                              "model output was not parseable as JSON"
                              {:reason :not-json
                               :excerpt (subs text 0 (min 200 (count text)))})))
          (let [raw-hypotheses (as-vector (:hypotheses parsed))]
            (when-not raw-hypotheses
              (throw (err/error :diagnosis/llm-response-invalid
                                "model output has no usable hypotheses array"
                                {:reason :missing-hypotheses
                                 :keys (vec (keys parsed))})))
            ;; 5. normalize + validate each entry; skip noise entries
            (let [validated (->> raw-hypotheses
                                 (keep parse-raw-entry)
                                 (keep finalize-hypothesis)
                                 (mapv (fn [h] (assoc h :confidence-band confidence-band))))]
              ;; all-noise and non-empty arrays are a failure (fail-loud)
              (when (and (seq raw-hypotheses) (empty? validated))
                (throw (err/error :diagnosis/llm-response-invalid
                                  "model output contained hypotheses but none validated"
                                  {:reason :all-hypotheses-invalid
                                   :count (count raw-hypotheses)})))
              ;; 6. bound, assemble, content-address
              (let [hypotheses (into [] (take max-hypotheses) validated)
                    data {:evidence/id (:evidence/id evidence-pack)
                          :hypotheses hypotheses}
                    id (digest data)
                    diagnosis (assoc data :diagnosis/id id)]
                (ds/validate-diagnosis diagnosis)
                diagnosis))))))))

(defn llm-diagnostician
  "Construct the LLM-driven Diagnostician adapter from a config map.

  The config is a CLOSED map (any unknown key — a store handle, a
  Selection/Audit fixture, a provider object — is rejected with
  :diagnosis/config-invalid; Global Constraint 11). Required keys:

    :model-call  — a Clojure fn injected by the host:
                   (fn [model-id messages options]) -> the broker
                   dispatch result of exactly ONE model call:
                   {:result/status :ok
                    :value {:model/output {:text string?
                                           :reasoning (optional)}
                            :usage {...}}}
                   (the shape evoclj.intent.dispatch/dispatch!
                   returns for :intent/model-call). The fn either
                   returns that success result or throws ExceptionInfo
                   with a stable :error/type; the adapter takes the
                   :value of the returned map and never calls a
                   provider directly (Global Constraint 8).
    :model/id    — a string model identifier, passed through to
                   :model-call unchanged.

  Optional keys (with defaults):

    :max-hypotheses  — pos-int, default 3: the hard cap on returned
                       hypotheses.
    :confidence-band — :low | :medium | :high, default :medium;
                       stamped onto every returned hypothesis.
    :system-prompt   — a string overriding the built-in STRICT-JSON
                       instruction prompt.

  The adapter holds no store handle and sees only the pack it is
  handed (Global Constraint 11). diagnose never fails silently:
  per-entry schema noise is tolerated, but a model that produces
  nothing usable throws a typed error (LLM-NOISE TOLERANCE POLICY)."
  [config]
  (let [v (validate-llm-config config)]
    (->LlmDiagnostician {:model-call (:model-call v)
                         :model/id (:model/id v)
                         :max-hypotheses (or (:max-hypotheses v) 3)
                         :confidence-band (or (:confidence-band v) :medium)
                         :system-prompt (:system-prompt v)})))



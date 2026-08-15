(ns evoclj.evolution.llm-mutator
  "An LLM-driven Mutator adapter (feature 2 of 3).

  This adapter conforms to the Mutator protocol of
  evoclj.evolution.core: it consumes the FULL, closed per-cycle
  context (the generation id, the loaded parent Genome, the validated
  Diagnosis, the Task 7.7 negative-history entries, and the budget
  profile) and returns a finite vector of Mutation IR maps to propose
  this cycle, or nil when nothing is proposed.

      {:generation/id \"generation-1\"
       :parent/genome-id \"sha256:...\"
       :parent-genome <loaded Genome map (evoclj.genome.load/load-genome)>
       :diagnosis {:diagnosis/id ... :evidence/id ... :hypotheses [...]}
       :history [<Task 7.7 history entries>]
       :budget-profile <map from evoclj.evolution.budget/v0-profile>}

  It returns mutations carrying ONLY what the adapter owns — :risk
  (defaulted), :ops, and optionally :expected-effect and
  :hypothesis/id. The orchestrator completes the lineage fields
  (:parent/genome-id, :evidence/id, :mutation/id, and the default
  :hypothesis/id) itself.

  GLOBAL CONSTRAINT 11 (store isolation) and GLOBAL CONSTRAINT 8
  (all external effects cross the kernel-owned capability broker):
  this adapter holds NO store handle and calls NO provider directly.
  :model-call is the ONLY effect channel — a zero-dependency closure
  injected by the host, closed over the kernel's capability broker. It
  is always injected, never constructed here.

  THE KERNEL-COMPUTES-HASH RULE (the adapter's security property):
  the Mutation IR op schemas (evoclj.evolution.mutation-schema)
  REQUIRE :expect/hash — the \"sha256:<64 hex>\" preimage digest of the
  op's target file's CURRENT content — on every destructive/replace op
  (:set-edn :delete-edn :replace-text :delete-text :replace-form
  :delete-form :remove-node :remove-edge :update-node), and take it
  (optionally) on the pure-add ops (:insert-text :insert-form
  :add-node :add-edge). A language model cannot compute digests, so the
  model is asked to propose ops WITHOUT :expect/hash (an \"intent\" form)
  and this adapter computes and attaches :expect/hash for EVERY op from
  the parent genome's :files digest:

      (get-in parent [:files rel-file :digest])

  The digest convention is identical to the patch runtime's preimage
  verification (:files :digest comes from evoclj.genome.hash/file-digest,
  the same convention evoclj.genome.patch hashes against). A model can
  never name a preimage it does not know, so stale patches are
  impossible.

  HASH-COMPLETION / DROPPED-MUTATION POLICY: a mutation whose ANY op
  references a file NOT in the parent's :files map (the model
  hallucinated a file) cannot be made schema-valid — that whole
  mutation is DROPPED from the returned vector (the cycle is not
  crashed). Structurally broken mutations (a non-map element, or a
  missing/empty :ops vector) are also dropped silently (LLM noise).
  BUT the adapter fails loud when the model produced a NON-EMPTY
  :mutations array and every last mutation was dropped (all-noise) —
  see the LLM-NOISE TOLERANCE POLICY below. It does NOT pre-filter
  protected paths (eval/, capability/, kernel/, manifest.edn, the
  :evolution module file) or undeclared mutable classes — that is
  evoclj.evolution.mutation/validate-mutation's job in the
  orchestrator, which records an :evolution/candidate-invalid event
  per mutation (observability). Hash completion is this adapter's
  unique responsibility.

  LLM-NOISE TOLERANCE POLICY (fail-loud): the adapter never guesses at
  structurally broken model output. A response that cannot be parsed,
  or whose \"mutations\" array is entirely unusable, throws a typed
  error rather than silently returning nil, so the evolution loop does
  not mistake a model failure for an absence of proposals. Individually
  invalid mutations are skipped (noise — the model may emit a malformed
  entry among good ones), but a response that parses to nothing, or
  whose array is ALL noise, is a failure. An empty, structurally valid
  \"mutations\": [] array is the ONLY honest way to return nil.

  Error contract (Global Constraint 22 — plain serializable data):
    :mutation/config-invalid       — bad constructor config (closed
                                       map, missing required fields,
                                       wrong types)
    :mutation/context-invalid      — the handed context is not the
                                       closed Mutator context map
    :mutation/llm-failed           — the :model-call execution failed
                                       (provider error or any Throwable)
    :mutation/llm-response-invalid — the model text was not usable
                                       (non-JSON, missing/unusable
                                       mutations, or all-noise)"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [evoclj.evolution.core :refer [Mutator]]
            [evoclj.kernel.error :as err]
            [malli.core :as m]
            [malli.error :as me])
  (:import (java.nio.charset StandardCharsets)
           (java.util UUID)))

;; --- constructor config (closed map — Global Constraint 11) ------------------

(def ^:private default-system-prompt
  (str/join
   "\n"
   ["You are an evolution mutator for EvoCLJ, a self-evolving agent runtime."
    "You receive a diagnosis (evidence-grounded hypotheses), the parent Genome's"
    "mutable asset classes and files, recent mutation history, and a budget"
    "profile. You propose structured, schema-valid mutations the kernel can apply"
    "to the parent Genome to address a hypothesis."
    ""
    "Respond with STRICT JSON only — an object with a single top-level"
    "\"mutations\" array:"
    "{\"mutations\": ["
    "  {\"ops\": ["
    "    {\"op\": \"set-edn\", \"file\": \"<relative path under the genome root>\","
    "     \"path\": [\"...\"], \"value\": <EDN value>}"
    "  ],"
    "  \"risk\": \"behavioral\","
    "  \"expected-effect\": {\"primary-metric\": \"<keyword>\", \"direction\": \"increase|decrease\"},"
    "  \"hypothesis-id\": \"<uuid or omit>\"}"
    "]}"
    ""
    "The parent Genome file list tells you WHICH files exist and their kind. The"
    "\"file\" of every op MUST name a file in that list (a relative path under the"
    "genome root) — never invent a file; the adapter drops such mutations."
    ""
    "The op vocabulary is exactly these thirteen ops:"
    "  :set-edn :delete-edn        EDN value navigation (:path selects a nested key;"
    "                              :set-edn also carries :value). Prefer :set-edn for EDN files."
    "  :insert-text :replace-text :delete-text   bounded text edits. Prefer"
    "                              :replace-text for text files."
    "  :replace-form :insert-form :delete-form   Clojure source form edits"
    "  :add-node :remove-node :add-edge :remove-edge :update-node   topology graph edits"
    ""
    "For :set-edn and :delete-edn, :path is a vector of EDN keys selecting a nested"
    "value. For the text ops use :anchor (an exact source string or a 1-based line"
    "number) plus :text for :insert-text / :replace-text. NEVER include :expect/hash"
    "— the kernel computes and attaches it itself. Keep each mutation small and"
    "within the budget profile's limits. Always target mutable asset classes."]))

(def LlmMutatorConfigSchema
  "The LLM adapter's constructor config — a CLOSED map (Global
  Constraint 11: no store handle or any other unknown key may be
  smuggled in). :model-call is a zero-dependency closure injected by
  the host that performs exactly one provider call through the kernel's
  capability broker."
  [:map {:closed true}
   [:model-call fn?]
   [:model/id string?]
   [:max-mutations {:optional true} pos-int?]
   [:risk {:optional true} [:enum :parameter :behavioral :program
                            :topology :meta]]
   [:system-prompt {:optional true} string?]])

(defn- throw-config-invalid!
  "Throw :mutation/config-invalid carrying the humanized Malli
  explanation."
  [expl]
  (throw (err/error :mutation/config-invalid
                    "llm-mutator config does not satisfy the mutation contract"
                    {:errors (me/humanize expl)})))

(defn- validate-llm-config
  "Validate an LLM adapter config map. Returns it unchanged, or throws
  :mutation/config-invalid. This is a private closed-schema check."
  [config]
  (if-let [expl (m/explain LlmMutatorConfigSchema config)]
    (throw-config-invalid! expl)
    config))


;; --- context trust boundary -----------------------------------------------------

(defn- validate-context!
  "Validate the Mutator context at the trust boundary. The context is
  the FULL closed input the protocol describes; the adapter needs at
  least a map carrying :parent-genome (a map with a :files map — the
  source of every op's :expect/hash) and :diagnosis. Throws
  :mutation/context-invalid otherwise, so a cryptic downstream failure
  becomes a typed error."
  [context]
  (when-not (map? context)
    (throw (err/error :mutation/context-invalid
                      "the Mutator context must be a map"
                      {:value (err/sanitize context)})))
  (let [parent (:parent-genome context)
        files (:files parent)]
    (when-not (and (map? parent) (map? files))
      (throw (err/error :mutation/context-invalid
                        "the Mutator context must carry a :parent-genome map with a :files map"
                        {:value (err/sanitize context)}))))
  (when-not (map? (:diagnosis context))
    (throw (err/error :mutation/context-invalid
                      "the Mutator context must carry a :diagnosis map"
                      {:value (err/sanitize context)})))
  context)

;; --- bounded user-message rendering ----------------------------------------------

(def ^:private max-content-bytes
  "The hard cap on how many bytes of mutable-file TEXT content are
  rendered into the user message. Small files (under the running cap)
  are included by content so the model can ground its EDN/text edits;
  larger ones are digest-only. Truncation is explicitly noted to the
  model so it knows some content was omitted."
  20000)

(def ^:private max-history-entries
  "The hard cap on how many recent history entries are rendered into
  the user message. Older entries are omitted and a :truncated flag is
  set, keeping the model context bounded."
  10)

(defn- file-class
  "The mutable asset class of a relative Genome path: its first path
  component, with a root-level file extension stripped.
  \"skills/debugging.edn\" → :skills; \"topology.edn\" → :topology. The
  same rule evoclj.evolution.mutation and evoclj.evolution.budget use
  for the class gates."
  [file]
  (keyword (str/replace (first (str/split file #"/")) #"\.[^.]+$" "")))

(defn- bytes->str
  "Decode a vector of byte values (the immutable :bytes payload of a
  loaded-genome :files entry) back to a UTF-8 string."
  [bs]
  (String. (byte-array (map byte bs)) StandardCharsets/UTF_8))

(defn- render-diagnosis
  "A compact, bounded rendering of the diagnosis for the model: the
  :diagnosis/id and, per hypothesis, the id (as a string), pattern,
  claim, target, and expected effect."
  [diagnosis]
  {:diagnosis/id (:diagnosis/id diagnosis)
   :hypotheses (mapv (fn [h]
                       {:hypothesis/id (some-> (:hypothesis/id h) str)
                        :pattern (:pattern h)
                        :claim (:claim h)
                        :target (:target h)
                        :expected-effect (:expected-effect h)})
                     (:hypotheses diagnosis))})

(defn- render-files
  "Build the bounded file rows for the model: a vector of
  {:path :digest :kind}, plus for small editable (non-binary) files
  their text :content. The global cap is max-content-bytes of included
  content across all files; once the budget is exhausted the remaining
  files are digest-only. Returns {:files rows :content-truncated bool
  :note (optional)}."
  [mutable-rows]
  (reduce (fn [{:keys [remaining files truncated]} [file {:keys [digest kind bytes]}]]
            (let [included? (and (not= :binary kind)
                                 (seq bytes)
                                 (<= (count bytes) remaining))
                  row (cond-> {:path file :digest digest :kind kind}
                        included? (assoc :content (bytes->str bytes)))]
              {:remaining (if included? (- remaining (count bytes)) remaining)
               :files (conj files row)
               :truncated (or truncated (and (not= :binary kind)
                                             (seq bytes)
                                             (not included?)))}))
          {:remaining max-content-bytes :files [] :truncated false}
          mutable-rows))

(defn- render-genome
  "Render the parent's mutable surface for the model: the manifest's
  :evolution :mutable set (the ONLY mutable asset classes) and a file
  table for the mutable-class files. When the declared :mutable set is
  missing or not a set, every file is shown (the orchestrator's class
  gate still rejects undeclared targets); otherwise only files whose
  asset class is declared mutable are shown. Small editable files carry
  their text content; larger ones are digest-only, and the truncation
  is noted."
  [parent]
  (let [files (:files parent)
        mutable (get-in parent [:manifest :evolution :mutable])
        rows (if (set? mutable)
               (sort-by key
                        (filter (fn [[file _]]
                                  (contains? mutable (file-class file)))
                                files))
               (sort-by key files))
        {:keys [row-files content-truncated]}
        (let [r (render-files rows)]
          {:row-files (:files r) :content-truncated (:truncated r)})]
    {:mutable (vec (sort (if (set? mutable) mutable #{})))
     :files row-files
     :content-truncated content-truncated
     :note (when content-truncated
             (str "Only the first " max-content-bytes
                  " bytes of mutable-file content are shown; larger files "
                  "are listed by digest only."))}))

(defn- render-history
  "A bounded rendering of the recent mutation history for the model:
  up to max-history-entries summaries with the mutation id (as a
  string), risk, verdict state, metric deltas, rejection reason, and
  the negative-evidence flag. A :truncated flag is set when older
  entries were omitted."
  [entries]
  (let [rows (take max-history-entries entries)]
    {:count (count entries)
     :truncated (> (count entries) max-history-entries)
     :entries (mapv (fn [e]
                      {:mutation/id (some-> (:mutation/id e) str)
                       :risk (:risk e)
                       :state (:state e)
                       :metric-deltas (:metric-deltas e)
                       :reason (:reason e)
                       :negative-evidence (:negative-evidence e)})
                    rows)}))

(defn- user-message
  "Build the user-message string handed to the model: a bounded EDN
  rendering of the diagnosis, the parent genome (mutable classes + file
  table + small-file content), the recent history, and the budget
  profile. Truncation within each section is explicit."
  [context]
  (str "Propose structured mutations per the system instructions.\n"
       "Diagnosis:\n"
       (pr-str (render-diagnosis (:diagnosis context))) "\n\n"
       "Parent genome:\n"
       (pr-str (render-genome (:parent-genome context))) "\n\n"
       "Recent mutation history:\n"
       (pr-str (render-history (:history context))) "\n\n"
       "Budget profile:\n"
       (pr-str (:budget-profile context))))


;; --- robust JSON / mutation extraction ----------------------------------------

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
  "Return a value as a vector when it is sequential, else nil."
  [x]
  (when (sequential? x) (vec x)))

(defn- coerce-uuid
  "Coerce a value to a java.util.UUID, or nil when it cannot be parsed
  as one."
  [x]
  (when x
    (try (java.util.UUID/fromString (str x))
         (catch Exception _ nil))))

(defn- parse-model-json
  "Parse the model's output text into a keywordized object map.

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

;; --- normalization + the kernel-computed :expect/hash ---------------------------

(defn- coerce-op-keywords
  "Coerce the keyword-valued payload fields of an op from whatever the
  model emitted (normally JSON strings) into the schema's keyword
  vocabulary, so a model's naturally string-typed op survives schema
  validation. Only the fields the op schemas mark as keyword? are
  touched — :position (insert ops), :node/:node-id (topology node ops),
  :from/:to (edge ops), :update/keys (update-node). A value that cannot
  be coerced is LEFT unchanged so the orchestrator's schema gate
  rejects that op with :mutation/op-invalid (observability — a
  :evolution/candidate-invalid event) rather than a silent drop here."
  [op]
  (let [opk (:op op)]
    (if (contains? #{:insert-text :insert-form} opk)
      (update op :position (fn [p] (or (to-keyword p) p)))
      (cond
        (= :add-node opk)
        (update-in op [:node :node/id] (fn [x] (or (to-keyword x) x)))

        (contains? #{:remove-node :update-node} opk)
        (-> op
            (update :node/id (fn [x] (or (to-keyword x) x)))
            (update :update/keys
                    (fn [xs] (if (sequential? xs)
                               (mapv (fn [x] (or (to-keyword x) x)) xs)
                               xs))))

        (contains? #{:add-edge :remove-edge} opk)
        (-> op
            (update-in [:edge :from] (fn [x] (or (to-keyword x) x)))
            (update-in [:edge :to] (fn [x] (or (to-keyword x) x))))

        :else op))))

(defn- sanitize-edn
  "Recursively repair the EDN VALUES a model emits in its JSON: models
  routinely write keywords with leading colons in the JSON string
  (\"::type\", \":steps\"), which cheshire keywordizes into keywords
  whose NAME starts with a colon (e.g. ::steps). pr-str of such a
  keyword yields an ILLEGAL EDN token (::steps is an alias reference
  and fails to read without a namespace alias), so a set-edn value
  carrying one would compile-fail the candidate. This walks the value
  and rewrites every such keyword to the keyword WITHOUT its leading
  colons (:steps -> :steps), keeping maps/vectors/sets intact. A
  keyword whose name has no leading colon passes through unchanged."
  [x]
  (cond
    (keyword? x)
    (let [n (name x)]
      (if (str/starts-with? n ":")
        (keyword (str/replace-first n ":" ""))
        x))
    (map? x) (into {} (map (fn [[k v]] [(sanitize-edn k) (sanitize-edn v)])) x)
    (vector? x) (mapv sanitize-edn x)
    (seq? x) (map sanitize-edn x)
    (set? x) (into #{} (map sanitize-edn) x)
    :else x))

(defn- complete-op
  "Attach the kernel-computed :expect/hash to ONE op from the parent's
  :files map (the op's :file digested by the SAME convention the patch
  runtime verifies against). :op is coerced to a keyword (the model
  emits it as a JSON string; the op schemas dispatch on a keyword) and
  the keyword-valued payload fields are coerced alongside, so the
  completed op is schema-valid. EDN payload values (:value / :form)
  are sanitized (see sanitize-edn) so model-typical \"::type\"-style
  keywords never compile-fail the candidate. Returns the completed op,
  or nil when it cannot be hash-completed: a non-map op, an
  un-coercible :op, a missing/non-string :file, or a :file NOT in the
  parent's :files map (the model hallucinated a file). The
  kernel-computed digest OVERRIDES any model-supplied :expect/hash — a
  model can never name a preimage it does not know, so stale patches
  are impossible."
  [files op]
  (when (and (map? op) (to-keyword (:op op)))
    (let [opk (to-keyword (:op op))
          file (:file op)]
      (when (and (string? file) (seq file))
        (when-let [digest (get-in files [file :digest])]
          (assoc (sanitize-edn
                  (coerce-op-keywords (assoc op :op opk)))
                 :expect/hash digest))))))

(defn- normalize-expected-effect
  "Coerce a raw :expected-effect map into the schema vocabulary
  ({:primary-metric keyword? :direction :increase|:decrease}), or nil
  when the value is not a usable map (treated as absent — honestly not
  asserted rather than guessed at)."
  [x]
  (when (map? x)
    (let [metric (to-keyword (:primary-metric x))
          direction (to-keyword (:direction x))]
      (when (and metric (contains? #{:increase :decrease} direction))
        {:primary-metric metric :direction direction}))))

(defn- normalize-candidate
  "Normalize ONE raw model mutation into an adapter-returned Mutation
  IR, or nil when it cannot be made schema-valid:

  - keeps :risk (a keyword; defaults to config :risk when the model
    omits it), a non-empty :ops vector, and the optional
    :expected-effect / :hypothesis/id;
  - completes :expect/hash on EVERY op from the parent's :files (a
    model can never name a preimage it does not know).

  Returns nil for a structurally broken candidate (a non-map element, a
  non-sequential or empty :ops vector) or when any op's :file is
  unknown (cannot be hash-completed — the whole mutation is dropped).
  Only the keys the adapter owns are returned, so model junk is not
  carried into the Mutation IR."
  [config files raw]
  (when (map? raw)
    (let [raw-ops (as-vector (:ops raw))]
      (when raw-ops
        (let [ops (mapv (partial complete-op files) raw-ops)]
          (when (and (seq ops) (every? some? ops))
            (let [candidate {:risk (or (to-keyword (:risk raw)) (:risk config))
                             :ops ops}
                  ;; the model emits hyphens (the flat JSON keys the system
                  ;; prompt names); the schema uses slashes. Accept both.
                  ee (normalize-expected-effect (some raw [:expected-effect
                                                           :expected/effect]))
                  hy (coerce-uuid (some raw [:hypothesis/id :hypothesis-id]))]
              (cond-> candidate
                ee (assoc :expected-effect ee)
                hy (assoc :hypothesis/id hy)))))))))

(defn- output-text
  "Extract the :text string from the provider result :value (a map
  {:model/output {:text ... :reasoning ...} :usage ...}), or nil when
  the shape is not as the model-call contract promises (a typed
  :mutation/llm-response-invalid failure follows)."
  [value]
  (when (map? value)
    (let [mo (:model/output value)]
      (when (map? mo)
        (let [t (:text mo)]
          (when (string? t) t))))))


(defrecord LlmMutator [config]
  Mutator
  (propose-mutations [_ context]
    ;; 1. validate the closed context at the trust boundary
    (validate-context! context)
    (let [model-call (:model-call config)
          model-id (:model/id config)
          max-mutations (:max-mutations config)
          system-prompt (:system-prompt config)
          ;; 2. bounded user message
          messages [{:role :system :content (or system-prompt default-system-prompt)}
                    {:role :user :content (user-message context)}]]
      ;; 3. exactly ONE model call through the injected :model-call closure
      (let [result (try
                     (model-call model-id messages
                                 {:temperature 0.2 :max-tokens 8192})
                     (catch clojure.lang.ExceptionInfo e
                       (throw (err/error :mutation/llm-failed
                                         "model call failed during mutation proposal"
                                         {:error/type (:error/type (ex-data e))
                                          :cause (err/error-data e)})))
                     (catch Throwable t
                       (throw (err/error :mutation/llm-failed
                                         "model call failed during mutation proposal"
                                         {:error/type :mutation/llm-failed
                                          :cause (err/error-data t)}))))
            ;; the host fn contract: a dispatch result with :result/status
            ;; :ok, or a thrown typed error. A non-ok status is a
            ;; defensive guard, not a substitute for the contract.
            _ (when (and (map? result)
                         (contains? result :result/status)
                         (not= :ok (:result/status result)))
                (throw (err/error :mutation/llm-failed
                                  "model-call returned a non-ok dispatch result"
                                  {:result/status (:result/status result)
                                   :cause (dissoc result :value)})))
            value (:value result)
            text (output-text value)]
        (when-not (string? text)
          (throw (err/error :mutation/llm-response-invalid
                            "model-call did not produce usable text output"
                            {:reason :non-string-output
                             :value-type (some-> value class .getName)})))
        ;; 4. parse as JSON (robust extractor)
        (let [parsed (parse-model-json text)]
          (when-not parsed
            (throw (err/error :mutation/llm-response-invalid
                              "model output was not parseable as JSON"
                              {:reason :not-json
                               :excerpt (subs text 0 (min 200 (count text)))})))
          (let [raw-mutations (as-vector (:mutations parsed))]
            (when-not raw-mutations
              (throw (err/error :mutation/llm-response-invalid
                                "model output has no usable mutations array"
                                {:reason :missing-mutations
                                 :keys (vec (keys parsed))})))
            ;; 5. normalize + hash-complete each mutation; drop noise and
            ;;    hallucinated-file mutations
            (let [parent (:parent-genome context)
                  files (:files parent)
                  validated (->> raw-mutations
                                 (keep (partial normalize-candidate
                                                config files))
                                 (vec))]
              ;; all-noise (non-empty array, everything dropped) is a
              ;; failure (fail-loud)
              (when (and (seq raw-mutations) (empty? validated))
                (throw (err/error :mutation/llm-response-invalid
                                  "model output contained mutations but none were usable"
                                  {:reason :all-mutations-dropped
                                   :count (count raw-mutations)})))
              ;; 6. bound and return (nil when nothing survived)
              (let [mutations (into [] (take max-mutations) validated)]
                (when (seq mutations) mutations)))))))))

(defn llm-mutator
  "Construct the LLM-driven Mutator adapter from a config map.

  The config is a CLOSED map (any unknown key — a store handle, a
  provider object — is rejected with :mutation/config-invalid; Global
  Constraint 11). Required keys:

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

    :max-mutations — pos-int, default 3: the hard cap on returned
                     mutations (the orchestrator also caps at
                     v0-max-candidates 3; the adapter never exceeds it).
    :risk          — a RiskClass keyword, default :behavioral (R1);
                     stamped onto every returned mutation when the model
                     omits :risk.
    :system-prompt — a string overriding the built-in STRICT-JSON
                     instruction prompt.

  The adapter holds no store handle and sees only the context it is
  handed (Global Constraint 11). The KERNEL-COMPUTES-HASH rule is the
  security property: the model proposes ops without :expect/hash and
  the adapter attaches the parent file's digest to every op — a model
  can never name a preimage it does not know. propose-mutations never
  fails silently: per-mutation noise is tolerated, but a model that
  produces nothing usable throws a typed error (LLM-NOISE TOLERANCE
  POLICY)."
  [config]
  (let [v (validate-llm-config config)]
    (->LlmMutator {:model-call (:model-call v)
                   :model/id (:model/id v)
                   :max-mutations (or (:max-mutations v) 3)
                   :risk (or (:risk v) :behavioral)
                   :system-prompt (:system-prompt v)})))

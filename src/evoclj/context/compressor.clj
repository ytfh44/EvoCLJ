(ns evoclj.context.compressor
  "Builds the compression prompt and wraps the model call.

   The compressor is pure except for the model-call step. It supports two
   modes:

   1. **Structured compression** (`compress-structured`) — the preferred
      path for `DefaultCompacter`. The caller supplies a structured
      summary (authoritative task/subgoal state + retained residue from
      the previous envelope) AND the raw context text. The prompt
      explicitly tells the model:

      - task/subgoal fields are AUTHORITATIVE and MUST NOT be overridden
      - the ONLY thing the model should produce is `:residue` and
        `:evidence`
      - raw context is provided for extraction only, not for restating
        structured state

      This eliminates the LLM's ability to hallucinate task/subgoal
      state, because those fields never come from the model.

   2. **Legacy compression** (`compress`) — retained for backward
      compatibility. Builds a prompt from a summary map and asks the
      model to fill all four sections. New callers should prefer
      `compress-structured`."
  (:require [evoclj.context.error :as err]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.residue :as residue]
            [evoclj.context.provenance :as prov]
            [evoclj.context.idempotency :as idem]))

;; ---------------------------------------------------------------------------
;; Legacy prompt template (kept for backward compatibility)
;; ---------------------------------------------------------------------------

(def ^:private legacy-prompt-template
  "You are a context compression engine for an autonomous agent runtime.

   Your job: produce a SHORT structured summary of the conversation that
   preserves everything load-bearing for future decisions.

   ─── MANDATORY PRESERVE (do NOT paraphrase away) ───
   1. RESIDUE constraints, decisions, and commitments (the non-structured
      memory that no tool can re-derive).
   2. Provenance of every claim: who said it, when, and in what context.
   3. Evidence: any concrete data points, file paths, measurements, or
      references the agent will need to cite later.

   ─── OPTIONAL DISCARD ───
   Verbatim back-and-forth text, intermediate reasoning, and anything
   that can be reconstructed from the structured fields above.

   ─── OUTPUT FORMAT (MUST match exactly) ───
   Return a Clojure map with these keys:
     :task      — current task map with :task/id, :task/status, :task/description
     :subgoals  — vector of subgoal maps with :subgoal/id, :subgoal/status,
                  :subgoal/description, :subgoal/parent
     :residue   — vector of residue entries (each with :residue/id,
                  :residue/kind, :residue/text, :residue/source, :residue/at)
     :evidence  — vector of evidence entries (each with :evidence/id,
                  :evidence/kind, :evidence/text)

   ─── CURRENT STATE ───
   %s

   ─── COMPRESS NOW ───
   Output ONLY the Clojure map. No explanation, no preamble.")

;; ---------------------------------------------------------------------------
;; Structured prompt template (preferred)
;; ---------------------------------------------------------------------------

(def ^:private structured-prompt-template
  "You are a context compression engine for an autonomous agent runtime.

   Your job: extract ONLY the non-reconstructible information from the
   raw conversation text below. Structured state is provided separately
   and MUST NOT be duplicated or overridden.

   ─── AUTHORITATIVE STRUCTURED STATE (DO NOT OVERRIDE) ───
   %s

   ─── RAW CONVERSATION TEXT ───
   %s

   ─── MANDATORY EXTRACT ───
   From the RAW CONVERSATION TEXT only, produce:
   1. :residue — non-structured memory that no tool can re-derive:
      constraints, decisions, commitments, compatibility hacks, open
      problems, and anything else the next agent turn MUST know but
      cannot query from a database.
   2. :evidence — concrete facts the agent will need to cite later:
      file paths, measurements, references, hashes, command outputs.

   ─── STRICT RULES ───
   - DO NOT include :task or :subgoals in your output; they are handled
     authoritatively by the caller.
   - DO NOT invent claims that are not grounded in the RAW text.
   - DO NOT paraphrase away constraints or commitments; preserve them
     verbatim when possible.
   - Provenance is implicit in the raw text ordering; you do not need to
     add :residue/source or :residue/at unless the text clearly states
     who said it.

   ─── OUTPUT FORMAT (MUST match exactly) ───
   Return a Clojure map with ONLY these keys:
     :residue   — vector of residue entries
     :evidence  — vector of evidence entries

   ─── COMPRESS NOW ───
   Output ONLY the Clojure map. No explanation, no preamble.")

;; ---------------------------------------------------------------------------
;; Prompt building
;; ---------------------------------------------------------------------------

(defn build-prompt
  "Build the legacy compression prompt from a context summary map.

   `summary` should contain at least :task, :subgoals, :residue, :evidence.
   Returns the full prompt string with the summary embedded."
  [summary]
  (when-not (map? summary)
    (throw (err/error :context/compression-invalid
                      "summary must be a map"
                      {:value (err/sanitize summary)})))
  (let [summary-str (pr-str summary)]
    (format legacy-prompt-template summary-str)))

(defn build-structured-prompt
  "Build the structured compression prompt from a structured summary and
   the raw context text.

   `structured-summary` should contain :task, :subgoals, and optionally
   :residue/:evidence from the previous envelope.
   `raw-context` is the verbatim conversation/event log text to compress.

   Returns the full prompt string with both inputs embedded."
  [structured-summary raw-context]
  (when-not (map? structured-summary)
    (throw (err/error :context/compression-invalid
                      "structured-summary must be a map"
                      {:value (err/sanitize structured-summary)})))
  (when-not (string? raw-context)
    (throw (err/error :context/compression-invalid
                      "raw-context must be a string"
                      {:value (err/sanitize raw-context)})))
  (let [structured-str (pr-str structured-summary)
        raw-str raw-context]
    (format structured-prompt-template structured-str raw-str)))

;; ---------------------------------------------------------------------------
;; Model call wrappers
;; ---------------------------------------------------------------------------

(defn compress
  "Compress a context summary by sending it to the model via `model-call`.

   This is the legacy entry point. It builds a prompt from `summary` and
   asks the model to fill all four sections (:task, :subgoals, :residue,
   :evidence).

   `model-call` is a function of one argument (the prompt string) that
   returns the raw model response string.

   Returns a full envelope map (validated against EnvelopeSchema) with
   compression metadata (:envelope/version, :envelope/created-at,
   :envelope/compressor, :envelope/tokens-before, :envelope/tokens-after).

   Throws :context/compression-invalid if the model response cannot be
   parsed into a valid envelope."
  [summary model-call & {:keys [model prompt-version tokens-before]
                         :or {model "unknown" prompt-version 1}}]
  (let [prompt (build-prompt summary)
        raw-response (model-call prompt)
        parsed (try
                 (read-string raw-response)
                 (catch Exception e
                   (throw (err/error :context/compression-invalid
                                     (str "model response is not valid EDN: "
                                          (.getMessage e))
                                     {:raw (err/sanitize raw-response)
                                      :cause (err/sanitize (.getMessage e))}))))
        now (str (java.time.Instant/now))
        tokens-after (int (/ (count raw-response) 4))
        envelope-base (envelope/make-envelope
                       {:task (:task parsed)
                        :subgoals (:subgoals parsed [])
                        :residue (:residue parsed [])
                        :evidence (:evidence parsed [])
                        :version 1
                        :created-at now
                        :window {:window/from 0 :window/to 100}
                        :tokens-before (int (or tokens-before 0))
                        :tokens-after tokens-after
                        :compressor {:compressor/model model
                                     :compressor/prompt prompt}})]
    (envelope/validate-envelope envelope-base)
    envelope-base))

(defn compress-structured
  "Compress a context using the structured path: the model sees both the
   authoritative structured summary and the raw context, and is asked to
   produce ONLY :residue and :evidence.

   This is the preferred entry point for `DefaultCompacter` because it
   prevents the model from hallucinating task/subgoal state.

   `structured-summary` — map with at least :task and :subgoals, plus
   optional :residue/:evidence from the previous envelope.
   `raw-context` — the verbatim conversation/event log text.
   `model-call` — function of one argument (prompt string) returning the
   raw model response string.

   Returns a partial envelope map containing ONLY :residue and :evidence
   (no compression metadata). The caller is responsible for merging this
   with the authoritative task/subgoal state and adding metadata.

   Throws :context/compression-invalid if the model response cannot be
   parsed or does not contain :residue and :evidence."
  [structured-summary raw-context model-call & {:keys [model prompt-version]
                                                 :or {model "unknown" prompt-version 1}}]
  (let [prompt (build-structured-prompt structured-summary raw-context)
        raw-response (model-call prompt)
        parsed (try
                 (read-string raw-response)
                 (catch Exception e
                   (throw (err/error :context/compression-invalid
                                     (str "model response is not valid EDN: "
                                          (.getMessage e))
                                     {:raw (err/sanitize raw-response)
                                      :cause (err/sanitize (.getMessage e))}))))]
    ;; Validate the parsed map has the expected keys
    (when-not (map? parsed)
      (throw (err/error :context/compression-invalid
                        "model response is not a map"
                        {:raw (err/sanitize raw-response)})))
    ;; The model MUST return :residue and :evidence; other keys are ignored
    {:residue (vec (or (:residue parsed) []))
     :evidence (vec (or (:evidence parsed) []))
     :raw-response raw-response
     :prompt prompt}))

;; ---------------------------------------------------------------------------
;; Mock caller (for testing)
;; ---------------------------------------------------------------------------

(defn mock-call
  "A mock model-call function for testing. Returns a canned response
   that matches the expected output format. `summary-fn` is called with
   the prompt and should return the canned response string."
  [summary-fn]
  (fn [prompt]
    (summary-fn prompt)))

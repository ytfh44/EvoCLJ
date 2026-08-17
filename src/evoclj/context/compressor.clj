(ns evoclj.context.compressor
  "Builds the compression prompt and wraps the model call.

  The compressor is pure except for the model-call step: given a
  structured context summary (envelope without compression metadata)
  and a prompt template, it produces the text the LLM sees. The caller
  supplies a `model-call` function; in tests this is a mock that returns
  a canned response. The returned raw text is then wrapped into a full
  envelope by the caller (or by `compress` below).

  The prompt follows the save-priority principle: it explicitly tells
  the model what MUST be preserved (residue, provenance, evidence) and
  what MAY be discarded (verbatim historical text)."
  (:require [evoclj.context.error :as err]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.residue :as residue]
            [evoclj.context.provenance :as prov]
            [evoclj.context.idempotency :as idem]))

;; ---------------------------------------------------------------------------
;; Prompt template
;; ---------------------------------------------------------------------------

(def ^:private prompt-template
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
;; Prompt building
;; ---------------------------------------------------------------------------

(defn build-prompt
  "Build the compression prompt from a context summary map.

  `summary` should contain at least :task, :subgoals, :residue, :evidence.
  Returns the full prompt string with the summary embedded."
  [summary]
  (when-not (map? summary)
    (throw (err/error :context/compression-invalid
                      "summary must be a map"
                      {:value (err/sanitize summary)})))
  (let [summary-str (pr-str summary)]
    (format prompt-template summary-str)))

;; ---------------------------------------------------------------------------
;; Model call wrapper
;; ---------------------------------------------------------------------------

(defn compress
  "Compress a context summary by sending it to the model via `model-call`.

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
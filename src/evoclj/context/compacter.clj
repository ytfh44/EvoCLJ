(ns evoclj.context.compacter
  "Compacter protocol and default implementation for context compression.

   A compacter is the object that orchestrates a compression run: it
   decides whether to compress (via trigger), collects authoritative
   structured state from registered archivers, builds the prompt, calls
   the model, crosschecks the result, evaluates it, builds the envelope,
   and produces the footer text.

   The separation between `Compacter` (protocol) and
   `DefaultCompacter` (record) lets users plug in their own compacter
   without touching the core context code.

   Public API:
   - `Compacter` protocol — implement this for custom compactors
   - `->DefaultCompacter` — built-in compacter record
   - `run` — execute a compacter against a context string"
  (:require [evoclj.context.trigger :as trigger]
            [evoclj.context.token-estimator :as token-estimator]
            [evoclj.context.compressor :as compressor]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.footer :as footer]
            [evoclj.context.registry :as registry]
            [evoclj.context.crosscheck :as crosscheck]
            [evoclj.context.eval :as eval]
            [evoclj.context.residue :as residue]))

;; ---------------------------------------------------------------------------
;; Protocol
;; ---------------------------------------------------------------------------

(defprotocol Compacter
  "Implemented by any compacter that can compress a context string.

   The returned map must contain:

     {:envelope <Envelope map>
      :footer   <string>}

   The `envelope` is the structured compression result. The `footer`
   is the instruction text appended to the context for the next agent
   turn.

   Advanced implementations may also return:
     :eval       <eval summary map>
     :mismatches <crosscheck mismatch vector>
     :trigger    <trigger result map>"
  (compress [this context opts]
    "Compress `context` using this compacter.

     `opts` is a map that may contain:
       :model            — model name string (default \"unknown\")
       :token-threshold  — max tokens before compression (default 4000)
       :marker           — explicit compression marker string (default nil)
       :cooldown-tokens  — min tokens saved to reset cooldown (default 500)
       :eval?            — run eval after compression (default false)
       :footer-opts      — map passed to footer generation (default nil)
       :previous-envelope — optional previous envelope map; when present,
                            its :residue and :evidence are retained and
                            merged with the new compression result.
       :structured-sections — optional authoritative structured state
                             map {:tasks [...] :subgoals [...]}; when
                             present, crosscheck is performed and
                             auto-correctable fields are fixed.
       :token-estimator  — optional TokenEstimator instance (default char-count)
       :trigger/last-savings — optional last compression savings in tokens

     Returns a map:
       {:envelope <Envelope map>
        :footer   <string>
        :eval     <eval summary map, when :eval? true>
        :mismatches <crosscheck mismatch vector>
        :trigger  <trigger result map>}"))

;; ---------------------------------------------------------------------------
;; Private helpers
;; ---------------------------------------------------------------------------

(defn- collect-structured-state
  "Collect authoritative structured state from optional caller-supplied
   sections and the previous envelope.

   Returns a map suitable for use as the structured summary in
   `compressor/compress-structured`:
     {:task      <first task from :tasks or previous envelope task>
      :subgoals  <from :subgoals or previous envelope subgoals>
      :residue   <from previous envelope or []>
      :evidence  <from previous envelope or []>}"
  [previous-envelope structured-sections]
  (let [tasks (:tasks structured-sections [])
        subgoals (:subgoals structured-sections [])
        current-task (first tasks)]
    {:task (or current-task
               (:envelope/task previous-envelope))
     :subgoals (or (seq subgoals)
                   (:envelope/subgoals previous-envelope []))
     :residue (or (:envelope/residue previous-envelope) [])
     :evidence (or (:envelope/evidence previous-envelope) [])}))

;; ---------------------------------------------------------------------------
;; Default compacter record
;; ---------------------------------------------------------------------------

(defrecord DefaultCompacter [model-call]
  Compacter
  (compress [this context opts]
    {:pre [(string? context)]}
    (let [opts (or opts {})
          model      (or (:model opts) "unknown")
          threshold  (or (:token-threshold opts) 4000)
          marker     (:marker opts)
          cooldown   (or (:cooldown-tokens opts) 500)
          eval?      (or (:eval? opts) false)
          footer-opts (:footer-opts opts)
          previous-envelope (:previous-envelope opts)
          structured-sections (:structured-sections opts)
          estimator (or (:token-estimator opts) token-estimator/default-char-count-estimator)
          last-savings (:trigger/last-savings opts)
          trigger-config {:trigger/token-threshold threshold
                          :trigger/marker marker
                          :trigger/cooldown-tokens cooldown
                          :trigger/token-estimator estimator
                          :trigger/last-savings last-savings}
          trigger-result (trigger/should-compress? context trigger-config)]
      (if-not (:trigger/compressed? trigger-result)
        ;; No compression needed — return the original context with an empty envelope
        (let [state (collect-structured-state previous-envelope structured-sections)
              token-count (:trigger/token-count trigger-result)]
          {:envelope (envelope/make-envelope
                       {:task (or (:task state)
                                  {:task/id "noop" :task/status :pending
                                   :task/description "No compression needed"})
                        :subgoals (or (:subgoals state) [])
                        :residue (or (:residue state) [])
                        :evidence (or (:evidence state) [])
                        :version 1
                        :created-at (str (java.time.Instant/now))
                        :window {:window/from 0 :window/to token-count}
                        :tokens-before token-count
                        :tokens-after  token-count
                        :compressor {:compressor/model model
                                     :compressor/prompt "none"}})
           :footer ""
           :mismatches []
           :eval nil
           :trigger trigger-result})
        ;; Proceed with structured compression
        (let [structured-summary (collect-structured-state previous-envelope structured-sections)
              ;; Run the structured compression path
              llm-result (compressor/compress-structured
                          structured-summary
                          context
                          model-call
                          :model model
                          :tokens-before (:trigger/token-count trigger-result))
              ;; Merge authoritative task/subgoals with LLM-produced residue/evidence.
              ;; Previous residue is deduplicated via residue-merge so that repeated
              ;; compressions do not accumulate duplicates.
              previous-residue (:residue structured-summary)
              previous-evidence (:evidence structured-summary)
              merged-residue (residue/residue-merge previous-residue (:residue llm-result))
              merged-evidence (vec (concat previous-evidence (:evidence llm-result)))
              ;; Merge authoritative task/subgoals with LLM-produced residue/evidence
              task (or (:task structured-summary)
                       {:task/id "compression-run" :task/status :in-progress
                        :task/description "Context compression"})
              subgoals (or (:subgoals structured-summary) [])
              now (str (java.time.Instant/now))
              tokens-after (token-estimator/estimate-tokens estimator (:raw-response llm-result))
              envelope-base (envelope/make-envelope
                             {:task task
                              :subgoals subgoals
                              :residue merged-residue
                              :evidence merged-evidence
                              :version 1
                              :created-at now
                              :window {:window/from 0 :window/to (:trigger/token-count trigger-result)}
                              :tokens-before (:trigger/token-count trigger-result)
                              :tokens-after tokens-after
                              :compressor {:compressor/model model
                                           :compressor/prompt (:prompt llm-result)}})
              ;; Crosscheck against authoritative structured state (if provided)
              crosscheck-result (when structured-sections
                                  (crosscheck/crosscheck* envelope-base structured-sections))
              corrected-envelope (if crosscheck-result
                                   (:crosscheck/envelope crosscheck-result)
                                   envelope-base)
              mismatches (if crosscheck-result
                           (:crosscheck/mismatches crosscheck-result)
                           [])
              ;; Eval if requested
              eval-result (when eval?
                            (eval/eval-summary
                              [(eval/eval-retention-score corrected-envelope context)
                               (eval/eval-regression-score corrected-envelope context)
                               (eval/eval-hallucination-score corrected-envelope context)]))
              ;; Build footer with archiver reports
              footer-opts' (assoc footer-opts :archiver-reports (registry/archiver-reports))
              f (footer/build-footer corrected-envelope footer-opts')]
          {:envelope corrected-envelope
           :footer f
           :eval eval-result
           :mismatches mismatches
           :trigger trigger-result})))))

;; ---------------------------------------------------------------------------
;; Public run helper
;; ---------------------------------------------------------------------------

(defn run
  "Compress `context` using `compacter` (any Compacter record).

   Returns `{:envelope <map> :footer <string>}` plus optional `:eval`
   and `:mismatches` keys when the compacter produces them.

   For the default compacter:
     (run context (->DefaultCompacter mock-call) {:model \"gpt-4\"})"
  [context compacter opts]
  {:pre [(string? context)
         (satisfies? Compacter compacter)]}
  (compress compacter context opts))

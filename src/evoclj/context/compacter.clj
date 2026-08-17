(ns evoclj.context.compacter
  "Compacter protocol and default implementation for context compression.

   A compacter is the object that orchestrates a compression run: it
   decides whether to compress (via trigger), builds the prompt, calls
   the model, builds the envelope, and produces the footer text.

   The separation between `Compacter` (protocol) and
   `DefaultCompacter` (record) lets users plug in their own compacter
   without touching the core context code.

   Public API:
   - `Compacter` protocol — implement this for custom compactors
   - `->DefaultCompacter` — built-in compacter record
   - `run` — execute a compacter against a context string"
  (:require [evoclj.context.trigger :as trigger]
            [evoclj.context.compressor :as compressor]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.footer :as footer]))

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
   turn."
  (compress [this context opts]
    "Compress `context` using this compacter.

     `opts` is a map that may contain:
       :model            — model name string (default \"unknown\")
       :token-threshold  — max tokens before compression (default 4000)
       :marker           — explicit compression marker string (default nil)
       :cooldown-tokens  — min tokens saved to reset cooldown (default 500)
       :eval?            — run eval after compression (default false)
       :footer-opts      — map passed to footer generation (default nil)

     Returns a map:
       {:envelope <Envelope map>
        :footer   <string>}"))

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
          trigger-config {:trigger/token-threshold threshold
                          :trigger/marker marker
                          :trigger/cooldown-tokens cooldown}
          trigger-result (trigger/should-compress? context trigger-config)]
      (if-not (:trigger/compressed? trigger-result)
        ;; No compression needed — return the original context with an empty envelope
        {:envelope (envelope/make-envelope
                     {:task {:task/id "noop" :task/status :pending
                             :task/description "No compression needed"}
                      :subgoals []
                      :residue []
                      :evidence []
                      :version 1
                      :created-at (str (java.time.Instant/now))
                      :window {:window/from 0 :window/to 100}
                      :tokens-before (:trigger/token-count trigger-result)
                      :tokens-after  (:trigger/token-count trigger-result)
                      :compressor {:compressor/model model
                                   :compressor/prompt "none"}})
         :footer ""}
        ;; Proceed with compression
        (let [summary {:task {:task/id "compression-run"
                              :task/status :in-progress
                              :task/description "Context compression"}
                       :subgoals []
                       :residue []
                       :evidence []}
              env (compressor/compress
                    summary
                    model-call
                    :model model
                    :tokens-before (:trigger/token-count trigger-result))
              f (footer/build-footer env footer-opts)]
          {:envelope env
           :footer f})))))

;; ---------------------------------------------------------------------------
;; Public run helper
;; ---------------------------------------------------------------------------

(defn run
  "Compress `context` using `compacter` (any Compacter record).

   Returns `{:envelope <map> :footer <string>}`.

   For the default compacter:
     (run context (->DefaultCompacter mock-call) {:model \"gpt-4\"})"
  [context compacter opts]
  {:pre [(string? context)
         (satisfies? Compacter compacter)]}
  (compress compacter context opts))

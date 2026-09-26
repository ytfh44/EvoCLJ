(ns evoclj.context.compression.cli
  "CLI commands for the context compression subsystem.

  FAILURE CONTRACT. Every failure leaves through *exit*, never through a
  raw throw, and the exit carries its CAUSE:

    *exit* is called with FOUR arguments — (code error-type message data)
    — and throws exactly one ExceptionInfo (message
    \"context-cli-exit\") whose ex-data holds :exit, :error/type,
    :message and the cause `data`. Callers may REBIND *exit* (that is how
    the tests observe it); the shared handlers below detect our own exit by
    the :exit key in ex-data rather than by identity, so a rebound *exit*
    still round-trips correctly.

  This is what the old one-argument *exit* could not do. It was called
  from INSIDE the try blocks, so each catch Exception swallowed it,
  printed a second, false error line, and re-exited with the reason lost.
  An I/O failure and a usage error both surfaced as :exit 1 with no cause.
  reraise-or-exit! now re-throws our own exit untouched and converts every
  other throwable into a typed exit that keeps the subsystem's own
  :error/type."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [evoclj.context.compression.compacter :as compacter]
            [evoclj.context.compression.eval :as eval]
            [evoclj.context.compression.loop :as loop]))

(def ^:dynamic *exit*
  "The exit function used by the context CLI. Takes
  (code error-type message data) and throws exactly one ExceptionInfo so
  the main CLI can catch it and return a typed error map carrying the
  original cause."
  (fn [code error-type message data]
    (throw (ex-info "context-cli-exit"
                    (assoc data
                           :exit code
                           :error/type error-type
                           :message message)))))

(def ^:private exit-type
  "The :error/type used when a failure carries no better classification."
  :context/cli-failed)

(defn- parse-args [args opts]
  "Simple argument parser. Returns {:options <map> :errors <vector>}."
  (loop [i 0
         opts opts
         options {}
         errors []]
    (if (>= i (count opts))
      {:options options :errors errors}
      (let [opt (nth opts i)
            flag (:long opt)]
        (if (and flag (>= i (count args)))
          (if (:required opt)
            (recur (inc i) opts options (conj errors (str "Missing required option: " flag)))
            (recur (inc i) opts (assoc options flag true) errors))
          (let [next-arg (nth args (inc i) nil)
                has-default (some? (:default opt))
                has-value (and next-arg (not (str/starts-with? next-arg "-")))]
            (if has-default
              (recur (+ i 2) opts (assoc options flag (:default opt)) errors)
              (if has-value
                (recur (+ i 2) opts (assoc options flag next-arg) errors)
                (if (:required opt)
                  (recur (inc i) opts options (conj errors (str "Missing value for " flag)))
                  (recur (+ i 2) opts (assoc options flag true) errors))))))))))

(def compress-opts
  [{:long "input" :short "-i" :required true}
   {:long "output" :short "-o" :required true}
   {:long "threshold" :short "-t" :default 4000}
   {:long "marker" :short "-m"}
   {:long "model" :default "unknown"}
   {:long "eval"}])

(def loop-opts
  [{:long "input" :short "-i" :required true}
   {:long "output" :short "-o" :required true}
   {:long "iterations" :short "-n" :default 3}
   {:long "threshold" :short "-t" :default 4000}
   {:long "marker" :short "-m"}
   {:long "model" :default "unknown"}
   {:long "eval"}])

(def recompress-opts
  [{:long "input" :short "-i" :required true}
   {:long "output" :short "-o" :required true}
   {:long "threshold" :short "-t" :default 4000}
   {:long "marker" :short "-m"}
   {:long "model" :default "unknown"}
   {:long "eval"}])

(def inspect-opts
  [{:long "input" :short "-i" :required true}])

(defn- exit-exception?
  "True when `t` is one of OUR exit exceptions. Keyed on the :exit marker
  in ex-data rather than on identity, so a test that REBINDS *exit* to a
  plain thrower is still recognised."
  [t]
  (and (instance? clojure.lang.ExceptionInfo t)
       (contains? (ex-data t) :exit)))

(defn- fail!
  "Write EXACTLY ONE human-readable line to *err*, then exit. One line,
  because the caller is already reporting this failure once."
  [code error-type message data]
  (binding [*out* *err*]
    (println (str "Error: " message)
             (when error-type (str "[" error-type "]"))))
  (*exit* code error-type message data))

(defn- reraise-or-exit!
  "The shared failure handler for every command body. Our own exit
  exceptions pass through UNCHANGED so their :error/type and :message
  reach the main CLI intact; anything else becomes a typed exit that
  preserves the subsystem's :error/type and the original message.

  Catches Throwable, not Exception: context.compression.loop asserts its
  preconditions with `assert`, and an AssertionError is an Error, so an
  Exception catch here would let bad input escape unhandled."
  [e]
  (if (exit-exception? e)
    (throw e)
    (let [ed (ex-data e)
          error-type (or (:error/type ed) exit-type)
          message (or (:error/message ed) (ex-message e))]
      (fail! 1 error-type message {:error/class (.getName (class e))}))))

(defn- read-context [file-path]
  (when-not (str/blank? file-path)
    ;; A missing/unreadable input is an I/O failure with a real cause, not
    ;; a usage error: slurp would raise FileNotFoundException with no
    ;; ex-data at all, so the reason reached only stdout.
    (try
      (slurp (io/file file-path))
      (catch Throwable t
        (fail! 1 :context/input-unreadable
               (str "cannot read input file: " (.getMessage t))
               {:path file-path
                :error/class (.getName (class t))})))))

(defn- write-context [file-path content]
  (spit file-path content))

(defn print-envelope [env]
  (println "=== Envelope ===")
  (println (str "Version:     " (:envelope/version env)))
  (println (str "Created at:  " (:envelope/created-at env)))
  (println (str "Tokens before: " (:envelope/tokens-before env)))
  (println (str "Tokens after:  " (:envelope/tokens-after env)))
  (when-let [c (:envelope/compressor env)]
    (println (str "Compressor:  " (:compressor/model c))))
  (when-let [w (:envelope/window env)]
    (println (str "Window:      " (:window/from w) " to " (:window/to w))))
  (when-let [t (:envelope/task env)]
    (println "\n--- Task ---")
    (println (str "ID:          " (:task/id t)))
    (println (str "Status:      " (:task/status t)))
    (println (str "Description: " (:task/description t))))
  (when-let [sgs (:envelope/subgoals env)]
    (when (seq sgs)
      (println "\n--- Subgoals ---")
      (doseq [sg sgs]
        (println (str "  " (:subgoal/id sg) " [" (:subgoal/status sg) "] " (:subgoal/description sg))))))
  (when-let [rs (:envelope/residue env)]
    (when (seq rs)
      (println "\n--- Residue ---")
      (doseq [r rs]
        (println (str "  [" (:residue/kind r) "] " (:residue/text r))))))
  (when-let [es (:envelope/evidence env)]
    (when (seq es)
      (println "\n--- Evidence ---")
      (doseq [e es]
        (println (str "  [" (:evidence/kind e) "] " (:evidence/text e)))))))

(defn- make-compacter [model]
  (compacter/->DefaultCompacter
    (fn [_]
      (pr-str {:task {:task/id "cli-task" :task/status :in-progress
                      :task/description "CLI compression"}
               :subgoals []
               :residue []
               :evidence []}))))

(defn- run-eval-print [env context-str]
  (let [eval-records [(eval/eval-retention-score env context-str 0.9)
                      (eval/eval-regression-score env context-str 0.9)
                      (eval/eval-hallucination-score env context-str 0.9)]
        eval-summary (eval/eval-summary eval-records)]
    (println "\n=== Eval ===")
    (println (str "Overall: " (:eval/overall-status eval-summary)))
    (doseq [r (:eval/records eval-summary)]
      (println (str "  " (:eval/class r) ": " (:eval/score r) " [" (:eval/status r) "]")))))

(defn compress-command [args]
  (try
    (let [opts (parse-args args compress-opts)
          {:keys [options errors]} opts]
      (when (seq errors)
        (fail! 1 :cli/usage-invalid
               (str/join "; " errors)
               {:errors (vec errors)}))
      (let [input (get options "input")
            output (get options "output")
            threshold (get options "threshold" 4000)
            marker (get options "marker")
            model (get options "model" "unknown")
            run-eval? (get options "eval")
            context-str (read-context input)
            comp (make-compacter model)]
        (when (str/blank? context-str)
          (fail! 1 :context/input-empty "input file is empty or missing"
                 {:path input}))
        (let [result (loop/recompress! context-str comp
                                      {:model model
                                       :token-threshold threshold
                                       :marker marker})
              applied (:context result)
              env (:envelope result)
              footer (:footer result)]
          (write-context output applied)
          (println (str "Compressed context written to " output))
          (println (str "Tokens before: " (:envelope/tokens-before env)))
          (println (str "Tokens after:  " (:envelope/tokens-after env)))
          (when run-eval?
            (run-eval-print env context-str))
          0)))
    (catch Throwable e
      (reraise-or-exit! e))))

(defn recompress-command [args]
  (try
    (let [opts (parse-args args recompress-opts)
          {:keys [options errors]} opts]
      (when (seq errors)
        (fail! 1 :cli/usage-invalid
               (str/join "; " errors)
               {:errors (vec errors)}))
      (let [input (get options "input")
            output (get options "output")
            threshold (get options "threshold" 4000)
            marker (get options "marker")
            model (get options "model" "unknown")
            run-eval? (get options "eval")
            context-str (read-context input)
            comp (make-compacter model)]
        (when (str/blank? context-str)
          (fail! 1 :context/input-empty "input file is empty or missing"
                 {:path input}))
        (let [result (loop/recompress! context-str comp
                                      {:model model
                                       :token-threshold threshold
                                       :marker marker})
              applied (:context result)
              env (:envelope result)]
          (write-context output applied)
          (println (str "Recompressed context written to " output))
          (println (str "Tokens before: " (:envelope/tokens-before env)))
          (println (str "Tokens after:  " (:envelope/tokens-after env)))
          (when run-eval?
            (run-eval-print env context-str))
          0)))
    (catch Throwable e
      (reraise-or-exit! e))))

(defn loop-command [args]
  (try
    (let [opts (parse-args args loop-opts)
          {:keys [options errors]} opts]
      (when (seq errors)
        (fail! 1 :cli/usage-invalid
               (str/join "; " errors)
               {:errors (vec errors)}))
      (let [input (get options "input")
            output (get options "output")
            iterations (get options "iterations" 3)
            threshold (get options "threshold" 4000)
            marker (get options "marker")
            model (get options "model" "unknown")
            run-eval? (get options "eval")
            context-str (read-context input)
            comp (make-compacter model)]
        (when (str/blank? context-str)
          (fail! 1 :context/input-empty "input file is empty or missing"
                 {:path input}))
        (let [result (loop/compress-and-apply context-str comp
                                              {:model model
                                               :token-threshold threshold
                                               :marker marker})]
          (write-context output result)
          (println (str "Loop compression completed (" iterations " iterations)"))
          (println (str "Final context written to " output))
          (let [final-envelope (:envelope (loop/recompress! result comp
                                                      {:model model
                                                       :token-threshold threshold
                                                       :marker marker}))]
            (println (str "Tokens before: " (:envelope/tokens-before final-envelope)))
            (println (str "Tokens after:  " (:envelope/tokens-after final-envelope)))
            (when run-eval?
              (run-eval-print final-envelope context-str)))
          0)))
    (catch Throwable e
      (reraise-or-exit! e))))

(defn inspect-command [args]
  (try
    (let [opts (parse-args args inspect-opts)
          {:keys [options errors]} opts]
      (when (seq errors)
        (fail! 1 :cli/usage-invalid
               (str/join "; " errors)
               {:errors (vec errors)}))
      (let [input (get options "input")
            content (read-context input)]
        (when (str/blank? content)
          (fail! 1 :context/input-empty "input file is empty or missing"
                 {:path input}))
        (let [envelope-str (first (str/split content #"\n\n" 2))
              parsed (try (edn/read-string envelope-str) (catch Exception _ nil))]
          (if (and (map? parsed) (:envelope/version parsed))
            (do (print-envelope parsed) 0)
            (fail! 1 :context/envelope-invalid
                   "could not find a valid envelope in the input"
                   {:path input})))))
    (catch Throwable e
      (reraise-or-exit! e))))

(defn -main [& args]
  (if (seq args)
    (case (first args)
      "compress" (compress-command (rest args))
      "recompress" (recompress-command (rest args))
      "loop" (loop-command (rest args))
      "inspect" (inspect-command (rest args))
      (do (println "Usage: context compress|recompress|loop|inspect [options]") 1))
    (do (println "Usage: context compress|recompress|loop|inspect [options]") 1)))

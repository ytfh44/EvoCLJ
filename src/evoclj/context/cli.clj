(ns evoclj.context.cli
  "CLI commands for the context compression subsystem."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [evoclj.context.error :as err]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.compacter :as compacter]
            [evoclj.context.apply :as apply]
            [evoclj.context.eval :as eval]
            [evoclj.context.loop :as loop]))

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
  [{"long" "input", "short" "-i", "required" true}
   {"long" "output", "short" "-o", "required" true}
   {"long" "threshold", "short" "-t", "default" 4000}
   {"long" "marker", "short" "-m"}
   {"long" "model", "default" "unknown"}
   {"long" "eval"}])

(def loop-opts
  [{"long" "input", "short" "-i", "required" true}
   {"long" "output", "short" "-o", "required" true}
   {"long" "iterations", "short" "-n", "default" 3}
   {"long" "threshold", "short" "-t", "default" 4000}
   {"long" "marker", "short" "-m"}
   {"long" "model", "default" "unknown"}
   {"long" "eval"}])

(def recompress-opts
  [{"long" "input", "short" "-i", "required" true}
   {"long" "output", "short" "-o", "required" true}
   {"long" "threshold", "short" "-t", "default" 4000}
   {"long" "marker", "short" "-m"}
   {"long" "model", "default" "unknown"}
   {"long" "eval"}])

(def inspect-opts
  [{"long" "input", "short" "-i", "required" true}])

(defn- read-context [file-path]
  (when-not (str/blank? file-path)
    (slurp file-path)))

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
        (doseq [e errors] (println "Error:" e))
        (System/exit 1))
      (let [input (get options "input")
            output (get options "output")
            threshold (get options "threshold" 4000)
            marker (get options "marker")
            model (get options "model" "unknown")
            run-eval? (get options "eval")
            context-str (read-context input)
            comp (make-compacter model)]
        (when (str/blank? context-str)
          (println "Error: input file is empty or missing")
          (System/exit 1))
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
    (catch Exception e
      (let [ed (ex-data e)]
        (println "Error:" (or (:error/message ed) (.getMessage e))
                 (when-let [t (:error/type ed)] (str "[" t "]")))
        (System/exit 1)))))

(defn recompress-command [args]
  (try
    (let [opts (parse-args args recompress-opts)
          {:keys [options errors]} opts]
      (when (seq errors)
        (doseq [e errors] (println "Error:" e))
        (System/exit 1))
      (let [input (get options "input")
            output (get options "output")
            threshold (get options "threshold" 4000)
            marker (get options "marker")
            model (get options "model" "unknown")
            run-eval? (get options "eval")
            context-str (read-context input)
            comp (make-compacter model)]
        (when (str/blank? context-str)
          (println "Error: input file is empty or missing")
          (System/exit 1))
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
    (catch Exception e
      (let [ed (ex-data e)]
        (println "Error:" (or (:error/message ed) (.getMessage e))
                 (when-let [t (:error/type ed)] (str "[" t "]")))
        (System/exit 1)))))

(defn loop-command [args]
  (try
    (let [opts (parse-args args loop-opts)
          {:keys [options errors]} opts]
      (when (seq errors)
        (doseq [e errors] (println "Error:" e))
        (System/exit 1))
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
          (println "Error: input file is empty or missing")
          (System/exit 1))
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
    (catch Exception e
      (let [ed (ex-data e)]
        (println "Error:" (or (:error/message ed) (.getMessage e))
                 (when-let [t (:error/type ed)] (str "[" t "]")))
        (System/exit 1)))))

(defn inspect-command [args]
  (try
    (let [opts (parse-args args inspect-opts)
          {:keys [options errors]} opts]
      (when (seq errors)
        (doseq [e errors] (println "Error:" e))
        (System/exit 1))
      (let [input (get options "input")
            content (read-context input)]
        (when (str/blank? content)
          (println "Error: input file is empty or missing")
          (System/exit 1))
        (let [envelope-str (first (str/split content #"\n\n" 2))
              parsed (try (edn/read-string envelope-str) (catch Exception _ nil))]
          (if (and (map? parsed) (:envelope/version parsed))
            (do (print-envelope parsed) 0)
            (do (println "Could not find a valid envelope.") (System/exit 1))))))
    (catch Exception e
      (let [ed (ex-data e)]
        (println "Error:" (or (:error/message ed) (.getMessage e))
                 (when-let [t (:error/type ed)] (str "[" t "]")))
        (System/exit 1)))))

(defn -main [& args]
  (if (seq args)
    (case (first args)
      "compress" (compress-command (rest args))
      "recompress" (recompress-command (rest args))
      "loop" (loop-command (rest args))
      "inspect" (inspect-command (rest args))
      (do (println "Usage: context compress|recompress|loop|inspect [options]") 1))
    (do (println "Usage: context compress|recompress|loop|inspect [options]") 1)))

(ns evoclj.context.cli
  "CLI commands for the context compression subsystem."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [evoclj.context.error :as err]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.trigger :as trigger]
            [evoclj.context.compressor :as compressor]
            [evoclj.context.apply :as apply]
            [evoclj.context.eval :as eval]))

(def compress-opts
  [["-i" "--input FILE" "Context file to compress" :required true]
   ["-o" "--output FILE" "Output file" :required true]
   ["-t" "--threshold TOKENS" :parse-fn #(Integer/parseInt %) :default 4000]
   ["-m" "--marker STR" "Compression marker"]
   ["--model MODEL" :default "unknown"]
   ["-e" "--eval"]])

(def inspect-opts
  [["-i" "--input FILE" :required true]])

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

(defn compress-command [args]
  (try
    (let [opts (clojure.tools.cli/parse-opts args compress-opts)
          {:keys [options errors]} opts]
      (when (seq errors)
        (doseq [e errors] (println "Error:" e))
        (System/exit 1))
      (let [input (:input options)
            output (:output options)
            threshold (:threshold options)
            marker (:marker options)
            model (:model options)
            run-eval? (:eval options)
            context-str (read-context input)]
        (when (str/blank? context-str)
          (println "Error: input file is empty or missing")
          (System/exit 1))
        (let [trigger-result (trigger/should-compress? context-str
                                                        {:trigger/token-threshold threshold
                                                         :trigger/marker marker})]
          (if-not (:trigger/compressed? trigger-result)
            (do
              (println (str "No compression needed. Reason: "
                            (:trigger/reason trigger-result)
                            ", tokens: " (:trigger/token-count trigger-result)))
              (System/exit 0))
            (let [summary {:task {:task/id "cli-task" :task/status :in-progress
                                  :task/description "CLI compression"}
                           :subgoals []
                           :residue []
                           :evidence []}
                  mock-call (fn [_] (pr-str summary))
                  env (compressor/compress summary mock-call :model model
                                           :tokens-before (:trigger/token-count trigger-result))
                  applied (apply/apply-envelope env context-str)]
              (write-context output applied)
              (println (str "Compressed context written to " output))
              (println (str "Tokens before: " (:envelope/tokens-before env)))
              (println (str "Tokens after:  " (:envelope/tokens-after env)))
              (when run-eval?
                (let [eval-records [(eval/eval-retention-score env context-str 0.9)
                                    (eval/eval-regression-score env context-str {} 0.9)
                                    (eval/eval-hallucination-score env context-str 0.9)]
                      eval-summary (eval/eval-summary eval-records)]
                  (println "\n=== Eval ===")
                  (println (str "Overall: " (:eval/overall-status eval-summary)))
                  (doseq [r (:eval/records eval-summary)]
                    (println (str "  " (:eval/class r) ": " (:eval/score r) " [" (:eval/status r) "]")))))
              0)))))
    (catch Exception e
      (let [ed (ex-data e)]
        (println "Error:" (or (:error/message ed) (.getMessage e))
                 (when-let [t (:error/type ed)] (str "[" t "]")))
        (System/exit 1)))))

(defn inspect-command [args]
  (try
    (let [opts (clojure.tools.cli/parse-opts args inspect-opts)
          {:keys [options errors]} opts]
      (when (seq errors)
        (doseq [e errors] (println "Error:" e))
        (System/exit 1))
      (let [input (:input options)
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
      "inspect" (inspect-command (rest args))
      (do (println "Usage: context compress|inspect [options]") 1))
    (do (println "Usage: context compress|inspect [options]") 1)))

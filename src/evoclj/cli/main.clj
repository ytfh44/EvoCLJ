(ns evoclj.cli.main
  "The CLI entry point (Task 10.2): argument parsing, command
  dispatch, EDN output, and the exit contract.

  ARGUMENT PARSING is a tiny hand-rolled parser (tools.cli is not on
  the classpath and the task adds no dependencies): tokens starting
  with '--' are options, the following token is the value unless it
  also starts with '--' or is absent (a flag); everything else is a
  positional. --tool accumulates (repeatable); other value options
  keep their last value. Global options:

    --state-dir <dir>   runtime state root (default: the
                        EVOCLJ_STATE_DIR env var, else ./evoclj-state)
    --pretty            concise human renderer instead of raw EDN

  OUTPUT: machine-readable EDN by default — (prn data) of the result
  map, which round-trips through clojure.edn/read-string. --pretty
  switches to a concise human rendering. Typed failures print
  {:error/type <kw> :message <str> :data <sanitized>} and exit 1.

  EXIT CONTRACT: execute returns {:exit 0|1 :data <map>} without
  exiting; run prints the data and returns the code; -main wraps run
  with System/exit. Tests drive execute/run in-process with temp
  state dirs and captured *out*.

  DISPATCH: the sub-namespaces own the commands —
    evoclj.cli.genome    genome validate|inspect|diff
    evoclj.cli.session   run, replay, events, capability inspect
    evoclj.cli.evolution evolve, candidate list|inspect, eval
    evoclj.cli.promotion promote, rollback, lineage"
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [evoclj.cli.cost :as cost]
            [evoclj.cli.deploy :as deploy]
            [evoclj.cli.eval-inspect :as eval-inspect]
            [evoclj.cli.evolution :as evolution]
            [evoclj.cli.genome :as genome]
            [evoclj.cli.model :as model]
            [evoclj.cli.promotion :as promotion]
            [evoclj.cli.recovery :as recovery]
            [evoclj.cli.session :as session]
            [evoclj.kernel.error :as err]))

;; --- the hand-rolled option parser -------------------------------------------

(def ^:private flag-options
  "Options that take no value (present => true)."
  #{:pretty :evolve :no-promote :tree})

(defn- option-value?
  "True when the next token can be this option's value."
  [tokens]
  (let [nxt (second tokens)]
    (and (some? nxt) (not (str/starts-with? nxt "--")))))

(defn parse-args
  "Parse an argv vector into {:options {kw [values ...]} :positionals
  [...]}. Every occurrence of an option is recorded; :pretty is a
  flag (true), value options consume the following token when there
  is one, and :tool accumulates."
  [argv]
  (loop [tokens argv options {} positionals []]
    (if-let [t (first tokens)]
      (if (str/starts-with? t "--")
        (let [k (keyword (subs t 2))]
          (if (or (contains? flag-options k) (not (option-value? tokens)))
            (recur (rest tokens)
                   (update options k (fnil conj []) true)
                   positionals)
            (recur (drop 2 tokens)
                   (update options k (fnil conj []) (second tokens))
                   positionals)))
        (recur (rest tokens) options (conj positionals t)))
      {:options options :positionals positionals})))

;; --- the command table --------------------------------------------------------

(def ^:private commands
  "Command path -> {:fn <command fn> :arity <positional count>}."
  {["genome" "validate"]      {:fn genome/validate! :arity 1}
   ["genome" "inspect"]       {:fn genome/inspect! :arity 1}
   ["genome" "diff"]          {:fn genome/diff! :arity 2}
   ["run"]                    {:fn session/run-cmd! :arity 0}
   ["replay"]                 {:fn session/replay! :arity 0}
   ["events"]                 {:fn session/events! :arity 0}
   ["capability" "inspect"]   {:fn session/capability-inspect! :arity 0}
   ["model" "list"]            {:fn model/model-list! :arity 0}
   ["model" "inspect"]         {:fn model/model-inspect! :arity 1}
   ["evolve"]                 {:fn evolution/evolve! :arity 0}
   ["candidate" "list"]       {:fn evolution/candidate-list! :arity 0}
   ["candidate" "inspect"]    {:fn evolution/candidate-inspect! :arity 1}
   ["eval"]                   {:fn evolution/eval! :arity 0}
   ["eval-inspect"]           {:fn eval-inspect/eval-inspect! :arity 1}
   ["cycle"]                  {:fn evolution/cycle! :arity 0}
    ["loop"]                   {:fn evolution/loop! :arity 0}
    ["deploy"]                 {:fn deploy/deploy! :arity 0}
   ["promote"]                {:fn promotion/promote! :arity 0}
   ["rollback"]               {:fn promotion/rollback! :arity 0}
   ["lineage"]                {:fn promotion/lineage! :arity 1}
   ["cost"]                   {:fn cost/cost-report! :arity 0}
   ["recovery"]               {:fn recovery/recovery-scan! :arity 0}})

(defn- command-for
  "The command entry for a positional vector, or nil."
  [positionals]
  (some (fn [[path {:keys [arity] :as entry}]]
          (when (and (<= (count path) (count positionals))
                     (= path (take (count path) positionals)))
            (assoc entry :path path)))
        commands))

(defn- check-arity!
  "The command's positional args after its path must satisfy its
  arity (:cli/usage-invalid otherwise)."
  [entry positionals]
  (let [args (nthnext positionals (count (:path entry)))]
    (when (< (count args) (:arity entry))
      (throw (err/error :cli/usage-invalid
                        (str "command expects " (:arity entry)
                             " positional argument(s), got " (count args))
                        {:command (str/join " " (:path entry))})))
    entry))

(defn- single-value-options
  "Collapse the parser's per-option value vectors: every option keeps
  its LAST value except :tool, which accumulates (repeatable)."
  [options]
  (reduce-kv (fn [acc k vs]
               (assoc acc k (if (= k :tool) (vec vs) (last vs))))
             {}
             options))

(defn- command-opts
  "The options map handed to a command fn: the global :state-dir
  default (opts, then EVOCLJ_STATE_DIR, then ./evoclj-state), the
  parsed :options (collapsed to single values), and the parsed
  :positionals with the command path stripped."
  [parsed opts path]
  (assoc opts
         :state-dir (or (:state-dir opts)
                        (System/getenv "EVOCLJ_STATE_DIR")
                        "./evoclj-state")
         :options (single-value-options (:options parsed))
         :positionals (nthnext (:positionals parsed) (count path))))

(defn dispatch
  "Dispatch a parsed invocation to its command fn and return the
  result data map. Unknown commands and arity violations throw typed
  :cli/* errors."
  [parsed opts]
  (let [positionals (:positionals parsed)
        entry (command-for positionals)]
    (when-not entry
      (throw (err/error :cli/unknown-command
                        "unknown command"
                        {:command (str/join " " positionals)
                         :usage "evoclj <command> ... — see docs/implementation-plan.md Task 10.2"})))
    (check-arity! entry positionals)
    ((:fn entry) (command-opts parsed opts (:path entry)))))

;; --- output and the exit contract --------------------------------------------

(defn- typed-error
  "A typed ExceptionInfo as the error output map
  {:error/type <kw> :message <str> :data <sanitized>}."
  [^clojure.lang.ExceptionInfo e]
  (let [data (ex-data e)]
    (cond-> {:error/type (or (:error/type data) :error/unknown)
             :message (.getMessage e)}
      (seq data) (assoc :data (err/sanitize (dissoc data :error/type))))))

(defn- render-human
  "The --pretty concise human renderer: one 'key:' header per top-level
  entry, with the value pretty-printed."
  [data]
  (if (map? data)
    (doseq [[k v] data]
      (println (str k ":"))
      (pprint/pprint v))
    (pprint/pprint data)))

(defn execute
  "Run one CLI invocation and return {:exit 0|1 :data <map>
  :pretty bool} WITHOUT printing or exiting. A typed ExceptionInfo
  becomes {:exit 1 :data {:error/type ...}}; any other Throwable
  becomes :cli/internal-error."
  [argv opts]
  (try
    (let [parsed (parse-args argv)
          data (dispatch parsed opts)]
      {:exit 0
       :data data
       :pretty (boolean (first (get-in parsed [:options :pretty])))})
    (catch clojure.lang.ExceptionInfo e
      {:exit 1 :data (typed-error e) :pretty false})
    (catch Throwable t
      {:exit 1
       :data {:error/type :cli/internal-error
              :message (or (.getMessage t) (str t))}
       :pretty false})))

(defn run
  "Execute argv against the CLI, print the result to *out* (EDN by
  default, the --pretty renderer on demand; errors always print the
  error EDN), and return the exit code WITHOUT calling System/exit."
  [argv opts]
  (let [{:keys [exit data pretty]} (execute argv opts)]
    (if (and pretty (not (:error/type data)))
      (render-human data)
      (prn data))
    exit))

(defn -main
  "The JVM entry point: clojure -M -m evoclj.cli.main <args>."
  [& args]
  (let [code (run args {})]
    (System/exit code)))

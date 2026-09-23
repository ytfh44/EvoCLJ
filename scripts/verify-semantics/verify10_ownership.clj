(ns verify10-ownership
  "Semantic verification #10 — state-table write ownership.

  Model: every mutable state table has exactly ONE writer module (its
  owner). 'Owner' is a structural claim about the SOURCE, not about
  behavior, so it is checked by reading the source: collect every SQL
  write that names table T from every production namespace, and assert
  the set of files that do so is exactly the allowed set.

  The scan is AST-based (the file is read as Clojure forms, so docstring
  prose that mentions a statement is never mistaken for a write): a
  write is a string or table keyword appearing as an argument of an
  SQL-executing call (sqlite/insert-raw!, sqlite/exec!,
  sqlite/exec-raw!, jdbc/execute!, jdbc/insert!, jdbc/update!) whose
  text matches INSERT [OR ...] INTO <table> / UPDATE <table> /
  DELETE FROM <table>.

  Allowed owners (and why the exceptions are exceptions):

    works           evoclj.store.work
    capabilities    evoclj.store.capability-store
    generations     evoclj.store.generation-store
    candidates      evoclj.store.candidate-store
    kernel_state    evoclj.store.current-store (the singleton
                    definition: the only INSERT) AND
                    evoclj.promotion.current (the CURRENT
                    compare-and-set — Global Constraint 15 makes the
                    promotion transaction the pointer authority)
    episodic_memory evoclj.store.memory-store (the memory provider
                    holds the handle and issues no SQL of its own)"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

(def ^:private exec-fns
  "The SQL-executing functions whose arguments carry write SQL. The
  promotion namespaces use their own private raw JDBC helpers
  (raw-update!/raw-insert!) instead of the sqlite namespace, so those
  names are part of the scan too."
  #{'insert-raw! 'exec-raw! 'exec! 'execute! 'insert! 'update!
    'raw-update! 'raw-insert! 'raw-exec!})

(defn- source-files [root]
  (->> (file-seq (io/file root))
       (filter #(.isFile %))
       (filter #(str/ends-with? (str %) ".clj"))
       (map #(str/replace (str %) "\\" "/"))
       (sort)))

(defn- read-forms
  "Every top-level form in `path` (nil on a read error — a file that
  does not parse is reported by the parse gate, not here)."
  [path]
  (try
    (with-open [r (java.io.PushbackReader. (io/reader path))]
      (loop [forms []]
        (let [form (read {:eof ::eof :read-cond :allow} r)]
          (if (= ::eof form) forms (recur (conj forms form))))))
    (catch Exception _ nil)))

(defn- exec-calls
  "Every SQL-executing call form in a form tree."
  [form]
  (let [walk (fn walk [f]
               (cond
                 (seq? f) (concat (when (and (symbol? (first f))
                                             (exec-fns (symbol (name (first f)))))
                                    [f])
                                  (mapcat walk f))
                 (coll? f) (mapcat walk f)
                 :else nil))]
    (walk form)))

(def ^:private owners
  "The state tables under verification and their allowed writers."
  {"works"           #{"src/evoclj/store/work.clj"}
   "capabilities"    #{"src/evoclj/store/capability_store.clj"}
   "generations"     #{"src/evoclj/store/generation_store.clj"}
   "candidates"      #{"src/evoclj/store/candidate_store.clj"}
   "kernel_state"    #{"src/evoclj/store/current_store.clj"
                       "src/evoclj/promotion/current.clj"}
   "episodic_memory" #{"src/evoclj/store/memory_store.clj"}})

(defn- write-targets
  "The state tables a single exec-call form writes, as a set of table
  names (a call that only reads yields #{}). Only the tables under
  verification are considered, so an unrelated keyword argument can
  never be mistaken for a table."
  [call]
  (let [texts (->> (tree-seq coll? seq call) (filter string?))
        keywords (->> (tree-seq coll? seq call) (filter keyword?) (map name))
        from-sql (->> texts
                      (mapcat #(re-seq #"(?i)(?:INSERT\s+(?:OR\s+\w+\s+)?INTO|UPDATE|DELETE\s+FROM)\s+([a-z_]+)" %))
                      (map (comp str/trim second)))]
    (into #{}
          (filter (set (keys owners)))
          (concat from-sql keywords))))

(let [files (source-files "src/evoclj")
      writers (reduce (fn [acc path]
                        (reduce (fn [acc form]
                                  (reduce (fn [acc call]
                                            (reduce (fn [acc t]
                                                      (update acc t (fnil conj #{}) path))
                                                    acc
                                                    (write-targets call)))
                                          acc
                                          (exec-calls form)))
                                acc
                                (or (read-forms path) [])))
                      {}
                      files)]
  (doseq [[table allowed] (sort-by key owners)]
    (let [actual (get writers table #{})]
      (check! (str table " is written only by its owner")
              (= actual allowed)
              (str "owners " (vec (sort actual))
                   (when-not (= actual allowed)
                     (str " | expected " (vec (sort allowed))))))
      (check! (str table " has at least one writer")
              (boolean (seq actual))
              "a state table nobody writes would be dead schema"))))

(println "VERIFY10 DONE")

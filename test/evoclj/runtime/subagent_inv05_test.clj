(ns evoclj.runtime.subagent-inv05-test
  "INV-05 regression: evoclj.runtime.subagent must not use requiring-resolve
  or ns-resolve except for the documented scheduler cycle exception
  (see subagent.clj line ~673 — the comment annotating the cycle).
  Any future reflection reintroduced by accident fails this test."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private subagent-source
  (slurp (io/resource "evoclj/runtime/subagent.clj")))

(defn- reflection-sites
  "Match lines mentioning requiring-resolve or ns-resolve, excluding the
  docstring/comment that documents the cycle exception."
  [source]
  (->> (str/split-lines source)
       (map-indexed (fn [i line] [(inc i) line]))
       (filter (fn [[_ line]]
                 (re-find #"\b(requiring-resolve|ns-resolve)\b" line)))
       (remove (fn [[_ line]]
                 (or (str/starts-with? line ";")
                     (str/starts-with? line " *")
                     (str/starts-with? line ";"))))
       (vec)))

(deftest only-documented-cycle-carveouts
  (let [sites (reflection-sites subagent-source)
        ;; Both carve-outs are documented cycle exceptions. Anything else
        ;; is an INV-05 violation: a future reflection reintroduced by
        ;; accident will fail this test.
        ;;   - evoclj.runtime.hydrate closes a cycle via
        ;;     hydrate -> intent.dispatch -> intent.pipeline -> subagent
        ;;   - evoclj.runtime.scheduler closes a cycle via its own
        ;;     terminal-step callback into cancel-non-terminal-children!
        allowed ["evoclj.runtime.hydrate/hydrate"
                 "evoclj.runtime.scheduler/run-session!"]
        offending (remove (fn [[_ line]]
                            (some #(str/includes? line %) allowed))
                          sites)]
    (is (empty? offending)
        (str "INV-05 violation: unexpected requiring-resolve / ns-resolve sites in subagent.clj: "
             (pr-str offending)))
    (is (= (count allowed) (count sites))
        (str "Expected exactly " (count allowed) " reflection sites (the documented cycle carve-outs); got "
             (count sites) ": " (pr-str sites)))))
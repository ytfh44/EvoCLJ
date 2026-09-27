(ns evoclj.cli.model-option-test
  "The option-shape contract of the hand-rolled CLI parser
  (evoclj.cli.main).

  :tool and :model are repeatable options whose values are handed to
  `(mapv …)` at their call sites (cli/session.clj:831-832). `mapv` over a
  String seqs CHARACTERS, so if the parser collapsed either option to a
  single value, `--model deepseek/v4` would mint one capability lease per
  character (13 of them) and authorize no real model at all. These tests
  pin the vector-ness of exactly those two options, and pin that the other
  value options still collapse."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.cli.main :as main]))

(defn- opts-for
  "The options map `evoclj run` would receive for `argv`."
  [argv]
  ((deref (resolve 'evoclj.cli.main/command-opts))
   (main/parse-args argv) {} ["run"]))

(deftest repeatable-options-stay-vectors
  (testing ":model accumulates every occurrence, in order"
    (let [opts (opts-for ["run" "--genome" "current" "--task" "t.edn"
                          "--model" "deepseek/v4" "--model" "openai/gpt-4o"])]
      (is (= ["deepseek/v4" "openai/gpt-4o"] (get-in opts [:options :model]))
          "both --model values survive, in argv order")
      (is (instance? clojure.lang.IPersistentVector (get-in opts [:options :model]))
          "and the value is a vector, so (mapv str …) maps MODELS, not characters")
      (is (= "t.edn" (get-in opts [:options :task]))
          ":task is a file path read as a scalar, and still collapses")
      (is (= "current" (get-in opts [:options :genome]))
          ":genome still collapses to a scalar"))))

(deftest tool-option-stays-a-vector
  (testing ":tool accumulates every occurrence"
    (let [opts (opts-for ["run" "--genome" "current" "--task" "t.edn"
                          "--tool" "fixture/echo" "--tool" "fixture/upper"])]
      (is (= ["fixture/echo" "fixture/upper"] (get-in opts [:options :tool])))
      (is (instance? clojure.lang.IPersistentVector (get-in opts [:options :tool]))))))

(deftest other-value-options-still-collapse
  (testing "the remaining value options keep their LAST value, as scalars"
    (let [opts (opts-for ["run" "--genome" "g1" "--genome" "g2"
                          "--task" "a.edn" "--task" "b.edn"
                          "--max-cycles" "3" "--max-cycles" "9"
                          "--profile" "p1" "--profile" "p2"
                          "--status" "s1" "--status" "s2"
                          "--provider" "x" "--provider" "y"])]
      (doseq [k [:genome :task :max-cycles :profile :status :provider]]
        (is (string? (get-in opts [:options k]))
            (str k " stays a scalar string"))
        (is (= (str (name (get-in opts [:options k]))) (get-in opts [:options k]))
            (str k " is not a per-character seq"))))))

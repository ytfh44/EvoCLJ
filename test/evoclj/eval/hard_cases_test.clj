(ns evoclj.eval.hard-cases-test
  "Feature V4 - the hidden hard-case library: selection dataset
  cases shipped as EDN fixtures load deterministically."
  (:require [clojure.test :refer [deftest is]]
            [evoclj.eval.dataset :as dataset]))

(def selection-root "test/fixtures/evals/selection")

(deftest hard-case-library-loads
  (let [cases (dataset/load-cases selection-root)
        ids (mapv :case/id cases)]
    (is (= [:case/basic-echo :case/edge-empty :case/sum] ids))))

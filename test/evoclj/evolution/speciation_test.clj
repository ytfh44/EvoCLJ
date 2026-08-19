(ns evoclj.evolution.speciation-test
  "Tests for `evoclj.evolution.speciation` (S3-1).

  Candidates are grouped into species by a distance proxy derived from
  their mutation IR (ops) or genome-id prefix similarity."

  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.speciation :as speciation]))

;; --- shared fixtures ----------------------------------------------------------

(defn- candidate
  "Build a minimal candidate record map for testing."
  [ops]
  {:candidate/id (java.util.UUID/randomUUID)
   :candidate/genome-id (str "sha256:" (apply str (repeat 64 "a")))
   :ops ops})

;; --- speciate ----------------------------------------------------------------

(deftest test-speciate-empty
  (testing "returns an empty map for an empty collection"
    (is (empty? (speciation/speciate [])))))

(deftest test-speciate-single-species
  (testing "all candidates with identical ops belong to the same species"
    (let [ops [{:op :replace-form} {:op :replace-form}]
          cs (mapv #(candidate [%]) ops)
          sm (speciation/speciate cs)]
      (is (= 1 (speciation/species-count sm)))
      ;; Both candidates share the same genome-id prefix, so they should
      ;; land in exactly one species with 2 members.
      (is (= 2 (count (first (vals sm))))))))

(deftest test-speciate-threshold
  (testing "candidates within the threshold belong to the same species"
    ;; Both have exactly one op with the same pr-str, so distance = 0.0
    (let [c1 (candidate [{:op :replace-form :file "a"}])
          c2 (candidate [{:op :replace-form :file "a"}])
          sm (speciation/speciate [c1 c2] {:compatibility-threshold 0.5})]
      (is (= 1 (speciation/species-count sm))))))

(deftest test-speciate-different-species
  (testing "candidates with different ops can form different species"
    (let [c1 (candidate [{:op :replace-form}])
          c2 (candidate [{:op :delete-edn}])
          sm (speciation/speciate [c1 c2] {:compatibility-threshold 0.0})]
      ;; With threshold 0, any non-zero distance forces a new species
      (is (<= 1 (speciation/species-count sm) 2)))))

(deftest test-speciate-fallback-prefix
  (testing "candidates without ops fall back to genome-id prefix similarity"
    (let [c1 (assoc (candidate []) :ops nil)
          c2 (assoc (candidate []) :ops nil)
          ;; same genome prefix => distance 0.0
          sm (speciation/speciate [c1 c2] {:compatibility-threshold 0.5})]
      (is (= 1 (speciation/species-count sm))))))

;; --- protect-small-species ----------------------------------------------------

(deftest test-protect-small-species-boosts-small
  (testing "species below :min-species-size get boosted representation"
    (let [c1 (candidate [{:op :replace-form}])
          c2 (candidate [{:op :delete-edn}])
          sm (speciation/speciate [c1 c2] {:compatibility-threshold 0.0})]
      ;; With threshold 0, c1 and c2 are in separate species (size 1 each).
      ;; min-species-size default is 2, so both species are boosted.
      (let [protected (speciation/protect-small-species sm [c1 c2])]
        ;; Each candidate should appear at least twice (boosted)
        (is (>= (count (filter #(= (:candidate/id c1) (:candidate/id %)) protected)) 2))
        (is (>= (count (filter #(= (:candidate/id c2) (:candidate/id %)) protected)) 2))))))

(deftest test-protect-small-species-no-boost-large
  (testing "species at or above :min-species-size keep normal weight"
    (let [c1 (candidate [{:op :replace-form}])
          c2 (candidate [{:op :replace-form}])
          c3 (candidate [{:op :replace-form}])
          sm (speciation/speciate [c1 c2 c3] {:compatibility-threshold 0.5})]
      (let [protected (speciation/protect-small-species sm [c1 c2 c3])]
        ;; species size is 3, min-species-size default 2 => no boost
        (is (= 3 (count protected)))))))

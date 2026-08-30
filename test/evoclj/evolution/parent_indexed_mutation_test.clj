(ns evoclj.evolution.parent-indexed-mutation-test
  "PLT7 tests for ValidatedMutation<G> parent indexing and patch revalidation."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.genome.patch :as patch]
            [clojure.java.io :as io])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- fixture-parent []
  (load/load-genome (.toPath (io/file (io/resource "fixtures/genomes/minimal-valid")))))

(defn- temp-dir [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(deftest validated-mutation-is-parent-indexed
  (let [parent-a (fixture-parent)
        parent-b (assoc parent-a :genome/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
        raw {:mutation/id (random-uuid)
             :parent/genome-id (:genome/id parent-a)
             :hypothesis/id (random-uuid)
             :evidence/id "sha256:1111111111111111111111111111111111111111111111111111111111111111"
             :risk :behavioral
             :expected-effect {:direction :increase :primary-metric :task/success}
             :ops [{:op :replace-text :file "programs/route.clj" :anchor 1 :expect/hash (hash/text-digest (slurp (io/resource "fixtures/genomes/minimal-valid/programs/route.clj"))) :text "(ns agent.route)\n"}]}
        vm-a (mutation/validate-mutation raw parent-a)]
    (testing "ValidatedMutation carries target parent-genome-id"
      (is (= (:genome/id parent-a) (mutation/validated-parent-genome-id vm-a)))
      (is (= (:genome/id parent-a) (:parent-genome-id (first (mutation/validated-refs vm-a))))))
    (testing "applying ValidatedMutation against the matching parent succeeds"
      (let [candidate (patch/apply-mutation parent-a vm-a (temp-dir "evoclj-plt7-ok-"))]
        (is (map? candidate))
        (is (re-matches #"^sha256:[0-9a-f]{64}$" (:genome/id candidate)))))
    (testing "applying ValidatedMutation against a mismatched parent is rejected fail-closed"
      (let [e (try
                (patch/apply-mutation parent-b vm-a (temp-dir "evoclj-plt7-err-"))
                nil
                (catch clojure.lang.ExceptionInfo e e))]
        (is (= :patch/parent-mismatch (:error/type (ex-data e))))
        (is (= (:genome/id parent-b) (:expected-parent (ex-data e))))
        (is (= (:genome/id parent-a) (:mutation-parent (ex-data e))))))))

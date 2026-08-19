(ns evoclj.evolution.meta-history-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.meta-history :as meta-hist]
            [evoclj.store.sqlite :as sqlite]))

(defn- temp-db-path []
  (str (java.io.File/createTempFile "evoclj-meta-" ".db") ".db"))

(deftest record-meta-attempt!
  (testing "records and retrieves a meta attempt"
    (let [db-path (temp-db-path)
          store {:sqlite db-path}
          mg {:meta/params [] :meta/fitness 0.5 :meta/generation-id "meta-1"}]
      (try
        (let [id (meta-hist/record-meta-attempt! store mg)]
          (is (string? id))
          (let [history (meta-hist/recent-meta-history store 10)]
            (is (= 1 (count history)))
            (is (= "meta-1" (:meta/generation-id (first history))))
            (is (= 0.5 (:meta/fitness (first history))))))
        (finally
          (clojure.java.io/delete-file db-path true))))))

(deftest recent-meta-history-respects-limit
  (testing "returns at most N records"
    (let [db-path (temp-db-path)
          store {:sqlite db-path}]
      (try
        (dotimes [i 5]
          (meta-hist/record-meta-attempt! store {:meta/params [] :meta/fitness (double i)}))
        (is (= 3 (count (meta-hist/recent-meta-history store 3))))
        (finally
          (clojure.java.io/delete-file db-path true))))))

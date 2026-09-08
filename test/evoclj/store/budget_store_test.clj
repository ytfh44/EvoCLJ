(ns evoclj.store.budget-store-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as jdbc]
            [evoclj.store.budget-store :as store]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [p (str (java.nio.file.Files/createTempFile
                "evoclj-budget-" ".db"
                (make-array java.nio.file.attribute.FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (java.nio.file.Files/deleteIfExists
     (java.nio.file.Paths/get p (make-array String 0))))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- error-type [f]
  (try (f)
       nil
       (catch clojure.lang.ExceptionInfo e
         (:error/type (ex-data e)))))

(deftest migration-creates-budget-market-tables
  (let [db (fresh-db)]
    (is (= #{"capability_budgets"
             "capability_budget_reservations"
             "capability_budget_ledger"}
           (set (map :name
                     (sqlite/query db
                                   ["SELECT name FROM sqlite_master WHERE type='table' AND name LIKE 'capability_budget%' "])))))
    (is (= 25 (migrate/current-version db)))))

(deftest child-allocation-reserves-parent-and-prevents-oversubscription
  (let [db (fresh-db)
        root (store/create-budget! db {:budget/id "root"
                                       :lease/id "lease-root"
                                       :budget {:calls 10 :tokens 100}})
        child (store/create-budget! db {:budget/id "child-a"
                                        :lease/id "lease-a"
                                        :parent/id (:budget/id root)
                                        :budget {:calls 6 :tokens 40}})]
    (is (= {:calls 6 :tokens 40} (:budget child)))
    (is (= {:calls 6 :tokens 40} (:reserved (store/get-budget db "root"))))
    (is (= :capability/budget-exceeded
           (error-type #(store/create-budget! db {:budget/id "child-b"
                                                  :parent/id "root"
                                                  :budget {:calls 5}}))))
    (is (= "child-a" (:budget/id (store/budget-for-lease db "lease-a"))))))

(deftest reservation-settlement-release-and-idempotency
  (let [db (fresh-db)
        b (store/create-budget! db {:budget/id "b" :budget {:calls 5 :bytes 10}})
        first-reservation (store/reserve! db (:budget/id b)
                                          {:intent/id "i"
                                           :attempt 1
                                           :idempotency/key "intent:i:1"
                                           :amount {:calls 2 :bytes 4}})
        repeated (store/reserve! db (:budget/id b)
                                 {:intent/id "i" :attempt 1
                                  :idempotency/key "intent:i:1"
                                  :amount {:calls 2 :bytes 4}})]
    (is (= (:reservation/id first-reservation) (:reservation/id repeated)))
    (is (= :capability/budget-idempotency-conflict
           (error-type #(store/reserve! db "b"
                                        {:idempotency/key "intent:i:1"
                                         :amount {:calls 3}}))))
    (is (= :capability/budget-exhausted
           (error-type #(store/reserve! db "b"
                                        {:idempotency/key "intent:i:2"
                                         :amount {:calls 4}}))))
    (let [settled (store/settle! db (:reservation/id first-reservation)
                                  {:calls 1 :bytes 3})]
      (is (= :settled (:status settled)))
      (is (= {:calls 1 :bytes 3} (:actual settled)))
      (is (= {} (:reserved (store/get-budget db "b"))))
      (is (= {:calls 1 :bytes 3}
             (:consumed (store/get-budget db "b"))))
      (is (= :settled
             (:status (store/settle! db (:reservation/id first-reservation)
                                     {:calls 1 :bytes 3})))))))

(deftest reallocation-and-revocation-are-idempotent-and-fail-closed
  (let [db (fresh-db)
        _ (store/create-budget! db {:budget/id "root" :budget {:calls 20}})
        _ (store/create-budget! db {:budget/id "left" :parent/id "root" :budget {:calls 5}})
        _ (store/create-budget! db {:budget/id "right" :parent/id "root" :budget {:calls 5}})
        moved (store/reallocate! db "left" "right" {:calls 2} "move-1")
        repeated (store/reallocate! db "left" "right" {:calls 2} "move-1")]
    (is (= {:calls 3} (:budget (:from moved))))
    (is (= {:calls 7} (:budget (:to moved))))
    (is (= (:idempotency/key moved) (:idempotency/key repeated)))
    (let [reservation (store/reserve! db "right"
                                       {:intent/id "i" :attempt 1
                                        :idempotency/key "i:1"
                                        :amount {:calls 2}})]
      (is (= :revoked (:state (store/revoke-budget! db "root"))))
      (is (= :released
             (:status (store/release! db (:reservation/id reservation)))))
      (is (nil? (store/budget-for-lease db "missing")))
      (is (= :capability/budget-closed
             (error-type #(store/reserve! db "right"
                                          {:idempotency/key "i:2"
                                           :amount {:calls 1}}))))
      (is (= :revoked (:state (store/revoke-budget! db "root")))))))

(deftest child-revocation-preserves-siblings-and-releases-parent-allocation
  (let [db (fresh-db)
        _ (store/create-budget! db {:budget/id "root" :budget {:calls 20}})
        _ (store/create-budget! db {:budget/id "left" :parent/id "root" :budget {:calls 5}})
        _ (store/create-budget! db {:budget/id "right" :parent/id "root" :budget {:calls 5}})]
    (is (= :revoked (:state (store/revoke-budget! db "left"))))
    (is (= :active (:state (store/get-budget db "right"))))
    (is (= :active (:state (store/get-budget db "root"))))
    (is (= {:calls 5} (:reserved (store/get-budget db "root"))))
    (is (= :capability/budget-closed
           (error-type #(store/reserve! db "left"
                                        {:idempotency/key "left:after-revoke"
                                         :amount {:calls 1}}))))))

(deftest recovery-releases-interrupted-reservations
  (let [db (fresh-db)
        _ (store/create-budget! db {:budget/id "b" :budget {:calls 2}})
        reservation (store/reserve! db "b"
                                    {:intent/id "i" :attempt 1
                                     :idempotency/key "recover:1"
                                     :amount {:calls 1}})]
    (is (= 1 (store/recover! db)))
    (is (= :released
           (:status (store/release! db (:reservation/id reservation)))))
    (is (= {} (:reserved (store/get-budget db "b"))))))

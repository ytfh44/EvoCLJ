(ns evoclj.capability.budget-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.budget :as budget]
            [evoclj.capability.schema :as schema]))

(defn- error-type [f]
  (try (f)
       nil
       (catch clojure.lang.ExceptionInfo e
         (:error/type (ex-data e)))))

(deftest budget-dimensions-are-closed-and-integer-valued
  (is (= #{:calls :tokens :bytes :money-usd-micros}
         budget/canonical-dimensions))
  (is (= {:calls 2 :tokens 10 :bytes 4 :money-usd-micros 7}
         (budget/canonicalize {:model-calls 2
                               :model-tokens 10
                               :filesystem-bytes 4
                               :micro-usd 7})))
  (is (= :capability/budget-invalid
         (error-type #(budget/canonicalize {:unknown 1}))))
  (is (= :capability/budget-invalid
         (error-type #(budget/canonicalize {:calls -1}))))
  (is (= :capability/budget-invalid
         (error-type #(budget/canonicalize {:calls 1.5}))))
  (is (= :capability/budget-invalid
         (error-type #(budget/canonicalize {:calls 1 :model-calls 2})))))

(deftest budget-algebra-attenuates-and-settles
  (is (= {:calls 3 :tokens 5}
         (budget/meet {:calls 3 :tokens 10} {:calls 5 :tokens 5})))
  (is (= {:calls 2 :tokens 4}
         (budget/reserve {:calls 5 :tokens 10} {} {} {:calls 2 :tokens 4})))
  (is (= {:reserved {:calls 1}
           :consumed {:calls 2}}
         (budget/settle {:calls 3} {:calls 1} {:calls 2} {:calls 1})))
  (is (false? (budget/can-reserve? {:calls 1} {:calls 1} {} {:calls 1})))
  (let [e (error-type #(budget/settle {:calls 1} {} {:calls 1} {:calls 2}))]
    (is (= :capability/budget-invalid e))))

(deftest lease-budget-schema-is-closed
  (let [issued (java.util.Date.)
        expires (java.util.Date. (+ (.getTime issued) 60000))
        lease {:cap/id (java.util.UUID/randomUUID)
               :principal (schema/session-principal (java.util.UUID/randomUUID))
               :resource {:kind :tool :id :fixture/echo}
               :actions #{:invoke}
               :constraints {}
               :budget {:calls 3 :money-usd-micros 100}
               :issued-at issued
               :expires-at expires}]
    (is (schema/lease? (schema/make-lease lease)))
    (is (= :capability/budget-invalid
           (error-type #(schema/make-lease (assoc lease :budget {:bogus 1})))))))

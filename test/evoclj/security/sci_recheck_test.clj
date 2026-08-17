(ns evoclj.security.sci-recheck-test
  "Tests for evoclj.security.sci-recheck/recheck-candidate.

  Coverage maps to the module contract:
  - an interop / System-access sample is :safe? false with non-empty :violations;
  - a pure functional sample (defn f [x] (+ x 1)) is :safe? true with no violations;
  - a sample containing a java.io namespace literal trips exactly the
    corresponding violation (pattern \"java.io namespace literal\", match \"java.io\")."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.security.sci-recheck :as recheck]))

(deftest interop-source-is-unsafe
  (testing "System/exit interop trips the red light"
    (let [src "(ns bad.genome)\n(defn run [x] (System/exit 0))"
          r   (recheck/recheck-candidate src)]
      (is (false? (:safe? r)) "host interop must be rejected")
      (is (seq (:violations r)) "at least one violation must be reported")
      (is (some #(= "System host class access" (:pattern %)) (:violations r))
          "the System access violation is listed")))
  (testing "a Java interop dot special form is also caught"
    (let [src "(.getDeclaredMethods (Class/forName \"java.lang.Runtime\"))"
          r   (recheck/recheck-candidate src)]
      (is (false? (:safe? r)))
      (is (seq (:violations r))))))

(deftest pure-functional-source-is-safe
  (testing "a plain pure function is cleared by the gate"
    (let [src "(defn f [x] (+ x 1))"
          r   (recheck/recheck-candidate src)]
      (is (true? (:safe? r)) "pure clojure.core code is safe")
      (is (empty? (:violations r)) "no violations for pure code")
      (is (not (recheck/violation? r)) "violation? agrees it is safe"))))

(deftest java-io-literal-trips-corresponding-violation
  (testing "a java.io namespace literal is reported by its own pattern"
    (let [src "(defn bad [] java.io.File)"
          r   (recheck/recheck-candidate src)]
      (is (false? (:safe? r)))
      (is (some (fn [v]
                  (and (= "java.io namespace literal" (:pattern v))
                       (= "java.io" (:match v))))
                (:violations r))
          "the java.io literal violation carries pattern + match"))))

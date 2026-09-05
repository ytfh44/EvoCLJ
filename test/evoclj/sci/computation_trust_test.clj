(ns evoclj.sci.computation-trust-test
  "Trust provenance and acknowledgment-gate tests for
  evoclj.sci.computation/make-computation (audit item: explicit
  SCI host-extension boundary).

  A default build records :default-pure (no ambient host authority);
  a non-empty caller :api-namespaces without :trust-host-surface? true
  fails closed with typed :sci/untrusted-host-surface; an acknowledged
  extended build records exactly the granted host set plus the caller
  provenance. The gate implementation is shared with
  evoclj.sci.context/make-context (single implementation, INV-05)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.sci.computation :as computation]
            [sci.core :as sci]))

(defn- error-type-of
  "Evaluate (thunk) and return the :error/type of the thrown
  ExceptionInfo, or nil when nothing throws."
  [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:error/type (ex-data e)))))

(deftest default-computation-records-default-pure-provenance
  (testing "a default build records :default-pure with an empty granted set"
    (is (= {:trust/kind :default-pure
            :trust/granted #{}
            :trust/provenance nil}
           (computation/trust-provenance (computation/make-computation {}))))))

(deftest extended-computation-without-acknowledgment-fails-closed
  (testing "non-empty :api-namespaces without :trust-host-surface? true throws typed :sci/untrusted-host-surface"
    (is (= :sci/untrusted-host-surface
           (error-type-of #(computation/make-computation
                            {:api-namespaces {'evo.api.fixture {'answer (constantly 42)}}}))))))

(deftest extended-computation-with-acknowledgment-records-granted-set
  (testing "an acknowledged extended build records exactly the granted host set and stays callable"
    (let [c (computation/make-computation
             {:api-namespaces {'evo.api.fixture {'answer (constantly 42)}}
              :trust-host-surface? true
              :trust-provenance "computation-trust-test: granted-set recording proof"})]
      (is (= {:trust/kind :extended-host
              :trust/granted '#{evo.api.fixture/answer}
              :trust/provenance "computation-trust-test: granted-set recording proof"}
             (computation/trust-provenance c)))
      (is (= 42 (sci/eval-string* (:computation/context c) "(evo.api.fixture/answer)"))))))

(deftest malformed-trust-keys-are-rejected-with-typed-error
  (testing "a non-boolean acknowledgment is :sci/context-invalid"
    (is (= :sci/context-invalid
           (error-type-of #(computation/make-computation
                            {:trust-host-surface? "yes"})))))
  (testing "a non-string non-map provenance is :sci/context-invalid"
    (is (= :sci/context-invalid
           (error-type-of #(computation/make-computation
                            {:trust-provenance 42}))))))

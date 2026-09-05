(ns evoclj.capability.constraint-test
  "C3 regression suite: :max-bytes measures BYTES and :max-calls measures
  CALLS as DISTINCT dimensions in the usage map.

  This pins the fix for the bug where MaxBytesDescriptor's exceeded? read
  the SAME per-lease call counter as MaxCallsDescriptor, so :max-bytes N
  silently enforced 'N attempts' instead of 'N bytes'. The usage map is
  now shaped {lease-id {:calls N :bytes B}}, and each descriptor reads
  only its own dimension."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.constraint :as constraint]))

(def ^:private lease-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def ^:private max-calls (constraint/->MaxCallsDescriptor))
(def ^:private max-bytes (constraint/->MaxBytesDescriptor))
(def ^:private max-bytes-alias (constraint/->MaxBytesAliasDescriptor))

(deftest accessors-read-distinct-dimensions
  (testing "used-calls reads :calls, used-bytes reads :bytes"
    (let [usage {lease-id {:calls 7 :bytes 1234}}]
      (is (= 7 (constraint/used-calls usage lease-id)))
      (is (= 1234 (constraint/used-bytes usage lease-id)))))
  (testing "missing entries (and missing per-dimension fields) read as zero"
    (is (= 0 (constraint/used-calls {} lease-id)))
    (is (= 0 (constraint/used-bytes {} lease-id)))
    (is (= 0 (constraint/used-calls {lease-id {:bytes 9}} lease-id)))
    (is (= 0 (constraint/used-bytes {lease-id {:calls 9}} lease-id)))))

(deftest totals-sum-their-dimension
  (let [usage {#uuid "00000000-0000-0000-0000-000000000001" {:calls 3 :bytes 10}
               #uuid "00000000-0000-0000-0000-000000000002" {:calls 5 :bytes 20}}]
    (is (= 8 (constraint/total-calls usage)))
    (is (= 30 (constraint/total-bytes usage)))))

(deftest max-bytes-measures-bytes-not-calls
  (testing "a :max-bytes 100 lease with 40 accumulated bytes after 2 calls is NOT exceeded"
    (is (not (constraint/exceeded? max-bytes {:max-bytes 100}
                                   {lease-id {:calls 2 :bytes 40}} lease-id))))
  (testing "crossing the byte quota trips even after a single call"
    (is (constraint/exceeded? max-bytes {:max-bytes 100}
                              {lease-id {:calls 1 :bytes 100}} lease-id)))
  (testing "the boundary is exact: 99 stays under, 100 trips"
    (is (not (constraint/exceeded? max-bytes {:max-bytes 100}
                                   {lease-id {:calls 9 :bytes 99}} lease-id)))
    (is (constraint/exceeded? max-bytes {:max-bytes 100}
                              {lease-id {:calls 9 :bytes 100}} lease-id)))
  (testing "many calls with few bytes never trips :max-bytes"
    (is (not (constraint/exceeded? max-bytes {:max-bytes 100}
                                   {lease-id {:calls 1000 :bytes 1}} lease-id)))))

(deftest max-calls-measures-calls-not-bytes
  (testing "a :max-calls 1 lease is exceeded after the second call lands, even with zero bytes"
    (is (constraint/exceeded? max-calls {:max-calls 1}
                              {lease-id {:calls 2 :bytes 0}} lease-id)))
  (testing "a huge byte count alone never trips :max-calls"
    (is (not (constraint/exceeded? max-calls {:max-calls 100}
                                   {lease-id {:calls 1 :bytes 999999}} lease-id))))
  (testing "the boundary is exact: consumed < max-calls allows, == trips"
    (is (not (constraint/exceeded? max-calls {:max-calls 2}
                                   {lease-id {:calls 1 :bytes 0}} lease-id)))
    (is (constraint/exceeded? max-calls {:max-calls 2}
                              {lease-id {:calls 2 :bytes 0}} lease-id))))

(deftest dimensions-do-not-cross-contaminate
  (testing "an unbounded (nil) dimension never blocks"
    (is (not (constraint/exceeded? max-bytes nil
                                   {lease-id {:calls 0 :bytes 999999}} lease-id)))
    (is (not (constraint/exceeded? max-calls nil
                                   {lease-id {:calls 999 :bytes 0}} lease-id))))
  (testing "the compound case: :max-calls 100 / :max-bytes 10 trips at 11 bytes
            after only 5 calls, and allows at 10 bytes"
    (is (not (constraint/within-budget? {:max-calls 100 :max-bytes 10}
                                        {lease-id {:calls 5 :bytes 10}} lease-id)))
    (is (not (constraint/within-budget? {:max-calls 100 :max-bytes 10}
                                        {lease-id {:calls 5 :bytes 11}} lease-id)))))

(deftest max-bytes-alias-reads-the-bytes-dimension
  (testing "the camelCase :maxBytes alias reads :bytes, never :calls"
    (is (not (constraint/exceeded? max-bytes-alias {:maxBytes 100}
                                   {lease-id {:calls 2 :bytes 40}} lease-id)))
    (is (constraint/exceeded? max-bytes-alias {:maxBytes 100}
                              {lease-id {:calls 1 :bytes 100}} lease-id))))

(deftest legacy-flat-entries-read-as-calls-with-zero-bytes
  (testing "a legacy flat {lease-id n} number entry reads as n calls, 0 bytes"
    (is (= 3 (constraint/used-calls {lease-id 3} lease-id)))
    (is (= 0 (constraint/used-bytes {lease-id 3} lease-id))))
  (testing "totals mix map and legacy entries"
    (let [other #uuid "00000000-0000-0000-0000-000000000002"
          usage {lease-id 3 other {:calls 5 :bytes 20}}]
      (is (= 8 (constraint/total-calls usage)))
      (is (= 20 (constraint/total-bytes usage)))))
  (testing "writers normalize the legacy form to the map shape"
    (is (= {lease-id {:calls 4 :bytes 0}}
           (constraint/bump-calls {lease-id 3} lease-id)))
    (is (= {lease-id {:calls 3 :bytes 7}}
           (constraint/add-bytes {lease-id 3} lease-id 7))))
  (testing "legacy entries enforce the right dimension"
    (is (constraint/exceeded? max-calls {:max-calls 3}
                              {lease-id 3} lease-id))
    (is (not (constraint/exceeded? max-bytes {:max-bytes 3}
                                   {lease-id 3} lease-id))
        "3 legacy calls carry 0 bytes, so :max-bytes 3 is NOT tripped")))
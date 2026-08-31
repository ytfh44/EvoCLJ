(ns evoclj.capability.subject-anchor-test
  "P3 — subject dual-anchor session+phenotype ([W-01]).
  Verifies: mint with missing session fails, sibling sessions diverge, same session+phenotype matches."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.capability.lease :as lease]
            [evoclj.capability.mint :as mint]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]
            [evoclj.mount.filesystem :as fs])
  (:import (java.util Date UUID)))

(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(def ^:private issued-at (Date. 1700000000000))
(def ^:private expires-at (Date. 1700003600000))

(def ^:private session-a (UUID/fromString "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"))
(def ^:private session-b (UUID/fromString "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"))

(defn- base-subject [session-id phenotype]
  {:session/id session-id :phenotype/id phenotype})

(defn- base-lease-opts [session-id phenotype]
  {:cap-id (UUID/fromString "11111111-1111-4111-8111-111111111111")
   :subject (base-subject session-id phenotype)
   :resource {:kind :tool :id :fixture/echo}
   :actions #{:invoke}
   :constraints {}
   :issued-at issued-at
   :expires-at expires-at})

;; --- 1. mint with missing session -> fails :capability/schema-invalid ---

(deftest mint-missing-session-rejected
  (testing "mint-lease! with subject missing :session/id is rejected with :capability/schema-invalid"
    (let [opts (assoc (base-lease-opts session-a phenotype-p1)
                      :subject {:phenotype/id phenotype-p1})]
      (try
        (mint/mint-lease! nil opts)
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :capability/schema-invalid (:error/type (ex-data e)))
              "missing session must be :capability/schema-invalid")))))
  (testing "make-lease with subject missing :session/id is rejected"
    (let [m {:cap/id (UUID/fromString "22222222-2222-4222-8222-222222222222")
             :subject {:phenotype/id phenotype-p1}
             :resource {:kind :tool :id :fixture/echo}
             :actions #{:invoke}
             :constraints {}
             :issued-at issued-at
             :expires-at expires-at}]
      (try
        (schema/make-lease m)
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :capability/schema-invalid (:error/type (ex-data e))))))))
  (testing "mint with subject missing :phenotype/id is also rejected"
    (let [opts (assoc (base-lease-opts session-a phenotype-p1)
                      :subject {:session/id session-a})]
      (try
        (mint/mint-lease! nil opts)
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :capability/schema-invalid (:error/type (ex-data e)))))))))

;; --- 2. sibling sessions same genome+phenotype but different session -> subject-matches? false ---

(deftest sibling-sessions-do-not-match
  (testing "same phenotype, different session ids must not match (dual-anchor isolation)"
    (let [lease (mint/mint-lease! nil (base-lease-opts session-a phenotype-p1))
          sibling-subject (base-subject session-b phenotype-p1)]
      (is (false? (lease/subject-matches? lease sibling-subject))
          "sibling session with same phenotype must be false")
      (is (true? (lease/subject-matches? lease (base-subject session-a phenotype-p1)))
          "same session+phenotype must be true")))
  (testing "same session, different phenotype must not match"
    (let [lease (mint/mint-lease! nil (base-lease-opts session-a phenotype-p1))
          other-phenotype "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
          other-subject (base-subject session-a other-phenotype)]
      (is (false? (lease/subject-matches? lease other-subject))
          "same session but different phenotype must be false"))))

;; --- 3. same session+phenotype -> true (positive case) ---

(deftest same-session-phenotype-matches
  (testing "identical session+phenotype subject matches"
    (let [subject (base-subject session-a phenotype-p1)
          lease (mint/mint-lease! nil (base-lease-opts session-a phenotype-p1))]
      (is (true? (lease/subject-matches? lease subject)))
      ;; also verify via raw subject map equality through schema
      (is (true? (lease/subject-matches? lease {:session/id session-a :phenotype/id phenotype-p1})))))
  (testing "string session ids also work when lease uses string"
    (let [sess-str "session-123"
          subj {:session/id sess-str :phenotype/id phenotype-p1}
          lease (mint/mint-lease! nil {:cap-id (UUID/fromString "33333333-3333-4333-8333-333333333333")
                                       :subject subj
                                       :resource {:kind :tool :id :fixture/echo}
                                       :actions #{:invoke}
                                       :constraints {}
                                       :issued-at issued-at
                                       :expires-at expires-at})]
      (is (true? (lease/subject-matches? lease subj)))
      (is (false? (lease/subject-matches? lease (assoc subj :session/id "other-session")))))))

;; --- 4. filesystem lease also enforces dual-anchor ---

(deftest filesystem-lease-requires-dual-anchor
  (testing "mount/filesystem issue-fs-lease rejects missing session"
    (try
      (fs/issue-fs-lease nil {:subject {:phenotype/id phenotype-p1}
               :mount-id [:workspace "id"]
               :path "foo"
               :actions #{:read}})
      (is false "should have thrown for missing session")
      (catch clojure.lang.ExceptionInfo e
        (is (= :capability/schema-invalid (:error/type (ex-data e))))))))

(ns evoclj.context.residue-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.context.residue :as r]
            [evoclj.context.compression.error :as err]))

;; ----------------------------------------------------------------------
;; residue-kind?
;; ----------------------------------------------------------------------

(deftest residue-kind-valid
  (testing "residue-kind? returns true for all five valid kinds"
    (is (r/residue-kind? :constraint))
    (is (r/residue-kind? :decision))
    (is (r/residue-kind? :discovery))
    (is (r/residue-kind? :open))
    (is (r/residue-kind? :state))))

(deftest residue-kind-invalid
  (testing "residue-kind? returns false for invalid kinds"
    (is (not (r/residue-kind? :unknown)))
    (is (not (r/residue-kind? nil)))
    (is (not (r/residue-kind? "constraint")))
    (is (not (r/residue-kind? 1)))))

;; ----------------------------------------------------------------------
;; make-residue validation
;; ----------------------------------------------------------------------

(deftest make-residue-rejects-invalid-kind
  (testing "make-residue throws :context/residue-invalid for unknown kind"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/make-residue 0 :unknown-kind "some text" "some source" "2024-01-01T00:00:00Z")))))

(deftest make-residue-rejects-empty-text
  (testing "make-residue throws :context/residue-invalid for empty :residue/text"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/make-residue 0 :constraint "" "some source" "2024-01-01T00:00:00Z")))))

(deftest make-residue-rejects-non-int-id
  (testing "make-residue throws :context/residue-invalid for non-integer :residue/id"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/make-residue "0" :constraint "text" "source" "2024-01-01T00:00:00Z")))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/make-residue -1 :constraint "text" "source" "2024-01-01T00:00:00Z")))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/make-residue 1.5 :constraint "text" "source" "2024-01-01T00:00:00Z")))))

(deftest make-residue-rejects-non-string-source
  (testing "make-residue throws for non-string :residue/source"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/make-residue 0 :constraint "text" 123 "2024-01-01T00:00:00Z")))))

(deftest make-residue-rejects-invalid-at
  (testing "make-residue throws for invalid :residue/at"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/make-residue 0 :constraint "text" "source" :not-a-date)))))

(deftest make-residue-success
  (testing "make-residue returns a valid residue map"
    (let [res (r/make-residue 42 :constraint "user wants dark mode" "turn 3" "2024-01-01T00:00:00Z")]
      (is (= 42 (:residue/id res)))
      (is (= :constraint (:residue/kind res)))
      (is (= "user wants dark mode" (:residue/text res)))
      (is (= "turn 3" (:residue/source res)))
      (is (= "2024-01-01T00:00:00Z" (:residue/at res))))))

;; ----------------------------------------------------------------------
;; validate-residue
;; ----------------------------------------------------------------------

(deftest validate-residue-rejects-non-map
  (testing "validate-residue throws for non-map input"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/validate-residue "not a map")))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/validate-residue nil)))))

(deftest validate-residue-rejects-missing-fields
  (testing "validate-residue throws for missing required fields"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/validate-residue {:residue/kind :constraint :residue/text "x" :residue/source "s" :residue/at "d"})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/validate-residue {:residue/id 0 :residue/text "x" :residue/source "s" :residue/at "d"})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/validate-residue {:residue/id 0 :residue/kind :constraint :residue/source "s" :residue/at "d"})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/validate-residue {:residue/id 0 :residue/kind :constraint :residue/text "x" :residue/at "d"})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/validate-residue {:residue/id 0 :residue/kind :constraint :residue/text "x" :residue/source "s"})))))

(deftest validate-residue-passes-valid
  (testing "validate-residue returns input unchanged on success"
    (let [res (r/make-residue 0 :state "system is healthy" "health check" "2024-01-01T00:00:00Z")]
      (is (= res (r/validate-residue res))))))

;; ----------------------------------------------------------------------
;; residue-text
;; ----------------------------------------------------------------------

(deftest residue-text-accessor
  (testing "residue-text returns the :residue/text field"
    (let [res (r/make-residue 0 :discovery "found api shape" "scan" "2024-01-01T00:00:00Z")]
      (is (= "found api shape" (r/residue-text res))))))

;; ----------------------------------------------------------------------
;; append-residue
;; ----------------------------------------------------------------------

(deftest append-residue-deduplicates-by-text
  (testing "appending a residue whose text already exists yields the same vector"
    (let [existing [(r/make-residue 0 :constraint "same constraint" "src" "2024-01-01T00:00:00Z")
                    (r/make-residue 1 :decision "some decision" "src" "2024-01-01T00:00:00Z")]]
      (is (= existing (r/append-residue existing (r/make-residue 2 :state "same constraint" "src" "2024-01-01T00:00:00Z")))))
    (testing "same text but different kind is still a duplicate"
      (let [existing [(r/make-residue 0 :constraint "the constraint" "src" "2024-01-01T00:00:00Z")]]
        (is (= existing (r/append-residue existing (r/make-residue 1 :decision "the constraint" "src" "2024-01-01T00:00:00Z"))))))))

(deftest append-residue-appends-new-residues
  (testing "appending genuinely new residues appends and preserves order"
    (let [existing [(r/make-residue 0 :constraint "first" "src" "2024-01-01T00:00:00Z")]
          new (r/make-residue 1 :decision "second" "src" "2024-01-01T00:00:00Z")]
      (let [result (r/append-residue existing new)]
        (is (= 2 (count result)))
        (is (= "first" (:residue/text (first result))))
        (is (= "second" (:residue/text (second result))))))))

(deftest append-residue-accepts-vector
  (testing "append-residue accepts a vector of new residues"
    (let [existing [(r/make-residue 0 :constraint "existing" "src" "2024-01-01T00:00:00Z")]
          new [(r/make-residue 1 :discovery "new1" "src" "2024-01-01T00:00:00Z")
               (r/make-residue 2 :open "new2" "src" "2024-01-01T00:00:00Z")]]
      (let [result (r/append-residue existing new)]
        (is (= 3 (count result)))
        (is (= "existing" (:residue/text (nth result 0))))
        (is (= "new1" (:residue/text (nth result 1))))
        (is (= "new2" (:residue/text (nth result 2))))))))

(deftest append-residue-wraps-single-residue
  (testing "append-residue accepts a single residue (not just a vector)"
    (let [existing []
          single (r/make-residue 0 :state "only" "src" "2024-01-01T00:00:00Z")]
      (let [result (r/append-residue existing single)]
        (is (= 1 (count result)))
        (is (= "only" (:residue/text (first result))))))))

;; ----------------------------------------------------------------------
;; residues-by-kind
;; ----------------------------------------------------------------------

(deftest residues-by-kind-filters
  (testing "residues-by-kind returns only entries of that kind"
    (let [residues [(r/make-residue 0 :constraint "c1" "s" "2024-01-01T00:00:00Z")
                    (r/make-residue 1 :decision "d1" "s" "2024-01-01T00:00:00Z")
                    (r/make-residue 2 :constraint "c2" "s" "2024-01-01T00:00:00Z")
                    (r/make-residue 3 :discovery "di1" "s" "2024-01-01T00:00:00Z")]]
      (is (= 2 (count (r/residues-by-kind residues :constraint))))
      (is (= 1 (count (r/residues-by-kind residues :decision))))
      (is (= 0 (count (r/residues-by-kind residues :state)))))))

(deftest residues-by-kind-throws-for-unknown-kind
  (testing "residues-by-kind throws :context/residue-invalid for unknown kind"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/residues-by-kind [] :not-a-kind)))))

;; ----------------------------------------------------------------------
;; residue-merge
;; ----------------------------------------------------------------------

(deftest residue-merge-deduplicates-first-seen-order
  (testing "residue-merge deduplicates across both vectors, preserving first-seen order"
    (let [a [(r/make-residue 0 :constraint "shared" "s" "2024-01-01T00:00:00Z")
             (r/make-residue 1 :decision "in-a" "s" "2024-01-01T00:00:00Z")]
          b [(r/make-residue 2 :constraint "shared" "s" "2024-01-01T00:00:00Z")
             (r/make-residue 3 :discovery "only-in-b" "s" "2024-01-01T00:00:00Z")]]
      (let [result (r/residue-merge a b)]
        (is (= 3 (count result)))
        (is (= "shared" (:residue/text (nth result 0))))
        (is (= "in-a" (:residue/text (nth result 1))))
        (is (= "only-in-b" (:residue/text (nth result 2))))))))

(deftest residue-merge-empty-vectors
  (testing "residue-merge handles empty vectors"
    (is (= [] (r/residue-merge [] [])))
    (is (= [(r/make-residue 0 :state "x" "s" "2024-01-01T00:00:00Z")]
           (r/residue-merge [] [(r/make-residue 0 :state "x" "s" "2024-01-01T00:00:00Z")])))
    (is (= [(r/make-residue 0 :state "x" "s" "2024-01-01T00:00:00Z")]
           (r/residue-merge [(r/make-residue 0 :state "x" "s" "2024-01-01T00:00:00Z")] [])))))

;; ----------------------------------------------------------------------
;; EDN round-trip: single residue
;; ----------------------------------------------------------------------

(deftest residue-edn-roundtrip
  (testing "residue->edn / edn->residue round-trips a single residue"
    (let [original (r/make-residue 99 :discovery "api shape is paginated" "scan-ns" "2024-01-01T00:00:00Z")
          edn-str (r/residue->edn original)
          parsed (r/edn->residue edn-str)]
      (is (= original parsed)))))

(deftest edn->residue-rejects-invalid-edn
  (testing "edn->residue throws for malformed EDN"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/edn->residue "{:bad :map}")))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/edn->residue "not even a map")))))

;; ----------------------------------------------------------------------
;; EDN round-trip: vector of residues
;; ----------------------------------------------------------------------

(deftest residues-edn-roundtrip
  (testing "residues->edn / edn->residues round-trips a vector"
    (let [original [(r/make-residue 0 :constraint "c1" "s" "2024-01-01T00:00:00Z")
                    (r/make-residue 1 :open "o1" "s" "2024-01-01T00:00:00Z")
                    (r/make-residue 2 :state "st1" "s" "2024-01-01T00:00:00Z")]
          edn-str (r/residues->edn original)
          parsed (r/edn->residues edn-str)]
      (is (= original parsed)))))

(deftest edn->residues-rejects-non-vector
  (testing "edn->residues throws when EDN does not decode to a vector"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/edn->residues ":a-keyword")))))

(deftest edn->residues-validates-each-entry
  (testing "edn->residues validates each entry after reading"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/residue-invalid"
         (r/edn->residues "[{:residue/id 0}]")))))

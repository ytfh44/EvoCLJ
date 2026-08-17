(ns evoclj.context.provenance-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.context.provenance :as prov]))

;; ----------------------------------------------------------------------
;; make-source validation
;; ----------------------------------------------------------------------

(deftest make-source-rejects-unknown-kind
  (testing "make-source throws :context/provenance-invalid for unknown kind"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/provenance-invalid"
         (prov/make-source :unknown-kind "some where" "some summary")))))

(deftest make-source-rejects-empty-where
  (testing "make-source throws for empty :where"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":context/provenance-invalid"
         (prov/make-source :user-message "" "some summary")))))

;; ----------------------------------------------------------------------
;; make-claim validation
;; ----------------------------------------------------------------------

(deftest make-claim-rejects-confidence-out-of-range
  (testing "make-claim throws for confidence outside [0,1]"
    (let [src (prov/make-source :user-message "turn 1" "the user said hi")]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #":context/provenance-invalid"
           (prov/make-claim "claim text" src :confidence 1.5)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #":context/provenance-invalid"
           (prov/make-claim "claim text" src :confidence -0.1))))))

(deftest make-claim-rejects-non-string-text
  (testing "make-claim throws for non-string :text"
    (let [src (prov/make-source :user-message "turn 1" "the user said hi")]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #":context/provenance-invalid"
           (prov/make-claim 123 src)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #":context/provenance-invalid"
           (prov/make-claim nil src)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #":context/provenance-invalid"
           (prov/make-claim [] src))))))

;; ----------------------------------------------------------------------
;; trace-claim
;; ----------------------------------------------------------------------

(deftest trace-claim-returns-claim-when-matched
  (testing "trace-claim returns claim unchanged when source matches known source"
    (let [src       (prov/make-source :user-message "user turn 3" "the user asked about X")
          claim     (prov/make-claim "some residue entry" src :id 1)
          known-src [(prov/make-source :user-message "user turn 3" "the user asked about X")]]
      (is (= claim (prov/trace-claim claim known-src))))))

(deftest trace-claim-throws-for-untraceable
  (testing "trace-claim throws :context/provenance-invalid with reason :untraceable"
    (let [src       (prov/make-source :user-message "user turn 99" "never seen this")
          claim     (prov/make-claim "orphan residue" src :id 42)
          known-src [(prov/make-source :tool-output "scheduler/run-session!" "tool output")]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #":context/provenance-invalid"
           (prov/trace-claim claim known-src))))))

;; ----------------------------------------------------------------------
;; EDN round-trip
;; ----------------------------------------------------------------------

(deftest source-edn-round-trip
  (testing "source->edn / edn->source round-trips correctly"
    (let [src (prov/make-source :tool-output "evoclj.runtime.scheduler/run-session!"
                                "scheduled tasks" :turn 5 :hash "abc123")
          edn (prov/source->edn src)
          out (prov/edn->source edn)]
      (is (= src out)))))

(deftest claim-edn-round-trip
  (testing "claim->edn / edn->claim round-trips with nested source"
    (let [src   (prov/make-source :observation "turn 7" "system observed fact"
                                  :hash "obs-789")
          claim (prov/make-claim "the model noted that X" src :id 7 :confidence 0.85)
          edn   (prov/claim->edn claim)
          out   (prov/edn->claim edn)]
      (is (= claim out))
      (is (= src (:claim/source out))))))

;; ----------------------------------------------------------------------
;; provenance-report
;; ----------------------------------------------------------------------

(deftest provenance-report-counts-correctly
  (testing "provenance-report distinguishes traced vs untraceable and by-kind counts"
    (let [src1 (prov/make-source :user-message "turn 1" "user says hello")
          src2 (prov/make-source :tool-output "run-session!" "tool output here")
          ;; hand-crafted claim with nil source/where to simulate an untraceable source
          untraced-claim {:claim/id 200
                          :claim/text "untraced claim"
                          :claim/source {:source/kind :observation
                                         :source/where ""
                                         :source/summary "unmatched"}}
          claims [(prov/make-claim "traced claim 1" src1 :id 100)
                  (prov/make-claim "traced claim 2" src2 :id 101)
                  untraced-claim]
          rpt (prov/provenance-report claims)]
      (is (= 3 (:provenance/total rpt)))
      (is (= 2 (:provenance/traced rpt)))
      (is (= 1 (:provenance/untraceable rpt)))
      (is (= {:user-message 1
              :tool-output   1
              :observation   1
              :decision      0
              :compression-output 0}
             (:provenance/by-kind rpt)))
      (is (= [200] (:provenance/untraceable-ids rpt))))))

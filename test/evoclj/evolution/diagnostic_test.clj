(ns evoclj.evolution.diagnostic-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.diagnostic :as diagnostic]
            [evoclj.evolution.diagnosis-schema :as diagnosis]
            [evoclj.evolution.evidence-schema :as evidence]))

(def ^:private hash64
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private artifact-id (str "sha256:" hash64))

(def ^:private evidence-pack
  {:evidence/id artifact-id
   :generation/id "generation-1"
   :cutoff-event-id 12
   :episodes []
   :summary {}})

(def ^:private diagnosis-result
  {:diagnosis/id artifact-id
   :evidence/id artifact-id
   :hypotheses []})

(def ^:private details
  {:producer {:kind :clj-kondo :version "2026.09"}
   :subject {:artifact/revision artifact-id
             :workspace/id "workspace-1"}
   :findings []
   :exit-status 0
   :status :complete
   :captured-at (java.util.Date. 0)})

(defn- thrown-error-type
  [f]
  (:error/type
   (ex-data
    (try
      (f)
      nil
      (catch clojure.lang.ExceptionInfo e e)))))

(deftest build-bundle-carries-provenance
  (let [bundle (diagnostic/build-bundle evidence-pack diagnosis-result details)]
    (is (map? (diagnosis/validate-diagnosis diagnosis-result)))
    (is (map? (evidence/validate-pack evidence-pack)))
    (is (= artifact-id (:evidence/id bundle)))
    (is (= artifact-id (:diagnosis/id bundle)))
    (is (re-matches #"^sha256:[0-9a-f]{64}$" (:diagnostic/id bundle)))))

(deftest build-bundle-is-deterministic
  (is (= (diagnostic/build-bundle evidence-pack diagnosis-result details)
         (diagnostic/build-bundle evidence-pack diagnosis-result details))))

(deftest build-bundle-rejects-provenance-mismatch
  (let [other-diagnosis (assoc diagnosis-result
                               :evidence/id
                               (str "sha256:" (apply str (repeat 64 "b"))))]
    (is (= :diagnostic/provenance-mismatch
           (thrown-error-type
            #(diagnostic/build-bundle evidence-pack other-diagnosis details))))))

(deftest build-bundle-validates-capture-details
  (is (= :diagnostic/bundle-invalid
         (thrown-error-type
          #(diagnostic/build-bundle evidence-pack diagnosis-result
                                    (assoc details :status :running))))))

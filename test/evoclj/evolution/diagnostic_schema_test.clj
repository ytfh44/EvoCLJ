(ns evoclj.evolution.diagnostic-schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.diagnostic-schema :as ds]))

(def ^:private hash64
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private artifact-id (str "sha256:" hash64))

(defn- finding
  []
  {:rule/id "no-unused-vars"
   :severity :error
   :location {:path "src/foo.clj" :line 37 :column 4}
   :message "unused binding"
   :fixable? true})

(defn- bundle
  ([] (bundle {}))
  ([overrides]
   (merge
    {:diagnostic/id artifact-id
     :producer {:kind :clj-kondo :version "2026.09"}
     :subject {:artifact/revision artifact-id
               :workspace/id "workspace-1"
               :snapshot/id artifact-id}
     :evidence/id artifact-id
     :diagnosis/id artifact-id
     :findings [(finding)]
     :exit-status 1
     :status :complete
     :captured-at (java.util.Date.)}
    overrides)))

(defn- thrown-error-type
  [f]
  (:error/type
   (ex-data
    (try
      (f)
      nil
      (catch clojure.lang.ExceptionInfo e e)))))

(deftest diagnostic-bundle-validates
  (testing "complete bundles validate"
    (is (= (bundle) (ds/validate-bundle (bundle)))))
  (testing "suppressed bundles remain valid evidence"
    (is (= :suppressed
           (:status (ds/validate-bundle (bundle {:status :suppressed})))))
    (is (empty? (:findings
                 (ds/validate-bundle (bundle {:findings []})))))))

(deftest diagnostic-bundle-rejects-invalid-shape
  (testing "top-level and nested unknown keys are rejected"
    (is (= :diagnostic/bundle-invalid
           (thrown-error-type
            #(ds/validate-bundle (assoc (bundle) :unexpected true)))))
    (is (= :diagnostic/bundle-invalid
           (thrown-error-type
            #(ds/validate-bundle
              (assoc-in (bundle) [:producer :unexpected] true))))))
  (testing "artifact references must use canonical ids"
    (is (= :diagnostic/bundle-invalid
           (thrown-error-type
            #(ds/validate-bundle
              (assoc (bundle) :diagnostic/id "not-an-artifact")))))
    (is (= :diagnostic/bundle-invalid
           (thrown-error-type
            #(ds/validate-bundle
              (assoc-in (bundle)
                        [:subject :artifact/revision]
                        "not-an-artifact"))))))
  (testing "status is closed"
    (is (= :diagnostic/bundle-invalid
           (thrown-error-type
            #(ds/validate-bundle (bundle {:status :running})))))))

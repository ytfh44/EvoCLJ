(ns evoclj.eval.judge-test
  "Tests for the LLM-as-judge equivalence fn (feature V1).

  The judge decides semantic output equivalence through a real model
  via the host-injected :model-call closure. These tests use fake
  closures returning canned provider :values, so the judge's prompt
  rendering, JSON verdict parsing, fail-loud error contract, and
  config validation are exercised without any provider.

  Error contract under test: :eval/judge-config-invalid,
  :eval/judge-failed."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.judge :as judge]
            [evoclj.store.cas :as cas]
            [evoclj.store.enrichment :as enrichment]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files LinkOption Paths FileVisitOption)
           (java.nio.file.attribute FileAttribute)))

;; --- canned :model-call closures ----------------------------------------------

(defn- value-with
  "A provider :value wrapping the given text."
  [text]
  {:value {:model/output {:text text}
           :usage {:prompt-tokens 5 :completion-tokens 3}}})

(defn- canned-call
  "A fake :model-call closure returning the given :value and recording
  the (model-id messages options) it was invoked with."
  [value]
  (let [calls (atom [])]
    [(fn [id messages options]
       (swap! calls conj {:id id :messages messages :options options})
       value)
     calls]))

(defn- yes-text []
  (json/generate-string {:equivalent true}))

(defn- no-text []
  (json/generate-string {:equivalent false}))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by f, or nil."
  [f]
  (:error/type (ex-data (try (f) nil
                             (catch clojure.lang.ExceptionInfo e e)))))

;; --- happy paths ----------------------------------------------------------------

(deftest happy-path
  (testing "a true verdict yields true"
    (let [[mc calls] (canned-call (value-with (yes-text)))
          j (judge/llm-judge {:model-call mc :model/id "lmstudio/fake"})]
      (is (true? (j {:text "hello"} [{:text "hello there"}])))
      (let [call (first @calls)]
        (is (= "lmstudio/fake" (:id call)))
        (is (= 0.0 (get-in call [:options :temperature])))
        (is (= 1024 (get-in call [:options :max-tokens]))))))
  (testing "a false verdict yields false"
    (let [[mc _] (canned-call (value-with (no-text)))
          j (judge/llm-judge {:model-call mc :model/id "m"})]
      (is (false? (j {:text "a"} [{:text "b"}])))))
  (testing "the user message renders expected and actual outputs"
    (let [[mc calls] (canned-call (value-with (yes-text)))
          j (judge/llm-judge {:model-call mc :model/id "m"})
          _ (j {:text "EXPECTED"} [{:text "ACTUAL-1"} {:text "ACTUAL-2"}])
          user (:content (second (get-in (first @calls) [:messages])))]
      (is (re-find #"EXPECTED" user))
      (is (re-find #"ACTUAL-1" user))
      (is (re-find #"ACTUAL-2" user)))))

(deftest custom-max-tokens
  (testing ":max-tokens is configurable"
    (let [[mc calls] (canned-call (value-with (yes-text)))
          j (judge/llm-judge {:model-call mc :model/id "m" :max-tokens 64})]
      (is (true? (j 1 [2])))
      (is (= 64 (get-in (first @calls) [:options :max-tokens]))))))

;; --- response robustness ---------------------------------------------------------

(deftest json-in-code-fences
  (testing "JSON wrapped in prose/code fences is parsed"
    (let [wrapped (str "My verdict:\n"
                        (yes-text) "\nend")
          [mc _] (canned-call (value-with wrapped))
          j (judge/llm-judge {:model-call mc :model/id "m"})]
      (is (true? (j 1 [1]))))))

(deftest non-json-fails-loud
  (testing "a non-JSON response is a typed :eval/judge-failed, never a
            silent false"
    (let [[mc _] (canned-call (value-with "I cannot judge this."))
          j (judge/llm-judge {:model-call mc :model/id "m"})]
      (is (= :eval/judge-failed
             (thrown-error-type #(j 1 [1])))))))

(deftest missing-verdict-fails-loud
  (testing "a JSON response without a boolean :equivalent is a failure"
    (let [[mc _] (canned-call (value-with (json/generate-string {:note "maybe"})))
          j (judge/llm-judge {:model-call mc :model/id "m"})]
      (is (= :eval/judge-failed
             (thrown-error-type #(j 1 [1])))))))

;; --- model-call failures -----------------------------------------------------------

(deftest model-call-throws
  (testing "a thrown ExceptionInfo becomes :eval/judge-failed with the
            cause :error/type preserved"
    (let [mc (fn [& _] (throw (ex-info "model down"
                                       {:error/type :provider/transient-error})))
          j (judge/llm-judge {:model-call mc :model/id "m"})
          e (try (j 1 [1]) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (= :eval/judge-failed (:error/type (ex-data e))))
      (is (= :provider/transient-error
             (get-in (ex-data e) [:cause :error/type])))))
  (testing "a generic Throwable also becomes :eval/judge-failed"
    (let [mc (fn [& _] (throw (RuntimeException. "boom")))
          j (judge/llm-judge {:model-call mc :model/id "m"})]
      (is (= :eval/judge-failed (thrown-error-type #(j 1 [1])))))))

(deftest non-ok-dispatch-result-fails-loud
  (testing "a non-ok :result/status is a typed failure"
    (let [mc (fn [& _] {:result/status :provider/not-found :value nil})
          j (judge/llm-judge {:model-call mc :model/id "m"})]
      (is (= :eval/judge-failed (thrown-error-type #(j 1 [1])))))))

(deftest non-string-text-fails-loud
  (testing "a non-string :text is a typed failure"
    (let [mc (fn [& _] {:value {:model/output {:text 42}}})
          j (judge/llm-judge {:model-call mc :model/id "m"})]
      (is (= :eval/judge-failed (thrown-error-type #(j 1 [1])))))))

;; --- config validation --------------------------------------------------------------

(deftest config-validation
  (testing "unknown config keys are rejected (closed map)"
    (is (= :eval/judge-config-invalid
           (thrown-error-type #(judge/llm-judge
                                {:model-call identity :model/id "m"
                                 :store {:cas "/tmp"}})))))
  (testing "missing :model-call is rejected"
    (is (= :eval/judge-config-invalid
           (thrown-error-type #(judge/llm-judge {:model/id "m"})))))
  (testing "missing :model/id is rejected"
    (is (= :eval/judge-config-invalid
           (thrown-error-type #(judge/llm-judge {:model-call identity})))))
  (testing "non-map config is rejected"
    (is (= :eval/judge-config-invalid
           (thrown-error-type #(judge/llm-judge :nope))))))

;; --- keyword registration -------------------------------------------------------------

(deftest keyword-registration
  (testing "judge-keyword is :equivalence/llm-judge and merge-judge puts
            the fn in an equivalence registry"
    (is (= :equivalence/llm-judge (judge/judge-keyword)))
    (let [[mc _] (canned-call (value-with (yes-text)))
          j (judge/llm-judge {:model-call mc :model/id "m"})
          reg (judge/merge-judge {:equivalence/byte-identical =} j)]
      (is (= j (:equivalence/llm-judge reg)))
      (is (= = (:equivalence/byte-identical reg))))))

;; ============================================================================
;; Task A6 — judge verdicts as enrichment records (Foundation F3)
;; ============================================================================

;; --- temp stores (mirrors evoclj.store.enrichment-test temp-stores!) ---------

(def ^:private store-paths (atom []))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifacts)."
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- temp-stores!
  "A migrated sqlite database in a temp file plus a fresh temp CAS
  root, as the executor :stores map {:sqlite <db> :cas <cas>}. The temp
  paths are recorded for cleanup."
  []
  (let [db-path (str (Files/createTempFile "evoclj-judge-" ".db"
                                           (make-array FileAttribute 0)))
        cas-path (str (Files/createTempDirectory "evoclj-judge-cas-"
                                                 (make-array FileAttribute 0)))
        db (sqlite/spec db-path)]
    (migrate/migrate! db)
    (swap! store-paths conj db-path cas-path)
    {:sqlite db :cas (cas/->cas cas-path)}))

(defn- dispose-stores!
  "Delete the temp paths created by temp-stores! (idempotent)."
  []
  (doseq [p @store-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! store-paths []))

(use-fixtures :each (fn [f] (f) (dispose-stores!)))

;; --- fixture verdict batches ---------------------------------------------------

(def ^:private evaluation-id
  "The evaluation id (a uuid string) the verdict enrichments attach to."
  "8f8f8f8f-8f8f-4f8f-8f8f-8f8f8f8f8f8f")

(defn- sample-verdicts
  "A two-verdict batch: one equivalent, one not."
  []
  [(judge/verdict-record :sel/c1 1 "seed-1" {:text "expected"}
                         [{:text "actual"}] true)
   (judge/verdict-record :sel/c2 1 "seed-2" {:text "expected"}
                         [{:text "different"}] false)])

;; --- the pure mapping ----------------------------------------------------------

(deftest verdict-record-mapping
  (testing "the pure verdict record carries case context, the boolean, and a derived score"
    (let [v (judge/verdict-record :sel/c1 1 "seed-1" {:text "expected"}
                                  [{:text "actual"}] true)]
      (is (= :sel/c1 (:case/id v)))
      (is (= 1 (:repetition v)))
      (is (= "seed-1" (:pair/seed v)))
      (is (= {:text "expected"} (:expected-output v)))
      (is (= [{:text "actual"}] (:outputs v)))
      (is (true? (:equivalent v)))
      (is (= 1.0 (:score v)))))
  (testing "a false verdict scores zero"
    (is (= 0.0 (:score (judge/verdict-record :sel/c1 1 "s" 1 [2] false))))))

(deftest verdict-payload-is-pure-edn
  (testing "verdict-payload is a plain EDN-safe map with the batch and its count"
    (let [vs (sample-verdicts)
          p (judge/verdict-payload vs)]
      (is (= (count vs) (:count p)))
      (is (= vs (:verdicts p)))
      (is (= p (edn/read-string (pr-str p)))))))

;; --- the store adapter ----------------------------------------------------------

(deftest verdict-roundtrips-via-latest-enrichment
  (testing "a verdict batch persists under (entity-kind :evaluation,
            entity-id = evaluation id, kind :judge-verdict) and round-trips
            via latest-enrichment/payload"
    (let [store (temp-stores!)
          result (judge/persist-verdict-enrichment!
                  store evaluation-id (sample-verdicts))
          rec (enrichment/latest-enrichment store :evaluation
                                            evaluation-id :judge-verdict)]
      (is (= :ok (:enrichment/status result)))
      (is (= 1 (:version rec)))
      (is (= :evaluation (:entity/kind rec)))
      (is (= evaluation-id (:entity/id rec)))
      (is (= :judge-verdict (:kind rec)))
      (is (= (judge/verdict-payload (sample-verdicts))
             (enrichment/payload store rec))))))

(deftest version-increments-per-verdict-batch
  (testing "each verdict batch gets max+1 version per (entity, kind)"
    (let [store (temp-stores!)
          r1 (judge/persist-verdict-enrichment!
              store evaluation-id [(first (sample-verdicts))])
          r2 (judge/persist-verdict-enrichment!
              store evaluation-id (sample-verdicts))]
      (is (= 1 (:version (:enrichment r1))))
      (is (= 2 (:version (:enrichment r2))))
      (is (= [1 2] (mapv :version
                         (enrichment/enrichments store :evaluation
                                                 evaluation-id :judge-verdict)))))))

(deftest store-write-failure-does-not-fail-the-evaluation
  (testing "an invalid store yields a recorded :failed result, never a throw"
    (let [result (judge/persist-verdict-enrichment!
                  {:sqlite nil} evaluation-id (sample-verdicts))]
      (is (= :failed (:enrichment/status result)))
      (is (= :enrichment/store-invalid (:error/type result)))))
  (testing "a mid-flight CAS write failure is also isolated and recorded, and
            nothing is attached to the entity"
    (let [store (temp-stores!)
          broken {:sqlite (:sqlite store) :cas nil}
          result (judge/persist-verdict-enrichment!
                  broken evaluation-id (sample-verdicts))]
      (is (= :failed (:enrichment/status result)))
      (is (keyword? (:error/type result)))
      (is (empty? (enrichment/enrichments store :evaluation
                                          evaluation-id :judge-verdict))))))

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
            [clojure.test :refer [deftest is testing]]
            [evoclj.eval.judge :as judge]))

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

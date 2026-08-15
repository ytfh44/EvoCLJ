(ns evoclj.adversarial.model-output-hardening-test
  "Feature S4 - model-output schema hardening: the LLM judge must
  reject MALFORMED model verdicts with a typed error, never crash,
  never guess, and never leak garbage into the evaluation pipeline."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [evoclj.eval.judge :as judge]))

(defn- value-with [text]
  {:value {:model/output {:text text}
           :usage {:prompt-tokens 1 :completion-tokens 1}}})

(defn- judge-over [text]
  (judge/llm-judge {:model-call (fn [& _] (value-with text))
                   :model/id "m"}))

(defn- verdict-error-type [text]
  (let [j (judge-over text)]
    (:error/type (ex-data (try (j {:text "e"} [{:text "a"}])
                              nil
                              (catch clojure.lang.ExceptionInfo e e))))))

(deftest judge-rejects-malformed-verdicts
  (is (= :eval/judge-failed (verdict-error-type "null")) "null json")
  (is (= :eval/judge-failed (verdict-error-type "[1,2]")) "array json")
  (is (= :eval/judge-failed (verdict-error-type "yes")) "string json")
  (is (= :eval/judge-failed (verdict-error-type "42")) "number json")
  (is (= :eval/judge-failed
         (verdict-error-type
          (json/generate-string {:equivalent "yes"})))
      "non-bool equivalent")
  (is (= :eval/judge-failed
         (verdict-error-type
          (json/generate-string {:verdict true})))
      "missing equivalent key")
  (is (= :eval/judge-failed (verdict-error-type "{}")) "empty object")
  (is (= :eval/judge-failed
         (verdict-error-type
          (json/generate-string {:equivalent {:nested true}})))
      "nested object equivalent")
  (is (= :eval/judge-failed
         (verdict-error-type
          (json/generate-string {:equivalent nil})))
      "null equivalent")
  (is (= :eval/judge-failed (verdict-error-type "just prose")) "no braces"))

(deftest judge-accepts-valid-verdicts-after-barrage
  (is (true? ((judge-over (json/generate-string {:equivalent true}))
              {:text "e"} [{:text "a"}])))
  (is (false? ((judge-over (json/generate-string {:equivalent false}))
               {:text "e"} [{:text "a"}]))))

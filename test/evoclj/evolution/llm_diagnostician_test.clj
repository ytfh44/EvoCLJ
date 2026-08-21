(ns evoclj.evolution.llm-diagnostician-test
  "Tests for the LLM-driven Diagnostician adapter (feature 1).

  The adapter sits at the same Diagnostician contract as the
  deterministic pattern adapter, but sources hypotheses from a real
  language model via the host-injected :model-call closure. These tests
  use fake :model-call closures that return canned provider :values,
  so the adapter's JSON parsing, per-entry schema filtering (LLM-noise
  tolerance), deterministic id assignment, and fail-loud error contract
  are exercised without any provider. Fixture evidence packs mirror the
  frozen-pack shape from the component schema (compact episode refs +
  summary), exactly as diagnosed in evoclj.evolution.diagnose-test.

  Error contract under test: :diagnosis/config-invalid,
  :diagnosis/llm-failed, :diagnosis/llm-response-invalid.

  JSON responses are built with cheshire generate-string rather than
  hand-written, so the test text exactly matches what a real model
  (post keywordize) would produce."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.diagnose :as diag]
            [evoclj.evolution.diagnosis-schema :as ds]
            [evoclj.evolution.llm-diagnostician :as llm])
  (:import (java.util UUID)))

;; --- shared fixture identity ---

(def ^:private placeholder-hash (str "sha256:" (apply str (repeat 64 "0"))))
(def ^:private generation-id "generation-1")

(defn- uuid-of
  "A fixed, readable UUID for fixture ids."
  [n]
  (UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- ep
  "One compact episode ref for a hand-built, schema-valid evidence
  pack (component EpisodeRefSchema)."
  [{:keys [id first last outcome usage]}]
  {:episode/id id
   :session/id (random-uuid)
   :generation/id generation-id
   :excerpt-ref placeholder-hash
   :outcome outcome
   :trace {:first-event first :last-event last}
   :usage (or usage {})})

(defn- pack
  "A hand-built, schema-valid frozen evidence pack."
  [episodes & [summary]]
  {:evidence/id placeholder-hash
   :generation/id generation-id
   :cutoff-event-id 1000
   :episodes episodes
   :summary (merge {:selector {:recent 10 :include-successes 5
                               :include-failures 5 :include-high-cost 5}
                    :eligible (count episodes)
                    :selected (count episodes)
                    :successes 0 :failures 0 :high-cost 0}
                   summary)})

(def episodes1
  [(ep {:id (uuid-of 1) :first 10 :last 12
        :outcome {:status :failed :score nil}})])

(defn- value-with
  "A provider :value wrapping the given text."
  [text]
  {:value {:model/output {:text text}
           :usage {:prompt-tokens 10 :completion-tokens 10}}})

(defn- canned-call
  "A fake :model-call closure returning the given :value and recording
  the (model-id messages options) it was invoked with (for asserting the
  adapter passes its config through)."
  [value]
  (let [calls (atom [])]
    [(fn [id messages options]
       (swap! calls conj {:id id :messages messages :options options})
       value)
     calls]))

(defn- model-text
  "Render a vector of raw hypothesis maps as the model's JSON text
  (the flat-key vocabulary the system prompt asks for)."
  [hypotheses]
  (json/generate-string {:hypotheses hypotheses}))

(defn- valid-hypothesis
  "A raw (pre-normalization) hypothesis map in the flat-key vocabulary
  the adapter normalizes. Keyword overrides win."
  [& [overrides]]
  (merge {:pattern "task/success"
          :claim "task success rate is low"
          :support [{:episode-id (str (uuid-of 1)) :event-ids [12]}]
          :counterevidence []
          :target-kind "workflow"
          :target-id "task"
          :effect-metric "task/success"
          :effect-direction "increase"
          :confidence-band "high"}
         overrides))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))
;; --- happy path ---

(deftest happy-path
  (testing "a canned JSON model response yields a valid Diagnosis"
    (let [[mc calls] (canned-call (value-with (model-text [(valid-hypothesis)])))
          d (llm/llm-diagnostician {:model-call mc :model/id "lmstudio/fake"})
          p (pack episodes1)
          diagnosis (diag/diagnose d p)]
      ;; the :evidence/id provenance carries through
      (is (= (:evidence/id p) (:evidence/id diagnosis)))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:diagnosis/id diagnosis)))
      ;; one validated hypothesis
      (is (= 1 (count (:hypotheses diagnosis))))
      (let [h (first (:hypotheses diagnosis))]
        (is (= :task/success (:pattern h)))
        (is (string? (:claim h)))
        (is (= [{:episode/id (uuid-of 1) :event-ids [12]}] (:support h)))
        (is (= {:kind :workflow :id :task} (:target h)))
        (is (= {:metric :task/success :direction :increase}
               (:expected-effect h)))
        ;; the config :confidence-band default is stamped on
        (is (= :medium (:confidence-band h))))
      ;; the whole diagnosis validates against the schema
      (is (map? (ds/validate-diagnosis diagnosis)))
      ;; the adapter passes model id, messages, and options through
      (is (= "lmstudio/fake" (:id (first @calls))))
      (is (= :system (:role (first (get-in (first @calls) [:messages])))))
      (is (= 0.2 (get-in (first @calls) [:options :temperature])))
      (is (= 4096 (get-in (first @calls) [:options :max-tokens])))))
  (testing "deterministic ids are stable across two diagnoses"
    (let [[mc _] (canned-call (value-with (model-text [(valid-hypothesis)])))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack episodes1 {:selected 1 :successes 0 :failures 1})
          d1 (diag/diagnose d p)
          d2 (diag/diagnose d p)]
      (is (= d1 d2))
      (is (= (:diagnosis/id d1) (:diagnosis/id d2)))
      (is (= (:hypotheses d1) (:hypotheses d2))))))

;; --- bounded by config ---

(deftest max-hypotheses-cap
  (testing "model returns 5 but only :max-hypotheses are kept"
    (let [[mc _] (canned-call
                  (value-with (model-text (mapv (fn [_] (valid-hypothesis)) (range 5)))))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"
                                   :max-hypotheses 3})
          p (pack episodes1 {:selected 1 :successes 0 :failures 1})
          diagnosis (diag/diagnose d p)]
      (is (= 3 (count (:hypotheses diagnosis)))))))

(deftest few-hypotheses-below-max
  (testing "fewer hypotheses than :max-hypotheses is fine"
    (let [[mc _] (canned-call (value-with (model-text [(valid-hypothesis)])))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"
                                   :max-hypotheses 5})
          p (pack episodes1 {:selected 1})
          diagnosis (diag/diagnose d p)]
      (is (= 1 (count (:hypotheses diagnosis)))))))

;; --- LLM-noise tolerance ---

(deftest partial-garbage-is-tolerated
  (testing "one invalid entry is skipped (noise), the valid one survives"
    (let [raw [(valid-hypothesis)
               {:pattern "task/success"}  ;; no claim -> unusable
               ]
          [mc _] (canned-call (value-with (model-text raw)))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack episodes1 {:selected 1})
          diagnosis (diag/diagnose d p)]
      (is (= 1 (count (:hypotheses diagnosis))))
      (is (= "task success rate is low" (:claim (first (:hypotheses diagnosis))))))))

(deftest string-array-counterevidence-is-dropped
  (testing "models often emit a string-array counterevidence (strings,
            not refs) — those entries are dropped as noise, and a
            hypothesis whose refs survive still validates"
    (let [raw [(assoc (valid-hypothesis)
                      :counterevidence ["None" "nothing"])
               (assoc (valid-hypothesis)
                      :claim "second hypothesis"
                      :support [])]
          [mc _] (canned-call (value-with (model-text raw)))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack episodes1 {:selected 1})
          diagnosis (diag/diagnose d p)
          h1 (first (:hypotheses diagnosis))]
      (is (= 1 (count (:hypotheses diagnosis))) "the empty-support entry is dropped")
      (is (= [] (:counterevidence h1)) "string entries were dropped")
      (is (= [{:episode/id (uuid-of 1) :event-ids [12]}] (:support h1))))))

(deftest all-invalid-fails-loud
  (testing "a non-empty hypotheses array that is ALL noise throws"
    (let [[mc _] (canned-call
                  (value-with (model-text [{:pattern "task/success"}
                                             {:pattern "task/high-cost"}])))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack episodes1 {:selected 1})]
      (is (= :diagnosis/llm-response-invalid
             (thrown-error-type #(diag/diagnose d p)))))))

;; --- response shape ---

(deftest non-json-fails-loud
  (testing "non-JSON text is rejected"
    (let [[mc _] (canned-call (value-with "Sorry, I cannot help with that."))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack [] {:selected 0})]
      (is (= :diagnosis/llm-response-invalid
             (thrown-error-type #(diag/diagnose d p)))))))

(deftest json-in-code-fences
  (testing "JSON wrapped in code fences is parsed"
    (let [wrapped (str "Here is my analysis:\n```json\n"
                       (model-text [(valid-hypothesis)]) "\n```")
          [mc _] (canned-call (value-with wrapped))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack episodes1 {:selected 1})
          diagnosis (diag/diagnose d p)]
      (is (= 1 (count (:hypotheses diagnosis)))))))

;; --- model-call failures ---

(deftest model-call-throws-exception-info
  (testing "a thrown ExceptionInfo becomes :diagnosis/llm-failed with
            the cause :error/type preserved"
    (let [mc (fn [& _]
               (throw (ex-info "provider down"
                                {:error/type :provider/transient-error})))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack [] {:selected 0})
          e (try (diag/diagnose d p) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :diagnosis/llm-failed (:error/type (ex-data e))))
      (is (= :provider/transient-error
             (get-in (ex-data e) [:cause :error/type])))))
  (testing "a generic Throwable also becomes :diagnosis/llm-failed"
    (let [mc (fn [& _] (throw (RuntimeException. "oops")))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack [] {:selected 0})]
      (is (= :diagnosis/llm-failed
             (thrown-error-type #(diag/diagnose d p)))))))

(deftest non-string-output-fails-loud
  (testing "a non-string :text is a typed response failure"
    (let [mc (fn [& _] {:value {:model/output {:text 123}}})
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack [] {:selected 0})]
      (is (= :diagnosis/llm-response-invalid
             (thrown-error-type #(diag/diagnose d p)))))))

(deftest non-ok-dispatch-result-fails-loud
  (testing "a dispatch result with a non-ok :result/status is a typed
            :diagnosis/llm-failed, never mistaken for model output"
    (let [mc (fn [& _] {:result/status :provider/not-found
                        :error/type :provider/not-found
                        :value nil})
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack [] {:selected 0})
          e (try (diag/diagnose d p) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :diagnosis/llm-failed (:error/type (ex-data e))))
      (is (= :provider/not-found
             (get-in (ex-data e) [:result/status]))))))
;; --- constructor config ---

(deftest config-validation
  (testing "unknown config keys are rejected (closed map)"
    (is (= :diagnosis/config-invalid
           (thrown-error-type #(llm/llm-diagnostician
                                {:model-call identity :model/id "m"
                                 :store {:cas "/tmp"}})))))
  (testing "missing :model-call is rejected"
    (is (= :diagnosis/config-invalid
           (thrown-error-type #(llm/llm-diagnostician {:model/id "m"})))))
  (testing "missing :model/id is rejected"
    (is (= :diagnosis/config-invalid
           (thrown-error-type #(llm/llm-diagnostician {:model-call identity})))))
  (testing "non-map config is rejected"
    (is (= :diagnosis/config-invalid
           (thrown-error-type #(llm/llm-diagnostician :not-a-map)))))
  (testing "defaults apply for optional keys"
    (let [d (llm/llm-diagnostician {:model-call identity :model/id "m"})]
      (is (satisfies? diag/Diagnostician d)))))

;; --- empty pack and zero-hypotheses ---

(deftest empty-episodes-pack
  (testing "an empty pack still yields a valid Diagnosis; the model may
            return zero hypotheses and that is honest, not a failure"
    (let [[mc _] (canned-call (value-with (model-text [])))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack [] {:selected 0 :successes 0 :failures 0})
          diagnosis (diag/diagnose d p)]
      (is (= (:evidence/id p) (:evidence/id diagnosis)))
      (is (empty? (:hypotheses diagnosis)))
      (is (map? (ds/validate-diagnosis diagnosis))))))

(deftest zero-hypotheses-from-full-pack
  (testing "a non-empty pack diagnosed to zero hypotheses is valid"
    (let [[mc _] (canned-call (value-with (model-text [])))
          d (llm/llm-diagnostician {:model-call mc :model/id "m"})
          p (pack episodes1 {:selected 1 :successes 0 :failures 1})
          diagnosis (diag/diagnose d p)]
      (is (empty? (:hypotheses diagnosis)))
      (is (map? (ds/validate-diagnosis diagnosis))))))

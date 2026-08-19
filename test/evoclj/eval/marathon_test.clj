(ns evoclj.eval.marathon-test
  "S3-5 — real-model marathon timings.

  Tests use a fake provider (reify Provider) that simulates controlled
  latency and returns deterministic usage, so every test is fast and
  deterministic. No real network calls."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.eval.marathon :as marathon]
            [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]))

;; --- fake provider -----------------------------------------------------------

(defn- fake-provider
  "A Provider that simulates controlled latency and returns deterministic
  usage.

  opts:
    :latency-ms   — sleep this many ms per call (default 0)
    :usage        — map {:model-input-tokens n :model-output-tokens n
                        :model-reasoning-tokens n} returned on every call
    :fail-every   — throw on every Nth call (1-based); nil means never
    :fail-error   — the typed error to throw when :fail-every hits
  Returns a reified Provider."
  ([]
   (fake-provider {}))
  ([{:keys [latency-ms usage fail-every fail-error]
     :or {latency-ms 0
          usage {:model-input-tokens 10
                 :model-output-tokens 20
                 :model-reasoning-tokens 5}
          fail-every nil
          fail-error (err/error :provider/transient-error
                                "simulated transient failure" {})}}]
   (let [counter (atom 0)]
     (reify proto/Provider
       (describe [_]
         {:tool/id :model/fake
          :effect :model-call
          :input-schema [:map [:model/id keyword?]]
          :output-schema [:map]
          :required-action :invoke
          :retry {:safe? true}})
       (normalize-request [_ intent]
         {:model/id (get-in intent [:payload :model/id])
          :resource {:kind :model :id (get-in intent [:payload :model/id])}
          :request {:model/id (get-in intent [:payload :model/id])
                    :messages []
                    :options {}}})
       (execute-request! [_ _]
         (swap! counter inc)
         (when (and fail-every (= (mod @counter fail-every) 0))
           (throw fail-error))
         (when (pos? latency-ms)
           (Thread/sleep latency-ms))
         {:model/output {:text "hello" :reasoning "thinking..."}
          :usage usage
          :model-cost-units 0.1})))))

;; --- tests --------------------------------------------------------------------

(deftest shape-test
  (testing "run-marathon! returns the expected shape"
    (let [p (fake-provider)
          result (marathon/run-marathon! p :fake/model [{:role :user :content "hi"}]
                                        {:n 3})]
      (is (keyword? (:model/id result)) "model/id is a keyword")
      (is (= 3 (:trials result)) "trials matches n")
      (is (number? (:mean-latency-ms result)) "mean-latency-ms is a number")
      (is (number? (:p99-latency-ms result)) "p99-latency-ms is a number")
      (is (number? (:mean-input-tokens result)) "mean-input-tokens is a number")
      (is (number? (:mean-output-tokens result)) "mean-output-tokens is a number")
      (is (number? (:errors result)) "errors is a number")
      (is (zero? (:errors result)) "no errors on a healthy provider"))))

(deftest latency-statistics-test
  (testing "mean and p99 are computed correctly with controlled delays"
    (let [p (fake-provider {:latency-ms 0})
          result (marathon/run-marathon! p :fake/model [{:role :user :content "hi"}]
                                        {:n 5})]
      (is (<= 0 (:mean-latency-ms result)) "mean latency is non-negative")
      (is (<= 0 (:p99-latency-ms result)) "p99 latency is non-negative")
      (is (<= (:mean-latency-ms result) (:p99-latency-ms result))
          "p99 >= mean for non-empty samples"))))

(deftest token-aggregation-test
  (testing "token counts are aggregated across trials"
    (let [p (fake-provider {:usage {:model-input-tokens 10
                                     :model-output-tokens 20
                                     :model-reasoning-tokens 5}})
          result (marathon/run-marathon! p :fake/model [{:role :user :content "hi"}]
                                        {:n 4})]
      (is (= 10.0 (:mean-input-tokens result)) "mean input tokens correct")
      (is (= 20.0 (:mean-output-tokens result)) "mean output tokens correct")
      (is (= 5.0 (:mean-reasoning-tokens result)) "mean reasoning tokens correct"))))

(deftest token-aggregation-without-reasoning-test
  (testing "mean-reasoning-tokens is omitted when no trial reports it"
    (let [p (fake-provider {:usage {:model-input-tokens 10
                                     :model-output-tokens 20}})
          result (marathon/run-marathon! p :fake/model [{:role :user :content "hi"}]
                                        {:n 2})]
      (is (= 10.0 (:mean-input-tokens result)))
      (is (= 20.0 (:mean-output-tokens result)))
      (is (nil? (:mean-reasoning-tokens result))
          "reasoning tokens are nil when absent"))))

(deftest error-counting-test
  (testing "errors are counted without throwing"
    (let [p (fake-provider {:latency-ms 0
                            :fail-every 2
                            :fail-error (err/error :provider/transient-error
                                                   "simulated transient failure" {})})
          result (marathon/run-marathon! p :fake/model [{:role :user :content "hi"}]
                                        {:n 4})]
      (is (= 2 (:errors result)) "two failures out of four trials")
      (is (= 4 (:trials result)) "all four trials were attempted"))))

(deftest default-n-is-three-test
  (testing "default n is 3"
    (let [p (fake-provider {:latency-ms 0})
          result (marathon/run-marathon! p :fake/model [{:role :user :content "hi"}])]
      (is (= 3 (:trials result))))))

(deftest validation-test
  (testing "validation errors are thrown as typed ExceptionInfo"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"provider is required"
          (marathon/run-marathon! nil :fake/model [])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"model-id must be a keyword"
          (marathon/run-marathon! (fake-provider) "not-a-kw" [])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"messages must be a vector"
          (marathon/run-marathon! (fake-provider) :fake/model "not-a-vector")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"n must be a positive integer"
          (marathon/run-marathon! (fake-provider) :fake/model [] {:n 0})))))

(deftest all-failures-test
  (testing "all failures yields zero means and error count equals trials"
    (let [p (fake-provider {:latency-ms 0
                            :fail-every 1
                            :fail-error (err/error :provider/model-error
                                                   "always fails" {})})
          result (marathon/run-marathon! p :fake/model [{:role :user :content "hi"}]
                                        {:n 3})]
      (is (= 3 (:errors result)))
      (is (= 0.0 (:mean-latency-ms result)))
      (is (= 0.0 (:mean-input-tokens result)))
      (is (= 0.0 (:mean-output-tokens result)))
      (is (nil? (:mean-reasoning-tokens result))))))

(deftest p99-with-single-trial-test
  (testing "p99 with a single trial equals the trial latency"
    (let [p (fake-provider {:latency-ms 0})
          result (marathon/run-marathon! p :fake/model [{:role :user :content "hi"}]
                                        {:n 1})]
      (is (= 1 (:trials result)))
      (is (= (:mean-latency-ms result) (:p99-latency-ms result))
          "single trial: mean == p99"))))

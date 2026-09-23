(ns evoclj.runtime.assembler-test
  "RequestAssembler tests: the optional context-compression seam (P10).

  The assembler may be handed a Compacter (evoclj.context.compression)
  through opts; a string history is then run through it and replaced by
  the compressed envelope prefix — but ONLY when the compacter reports a
  FIRED trigger. Without a compacter the assembly is byte-identical to
  the shipped behavior, so a host that injects none pays nothing."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.context.compression.apply :as compression-apply]
            [evoclj.context.compression.compacter :as compacter]
            [evoclj.context.compression.envelope :as envelope]
            [evoclj.runtime.assembler :as assembler]))

;; --- fixtures ----------------------------------------------------------------

(defn- base-call []
  {:base/messages [{:role :system :content "KERNEL: obey"}
                   {:role :user :content "do the task"}]
   :requested-tools []
   :options {}})

(defn- sample-envelope []
  (envelope/make-envelope
   {:task {:task/id "t1" :task/status :completed :task/description "compressed"}
    :subgoals []
    :residue []
    :evidence []
    :version 1
    :created-at "2026-01-01T00:00:00Z"
    :window {:window/from 0 :window/to 10}
    :tokens-before 100
    :tokens-after 50
    :compressor {:compressor/model "stub"
                 :compressor/prompt "p"}}))

(defn- stub-compacter
  "A Compacter that reports `compressed?` in its trigger and echoes the
  context it was handed (so a test can prove what reached it)."
  [compressed? seen]
  (reify compacter/Compacter
    (compress [_ context opts]
      (swap! seen conj {:context context :opts opts})
      (cond-> {:envelope (sample-envelope)
               :footer "stub footer"}
        (some? compressed?)
        (assoc :trigger {:trigger/compressed? compressed?
                         :trigger/reason (if compressed? :threshold :none)})))))

(defn- history-text
  "The history the assembled manifest carries (:text for a string
  history, :messages otherwise)."
  [prepared]
  (get-in prepared [:context/manifest :history]))

;; --- the shipped path is untouched -------------------------------------------

(deftest no-compacter-keeps-the-shipped-assembly
  (let [plain (assembler/base->prepared (base-call) [] {} nil "history text" {})
        explicit-nil (assembler/base->prepared (base-call) [] {} nil "history text"
                                               {:compacter nil})
        via-wrapper (assembler/assemble (base-call) {:catalog {}
                                                     :history "history text"})]
    (testing "the history passes through unchanged"
      (is (= {:text "history text"} (history-text plain)))
      (is (= {:text "history text"} (history-text explicit-nil)))
      (is (= {:text "history text"} (history-text via-wrapper))))
    (testing "an explicit nil compacter is identical to omitting the key"
      ;; the tool-catalog binding carries a fresh id/timestamp per pin, so
      ;; compare the assembled PAYLOAD (messages, tools, model request,
      ;; provenance, manifest history) rather than the binding identity.
      (is (= (select-keys plain [:messages :tools :tool-map :model/request
                                 :prompt/provenance])
             (select-keys explicit-nil [:messages :tools :tool-map
                                        :model/request :prompt/provenance])))
      (is (= (get-in plain [:context/manifest :history])
             (get-in explicit-nil [:context/manifest :history]))))))

(deftest empty-and-non-string-histories-are-never-compressed
  (let [seen (atom [])
        fired (stub-compacter true seen)]
    (testing "an empty history is left alone"
      (let [prepared (assembler/base->prepared (base-call) [] {} nil ""
                                               {:compacter fired})]
        (is (= {:text ""} (history-text prepared)))
        (is (empty? @seen) "the compacter was never invoked")))
    (testing "a message-vector history is left alone"
      (let [messages [{:role :user :content "hi"}]
            prepared (assembler/base->prepared (base-call) [] {} nil messages
                                               {:compacter fired})]
        (is (= {:messages messages} (history-text prepared)))
        (is (empty? @seen))))))

;; --- the seam fires only on a reported trigger --------------------------------

(deftest fired-compacter-substitutes-the-envelope-prefix
  (let [seen (atom [])
        fired (stub-compacter true seen)
        prepared (assembler/base->prepared (base-call) [] {} nil "raw history"
                                           {:compacter fired
                                            :compacter/opts {:token-threshold 7}})]
    (testing "the compacter saw the history and its own opts"
      (is (= [{:context "raw history" :opts {:token-threshold 7}}] @seen)))
    (testing "the history becomes the serialized envelope prefix"
      (let [prefix (history-text prepared)]
        (is (= (compression-apply/envelope-prefix (sample-envelope))
               (:text prefix))
            "the substituted text is exactly apply/envelope-prefix of the envelope")
        (is (not (str/includes? (:text prefix) "raw history")))))))

(deftest unfired-compacter-preserves-the-history
  (testing "a compacter reporting an unfired trigger changes nothing"
    (let [seen (atom [])
          unfired (stub-compacter false seen)
          prepared (assembler/base->prepared (base-call) [] {} nil "raw history"
                                             {:compacter unfired})]
      (is (= {:text "raw history"} (history-text prepared)))
      (is (= 1 (count @seen)) "it still ran (the trigger is its own decision)")))
  (testing "a compacter that reports NO trigger at all cannot rewrite history"
    (let [seen (atom [])
          silent (stub-compacter nil seen)
          prepared (assembler/base->prepared (base-call) [] {} nil "raw history"
                                             {:compacter silent})]
      (is (= {:text "raw history"} (history-text prepared))))))

;; --- the scheduler-facing wrapper forwards the injection ----------------------

(deftest assemble-wrapper-forwards-the-compacter
  (let [seen (atom [])
        fired (stub-compacter true seen)
        prepared (assembler/assemble (base-call)
                                     {:catalog {}
                                      :history "raw history"
                                      :compacter fired
                                      :compacter/opts {:token-threshold 3}})]
    (is (= 1 (count @seen)) "the wrapper handed the compacter through")
    (is (= {:token-threshold 3} (:opts (first @seen))))
    (is (not= {:text "raw history"} (history-text prepared))
        "the fired compacter's envelope prefix replaced the history")))

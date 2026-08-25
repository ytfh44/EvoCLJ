(ns evoclj.binding.m21-consolidation-test
  "M21 closure-repair tests for evoclj.binding.call:
   - a SINGLE canonical :binding/* (+ :revision/*) key set for CallBinding records
     (no scattered :contract/* / :mcp/* duplicates stored in the record; INV-05)
   - a UNIFIED stale? arity (canonical [x] and [x freshness]; the 3-arity
     overload with inconsistent logic is removed)
   - the dead coerce-revision-seq helper is deleted AND the ClassCastException
     regression (int coercion of a non-int revision/seq) is gone: a non-int
     revision/seq now fails closed with a typed error instead of CCE.

   These tests exercise production paths (capture-tool-binding, stale?,
   validate-binding) — no injected fns, no shape-only assertions."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.binding.call :as binding]
            [evoclj.mcp.contract :as contract]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]))

(defn- threw?
  "True when thunk throws an instance of ex-class. Avoids the clojure.test
  thrown? macro scoping so the assertion is plain behavior, not a macro."
  [ex-class thunk]
  (try
    (thunk)
    false
    (catch Throwable t
      (instance? ex-class t))))

;; Canonical key set: the ONLY keys a CallBinding record may carry.
(def ^:private canonical-binding-keys
  #{:binding/id :tool/id :revision/id :revision/seq :source/id
    :binding/descriptor :binding/provider :binding/freshness
    :binding/stale? :binding/captured-at})

(defn- capture-sample
  "Build a CallBinding through the production capture path from a fixture provider."
  ([]
   (capture-sample {}))
  ([opts]
   (let [p (fixture/echo-provider {})]
     (binding/capture-tool-binding p (merge {:freshness :best-effort :revision/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :revision/seq 5} opts)))))

;; ---------------------------------------------------------------------------
;; Happy path: canonical key set + stale? works at the unified arity
;; ---------------------------------------------------------------------------

(deftest binding-record-uses-canonical-key-set-only
  (testing "a captured CallBinding contains exactly the canonical :binding/* + :revision/* keys (no :contract/* / :mcp/* duplicates)"
    (let [b (capture-sample)]
      (is (= canonical-binding-keys (set (keys b)))
          "binding record must carry ONLY canonical keys")
      (is (not-any? (fn [k] (or (= (namespace k) "contract")
                                (= (namespace k) "mcp")))
                    (keys b))
          "no :contract/* or :mcp/* alias keys leak into the record")
      (is (binding/validate-binding b) "canonical record still validates against CallBindingSchema"))))

(deftest stale-works-at-unified-arity
  (testing "stale? at the canonical 2-arity honours explicit override and compat keys"
    (let [fresh {:tool/id :x :effect :pure :input-schema :any :output-schema :any :required-action :invoke :mcp/last-refreshed (System/currentTimeMillis)}
          stale {:tool/id :x :effect :pure :input-schema :any :output-schema :any :required-action :invoke :mcp/last-refreshed nil :binding/stale? true}
          plain {:tool/id :x :effect :pure :input-schema :any :output-schema :any :required-action :invoke}]
      (is (false? (binding/stale? fresh :best-effort)))
      (is (true? (binding/stale? stale :best-effort)) "explicit :binding/stale? true honoured at 2-arity")
      (is (false? (binding/stale? stale :pinned)) "pinned never stale")
      (is (true? (binding/stale? plain :best-effort)) "no provenance => stale (conservative)")
      ;; 1-arity default
      (is (false? (binding/stale? fresh)) "1-arity defaults to :best-effort and is not stale"))))

;; ---------------------------------------------------------------------------
;; New branch 1: canonical set is the single source of truth and is projected
;; through the compat audit map (the only sanctioned surface for :contract/*/:mcp/*)
;; ---------------------------------------------------------------------------

(deftest compat-aliases-live-only-in-audit-projection
  (testing "MCP/contract compat keys appear via binding->audit, not stored in the record"
    (let [b (capture-sample)
          audit (binding/binding->audit b)]
      (is (false? (contains? b :contract/stale?)) "record has no :contract/stale?")
      (is (false? (contains? b :mcp/stale?)) "record has no :mcp/stale?")
      (is (false? (contains? b :contract/generation)) "record has no :contract/generation")
      (is (false? (contains? b :mcp/generation)) "record has no :mcp/generation")
      ;; the audit projection is the sanctioned compat surface
      (is (= (:binding/stale? b) (:contract/stale? audit)) "audit projects :contract/stale? from canonical")
      (is (= (:binding/stale? b) (:mcp/stale? audit)) "audit projects :mcp/stale? from canonical")
      (is (= (:revision/seq b) (:contract/generation audit)) "audit projects :contract/generation from canonical")
      (is (= (:revision/seq b) (:mcp/generation audit)) "audit projects :mcp/generation from canonical"))))

(deftest contract-wrapper-projection-keeps-compat
  (testing "evoclj.mcp.contract is a thin wrapper: its record carries compat :contract/* / :mcp/* aliases derived from the canonical binding (not a duplicate source of truth)"
    (let [desc {:tool/id :test/c :effect :pure :input-schema :any :output-schema :any :required-action :invoke :mcp/generation 3 :mcp/last-refreshed (System/currentTimeMillis)}
          c (contract/capture desc nil nil :best-effort {})
          audit (contract/contract->audit c)]
      ;; the wrapper decorates with compat alias keys (the sanctioned compat surface)
      (is (= 3 (:contract/generation c)) "wrapper projects :contract/generation from canonical :revision/seq")
      (is (= 3 (:mcp/generation c)) "wrapper projects :mcp/generation from canonical :revision/seq")
      (is (= (:binding/stale? c) (:contract/stale? c)) "wrapper projects :contract/stale? from canonical")
      ;; and the canonical binding underneath is clean
      (is (= canonical-binding-keys (set (keys (binding/capture-tool-binding desc {:freshness :best-effort}))))
          "the underlying canonical CallBinding record carries only canonical keys")
      ;; audit projection also exposes compat
      (is (= 3 (:contract/generation audit)) "compat generation via audit")
      (is (= 3 (:mcp/generation audit)) "compat generation via audit"))))

;; ---------------------------------------------------------------------------
;; New branch 2: stale? arity unified — 3-arity overload removed
;; ---------------------------------------------------------------------------

(deftest stale-has-exactly-unified-arity
  (testing "stale? exposes only the canonical arities; a 3-arg call is a clear arity error"
    ;; 2-arity and 1-arity are valid
    (is (false? (binding/stale? {:mcp/last-refreshed (System/currentTimeMillis)} :best-effort)))
    (is (false? (binding/stale? {:mcp/last-refreshed (System/currentTimeMillis)})))
    ;; 3-arity must NOT silently dispatch to an inconsistent overload
    (is (threw? clojure.lang.ArityException
                #(binding/stale? {:mcp/last-refreshed (System/currentTimeMillis)} :best-effort 5))
        "3-arity stale? must be rejected (arity error), not an inconsistent overload")))

;; ---------------------------------------------------------------------------
;; New branch 3: coerce dead code deleted + ClassCastException regression gone
;; ---------------------------------------------------------------------------

(deftest revision-seq-non-int-fails-closed-not-cce
  (testing "a non-integer :revision/seq fails closed with a typed error, not ClassCastException"
    (let [p (fixture/echo-provider {})]
      (try
        (binding/capture-tool-binding p {:freshness :best-effort :revision/seq "5"})
        (is false "expected capture to fail-closed on a non-int revision/seq")
        (catch clojure.lang.ExceptionInfo e
          (is (= :binding/invalid-revision-seq (:error/type (ex-data e)))
              "typed error :binding/invalid-revision-seq, not a raw ClassCastException")
          (is (some? (:value (ex-data e)))))
        (catch ClassCastException _
          (is false "ClassCastException regression must be eliminated")))))
  (testing "a valid integer revision/seq is accepted and stored as canonical int"
    (let [b (capture-sample {:revision/seq (int 9)})]
      (is (= 9 (:revision/seq b)))
      (is (int? (:revision/seq b))))))

;; ---------------------------------------------------------------------------
;; Fault case 1: unknown binding key rejected fail-closed
;; ---------------------------------------------------------------------------

(deftest unknown-binding-key-rejected-fail-closed
  (testing "CallBindingSchema rejects a record carrying an unknown/extra key"
    (let [b (capture-sample)
          polluted (assoc b :binding/bogus "x")]
      (is (threw? clojure.lang.ExceptionInfo
                   #(binding/validate-binding polluted))
          "extra unknown :binding/* key must fail validation fail-closed"))))

;; ---------------------------------------------------------------------------
;; Fault case 2: stale? at wrong arity fails clearly (covered above); also
;; an invalid freshness value fails closed through capture
;; ---------------------------------------------------------------------------

(deftest invalid-freshness-fails-closed
  (testing "capture rejects an invalid freshness value fail-closed"
    (let [p (fixture/echo-provider {})]
      (is (threw? clojure.lang.ExceptionInfo
                   #(binding/capture-tool-binding p {:freshness :bogus}))
          "invalid freshness must throw typed error"))))

;; ---------------------------------------------------------------------------
;; Regression: old scattered coerce + duplicate keys are gone (dead-code audit)
;; ---------------------------------------------------------------------------

(deftest dead-coerce-helper-and-scatter-removed
  (testing "the dead coerce-revision-seq helper no longer exists and binding records carry no aliases"
    ;; grep-level guarantee: the production source must not define coerce-revision-seq
    (let [src (slurp "src/evoclj/binding/call.clj")]
      (is (not (str/includes? src "coerce-revision-seq"))
          "dead coerce-revision-seq helper must be deleted from evoclj.binding.call")
      ;; The capture-tool-binding record literal must not ASSIGN :contract/* / :mcp/* alias keys.
      ;; (alias keys are legitimately read as compat INPUT and projected by binding->audit /
      ;;  attach-audit-to-result, so we match only the stored-assignment patterns.)
      (is (not (str/includes? src ":contract/id binding-id"))
          "no :contract/id alias assignment in the capture-tool-binding record")
      (is (not (str/includes? src ":mcp/generation (int revision-seq)"))
          "no :mcp/generation alias assignment in the capture-tool-binding record")
      (is (not (str/includes? src ":contract/stale? (boolean stale)"))
          "no :contract/stale? alias assignment in the capture-tool-binding record")
      (is (not (str/includes? src ":mcp/stale? (boolean stale)"))
          "no :mcp/stale? alias assignment in the capture-tool-binding record")))
    ;; behavioral confirmation: capturing then projecting yields no stray aliases on the record
    (is (= canonical-binding-keys (set (keys (capture-sample))))))

;; ---------------------------------------------------------------------------
;; Doc / behavior consistency
;; ---------------------------------------------------------------------------

(deftest doc-behavior-consistency
  (testing "stale? canonical arities match the documented contract and generation prefers :revision/seq"
    ;; generation helper prefers canonical :revision/seq over compat aliases (single source of truth)
    (is (= 5 (binding/generation {:revision/seq 5 :mcp/generation 99 :contract/generation 99}))
        "generation must read canonical :revision/seq, ignoring stale compat aliases")
    ;; stale? 1-arity is documented to default to :best-effort
    (is (= (binding/stale? {:mcp/last-refreshed nil} :best-effort)
           (binding/stale? {:mcp/last-refreshed nil})))))

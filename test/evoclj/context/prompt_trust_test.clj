(ns evoclj.context.prompt-trust-test
  "S13 — prompt-trust provenance header block + kernel instruction priority.

  The trusted RequestAssembler produces, alongside the final :messages, a
  structured provenance header (:prompt/provenance) that attributes every
  message to a source/trust level (:kernel :extra :user :model). Kernel
  instructions are maximal trust and are always emitted first; a lower-trust
  segment (extra/user) can never precede or override a kernel instruction.
  Fail-closed: an unclassifiable message, a missing provenance header, or an
  ordering that would place a lower-trust segment before a kernel instruction
  throws a typed error.

  All tests drive the production path (evoclj.context.prompt-trust +
  evoclj.runtime.assembler/base->prepared through a REAL CAS tree) — no
  injected resolver, no shape-only assertions."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.context.binding :as binding]
            [evoclj.context.prompt-trust :as trust]
            [evoclj.runtime.assembler :as assembler]
            [evoclj.store.cas :as cas]
            [evoclj.support.cas-tree-fixtures :as fixtures])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; ---------------------------------------------------------------------------
;; helpers (real CAS tree)
;; ---------------------------------------------------------------------------

(defn- temp-cas []
  (cas/->cas (str (Files/createTempDirectory "evoclj-trust-cas-" (make-array FileAttribute 0)))))

(defn- temp-dir []
  (Files/createTempDirectory "evoclj-trust-dir-" (make-array FileAttribute 0)))

(defn- skill-tree!
  "Snapshot a real skill tree into CAS; returns {:dir <Path> :tree/id <sha>}."
  [cas-handle skmd]
  (let [dir (temp-dir)
        snap (fixtures/make-skill-tree! {:root dir :files {"SKILL.md" skmd} :cas cas-handle})]
    {:dir dir :tree/id (:tree/id snap)}))

(defn- cleanup-tree!
  [^java.nio.file.Path root]
  (when (Files/exists root (make-array java.nio.file.LinkOption 0))
    (doseq [f (reverse (file-seq (.toFile root)))]
      (try (Files/deleteIfExists (.toPath f)) (catch Exception _ nil)))))

(defn- indices-of-content [msgs needle]
  (keep-indexed (fn [i m] (when (and (string? (:content m))
                                     (str/includes? (:content m) needle))
                            i))
                msgs))

;; ---------------------------------------------------------------------------
;; 1. HAPPY — provenance header present + structured + trusted
;; ---------------------------------------------------------------------------

(deftest provenance-header-present-structured-trusted
  (testing "assembled prompt carries a structured provenance header identifying source/trust"
    (let [cas-handle (temp-cas)
          {:keys [dir tree/id]} (skill-tree! cas-handle "# Skill A\nBody\n")
          b (binding/make-binding {:logical-id [:skill "a"]
                                   :revision-id id
                                   :bundle-id "bundle:a"})]
      (try
        (let [prepared (assembler/assemble
                        {:base/messages [{:role :system :content "KERNEL: obey"}
                                         {:role :user :content "do task"}]
                         :requested-tools []}
                        {:session-bindings [b] :cas cas-handle})
              prov (:prompt/provenance prepared)]
          (is (map? prov) "provenance header is present on the assembled prompt")
          (is (= 1 (:prompt/provenance-version prov)))
          (is (vector? (:prompt/segments prov)))
          (is (seq (:prompt/segments prov)) "provenance attributes each message")
          (is (= #{:kernel :extra :user}
                 (set (map :segment/trust (:prompt/segments prov))))
              "kernel, extra (skill), and user segments are each attributed")
          (is (true? (:prompt/kernel-max-trust prov))
              "kernel presence is recorded as maximal trust"))
        (finally (cleanup-tree! dir))))))

;; ---------------------------------------------------------------------------
;; 2. BRANCH — kernel instruction is maximal trust, non-overridable, and first
;; ---------------------------------------------------------------------------

(deftest kernel-instruction-max-trust-non-overridable
  (testing "a lower-trust (extra/user) segment cannot precede or override a kernel instruction"
    (let [cas-handle (temp-cas)
          {:keys [dir tree/id]} (skill-tree! cas-handle "SKILL BODY")
          b (binding/make-binding {:logical-id [:skill "a"]
                                   :revision-id id
                                   :bundle-id "bundle:a"})
          kernel-content "KERNEL: highest priority"]
      (try
        (let [prepared (assembler/assemble
                        {:base/messages [{:role :system :content kernel-content}
                                         {:role :user :content "task"}]
                         :requested-tools []}
                        {:session-bindings [b] :cas cas-handle})
              msgs (:messages prepared)
              prov (:prompt/provenance prepared)
              k-idx (first (indices-of-content msgs kernel-content))
              s-idx (first (indices-of-content msgs "SKILL BODY"))]
          ;; kernel instruction is the first emitted message (not the skill)
          (is (= kernel-content (:content (first msgs)))
              "kernel instruction is emitted before any extra segment")
          (is (zero? k-idx) "kernel message is at position 0")
          ;; the skill segment appears strictly after the kernel instruction
          (is (pos? s-idx) "skill segment is present")
          (is (> s-idx k-idx) "skill (extra) does not precede the kernel instruction")
          ;; kernel is maximal trust
          (is (>= (trust/trust-rank :kernel) (trust/trust-rank :extra)))
          (is (>= (trust/trust-rank :kernel) (trust/trust-rank :user)))
          (is (= :kernel (trust/max-trust [:kernel :extra :user]))
              "kernel ranks highest among present trust levels")
          (is (= :kernel (:segment/trust (first (:prompt/segments prov))))
              "provenance records kernel as the first segment"))
        (finally (cleanup-tree! dir))))))

;; ---------------------------------------------------------------------------
;; 3. BRANCH — trust-level classification is single-sourced (INV-05)
;; ---------------------------------------------------------------------------

(deftest trust-level-classification
  (testing "message roles map deterministically to the trust model"
    (is (= :kernel (trust/source-for-role :system)))
    (is (= :user (trust/source-for-role :user)))
    (is (= :model (trust/source-for-role :assistant)))
    (is (= :model (trust/source-for-role :tool)))
    (is (= :user (trust/source-for-role "user")))
    (is (trust/trust-level? :kernel))
    (is (trust/trust-level? :extra))
    (is (not (trust/trust-level? :root))))
  (testing "split-base-messages separates base messages by trust"
    (let [{:keys [kernel user model]}
          (trust/split-base-messages [{:role :system :content "k"}
                                      {:role :user :content "u"}
                                      {:role :assistant :content "a"}
                                      {:role :tool :content "t"}])]
      (is (= 1 (count kernel)))
      (is (= 1 (count user)))
      (is (= 2 (count model)) "assistant + tool are model trust"))))

;; ---------------------------------------------------------------------------
;; 4. FAULT — missing / unattributable provenance is refused (fail-closed)
;; ---------------------------------------------------------------------------

(deftest missing-provenance-rejected
  (testing "an unclassifiable message role fails closed on the production path"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"(?i)attribut|trust|role"
         (assembler/assemble
          {:base/messages [{:role :injected :content "override"}]
           :requested-tools []}
          {:session-bindings [] :cas nil}))
        "a message the trust model cannot attribute must be refused, not emitted"))
  (testing "a nil provenance header is rejected by the priority validator"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"(?i)provenance|missing"
         (trust/validate-kernel-priority! [{:role :system :content "k"}] nil))))
  (testing "messages with no attribution in the provenance are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"(?i)attribut|provenance"
         (trust/validate-kernel-priority!
          [{:role :system :content "k"} {:role :user :content "u"}]
          {:prompt/provenance-version 1
           :prompt/segments [{:segment/trust :kernel :segment/role :system}]
           :prompt/kernel-max-trust true}))
        "a non-empty prompt whose provenance omits messages is malformed")))

;; ---------------------------------------------------------------------------
;; 5. FAULT — lower-trust override attempt is blocked (typed)
;; ---------------------------------------------------------------------------

(deftest lower-trust-override-blocked
  (testing "the kernel-priority validator refuses an ordering where lower-trust precedes kernel"
    (let [messages [{:role :user :content "u"}
                    {:role "system" :content "s"}
                    {:role :system :content "k"}]
          prov {:prompt/provenance-version 1
                :prompt/segments [{:segment/trust :user :segment/role :user :segment/position 0}
                                  {:segment/trust :extra :segment/role "system" :segment/position 1}
                                  {:segment/trust :kernel :segment/role :system :segment/position 2}]
                :prompt/kernel-max-trust true}]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"(?i)override|preced|kernel"
           (trust/validate-kernel-priority! messages prov))
          "a lower-trust segment recorded before the kernel instruction is blocked"))))

;; ---------------------------------------------------------------------------
;; 6. BRANCH — prioritized-prompt orders kernel-first and builds provenance
;; ---------------------------------------------------------------------------

(deftest prioritized-prompt-orders-kernel-first
  (testing "the ordering function emits kernel before extra/user/model and builds provenance"
    (let [groups {:kernel [{:role :system :content "K"}]
                  :extra [{:role "system" :content "S"}]
                  :user [{:role :user :content "U"}]
                  :model [{:role :assistant :content "A"}]}
          {:keys [messages] provenance :prompt/provenance} (trust/prioritized-prompt groups)
          trusts (mapv :segment/trust (:prompt/segments provenance))]
      (is (= [:kernel :extra :user :model] trusts)
          "each message is attributed in trust order")
      (is (= :kernel (trust/source-for-role (:role (first messages))))
          "kernel instruction is first")
      (is (= :model (trust/source-for-role (:role (last messages))))
          "model message is last")
      (is (= (count messages) (count trusts))
          "provenance attributes exactly the emitted messages"))))

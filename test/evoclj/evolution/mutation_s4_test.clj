(ns evoclj.evolution.mutation-s4-test
  "S4 — Manifest cannot be ValidatedMutation (file String, then validate).

  ValidatedMutation is the validated right (MutableAssetRef + VerifiedDigest)
  ONLY constructible via validate-mutation (sealed private constructor +
  private validated marker). RawMutation is the current IR (file : String).
  patch/apply-mutation ONLY accepts sealed ValidatedMutation and rejects
  RawMutation without re-validation.

  This test proves manifest.edn can never become a ValidatedMutation,
  that a fake defrecord without the sealed marker is NOT validated,
  and that the ValidatedMutation carries MutableAssetRef and VerifiedDigest."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.helpers :as h]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.genome.patch :as patch]
            [evoclj.genome.load :as load]
            [evoclj.genome.hash :as hash])
  (:import [java.nio.file Files Path Paths]
           [java.nio.file.attribute FileAttribute]
           [java.nio.charset StandardCharsets]
           [java.util UUID]))

;; temp helpers collapsed to evoclj.helpers
(def ^:private temp-dir! h/temp-dir!)
(def ^:private write-text! h/write-text!)

(defn- write-genome! [dir]
  (let [manifest (pr-str {:genome/format 1
                          :agent/id :main
                          :agent/entry :graph/main
                          :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
                          :modules {:topology "topology.edn"
                                    :models "models.edn"
                                    :memory "memory.edn"
                                    :evolution "evolution.edn"}
                          :capabilities/requested #{:model/call}
                          :evolution {:max-risk :program :mutable #{:skills}}
                          :metadata {:name "test"}})]
    (write-text! dir "manifest.edn" manifest)
    (write-text! dir "topology.edn" "{:graph/id :graph/main :entry :node/a :nodes {:node/a {:node/type :emit}} :limits {:max-steps 10}}")
    (write-text! dir "models.edn" "{:models {}}")
    (write-text! dir "memory.edn" "{:memory {}}")
    (write-text! dir "evolution.edn" "{:evolution {}}")
    (write-text! dir "skills/debugging.edn" "{:workflow {:before-edit []}}")
    dir))

(def ^:private hex64 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
(def ^:private genome-id (str "sha256:" hex64))
(def ^:private evidence-id (str "sha256:" (apply str (repeat 64 "e"))))
(def ^:private file-hash (str "sha256:" (apply str (repeat 64 "f"))))

(defn- base-mutation [file]
  {:mutation/id (UUID/fromString "00000000-0000-0000-0000-000000000001")
   :parent/genome-id genome-id
   :hypothesis/id (UUID/fromString "00000000-0000-0000-0000-000000000002")
   :evidence/id evidence-id
   :risk :behavioral
   :ops [{:op :set-edn :file file :path [:workflow :before-edit] :expect/hash file-hash :value [:new]}]
   :expected-effect {:primary-metric :task/success :direction :increase}})

(deftest manifest-edn-cannot-be-validated-mutation
  (testing "manifest.edn is kernel-protected and can never be a ValidatedMutation"
    (let [mut (base-mutation "manifest.edn")
          manifest {:evolution {:mutable #{:skills}} :modules {:topology "topology.edn" :models "models.edn" :memory "memory.edn" :evolution "evolution.edn"}}]
      (is (thrown? clojure.lang.ExceptionInfo (mutation/validate-mutation mut manifest))
          "validate-mutation must reject manifest.edn with :mutation/protected-path")
      (try
        (mutation/validate-mutation mut manifest)
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :mutation/protected-path (:error/type (ex-data e))))
          (is (= :kernel-file (:reason (ex-data e))))
          (is (not (mutation/validated-mutation? e)) "exception is not a ValidatedMutation"))))
    (testing "even direct construction via ->ValidatedMutation is NOT the validated right (only validate-mutation is)"
      (let [raw (base-mutation "manifest.edn")
            ;; Constructors are private and sealed via closure + private field — any fake
            ;; without the closed-over secret fails validated-mutation? (factory closes over secret)
            fake-ctor (try @(ns-resolve (find-ns 'evoclj.evolution.mutation) '->ValidatedMutation) (catch Exception _ nil))
            fake (try (when fake-ctor (fake-ctor raw [] [] [] nil))
                      (catch Exception _ nil))
            ;; map->ValidatedMutation does not exist for deftype (only defrecord had it) — forging via map fails
            fake2-var (try (ns-resolve (find-ns 'evoclj.evolution.mutation) 'map->ValidatedMutation) (catch Exception _ nil))
            fake2-instance (try (when fake2-var (@fake2-var {:raw-mutation raw :canonical-ops [] :asset-refs [] :verified-digests [] :secret nil}))
                                (catch Exception _ nil))]
        (is (not (mutation/validated-mutation? fake)) "fake via ->ValidatedMutation without sealed secret is NOT validated")
        (is (not (mutation/validated-mutation? fake2-instance)) "fake via map->ValidatedMutation (or missing) is NOT validated")
        (is (not (mutation/validated-mutation? {:raw-mutation raw :canonical-ops [] :asset-refs [] :verified-digests []}))
            "plain map is not a ValidatedMutation")
        ;; The real gate is that validate-mutation would have thrown for manifest.edn,
        ;; so a ValidatedMutation for manifest.edn should never be produced by the gate
        (is (thrown? clojure.lang.ExceptionInfo (mutation/validate-mutation raw {:evolution {:mutable #{:skills}} :modules {:evolution "evolution.edn"}}))
            "the gate still rejects manifest.edn")))))

(deftest validated-mutation-carries-mutable-asset-ref-and-verified-digest
  (testing "a valid skills file produces a ValidatedMutation with MutableAssetRef and VerifiedDigest"
    (let [mut (base-mutation "skills/debugging.edn")
          manifest {:evolution {:mutable #{:skills}} :modules {:topology "topology.edn" :models "models.edn" :memory "memory.edn" :evolution "evolution.edn"}}
          vm (mutation/validate-mutation mut manifest)]
      (is (mutation/validated-mutation? vm) "must be a ValidatedMutation")
      (is (not (mutation/raw-mutation? vm)) "ValidatedMutation is not RawMutation")
      (is (mutation/raw-mutation? (base-mutation "skills/debugging.edn")) "RawMutation is file String")
      (is (= 1 (count (:asset-refs vm))) "one MutableAssetRef per op")
      (is (instance? evoclj.evolution.mutation.MutableAssetRef (first (:asset-refs vm))))
      (is (= "skills/debugging.edn" (:canonical-path (first (:asset-refs vm)))))
      (is (= genome-id (:parent-genome-id (first (:asset-refs vm)))) "MutableAssetRef carries parent Genome")
      (is (= 1 (count (:verified-digests vm))))
      (is (instance? evoclj.evolution.mutation.VerifiedDigest (first (:verified-digests vm))))
      (is (= file-hash (:digest (first (:verified-digests vm)))) "VerifiedDigest carries canonical sha256"))))

(deftest patch-only-accepts-validated-mutation
  (testing "patch/apply-mutation ONLY accepts sealed ValidatedMutation; RawMutation is rejected without re-validation"
    (let [parent-dir (temp-dir!)
          out-dir (temp-dir!)
          _ (write-genome! parent-dir)
          parent (load/load-genome parent-dir)
          valid-raw {:mutation/id (UUID/randomUUID) :parent/genome-id (:genome/id parent) :hypothesis/id (UUID/randomUUID) :evidence/id evidence-id :risk :behavioral :ops [{:op :set-edn :file "skills/debugging.edn" :path [:workflow :before-edit] :expect/hash (hash/text-digest "{:workflow {:before-edit []}}") :value [:new]}] :expected-effect {:primary-metric :task/success :direction :increase}}
          validated (mutation/validate-mutation valid-raw parent)
          ;; ValidatedMutation should succeed
          candidate (patch/apply-mutation parent validated (.toString out-dir))]
      (is (some? candidate) "ValidatedMutation patches")
      (is (= (:genome/id parent) (:parent-genome-id (first (:asset-refs validated)))))
      ;; RawMutation is REJECTED without re-validation (definition > validation)
      (let [out2 (temp-dir!)]
        (is (thrown? clojure.lang.ExceptionInfo (patch/apply-mutation parent valid-raw (.toString out2)))
            "RawMutation must be rejected with :patch/mutation-invalid")
        (try (patch/apply-mutation parent valid-raw (.toString out2))
             (catch clojure.lang.ExceptionInfo e
               (is (= :patch/mutation-invalid (:error/type (ex-data e)))))))
      ;; Fake ValidatedMutation without sealed secret is also rejected by patch (closure + private field)
      (let [fake-ctor (try @(ns-resolve (find-ns 'evoclj.evolution.mutation) '->ValidatedMutation) (catch Exception _ nil))
            fake (try (when fake-ctor (fake-ctor valid-raw [] [] [] nil))
                      (catch Exception _ nil))]
        (when fake
          (is (not (mutation/validated-mutation? fake)) "fake is not validated")
          (is (thrown? clojure.lang.ExceptionInfo (patch/apply-mutation parent fake (.toString (temp-dir!))))
              "patch must reject fake ValidatedMutation")))
      ;; manifest.edn via ValidatedMutation should never be produced, and patch with raw manifest should fail
      (let [bad-raw (assoc-in valid-raw [:ops 0 :file] "manifest.edn")]
        (is (thrown? clojure.lang.ExceptionInfo (mutation/validate-mutation bad-raw parent)))
        (is (thrown? clojure.lang.ExceptionInfo (patch/apply-mutation parent bad-raw (.toString (temp-dir!))))
            "patch with manifest.edn must fail")))))
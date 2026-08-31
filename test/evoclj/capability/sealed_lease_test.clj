(ns evoclj.capability.sealed-lease-test
  "P1 — sealed CapabilityLease deftype: construct-time validated,
  positive-window asserted, projection round-trip.
  Mirrors Wolfram model leaseChecks [W-01..W-07]."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]
            [malli.core :as m]))

(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(def ^:private session-a #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")

(def ^:private issued-at (java.util.Date. 1700000000000))
(def ^:private expires-at (java.util.Date. 1700003600000))

(defn- base-map
  "Canonical well-formed lease map for P1 tests."
  []
  {:cap/id #uuid "11111111-1111-4111-8111-111111111111"
   :subject {:session/id session-a :phenotype/id phenotype-p1}
   :resource {:kind :tool :id :fixture/echo}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

(defn- throws-schema-invalid?
  "True when f throws with :error/type :capability/schema-invalid."
  [f]
  (try
    (f)
    false
    (catch clojure.lang.ExceptionInfo e
      (= :capability/schema-invalid (:error/type (ex-data e))))))

;; --- W-01..W-04: well-formed sealed lease passes validate + projection round-trip ---

(deftest well-formed-sealed-lease-passes-validate
  (testing "make-lease returns sealed instance that passes validate-lease"
    (let [m (base-map)
          lease (schema/make-lease m)]
      (is (schema/lease? lease) "sealed predicate")
      (is (identical? lease (schema/validate-lease lease))
          "validate-lease returns sealed instance unchanged")
      (testing "projection round-trip: lease->map is plain EDN and re-validates"
        (let [proj (schema/lease->map lease)]
          (is (= m proj) "projected map equals original map")
          (is (m/validate schema/CapabilityLeaseSchema proj)
              "projected map validates against schema (validLeaseQ)")
          (is (identical? proj (schema/validate-lease proj))
              "validate-lease on projected map returns map unchanged")
          (is (= proj (edn/read-string (pr-str proj)))
              "projected map is EDN round-trippable for event log GC-20")
          (is (schema/lease? (schema/make-lease proj))
              "projected map can be re-sealed via make-lease")))
      (testing "sealed lease is not assoc-able (secret never assoc-able)"
        (is (thrown? UnsupportedOperationException (assoc lease :cap/id (random-uuid)))))
      (testing "plain map also validates"
        (let [mm (base-map)]
          (is (identical? mm (schema/validate-lease mm))))))))

;; --- W-05: missing phenotype -> rejected ---

(deftest missing-phenotype-rejected
  (testing "missing :phenotype/id in subject is rejected with :capability/schema-invalid"
    (let [missing-pheno (assoc (base-map) :subject {})
          missing-subject (dissoc (base-map) :subject)]
      (is (throws-schema-invalid? #(schema/validate-lease missing-pheno))
          "validate-lease on map missing phenotype")
      (is (throws-schema-invalid? #(schema/make-lease missing-pheno))
          "make-lease missing phenotype")
      (is (throws-schema-invalid? #(schema/validate-lease missing-subject))
          "validate-lease missing subject key")
      (is (throws-schema-invalid? #(schema/make-lease missing-subject))
          "make-lease missing subject"))))

;; --- W-06: bad action outside allowlist -> rejected ---

(deftest bad-action-outside-allowlist-rejected
  (testing "action outside closed allowlist is rejected"
    (let [bad #{:fly}
          bad-mixed #{:invoke :fly}
          bad-single (assoc (base-map) :actions bad)
          bad-mixed-map (assoc (base-map) :actions bad-mixed)]
      (is (throws-schema-invalid? #(schema/validate-lease bad-single))
          "validate-lease rejects unknown action :fly")
      (is (throws-schema-invalid? #(schema/make-lease bad-single))
          "make-lease rejects unknown action")
      (is (throws-schema-invalid? #(schema/validate-lease bad-mixed-map))
          "mixed allowlist+unknown rejected")
      (is (throws-schema-invalid? #(schema/make-lease bad-mixed-map))
          "make-lease mixed rejected"))))

;; --- W-07: zero-window (issued==expires) -> rejected ---

(deftest zero-window-rejected
  (testing "zero-window (issued==expires) is rejected"
    (let [zero-win (assoc (base-map) :expires-at issued-at)
          neg-win (assoc (base-map) :expires-at (java.util.Date. 1699999999999))]
      (is (throws-schema-invalid? #(schema/validate-lease zero-win))
          "validate-lease zero-window")
      (is (throws-schema-invalid? #(schema/make-lease zero-win))
          "make-lease zero-window")
      (is (throws-schema-invalid? #(schema/validate-lease neg-win))
          "validate-lease negative window also rejected")
      (is (throws-schema-invalid? #(schema/make-lease neg-win))
          "make-lease negative window"))))

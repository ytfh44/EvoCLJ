(ns evoclj.compiler.resolution-test
  "Tests for Resolution and provider alias resolution (component).

  resolve-models turns a Genome's models.edn config plus a provider
  catalog into a pure data Resolution: a canonical :resolution/id over
  the concrete resolved model entries and a :models map of model name
  to {:alias ... :provider ... :provider-model ... :adapter-version ...}.
  Resolution is pure data (Global Constraints 22): secrets never appear
  in it, and secret-looking keys (:api-key, :token, :password, :secret
  and their spellings) in resolved data are rejected with a typed
  :resolution/secret-key error. The Resolution ID must follow the same
  deterministic canonical-EDN conventions as evoclj.genome.hash, so two
  provider catalogs resolving to different concrete model IDs yield
  different Resolution IDs for the same Genome models config (Global
  Constraint 2's session pinning and component acceptance)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.compiler.resolution :as resolution]
            [evoclj.genome.types :as types]))

;; --- fixture helpers -------------------------------------------------------

(defn- fixture-catalog
  "The on-disk provider catalog fixture."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- seed-models-config
  "The model alias declared by the seed Genome's models.edn."
  []
  (edn/read-string (slurp (io/resource "fixtures/genomes/minimal-valid/models.edn"))))

(defn- resolution-error
  "The ExceptionInfo thrown by resolve-models, or nil."
  [models-config provider-catalog]
  (try (resolution/resolve-models models-config provider-catalog)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

;; --- provider catalog fixture shape ----------------------------------------

(deftest provider-catalog-fixture-shape
  (let [catalog (fixture-catalog)]
    (testing "all seed aliases are present"
      (is (contains? catalog :reasoning/high))
      (is (contains? catalog :reasoning/low))
      (is (contains? catalog :fast)))
    (testing "the :fixture provider / fixture-model-v1 concrete entry used by later tasks"
      (is (= {:provider :fixture
              :provider-model "fixture-model-v1"
              :adapter-version "1"}
             (:reasoning/high catalog))))
    (testing "the catalog holds no secret-looking keys"
      (is (every? (fn [entry]
                    (every? (fn [k]
                              (not (contains? #{:api-key :token :password :secret} k)))
                            (keys entry)))
                  (vals catalog))))))

;; --- deterministic alias resolution ----------------------------------------

(deftest resolves-aliases-deterministically
  (let [r1 (resolution/resolve-models (seed-models-config) (fixture-catalog))
        r2 (resolution/resolve-models (seed-models-config) (fixture-catalog))]
    (testing "the seed model resolves to its concrete provider entry plus :alias"
      (is (= {:alias :reasoning/high
              :provider :fixture
              :provider-model "fixture-model-v1"
              :adapter-version "1"}
             (get-in r1 [:models :planner]))))
    (testing "resolution id is a canonical content-addressed id"
      (is (types/resolution-id? (:resolution/id r1))))
    (testing "resolution is deterministic across calls"
      (is (= r1 r2)))
    (testing "the result is pure serializable EDN data (Global Constraint 22)"
      (is (= r1 (edn/read-string (pr-str r1)))))))

(deftest resolves-every-seed-alias
  (let [r (resolution/resolve-models {:models {:planner {:alias :reasoning/high}
                                               :scout {:alias :reasoning/low}
                                               :sprinter {:alias :fast}}}
                                     (fixture-catalog))]
    (is (= "fixture-model-v1" (get-in r [:models :planner :provider-model])))
    (is (= "fixture-model-low" (get-in r [:models :scout :provider-model])))
    (is (= "fixture-model-fast" (get-in r [:models :sprinter :provider-model])))
    (is (= #{:planner :scout :sprinter} (set (keys (:models r)))))))

(deftest model-config-order-does-not-affect-resolution-id
  (let [cfg-a {:models {:a {:alias :reasoning/high}
                        :b {:alias :fast}}}
        cfg-b {:models {:b {:alias :fast}
                        :a {:alias :reasoning/high}}}
        catalog (fixture-catalog)]
    (is (= (:resolution/id (resolution/resolve-models cfg-a catalog))
           (:resolution/id (resolution/resolve-models cfg-b catalog))))))

(deftest seed-genome-models-fixture-resolves
  (let [r (resolution/resolve-models (seed-models-config) (fixture-catalog))]
    (is (= {:alias :reasoning/high
            :provider :fixture
            :provider-model "fixture-model-v1"
            :adapter-version "1"}
           (get-in r [:models :planner])))
    (is (types/resolution-id? (:resolution/id r)))))

;; --- missing alias failure -------------------------------------------------

(deftest missing-alias-fails-with-typed-error
  (let [e (resolution-error {:models {:planner {:alias :reasoning/ultra}}}
                            (fixture-catalog))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :resolution/alias-missing (:error/type (ex-data e))))
    (is (= :reasoning/ultra (:alias (ex-data e))))
    (is (= :planner (:model (ex-data e))))))

;; --- different concrete models => different Resolution IDs -----------------

(deftest different-concrete-models-produce-different-resolution-ids
  (let [catalog-v1 (fixture-catalog)
        catalog-v2 (assoc-in (fixture-catalog)
                             [:reasoning/high :provider-model]
                             "fixture-model-v2")]
    (testing "same genome config, different concrete model id"
      (is (not= (:resolution/id (resolution/resolve-models (seed-models-config) catalog-v1))
                (:resolution/id (resolution/resolve-models (seed-models-config) catalog-v2)))))
    (testing "same concrete model id under a different provider changes the id"
      (let [catalog-v3 (assoc-in (fixture-catalog)
                                 [:reasoning/high :provider]
                                 :fixture-alt)]
        (is (not= (:resolution/id (resolution/resolve-models (seed-models-config) catalog-v1))
                  (:resolution/id (resolution/resolve-models (seed-models-config) catalog-v3))))))
    (testing "even a changed adapter version alone changes the id"
      (let [catalog-v4 (assoc-in (fixture-catalog)
                                 [:reasoning/high :adapter-version]
                                 "2")]
        (is (not= (:resolution/id (resolution/resolve-models (seed-models-config) catalog-v1))
                  (:resolution/id (resolution/resolve-models (seed-models-config) catalog-v4))))))))

;; --- secret-looking key rejection ------------------------------------------

(deftest secret-looking-keys-are-rejected
  (doseq [secret-key [:api-key :token :password :secret]]
    (testing (str "rejects " secret-key " in a resolved provider entry")
      (let [catalog (assoc (fixture-catalog)
                           :reasoning/high
                           (assoc (:reasoning/high (fixture-catalog))
                                  secret-key "sekret"))
            e (resolution-error (seed-models-config) catalog)]
        (is (instance? clojure.lang.ExceptionInfo e))
        (is (= :resolution/secret-key (:error/type (ex-data e))))
        (is (= secret-key (:key (ex-data e))))
        (is (= :planner (:model (ex-data e))))))))

(deftest secret-looking-model-names-are-rejected
  (let [e (resolution-error {:models {:api-key {:alias :fast}}}
                            (fixture-catalog))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :resolution/secret-key (:error/type (ex-data e))))
    (is (= :api-key (:key (ex-data e))))))

;; --- input shape validation -------------------------------------------------

(deftest invalid-models-config-rejected
  (doseq [bad [{:models "not-a-map"}
               {:models {:planner "not-a-map"}}
               {:models {:planner {:provider :fixture}}}]]
    (let [e (resolution-error bad (fixture-catalog))]
      (is (instance? clojure.lang.ExceptionInfo e) (pr-str bad))
      (is (= :resolution/invalid (:error/type (ex-data e))) (pr-str bad)))))

(deftest invalid-catalog-rejected
  (testing "alias bound to a non-map entry"
    (let [e (resolution-error (seed-models-config) {:reasoning/high "oops"})]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (= :resolution/invalid (:error/type (ex-data e))))
      (is (= :reasoning/high (:alias (ex-data e))))))
  (testing "catalog that is not a map"
    (let [e (resolution-error (seed-models-config) [:reasoning/high])]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (= :resolution/invalid (:error/type (ex-data e)))))))

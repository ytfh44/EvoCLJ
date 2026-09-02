(ns evoclj.capability.lease-test
  "Tests for capability resources and lease semantics (component).

  I2 Principal algebra: a lease binds ONE Principal tagged union;
  matching is exact equality — no wildcard, no dual-anchor, no placeholder.

  Step 1 asserts EXACT principal matching — SessionPrincipal(sid) only
  matches same sid; Job/Eval/Operator are distinct. Step 2 expiry, Step 3
  filesystem canonical, Step 4 schema closed shape requires :principal, Step 5 model."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.capability.lease :as lease]
            [evoclj.capability.resource-kind :as rk]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private session-a #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def ^:private session-b #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def ^:private job-a #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc")
(def ^:private eval-a #uuid "dddddddd-dddd-4ddd-8ddd-dddddddddddd")

(def ^:private principal-a {:principal/type :session :session/id session-a})
(def ^:private principal-b {:principal/type :session :session/id session-b})
(def ^:private job-principal-a {:principal/type :job :job/id job-a})
(def ^:private eval-principal-a {:principal/type :eval :eval/id eval-a})
(def ^:private operator-principal {:principal/type :operator})

(def ^:private issued-at (java.util.Date. 1700000000000))
(def ^:private expires-at (java.util.Date. 1700003600000))

(def ^:private base-lease
  {:cap/id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
   :principal principal-a
   :resource {:kind :tool :id :fixture/echo}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})
(defn- lease
  [& kvs]
  (if (seq kvs)
    (apply assoc base-lease kvs)
    base-lease))

(defn- lease-error [f] (try (f) nil (catch clojure.lang.ExceptionInfo e e)))
(defn- is-schema-invalid [f]
  (let [e (lease-error f)]
    (is (some? e) "the call is rejected")
    (is (= :capability/schema-invalid (:error/type (ex-data e))))))

;; Step 1 — exact principal matching (I2)
(deftest exact-principal-matching
  (testing "the lease's own principal is authorized"
    (is (lease/principal-matches? (lease) principal-a)))
  (testing "different session principal never matches"
    (is (not (lease/principal-matches? (lease) principal-b))))
  (testing "different principal types never match even with same id string"
    (is (not (lease/principal-matches? (lease) job-principal-a)))
    (is (not (lease/principal-matches? (lease) eval-principal-a)))
    (is (not (lease/principal-matches? (lease) operator-principal))))
  (testing "operator lease only matches operator"
    (let [op-lease (lease :principal operator-principal)]
      (is (lease/principal-matches? op-lease operator-principal))
      (is (not (lease/principal-matches? op-lease principal-a)))))
  (testing "malformed principal is rejected"
    (is-schema-invalid #(lease/principal-matches? (lease) {}))
    (is-schema-invalid #(lease/principal-matches? (lease) {:principal/type :session}))
    (is-schema-invalid #(lease/principal-matches? (lease) {:principal/type :bogus :session/id session-a}))))

(deftest expiry-boundaries
  (testing "before :issued-at the lease is not yet valid"
    (is (not (lease/valid-at? (lease) (java.util.Date. 1699999999999)))))
  (testing "AT :issued-at the lease becomes valid (inclusive lower bound)"
    (is (lease/valid-at? (lease) issued-at)))
  (testing "inside the window the lease is valid"
    (is (lease/valid-at? (lease) (java.util.Date. 1700001800000))))
  (testing "just before :expires-at the lease is still valid"
    (is (lease/valid-at? (lease) (java.util.Date. 1700003599999))))
  (testing "AT :expires-at the lease has expired (exclusive upper bound)"
    (is (not (lease/valid-at? (lease) expires-at))))
  (testing "after :expires-at the lease has expired"
    (is (not (lease/valid-at? (lease) (java.util.Date. 1700003600001))))))

(deftest action-mismatch-never-covered
  (testing "an action outside the lease's :actions set is never covered, even when the resource matches"
    (is (not (lease/resource-covers? (lease)
                                     {:kind :tool :id :fixture/echo}
                                     :delete)))
    (is (lease/resource-covers? (lease)
                                {:kind :tool :id :fixture/echo}
                                :invoke))))

(deftest canonicalize-path-resolves-dot-segments
  (is (= "/work" (rk/canonicalize-path "/work")))
  (is (= "/work" (rk/canonicalize-path "/work/")))
  (is (= "/work/secret" (rk/canonicalize-path "/work/a/../secret")))
  (is (= "/work/a" (rk/canonicalize-path "/work/./a")))
  (is (= "/etc" (rk/canonicalize-path "/work/../../etc")))
  (is (= "/" (rk/canonicalize-path "/")))
  (is (nil? (rk/canonicalize-path nil)))
  (is (nil? (rk/canonicalize-path 42))))

(deftest filesystem-scope-on-canonical-paths
  (let [fs-lease (lease :resource {:kind :filesystem :path "/work"})]
    (testing "a canonical path inside the root is covered"
      (is (lease/resource-covers? fs-lease
                                  {:kind :filesystem :path "/work/secret"}
                                  :invoke)))
    (testing "the root itself is covered"
      (is (lease/resource-covers? fs-lease
                                  {:kind :filesystem :path "/work"}
                                  :invoke)))
    (testing "a sibling prefix is NOT covered (segment boundary)"
      (is (not (lease/resource-covers? fs-lease
                                       {:kind :filesystem :path "/workspace/x"}
                                       :invoke))))
    (testing "an unrelated absolute path is not covered"
      (is (not (lease/resource-covers? fs-lease
                                       {:kind :filesystem :path "/etc/passwd"}
                                       :invoke))))
    (testing "a traversal resolves to a canonical path INSIDE the root and is covered"
      (is (lease/resource-covers? fs-lease
                                  {:kind :filesystem :path "/work/a/../secret"}
                                  :invoke)))
    (testing "a traversal escaping the root resolves OUTSIDE and is never covered"
      (is (not (lease/resource-covers? fs-lease
                                       {:kind :filesystem :path "/work/../../etc"}
                                       :invoke))))
    (testing "a relative path is never covered by an absolute root"
      (is (not (lease/resource-covers? fs-lease
                                       {:kind :filesystem :path "work/secret"}
                                       :invoke))))))

(deftest resource-kind-mismatch-and-fail-closed
  (testing "a tool lease never covers a filesystem resource"
    (is (not (lease/resource-covers? (lease)
                                     {:kind :filesystem :path "/tmp/x"}
                                     :invoke))))
  (testing "a filesystem lease never covers a tool resource"
    (is (not (lease/resource-covers? (lease :resource {:kind :filesystem :path "/work"})
                                     {:kind :tool :id :fixture/echo}
                                     :invoke))))
  (testing "tool resources match by exact canonical id"
    (is (lease/resource-covers? (lease) {:kind :tool :id :fixture/echo} :invoke))
    (is (not (lease/resource-covers? (lease) {:kind :tool :id :fixture/other} :invoke))))
  (testing "an unknown resource kind is fail-closed: nothing is covered"
    (is (not (lease/resource-covers? (lease :resource {:kind :teleport :id :any})
                                     {:kind :teleport :id :any}
                                     :invoke)))))

(deftest valid-lease-validates-unchanged
  (let [l (lease)]
    (testing "a valid lease validates and is returned unchanged (no coercion)"
      (is (identical? l (schema/validate-lease l))))
    (testing "leases are plain immutable EDN data"
      (is (= l (edn/read-string (pr-str l)))))))

(deftest lease-schema-rejects-malformed-maps
  (testing "every field is required"
    (doseq [k [:cap/id :principal :resource :actions :constraints
               :issued-at :expires-at]]
      (is-schema-invalid #(schema/validate-lease (dissoc (lease) k)))))
  (testing "the top-level shape is closed — unknown keys are rejected"
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :rogue/key 1))))
  (testing "field types are enforced"
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :cap/id "not-a-uuid")))
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :actions [:invoke])))
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :actions #{42})))
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :resource "echo")))
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :principal {})))
    (is-schema-invalid #(schema/validate-lease
                         (assoc (lease) :principal {:principal/type :session :session/id 42})))
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :issued-at 1700000000000)))
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :expires-at nil))))
  (testing "a grant must span a positive window"
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :expires-at issued-at)))
    (is-schema-invalid #(schema/validate-lease
                         (assoc (lease) :expires-at (java.util.Date. 1699999999999))))))
(deftest lease-rejects-non-edn-safe-values
  (testing "a raw Java object inside the lease cannot cross the boundary (Global Constraint 22)"
    (let [e (lease-error #(schema/validate-lease
                           (assoc-in (lease) [:constraints :secret]
                                     (java.io.File. "C:/tmp/secret.txt"))))]
      (is (some? e))
      (is (= :capability/not-edn-safe (:error/type (ex-data e)))))))

(deftest predicates-reject-malformed-input
  (testing "a malformed lease is rejected, never silently matched"
    (is-schema-invalid #(lease/valid-at? (dissoc (lease) :expires-at) issued-at))
    (is-schema-invalid #(lease/principal-matches? (dissoc (lease) :principal)
                                               principal-a))
    (is-schema-invalid #(lease/resource-covers? (dissoc (lease) :actions)
                                                {:kind :tool :id :fixture/echo}
                                                :invoke)))
  (testing "malformed decision inputs are rejected"
    (is-schema-invalid #(lease/valid-at? (lease) "2023-11-15"))
    (is-schema-invalid #(lease/resource-covers? (lease) "not-a-map" :invoke))
    (is-schema-invalid #(lease/resource-covers? (lease)
                                                {:kind :tool :id :fixture/echo}
                                                "invoke"))))

(deftest schema-explanation-round-trips-through-edn
  (let [e (lease-error #(schema/validate-lease (dissoc (lease) :expires-at)))
        d (err/error-data e)]
    (is (= :capability/schema-invalid (:error/type d)))
    (is (contains? (:error/data d) :explanation))))

(deftest model-resource-coverage
  (testing "an exact model lease covers model A, never model B"
    (let [model-lease (lease :resource {:kind :model :id "deepseek/deepseek-v4-flash"})]
      (is (lease/resource-covers? model-lease
                                  {:kind :model :id "deepseek/deepseek-v4-flash"}
                                  :invoke))
      (is (not (lease/resource-covers? model-lease
                                       {:kind :model :id "anthropic/claude-sonnet-4-5"}
                                       :invoke))
          "a lease for model A never covers model B")))
  (testing "a wildcard model lease covers models inside the provider prefix, never outside"
    (let [wild (lease :resource {:kind :model :id "deepseek/*"})]
      (is (lease/resource-covers? wild
                                  {:kind :model :id "deepseek/deepseek-v4-flash"}
                                  :invoke))
      (is (lease/resource-covers? wild
                                  {:kind :model :id "deepseek/other-v1"}
                                  :invoke))
      (is (not (lease/resource-covers? wild
                                       {:kind :model :id "anthropic/claude-sonnet-4-5"}
                                       :invoke))
          "a different provider is outside the wildcard prefix")
      (is (not (lease/resource-covers? wild
                                       {:kind :model :id "deepseek"}
                                       :invoke))
          "a bare prefix without the separator is not covered")))
  (testing "a model lease never covers a tool or filesystem resource, and vice versa"
    (let [model-lease (lease :resource {:kind :model :id "deepseek/*"})]
      (is (not (lease/resource-covers? model-lease {:kind :tool :id :fixture/echo} :invoke)))
      (is (not (lease/resource-covers? model-lease {:kind :filesystem :path "/work"} :invoke))))
    (is (not (lease/resource-covers? (lease)
                                     {:kind :model :id "deepseek/deepseek-v4-flash"}
                                     :invoke))
        "a tool lease never covers a model resource")))

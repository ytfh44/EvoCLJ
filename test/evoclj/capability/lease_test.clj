(ns evoclj.capability.lease-test
  "Tests for capability resources and lease semantics (component).

  A CapabilityLease is a bounded HOST-OWNED grant: a plain immutable
  map validated by Malli, never a string name visible to the model.
  Step 1 asserts EXACT subject matching — a lease for P1 must never
  authorize P2, even when both phenotypes share the same Genome (the
  lease carries only the phenotype id, so a same-Genome sibling is a
  different id and must not match). Step 2 asserts the expiry window
  boundaries (:issued-at inclusive, :expires-at EXCLUSIVE — a lease is
  dead AT its expiry instant) and that an action outside the lease's
  :actions set is never covered, even when the resource matches. Step 3
  asserts filesystem scoping over CANONICAL RESOLVED PATHS: matching
  happens on canonical forms, never on user-supplied strings —
  \"/work/a/../secret\" is covered only because it resolves to
  \"/work/secret\", which lies inside the \"/work\" root, while a
  traversal escaping to \"/etc\" is never covered. Step 4 asserts
  schema validation of the lease map itself: closed shape, every field
  required, a positive grant window, typed errors, and EDN-safety
  (Global Constraint 22). Step 5 asserts model resource coverage
  (roadmap S3): an exact model lease covers model A and denies model
  B, a wildcard \"<provider>/*\" lease covers only models inside the
  provider prefix, and a model lease never covers a tool or
  filesystem resource (kind mismatch fails closed).

  Provider-side normalization of user-facing requests is component
  (evoclj.provider); the filesystem matcher here canonicalizes the pure
  path forms itself, so the tests use canonical \"/\"-separated fixture
  paths and note that dependency."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.capability.lease :as lease]
            [evoclj.capability.schema :as schema]
            [evoclj.kernel.error :as err]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private phenotype-p1
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(def ^:private phenotype-p2
  "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")

(def ^:private issued-at (java.util.Date. 1700000000000))
(def ^:private expires-at (java.util.Date. 1700003600000)) ; issued-at + 1h

(def ^:private base-lease
  {:cap/id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
   :subject {:phenotype/id phenotype-p1}
   :resource {:kind :tool :id :fixture/echo}
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

(defn- lease
  "A valid lease map, optionally with assoc-style overrides."
  [& kvs]
  (if (seq kvs)
    (apply assoc base-lease kvs)
    base-lease))

;; --- shared helpers --------------------------------------------------------

(defn- lease-error
  "The ExceptionInfo thrown by the thunk f, or nil when f succeeds."
  [f]
  (try (f)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- is-schema-invalid
  "Assert the thunk f throws :capability/schema-invalid."
  [f]
  (let [e (lease-error f)]
    (is (some? e) "the call is rejected")
    (is (= :capability/schema-invalid (:error/type (ex-data e))))))

;; ============================================================================
;; Step 1 — exact subject matching
;; ============================================================================

(deftest exact-subject-matching
  (testing "the lease's own phenotype is authorized"
    (is (lease/subject-matches? (lease) {:phenotype/id phenotype-p1})))
  (testing "a lease for P1 never authorizes P2, even a same-Genome sibling"
    (is (not (lease/subject-matches? (lease) {:phenotype/id phenotype-p2}))))
  (testing "a subject with a malformed or missing phenotype id is rejected, never matched"
    (is-schema-invalid #(lease/subject-matches? (lease) {}))
    (is-schema-invalid #(lease/subject-matches? (lease) {:phenotype/id "not-a-hash"}))))

;; ============================================================================
;; Step 2 — expiry boundaries and action mismatch
;; ============================================================================

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

;; ============================================================================
;; Step 3 — filesystem-style resource scoping on canonical resolved paths
;; ============================================================================

(deftest canonicalize-path-resolves-dot-segments
  (is (= "/work" (lease/canonicalize-path "/work")))
  (is (= "/work" (lease/canonicalize-path "/work/")))
  (is (= "/work/secret" (lease/canonicalize-path "/work/a/../secret")))
  (is (= "/work/a" (lease/canonicalize-path "/work/./a")))
  (is (= "/etc" (lease/canonicalize-path "/work/../../etc")))
  (is (= "/" (lease/canonicalize-path "/")))
  (is (nil? (lease/canonicalize-path nil)))
  (is (nil? (lease/canonicalize-path 42))))

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

;; ============================================================================
;; Step 4 — schema validation of the lease map
;; ============================================================================

(deftest valid-lease-validates-unchanged
  (let [l (lease)]
    (testing "a valid lease validates and is returned unchanged (no coercion)"
      (is (identical? l (schema/validate-lease l))))
    (testing "leases are plain immutable EDN data"
      (is (= l (edn/read-string (pr-str l)))))))

(deftest lease-schema-rejects-malformed-maps
  (testing "every field is required"
    (doseq [k [:cap/id :subject :resource :actions :constraints
               :issued-at :expires-at]]
      (is-schema-invalid #(schema/validate-lease (dissoc (lease) k)))))
  (testing "the top-level shape is closed — unknown keys are rejected"
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :rogue/key 1))))
  (testing "field types are enforced"
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :cap/id "not-a-uuid")))
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :actions [:invoke])))
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :actions #{42})))
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :resource "echo")))
    (is-schema-invalid #(schema/validate-lease (assoc (lease) :subject {})))
    (is-schema-invalid #(schema/validate-lease
                         (assoc (lease) :subject {:phenotype/id "not-a-hash"})))
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
    (is-schema-invalid #(lease/subject-matches? (dissoc (lease) :subject)
                                               {:phenotype/id phenotype-p1}))
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
    (is (contains? (:error/data d) :explanation))
    (is (seq (:errors (:explanation (:error/data d)))))
    (is (= d (edn/read-string (pr-str d))))))

;; ============================================================================
;; Step 5 — model resource coverage (S3: per-model lease denial cases)
;; ============================================================================

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

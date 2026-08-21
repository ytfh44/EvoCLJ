(ns evoclj.sci.boundary-test
  "Tests for the EDN-safe boundary and eager realization (component).

  Every value crossing the SCI boundary must be plain, fully realized,
  EDN-safe data (Global Constraint 22): edn-safe? tests a value
  recursively without ever realizing a lazy sequence; materialize-edn
  recursively converts a value to plain EDN-safe data under explicit
  maximum depth (default 64) and maximum collection size (default
  100000) limits, realizing lazy sequences under the limit and
  rejecting — with typed errors — what cannot be materialized: Java
  objects (File, InputStream), functions, atoms, promises, futures,
  delays, Clojure/SCI vars, and unregistered records.

  Step 1 asserts the accepted universe: nil, booleans, all number
  types, strings, keywords, symbols, chars, UUIDs, #inst dates, and
  vectors/lists/maps/sets with nested combinations. Step 2 asserts the
  rejected universe: java.io.File, InputStream, function, atom, promise,
  future, an unrealized lazy seq, a SCI var object, and an arbitrary
  record each fail edn-safe? and are rejected by materialize-edn with a
  typed error. Steps 3–4 assert infinite/lazy sequences cannot escape
  (materializing (range) hits the size limit or fails — it never hangs
  and never returns a lazy seq) and that depth/size limits are enforced
  with exact boundary acceptance. Step 5 covers
  validate-program-input/output against Malli schemas (a schema value is
  passed directly; keyword schema-registry lookup comes later)."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.sci.boundary :as boundary]
            [sci.core :as sci]))

(defrecord BoundaryProbe [x])

;; --- shared helpers --------------------------------------------------------

(defn- boundary-error
  "The ExceptionInfo thrown by materialize-edn for x (with optional
  opts), or nil when materialization succeeds."
  [x & [opts]]
  (try (boundary/materialize-edn x opts)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- program-error
  "The ExceptionInfo thrown by (f), or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- is-unsupported
  "Assert materialize-edn rejects x with :edn/unsupported and the given
  :reason. The label (never the value) is printed, because printing an
  offending value could realize a lazy sequence or touch a Java object."
  [x reason]
  (let [e (boundary-error x)]
    (is (some? e) (str "materialize-edn rejects " (name reason)))
    (is (= :edn/unsupported (:error/type (ex-data e)))
        (str "typed error :edn/unsupported for " (name reason)))
    (is (= reason (:reason (ex-data e)))
        (str "reason for " (name reason)))))

(defn- nest
  "A value nested n collection levels deep, e.g. (nest 3) -> [[[0]]]."
  [n]
  (reduce (fn [acc _] [acc]) 0 (range n)))

;; ============================================================================
;; Step 1 — accepted values
;; ============================================================================

(deftest edn-safe?-accepts-edn-primitives
  (doseq [x [nil true false
             0 1 -42 3.14 1/3 2N 2.5M (long 7) (byte 7) (float 1.5)
             "" "hello"
             :a :agent/main ::self
             'a 'agent/run
             \a \space
             #uuid "550e8400-e29b-41d4-a716-446655440000"
             #inst "2020-01-01T00:00:00.000-00:00"]]
    (is (boundary/edn-safe? x) (pr-str x))))

(deftest edn-safe?-accepts-collections-and-nesting
  (doseq [x [[1 2 3] [[1 2] [3 [4]]] []
             '(1 2 3) '((1) (2 3)) '()
             {:a 1} {:a {:b [1 2 {:c #{3}}]}} {}
             #{1 2} #{#{1} {2 3}} #{}
             {:k [:v {:deep #{1 [2]}}] 'sym "str"}
             [{:a #{1 2}} '(3 {:b [4]})]]]
    (is (boundary/edn-safe? x) (pr-str x))))

(deftest materialize-edn-preserves-edn-data
  (doseq [x [nil true false 0 -42 3.14 1/3 2N "s" :k 'sym \a
             #uuid "550e8400-e29b-41d4-a716-446655440000"
             #inst "2020-01-01T00:00:00.000-00:00"
             [1 {:a [2]}] '(1 2) {:m #{1}} #{:a}]]
    (testing (pr-str x)
      (is (= x (boundary/materialize-edn x)))
      (is (boundary/edn-safe? (boundary/materialize-edn x)))
      (is (= x (edn/read-string (pr-str (boundary/materialize-edn x))))
          "materialized data round-trips through pr-str / clojure.edn"))))

(deftest materialized-values-are-always-edn-safe
  (doseq [x [nil true 42 "s" :k 'sym [1 {:a [2]}] '(1 2) {:m #{1}} #{:a}
             #uuid "550e8400-e29b-41d4-a716-446655440000"
             #inst "2020-01-01T00:00:00.000-00:00"
             (range 10)
             (map (fn [x] {:v (range x)}) [2 3])]]
    (is (boundary/edn-safe? (boundary/materialize-edn x)))))

;; ============================================================================
;; Step 2 — rejected values
;; ============================================================================

(deftest edn-safe?-rejects-unsupported-values
  (doseq [[label x] [["java.io.File" (java.io.File. "/tmp/x")]
                     ["ByteArrayInputStream" (java.io.ByteArrayInputStream. (byte-array 3))]
                     ["function" (fn [x] x)]
                     ["atom" (atom 1)]
                     ["promise" (promise)]
                     ["future" (future 1)]
                     ["unrealized lazy seq (range)" (range)]
                     ["unrealized lazy seq (map)" (map inc (range))]
                     ["delay" (delay 1)]
                     ["SCI var object" (sci/eval-string "(def x 1) #'x")]
                     ["arbitrary record" (->BoundaryProbe 1)]]]
    (is (false? (boundary/edn-safe? x)) label)))

(deftest materialize-edn-rejects-unsupported-with-typed-errors
  (is-unsupported (java.io.File. "/tmp/x") :java-object)
  (is-unsupported (java.io.ByteArrayInputStream. (byte-array 3)) :java-object)
  (is-unsupported (fn [x] x) :function)
  (is-unsupported (atom 1) :atom)
  (is-unsupported (promise) :promise)
  (is-unsupported (future 1) :future)
  (is-unsupported (delay 1) :delay)
  (is-unsupported (sci/eval-string "(def x 1) #'x") :sci-var)
  (is-unsupported (->BoundaryProbe 1) :record))

(deftest unsupported-values-are-located-by-path
  (let [e (boundary-error [1 {:f (fn [] 2)}])]
    (is (= :edn/unsupported (:error/type (ex-data e))))
    (is (= :function (:reason (ex-data e))))
    (is (= [1 :f] (:path (ex-data e))))))

(deftest boundary-error-data-round-trips-through-edn
  (doseq [[label x] [["file" (java.io.File. "/tmp/x")]
                     ["function" (fn [x] x)]
                     ["record" (->BoundaryProbe 1)]]]
    (let [e (boundary-error x)]
      (is (= (ex-data e) (edn/read-string (pr-str (ex-data e)))) label))))

;; ============================================================================
;; Step 3 — infinite/lazy sequences cannot escape
;; ============================================================================

(deftest lazy-sequences-are-realized-under-the-limit
  (testing "a finite lazy seq is realized into a proper list, never returned lazily"
    (let [r (boundary/materialize-edn (range 5))]
      (is (= '(0 1 2 3 4) r))
      (is (boundary/edn-safe? r))
      (is (counted? r))
      (is (not (instance? clojure.lang.LazySeq r)))))
  (testing "nested laziness is realized recursively"
    (is (= {:a '(0 1 2) :b '((0 1) (0 1 2))}
           (boundary/materialize-edn {:a (range 3)
                                      :b (map (fn [x] (range x)) [2 3])}))))
  (testing "an unrealized lazy seq is not EDN-safe"
    (is (false? (boundary/edn-safe? (filter even? (range 100)))))))

(deftest infinite-lazy-sequences-cannot-escape
  (testing "materializing (range) hits the default size limit instead of hanging"
    (let [e (boundary-error (range))]
      (is (some? e))
      (is (= :edn/size-exceeded (:error/type (ex-data e))))))
  (testing "materializing (repeat) with a small limit fails fast"
    (let [e (boundary-error (repeat :x) {:max-size 10})]
      (is (= :edn/size-exceeded (:error/type (ex-data e))))))
  (testing "an infinite lazy seq nested inside data cannot escape either"
    (let [e (boundary-error {:xs (range)})]
      (is (= :edn/size-exceeded (:error/type (ex-data e)))))
    (let [e (boundary-error [(range)])]
      (is (= :edn/size-exceeded (:error/type (ex-data e)))))))

;; ============================================================================
;; Step 4 — depth/size limits enforced
;; ============================================================================

(deftest collection-size-limit-is-enforced
  (testing "explicit limit rejects oversized collections with the found size"
    (let [e (boundary-error (vec (range 4)) {:max-size 3})]
      (is (= :edn/size-exceeded (:error/type (ex-data e))))
      (is (= 3 (:limit (ex-data e))))
      (is (= 4 (:found (ex-data e))))))
  (testing "boundary acceptance: size exactly at the limit passes"
    (is (= [0 1 2] (boundary/materialize-edn (vec (range 3)) {:max-size 3}))))
  (testing "empty collections pass even with max-size 0"
    (is (= [] (boundary/materialize-edn [] {:max-size 0}))))
  (testing "default limit is 100000"
    (is (= 100000 (count (boundary/materialize-edn (vec (range 100000))))))
    (let [e (boundary-error (vec (range 100001)))]
      (is (= :edn/size-exceeded (:error/type (ex-data e))))))
  (testing "maps and sets are size-limited too"
    (is (= :edn/size-exceeded
           (:error/type (ex-data (boundary-error
                                  (into {} (map (fn [i] [i i]) (range 4)))
                                  {:max-size 3})))))
    (is (= :edn/size-exceeded
           (:error/type (ex-data (boundary-error (set (range 4)) {:max-size 3})))))))

(deftest nesting-depth-limit-is-enforced
  (testing "explicit limit rejects over-deep nesting"
    (let [e (boundary-error (nest 4) {:max-depth 3})]
      (is (= :edn/depth-exceeded (:error/type (ex-data e))))
      (is (= 3 (:limit (ex-data e))))))
  (testing "boundary acceptance: depth exactly at the limit passes"
    (is (= [[[0]]] (boundary/materialize-edn (nest 3) {:max-depth 3}))))
  (testing "default depth limit is 64"
    (is (boundary/edn-safe? (boundary/materialize-edn (nest 64))))
    (let [e (boundary-error (nest 65))]
      (is (= :edn/depth-exceeded (:error/type (ex-data e)))))))

;; ============================================================================
;; Step 5 — validate-program-input / validate-program-output
;; ============================================================================

(deftest validate-program-input-validates-against-schema
  (testing "matching input is returned unchanged (no coercion)"
    (let [input {:text "hi" :count 3}]
      (is (identical? input (boundary/validate-program-input
                             [:map [:text :string] [:count int?]] input)))))
  (testing "keyword schemas are accepted as schema values"
    (is (= "hi" (boundary/validate-program-input :string "hi"))))
  (testing "schema mismatch throws :program/input-invalid"
    (let [e (program-error #(boundary/validate-program-input
                             [:map [:text :string]] {:text 42}))]
      (is (= :program/input-invalid (:error/type (ex-data e))))
      (is (= :schema-invalid (:reason (ex-data e))))
      (is (some? (:explanation (ex-data e))))
      (is (= (ex-data e) (edn/read-string (pr-str (ex-data e))))
          "explanation data round-trips through EDN")))
  (testing "non-EDN-safe input is rejected before schema checking"
    (let [e (program-error #(boundary/validate-program-input :any (fn [] 1)))]
      (is (= :program/input-invalid (:error/type (ex-data e))))
      (is (= :not-edn-safe (:reason (ex-data e)))))))

(deftest validate-program-output-validates-against-schema
  (testing "matching output is returned unchanged (no coercion)"
    (let [out {:ok true}]
      (is (identical? out (boundary/validate-program-output [:map [:ok :boolean]] out)))))
  (testing "schema mismatch throws :program/output-invalid"
    (let [e (program-error #(boundary/validate-program-output
                             [:map [:ok :boolean]] {:ok "yes"}))]
      (is (= :program/output-invalid (:error/type (ex-data e))))
      (is (= :schema-invalid (:reason (ex-data e))))))
  (testing "non-EDN-safe output is rejected"
    (let [e (program-error #(boundary/validate-program-output :any (atom 1)))]
      (is (= :program/output-invalid (:error/type (ex-data e))))
      (is (= :not-edn-safe (:reason (ex-data e)))))))

(deftest validate-program-rejects-invalid-schema-parameter
  (let [e (program-error #(boundary/validate-program-input 42 {:a 1}))]
    (is (= :program/schema-invalid (:error/type (ex-data e)))))
  (let [e (program-error #(boundary/validate-program-output "nope" 1))]
    (is (= :program/schema-invalid (:error/type (ex-data e))))))

;; ============================================================================
;; records: rejected unless explicitly registered
;; ============================================================================

(deftest explicitly-registered-records-materialize-as-plain-maps
  (testing "an unregistered record is rejected"
    (is (= :edn/unsupported (:error/type (ex-data (boundary-error (->BoundaryProbe 1)))))))
  (testing "an explicitly registered record materializes as a plain map"
    (let [m (boundary/materialize-edn (->BoundaryProbe 7)
                                      {:allowed-records #{'evoclj.sci.boundary-test.BoundaryProbe}})]
      (is (= {:x 7} m))
      (is (map? m))
      (is (not (record? m)))
      (is (boundary/edn-safe? m))))
  (testing "the record class object is accepted as a registration too"
    (let [probe (->BoundaryProbe 8)
          ;; the class object is derived at runtime: the dynamically
          ;; generated record class is not resolvable as a compile-time
          ;; class literal in a require-loaded namespace
          c (class probe)]
      (is (= {:x 8} (boundary/materialize-edn probe {:allowed-records #{c}}))))))

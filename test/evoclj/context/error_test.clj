(ns evoclj.context.error-test
  (:require [clojure.test :as t]
            [evoclj.context.error :as e]))

(t/deftest error-overrides-caller-supplied-type
  (let [err (e/error :context/compression-invalid "msg" {:error/type :some/other-type})]
    (t/is (= :context/compression-invalid (:error/type (ex-data err))))))

(t/deftest error-data-returns-plain-map
  (let [orig-data {:foo "bar" :baz 42}
        err (e/error :context/trigger-invalid "test error" orig-data)
        ed (e/error-data err)]
    (t/is (map? ed))
    (t/is (contains? ed :error/type))
    (t/is (contains? ed :error/message))
    (t/is (contains? ed :error/class))
    (t/is (contains? ed :error/data))
    (t/is (contains? ed :error/cause))))

(t/deftest error-data-round-trips-through-pr-str-read-string
  (let [err (e/error :context/apply-invalid "test" {:a 1 :b [1 2 3]})
        ed (e/error-data err)
        serialized (pr-str ed)
        deserialized (clojure.edn/read-string serialized)]
    (t/is (= ed deserialized))))

(t/deftest error-data-contains-no-throwable-class-lazy-seq-fn
  (let [f (fn [] "fn")
        lazy-seq (map inc (range 100))
        err (e/error :context/residue-invalid "test"
                     {:fn f :lazy lazy-seq :nested {:fn f :lazy lazy-seq}})
        ed (e/error-data err)
        all-vals (tree-seq coll? seq ed)]
    (t/is (not (some fn? all-vals)))
    (t/is (not (some #(instance? Throwable %) all-vals)))
    (t/is (not (some #(instance? Class %) all-vals)))))

(t/deftest sanitize-function-returns-sentinel
  (let [f (fn [] "test")]
    (t/is (= :evoclj.context.error/fn (e/sanitize f)))))

(t/deftest sanitize-lazy-seq-returns-realized-vector
  (let [lazy-seq (map inc (range 10))
        result (e/sanitize lazy-seq)]
    (t/is (vector? result))
    (t/is (= [1 2 3 4 5 6 7 8 9 10] result))))

(t/deftest sanitize-lazy-seq-is-bounded
  (let [lazy-seq (map inc (range 10000))]
    (t/is (<= (count (e/sanitize lazy-seq)) 1024))))

(t/deftest sanitize-nested-map-returns-plain-map
  (let [nested {:a {:b {:c 1}}}
        result (e/sanitize nested)]
    (t/is (map? result))
    (t/is (map? (:a result)))
    (t/is (map? (:b (:a result))))))

(t/deftest sanitize-java-io-file-returns-string
  (let [f (java.io.File. "/tmp/test")]
    (t/is (string? (e/sanitize f)))))

(t/deftest sanitize-round-trips
  (let [orig {:a 1 :b [1 2 3] :c {:d 4}}
        sanitized (e/sanitize orig)
        serialized (pr-str sanitized)
        deserialized (clojure.edn/read-string serialized)]
    (t/is (= orig deserialized))))

;; ----------------------------------------------------------------------
;; Typed error keywords
;; ----------------------------------------------------------------------

(t/deftest compression-invalid
  (let [err (try (throw (e/error :context/compression-invalid "malformed envelope" {}))
                 (catch Exception e e))
        ed (ex-data err)]
    (t/is (= :context/compression-invalid (:error/type ed)))))

(t/deftest trigger-invalid
  (let [err (try (throw (e/error :context/trigger-invalid "bad config" {}))
                 (catch Exception e e))
        ed (ex-data err)]
    (t/is (= :context/trigger-invalid (:error/type ed)))))

(t/deftest apply-invalid
  (let [err (try (throw (e/error :context/apply-invalid "apply failed" {}))
                 (catch Exception e e))
        ed (ex-data err)]
    (t/is (= :context/apply-invalid (:error/type ed)))))

(t/deftest residue-invalid
  (let [err (try (throw (e/error :context/residue-invalid "bad residue" {}))
                 (catch Exception e e))
        ed (ex-data err)]
    (t/is (= :context/residue-invalid (:error/type ed)))))

(t/deftest provenance-invalid
  (let [err (try (throw (e/error :context/provenance-invalid "cannot trace" {}))
                 (catch Exception e e))
        ed (ex-data err)]
    (t/is (= :context/provenance-invalid (:error/type ed)))))

(t/deftest crosscheck-mismatch
  (let [err (try (throw (e/error :context/crosscheck-mismatch "fields disagree" {}))
                 (catch Exception e e))
        ed (ex-data err)]
    (t/is (= :context/crosscheck-mismatch (:error/type ed)))))

(t/deftest idempotency-violation
  (let [err (try (throw (e/error :context/idempotency-violation "lost field" {}))
                 (catch Exception e e))
        ed (ex-data err)]
    (t/is (= :context/idempotency-violation (:error/type ed)))))

(t/deftest eval-invalid
  (let [err (try (throw (e/error :context/eval-invalid "malformed eval" {}))
                 (catch Exception e e))
        ed (ex-data err)]
    (t/is (= :context/eval-invalid (:error/type ed)))))

(t/run-tests)

(ns evoclj.kernel.error-test
  "Tests for the typed error contract (Task 1.1).

  Failures crossing EvoCLJ module boundaries must carry a stable
  machine-readable :error/type and must be fully serializable:
  no Throwable objects, class objects, lazy seqs, or functions
  (Global Constraint 22)."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.kernel.error :as err]))

(deftest typed-error-is-data-readable
  (let [e (err/error :genome/invalid "bad genome" {:path "manifest.edn"})]
    (is (= :genome/invalid (:error/type (ex-data e))))
    (is (= "manifest.edn" (:path (ex-data e))))))

(deftest error-returns-exception-info
  (let [e (err/error :genome/io "missing file" {:path "x.edn"})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= "missing file" (ex-message e)))))

(deftest error-type-is-stable-over-data
  ;; A caller-supplied :error/type must not shadow the contract type.
  (let [e (err/error :genome/invalid "bad" {:error/type :user/claimed :path "x"})]
    (is (= :genome/invalid (:error/type (ex-data e))))
    (is (= "x" (:path (ex-data e))))))

(deftest error-data-is-serializable
  (let [e (err/error :genome/invalid "bad genome"
                     {:path "manifest.edn" :n 42 :tags #{:a :b} :v [1 2 3]})
        d (err/error-data e)]
    (is (= :genome/invalid (:error/type d)))
    (is (= "bad genome" (:error/message d)))
    (is (= {:path "manifest.edn" :n 42 :tags #{:a :b} :v [1 2 3]} (:error/data d)))
    (is (= d (edn/read-string (pr-str d))))))

(deftest error-data-excludes-unsafe-values
  ;; Task Step 4: error-data must not contain a Throwable object, Java
  ;; class instance, lazy sequence, or function — and the result must
  ;; round-trip through pr-str/edn read-string.
  (let [e (err/error :kernel/unsafe "unsafe payload"
                     {:throwable (ex-info "inner" {:n 1})
                      :class-object String
                      :lazy-seq (map inc (range))       ; infinite lazy seq
                      :function (fn [x] x)
                      :nested {:deep (range 5)}})
        d (err/error-data e)
        nodes (tree-seq coll? seq d)]
    (is (= d (edn/read-string (pr-str d))))
    (is (not-any? #(instance? Throwable %) nodes))
    (is (not-any? class? nodes))
    (is (not-any? fn? nodes))
    (is (not-any? #(instance? clojure.lang.LazySeq %) nodes))
    ;; Unsafe values are transformed, not silently dropped.
    (is (= :evoclj.kernel.error/fn (:function (:error/data d))))
    (is (= 'java.lang.String (:class-object (:error/data d))))
    (is (= {:n 1} (:error/data (:throwable (:error/data d)))))
    (is (= [0 1 2 3 4] (:deep (:nested (:error/data d)))))))

(deftest error-data-of-plain-throwable
  (let [d (err/error-data (IllegalStateException. "boom"))]
    (is (= :error/unknown (:error/type d)))
    (is (= "boom" (:error/message d)))
    (is (= "java.lang.IllegalStateException" (:error/class d)))
    (is (= d (edn/read-string (pr-str d))))))

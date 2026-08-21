(ns evoclj.genome.path
  "Canonical relative-path validation for Genome bundles (component).

  Genome file references are canonical slash-separated relative paths.
  Implementation delegates to evoclj.fs.path for single-source safety;
  this namespace retains :genome error types for backward compatibility."
  (:require [evoclj.fs.path :as fs-path]
            [evoclj.kernel.error :as err])
  (:import (java.nio.file Path Paths)))

(defn normalize-relative-path
  "Genome wrapper over evoclj.fs.path/normalize-relative-path.
  Preserves :genome/path-invalid error type."
  [s]
  (try
    (fs-path/normalize-relative-path s)
    (catch clojure.lang.ExceptionInfo e
      (if (= :fs/path-invalid (:error/type (ex-data e)))
        (throw (err/error :genome/path-invalid (ex-message e) (dissoc (ex-data e) :error/type)))
        (throw e)))))

(defn bytewise-compare
  "Genome wrapper over evoclj.fs.path/bytewise-compare."
  [a b]
  (fs-path/bytewise-compare a b))

(defn allowed-genome-path?
  "Genome wrapper over evoclj.fs.path/allowed-path?."
  ([path]
   (try
     (fs-path/allowed-path? path)
     (catch Exception _ false)))
  ([base path]
   (try
     (fs-path/allowed-path? base path)
     (catch Exception _ false))))

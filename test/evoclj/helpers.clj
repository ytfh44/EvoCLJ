(ns evoclj.helpers
  "Shared test helpers — collapsed from duplicated temp-dir, symlink, and
  mutation assertion helpers across patch, mutation, adversarial, vendor, and
  demo tests (S4 fix2: collapse test helper duplication)."
  (:require [clojure.test :refer [is]]
            [evoclj.evolution.mutation :as mutation])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption OpenOption Path Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- temp dirs -------------------------------------------------------------

(defn temp-dir!
  "Create a temp directory with given prefix (default \"evoclj-test-\")."
  ([] (temp-dir! "evoclj-test-"))
  ([prefix] (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(def ^:private nofollow-links
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn delete-recursively!
  "Delete dir recursively. Symlinks are deleted as links, never followed."
  [^Path dir]
  (when (Files/exists dir nofollow-links)
    (if (Files/isSymbolicLink dir)
      (Files/delete dir)
      (let [f (.toFile dir)]
        (when (.isDirectory f)
          (doseq [c (.listFiles f)]
            (delete-recursively! (.toPath c))))
        (Files/deleteIfExists dir))))
  nil)

(defmacro with-temp-dirs
  "Bind each name to a fresh temp dir; delete all on exit."
  [names & body]
  (let [names (vec names)]
    `(let [~@(mapcat (fn [n] [n `(temp-dir!)]) names)]
       (try
         ~@body
         (finally
           (doseq [d# ~names]
             (delete-recursively! d#)))))))

(defn write-text!
  "Write content to dir/rel, creating parent directories."
  [^Path dir rel ^String content]
  (let [p (.resolve dir rel)]
    (when-let [parent (.getParent p)]
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes content StandardCharsets/UTF_8) (make-array OpenOption 0))
    p))

(defn try-create-symlink!
  "Best-effort Files/createSymbolicLink. Returns false when host refuses."
  [^Path target ^Path link]
  (try
    (Files/createSymbolicLink link target (make-array FileAttribute 0))
    true
    (catch Exception _ false)))

(defn dir-entries
  "Sorted vector of dir entries."
  [^Path dir]
  (->> (.list (.toFile dir)) sort vec))

(defn text-of
  "Decode bytes as UTF-8 text."
  [file-value]
  (String. ^bytes (byte-array (:bytes file-value)) StandardCharsets/UTF_8))

(defn thrown-error
  "The ExceptionInfo thrown by (f), or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn error-type
  "The :error/type of the ExceptionInfo thrown by (f), or nil."
  [f]
  (:error/type (ex-data (thrown-error f))))

(defn thrown-error-type
  [f]
  (error-type f))

;; --- S4 helpers ------------------------------------------------------------

(defn assert-validated
  "Assert that validate-mutation returns a ValidatedMutation whose raw equals expected."
  [expected validated]
  (is (mutation/validated-mutation? validated))
  (is (= expected (mutation/validated->raw validated)))
  (is (every? #(instance? evoclj.evolution.mutation.MutableAssetRef %) (:asset-refs validated)))
  (is (every? #(or (nil? %) (instance? evoclj.evolution.mutation.VerifiedDigest %)) (:verified-digests validated))))

(defn assert-validated-simple
  "Simpler assert-validated for tests that only check validated? and raw."
  [expected validated]
  (is (mutation/validated-mutation? validated))
  (is (= expected (mutation/validated->raw validated))))
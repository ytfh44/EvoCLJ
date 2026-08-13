(ns evoclj.genome.load-test
  "Tests for loading an immutable Genome bundle from disk (Task 1.4).

  load-genome walks the bundle directory without following symbolic
  links, validates the manifest before trusting its module paths, and
  returns an immutable content-addressed Genome value. Failure classes
  carry stable :error/type keywords: missing manifest
  (:genome/manifest-missing), required module absent from the bundle
  (:genome/module-missing), symbolic link anywhere in the bundle
  (:genome/symlink-rejected), unreadable entry (:genome/unreadable),
  duplicate normalized paths (:genome/duplicate-path), traversal or
  absolute module paths (:genome/schema-invalid, rejected before any
  path is trusted), and declared EDN that fails to parse
  (:genome/edn-invalid)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.genome.schema :as schema]
            [evoclj.genome.types :as types])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path Paths LinkOption OpenOption)
           (java.nio.file.attribute FileAttribute)))

;; --- fixture and temp-dir helpers ----------------------------------------

(defn- fixture-root
  "The bundle directory for a named fixture under test/fixtures/genomes."
  [name]
  (.toPath (io/file (io/resource (str "fixtures/genomes/" name)))))

(defn- temp-dir!
  ^Path []
  (Files/createTempDirectory "evoclj-load-test" (make-array FileAttribute 0)))

(defn- write-text-file!
  "Write `content` (UTF-8) to `dir`/`rel`, creating parent directories."
  [^Path dir rel ^String content]
  (let [p (.resolve dir rel)]
    (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
    (Files/write p (.getBytes content StandardCharsets/UTF_8)
                 (make-array OpenOption 0))
    p))

(defn- try-create-symlink!
  "Best-effort Files/createSymbolicLink. Returns false when the host
  refuses (Windows hosts without Developer Mode or symlink privileges)."
  [^Path target ^Path link]
  (try
    (Files/createSymbolicLink link target (make-array FileAttribute 0))
    true
    (catch Exception _ false)))

(defn- delete-recursively! [^Path dir]
  (when (Files/exists dir (make-array LinkOption 0))
    (let [f (.toFile dir)]
      (when (.isDirectory f)
        (doseq [c (.listFiles f)]
          (delete-recursively! (.toPath c))))
      (Files/deleteIfExists dir))))

(def ^:private default-modules
  {:topology "topology.edn"
   :models "models.edn"
   :memory "memory.edn"
   :evolution "evolution.edn"})

(defn- manifest-map
  "The required v1 manifest shape, with module paths overridable."
  ([] (manifest-map default-modules))
  ([modules]
   {:genome/format 1
    :agent/id :main
    :agent/entry :graph/main
    :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
    :modules modules
    :capabilities/requested #{:model/call}
    :evolution {:max-risk :behavioral
                :mutable #{:parameters :prompts :skills :programs}}
    :metadata {:name "seed-agent" :description "minimal fixture"}}))

(defn- write-manifest! [dir modules]
  (write-text-file! dir (Paths/get "manifest.edn" (make-array String 0))
                    (pr-str (manifest-map modules))))

(defn- write-module! [dir name content]
  (write-text-file! dir (Paths/get name (make-array String 0)) content))

(defn- write-minimal-bundle! [dir]
  (write-manifest! dir default-modules)
  (write-module! dir "topology.edn" "{:graph/id :graph/main}\n")
  (write-module! dir "models.edn" "{:models {}}\n")
  (write-module! dir "memory.edn" "{:memory {}}\n")
  (write-module! dir "evolution.edn" "{:evolution {}}\n"))

(defn- load-error
  "The ExceptionInfo thrown by load-genome, or nil when it succeeds."
  [root]
  (try (load/load-genome root)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- is-load-error
  "Assert that load-genome on root throws ExceptionInfo with the given
  :error/type; returns the exception."
  [root expected-type]
  (let [e (load-error root)]
    (is (instance? clojure.lang.ExceptionInfo e) (str "expected " expected-type))
    (is (= expected-type (:error/type (ex-data e))) (pr-str (ex-data e)))
    e))

;; --- success paths --------------------------------------------------------

(deftest minimal-valid-fixture-loads-with-stable-id
  (let [g (load/load-genome (fixture-root "minimal-valid"))]
    (testing "genome id is a canonical content-addressed id"
      (is (types/genome-id? (:genome/id g))))
    (testing "the manifest is the validated on-disk manifest, unchanged"
      (let [expected (schema/validate-manifest
                      (edn/read-string
                       (slurp (io/resource "fixtures/genomes/minimal-valid/manifest.edn"))))]
        (is (= expected (:manifest g)))))
    (testing ":genome/root is preserved as the java.nio.file.Path anchor"
      (is (instance? java.nio.file.Path (:genome/root g)))
      (is (= (str (fixture-root "minimal-valid")) (str (:genome/root g)))))
    (testing "all five bundle files load with :edn kind"
      (is (= #{"manifest.edn" "topology.edn" "models.edn" "memory.edn" "evolution.edn"}
             (set (keys (:files g)))))
      (is (every? #(= :edn (:kind %)) (vals (:files g)))))
    (testing "declared file digests match their on-disk logical content"
      (doseq [f ["manifest.edn" "topology.edn" "models.edn" "memory.edn" "evolution.edn"]]
        (is (= (hash/text-digest
                (slurp (io/resource (str "fixtures/genomes/minimal-valid/" f))))
               (:digest (get-in g [:files f])))
            f)))
    (testing ":genome/id is stable across repeated loads"
      (is (apply = (map (fn [_]
                          (:genome/id (load/load-genome (fixture-root "minimal-valid"))))
                        (range 5)))))
    (testing ":genome/id equals recomputation from the returned :files"
      (let [entries (mapv (fn [[p {:keys [digest]}]] {:path p :digest digest})
                          (:files g))]
        (is (= (:genome/id g) (hash/tree-digest entries)))))))

(deftest loaded-payload-round-trips-through-edn
  (let [g (load/load-genome (fixture-root "minimal-valid"))
        payload (dissoc g :genome/root)]
    (testing "only :genome/root holds a raw Java value; the rest is EDN data"
      (is (= payload (edn/read-string (pr-str payload)))))
    (testing ":bytes are immutable vectors, never mutable byte arrays"
      (is (every? (fn [v] (and (vector? (:bytes v))
                               (every? integer? (:bytes v))))
                  (vals (:files g)))))))

(deftest undeclared-files-included-with-inferred-kinds
  (let [dir (temp-dir!)]
    (try
      (write-minimal-bundle! dir)
      (write-text-file! dir (Paths/get "notes/readme.txt" (make-array String 0))
                        "hello\n")
      (Files/createDirectories (.resolve dir (Paths/get "assets" (make-array String 0)))
                               (make-array FileAttribute 0))
      (Files/write (.resolve dir (Paths/get "assets/blob.bin" (make-array String 0)))
                   (byte-array [(byte 0) (byte 1) (byte 2)])
                   (make-array OpenOption 0))
      (let [g (load/load-genome dir)]
        (is (= :text (:kind (get-in g [:files "notes/readme.txt"]))))
        (is (= :binary (:kind (get-in g [:files "assets/blob.bin"]))))
        (is (= (hash/text-digest "hello\n")
               (:digest (get-in g [:files "notes/readme.txt"]))))
        (is (= (hash/file-digest (byte-array [(byte 0) (byte 1) (byte 2)]))
               (:digest (get-in g [:files "assets/blob.bin"])))))
      (finally (delete-recursively! dir)))))

;; --- failure cases --------------------------------------------------------

(deftest missing-manifest-rejected
  (let [dir (temp-dir!)]
    (try
      (write-module! dir "topology.edn" "{:graph/id :graph/main}\n")
      (let [e (is-load-error dir :genome/manifest-missing)]
        (is (= "manifest.edn" (:path (ex-data e)))))
      (finally (delete-recursively! dir)))))

(deftest undeclared-required-module-rejected
  (let [dir (temp-dir!)]
    (try
      (write-manifest! dir default-modules)
      (write-module! dir "models.edn" "{:models {}}\n")
      (write-module! dir "memory.edn" "{:memory {}}\n")
      (write-module! dir "evolution.edn" "{:evolution {}}\n")
      (let [e (is-load-error dir :genome/module-missing)]
        (is (= "topology.edn" (:path (ex-data e)))))
      (finally (delete-recursively! dir)))))

(deftest module-path-that-is-a-directory-rejected
  (let [dir (temp-dir!)]
    (try
      (write-minimal-bundle! dir)
      (Files/delete (.resolve dir (Paths/get "topology.edn" (make-array String 0))))
      (Files/createDirectories (.resolve dir (Paths/get "topology.edn" (make-array String 0)))
                               (make-array FileAttribute 0))
      (is-load-error dir :genome/unreadable)
      (finally (delete-recursively! dir)))))

(deftest symlink-module-rejected
  (let [dir (temp-dir!)]
    (try
      (write-minimal-bundle! dir)
      (let [real (.resolve dir (Paths/get "topology.edn" (make-array String 0)))
            link (.resolve dir (Paths/get "link.edn" (make-array String 0)))]
        (if (try-create-symlink! real link)
          (let [e (is-load-error dir :genome/symlink-rejected)]
            (is (= "link.edn" (:path (ex-data e)))))
          (testing "symlink creation unavailable on this host; skipped"
            (is true))))
      (finally (delete-recursively! dir)))))

(deftest symlink-directory-escape-rejected
  (let [dir (temp-dir!)
        outside (temp-dir!)]
    (try
      (write-minimal-bundle! dir)
      (write-text-file! outside (Paths/get "secret.edn" (make-array String 0))
                        "{:secret true}\n")
      (if (try-create-symlink! outside
                               (.resolve dir (Paths/get "escape" (make-array String 0))))
        (is-load-error dir :genome/symlink-rejected)
        (testing "symlink creation unavailable on this host; skipped"
          (is true)))
      (finally
        (delete-recursively! dir)
        (delete-recursively! outside)))))

(deftest posix-unreadable-module-rejected
  (let [dir (temp-dir!)]
    (try
      (write-minimal-bundle! dir)
      (let [p (.resolve dir (Paths/get "topology.edn" (make-array String 0)))]
        (if (try (Files/setPosixFilePermissions p #{})
                 true
                 (catch UnsupportedOperationException _ false))
          (if (Files/isReadable p)
            (testing "running with privileges that ignore file permissions; skipped"
              (is true))
            (is-load-error dir :genome/unreadable))
          (testing "POSIX permissions unsupported on this host; skipped"
            (is true))))
      (finally (delete-recursively! dir)))))

(deftest duplicate-declared-module-paths-rejected
  (let [dir (temp-dir!)]
    (try
      (write-manifest! dir {:topology "models.edn"
                            :models "models.edn"
                            :memory "memory.edn"
                            :evolution "evolution.edn"})
      (write-module! dir "models.edn" "{:models {}}\n")
      (write-module! dir "memory.edn" "{:memory {}}\n")
      (write-module! dir "evolution.edn" "{:evolution {}}\n")
      (let [e (is-load-error dir :genome/duplicate-path)]
        (is (= ["models.edn"] (:paths (ex-data e)))))
      (finally (delete-recursively! dir)))))

(defn- literal-backslash-filenames-supported?
  "Probe whether the host allows a file name containing a literal
  backslash (POSIX hosts do; Windows treats backslash as a separator,
  so the probe becomes a subdirectory instead)."
  []
  (let [dir (temp-dir!)]
    (try
      (Files/write (.resolve dir "probe\\name") (byte-array [(byte 1)])
                   (make-array OpenOption 0))
      (with-open [s (Files/newDirectoryStream dir)]
        (boolean (some #(str/includes? (str (.getFileName %)) "\\")
                       (iterator-seq (.iterator s)))))
      (catch Exception _ false)
      (finally (delete-recursively! dir)))))

(deftest duplicate-normalized-on-disk-paths-rejected
  (let [dir (temp-dir!)]
    (try
      (write-minimal-bundle! dir)
      (write-text-file! dir (Paths/get "a\\b.edn" (make-array String 0)) "{:x 1}\n")
      (write-text-file! dir (Paths/get "a/b.edn" (make-array String 0)) "{:y 2}\n")
      (if (literal-backslash-filenames-supported?)
        (let [e (is-load-error dir :genome/duplicate-path)]
          (is (= ["a/b.edn"] (:paths (ex-data e)))))
        (testing "host cannot create backslash filenames; skipped"
          (is true)))
      (finally (delete-recursively! dir)))))

(deftest path-traversal-in-manifest-rejected-before-trust
  (let [dir (temp-dir!)]
    (try
      (write-manifest! dir {:topology "../outside.edn"
                            :models "models.edn"
                            :memory "memory.edn"
                            :evolution "evolution.edn"})
      (write-module! dir "models.edn" "{:models {}}\n")
      (write-module! dir "memory.edn" "{:memory {}}\n")
      (write-module! dir "evolution.edn" "{:evolution {}}\n")
      (testing "the manifest is validated before any module path is trusted"
        (is-load-error dir :genome/schema-invalid))
      (finally (delete-recursively! dir)))))

(deftest reader-eval-in-declared-module-rejected
  (let [dir (temp-dir!)]
    (try
      (write-minimal-bundle! dir)
      (write-module! dir "topology.edn" "#=(System/exit 1)\n")
      (let [e (is-load-error dir :genome/edn-invalid)]
        (is (= "topology.edn" (:path (ex-data e)))))
      (finally (delete-recursively! dir)))))

(deftest nonexistent-root-rejected
  (let [dir (temp-dir!)]
    (try
      (let [e (is-load-error (.resolve dir "does-not-exist") :genome/root-invalid)]
        (is (= :not-found (:reason (ex-data e)))))
      (finally (delete-recursively! dir)))))

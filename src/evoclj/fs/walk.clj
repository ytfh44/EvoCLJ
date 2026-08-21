(ns evoclj.fs.walk
  "Generic safe directory walk for immutable snapshots.

  Extracted from evoclj.genome.load so that Genome and future Skill
  package snapshots share the same security rules:

  - canonical relative path (via evoclj.fs.path)
  - no symlink anywhere (file, directory, or escape)
  - no junction escape (real path must stay inside root)
  - no duplicate canonical paths
  - unreadable entry -> fail closed

  Returns pure data: [{:path \"foo/bar\" :physical-path <Path>} ...]
  No I/O beyond the walk and no hashing here."
  (:require [evoclj.fs.path :as fs-path]
            [evoclj.kernel.error :as err])
  (:import (java.nio.file FileVisitResult Files LinkOption Path SimpleFileVisitor)))

(defn- outside-bundle?
  [^Path root-real ^Path dir]
  (try
    (not (.startsWith (.toRealPath dir (make-array LinkOption 0)) root-real))
    (catch Exception _ true)))

(defn- make-walk-visitor
  [^Path root acc]
  (let [root-real (.toRealPath root (make-array LinkOption 0))]
    (proxy [SimpleFileVisitor] []
      (preVisitDirectory [dir _attrs]
        (if (outside-bundle? root-real dir)
          (throw (err/error :fs/symlink-rejected
                            "directory must not contain links outside the root"
                            {:path (str (.relativize root dir))}))
          FileVisitResult/CONTINUE))
      (visitFile [file _attrs]
        (if (Files/isSymbolicLink file)
          (throw (err/error :fs/symlink-rejected
                            "bundle must not contain symbolic links"
                            {:path (str (.relativize root file))}))
          (do (vswap! acc conj [file (str (.relativize root file))])
              FileVisitResult/CONTINUE)))
      (visitFileFailed [_file exc]
        (throw (err/error :fs/unreadable
                          "entry is unreadable"
                          {:message (.getMessage exc)}))))))

(defn- walk-bundle!
  [^Path root]
  (let [acc (volatile! [])]
    (Files/walkFileTree root (make-walk-visitor root acc))
    @acc))

(defn- normalize-files
  [walked]
  (let [normalized (mapv (fn [[file rel]]
                           {:file file :path (fs-path/normalize-relative-path rel) :physical-path file})
                         walked)
        dupes (->> normalized
                   (group-by :path)
                   (filter (fn [[_ entries]] (> (count entries) 1))))]
    (when (seq dupes)
      (throw (err/error :fs/duplicate-path
                        "duplicate normalized paths"
                        {:paths (mapv first dupes)})))
    normalized))

(defn walk-tree
  "Walk root without following symlinks and return a vector of
  {:path canonical-relative-string :physical-path Path}.

  Throws on symlink, junction escape, duplicate canonical paths, or
  unreadable entries. Pure data only."
  [root]
  (let [^Path r (cond
                  (instance? Path root) root
                  (string? root) (java.nio.file.Paths/get root (make-array String 0))
                  :else (throw (err/error :fs/root-invalid "root must be Path or string" {:path root})))]
    (when (Files/isSymbolicLink r)
      (throw (err/error :fs/root-invalid "root must not be a symbolic link" {:path (str r)})))
    (when-not (Files/isDirectory r (make-array LinkOption 0))
      (throw (err/error :fs/root-invalid "root must be an existing directory" {:path (str r)})))
    ;; use internal walk then normalize
    (let [walked (walk-bundle! r)
          normed (normalize-files walked)]
      (mapv (fn [{:keys [path physical-path]}] {:path path :physical-path physical-path}) normed))))

(defn walk-tree-genome-compat
  "Compatibility wrapper that maps :fs/* errors to :genome/* for Genome callers.
  Prefer walk-tree for new code."
  [root]
  (try
    (walk-tree root)
    (catch clojure.lang.ExceptionInfo e
      (let [t (:error/type (ex-data e))]
        (throw (ex-info (ex-message e)
                        (assoc (ex-data e)
                               :error/type (case t
                                             :fs/symlink-rejected :genome/symlink-rejected
                                             :fs/duplicate-path :genome/duplicate-path
                                             :fs/unreadable :genome/unreadable
                                             :fs/root-invalid :genome/root-invalid
                                             :fs/path-invalid :genome/path-invalid
                                             t))
                        e))))))

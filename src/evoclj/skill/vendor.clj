(ns evoclj.skill.vendor
  "Vendor external Skill snapshot revision to Genome-owned skills/.

  Principle: Evolution may only mutate Genome-owned assets. Installed
  external Skills (~/.agents/skills, project/.agents/skills discovered
  via SkillSource) are NOT part of Genome, so mutation engine never
  receives that write target. If evolution of a Skill is desired, the
  explicit flow is:

    external Skill -> vendor/fork -> Genome skills/<name>/* -> candidate-owned asset -> Mutation IR

  Vendor MUST copy the immutable snapshot revision (CAS tree id), not a
  live host path. Upstream changes after vendor therefore do not affect
  the vendored copy. The copy lives under <genome-root>/skills/<name>/;
  all bundle files participate in Genome identity (load-genome tree-digest
  includes every file), and :skills is already a declared mutable class,
  so vendored files are immediately evolvable via existing Mutation IR
  without manifest edits."
  (:require [clojure.string :as str]
            [evoclj.fs.snapshot :as snapshot]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.genome.types :as types])
  (:import (java.nio.file Files Path Paths LinkOption)
           (java.nio.file.attribute FileAttribute)
           (java.nio.charset StandardCharsets)))

(defn- coerce-path
  ^Path [x]
  (cond
    (instance? Path x) x
    (string? x) (Paths/get x (make-array String 0))
    :else (throw (err/error :skill/vendor-invalid-args "genome root must be Path or string" {:value (err/sanitize x)}))))

(defn- validate-skill-name!
  [n]
  (when-not (and (string? n) (not (str/blank? n)))
    (throw (err/error :skill/vendor-invalid-name "skill name must be non-empty string" {:skill/name n})))
  (when (or (str/includes? n "/") (str/includes? n "\\"))
    (throw (err/error :skill/vendor-invalid-name "skill name must be single path component (no slash)" {:skill/name n})))
  (when (contains? #{"." ".."} n)
    (throw (err/error :skill/vendor-invalid-name "skill name must not be '.' or '..'" {:skill/name n})))
  n)

(defn- validate-tree-id!
  [id]
  (when-not (types/artifact-id? id)
    (throw (err/error :skill/vendor-invalid-args "tree/id must be sha256:<64 hex>" {:tree/id id})))
  id)

(defn- delete-recursively!
  [^Path dir]
  (when (Files/exists dir (make-array LinkOption 0))
    (let [f (.toFile dir)]
      (when (.isDirectory f)
        (doseq [c (.listFiles f)]
          (delete-recursively! (.toPath c))))
      (Files/deleteIfExists dir))
    nil))

(defn vendor-skill!
  "Vendor an external Skill snapshot revision into the Genome.

  Copies the immutable CAS tree identified by :tree/id (produced by
  fs/snapshot and CAS), NOT any live host path, into
  <genome-root>/skills/<skill-name>/.

  opts map:
    :genome/root  Path or string of genome bundle root (required)
    :cas          CAS handle (string path or {:root ...} map) (required)
    :skill/name   skill directory name under skills/ (required, single component)
    :tree/id      CAS tree id sha256:... of the skill snapshot (required)

  Also accepts :genome-root as alias for :genome/root for convenience.
  Accepts :skill-name as alias for :skill/name and :tree-id as alias.

  Returns {:skill/name ... :tree/id ... :genome/path ... :files [...] }.

  Throws :skill/vendor-* on invalid args, missing tree, or IO failure.
  The vendored files immediately participate in Genome identity (tree-digest)
  and are mutable via the existing :skills class — no manifest change needed.
  Upstream package changes do not affect the vendored copy because the copy
  is from snapshot revision."
  [{:keys [genome/root genome-root cas skill/name skill-name tree/id tree-id] :as opts}]
  (let [root-raw (or root genome-root (:genome/root opts) (:genome-root opts))
        cas-raw cas
        name-raw (or name skill-name (:skill/name opts) (:skill-name opts))
        tree-raw (or id tree-id (:tree/id opts) (:tree-id opts))]
    (when-not root-raw
      (throw (err/error :skill/vendor-invalid-args "vendor-skill! requires :genome/root" {:opts (err/sanitize opts)})))
    (when-not cas-raw
      (throw (err/error :skill/vendor-invalid-args "vendor-skill! requires :cas" {:opts (err/sanitize opts)})))
    (when-not name-raw
      (throw (err/error :skill/vendor-invalid-args "vendor-skill! requires :skill/name" {:opts (err/sanitize opts)})))
    (when-not tree-raw
      (throw (err/error :skill/vendor-invalid-args "vendor-skill! requires :tree/id" {:opts (err/sanitize opts)})))
    (let [^Path genome-root-path (coerce-path root-raw)
          skill-name (validate-skill-name! (str name-raw))
          tree-id (validate-tree-id! (str tree-raw))]
      (when-not (Files/isDirectory genome-root-path (make-array LinkOption 0))
        (throw (err/error :skill/vendor-invalid-args "genome root must be existing directory" {:genome/root (str genome-root-path)})))
      ;; Load snapshot manifest via CAS — this is the snapshot revision, not live path.
      (let [manifest (try
                       (snapshot/load-tree cas-raw tree-id)
                       (catch clojure.lang.ExceptionInfo e
                         (throw (err/error :skill/vendor-missing-tree "CAS tree not found for tree/id" {:tree/id tree-id :cause (ex-data e)})))
                       (catch Exception e
                         (throw (err/error :skill/vendor-missing-tree "CAS tree not found" {:tree/id tree-id :message (.getMessage e)}))))
            entries (:entries manifest)]
        (when-not (map? entries)
          (throw (err/error :skill/vendor-missing-tree "snapshot manifest missing :entries" {:tree/id tree-id :manifest manifest})))
        ;; Ensure destination is under genome's skills/<name>/
        (let [skills-dir (.resolve genome-root-path (Paths/get "skills" (make-array String 0)))
              dest-skill-dir (.resolve skills-dir (Paths/get skill-name (make-array String 0)))]
          ;; ponytail: clean dest before write so removed upstream files don't linger
          (when (Files/exists dest-skill-dir (make-array LinkOption 0))
            (delete-recursively! dest-skill-dir))
          (Files/createDirectories dest-skill-dir (make-array FileAttribute 0))
          (let [written (atom [])]
            (doseq [[rel {:keys [artifact/id]}] entries]
              (when-not (string? rel)
                (throw (err/error :skill/vendor-invalid-args "manifest entry path must be string" {:path rel})))
              ;; rel was normalized by walker, but guard against escapes
              (when (or (str/starts-with? rel "/") (str/includes? rel "\\") (str/includes? rel ".."))
                (throw (err/error :skill/vendor-invalid-args "manifest entry path invalid" {:path rel})))
              (let [^Path dest (.resolve dest-skill-dir (Paths/get rel (make-array String 0)))
                    ^Path normalized (.normalize dest)]
                ;; ensure dest stays inside dest-skill-dir (and thus genome root)
                (when-not (.startsWith normalized dest-skill-dir)
                  (throw (err/error :skill/vendor-path-escape "vendored path escapes skill dir" {:path rel :dest (str normalized)})))
                (let [ba (try
                           (cas/get-bytes cas-raw id)
                           (catch Exception e
                             (throw (err/error :skill/vendor-missing-artifact "CAS artifact missing for entry" {:path rel :artifact/id id :message (.getMessage e)}))))]
                  (let [parent (.getParent dest)]
                    (when parent (Files/createDirectories parent (make-array FileAttribute 0))))
                  (Files/write dest ^bytes ba (make-array java.nio.file.OpenOption 0))
                  (swap! written conj rel))))
            {:skill/name skill-name
             :tree/id tree-id
             :genome/path (str dest-skill-dir)
             :genome/root (str genome-root-path)
             :files (sort @written)
             :manifest manifest}))))))

(ns evoclj.support.cas-tree-fixtures
  "Deterministic CAS tree fixtures for Skills/Bindings tests (WO-T3).

   CONTRACT — raw bytes, no normalization: fixture contents are written as
   exact UTF-8 bytes (Files/write with byte arrays, never text mode), so a
   content string containing \\n produces LF-only bytes on every platform,
   Windows included. Snapshots go through the production
   evoclj.fs.snapshot/snapshot-tree!, which stores every file verbatim in CAS;
   no CRLF normalization happens anywhere in this path (see
   src/evoclj/fs/snapshot.clj lines 4-6). Tests pin this contract by asserting
   golden sha256 ids computed independently over those exact bytes.

   All results are pure functions of the supplied :files bytes: same bytes in,
   same tree/id out, regardless of host, mtime, or line-ending policy."
  (:require [evoclj.fs.snapshot :as snapshot]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files OpenOption Path)
           (java.nio.file.attribute FileAttribute)))

(def ^:private default-limits
  "Generous default snapshot limits for test fixtures."
  {:max-depth 32
   :max-files 2000
   :max-total-bytes (* 20 1024 1024)
   :max-file-bytes (* 5 1024 1024)})

(defn- ->path
  [x]
  (cond
    (instance? Path x) x
    :else (.toPath (clojure.java.io/file x))))

(defn- write-entry!
  "Write `content` (String or byte[]) under dir/rel as EXACT bytes.
   Files/write with an empty OpenOption array means CREATE+TRUNCATE_EXISTING+WRITE
   in binary mode: no newline translation can occur. Returns the written Path."
  [^Path dir rel content]
  (let [ba (if (instance? String content)
             (.getBytes ^String content StandardCharsets/UTF_8)
             content)
        p (.resolve dir ^String rel)]
    (when-let [parent (.getParent p)]
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p ba (make-array OpenOption 0))
    p))

(defn make-skill-tree!
  "Materialize a deterministic skill directory and snapshot it to CAS.

   opts:
     :root  <dir>    target directory (string/File/Path); created if absent
     :files {rel-path content}   relative POSIX-style paths (\"SKILL.md\",
                      \"references/x.md\"); content is String (encoded UTF-8)
                      or byte[]; written as raw bytes — no CRLF normalization
     :cas   <handle> CAS config or root for evoclj.store.cas
     :limits (optional) overrides for snapshot limits

   Reuses the production evoclj.fs.snapshot/snapshot-tree! and returns

     {:tree/id \"sha256:...\"
      :manifest {:tree/version 1 :entries {...}}
      :content-ids {\"SKILL.md\" \"sha256:...\" ...}
      :dir <root Path>}

   Deterministic: identical :files bytes yield identical tree/id and
   content-ids on any host. The raw-bytes/no-normalization contract is pinned
   externally by golden-value tests."
  [{:keys [root files cas limits]}]
  (let [root-path (->path root)
        _         (Files/createDirectories root-path (make-array FileAttribute 0))
        _         (doseq [[rel content] files]
                    (write-entry! root-path rel content))
        res       (snapshot/snapshot-tree! root-path cas (or limits default-limits))]
    {:tree/id (:tree/id res)
     :manifest (:manifest res)
     :content-ids (into {}
                        (map (fn [{:keys [path artifact/id]}] [path id]))
                        (:entries res))
     :dir root-path}))

(defn load-back!
  "Given a CAS handle and a tree/id, return {path bytes} for every file in
   that tree's manifest (bytes are the exact stored byte arrays). Intended for
   restart/rebuild tests: after reloading a tree from CAS, compare these bytes
   against the originals."
  [cas tree-id]
  (let [manifest (snapshot/load-tree cas tree-id)]
    (into {}
          (map (fn [[path {:keys [artifact/id]}]]
                 [path (cas/get-bytes cas id)]))
          (:entries manifest))))

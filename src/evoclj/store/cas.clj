(ns evoclj.store.cas
  "Filesystem content-addressed storage (Task 5.2).

  Artifacts are stored under a root directory keyed by content hash with
  the normative physical layout:

      <root>/sha256/<ab>/<64 hex>/body
      <root>/sha256/<ab>/<64 hex>/meta.edn

  where <64 hex> is the lowercase hex digest after \"sha256:\" and <ab>
  is its first two characters (the shard). Global Constraint 21 follows:
  a payload is identified by what it IS, never by where it was put.

  `cas` arguments are either a root path (string/Path/File, verification
  off) or a config map {:root <path> :verify <boolean>} as produced by
  ->cas. With verification on, every read re-hashes the body and fails
  loudly (:store/cas-corrupt) instead of serving bytes that do not match
  their id; with verification off, reads serve stored bytes as-is.
  Verification is intentionally a READ-path concern: put-bytes! never
  re-reads what it just wrote.

  Writes are atomic: bytes are written to a temp file created in the
  SAME directory as the target (so the rename never crosses a volume
  boundary), the file is fsynced, then renamed into place with
  ATOMIC_MOVE. On this host (Windows/NTFS) Files/move ATOMIC_MOVE maps
  to MoveFileEx with MOVEFILE_REPLACE_EXISTING, which NTFS guarantees
  atomically within a volume; the directory itself is fsynced after the
  rename on POSIX only, because Windows cannot open a directory handle
  for fsync. A failed write deletes its temp file and propagates, so a
  put never leaves a partial artifact behind. Putting identical bytes
  twice yields the same id and one logical artifact: the second put is a
  no-op on the body and only rewrites meta.edn when the supplied media
  type (or size) disagrees.

  meta.edn is written ONLY for the hash being put and always carries
  that hash as :artifact/id; because the body path is derived from the
  requested id (never from meta content), meta can never overwrite or
  redirect body identity. A meta.edn that disagrees with the directory
  it lives in is rejected loudly (:store/cas-meta-mismatch).

  Typed errors: :store/cas-invalid-id, :store/cas-missing,
  :store/cas-corrupt, :store/cas-meta-missing, :store/cas-meta-corrupt,
  :store/cas-meta-mismatch."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err])
  (:import (java.io File)
           (java.nio ByteBuffer)
           (java.nio.channels FileChannel)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Path StandardCopyOption StandardOpenOption)
           (java.nio.file.attribute FileAttribute)))

(def ^:private algorithm "sha256")
(def ^:private body-name "body")
(def ^:private meta-name "meta.edn")
(def ^:private default-media-type "application/octet-stream")
(def ^:private tmp-prefix ".evoclj-")
(def ^:private tmp-suffix ".tmp")

(defn- path-exists?
  "Files/exists with the varargs LinkOption array made explicit (JDK 26
  reflection cannot resolve the 1-arg varargs form)."
  [^Path p]
  (Files/exists p (make-array LinkOption 0)))

;; --- cas values -------------------------------------------------------------

(defn- to-path
  "Coerce a string, File, or Path into a Path."
  [x]
  (cond
    (instance? Path x) x
    (instance? File x) (.toPath ^File x)
    :else (.toPath (io/file x))))

(defn ->cas
  "Return a CAS config map {:root <Path> :verify <boolean>}.

  `root` is the storage root directory (string, File, or Path).
  Verification defaults to off; pass {:verify true} to re-hash every
  body on read (see get-bytes)."
  [root & [opts]]
  {:root (to-path root)
   :verify (boolean (:verify opts))})

(defn- coerce
  "Normalize a cas argument (config map or bare root path) into a
  config map."
  [cas]
  (if (map? cas)
    {:root (to-path (:root cas))
     :verify (boolean (:verify cas))}
    (->cas cas)))

;; --- artifact paths ----------------------------------------------------------

(defn- valid-id!
  "Throw :store/cas-invalid-id unless x is a canonical
  \"sha256:<64 lowercase hex>\" artifact id."
  [x]
  (when-not (types/artifact-id? x)
    (throw (err/error :store/cas-invalid-id
                      "artifact id must be sha256:<64 lowercase hex>"
                      {:artifact/id x})))
  x)

(defn- hex-of [artifact-id]
  (subs artifact-id 7))

(defn artifact-dir
  "The artifact directory Path for `artifact-id`:
  <root>/sha256/<ab>/<64 hex>. A malformed id throws
  :store/cas-invalid-id."
  [cas artifact-id]
  (valid-id! artifact-id)
  (let [hex (hex-of artifact-id)
        root (:root (coerce cas))]
    (-> root (.resolve algorithm) (.resolve (subs hex 0 2)) (.resolve hex))))

(defn body-path
  "The body file Path for `artifact-id`."
  [cas artifact-id]
  (.resolve (artifact-dir cas artifact-id) body-name))

(defn meta-path
  "The meta.edn file Path for `artifact-id`."
  [cas artifact-id]
  (.resolve (artifact-dir cas artifact-id) meta-name))

;; --- atomic writes -----------------------------------------------------------

(defn- windows?
  []
  (str/starts-with? (System/getProperty "os.name") "Windows"))

(defn- fsync-dir!
  "Persist the rename by fsyncing the directory itself.

  Windows cannot open a directory handle for fsync, and NTFS renames
  via MoveFileEx are atomic without it, so this is skipped there. On
  POSIX the directory must be forced for the rename to survive a crash;
  failures there are loud."
  [^Path dir]
  (when-not (windows?)
    (with-open [ch (FileChannel/open dir
                                     (into-array StandardOpenOption
                                                 [StandardOpenOption/READ]))]
      (.force ch true))))

(defn- write-all!
  "Write every remaining byte of `buf` to `ch` (FileChannel.write may
  write partially)."
  [^FileChannel ch ^ByteBuffer buf]
  (loop []
    (when (.hasRemaining buf)
      (.write ch buf)
      (recur))))

(defn- atomic-write!
  "Atomically create `dir`/`name` with `ba` as its content.

  The bytes are written to a temp file created inside `dir` itself (so
  the rename never crosses a filesystem boundary), the temp file is
  fsynced, and it is moved onto the target with ATOMIC_MOVE +
  REPLACE_EXISTING. Returns the target Path. On any failure the temp
  file is deleted and the error propagates, so a caller never observes
  a partial artifact."
  [^Path dir ^String name ^bytes ba]
  (Files/createDirectories dir (make-array FileAttribute 0))
  (let [tmp (Files/createTempFile dir tmp-prefix tmp-suffix
                                  (make-array FileAttribute 0))
        target (.resolve dir name)]
    (try
      (with-open [ch (FileChannel/open tmp
                                       (into-array StandardOpenOption
                                                   [StandardOpenOption/WRITE]))]
        (write-all! ch (ByteBuffer/wrap ba))
        (.force ch true))
      (Files/move tmp target
                  (into-array StandardCopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (fsync-dir! dir)
      target
      (catch Throwable t
        (Files/deleteIfExists tmp)
        (throw t)))))

;; --- meta ---------------------------------------------------------------------

(defn- parse-meta
  "Read and validate meta.edn for `artifact-id`.

  Throws :store/cas-meta-missing when the file is absent,
  :store/cas-meta-corrupt when it is unreadable or not a map, and
  :store/cas-meta-mismatch when its :artifact/id disagrees with the id
  of the directory it lives in."
  [cas artifact-id]
  (let [p (meta-path cas artifact-id)]
    (when-not (path-exists? p)
      (throw (err/error :store/cas-meta-missing
                        "artifact body exists but meta.edn is absent"
                        {:artifact/id artifact-id})))
    (let [m (try
              (edn/read-string (slurp (str p)))
              (catch Exception e
                (throw (err/error :store/cas-meta-corrupt
                                  "meta.edn is not readable EDN"
                                  {:artifact/id artifact-id
                                   :cause (.getMessage e)}))))]
      (when-not (map? m)
        (throw (err/error :store/cas-meta-corrupt
                          "meta.edn does not contain a map"
                          {:artifact/id artifact-id :meta m})))
      (when-not (= artifact-id (:artifact/id m))
        (throw (err/error :store/cas-meta-mismatch
                          "meta.edn disagrees with the artifact directory"
                          {:artifact/id artifact-id
                           :meta/artifact-id (:artifact/id m)})))
      m)))

;; --- public API ---------------------------------------------------------------

(defn put-bytes!
  "Store `bytes` by content hash under `cas` and return
  {:artifact/id \"sha256:<64 hex>\" :size n :media-type <mt>}.

  `bytes` may be a byte array or any seq of bytes; the id is the
  SHA-256 of the exact bytes (evoclj.genome.hash/file-digest). The
  media type defaults to \"application/octet-stream\".

  Identical bytes always map to the same id and the same single logical
  artifact: when the body already exists the write is skipped, and
  meta.edn is rewritten only when it is absent or disagrees with the
  supplied media type/size. meta.edn is written exclusively for the hash
  produced by these bytes (never for any other hash), so it can never
  overwrite body identity. A body write is atomic (see atomic-write!);
  a failed put leaves nothing behind and propagates a typed error."
  [cas bytes opts]
  (let [{:keys [root]} (coerce cas)
        ba (if (bytes? bytes) bytes (byte-array bytes))
        id (hash/file-digest ba)
        size (alength ba)
        media-type (or (:media-type opts) default-media-type)
        dir (artifact-dir root id)
        meta-file (meta-path root id)]
    (when-not (path-exists? (body-path root id))
      (atomic-write! dir body-name ba))
    (let [existing (when (path-exists? meta-file)
                     (parse-meta root id))]
      (when (or (nil? existing)
                (not= media-type (:media-type existing))
                (not= size (:size existing)))
        (atomic-write! dir meta-name
                       (.getBytes (pr-str {:artifact/id id
                                           :size size
                                           :media-type media-type})
                                  StandardCharsets/UTF_8))))
    {:artifact/id id :size size :media-type media-type}))

(defn get-bytes
  "Return the body of `artifact-id` as a byte array.

  Throws :store/cas-invalid-id for a malformed id and :store/cas-missing
  when the artifact is absent. When verification is enabled on `cas`
  (->cas with {:verify true}), the body is re-hashed on every read and
  compared against the id; a mismatch throws :store/cas-corrupt carrying
  the expected and actual digests, so corrupted storage fails loudly
  instead of being served as if it were the artifact. When verification
  is off, the stored bytes are returned as-is."
  [cas artifact-id]
  (let [{:keys [verify]} (coerce cas)
        body (body-path cas artifact-id)]
    (when-not (path-exists? body)
      (throw (err/error :store/cas-missing
                        "no artifact with this id"
                        {:artifact/id artifact-id})))
    (let [ba (Files/readAllBytes body)]
      (when verify
        (let [actual (hash/file-digest ba)]
          (when-not (= artifact-id actual)
            (throw (err/error :store/cas-corrupt
                              "artifact body does not match its content id"
                              {:artifact/id artifact-id
                               :expected artifact-id
                               :actual actual})))))
      ba)))

(defn exists?
  "True when an artifact with `artifact-id` exists (its body file is
  present). A malformed id throws :store/cas-invalid-id; a missing
  artifact is simply false. No re-hashing is performed."
  [cas artifact-id]
  (path-exists? (body-path cas artifact-id)))

(defn get-meta
  "Return the parsed meta.edn map {:artifact/id ... :size ... :media-type ...}
  for `artifact-id`.

  Throws :store/cas-invalid-id for a malformed id, :store/cas-missing
  when the artifact is absent, :store/cas-meta-missing when the body
  exists without meta, :store/cas-meta-corrupt when meta.edn is
  unreadable, and :store/cas-meta-mismatch when meta.edn's
  :artifact/id disagrees with the id of the directory it lives in."
  [cas artifact-id]
  (let [body (body-path cas artifact-id)]
    (when-not (path-exists? body)
      (throw (err/error :store/cas-missing
                        "no artifact with this id"
                        {:artifact/id artifact-id})))
    (parse-meta cas artifact-id)))

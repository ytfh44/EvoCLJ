(ns evoclj.support.cas-tree-fixtures-test
  "WO-T3: deterministic CAS tree fixtures.
   Seams under test (pre-agreed by the work order):
   - evoclj.support.cas-tree-fixtures/make-skill-tree!
   - evoclj.support.cas-tree-fixtures/load-back!"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.support.cas-tree-fixtures :as fixtures]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Path)
           (java.nio.file.attribute FileAttribute)
           (java.security MessageDigest)
           (java.util Arrays)))

;; --- local helpers -----------------------------------------------------------

(defn- temp-dir!
  ^Path []
  (Files/createTempDirectory "evoclj-t3-fixtures" (make-array FileAttribute 0)))

(defn- delete-recursively!
  [^Path dir]
  (when (Files/exists dir (make-array LinkOption 0))
    (let [f (.toFile dir)]
      (when (.isDirectory f)
        (doseq [c (.listFiles f)]
          (delete-recursively! (.toPath c))))
      (Files/deleteIfExists dir))
    nil))

(defn- sha256-hex
  "Independent SHA-256 (java.security.MessageDigest, NOT evoclj.genome.hash)
   over the exact UTF-8 bytes of `s`. Returns lowercase hex."
  ^String [^String s]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (->> (.digest md (.getBytes s StandardCharsets/UTF_8))
         (map #(format "%02x" %))
         (apply str))))

(defn- sha256-id
  ^String [^String s]
  (str "sha256:" (sha256-hex s)))

(defn- bytes=
  [^bytes a ^bytes b]
  (Arrays/equals a b))

;; --- fixed fixture bytes (LF-only literals; never platform-dependent) --------

(def ^:private skill-md
  (str "---\n"
       "name: golden-skill\n"
       "description: deterministic CAS tree fixture\n"
       "---\n"
       "\n"
       "# Golden Skill\n"
       "\n"
       "Raw bytes preserved. No CRLF normalization.\n"))

(def ^:private ref-md
  (str "# Reference\n"
       "\n"
       "Fixed bytes for the golden anchor.\n"
       "Second line.\n"))

;; Golden anchors computed OUTSIDE this codebase with an independent SHA-256
;; implementation (.NET System.Security.Cryptography.SHA256) over the exact
;; UTF-8/LF bytes above. If any layer introduces CRLF normalization or encoding
;; drift these pinned ids break — that is their job.
(def ^:private golden-skill-id "sha256:74b3223c38aa5fa1d747b31472dbc38b40defd1b6b58561a1adbbc36868a449a")
(def ^:private golden-ref-id   "sha256:efea902bdf678da51b568b4eac5985ab98cd2f1a75dbdd4f8f43ef9a00c697f3")
(def ^:private golden-skill-size 132)
(def ^:private golden-ref-size   61)

;; --- 1. happy path -----------------------------------------------------------

(deftest two-file-tree-roundtrip-with-golden-ids
  (testing "double-file skill tree: ids recompute, load-back! restores exact bytes"
    (let [root    (temp-dir!)
          cas-dir (temp-dir!)
          cas     (cas/->cas (str cas-dir))]
      (try
        (let [{:keys [tree/id manifest content-ids dir]}
              (fixtures/make-skill-tree!
               {:root root
                :files {"SKILL.md" skill-md
                        "references/x.md" ref-md}
                :cas cas})]
          ;; shape of the returned map
          (is (string? id) "tree/id present")
          (is (map? manifest) "manifest present")
          (is (= root dir) ":dir echoes the snapshot root")
          ;; golden content ids: pinned external anchors AND independent digest
          (is (= golden-skill-id (get content-ids "SKILL.md"))
              "SKILL.md content id equals pinned golden value")
          (is (= golden-ref-id (get content-ids "references/x.md"))
              "references/x.md content id equals pinned golden value")
          (is (= golden-skill-id (sha256-id skill-md))
              "pinned anchor independently recomputes from raw LF bytes")
          (is (= golden-ref-id (sha256-id ref-md))
              "pinned anchor independently recomputes from raw LF bytes")
          ;; manifest carries golden sizes: raw byte counts, no CRLF inflation
          (is (= golden-skill-size (get-in manifest [:entries "SKILL.md" :size])))
          (is (= golden-ref-size (get-in manifest [:entries "references/x.md" :size])))
          (is (= golden-skill-id (get-in manifest [:entries "SKILL.md" :artifact/id])))
          (is (= golden-ref-id (get-in manifest [:entries "references/x.md" :artifact/id])))
          ;; load-back! roundtrip: exact bytes restored from CAS
          (let [back (fixtures/load-back! cas id)]
            (is (= #{"SKILL.md" "references/x.md"} (set (keys back)))
                "load-back! returns {path bytes} for every tree entry")
            (is (bytes= (.getBytes skill-md StandardCharsets/UTF_8) (get back "SKILL.md"))
                "SKILL.md bytes round-trip verbatim")
            (is (bytes= (.getBytes ref-md StandardCharsets/UTF_8) (get back "references/x.md"))
                "references/x.md bytes round-trip verbatim")))
        (finally
          (delete-recursively! root)
          (delete-recursively! cas-dir))))))

;; --- 2. branch: empty directory tree ----------------------------------------

(deftest empty-tree-is-legal-and-deterministic
  (testing "an empty skill dir snapshots to a valid manifest with a stable id"
    (let [cas-dir (temp-dir!)
          cas     (cas/->cas (str cas-dir))
          root-a  (temp-dir!)
          root-b  (temp-dir!)]
      (try
        (let [{:keys [tree/id manifest content-ids]}
              (fixtures/make-skill-tree! {:root root-a :files {} :cas cas})
              second-res (fixtures/make-skill-tree! {:root root-b :files {} :cas cas})]
          ;; legal: empty entries manifest, no content ids, load-back! is {}
          (is (= {:tree/version 1 :entries {}} manifest)
              "empty dir yields an empty-entries manifest")
          (is (= {} content-ids) "no content ids for an empty tree")
          (is (= {} (fixtures/load-back! cas id))
              "load-back! of an empty tree is the empty map")
          (is (string? id) "empty tree still has a tree/id")
          ;; deterministic: a different empty dir yields the SAME tree/id
          (is (= id (:tree/id second-res))
              "two distinct empty dirs produce the identical tree/id")
          ;; and the id recomputes independently from the canonical manifest EDN
          (is (= id (sha256-id (pr-str {:tree/version 1 :entries {}})))
              "tree/id equals independent sha256 of the manifest EDN"))
        (finally
          (delete-recursively! root-a)
          (delete-recursively! root-b)
          (delete-recursively! cas-dir))))))

;; --- 3. fault: unwritable CAS handle -----------------------------------------

(deftest unwritable-cas-error-passes-through
  (testing "a CAS handle pointing at a non-directory fails loudly, not silently"
    ;; Deterministic cross-platform denial without ACL games: occupy the CAS
    ;; root path with a regular FILE. evoclj.store.cas must create directories
    ;; under it, so put-bytes! fails; make-skill-tree! must let that storage
    ;; error pass through verbatim (transparent passthrough — never swallowed,
    ;; never converted into a fake success). The typed :store/* error classes
    ;; cover id/meta corruption; IO-level failures surface as raw
    ;; java.io.IOException (FileSystemException family).
    (let [root      (temp-dir!)
          cas-dir   (temp-dir!)
          blocker   (.resolve ^Path cas-dir "occupied")
          _         (Files/write blocker (byte-array 0) (make-array java.nio.file.OpenOption 0))
          cas       (cas/->cas (str blocker))]
      (try
        (let [e (try
                  (fixtures/make-skill-tree!
                   {:root root
                    :files {"SKILL.md" skill-md}
                    :cas cas})
                  nil
                  (catch Throwable t t))]
          (is (some? e) "must throw — silence here would corrupt every downstream test")
          (is (instance? java.io.IOException e)
              (str "storage error passes through as IOException, got: "
                   (some-> e .getClass .getName)))
          (is (not= "" (str (.getMessage ^Throwable e))) "carries a message"))
        (finally
          (delete-recursively! root)
          (delete-recursively! cas-dir))))))

;; --- 4. concurrency: same content twice -> one tree/id -----------------------

(deftest concurrent-snapshots-agree-on-one-tree-id
  (testing "two concurrent snapshots of identical content share one tree/id"
    ;; Each racer gets its own CAS root, mirroring the mitigation adopted by
    ;; evoclj.adversarial.concurrency-test (see its namespace docstring,
    ;; STORAGE FINDING): two concurrent puts of the SAME content race
    ;; Files/move (ATOMIC_MOVE + REPLACE_EXISTING) onto one body path and the
    ;; loser throws AccessDeniedException on Windows/NTFS. That is a known
    ;; :store finding, out of scope for T3 (src/ is frozen); what T3 pins
    ;; here is fixture-level determinism: identity is a pure function of the
    ;; bytes even when snapshots race — no mtime/host/order state leaks in.
    (let [root-a  (temp-dir!)
          root-b  (temp-dir!)
          cas-a   (temp-dir!)
          cas-b   (temp-dir!)
          files   {"SKILL.md" skill-md "references/x.md" ref-md}]
      (try
        ;; deref propagates worker exceptions: any failure fails loudly
        (let [ra (future (fixtures/make-skill-tree!
                          {:root root-a :files files :cas (cas/->cas (str cas-a))}))
              rb (future (fixtures/make-skill-tree!
                          {:root root-b :files files :cas (cas/->cas (str cas-b))}))
              {:keys [tree/id content-ids] :as res-a} (deref ra)
              res-b (deref rb)]
          (is (= (:tree/id res-b) id)
              "racing snapshots converge on the same tree/id")
          (is (= content-ids (:content-ids res-b)) "content ids agree too")
          (is (= golden-skill-id (get content-ids "SKILL.md"))
              "golden anchor holds under concurrency")
          ;; shared-CAS idempotency, sequenced so the known Windows move race
          ;; cannot flake this suite: re-snapshotting identical content into
          ;; the SAME store returns the identical tree/id and leaves one
          ;; logical artifact per distinct content
          (let [cas-dir (temp-dir!)
                cas     (cas/->cas (str cas-dir))
                first!  (fixtures/make-skill-tree! {:root root-a :files files :cas cas})]
            (try
              (let [second! (fixtures/make-skill-tree!
                             {:root root-b :files files :cas cas})]
                (is (= (:tree/id first!) (:tree/id second!))
                    "sequential re-snapshot into one CAS is idempotent")
                (is (cas/exists? cas golden-skill-id))
                (is (cas/exists? cas golden-ref-id)))
              (finally
                (delete-recursively! cas-dir)))))
        (finally
          (delete-recursively! root-a)
          (delete-recursively! root-b)
          (delete-recursively! cas-a)
          (delete-recursively! cas-b))))))

;; --- 5. regression: golden values ARE the anchors -----------------------------

(deftest golden-values-are-regression-anchors
  (testing "pinned ids recompute independently AND reproduce in a fresh run"
    (let [root    (temp-dir!)
          cas-dir (temp-dir!)
          cas     (cas/->cas (str cas-dir))]
      (try
        ;; independent MessageDigest over raw LF bytes agrees with the pins
        (is (= golden-skill-id (sha256-id skill-md)))
        (is (= golden-ref-id (sha256-id ref-md)))
        ;; a fresh fixture run reproduces them byte-for-byte
        (let [{:keys [content-ids]} 
              (fixtures/make-skill-tree!
               {:root root
                :files {"SKILL.md" skill-md "references/x.md" ref-md}
                :cas cas})]
          (is (= golden-skill-id (get content-ids "SKILL.md")))
          (is (= golden-ref-id (get content-ids "references/x.md"))))
        (finally
          (delete-recursively! root)
          (delete-recursively! cas-dir))))))

;; --- 6. contract: docstrings declare the raw-bytes/no-normalization rule ------

(deftest docstrings-declare-raw-bytes-no-normalization-contract
  (testing "the namespace and both public vars state the contract in prose"
    (let [ns-doc    (str (:doc (meta (find-ns 'evoclj.support.cas-tree-fixtures))))
          make-doc  (str (:doc (meta #'fixtures/make-skill-tree!)))
          load-doc  (str (:doc (meta #'fixtures/load-back!)))]
      (is (str/includes? ns-doc "raw bytes") "namespace declares raw-bytes contract")
      (is (str/includes? ns-doc "no CRLF normalization") "namespace rules out CRLF normalization")
      (is (str/includes? make-doc "raw bytes") "make-skill-tree! docstring declares raw bytes")
      (is (str/includes? make-doc "no CRLF normalization") "make-skill-tree! docstring rules out CRLF normalization")
      (is (str/includes? load-doc "exact") "load-back! promises exact stored bytes"))))

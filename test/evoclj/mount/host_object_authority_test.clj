(ns evoclj.mount.host-object-authority-test
  "Audit item 5 — filesystem authority is about OBJECTS, not lexical strings.

  A lease covering a host directory must NEVER authorize through a symlink,
  junction, or reparse point planted inside the grant, even though the request
  path is lexically inside the grant scope. Every denial below is asserted
  end-to-end through the provider (authorize-then-act), not just as a path
  comparison."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [evoclj.helpers :as h]
            [evoclj.mount.backend :as backend]
            [evoclj.mount.filesystem :as fs]
            [evoclj.capability.grant :as grant]
            [evoclj.capability.resource-kind :as rk]
            [evoclj.fs.resolve :as fsr]
            [evoclj.store.cas :as cas]
            [evoclj.fs.snapshot :as snap])
  (:import (java.nio.file Files LinkOption Path)
           (java.nio.file.attribute FileAttribute)
           (java.nio.charset StandardCharsets)))

;; --- fixtures ---------------------------------------------------------------

(def ^:private principal
  {:principal/type :session :session/id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"})

(defn- fresh-lease
  [mount-id path actions]
  (let [issued (java.util.Date.)]
    {:cap/id (random-uuid)
     :principal principal
     :resource {:kind :filesystem/path :mount/id mount-id :path path}
     :actions (set actions)
     :issued-at issued
     :expires-at (java.util.Date. (+ (.getTime issued) 3600000))
     :constraints {}}))

(defn- fresh-opts
  [lease]
  {:leases [lease] :principal principal :now (java.util.Date.)})

(defn- host-fixture
  "Host-directory mount rooted at dir, served by one provider with a
  full-access whole-mount lease."
  [^Path dir]
  (let [mount-id [:workspace "host-object-test"]
        mount (backend/make-host-mount mount-id (str dir))
        reg (backend/create-registry)
        _ (backend/register-mount! reg mount)
        provider (fs/make-provider reg)
        lease (fresh-lease mount-id "" #{:read :list :stat :write :create :delete})]
    {:provider provider :mount-id mount-id :lease lease}))

(defn- err-type
  [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (:error/type (ex-data e)))))

(defn- windows?
  []
  (str/starts-with? (str/lower-case (System/getProperty "os.name")) "windows"))

(defn- try-create-junction!
  "Best-effort Windows directory junction (needs no privilege, unlike a
  symlink). Returns true when created."
  [^Path target ^Path link]
  (try
    (zero? (:exit (shell/sh "cmd" "/c" "mklink" "/J" (str link) (str target))))
    (catch Exception _ false)))

(defn- make-escape-link!
  "Plant link -> target. Returns :symlink, :junction, or nil when the host
  refuses both (caller must take the skip branch)."
  [^Path target ^Path link]
  (cond
    (h/try-create-symlink! target link) :symlink
    (and (windows?) (try-create-junction! target link)) :junction
    :else nil))

(defn- cleanup-link-tree!
  "Delete the planted link FIRST (a junction is not a symlink: a naive
  recursive delete would descend into the outside target), then both roots."
  [^Path link ^Path grant-dir ^Path outside-dir]
  (try (Files/deleteIfExists link) (catch Exception _ nil))
  (h/delete-recursively! grant-dir)
  (h/delete-recursively! outside-dir))

(defn- fwd
  "Forward-slash form of a path string (resource-kind lexical helpers are
  slash-oriented; NIO accepts forward slashes on every platform)."
  [^Path p]
  (str/replace (str p) "\\" "/"))

;; --- end-to-end: escape through a planted link is denied --------------------

(deftest symlink-inside-grant-pointing-outside-denies-read
  (let [grant-dir (h/temp-dir!)
        outside-dir (h/temp-dir!)]
    (try
      (h/write-text! outside-dir "secret.txt" "top-secret")
      (let [link (.resolve grant-dir "link")
            kind (make-escape-link! outside-dir link)]
        (if (nil? kind)
          (testing "link creation unavailable on this host; skipped" (is true))
          (let [{:keys [provider mount-id lease]} (host-fixture grant-dir)
                opts (fresh-opts lease)]
            (testing "lexically-inside request that resolves outside is denied, never served"
              (is (thrown? clojure.lang.ExceptionInfo
                           (fs/provider-read provider mount-id "link/secret.txt" opts))))
            (testing "the denial is a typed fail-closed error"
              (is (contains? #{:filesystem/symlink-rejected :filesystem/path-outside-mount}
                             (err-type #(fs/provider-read provider mount-id "link/secret.txt" opts)))))
            (testing "the outside target was never exfiltrated"
              (is (= "top-secret"
                     (String. (Files/readAllBytes (.resolve outside-dir "secret.txt"))
                              StandardCharsets/UTF_8)))))))
      (finally
        (cleanup-link-tree! (.resolve grant-dir "link") grant-dir outside-dir)))))

(deftest symlink-inside-grant-pointing-outside-denies-write-stat-and-list
  (let [grant-dir (h/temp-dir!)
        outside-dir (h/temp-dir!)]
    (try
      (h/write-text! outside-dir "secret.txt" "top-secret")
      (let [link (.resolve grant-dir "link")
            kind (make-escape-link! outside-dir link)]
        (if (nil? kind)
          (testing "link creation unavailable on this host; skipped" (is true))
          (let [{:keys [provider mount-id lease]} (host-fixture grant-dir)
                opts (fresh-opts lease)
                payload (.getBytes "evil" StandardCharsets/UTF_8)]
            (testing "write through the link is denied before any effect"
              (is (contains? #{:filesystem/symlink-rejected :filesystem/path-outside-mount}
                             (err-type #(fs/provider-write provider mount-id "link/secret.txt" payload opts)))))
            (testing "stat through the link is denied"
              (is (contains? #{:filesystem/symlink-rejected :filesystem/path-outside-mount}
                             (err-type #(fs/provider-stat provider mount-id "link/secret.txt" opts)))))
            (testing "list through the link is denied"
              (is (contains? #{:filesystem/symlink-rejected :filesystem/path-outside-mount}
                             (err-type #(fs/provider-list provider mount-id "link" opts)))))
            (testing "no effect leaked outside the grant"
              (is (= "top-secret"
                     (String. (Files/readAllBytes (.resolve outside-dir "secret.txt"))
                              StandardCharsets/UTF_8)))
              (is (not (Files/exists (.resolve outside-dir "evil.txt")
                                     (make-array LinkOption 0))))))))
      (finally
        (cleanup-link-tree! (.resolve grant-dir "link") grant-dir outside-dir)))))

(deftest scoped-grant-is-anchored-to-the-granted-object
  (let [grant-dir (h/temp-dir!)
        outside-dir (h/temp-dir!)]
    (try
      (h/write-text! outside-dir "secret.txt" "top-secret")
      (Files/createDirectories (.resolve grant-dir "sub") (make-array FileAttribute 0))
      (h/write-text! grant-dir "sub/real.txt" "real")
      (h/write-text! grant-dir "other.txt" "other")
      (let [link (.resolve (.resolve grant-dir "sub") "link")
            kind (make-escape-link! outside-dir link)]
        (if (nil? kind)
          (testing "link creation unavailable on this host; skipped" (is true))
          (let [mount-id [:workspace "scoped-grant-test"]
                mount (backend/make-host-mount mount-id (str grant-dir))
                reg (backend/create-registry)
                _ (backend/register-mount! reg mount)
                provider (fs/make-provider reg)
                lease (fresh-lease mount-id "sub" #{:read :list :stat :write :create :delete})
                opts (fresh-opts lease)]
            (testing "a legitimate file under the granted scope is still served"
              (is (= "real" (String. (fs/provider-read provider mount-id "sub/real.txt" opts)))))
            (testing "the link escape under the granted scope is denied"
              (is (contains? #{:filesystem/symlink-rejected :filesystem/path-outside-mount}
                             (err-type #(fs/provider-read provider mount-id "sub/link/secret.txt" opts)))))
            (testing "a path outside the granted scope is denied as ungranted"
              (is (= :capability/denied
                     (err-type #(fs/provider-read provider mount-id "other.txt" opts))))))))
      (finally
        (cleanup-link-tree! (.resolve (.resolve grant-dir "sub") "link") grant-dir outside-dir)))))

;; --- lexical escapes and legitimate access are unchanged ---------------------

(deftest dotdot-escape-still-denied
  (let [grant-dir (h/temp-dir!)]
    (try
      (let [{:keys [provider mount-id lease]} (host-fixture grant-dir)
            opts (fresh-opts lease)]
        (is (= :filesystem/path-outside-mount
               (err-type #(fs/provider-read provider mount-id "../shadow.txt" opts)))))
      (finally
        (h/delete-recursively! grant-dir)))))

(deftest legitimate-nested-file-under-grant-still-allowed
  (let [grant-dir (h/temp-dir!)]
    (try
      (let [{:keys [provider mount-id lease]} (host-fixture grant-dir)
            opts (fresh-opts lease)
            payload (.getBytes "hello" StandardCharsets/UTF_8)]
        (is (some? (fs/provider-create provider mount-id "sub/real.txt" payload opts)))
        (is (= "hello" (String. (fs/provider-read provider mount-id "sub/real.txt" opts))))
        (is (= :file (:type (fs/provider-stat provider mount-id "sub/real.txt" opts))))
        (is (= [{:path "sub/real.txt" :name "real.txt" :type :file}]
               (mapv #(select-keys % [:path :name :type])
                     (fs/provider-list provider mount-id "sub" opts)))))
      (finally
        (h/delete-recursively! grant-dir)))))

(deftest virtual-tree-behavior-unchanged
  (testing "CAS-tree mounts stay purely lexical: no links can exist, reads work, escapes fail"
    (let [skill-dir (h/temp-dir!)
          cas-dir (h/temp-dir!)]
      (try
        (h/write-text! skill-dir "SKILL.md" "skill")
        (h/write-text! skill-dir "references/nested.md" "nested")
        (let [cas (cas/->cas (.toString cas-dir))
              snap-res (snap/snapshot-tree! (.toString skill-dir) cas {})
              mount-id [:skill "demo" (:tree/id snap-res)]
              mount (backend/make-skill-mount mount-id cas (:tree/id snap-res))
              reg (backend/create-registry)
              _ (backend/register-mount! reg mount)
              provider (fs/make-provider reg)
              lease (fresh-lease mount-id "" #{:read :list :stat})
              opts (fresh-opts lease)]
          (is (= "skill" (String. (fs/provider-read provider mount-id "SKILL.md" opts))))
          (is (= "nested" (String. (fs/provider-read provider mount-id "references/nested.md" opts))))
          (is (rk/covers-resource? (:resource lease)
                                   {:kind :filesystem/path :mount/id mount-id :path "references/nested.md"}
                                   :read))
          (is (= :filesystem/path-outside-mount
                 (err-type #(fs/provider-read provider mount-id "../etc/passwd" opts)))))
        (finally
          (h/delete-recursively! skill-dir)
          (h/delete-recursively! cas-dir))))))

;; --- descriptor level: host-object evidence ----------------------------------

(deftest host-escape-evidence-denies-descriptor-cover
  (let [grant-dir (h/temp-dir!)
        outside-dir (h/temp-dir!)]
    (try
      (h/write-text! outside-dir "secret.txt" "top-secret")
      (h/write-text! grant-dir "real.txt" "real")
      (let [grant (fwd grant-dir)
            link (.resolve grant-dir "link")
            kind (make-escape-link! outside-dir link)]
        (if (nil? kind)
          (testing "link creation unavailable on this host; skipped" (is true))
          (do
          (testing "proven realpath escape denies the :filesystem cover"
            (is (not (rk/covers-resource? {:kind :filesystem :path grant}
                                          {:kind :filesystem :path (str grant "/link/secret.txt")}
                                          :read))))
          (testing "a legitimate nested host file is still covered lexically"
            (is (rk/covers-resource? {:kind :filesystem :path grant}
                                     {:kind :filesystem :path (str grant "/real.txt")}
                                     :read)))
          (testing "an unverifiable (nonexistent) lexically-inside path keeps the lexical answer"
            (is (rk/covers-resource? {:kind :filesystem :path grant}
                                     {:kind :filesystem :path (str grant "/not/there-yet.txt")}
                                     :read)))
          (testing "the strict object decision requires a provably-inside object"
            (is (rk/covers-host-object? grant (str grant "/real.txt")))
            (is (not (rk/covers-host-object? grant (str grant "/link/secret.txt"))))
            (is (not (rk/covers-host-object? grant (str grant "/not/there-yet.txt"))))))))
      (finally
        (cleanup-link-tree! (.resolve grant-dir "link") grant-dir outside-dir)))))

(deftest symlinked-grant-root-is-denied
  (testing "a lease whose GRANT ROOT is itself a symlink never covers anything
            under it — the root's own links are followed by toRealPath, so
            checking only the request chain silently authorized the link target"
    (let [work (h/temp-dir!)
          outside (h/temp-dir!)]
      (try
        (h/write-text! outside "inside.txt" "secret")
        (h/write-text! work "real.txt" "real")
        (let [link (.resolve work "link")
              kind (make-escape-link! outside link)]
          (if (nil? kind)
            (testing "link creation unavailable on this host; skipped" (is true))
            (do
              (testing "the symlinked grant root is recognized as a link"
                (is (fsr/grant-root-symlink? (.toString link))))
              (testing "a real directory grant root is not"
                (is (not (fsr/grant-root-symlink? (.toString work)))))
              (testing "covers? is FALSE for a request under a symlinked grant root"
                (is (not (rk/covers-resource? {:kind :filesystem :path (.toString link)}
                                              {:kind :filesystem :path (str link "/inside.txt")}
                                              :read))))
              (testing "grant/covers? (the production path) is FALSE as well"
                (is (not (grant/covers?
                          {:resource {:kind :filesystem :path (.toString link)} :actions #{:read}}
                          {:resource {:kind :filesystem :path (str link "/inside.txt")}
                           :actions #{:read}}))))
              (testing "the bare :filesystem/path kind denies it as well"
                (is (not (rk/covers-resource? {:kind :filesystem/path :path (.toString link)}
                                              {:kind :filesystem/path :path (str link "/inside.txt")}
                                              :read))))
              (testing "the strict host-object decision denies it too"
                (is (not (rk/covers-host-object? (.toString link) (str link "/inside.txt")))))
              (testing "authorize-host-absolute! fails closed on a symlinked grant"
                (is (= :filesystem/symlink-rejected
                       (err-type #(fsr/authorize-host-absolute! (.toString link)
                                                            (str link "/inside.txt"))))))
              (testing "control: a real grant root still covers a real nested file
                        and still denies a link in the request chain"
                (is (rk/covers-resource? {:kind :filesystem :path (.toString work)}
                                         {:kind :filesystem :path (str work "/real.txt")}
                                         :read))
                (is (not (rk/covers-resource? {:kind :filesystem :path (.toString work)}
                                              {:kind :filesystem :path (str work "/link/inside.txt")}
                                              :read)))))))
        (finally
          (cleanup-link-tree! (.resolve work "link") work outside)
          (h/delete-recursively! work)
          (h/delete-recursively! outside))))))

(deftest lexically-outside-request-is-rejected-by-the-lexical-gate
  (testing "authorize-host-absolute! bounds the namespace lexically BEFORE any
            realpath work: a request sharing no path prefix with the grant is
            denied outright. Containment must not be left to relativize +
            canonical-segments! alone — a cross-drive request makes relativize
            throw IllegalArgumentException, which proven-host-escape? catches
            into \"no escape proven\" and falls through to the lexical answer"
    (let [work (h/temp-dir!)
          elsewhere (h/temp-dir!)]
      (try
        (h/write-text! work "real.txt" "real")
        (h/write-text! elsewhere "secret.txt" "top-secret")
        (let [grant-abs (str (.resolve work "grant"))]
          (Files/createDirectories (.resolve work "grant")
                                   (make-array FileAttribute 0))
          (testing "a request outside the grant prefix is denied as path-outside-mount"
            (is (= :filesystem/path-outside-mount
                   (err-type #(fsr/authorize-host-absolute!
                              grant-abs
                              (str elsewhere "/secret.txt"))))))
          (testing "and so is a request that walks out of it with .."
            (is (= :filesystem/path-outside-mount
                   (err-type #(fsr/authorize-host-absolute!
                              grant-abs
                              (str grant-abs "/../elsewhere/secret.txt"))))))
          (testing "control: a lexically-inside request still resolves"
            (is (some? (fsr/authorize-host-absolute! grant-abs
                                                  (str grant-abs "/real.txt")))))
          (testing "a CROSS-ROOT request is denied by the gate itself. This is the
                    case that cannot be left to relativize: across roots
                    Path.relativize throws, and proven-host-escape? swallows
                    that into \"no escape proven\", so without the gate the
                    decision falls through to the lexical answer instead of
                    an explicit deny"
            (let [roots (mapv str (.getRootDirectories
                                    (java.nio.file.FileSystems/getDefault)))
                  same-root (subs (str work) 0 2)
                  other-root (first (remove #(str/starts-with? % same-root) roots))]
              (if (nil? other-root)
                (testing "single-root host; cross-root case skipped" (is true))
                (is (= :filesystem/path-outside-mount
                       (err-type #(fsr/authorize-host-absolute!
                                  (str same-root "\\grant")
                                  (str other-root "\\elsewhere\\secret.txt"))))))))
          (testing "control: the gate denies the descriptor cover too, without
                    any symlink planted anywhere"
            (is (not (rk/covers-resource? {:kind :filesystem :path grant-abs}
                                          {:kind :filesystem :path (str elsewhere "/secret.txt")}
                                          :read)))))
        (finally
          (h/delete-recursively! work)
          (h/delete-recursively! elsewhere))))))

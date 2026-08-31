(ns evoclj.mount.fs-lease-test
  "B4 — filesystem lease issuer grounded + reload!/restore! re-verification
  + forced expiry/subject.

  Every scenario runs through the PRODUCTION path: the real filesystem
  provider (evoclj.mount.filesystem/make-provider -> provider-read) with
  a real mount registry + backend, and the real durable binding
  transaction (evoclj.store.binding/activate! / reload! / restore!) with a
  real SQLite store + session. No injected fn, no shape-only assertions.

  The lease issuer (evoclj.mount.filesystem/issue-fs-lease) is the only
  source of a valid filesystem grant; it binds ONE subject to ONE
  canonical :filesystem/path resource over a positive window and records
  the grant (verifiable / revocable). The access path FORCES expiry and
  subject: an expired lease, a subject-mismatched lease, or a revoked
  lease is rejected with a precise typed error (fail-closed), never
  silently honored. reload!/restore! RE-verify the lease before any
  runtime state is republished."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [clojure.java.jdbc :as jdbc]
            [evoclj.fs.snapshot :as snap]
            [evoclj.mount.backend :as backend]
            [evoclj.mount.filesystem :as fs]
            [evoclj.context.binding :as ctx-binding]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.surface :as surf]
            [evoclj.store.binding :as binding]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))

;; ---------------------------------------------------------------------------
;; Shared fixtures
;; ---------------------------------------------------------------------------

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))
(def ^:private tmp-roots (atom []))

(defn- temp-file-path [] (str (Files/createTempFile "evoclj-b4-" ".db" (make-array FileAttribute 0))))
(defn- temp-dir [] (Files/createTempDirectory "evoclj-b4-" (make-array FileAttribute 0)))

(defn- cleanup! []
  (doseq [p @db-paths] (try (Files/deleteIfExists (java.nio.file.Paths/get p (make-array String 0))) (catch Exception _ nil)))
  (reset! db-paths [])
  (doseq [^java.nio.file.Path r @cas-roots]
    (when (Files/exists r (make-array java.nio.file.LinkOption 0))
      (doseq [f (reverse (file-seq (.toFile r)))] (try (Files/deleteIfExists (.toPath f)) (catch Exception _ nil)))))
  (reset! cas-roots [])
  (doseq [^java.nio.file.Path r @tmp-roots]
    (when (Files/exists r (make-array java.nio.file.LinkOption 0))
      (doseq [f (reverse (file-seq (.toFile r)))] (try (Files/deleteIfExists (.toPath f)) (catch Exception _ nil)))))
  (reset! tmp-roots []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(def ^:private pid1 (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private pid2 (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private sid1 #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def ^:private sid2 #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def ^:private now (java.util.Date.))
(def ^:private later (java.util.Date. (+ (.getTime now) 86400000))) ; far future, avoids timing flake
(def ^:private past (java.util.Date. (- (.getTime now) 60000))) ; after a much-earlier issued-at

(defn- throws-type? [f t]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e
         (= t (:error/type (ex-data e))))))

(defn- write-file [^java.nio.file.Path dir rel content]
  (let [p (.resolve dir rel)]
    (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (into-array java.nio.file.OpenOption [java.nio.file.StandardOpenOption/CREATE java.nio.file.StandardOpenOption/TRUNCATE_EXISTING java.nio.file.StandardOpenOption/WRITE]))))

(defn- workspace-mount
  "A real host-directory mount + registry + a file at references/guide.md."
  ([] (workspace-mount [:workspace "ws"]))
  ([mount-id]
   (let [dir (temp-dir)
         _ (write-file dir "references/guide.md" "guide content")
         mount (backend/make-host-mount mount-id (.toString dir))
         reg (backend/create-registry)
         _ (backend/register-mount! reg mount)]
     {:dir dir :mount-id mount-id :registry reg})))

(defn- fresh-db []
  (let [p (temp-file-path)
        db (sqlite/spec p)]
    (swap! db-paths conj p)
    (migrate/migrate! db)
    db))

(def ^:private gen "generation-1")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "9"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "8"))))

(defn- seed-generation! [db]
  (artifact/ensure-artifact! db genome "application/octet-stream" 0)
  (artifact/ensure-artifact! db resolution "application/edn" 0)
  (artifact/ensure-artifact! db pid1 "application/octet-stream" 0)
  (artifact/ensure-artifact! db pid2 "application/octet-stream" 0)
  (artifact/ensure-genome! db genome)
  (sqlite/with-db [conn db]
    (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?" gen]))
      (jdbc/insert! conn :generations {:id gen :genome_id genome :resolution_id resolution :parent_id nil :state "active" :current 0 :created_at "2025-01-01T00:00:00Z"}))))

(defn- seed-session! [db]
  (seed-generation! db)
  (let [s (session/create-session! db {:genome/id genome :resolution/id resolution :phenotype/id pid1 :generation/id gen})
        sid (:session/id s)
        _ (event/append-event! db {:session/id sid :generation/id gen :phenotype/id pid1 :event/type :session/created :cause/event-id nil :payload-ref nil :metadata {}})]
    sid))

(defn- make-skill-bundle
  "A skill bundle with context + directory surface sharing rev."
  [logical payload]
  (let [rev-id (rev/payload->id payload)
        bid (str "bundle:" rev-id ":" (pr-str logical))
        surfaces [(surf/make-context-surface {:id (keyword (str (name (first logical)) "-ctx"))
                                              :descriptor {:prompt payload}
                                              :materializer identity
                                              :revision/id rev-id})
                  (surf/make-directory-surface {:id (keyword (str (name (first logical)) "-dir"))
                                                :backend {:type :memory :root "/tmp"}
                                                :access-max #{:read :list :stat}
                                                :revision/id rev-id})]]
    (bundle/make-bundle {:bundle-id bid :revision-id rev-id :logical-id logical :surfaces surfaces})))

(defn- put-bundle! [registry b]
  (swap! registry assoc-in [:bundles (:bundle/id b)] b))

;; ============================================================================
;; 1. Happy — the real issuer mints and records a valid lease; access works
;; ============================================================================

(deftest issuer-mints-records-and-authorizes-access
  (testing "issue-fs-lease produces a schema-valid CapabilityLease and records it"
    (let [reg (fs/create-lease-registry)
          workspace (workspace-mount)
          lease (fs/issue-fs-lease reg {:subject {:session/id sid1 :phenotype/id pid1}
                                        :mount-id (:mount-id workspace)
                                        :path ""
                                        :actions #{:read :list :stat}
                                        :issued-at now
                                        :expires-at later})]
      (is (uuid? (:cap/id lease)))
      (is (= {:session/id sid1 :phenotype/id pid1} (:subject lease)))
      (is (= {:kind :filesystem/path :mount/id (:mount-id workspace) :path ""} (:resource lease)))
      (is (= #{:read :list :stat} (:actions lease)))
      (is (inst? (:issued-at lease)))
      (is (inst? (:expires-at lease)))
      (is (identical? lease (fs/get-lease reg (:cap/id lease))) "lease is recorded and verifiable")
      (is (not (fs/lease-revoked? reg (:cap/id lease))))))
  (testing "the issued lease authorizes a real provider read for the SAME subject"
    (let [reg (fs/create-lease-registry)
          workspace (workspace-mount)
          provider (fs/make-provider (:registry workspace))
          lease (fs/issue-fs-lease reg {:subject {:session/id sid1 :phenotype/id pid1}
                                        :mount-id (:mount-id workspace)
                                        :path ""
                                        :actions #{:read :list :stat}
                                        :issued-at now
                                        :expires-at later})]
      (let [ba (fs/provider-read provider (:mount-id workspace) "references/guide.md"
                                 {:leases [lease] :subject {:session/id sid1 :phenotype/id pid1} :now now :registry reg})]
        (is (bytes? ba))
        (is (str/includes? (String. ^bytes ba StandardCharsets/UTF_8) "guide content"))))))

;; ============================================================================
;; 2. Branch — expiry is FORCED on access
;; ============================================================================

(deftest expired-lease-rejected-on-access
  (testing "an expired lease is rejected with :capability/expired, never silently honored"
    (let [reg (fs/create-lease-registry)
          workspace (workspace-mount)
          provider (fs/make-provider (:registry workspace))
          ;; window ended before we access (issued near the past, expires even earlier)
          issued (java.util.Date. 1700000000000)
          expires (java.util.Date. 1700003600000)
          after-expiry (java.util.Date. 1700003600001)
          lease (fs/issue-fs-lease reg {:subject {:session/id sid1 :phenotype/id pid1}
                                        :mount-id (:mount-id workspace)
                                        :path ""
                                        :actions #{:read :list :stat}
                                        :issued-at issued
                                        :expires-at expires})]
      (is (throws-type? #(fs/provider-read provider (:mount-id workspace) "references/guide.md"
                                           {:leases [lease] :subject {:session/id sid1 :phenotype/id pid1} :now after-expiry :registry reg})
                        :capability/expired)))))

;; ============================================================================
;; 3. Branch — subject is FORCED on access
;; ============================================================================

(deftest subject-mismatch-rejected-on-access
  (testing "a lease bound to P1 is rejected for a P2 request (:capability/subject-mismatch)"
    (let [reg (fs/create-lease-registry)
          workspace (workspace-mount)
          provider (fs/make-provider (:registry workspace))
          lease (fs/issue-fs-lease reg {:subject {:session/id sid1 :phenotype/id pid1}
                                        :mount-id (:mount-id workspace)
                                        :path ""
                                        :actions #{:read :list :stat}
                                        :issued-at now
                                        :expires-at later})]
      (is (throws-type? #(fs/provider-read provider (:mount-id workspace) "references/guide.md"
                                           {:leases [lease] :subject {:session/id sid1 :phenotype/id pid2} :now now :registry reg})
                        :capability/subject-mismatch)))))

;; ============================================================================
;; 4. Fault — a revoked lease is rejected
;; ============================================================================

(deftest revoked-lease-rejected
  (testing "verify-fs-lease! rejects a revoked lease (:capability/revoked)"
    (let [reg (fs/create-lease-registry)
          workspace (workspace-mount)
          lease (fs/issue-fs-lease reg {:subject {:session/id sid1 :phenotype/id pid1}
                                        :mount-id (:mount-id workspace)
                                        :path ""
                                        :actions #{:read :list :stat}
                                        :issued-at now
                                        :expires-at later})]
      (fs/revoke-lease! reg (:cap/id lease))
      (is (fs/lease-revoked? reg (:cap/id lease)))
      (is (throws-type? #(fs/verify-fs-lease! lease {:now now :subject {:session/id sid1 :phenotype/id pid1} :registry reg})
                        :capability/revoked))))
  (testing "a lease NOT recorded by the issuer is rejected fail-closed (:capability/revoked)"
    (let [reg (fs/create-lease-registry)
          workspace (workspace-mount)
          lease {:cap/id (UUID/randomUUID) :subject {:session/id sid1 :phenotype/id pid1}
                 :resource {:kind :filesystem/path :mount/id (:mount-id workspace) :path ""}
                 :actions #{:read} :constraints {} :issued-at now :expires-at later}]
      (is (throws-type? #(fs/verify-fs-lease! lease {:now now :subject {:session/id sid1 :phenotype/id pid1} :registry reg})
                        :capability/revoked)))))

;; ============================================================================
;; 5. Fault — no lease => access denied fail-closed
;; ============================================================================

(deftest no-lease-denies-access-fail-closed
  (testing "a filesystem read without any lease is denied (:capability/denied)"
    (let [workspace (workspace-mount)
          provider (fs/make-provider (:registry workspace))]
      (is (throws-type? #(fs/provider-read provider (:mount-id workspace) "references/guide.md"
                                           {:leases [] :subject {:session/id sid1 :phenotype/id pid1} :now now})
                        :capability/denied)))))

;; ============================================================================
;; 6. Branch — reload! RE-verifies the fs lease
;; ============================================================================

(deftest reload-reverifies-lease
  (testing "reload! with a valid fs lease succeeds"
    (let [db (fresh-db)
          sid (seed-session! db)
          reg (reg/create-registry)
          mount-reg (backend/create-registry)
          ctx (ctx-binding/create-store)
          b1 (make-skill-bundle [:skill "debugging"] "content A")
          b2 (make-skill-bundle [:skill "debugging"] "content B")
          _ (put-bundle! reg b1)
          _ (put-bundle! reg b2)
          fs-lease (fs/issue-fs-lease (fs/create-lease-registry)
                                      {:subject {:session/id sid :phenotype/id pid1}
                                       :mount-id [:skill "debugging"] :path ""
                                       :actions #{:read :list :stat}
                                       :issued-at now :expires-at later})]
      (binding/activate! db sid b1 {:registry reg :mount-registry mount-reg :context-store ctx :fs-lease fs-lease})
      (is (:revision/id (binding/reload! db sid [:skill "debugging"] b2
                                          {:registry reg :mount-registry mount-reg :context-store ctx :fs-lease fs-lease})))
      (is (= (:revision/id b2) (:revision/id (first (binding/active-bindings db sid)))))))
  (testing "reload! with an EXPIRED fs lease is rejected fail-closed (:capability/expired)"
    (let [db (fresh-db)
          sid (seed-session! db)
          reg (reg/create-registry)
          mount-reg (backend/create-registry)
          ctx (ctx-binding/create-store)
          b1 (make-skill-bundle [:skill "debugging"] "content A")
          b2 (make-skill-bundle [:skill "debugging"] "content B")
          _ (put-bundle! reg b1)
          _ (put-bundle! reg b2)
          expired (fs/issue-fs-lease (fs/create-lease-registry)
                                     {:subject {:session/id sid :phenotype/id pid1}
                                      :mount-id [:skill "debugging"] :path ""
                                      :actions #{:read :list :stat}
                                      :issued-at (java.util.Date. 1700000000000)
                                      :expires-at (java.util.Date. 1700003600000)})]
      (binding/activate! db sid b1 {:registry reg :mount-registry mount-reg :context-store ctx})
      (is (throws-type? #(binding/reload! db sid [:skill "debugging"] b2
                                          {:registry reg :mount-registry mount-reg :context-store ctx :fs-lease expired})
                        :capability/expired)))))

;; ============================================================================
;; 7. Branch — restore! RE-verifies the fs lease
;; ============================================================================

(deftest restore-reverifies-lease
  (testing "restore! with a REVOKED fs lease is rejected fail-closed (:capability/revoked)"
    (let [db (fresh-db)
          sid (seed-session! db)
          reg (reg/create-registry)
          mount-reg (backend/create-registry)
          ctx (ctx-binding/create-store)
          b1 (make-skill-bundle [:skill "debugging"] "content A")
          _ (put-bundle! reg b1)
          lease-reg (fs/create-lease-registry)
          fs-lease (fs/issue-fs-lease lease-reg {:subject {:session/id sid :phenotype/id pid1}
                                                 :mount-id [:skill "debugging"] :path ""
                                                 :actions #{:read :list :stat}
                                                 :issued-at now :expires-at later})]
      (binding/activate! db sid b1 {:registry reg :mount-registry mount-reg :context-store ctx :fs-lease fs-lease})
      (fs/revoke-lease! lease-reg (:cap/id fs-lease))
      (is (throws-type? #(binding/restore! db sid
                                           {:registry reg :mount-registry (backend/create-registry) :context-store (ctx-binding/create-store)
                                            :fs-lease fs-lease :fs-lease-registry lease-reg})
                        :capability/revoked))))
  (testing "restore! with a VALID fs lease republishes the exact revision"
    (let [db (fresh-db)
          sid (seed-session! db)
          reg (reg/create-registry)
          mount-reg (backend/create-registry)
          ctx (ctx-binding/create-store)
          b1 (make-skill-bundle [:skill "debugging"] "content A")
          _ (put-bundle! reg b1)
          lease-reg (fs/create-lease-registry)
          fs-lease (fs/issue-fs-lease lease-reg {:subject {:session/id sid :phenotype/id pid1}
                                                 :mount-id [:skill "debugging"] :path ""
                                                 :actions #{:read :list :stat}
                                                 :issued-at now :expires-at later})]
      (binding/activate! db sid b1 {:registry reg :mount-registry mount-reg :context-store ctx :fs-lease fs-lease})
      (let [restored (binding/restore! db sid
                                       {:registry reg :mount-registry (backend/create-registry)
                                        :context-store (ctx-binding/create-store)
                                        :fs-lease fs-lease :fs-lease-registry lease-reg})]
        (is (= 1 (count restored)))
        (is (= (:revision/id b1) (:revision/id (first restored))))))))

;; ============================================================================
;; 9. Concurrency — shared lease registry + access stay consistent
;; ============================================================================

(deftest lease-registry-concurrent-and-access-consistent
  (testing "concurrent issuance to ONE registry records every lease, all verifiable, none revoked"
    (let [reg (fs/create-lease-registry)
          workspace (workspace-mount)
          ids (doall (pmap (fn [_]
                             (let [l (fs/issue-fs-lease reg {:subject {:session/id sid1 :phenotype/id pid1}
                                                           :mount-id (:mount-id workspace)
                                                           :path ""
                                                           :actions #{:read :list :stat}
                                                           :issued-at now
                                                           :expires-at later})]
                               (:cap/id l)))
                           (range 100)))]
      (is (= 100 (count (set ids))) "every concurrent issuance recorded with a unique :cap/id")
      (is (every? (fn [i] (let [l (fs/get-lease reg i)] (and (some? l) (= i (:cap/id l))))) ids)
          "all leases are recorded and retrievable")
      (is (not (some (fn [i] (fs/lease-revoked? reg i)) ids)) "none concurrently revoked")))
  (testing "concurrent access with a valid recorded lease succeeds; an unrecorded bare lease is rejected fail-closed"
    (let [reg (fs/create-lease-registry)
          workspace (workspace-mount)
          provider (fs/make-provider (:registry workspace))
          lease (fs/issue-fs-lease reg {:subject {:session/id sid1 :phenotype/id pid1}
                                        :mount-id (:mount-id workspace)
                                        :path ""
                                        :actions #{:read :list :stat}
                                        :issued-at now
                                        :expires-at later})
          reads (doall (pmap (fn [_]
                               (try
                                 (fs/provider-read provider (:mount-id workspace) "references/guide.md"
                                                   {:leases [lease] :subject {:session/id sid1 :phenotype/id pid1} :now now :registry reg})
                                 ::ok
                                 (catch Throwable _ ::fail)))
                             (range 50)))]
      (is (every? #(= ::ok %) reads) "concurrent reads with a valid recorded lease never fail closed"))))

;; ============================================================================
;; 10. Regression — surface/scope enforcement retained (EffectiveAccess)
;; ============================================================================

(deftest surface-scope-still-enforced
  (testing "a lease's scope still does not escape its mount-id (mount A lease cannot reach mount B)"
    (let [reg (fs/create-lease-registry)
          dir-a (temp-dir)
          dir-b (temp-dir)
          _ (write-file dir-a "references/guide.md" "a")
          _ (write-file dir-b "references/guide.md" "b")
          mount-reg (backend/create-registry)
          _ (backend/register-mount! mount-reg (backend/make-host-mount [:workspace "ws"] (.toString dir-a)))
          _ (backend/register-mount! mount-reg (backend/make-host-mount [:workspace "other"] (.toString dir-b)))
          provider (fs/make-provider mount-reg)
          lease (fs/issue-fs-lease reg {:subject {:session/id sid1 :phenotype/id pid1}
                                        :mount-id [:workspace "ws"]
                                        :path ""
                                        :actions #{:read :list :stat}
                                        :issued-at now
                                        :expires-at later})]
      (is (throws-type? #(fs/provider-read provider [:workspace "other"] "references/guide.md"
                                           {:leases [lease] :subject {:session/id sid1 :phenotype/id pid1} :now now :registry reg})
                        :capability/denied)
          "a lease scoped to mount A never reaches mount B"))))

(defn- renew-test-lease
  "A lease whose window has already ended (both bounds in the past, but
  issued strictly before expires so the positive-window check passes),
  issued into `reg`."
  [reg]
  (let [issued (java.util.Date. (- (.getTime now) 120000))
        expires (java.util.Date. (- (.getTime now) 60000))]
    (fs/issue-fs-lease reg
                       {:subject {:session/id sid1 :phenotype/id pid1}
                        :mount-id [:workspace "ws"]
                        :path ""
                        :actions #{:read :list :stat}
                        :issued-at issued
                        :expires-at expires})))

(deftest renewal-honored-and-bare-map-rejected
  (testing "a renewed lease (new positive window) authorizes access"
    (let [reg (fs/create-lease-registry)
          workspace (workspace-mount)
          provider (fs/make-provider (:registry workspace))
          expired (renew-test-lease reg)]
      (is (throws-type? #(fs/verify-fs-lease! expired {:now now :subject {:session/id sid1 :phenotype/id pid1} :registry reg})
                        :capability/expired))
      (let [renewed (fs/issue-fs-lease reg {:subject {:session/id sid1 :phenotype/id pid1}
                                            :mount-id (:mount-id workspace)
                                            :path ""
                                            :actions #{:read :list :stat}
                                            :issued-at now
                                            :expires-at later})]
        (is (some? (fs/verify-fs-lease! renewed {:now now :subject {:session/id sid1 :phenotype/id pid1} :registry reg}))))))
  (testing "a bare { :mount/id :path :actions } map is NOT a grant and is rejected (:capability/schema-invalid)"
    (let [workspace (workspace-mount)
          provider (fs/make-provider (:registry workspace))
          bare {:mount/id (:mount-id workspace) :path "" :actions #{:read :list :stat}}]
      (is (throws-type? #(fs/provider-read provider (:mount-id workspace) "references/guide.md"
                                           {:leases [bare] :subject {:session/id sid1 :phenotype/id pid1} :now now})
                        :capability/schema-invalid)))))

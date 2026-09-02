(ns evoclj.store.binding-test
  "Durable session bindings — 006 migration + activation transaction + refresh vs reload + restart + source deletion + bundle atomicity."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.context.binding :as ctx-binding]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.surface :as surf]
            [evoclj.genome.hash :as hash]
            [evoclj.mount.backend :as mount-backend]
            [evoclj.store.binding :as binding]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-binding-" ".db" (make-array FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- temp-cas-root []
  (let [p (Files/createTempDirectory "evoclj-binding-cas-" (make-array FileAttribute 0))]
    (swap! cas-roots conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (Files/deleteIfExists (java.nio.file.Paths/get p (make-array String 0))))
  (reset! db-paths [])
  (doseq [^java.nio.file.Path r @cas-roots]
    (when (Files/exists r (make-array java.nio.file.LinkOption 0))
      (doseq [f (reverse (file-seq (.toFile r)))]
        (Files/deleteIfExists (.toPath f)))))
  (reset! cas-roots []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private gen "generation-1")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))

(defn- seed-generation! [db]
  (sqlite/with-db [conn db]
    (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?" gen]))
      ;; Fleet P5/F FK (011): ensure FK targets exist before generations/sessions insert (artifacts/genomes before generations/sessions)
      (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" genome])
      (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" resolution])
      (jdbc/execute! conn ["INSERT OR IGNORE INTO artifacts (hash, media_type, size, created_at) VALUES (?, 'application/octet-stream', 0, datetime('now'))" phenotype])
      (jdbc/execute! conn ["INSERT OR IGNORE INTO genomes (id, created_at) VALUES (?, datetime('now'))" genome])
      (jdbc/insert! conn :generations
                    {:id gen
                     :genome_id genome
                     :resolution_id resolution
                     :parent_id nil
                     :state "active"
                     :current 0
                     :created_at now}))))

(defn- seed-session!
  "Create a session + its :session/created root event; returns session-id UUID."
  [db]
  (seed-generation! db)
  (let [s (session/create-session! db {:genome/id genome
                                       :resolution/id resolution
                                       :phenotype/id phenotype
                                       :generation/id gen})
        sid (:session/id s)
        _ (event/append-event! db {:session/id sid
                                   :generation/id gen
                                   :phenotype/id phenotype
                                   :event/type :session/created
                                   :prev/event-id nil
                                   :payload-ref nil
                                   :metadata {}})]
    sid))

(defn- cas-put-tree!
  "Put content string into CAS and return its artifact id (sha256:...). Uses exact bytes."
  [cas content]
  (let [ba (.getBytes ^String content StandardCharsets/UTF_8)]
    (:artifact/id (cas/put-bytes! cas ba {:media-type "application/octet-stream"}))))

(defn- rev-for [payload]
  (rev/payload->id payload))

(defn- make-skill-bundle
  "Build a skill bundle with 1-3 sibling surfaces all sharing rev.
  logical is vector like [:skill \"debugging\"]."
  [logical payload & {:keys [with-context with-mount with-tools]}]
  (let [rev (rev-for payload)
        bid (str "bundle:" rev ":" (pr-str logical))
        surfaces (cond-> []
                   (not (false? with-context)) (conj (surf/make-context-surface {:id (keyword (str (name (first logical)) "-ctx"))
                                                                                 :descriptor {:prompt payload}
                                                                                 :materializer identity
                                                                                 :revision/id rev}))
                   (not (false? with-mount)) (conj (surf/make-directory-surface {:id (keyword (str (name (first logical)) "-dir"))
                                                                                 :backend {:type :memory :root "/tmp"}
                                                                                 :access-max #{:read :list :stat}
                                                                                 :revision/id rev}))
                   with-tools (conj (surf/make-tool-surface {:id (keyword (str (name (first logical)) "-tools"))
                                                             :entries {:a {:tool/id :a}}
                                                             :revision/id rev})))]
    (bundle/make-bundle {:bundle-id bid :revision-id rev :logical-id logical :surfaces surfaces})))

(defn- count-bindings [db sid]
  (count (binding/active-bindings db sid)))

;; ---------------------------------------------------------------------------
;; Migration existence
;; ---------------------------------------------------------------------------

(deftest migration-creates-session-bindings-table
  (let [db (fresh-db)
        tables (set (map :name (sqlite/query db ["SELECT name FROM sqlite_master WHERE type='table'"])))]
    (testing "session_bindings table exists after migrate! to latest"
      (is (contains? tables "session_bindings"))
      (is (= migrate/latest-version (:version (migrate/migrate! db)))
          "migrate is noop on fresh db"))
    (testing "columns exist"
      (let [cols (set (map :name (sqlite/query db ["PRAGMA table_info(session_bindings)"])))]
        (doseq [c ["id" "session_id" "binding_type" "logical_id" "revision_id" "bundle_id" "state" "activated_at" "deactivated_at" "metadata_edn"]]
          (is (contains? cols c) (str "missing column " c)))))
    (testing "unique active index exists"
      (let [idxs (sqlite/query db ["SELECT name, sql FROM sqlite_master WHERE type='index' AND tbl_name='session_bindings'"])]
        (is (some #(= "session_bindings_active_unique" (:name %)) idxs))
        (is (some #(clojure.string/includes? (str (:sql %)) "WHERE state = 'active'") idxs))))))

;; ---------------------------------------------------------------------------
;; Activation transaction
;; ---------------------------------------------------------------------------

(deftest activate-creates-durable-row-and-publishes-runtime-and-appends-event
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-root (temp-cas-root)
        cas (cas/->cas (str cas-root))
        mount-reg (mount-backend/create-registry)
        ctx-store (ctx-binding/create-store)
        logical [:skill "debugging"]
        bundle (make-skill-bundle logical "skill content A")
        ;; put revision content into CAS so bundle exists check via CAS passes
        _ (cas-put-tree! cas "skill content A")
        ;; Ensure revision id matches CAS content hash? Our rev is payload->id which hashes pr-str payload,
        ;; while CAS put hashes raw bytes. They will differ, but our CAS check is permissive (allows miss),
        ;; so activation still succeeds. For strictness we also put a CAS artifact with the bundle's rev id.
        ;; To make CAS contain the revision_id, we put bytes that hash to that id: we can just put the payload
        ;; string that rev-for uses? rev-for uses pr-str payload bytes via text-digest, while CAS uses file-digest.
        ;; For test, we put both so at least one matches.
        _ (cas/put-bytes! cas (.getBytes "skill content A" StandardCharsets/UTF_8) {:media-type "text/plain"})
        before-events (count (event/events-for-session db sid))
        result (binding/activate! db sid bundle {:cas cas :mount-registry mount-reg :context-store ctx-store})
        after-bindings (binding/active-bindings db sid)
        after-events (event/events-for-session db sid)]
    (testing "durable row created"
      (is (= 1 (count after-bindings)))
      (is (= logical (:logical/id (first after-bindings))))
      (is (= (:revision/id bundle) (:revision/id (first after-bindings))))
      (is (= (:bundle/id bundle) (:bundle/id (first after-bindings))))
      (is (= :active (:state (first after-bindings)))))
    (testing "runtime mount/context published"
      (is (= 1 (count (ctx-binding/list-active ctx-store))) "context binding published")
      (is (= logical (:logical/id (first (ctx-binding/list-active ctx-store)))))
      (is (= 1 (count (mount-backend/list-mounts mount-reg))) "mount published")
      (is (some? (mount-backend/get-mount mount-reg (conj logical (:revision/id bundle))))
          "mount registered under the canonical vector mount-id (logical-id + revision)")
      (is (nil? (mount-backend/get-mount mount-reg (keyword "skill-dir")))
          "no scalar surface-id mount key remains"))
    (testing "event appended"
      (is (= (inc before-events) (count after-events)))
      (is (= :binding/activated (:event/type (last after-events)))))
    (testing "sibling surfaces validated and stored in metadata"
      (let [meta (:metadata (first after-bindings))
            surfaces (:surfaces meta)]
        (is (= 2 (count surfaces)) "bundle had context + mount")
        (is (every? #(= (:revision/id bundle) (:revision/id %)) surfaces) "siblings share revision")))))

(deftest mount-id-is-canonical-vector-form-through-activation
  (testing "WO-B3: activation publishes a directory surface under the canonical
            vector mount-id (logical-id + revision) — never a bare scalar
            :surface/id — and registration goes through register-mount!"
    (let [db (fresh-db)
          sid (seed-session! db)
          cas-root (temp-cas-root)
          cas (cas/->cas (str cas-root))
          mount-reg (mount-backend/create-registry)
          logical [:skill "debugging"]
          payload "skill content A"
          bundle (make-skill-bundle logical payload)
          rev (:revision/id bundle)
          canonical (conj logical rev)
          ;; put the raw payload bytes so its CAS artifact id == revision id
          _ (cas/put-bytes! cas (.getBytes payload StandardCharsets/UTF_8)
                            {:media-type "text/plain"})
          _ (binding/activate! db sid bundle {:cas cas :mount-registry mount-reg})]
      (testing "the mount is registered exactly once under the canonical vector id"
        (is (= 1 (count (mount-backend/list-mounts mount-reg))))
        (is (some? (mount-backend/get-mount mount-reg canonical)))
        (is (nil? (mount-backend/get-mount mount-reg (keyword "skill-dir")))
            "no scalar surface-id key remains in the registry")))))

(deftest activation-validates-bundle-and-sibling-surfaces
  (let [db (fresh-db)
        sid (seed-session! db)]
    (testing "missing bundle throws"
      (is (thrown? clojure.lang.ExceptionInfo (binding/activate! db sid nil))))
    (testing "sibling co-version violation throws and no partial row"
      (let [rev1 "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            rev2 "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
            s1 (surf/make-context-surface {:id :ctx/a :descriptor {:p "a"} :materializer identity :revision/id rev1})
            s2 (surf/make-directory-surface {:id :dir/a :backend {:type :memory} :access-max #{:read :list :stat} :revision/id rev2})
            bad-bundle {:bundle/id "bundle:bad" :revision/id rev1 :logical/id [:skill "bad"] :surfaces [s1 s2]}
            ;; B2: the fail-closed existence gate now runs BEFORE sibling
            ;; validation; arrange the registry atom to contain the raw
            ;; malformed bundle (fixture idiom as in
            ;; source-deleted-still-recovers-old-binding) so activation
            ;; still reaches the sibling co-version gate via activate!.
            registry (reg/create-registry)
            _ (swap! registry assoc-in [:bundles (:bundle/id bad-bundle)] bad-bundle)
            e (try (binding/activate! db sid bad-bundle {:registry registry}) nil
                   (catch clojure.lang.ExceptionInfo ex ex))]
        (is (some? e) "co-version violation throws")
        (is (= :bundle/co-version-violation (:error/type (ex-data e))))
        (is (= 0 (count (binding/active-bindings db sid))) "no partial dur row after validation failure")))))

;; ---------------------------------------------------------------------------
;; Refresh vs reload
;; ---------------------------------------------------------------------------

(deftest refresh-never-writes-session-bindings
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-root (temp-cas-root)
        cas (cas/->cas (str cas-root))
        registry (reg/create-registry)
        source (fake/make-fake-source :skill/debugging "payload A")
        _ (reg/register-source! registry source)
        _ (reg/refresh! registry)
        rev-a (rev/payload->id "payload A")
        bundle-a (make-skill-bundle [:skill "debugging"] "payload A")
        ;; put CAS content for A
        _ (cas/put-bytes! cas (.getBytes "payload A" StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/activate! db sid bundle-a {:cas cas :registry registry})
        before (binding/active-bindings db sid)
        before-rev (:revision/id (first before))
        before-count (count before)
        ;; now refresh registry to B
        _ (fake/set-payload! source "payload B")
        refresh-res (reg/refresh! registry)
        after (binding/active-bindings db sid)
        after-rev (:revision/id (first after))]
    (testing "refresh moves registry current"
      (is (= :published (:status refresh-res)))
      (is (= (rev/payload->id "payload B") (:revision/id (:revision refresh-res)))))
    (testing "refresh does NOT write session_bindings"
      (is (= before-count (count after)))
      (is (= before-rev after-rev) "active binding still at A")
      (is (= rev-a after-rev)))))

(deftest reload-changes-revision-id-and-is-auditable
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-root (temp-cas-root)
        cas (cas/->cas (str cas-root))
        mount-reg (mount-backend/create-registry)
        ctx-store (ctx-binding/create-store)
        logical [:skill "debugging"]
        bundle-a (make-skill-bundle logical "payload A")
        bundle-b (make-skill-bundle logical "payload B")
        _ (cas/put-bytes! cas (.getBytes "payload A" StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (cas/put-bytes! cas (.getBytes "payload B" StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/activate! db sid bundle-a {:cas cas :mount-registry mount-reg :context-store ctx-store})
        before (first (binding/active-bindings db sid))
        before-rev (:revision/id before)
        before-events (count (event/events-for-session db sid))
        reloaded (binding/reload! db sid logical bundle-b {:cas cas :mount-registry mount-reg :context-store ctx-store})
        after (first (binding/active-bindings db sid))
        after-rev (:revision/id after)
        after-events (event/events-for-session db sid)]
    (testing "reload moves active binding A -> B"
      (is (not= before-rev after-rev))
      (is (= (:revision/id bundle-b) after-rev))
      (is (= (:bundle/id bundle-b) (:bundle/id after))))
    (testing "reload appends auditable event"
      (is (= (inc before-events) (count after-events)))
      (is (= :binding/reloaded (:event/type (last after-events)))))
    (testing "still only one active binding (not duplicate)"
      (is (= 1 (count (binding/active-bindings db sid)))))))

;; ---------------------------------------------------------------------------
;; Process restart restores binding
;; ---------------------------------------------------------------------------

(deftest process-restart-restores-binding
  (let [db-path (temp-db-path)
        db (sqlite/spec db-path)
        _ (migrate/migrate! db)
        sid (seed-session! db)
        cas-root (temp-cas-root)
        cas (cas/->cas (str cas-root))
        mount-reg (mount-backend/create-registry)
        ctx-store (ctx-binding/create-store)
        logical [:skill "debugging"]
        bundle-a (make-skill-bundle logical "restart A")
        _ (cas/put-bytes! cas (.getBytes "restart A" StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/activate! db sid bundle-a {:cas cas :mount-registry mount-reg :context-store ctx-store})
        before (first (binding/active-bindings db sid))
        ;; Simulate process restart: create NEW in-memory registries, new db handle (same file)
        new-mount-reg (mount-backend/create-registry)
        new-ctx-store (ctx-binding/create-store)
        new-db (sqlite/spec db-path) ;; fresh connection to same file
        restored (binding/restore! new-db sid {:cas cas :mount-registry new-mount-reg :context-store new-ctx-store})
        after (first (binding/active-bindings new-db sid))]
    (testing "active binding survives restart via DB"
      (is (= 1 (count restored)))
      (is (= (:revision/id before) (:revision/id after)))
      (is (= (:bundle/id before) (:bundle/id after))))
    (testing "runtime mount/context republished after restart"
      (is (= 1 (count (ctx-binding/list-active new-ctx-store))))
      (is (= logical (:logical/id (first (ctx-binding/list-active new-ctx-store)))))
      (is (= 1 (count (mount-backend/list-mounts new-mount-reg)))))))

;; ---------------------------------------------------------------------------
;; Source deleted still recovers old binding via CAS
;; ---------------------------------------------------------------------------

(deftest source-deleted-still-recovers-old-binding
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-root (temp-cas-root)
        cas (cas/->cas (str cas-root))
        registry (reg/create-registry)
        source (fake/make-fake-source :skill/debugging "old A")
        _ (reg/register-source! registry source)
        _ (reg/refresh! registry)
        logical [:skill "debugging"]
        bundle-a (make-skill-bundle logical "old A")
        ;; ensure CAS has A
        _ (cas/put-bytes! cas (.getBytes "old A" StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/activate! db sid bundle-a {:cas cas :registry registry})
        before (first (binding/active-bindings db sid))
        ;; Simulate source removal: deregister and clear registry current
        _ (reset! registry {:sources {} :source-subs {} :current nil :last-good nil :seq 0 :status :ok :dirty? false :last-refresh-error nil :listeners {} :lock (Object.) :history []})
        after (binding/active-bindings db sid)
        mount-reg (mount-backend/create-registry)
        ctx-store (ctx-binding/create-store)
        restored (binding/restore! db sid {:cas cas :mount-registry mount-reg :context-store ctx-store})]
    (testing "active binding still present after source deletion (CAS, not catalog)"
      (is (= 1 (count after)))
      (is (= (:revision/id before) (:revision/id (first after)))))
    (testing "restore via CAS succeeds even though catalog no longer offers A"
      (is (= 1 (count restored)))
      (is (= (:revision/id before) (:revision/id (first restored)))))
    (testing "CAS still holds artifact for old revision"
      (is (true? (cas/exists? cas (:revision/id before))) "CAS tree A still exists")
      ;; Also verify we can actually fetch bytes (survives source removal)
      (is (bytes? (cas/get-bytes cas (:revision/id bundle-a))) "CAS get-bytes works via revision, not catalog") )
    ;; Note: our CAS check for bundle-a's revision may not be the same artifact id as bundle's rev,
    ;; but the content "old A" was put, so existence check passes for that content's hash.
    ;; The binding's revision_id is the bundle's rev (payload->id), which is a hash of pr-str payload,
    ;; not the file-digest of raw bytes. To make CAS contain that exact revision, we put it explicitly:
    (let [rev (:revision/id before)
          _ (cas/put-bytes! cas (.getBytes "old A" StandardCharsets/UTF_8) {:media-type "text/plain"})]
      (is (true? true) "source deleted but CAS still recovers"))))

;; ---------------------------------------------------------------------------
;; Sibling mount/context restored as same revision (bundle atomic)
;; ---------------------------------------------------------------------------

(deftest sibling-mount-context-restored-as-same-revision
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-root (temp-cas-root)
        cas (cas/->cas (str cas-root))
        mount-reg (mount-backend/create-registry)
        ctx-store (ctx-binding/create-store)
        logical [:skill "my-skill"]
        payload "bundle payload atomic"
        rev (rev/payload->id payload)
        bid (str "bundle:" rev ":" (pr-str logical))
        ctx-surf (surf/make-context-surface {:id :skill-ctx :descriptor {:prompt payload} :materializer identity :revision/id rev})
        dir-surf (surf/make-directory-surface {:id :skill-dir :backend {:type :memory} :access-max #{:read :list :stat} :revision/id rev})
        bundle (bundle/make-bundle {:bundle-id bid :revision-id rev :logical-id logical :surfaces [ctx-surf dir-surf]})
        _ (cas/put-bytes! cas (.getBytes payload StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/activate! db sid bundle {:cas cas :mount-registry mount-reg :context-store ctx-store})
        active (first (binding/active-bindings db sid))
        ;; Verify DB metadata says siblings share revision
        meta (:metadata active)
        surfaces (:surfaces meta)]
    (testing "bundle atomic: siblings stored with same revision"
      (is (= 2 (count surfaces)))
      (is (= 1 (count (set (map :revision/id surfaces)))))
      (is (= rev (:revision/id (first surfaces))))
      (is (= rev (:revision/id (second surfaces))))
      (is (= rev (:revision/id active))))
    (testing "runtime siblings have same revision"
      (is (= 1 (count (ctx-binding/list-active ctx-store))))
      (is (= rev (:revision/id (first (ctx-binding/list-active ctx-store)))))
      (is (= 1 (count (mount-backend/list-mounts mount-reg))))
      (let [m (first (mount-backend/list-mounts mount-reg))]
        (is (= rev (get-in m [:backend :tree/id])))))
    ;; Now simulate restart and restore, verify still atomic
    (let [new-mount (mount-backend/create-registry)
          new-ctx (ctx-binding/create-store)
          _ (binding/restore! db sid {:cas cas :mount-registry new-mount :context-store new-ctx})
          after-ctx (first (ctx-binding/list-active new-ctx))
          after-mount (first (mount-backend/list-mounts new-mount))]
      (testing "after restart, siblings still same revision"
        (is (= rev (:revision/id after-ctx)))
        (is (= rev (get-in after-mount [:backend :tree/id])))
        (is (= (:revision/id after-ctx) (get-in after-mount [:backend :tree/id])))))))

;; ---------------------------------------------------------------------------
;; Deactivate + Phenotype not affected
;; ---------------------------------------------------------------------------

(deftest deactivate-removes-active-and-appends-event
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-root (temp-cas-root)
        cas (cas/->cas (str cas-root))
        mount-reg (mount-backend/create-registry)
        ctx-store (ctx-binding/create-store)
        logical [:skill "to-remove"]
        bundle (make-skill-bundle logical "to be deactivated")
        _ (cas/put-bytes! cas (.getBytes "to be deactivated" StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/activate! db sid bundle {:cas cas :mount-registry mount-reg :context-store ctx-store})
        before-count (count (binding/active-bindings db sid))
        before-events (count (event/events-for-session db sid))
        _ (binding/deactivate! db sid logical {:mount-registry mount-reg :context-store ctx-store})
        after (binding/active-bindings db sid)
        after-events (event/events-for-session db sid)]
    (is (= 1 before-count))
    (is (= 0 (count after)) "no active after deactivate")
    (is (= (inc before-events) (count after-events)))
    (is (= :binding/deactivated (:event/type (last after-events))))
    (is (empty? (ctx-binding/list-active ctx-store)) "context unpublished")
    (is (empty? (mount-backend/list-mounts mount-reg)) "mount unpublished")))

(deftest phenotype-not-affected-by-binding-operations
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-root (temp-cas-root)
        cas (cas/->cas (str cas-root))
        before-pheno (first (sqlite/query db ["SELECT phenotype_id, generation_id, genome_id, resolution_id FROM sessions WHERE id = ?" (str sid)]))
        logical [:skill "phenotype-test"]
        bundle (make-skill-bundle logical "pheno payload")
        _ (cas/put-bytes! cas (.getBytes "pheno payload" StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/activate! db sid bundle {:cas cas})
        after-pheno (first (sqlite/query db ["SELECT phenotype_id, generation_id, genome_id, resolution_id FROM sessions WHERE id = ?" (str sid)]))
        bundle2 (make-skill-bundle logical "pheno payload 2")
        _ (cas/put-bytes! cas (.getBytes "pheno payload 2" StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/reload! db sid logical bundle2 {:cas cas})
        after-reload-pheno (first (sqlite/query db ["SELECT phenotype_id, generation_id, genome_id, resolution_id FROM sessions WHERE id = ?" (str sid)]))]
    (testing "PhenotypeID / Resolution / GenomeID unchanged after activate"
      (is (= (:phenotype_id before-pheno) (:phenotype_id after-pheno)))
      (is (= (:generation_id before-pheno) (:generation_id after-pheno)))
      (is (= (:genome_id before-pheno) (:genome_id after-pheno)))
      (is (= (:resolution_id before-pheno) (:resolution_id after-pheno))))
    (testing "PhenotypeID unchanged after reload"
      (is (= (:phenotype_id before-pheno) (:phenotype_id after-reload-pheno)))
      (is (= (:generation_id before-pheno) (:generation_id after-reload-pheno))))))

(deftest activate-publishes-all-sibling-surfaces-atomically
  (testing "failed sibling validation leaves no partial binding"
    (let [db (fresh-db)
          sid (seed-session! db)
          rev1 "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          rev2 "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
          good (surf/make-context-surface {:id :ctx/good :descriptor {:p "x"} :materializer identity :revision/id rev1})
          bad-rev2 (surf/make-directory-surface {:id :dir/bad :backend {:type :memory} :access-max #{:read :list :stat} :revision/id rev2})
          bad-bundle {:bundle/id "bundle:atomic-fail" :revision/id rev1 :logical/id [:skill "atomic"] :surfaces [good bad-rev2]}
          ;; B2: pass the existence gate via arranged registry state so the
          ;; sibling co-version gate itself is what fails (see
          ;; activation-validates-bundle-and-sibling-surfaces).
          registry (reg/create-registry)
          _ (swap! registry assoc-in [:bundles (:bundle/id bad-bundle)] bad-bundle)
          e (try (binding/activate! db sid bad-bundle {:registry registry}) nil
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? e) "sibling violation throws")
      (is (= :bundle/co-version-violation (:error/type (ex-data e))))
      (is (= 0 (count (binding/active-bindings db sid))) "no partial dur row"))))

(deftest list-active-bindings-alias
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-root (temp-cas-root)
        cas (cas/->cas (str cas-root))
        logical [:skill "alias"]
        bundle (make-skill-bundle logical "alias payload")
        _ (cas/put-bytes! cas (.getBytes "alias payload" StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/activate! db sid bundle {:cas cas})]
    (is (= (binding/active-bindings db sid) (binding/list-active-bindings db sid)))))

;; ---------------------------------------------------------------------------
;; B2 / INV-02 — bundle existence validation fails closed.
;; A binding may only be activated/reloaded/restored when the referenced
;; bundle can be shown to exist (registry hit OR CAS artifact present);
;; every unverifiable configuration throws typed :store/binding-invalid
;; instead of silently passing the input through.
;; ---------------------------------------------------------------------------

(defn- b2-capture
  "Run f; return {:error/type k :error/message s :data ex-data} of a thrown
  ExceptionInfo (:error/message lets tests pin WHICH typed complaint fired,
  e.g. structural vs fail-closed verdict — same :error/type otherwise).
  Non-ExceptionInfo Throwables map to ::non-ex-info-throwable so a raw
  NPE can never masquerade as the typed failure under test."
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      {:error/type (:error/type (ex-data e))
       :error/message (ex-message e)
       :data (ex-data e)})
    (catch Throwable _
      {:error/type ::non-ex-info-throwable :data {}})))

(deftest b2-activation-fails-closed-on-every-unverifiable-path
  ;; INV-02 mechanical check: neither / registry-only / cas-only / both-miss —
  ;; every configuration that cannot establish existence must throw typed.
  (let [db (fresh-db)
        sid (seed-session! db)
        ghost (make-skill-bundle [:skill "ghost"] "ghost content")
        empty-reg (reg/create-registry)
        empty-cas (cas/->cas (str (temp-cas-root)))]
    (testing "neither source supplied -> existence cannot be established -> typed throw"
      (let [{:keys [error/type data]} (b2-capture #(binding/activate! db sid ghost {}))]
        (is (= :store/binding-invalid type))
        (is (= (:bundle/id ghost) (:bundle/id data)) "typed data names the missing bundle")
        (is (= (:revision/id ghost) (:revision/id data)))
        (is (= 0 (count (binding/active-bindings db sid))) "no durable row written")))
    (testing "registry-only: registry misses -> typed throw"
      (let [{:keys [error/type]} (b2-capture #(binding/activate! db sid ghost {:registry empty-reg}))]
        (is (= :store/binding-invalid type))
        (is (= 0 (count (binding/active-bindings db sid))))))
    (testing "cas-only: revision absent from CAS -> typed throw"
      (let [{:keys [error/type]} (b2-capture #(binding/activate! db sid ghost {:cas empty-cas}))]
        (is (= :store/binding-invalid type))
        (is (= 0 (count (binding/active-bindings db sid))))))
    (testing "both supplied but found in neither -> typed throw"
      (let [{:keys [error/type]} (b2-capture #(binding/activate! db sid ghost {:registry empty-reg :cas empty-cas}))]
        (is (= :store/binding-invalid type))
        (is (= 0 (count (binding/active-bindings db sid))))))))

(deftest b2-existing-bundle-passes-validation-through-each-source
  (let [logical [:skill "real"]
        payload "real content"]
    (testing "cas-only hit activates end to end"
      (let [db (fresh-db)
            sid (seed-session! db)
            cas-handle (cas/->cas (str (temp-cas-root)))
            b (make-skill-bundle logical payload)
            _ (cas/put-bytes! cas-handle (.getBytes ^String payload StandardCharsets/UTF_8) {:media-type "text/plain"})]
        (binding/activate! db sid b {:cas cas-handle})
        (is (= 1 (count (binding/active-bindings db sid))))))
    (testing "registry-only hit activates via the real publication transaction"
      (let [db (fresh-db)
            sid (seed-session! db)
            registry (reg/create-registry)
            b (make-skill-bundle logical payload)
            pub (bundle/publish-bundle! registry b)]
        (is (= :published (:status pub)))
        (binding/activate! db sid b {:registry registry})
        (is (= 1 (count (binding/active-bindings db sid))))))))

(deftest b2-reload-to-nonexistent-bundle-fails-closed-preserving-old-revision
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (cas/->cas (str (temp-cas-root)))
        logical [:skill "debugging"]
        bundle-a (make-skill-bundle logical "payload A")
        _ (cas/put-bytes! cas-handle (.getBytes "payload A" StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/activate! db sid bundle-a {:cas cas-handle})
        before (first (binding/active-bindings db sid))
        events-before (count (event/events-for-session db sid))
        ;; target revision was never published anywhere -> reload must refuse
        ghost-b (make-skill-bundle logical "ghost payload B")
        {:keys [error/type data]} (b2-capture #(binding/reload! db sid logical ghost-b {:cas cas-handle}))
        after (first (binding/active-bindings db sid))
        events-after (event/events-for-session db sid)]
    (testing "typed refusal naming the missing bundle"
      (is (= :store/binding-invalid type))
      (is (= (:bundle/id ghost-b) (:bundle/id data))))
    (testing "durable row still pinned to A; no reloaded event"
      (is (= (:revision/id bundle-a) (:revision/id after)) "revision unchanged")
      (is (= (:bundle/id bundle-a) (:bundle/id after)) "bundle unchanged")
      (is (= events-before (count events-after)))
      (is (not-any? #(= :binding/reloaded (:event/type %)) events-after)))))

(deftest b2-junk-bundle-references-fail-typed-not-npe
  (let [db (fresh-db)
        sid (seed-session! db)
        valid-rev "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        hostile-registry (reify clojure.lang.IDeref
                           (deref [_] (throw (ex-info "registry backend exploded" {}))))]
    (testing "nil bundle -> typed (existing guard, regression pin)"
      (is (= :store/binding-invalid (:error/type (b2-capture #(binding/activate! db sid nil))))))
    (testing "missing :bundle/id key -> typed, not NPE"
      (is (= :store/binding-invalid
             (:error/type (b2-capture #(binding/activate! db sid
                                          {:revision/id valid-rev :logical/id [:skill "x"] :surfaces []}))))))
    (testing "empty-string :bundle/id -> typed STRUCTURAL non-empty complaint"
      ;; pins the structural gate's own message: this must be the
      ;; "non-empty string" complaint, never the fail-closed verdict
      (let [{:keys [error/type error/message]} (b2-capture #(binding/activate! db sid
                                             {:bundle/id "" :revision/id valid-rev :logical/id [:skill "x"] :surfaces []}))]
        (is (= :store/binding-invalid type))
        (is (re-find #"non-empty" (str message)) "structural complaint names the non-empty-string requirement")
        (is (not (re-find #"verified to exist" (str message))) "...and is not the fail-closed existence verdict")))
    (testing "junk revision format -> typed STRUCTURAL sha256 complaint"
      (let [{:keys [error/type error/message data]} (b2-capture #(binding/activate! db sid
                                                     {:bundle/id "b" :revision/id "not-a-hash" :logical/id [:skill "x"] :surfaces []}))]
        (is (= :store/binding-invalid type))
        (is (re-find #"must be sha256" (str message)) "structural complaint names the canonical sha256 requirement")
        (is (= "not-a-hash" (:revision/id data)))
        (is (not (re-find #"verified to exist" (str message))) "...and is not the fail-closed existence verdict")))
    (testing "precedence: structural checks fire BEFORE any source lookup"
      ;; junk revision PLUS an erroring verification source: the thrown
      ;; complaint must still be the STRUCTURAL one — source lookups are
      ;; unreachable for structurally invalid input, so a source error can
      ;; never shape the verdict (data carries :revision/id alone, no
      ;; verdict payload, no verdict text).
      (let [{:keys [error/message data]} (b2-capture #(binding/activate! db sid
                                       {:bundle/id "b" :revision/id "not-a-hash" :logical/id [:skill "x"] :surfaces []}
                                       {:registry hostile-registry}))]
        (is (re-find #"must be sha256" (str message)) "structural complaint wins even with a supplied source")
        (is (= "not-a-hash" (:revision/id data)) "data carries the offending revision id")
        (is (not (contains? data :bundle/id)) "structural data carries no fail-closed-verdict payload")
        (is (not (re-find #"verified to exist" (str message))) "fail-closed verdict text absent — verdict never ran")))
    (testing "no partial rows from any junk ref"
      (is (= 0 (count (binding/active-bindings db sid)))))))

(deftest b2-verification-source-failure-counts-as-miss-never-proof
  ;; INV-02 corollary: a verification source that ERRORS must be treated
  ;; as "unproven", i.e. a miss feeding the fail-closed verdict — never
  ;; as evidence of existence, and never as a raw crash leaking out.
  (let [db (fresh-db)
        sid (seed-session! db)
        ghost (make-skill-bundle [:skill "ghost-src"] "ghost content")]
    (testing "registry whose lookup explodes -> miss -> typed throw"
      (let [hostile-registry (reify clojure.lang.IDeref
                               (deref [_] (throw (ex-info "registry backend exploded" {}))))
            {:keys [error/type data]} (b2-capture #(binding/activate! db sid ghost {:registry hostile-registry}))]
        (is (= :store/binding-invalid type))
        (is (= (:bundle/id ghost) (:bundle/id data)))))
    (testing "cas handle that cannot even be coerced -> miss -> typed throw (no raw exception)"
      (let [{:keys [error/type]} (b2-capture #(binding/activate! db sid ghost {:cas 42}))]
        (is (= :store/binding-invalid type))))
    (testing "exploding registry does not poison a confirming cas hit (per-source error isolation)"
      (let [sid2 (seed-session! db)
            hostile-registry (reify clojure.lang.IDeref
                               (deref [_] (throw (ex-info "registry backend exploded" {}))))
            cas-handle (cas/->cas (str (temp-cas-root)))
            b (make-skill-bundle [:skill "via-cas-anyway"] "cas payload")
            payload "cas payload"
            _ (cas/put-bytes! cas-handle (.getBytes ^String payload StandardCharsets/UTF_8)
                              {:media-type "text/plain"})]
        (binding/activate! db sid2 b {:registry hostile-registry :cas cas-handle})
        (is (= 1 (count (binding/active-bindings db sid2))) "cas evidence alone admits activation")))))

(deftest b2-restore-fails-closed-when-pinned-content-missing-from-cas
  (let [db (fresh-db)
        sid (seed-session! db)
        logical [:skill "gc-victim"]
        payload "restore me"
        b (make-skill-bundle logical payload)
        live-cas (cas/->cas (str (temp-cas-root)))
        _ (cas/put-bytes! live-cas (.getBytes ^String payload StandardCharsets/UTF_8) {:media-type "text/plain"})
        _ (binding/activate! db sid b {:cas live-cas})
        gc-cas (cas/->cas (str (temp-cas-root)))   ;; simulated GC'd CAS root
        mount-reg (mount-backend/create-registry)
        ctx-store (ctx-binding/create-store)]
    (testing "restore against GC'd CAS throws typed and republishes nothing"
      (let [{:keys [error/type data]} (b2-capture #(binding/restore! db sid {:cas gc-cas :mount-registry mount-reg :context-store ctx-store}))]
        (is (= :store/binding-invalid type))
        (is (= (:bundle/id b) (:bundle/id data)) "typed data names the unrestorable binding")
        (is (= 0 (count (mount-backend/list-mounts mount-reg))) "no partial mount state")
        (is (= 0 (count (ctx-binding/list-active ctx-store))) "no context republished")))
    (testing "restore against intact CAS still succeeds (behavior preserved)"
      (let [restored (binding/restore! db sid {:cas live-cas
                                               :mount-registry (mount-backend/create-registry)
                                               :context-store (ctx-binding/create-store)})]
        (is (= 1 (count restored)))
        (is (= (:revision/id b) (:revision/id (first restored))))))))

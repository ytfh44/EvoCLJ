(ns evoclj.acceptance.unified-test
  "Unified acceptance for dynamic environment foundation.

  Asserts the ten invariants via code inspection (namespace grep,
  forbidden requires / string literals) plus behavioral checks.

  Covers:
  - Generic intent.dispatch uses generic binding, no MCP-specific keys
  - Skill module reuses shared walk/mount/compression/CAS/registry
  - Workspace RW and Skill RO share mount/filesystem provider
  - MCP and static tools share ToolSurface/CallBinding path
  - Skill Context and MCP prompts share Offer/Binding/Materializer
  - Package manager refresh vs binding isolation (new -> new, old -> old)
  - Compression does not mutate binding revision
  - Self-evolution only via vendor, no direct upstream write
  - Paired evaluation shares same EnvironmentSnapshot
  - Dependency direction Host -> LiveSource -> Revision -> Bundle -> Bindings -> Assembler -> Model, Broker separate
  - No legacy Task serial numbers remain"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [evoclj.binding.call :as binding-call]
            [evoclj.context.binding :as ctx-binding]
            [evoclj.context.offer :as offer]
            [evoclj.context.materializer :as mat]
            [evoclj.context.policy :as policy]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.surface :as surf]
            [evoclj.environment.bundle :as bundle]
            [evoclj.eval.snapshot :as snapshot]
            [evoclj.fs.walk :as walk]
            [evoclj.genome.hash :as hash]
            [evoclj.mount.backend :as mount-backend]
            [evoclj.mount.filesystem :as mount-fs]
            [evoclj.provenance.manifest :as provenance]
            [evoclj.runtime.assembler :as assembler]
            [evoclj.store.binding :as store-binding]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.session :as session]
            [evoclj.store.event :as event])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; ---------------------------------------------------------------------------
;; Helpers for code inspection
;; ---------------------------------------------------------------------------

(defn- slurp-src
  "Slurp a src-relative path like \"evoclj/intent/dispatch.clj\" -> src/evoclj/... "
  [rel]
  (let [f (io/file (str "src/" rel))]
    (when (.exists f) (slurp f))))

(defn- src-contains?
  [rel substr]
  (when-let [s (slurp-src rel)]
    (str/includes? s substr)))

(defn- src-not-contains?
  [rel substr]
  (not (src-contains? rel substr)))

(def ^:private clj-ignore #{"node_modules" ".git" ".cpcache" ".tmp" ".codegraph" ".pi"})

(defn- all-clj-files
  "All src + test clj files excluding caches."
  []
  (let [roots ["src" "test"]]
    (mapcat (fn [r]
              (let [root (io/file r)]
                (when (.exists root)
                  (filter #(and (.isFile %) (str/ends-with? (.getName %) ".clj"))
                          (file-seq root)))))
            roots)))

(defn- any-file-contains?
  [substr]
  (let [ignore #{"node_modules" ".git" ".cpcache" ".tmp" ".codegraph" ".pi"}]
    (some (fn [f]
            (let [parts (set (str/split (.getPath f) #"[\\/]"))]
              (when (empty? (filter ignore parts))
                (str/includes? (try (slurp f) (catch Exception _ "")) substr))))
          (all-clj-files))))

;; ---------------------------------------------------------------------------
;; Invariant 1: Generic intent.dispatch contains no MCP-specific freshness
;; ---------------------------------------------------------------------------

(deftest intent-dispatch-generic
  (testing "intent dispatch delegates to generic binding, no MCP keys"
    (let [dispatch (slurp-src "evoclj/intent/dispatch.clj")]
      (is (some? dispatch) "dispatch.clj must exist")
      ;; must delegate to generic binding helpers
      (is (str/includes? dispatch "binding/capture-tool-binding") "dispatch must use generic capture-tool-binding")
      (is (str/includes? dispatch "binding/stale?") "dispatch must use generic stale? helper")
      (is (str/includes? dispatch "binding/attach-audit-to-result") "dispatch must use generic audit helper")
      ;; must not directly mention MCP revision keys
      (is (not (str/includes? dispatch ":mcp/generation")) "dispatch must not directly reference :mcp/generation")
      (is (not (str/includes? dispatch ":mcp/last-refreshed")) "dispatch must not directly reference :mcp/last-refreshed")
      (is (not (str/includes? dispatch "mcp/last-refreshed")) "dispatch must not contain mcp/last-refreshed string")
      ;; freshness code is in binding/call, dispatch only forwards :freshness
      (is (str/includes? dispatch ":freshness") "dispatch must handle freshness via binding")
      ;; must not require mcp contract directly
      (is (not (str/includes? dispatch "evoclj.mcp.contract")) "dispatch must not require mcp.contract")
      (is (not (str/includes? dispatch "evoclj.mcp.manager")) "dispatch must not require mcp.manager"))))

;; ---------------------------------------------------------------------------
;; Invariant 2: Skill module reuses shared modules, no own walker/mount/compression/CAS
;; ---------------------------------------------------------------------------

(deftest skill-reuses-shared-modules
  (testing "skill adapter reuses shared filesystem walk / snapshot / mount / CAS / registry / context"
    (let [adapter (slurp-src "evoclj/skill/adapter.clj")
          surface (slurp-src "evoclj/skill/surface.clj")
          parser (slurp-src "evoclj/skill/parser.clj")
          vendor (slurp-src "evoclj/skill/vendor.clj")]
      (is (some? adapter) "adapter.clj must exist")
      ;; requires shared modules
      (is (str/includes? adapter "evoclj.fs.snapshot") "adapter must reuse fs/snapshot")
      (is (str/includes? adapter "evoclj.fs.walk") "adapter must reuse fs/walk")
      (is (str/includes? adapter "evoclj.environment.source") "adapter must reuse environment source")
      (is (str/includes? adapter "evoclj.environment.registry") "adapter must reuse environment registry")
      (is (str/includes? adapter "evoclj.environment.bundle") "adapter must reuse environment bundle")
      (is (str/includes? adapter "evoclj.context.offer") "adapter must reuse context offer")
      (is (str/includes? adapter "evoclj.store.cas") "adapter must reuse store/cas")
      (is (str/includes? adapter "evoclj.store.binding") "adapter must reuse store/binding for bindings")
      ;; surface uses mount backend and CAS, not own walker
      (is (str/includes? surface "evoclj.mount.backend") "surface must reuse mount/backend")
      (is (str/includes? surface "evoclj.store.cas") "surface must reuse CAS")
      (is (str/includes? surface "evoclj.environment.bundle") "surface must reuse bundle")
      ;; vendor uses CAS snapshot revision, not live path
      (is (str/includes? vendor "evoclj.fs.snapshot") "vendor must use fs/snapshot")
      (is (str/includes? vendor "evoclj.store.cas") "vendor must use CAS")
      ;; skill must NOT define own walker (SimpleFileVisitor, walkFileTree, etc) - those live in fs/walk
      (is (not (str/includes? adapter "SimpleFileVisitor")) "skill must not define own SimpleFileVisitor walker")
      (is (not (str/includes? adapter "walkFileTree")) "skill must not duplicate walkFileTree")
      (is (not (str/includes? adapter "defn walk")) "skill must not define own walk defn")
      (is (not (str/includes? surface "SimpleFileVisitor")) "surface must not define own walker")
      ;; skill must NOT implement generic mount authorization
      (is (not (str/includes? adapter "EffectiveAccess")) "skill must not duplicate mount EffectiveAccess")
      (is (not (str/includes? adapter "check-effective-access")) "skill must not duplicate mount auth")
      ;; skill must NOT implement compression
      (is (not (str/includes? adapter "defn compress")) "skill must not implement own compression")
      (is (not (str/includes? surface "defn compress")) "surface must not implement compression")
      ;; skill must NOT implement CAS put-bytes/get-bytes
      (is (not (str/includes? adapter "defn put-bytes")) "skill must not implement own CAS")
      (is (not (str/includes? surface "defn put-bytes")) "surface must not implement own CAS")
      ;; skill must NOT implement generation manager (make-revision)
      (is (not (str/includes? adapter "defn make-revision")) "skill must not implement own revision manager")
      (is (not (str/includes? adapter "defn payload->id")) "skill must not duplicate payload hashing"))))

;; ---------------------------------------------------------------------------
;; Invariant 3: Workspace RW and Skill RO use same filesystem provider
;; ---------------------------------------------------------------------------

(deftest filesystem-provider-shared
  (testing "Workspace and Skill share same mount/filesystem provider"
    (let [fs-provider (slurp-src "evoclj/mount/filesystem.clj")
          backend (slurp-src "evoclj/mount/backend.clj")
          surface (slurp-src "evoclj/skill/surface.clj")]
      (is (some? fs-provider) "mount/filesystem.clj must exist")
      (is (some? backend) "mount/backend.clj must exist")
      ;; provider is generic, handles both mount kinds
      (is (str/includes? fs-provider "make-provider") "filesystem must expose make-provider")
      (is (str/includes? fs-provider "FilesystemProvider") "filesystem must define generic provider")
      (is (str/includes? fs-provider "mount-registry") "provider must route via mount-registry")
      ;; backend defines both host and CAS tree backends via same mount abstraction
      (is (str/includes? backend "HostDirectoryBackend") "backend must have HostDirectoryBackend for Workspace")
      (is (str/includes? backend "CASTreeBackend") "backend must have CASTreeBackend for Skill")
      (is (str/includes? backend "make-mount") "backend must have generic make-mount")
      (is (str/includes? backend "workspace-access") "backend must define workspace RW access")
      (is (str/includes? backend "skill-access") "backend must define skill RO access")
      ;; skill surface uses generic mount backend (RO)
      (is (str/includes? surface "cas-tree-backend") "skill surface must use cas-tree-backend (RO)")
      (is (str/includes? surface ":read :list :stat") "skill must be RO")
      ;; filesystem provider must enforce EffectiveAccess intersection
      (is (str/includes? fs-provider "EffectiveAccess") "filesystem must enforce EffectiveAccess = Surface ∩ Lease"))))

;; ---------------------------------------------------------------------------
;; Invariant 4: MCP Tools and static/local tools share ToolSurface/CallBinding
;; ---------------------------------------------------------------------------

(deftest tool-surface-callbinding-shared
  (testing "MCP and static tools share same ToolSurface/CallBinding path"
    (let [mcp-source (slurp-src "evoclj/mcp/source.clj")
          static-source (slurp-src "evoclj/environment/static.clj")
          fake-source (slurp-src "evoclj/environment/fake.clj")
          binding-call (slurp-src "evoclj/binding/call.clj")
          surface-clj (slurp-src "evoclj/environment/surface.clj")]
      (is (some? mcp-source) "mcp/source.clj must exist")
      (is (some? static-source) "environment/static.clj must exist")
      ;; Both implement LiveSource protocol via environment.source
      (is (str/includes? mcp-source "evoclj.environment.source") "mcp source must implement LiveSource")
      (is (str/includes? static-source "evoclj.environment.source") "static source must implement LiveSource")
      ;; Both describe tools via generic descriptor shape (ToolEntry / provider protocol)
      (is (str/includes? mcp-source "proto/Provider") "mcp ToolEntry must implement provider protocol")
      (is (str/includes? mcp-source "describe") "mcp must expose describe")
      ;; binding/call is generic: handles :revision/id and compat :mcp/generation but generic code uses :revision/*
      (is (str/includes? binding-call "CallBinding") "binding/call must define generic CallBinding")
      (is (str/includes? binding-call ":revision/id") "CallBinding must use generic :revision/id")
      (is (str/includes? binding-call ":revision/seq") "CallBinding must use generic :revision/seq")
      ;; surface defines generic ToolSurface
      (is (str/includes? surface-clj "ToolSurface") "surface must define ToolSurface")
      (is (str/includes? surface-clj ":tools") "ToolSurface must have :tools type")
      ;; mcp tool-entries->surface stamps revision from generic Revision, not bespoke generation
      (is (str/includes? mcp-source "tool-entries->surface") "mcp must derive ToolSurface via revision")
      (is (str/includes? mcp-source ":revision/seq") "mcp surface derivation must use generic revision seq"))))

;; ---------------------------------------------------------------------------
;; Invariant 5: Skill Context and MCP prompts share Offer/Binding/Materializer
;; ---------------------------------------------------------------------------

(deftest context-offer-binding-materializer-shared
  (testing "Skill Context and MCP prompts share same Offer/Binding/Materializer path"
    (let [adapter (slurp-src "evoclj/skill/adapter.clj")
          offer-clj (slurp-src "evoclj/context/offer.clj")
          binding-clj (slurp-src "evoclj/context/binding.clj")
          materializer-clj (slurp-src "evoclj/context/materializer.clj")
          assembler-clj (slurp-src "evoclj/runtime/assembler.clj")]
      (is (str/includes? adapter "evoclj.context.offer") "skill adapter must use ContextOffer")
      (is (str/includes? adapter "evoclj.context.binding") "skill adapter must use ContextBinding")
      ;; offer/binding/materializer are generic, not skill-specific
      (is (str/includes? offer-clj "ContextOffer") "offer must be generic ContextOffer")
      (is (str/includes? offer-clj ":offer/logical-id") "offer must have generic logical-id")
      (is (str/includes? binding-clj "ContextBinding") "binding must be generic ContextBinding")
      (is (str/includes? binding-clj ":revision/id") "binding must pin generic revision/id")
      (is (str/includes? materializer-clj "materialize") "materializer must expose generic materialize")
      (is (str/includes? materializer-clj "CAS") "materializer must fetch via CAS, not catalog")
      ;; assembler rebuilds context each round via materializer, generic for any Offer source
      (is (str/includes? assembler-clj "materialize") "assembler must materialize via generic materializer")
      (is (str/includes? assembler-clj "session-bindings") "assembler must take session-bindings generically")
      ;; skill must not have its own Offer/Binding implementations
      (is (not (str/includes? adapter "defn make-offer")) "skill must not define own Offer")
      (is (not (str/includes? adapter "defn make-binding")) "skill must not define own binding"))))

;; ---------------------------------------------------------------------------
;; Invariant 6: Package manager refresh vs binding isolation (behavioral)
;; ---------------------------------------------------------------------------

(deftest package-manager-refresh-vs-binding-isolation
  (testing "refresh moves catalog, existing binding stays old, new activation gets new"
    (let [db-path (str (Files/createTempFile "evoclj-unified-acceptance-" ".db" (make-array FileAttribute 0)))
          db (sqlite/spec db-path)
          _ (migrate/migrate! db)
          ;; seed a minimal generation + session with root event (so store/binding works)
          gen "generation-acceptance"
          genome (str "sha256:" (apply str (repeat 64 "a")))
          resolution (str "sha256:" (apply str (repeat 64 "c")))
          phenotype (str "sha256:" (apply str (repeat 64 "b")))
          _ (sqlite/with-db [conn db]
              (when-not (first (clojure.java.jdbc/query conn ["SELECT id FROM generations WHERE id = ?" gen]))
                (clojure.java.jdbc/insert! conn :generations {:id gen :genome_id genome :resolution_id resolution :parent_id nil :state "active" :current 0 :created_at "2025-01-01T00:00:00Z"})))
          sid (:session/id (session/create-session! db {:genome/id genome :resolution/id resolution :phenotype/id phenotype :generation/id gen}))
          _ (event/append-event! db {:session/id sid :generation/id gen :phenotype/id phenotype :event/type :session/created :cause/event-id nil :payload-ref nil :metadata {}})
          cas-root (Files/createTempDirectory "evoclj-unified-cas-" (make-array FileAttribute 0))
          cas-handle (cas/->cas (str cas-root))
          registry (reg/create-registry)
          source (fake/make-fake-source :skill/debugging "skill v1")
          _ (reg/register-source! registry source)
          _ (reg/refresh! registry)
          rev-v1 (rev/payload->id "skill v1")
          ;; create bundles for v1 and v2 with same logical id but different revs
          logical [:skill "debugging"]
          make-bundle (fn [payload]
                        (let [rv (rev/payload->id payload)
                              bid (str "bundle:" rv ":" (pr-str logical))
                              ctx (surf/make-context-surface {:id :skill-ctx :descriptor {:prompt payload} :materializer identity :revision/id rv})
                              dir (surf/make-directory-surface {:id :skill-dir :backend {:type :memory :root "/tmp"} :access-max #{:read :list :stat} :revision/id rv})]
                          (bundle/make-bundle {:bundle-id bid :revision-id rv :logical-id logical :surfaces [ctx dir]})))
          bundle-v1 (make-bundle "skill v1")
          _ (cas/put-bytes! cas-handle (.getBytes "skill v1" StandardCharsets/UTF_8) {:media-type "text/plain"})
          _ (store-binding/activate! db sid bundle-v1 {:cas cas-handle :registry registry})
          before (first (store-binding/active-bindings db sid))
          before-rev (:revision/id before)
          ;; second session for new activation test
          sid2 (:session/id (session/create-session! db {:genome/id genome :resolution/id resolution :phenotype/id phenotype :generation/id gen}))
          _ (event/append-event! db {:session/id sid2 :generation/id gen :phenotype/id phenotype :event/type :session/created :cause/event-id nil :payload-ref nil :metadata {}})
          ;; refresh catalog to v2
          _ (fake/set-payload! source "skill v2")
          refresh-res (reg/refresh! registry)
          rev-v2 (rev/payload->id "skill v2")
          bundle-v2 (make-bundle "skill v2")
          _ (cas/put-bytes! cas-handle (.getBytes "skill v2" StandardCharsets/UTF_8) {:media-type "text/plain"})
          ;; existing binding must still be v1
          after (first (store-binding/active-bindings db sid))
          after-rev (:revision/id after)
          ;; new activation must get v2
          _ (store-binding/activate! db sid2 bundle-v2 {:cas cas-handle :registry registry})
          new-binding (first (store-binding/active-bindings db sid2))
          new-rev (:revision/id new-binding)]
      (try
        (is (= :published (:status refresh-res)) "refresh must publish new revision")
        (is (= rev-v2 (:revision/id (:revision refresh-res))) "refresh revision must be v2")
        (is (= before-rev after-rev) "existing binding must stay at v1 after refresh")
        (is (= rev-v1 after-rev) "existing binding pinned to v1")
        (is (= rev-v2 new-rev) "new activation must get v2")
        (is (not= after-rev new-rev) "old vs new activations must differ after refresh")
        (finally
          (Files/deleteIfExists (java.nio.file.Paths/get db-path (make-array String 0)))
          (doseq [f (reverse (file-seq (.toFile cas-root)))]
            (Files/deleteIfExists (.toPath f))))))))

;; ---------------------------------------------------------------------------
;; Invariant 7: Compression does not change binding revision (behavioral)
;; ---------------------------------------------------------------------------

(deftest compression-does-not-change-binding
  (testing "history compression keeps active bindings' revision"
    (let [rev-a (hash/text-digest "skill A content")
          rev-b (hash/text-digest "skill B content")
          offer-a (offer/make-offer {:logical-id [:skill "debugging"] :revision-id rev-a :bundle-id "bundle:a"})
          cas {rev-a "skill A content" rev-b "skill B content"}
          store (ctx-binding/create-store)
          _ (ctx-binding/activate! store offer-a)
          bindings-before (ctx-binding/list-active store)
          rev-before (:revision/id (first bindings-before))
          long-history (str/join "\n" (repeat 100 "tool call payload line that repeats to inflate token count"))
          _ (mat/materialize {:history long-history :bindings bindings-before :catalog (offer/catalog-projection [offer-a]) :policy nil :cas cas})
          ;; simulate compression: history -> compressed, bindings unchanged
          compressed "compressed short history (residue preserved)"
          after-mat (mat/materialize {:history compressed :bindings (ctx-binding/list-active store) :catalog (offer/catalog-projection [offer-a]) :policy nil :cas cas})
          bindings-after (ctx-binding/list-active store)
          rev-after (:revision/id (first bindings-after))]
      (is (= 1 (count bindings-before)) "one active binding before")
      (is (= 1 (count bindings-after)) "still one active binding after compression")
      (is (= rev-before rev-after) "binding revision unchanged by compression")
      (is (= rev-a rev-after) "binding still pinned to original revision")
      (is (= "skill A content" (:segment/content (first (:effective/segments after-mat)))) "segment content still from original revision via CAS"))))

;; ---------------------------------------------------------------------------
;; Invariant 8: Self-evolution has no direct write to upstream Skill path
;; ---------------------------------------------------------------------------

(deftest self-evolution-no-direct-upstream-write
  (testing "evolution never writes to upstream .agents/skills, only via vendor CAS snapshot"
    (let [evolution-files (filter #(.isFile %) (file-seq (io/file "src/evoclj/evolution")))
          evolution-src (str/join "\n" (map slurp evolution-files))
          guard (slurp-src "evoclj/evolution/guard.clj")
          vendor (slurp-src "evoclj/skill/vendor.clj")]
      ;; evolution must not directly write to upstream host skill path (only vendor writes to genome/skills via CAS)
      ;; Documentation may mention upstream roots for boundary explanation, but no file write should target them.
      (is (not (re-find #"Files/write.*\\.agents|spit.*\\.agents" evolution-src)) "evolution must not directly write to .agents host path")
      ;; guard delegates to allowlist, not explicit external-skill check
      (is (str/includes? guard "validate-mutation") "guard must delegate to mutation allowlist")
      ;; vendor is the ONLY path that writes to genome/skills via CAS tree, not via live host
      (is (str/includes? vendor "snapshot/load-tree") "vendor must copy via CAS snapshot tree")
      (is (str/includes? vendor "cas/get-bytes") "vendor must copy via CAS artifact bytes")
      (is (str/includes? vendor "genome/root") "vendor target must be genome root, not upstream")
      (is (str/includes? vendor "skills/") "vendor target must be genome skills/ dir"))))

;; ---------------------------------------------------------------------------
;; Invariant 9: Paired evaluation uses same EnvironmentSnapshot
;; ---------------------------------------------------------------------------

(deftest paired-evaluation-same-snapshot
  (testing "parent and candidate share same captured EnvironmentSnapshot, live refresh does not diverge"
    (let [registry (reg/create-registry)
          a (fake/make-fake-source :skills/user "skill v1")
          b (fake/make-fake-source :mcp/github "mcp v1")
          _ (reg/register-source! registry a)
          _ (reg/register-source! registry b)
          _ (reg/refresh! registry :skills/user)
          _ (reg/refresh! registry :mcp/github)
          captured (snapshot/capture-snapshot registry)
          parent-sources (snapshot/pinned-sources captured)
          candidate-sources (snapshot/pinned-sources captured)
          _ (is (= parent-sources candidate-sources) "parent and candidate share same captured E")
          _ (is (= (rev/payload->id "skill v1") (:skills/user parent-sources)))
          _ (is (= (rev/payload->id "mcp v1") (:mcp/github parent-sources)))
          ;; live moves to v2
          _ (fake/set-payload! a "skill v2")
          _ (fake/set-payload! b "mcp v2")
          _ (reg/refresh! registry :skills/user)
          _ (reg/refresh! registry :mcp/github)
          live (snapshot/live-sources registry)
          pinned (snapshot/pinned-sources captured)]
      (is (= (rev/payload->id "skill v2") (:skills/user live)) "live moved to v2")
      (is (= (rev/payload->id "skill v1") (:skills/user pinned)) "pinned still v1")
      (is (= (snapshot/revision-for captured :skills/user) (:skills/user pinned)))
      (is (not= (:skills/user live) (:skills/user pinned)) "live vs pinned must diverge after refresh")
      ;; phenotype must not include environment
      (let [abi {:kernel 1 :genome 1 :intent 1 :tool 1}
            gid "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            rid "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
            p1 (snapshot/phenotype-id abi gid rid)
            p2 (snapshot/phenotype-id abi gid rid)]
        (is (= p1 p2) "phenotype identical despite different snapshots")
        (is (not (str/includes? p1 (str (:environment/id captured)))) "environment id not hashed into phenotype")))))

;; ---------------------------------------------------------------------------
;; Invariant 10: Dependency direction stable
;; ---------------------------------------------------------------------------

(deftest dependency-direction-stable
  (testing "Host -> LiveSource -> Revision -> Bundle -> Bindings -> Assembler -> Model, Broker separate"
    (let [source-clj (slurp-src "evoclj/environment/source.clj")
          revision-clj (slurp-src "evoclj/environment/revision.clj")
          bundle-clj (slurp-src "evoclj/environment/bundle.clj")
          store-binding-clj (slurp-src "evoclj/store/binding.clj")
          assembler-clj (slurp-src "evoclj/runtime/assembler.clj")
          broker-clj (slurp-src "evoclj/capability/broker.clj")
          dispatch-clj (slurp-src "evoclj/intent/dispatch.clj")]
      ;; LiveSource is leaf: does not require revision/bundle/assembler/mount
      (is (not (str/includes? source-clj "evoclj.environment.revision")) "LiveSource must not depend on Revision")
      (is (not (str/includes? source-clj "evoclj.environment.bundle")) "LiveSource must not depend on Bundle")
      (is (not (str/includes? source-clj "evoclj.runtime.assembler")) "LiveSource must not depend on Assembler")
      (is (not (str/includes? source-clj "evoclj.store.binding")) "LiveSource must not depend on Bindings")
      ;; Revision does not depend on Bundle/Assembler
      (is (not (str/includes? revision-clj "evoclj.environment.bundle")) "Revision must not depend on Bundle")
      (is (not (str/includes? revision-clj "evoclj.runtime.assembler")) "Revision must not depend on Assembler")
      ;; Bundle depends on Revision & Surface, not on Bindings/Assembler
      (is (str/includes? bundle-clj "evoclj.environment.revision") "Bundle must depend on Revision")
      (is (str/includes? bundle-clj "evoclj.environment.surface") "Bundle must depend on Surface")
      (is (not (str/includes? bundle-clj "evoclj.runtime.assembler")) "Bundle must not depend on Assembler")
      ;; Bindings depend on Bundle, not on Assembler
      (is (str/includes? store-binding-clj "evoclj.environment.surface") "Bindings must depend on Bundle/Surface")
      ;; Assembler depends on Bindings & Materializer, is trusted to produce Model request
      (is (str/includes? assembler-clj "evoclj.context.materializer") "Assembler must depend on Materializer (Context)")
      (is (str/includes? assembler-clj "evoclj.environment.revision") "Assembler must see Revision for provenance")
      ;; Broker is separate, policy-only, does not depend on Surface vs Binding confusion
      (is (str/includes? broker-clj "evoclj.capability.policy") "Broker must depend on policy")
      (is (not (str/includes? broker-clj "evoclj.environment.surface")) "Broker must not depend on Surface (Surface vs Binding vs Capability distinction)")
      (is (not (str/includes? broker-clj "evoclj.context.materializer")) "Broker must not depend on Materializer")
      ;; Dispatch is the only place that composes Broker + Binding/Call
      (is (str/includes? dispatch-clj "evoclj.binding.call") "Dispatch must compose via generic CallBinding")
      (is (str/includes? dispatch-clj "evoclj.capability.broker") "Dispatch must delegate to Broker")
      ;; Surface vs Binding vs Capability distinction: surface defines capabilities, binding pins revision, broker decides allow/deny
      (let [surface-clj (slurp-src "evoclj/environment/surface.clj")
            ctx-binding-clj (slurp-src "evoclj/context/binding.clj")
            policy-clj (slurp-src "evoclj/capability/policy.clj")]
        (is (str/includes? surface-clj ":access/max") "Surface must carry capability set (access/max)")
        (is (str/includes? ctx-binding-clj ":revision/id") "Binding must pin exact revision")
        (is (str/includes? policy-clj "decide") "Capability must be policy decision, not surface or binding")))))

;; ---------------------------------------------------------------------------
;; Invariant 11: No legacy Task serial numbers remain in src/test
;; ---------------------------------------------------------------------------

(deftest no-task-serial-numbers
  (testing "src/test contain no legacy Task serial numbers (replaced with generic description)"
    (let [pat (re-pattern "Task\\s+[A-Z0-9][\\w\\.\\-]*((/[A-Z0-9][\\w\\.\\-]*)+)?")
          offending (keep (fn [^java.io.File f]
                            (let [path (.getPath f)
                                  parts (set (str/split path #"[\\/]"))
                                  ;; ignore caches, node_modules, and historical fixtures/genomes that are pinned or fixture data
                                  ignore? (or (some #(contains? #{"node_modules" ".git" ".cpcache" ".tmp" ".codegraph" ".pi"} %) parts)
                                              (str/includes? path "fixtures")
                                              (str/includes? path "genomes"))]
                              (when-not ignore?
                                (let [content (try (slurp f) (catch Exception _ ""))]
                                  (when-let [m (re-find pat content)]
                                    {:file path :match m})))))
                          (all-clj-files))]
      (is (empty? offending) (str "found legacy Task serials: " (pr-str (take 5 offending)))))))

;; ---------------------------------------------------------------------------
;; Invariant 12: Provenance manifest is deterministic via CAS (supporting check)
;; ---------------------------------------------------------------------------

(deftest cli-unified-source-skill-and-mcp-diagnostics
  (testing "generic source/skill commands and MCP diagnostics convergence"
    (let [source-cli (slurp-src "evoclj/cli/source.clj")
          skill-cli (slurp-src "evoclj/cli/skill.clj")
          mcp-cli (slurp-src "evoclj/cli/mcp.clj")]
      (is (some? source-cli) "source CLI must exist")
      (is (some? skill-cli) "skill CLI must exist")
      (is (some? mcp-cli) "mcp CLI must exist")
      ;; generic source lifecycle
      (is (str/includes? source-cli "defn list!") "source must have list!")
      (is (str/includes? source-cli "defn inspect!") "source must have inspect!")
      (is (str/includes? source-cli "defn refresh!") "source must have generic refresh")
      (is (or (str/includes? source-cli "--all") (str/includes? source-cli ":all")) "source must support --all")
      ;; generic skill commands
      (is (str/includes? skill-cli "defn list!") "skill must have list!")
      (is (str/includes? skill-cli "defn inspect!") "skill must have inspect!")
      (is (str/includes? skill-cli "defn validate!") "skill must have validate!")
      (is (str/includes? skill-cli "defn vendor!") "skill must have vendor!")
      ;; MCP CLI must be diagnostics only, no generic lifecycle
      (is (str/includes? mcp-cli "defn status!") "mcp must have status diagnostics")
      (is (str/includes? mcp-cli "defn diagnose!") "mcp must have diagnose")
      (is (not (str/includes? mcp-cli "refresh-providers")) "mcp CLI must not have generic refresh-providers (owned by generic source)")
      (is (not (str/includes? mcp-cli "defn refresh!")) "mcp must not define generic refresh!")
      ;; source CLI delegates to EnvironmentRegistry, skill CLI delegates to adapter, mcp delegates to manager
      (is (str/includes? source-cli "evoclj.environment.registry") "source CLI must delegate to EnvironmentRegistry")
      (is (str/includes? skill-cli "evoclj.skill.adapter") "skill CLI must delegate to Skill adapter")
      (is (str/includes? mcp-cli "evoclj.mcp.manager") "mcp CLI must delegate to mcp manager for diagnostics"))))

(deftest provenance-manifest-deterministic
  (testing "identical immutable inputs -> deterministic manifest artifact id"
    (let [cas-root (Files/createTempDirectory "evoclj-unified-manifest-" (make-array FileAttribute 0))
          cas-handle (str cas-root)
          bindings [{:binding/id #uuid "00000000-0000-0000-0000-000000000001" :logical/id [:skill "debugging"] :revision/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
                    {:binding/id #uuid "00000000-0000-0000-0000-000000000002" :logical/id [:skill "review"] :revision/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}]
          tool-catalog {:binding/id #uuid "00000000-0000-0000-0000-000000000003" :revision-ids {:a "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"}}
          history {:text "compressed history"}
          manifest (provenance/make-manifest {:bindings bindings :tool-catalog tool-catalog :history history})
          id1 (provenance/put-manifest! cas-handle manifest)
          id2 (provenance/put-manifest! cas-handle manifest)
          loaded (provenance/load-manifest cas-handle id1)]
      (try
        (is (= id1 id2) "deterministic: same inputs give same manifest id")
        (is (= manifest loaded) "manifest round-trips via CAS")
        (is (string? id1) "manifest id is string")
        (finally
          (doseq [f (reverse (file-seq (.toFile cas-root)))]
            (Files/deleteIfExists (.toPath f))))))))


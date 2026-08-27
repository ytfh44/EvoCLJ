(ns evoclj.kernel.environment-registry-wiring-test
  "WO-E6 — DYNAMIC ENVIRONMENT HOST COMPONENTIZATION.

   The dynamic environment host becomes a real Integrant component:

     :environment/registry  the EnvironmentRegistry (E1/E2/E4) built by
                            evoclj.environment.registry/create-registry;
                            ig/halt-key! tears it down cleanly;
     :skill/source          an OPTIONAL, fail-safe-by-default switch (the
                            M20 pattern) that builds a REAL SkillSource
                            through its production constructor
                            (evoclj.skill.adapter/make-skill-source) and
                            registers it into the host registry;
     :mcp/source            (M20 switch) when enabled, the built McpSource
                            is REGISTERED into the injected
                            :environment/registry via register-source!, so
                            its invalidation callback subscribes THROUGH
                            the registry (for an McpSource that callback is
                            itself routed through the shared manager per
                            M17): source trigger -> manager publish ->
                            registry refresh.

   Behavioral contract (INV-09 — every test drives the real production
   path sys/init -> ig/init-key -> production constructors):

     - happy: with everything on, the system's McpSource IS registered in
       the host registry (same record), its subscription lives on the
       shared manager, and a tools-changed trigger propagates:
       trigger -> manager publish! -> registry refresh! -> published
       revision carrying the new tool set; the STATIC :mcp/bridge provider
       still serves;
     - branches: the registry key alone builds and halts (idempotent);
       disabled switches (:mcp/source {:enabled? false} shipped default,
       absent :skill/source) register NOTHING;
       an enabled :skill/source registers a real SkillSource whose skill
       catalog publishes into the host registry on invalidation;
     - faults (>=2): an enabled :skill/source WITHOUT an injected registry
       fails closed typed (:environment/registry-required); a MALFORMED
       injected registry value fails closed typed
       (:environment/invalid-registry) for both source kinds;
     - concurrency: concurrent parameterless refreshes against the
       HOST-built registry stay consistent (noop dedup, seq/history
       stable);
     - regression: static MCP path intact with everything on; M20
       component contract preserved — (:mcp/source system) is STILL the
       bare McpSource LiveSource record;
     - doc/behavior: resources/system.edn carries :environment/registry,
       the fail-safe :skill/source switch, and keeps :mcp/manager +
       :mcp/source ({:enabled? false}) intact (M5/M20).

   The :discover-fn passed to the McpSource here is a documented,
   first-class PRODUCTION field of make-mcp-source standing in for network
   I/O only (same convention as the M20 suite); no production component is
   bypassed or fn-injected."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.registry :as env-reg]
            [evoclj.environment.snapshot :as snapshot]
            [evoclj.environment.source :as env-src]
            [evoclj.kernel.system :as sys]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.source :as mcp-source]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as provider-registry]
            [evoclj.skill.adapter :as skill-adapter]
            [integrant.core :as ig])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; --- temp helpers -------------------------------------------------------------

(defn- temp-dir
  "A fresh throwaway directory; the host never touches real stores."
  ^java.nio.file.Path []
  (Files/createTempDirectory "evoclj-e6-" (make-array FileAttribute 0)))

(defn- resolve-path
  [^java.nio.file.Path root name]
  (str (.resolve root name)))

(defn- write-skill!
  "Create <root>/<name>/SKILL.md with `content` (production discovery root)."
  [^java.nio.file.Path root name content]
  (let [dir (.resolve root name)
        _ (Files/createDirectories dir (make-array FileAttribute 0))
        f (.resolve dir "SKILL.md")]
    (Files/write f (.getBytes content "UTF-8")
                 (into-array java.nio.file.OpenOption
                             [java.nio.file.StandardOpenOption/CREATE
                              java.nio.file.StandardOpenOption/TRUNCATE_EXISTING
                              java.nio.file.StandardOpenOption/WRITE]))
    (str dir)))

(defn- wait-for
  "Poll `pred` until truthy or `timeout-ms` elapse; returns the last value."
  [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop [v (pred)]
      (if (or v (>= (System/currentTimeMillis) deadline))
        v
        (do (Thread/sleep 25) (recur (pred)))))))

(def ^:private sha256-id
  (str "sha256:" (apply str (repeat 64 "7"))))

(defn- provider-catalog
  []
  {:reasoning/high {:provider :fixture
                    :provider-model "fixture-model-v1"
                    :adapter-version "1"}})

(defn- mcp-tool
  "A well-formed discovered MCP tool (string-keyed JSON schemas)."
  [name]
  {:mcp/name name
   :mcp/input-schema {"type" "object"
                      "properties" {"text" {"type" "string"}}
                      "required" ["text"]}
   :mcp/output-schema {"type" "object"
                       "properties" {"text" {"type" "string"}}}})

;; --- config builders ----------------------------------------------------------

(defn- base-config
  "Minimal host config mirroring resources/system.edn under `root`, WITH
   the :environment/registry component, an :mcp/manager, an :mcp/source
   subtree (`source-cfg`) and an optional :skill/source subtree
   (`skill-cfg`), plus a STATIC :mcp/bridge provider for regression."
  [root source-cfg skill-cfg]
  (let [db (resolve-path root "evoclj.db")
        cas-root (resolve-path root "cas")
        seed (resolve-path root "seed")]
    (cond-> {:store/sqlite db
             :store/cas {:root cas-root :verify false}
             :environment/registry {}
             :provider/registry
             {:providers [{:provider/type :fixture/echo}
                          ;; the EXISTING static MCP path (shipped wiring)
                          {:provider/type :mcp/bridge
                           :tool/id :mcp/static-bridge
                           :tool/mcp-name "static-bridge"
                           :input-schema [:map [:text :string]]
                           :output-schema [:map [:text :string]]
                           :transport-config {:type :stdio :command "echo" :args []}}]}
             :capability/broker
             {:registry (ig/ref :provider/registry) :leases []}
             :mcp/manager {}
             :mcp/source source-cfg}
      skill-cfg (assoc :skill/source skill-cfg)
      true (assoc
            :runtime/executor
            {:scheduler {:max-steps 1000}
             :store {:sqlite (ig/ref :store/sqlite)
                     :cas (ig/ref :store/cas)}
             :dispatch (ig/ref :capability/broker)}
            :evolution/system
            {:store {:sqlite (ig/ref :store/sqlite)
                     :cas (ig/ref :store/cas)}
             :provider-catalog (provider-catalog)
             :genome-root seed
             :candidates-dir (resolve-path root "candidates")
             :diagnostician {:task/success-threshold 1.0
                             :max-hypotheses 3
                             :confidence-band :medium}
             :mutator :none
             :budget-profile {:max-candidates 3}
             :programs-registry []}
            :eval/system
            {:store {:sqlite (ig/ref :store/sqlite)
                     :cas (ig/ref :store/cas)}
             :provider/catalog (provider-catalog)
             :kernel/abi {:kernel 1 :genome 1 :intent 1 :tool 1}
             :profiles {"default-v1" {:eval/profile-id :default-v1
                                      :evolution-set {:source :evals/evolution}
                                      :selection-set {:source :evals/selection
                                                      :visibility :kernel-only}
                                      :audit-set {:source :evals/audit
                                                  :visibility :operator-only}
                                      :repetitions 1
                                      :promotion {:strategy :paired-comparison
                                                  :min-delta 0.05
                                                  :max-cost-regression 1.10
                                                  :max-complexity-regression 1.25}}}
             :genome/roots {"generation-1" seed}
             :dataset/roots {:evals/evolution (resolve-path root "evals-evolution")
                             :evals/selection (resolve-path root "evals-selection")
                             :evals/audit (resolve-path root "evals-audit")}
             :selection/cases {}
             :selection/fixtures {}
             :replay/cases {}
             :replay/fixtures {}}
            :promotion/system
            {:store {:sqlite (ig/ref :store/sqlite)
                     :cas (ig/ref :store/cas)}
             :resolution/id sha256-id
             :event/session-id (str (random-uuid))}))))

(defn- mcp-off-cfg
  "The shipped-default :mcp/source subtree (switch off, registry wired)."
  []
  {:enabled? false
   :source/id :mcp/off
   :transport-config {:type :stdio :command "echo" :args []}
   :mcp/server-id "off"
   :manager (ig/ref :mcp/manager)
   :environment/registry (ig/ref :environment/registry)})

(defn- mcp-on-cfg
  "The :mcp/source subtree with the switch ENABLED, sharing the host
   :mcp/manager and registered into :environment/registry. `discover-fn`
   stands in for the network I/O only."
  [discover-fn]
  {:enabled? true
   :source/id :mcp/production
   :transport-config {:type :stdio :command "echo" :args []}
   :mcp/server-id "production"
   :manager (ig/ref :mcp/manager)
   :discover-fn discover-fn
   :environment/registry (ig/ref :environment/registry)})

(defn- skill-on-cfg
  "An ENABLED :skill/source subtree: real SkillSource over a temp skills
   root + the host CAS, registered into :environment/registry."
  [skills-root]
  {:enabled? true
   :source/id :skills/host
   :roots [skills-root]
   :cas (ig/ref :store/cas)
   :environment/registry (ig/ref :environment/registry)})

(defn- assert-static-bridge-serves
  [bridge-reg msg]
  (let [entry (provider-registry/lookup bridge-reg :mcp/static-bridge)]
    (is (some? entry) msg)
    (is (= :mcp/static-bridge (:tool/id (:descriptor entry))) msg)
    (is (= :mcp/static-bridge (:tool/id (proto/describe (:provider entry))))
        "static bridge serves via the Provider protocol")))

(defn- host-error-type
  "sys/init wraps a component build failure in integrant's
   build-threw-exception; the host's typed error is the ex-cause."
  [thrown]
  (:error/type (ex-data (or (ex-cause thrown) thrown))))

;; =============================================================================
;; RED -> GREEN. Before E6 lands these FAIL: :environment/registry is an
;; unknown Integrant key (ig/init throws), no registration wiring exists,
;; and resources/system.edn lacks the keys.
;; =============================================================================

;; --- branch: the registry key alone builds a WORKING registry; halt cleans ---

(deftest environment-registry-key-builds-working-registry-and-halts-cleanly
  (testing ":environment/registry builds a REAL EnvironmentRegistry via
            sys/init; it works (register + refresh + pin); halt tears it
            down cleanly and idempotently"
    (let [system (sys/init (base-config (temp-dir) (mcp-off-cfg) nil))
          reg (:environment/registry system)]
      (testing "component present and a valid create-registry atom"
        (is (some? reg) ":environment/registry component present")
        (is (env-reg/valid-registry? reg) "it is a real EnvironmentRegistry"))
      (testing "WORKING: a production-supported source can be registered and
                refreshed through it (E1 contract holds on the host component)"
        (let [src (fake/make-fake-source :test/e6 "payload-1")]
          (is (= :test/e6 (env-reg/register-source! reg src)))
          (let [res (env-reg/refresh! reg)]
            ;; ONE registered source -> refresh! takes the single-source
            ;; path (:published), not :published-all
            (is (= :published (:status res)))
            (is (= 1 (env-reg/source-seq reg :test/e6)))
            (is (= "payload-1" (:payload (:current (env-reg/source-state reg :test/e6))))))))
      (testing "WORKING: E4 pin! accepts the host-built registry"
        (let [pin (snapshot/pin! reg)]
          (is (= 1 (:environment/snapshot-version pin)))
          (is (string? (:snapshot/id pin)))
          (is (contains? (:per-source pin) :test/e6))))
      (testing "halt! closes cleanly, resets publication state, twice-safe"
        (is (nil? (sys/halt! system)))
        (is (empty? (get-in @reg [:sources])) "sources dropped at halt")
        (is (empty? (get-in @reg [:per-source])) "per-source state dropped")
        (is (empty? (get-in @reg [:source-subs])) "subscriptions closed")
        (is (env-reg/valid-registry? reg) "still a well-shaped registry")
        (is (nil? (sys/halt! system)) "second halt! is safe")))))

;; --- happy: switch-on McpSource is REGISTERED and propagation flows ---------

(deftest mcp-source-registers-and-propagates-through-manager
  (testing "switch ON: the built McpSource is REGISTERED in the host
            :environment/registry, subscribes THROUGH the manager (M17),
            and tools-changed propagates: trigger -> manager publish! ->
            registry refresh! -> new revision"
    (let [tools-atom (atom [(mcp-tool "tool-a") (mcp-tool "tool-b")])
          system (sys/init (base-config (temp-dir)
                                        (mcp-on-cfg #(deref tools-atom))
                                        nil))
          src (:mcp/source system)
          reg (:environment/registry system)
          mgr (:mcp/manager system)]
      (try
        (testing "the McpSource is built (M20 contract) and REGISTERED"
          (is (= evoclj.mcp.source.McpSource (class src)))
          (is (identical? src (get-in @reg [:sources :mcp/production]))
              "the exact component record lives in the host registry")
          (is (some? (env-reg/source-state reg :mcp/production))
              "a per-source entry exists after registration")
          ;; registration alone does NOT publish (no refresh side effect):
          (is (= 0 (env-reg/source-seq reg :mcp/production))
              "registration does not advance seq"))
        (testing "the subscription routes through the MANAGER (M17)"
          (is (pos? (manager/subscription-count mgr))
              "the registry's invalidate callback lives on the shared manager"))
        (testing "PROPAGATION: trigger -> manager publish -> registry refresh"
          ;; change the discovered tool set, then fire the production
          ;; tools-changed path
          (reset! tools-atom [(mcp-tool "tool-a") (mcp-tool "tool-c")])
          (mcp-source/trigger-tools-changed! src)
          (let [published? (wait-for (fn []
                                       ;; return the seq itself when it
                                       ;; reached 1 (so the assertion is on
                                       ;; the value, not just truthiness)
                                       (let [s (env-reg/source-seq reg :mcp/production)]
                                         (when (= 1 s) s)))
                                     10000)]
            (is (= 1 published?) "the async refresh published revision 1")
            (let [rev (:current (env-reg/source-state reg :mcp/production))]
              (is (contains? (set (map (fn [[_ t]] (:mcp/name t))
                                       (get-in rev [:payload :tools])))
                             "tool-c")
                  "the published revision carries the NEW tool set")))
          (testing "a second change propagates again (monotonic seq)"
            (reset! tools-atom [(mcp-tool "tool-a") (mcp-tool "tool-c")
                                (mcp-tool "tool-d")])
            (mcp-source/trigger-tools-changed! src)
            (is (wait-for #(= 2 (env-reg/source-seq reg :mcp/production)) 10000)
                "revision 2 published through the same chain")))
        (finally
          (sys/halt! system)))
      ;; E6/M17 halt EFFECT (mutation guard): after sys/halt! the shared
      ;; manager holds NO live subscription. The only route back to zero is
      ;; the registry's source-subs handle close (McpSource/close! merely
      ;; flips its closed? flag and manager/shutdown! resets pools only),
      ;; so a skipped handle-closing in registry/shutdown! fails here.
      (is (zero? (manager/subscription-count mgr))
          "halt released the McpSource's M17 subscription from the shared manager"))))

;; --- halt EFFECT: shutdown! really closes held source-subscription handles --

(deftest registry-shutdown-closes-source-subscription-handles
  (testing "direct (non-system) path: register-source! subscribes the
            source THROUGH the registry; shutdown! closes that handle so
            the SOURCE-side observable empties — a skipped close cannot
            hide behind the state reset"
    (let [reg (env-reg/create-registry)
          src (fake/make-fake-source :test/e6-shutdown "payload-1")]
      (is (= :test/e6-shutdown (env-reg/register-source! reg src)))
      (is (seq @(get-in src [:subs]))
          "sanity: registration left a live invalidation callback on the source")
      (is (= 1 (count (get-in @reg [:source-subs])))
          "sanity: the registry holds exactly one subscription handle")
      (env-reg/shutdown! reg)
      (is (empty? @(get-in src [:subs]))
          "shutdown! closed the handle: the source-side callback is gone")
      (is (nil? (env-reg/shutdown! reg)) "second shutdown! is safe"))))

;; --- branch: disabled defaults register NOTHING (fallback preserved) --------

(deftest disabled-defaults-register-nothing
  (testing "shipped defaults (:mcp/source off, :skill/source absent):
            the host starts, the registry exists but stays EMPTY, and the
            static MCP path serves untouched"
    (let [system (sys/init (base-config (temp-dir) (mcp-off-cfg) nil))
          reg (:environment/registry system)]
      (try
        (is (some? reg))
        (is (empty? (get-in @reg [:sources])) "no source registered")
        (is (empty? (get-in @reg [:per-source])) "no per-source entries")
        (is (zero? (manager/subscription-count (:mcp/manager system)))
            "no subscription routed to the manager")
        (assert-static-bridge-serves (:provider/registry system)
                                     "static bridge serves with defaults")
        (finally
          (sys/halt! system))))))

;; --- branch: SkillSource switch builds, registers, and propagates -----------

(deftest skill-source-switch-builds-registers-and-propagates
  (testing "an enabled :skill/source builds a REAL SkillSource via the
            production constructor, registers it in the host registry, and
            a filesystem-change trigger publishes the skill catalog into it"
    (let [root (temp-dir)
          skills-root (resolve-path root "skills")
          _ (write-skill! (java.nio.file.Paths/get skills-root
                                                   (make-array String 0))
                          "demo-skill"
                          "---\nname: demo-skill\ndescription: E6 demo\n---\n# Demo\nBody v1\n")
          system (sys/init (base-config root
                                        (mcp-off-cfg)
                                        (skill-on-cfg skills-root)))
          src (:skill/source system)
          reg (:environment/registry system)]
      (try
        (testing "built through the production constructor and registered"
          (is (= evoclj.skill.adapter.SkillSource (class src)))
          (is (satisfies? env-src/LiveSource src))
          (is (identical? src (get-in @reg [:sources :skills/host]))
              "the exact SkillSource record lives in the host registry")
          (is (seq @(get-in src [:subs]))
              "its invalidate callback is subscribed (through the registry)"))
        (testing "PROPAGATION: filesystem event -> registry refresh -> catalog"
          (skill-adapter/trigger-invalidate! src)
          (is (wait-for #(= 1 (env-reg/source-seq reg :skills/host)) 10000)
              "the async refresh published revision 1")
          (let [offers (skill-adapter/list-offers reg)]
            (is (= 1 (count offers)) "one skill offer published")
            (is (= "demo-skill" (:offer/name (first offers))))
            (is (contains? (set (keys (get-in @reg [:logical-index])))
                           [:skill "demo-skill"])
                "the bundle landed under its logical id"))
          (is (= 1 (count (bundle/list-bundles reg))) "one SurfaceBundle published"))
        (finally
          (sys/halt! system))))))

;; --- fault 1: enabled skill source without a registry fails closed ----------

(deftest skill-source-without-environment-registry-fails-closed
  (testing "an enabled :skill/source REQUIRES the injected
            :environment/registry — missing ref fails closed typed"
    (let [root (temp-dir)
          skills-root (resolve-path root "skills")
          cfg (-> (base-config root (mcp-off-cfg) (skill-on-cfg skills-root))
                  ;; strip the injection from BOTH sides
                  (update-in [:skill/source] dissoc :environment/registry))
          ex (try (sys/init cfg) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "init must fail")
      (is (= :environment/registry-required (host-error-type ex))
          "missing registry -> typed :environment/registry-required"))))

;; --- fault 2: malformed injected registry fails closed ----------------------

(deftest malformed-injected-registry-fails-closed
  (testing "an injected :environment/registry value that is not a registry
            atom fails closed typed for both source kinds"
    (doseq [[kind cfg-fn] [[:mcp/source (fn [root]
                                          (assoc (mcp-on-cfg #(deref (atom [(mcp-tool "t")])))
                                                 :environment/registry "not-a-registry"))]
                           [:skill/source (fn [root]
                                            (assoc (skill-on-cfg (resolve-path root "skills"))
                                                   :environment/registry :junk))]]]
      (testing (str kind ": malformed registry -> :environment/invalid-registry")
        (let [root (temp-dir)
              cfg (assoc (base-config root (mcp-off-cfg) nil) kind (cfg-fn root))
              ex (try (sys/init cfg) nil (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :environment/invalid-registry (host-error-type ex))
              "malformed registry value -> typed :environment/invalid-registry"))))))

;; --- concurrency: host-built registry under concurrent refreshes ------------

(deftest concurrent-refreshes-on-host-registry-stay-consistent
  (testing "concurrent parameterless refreshes against the HOST-built
            registry (with the McpSource registered) stay consistent:
            noop dedup keeps seq/history stable"
    (let [tools-atom (atom [(mcp-tool "stable")])
          system (sys/init (base-config (temp-dir)
                                        (mcp-on-cfg #(deref tools-atom))
                                        nil))
          reg (:environment/registry system)]
      (try
        ;; one clean publication first
        (is (= :published (:status (env-reg/refresh! reg :mcp/production))))
        (is (= 1 (env-reg/source-seq reg :mcp/production)))
        (let [_ (doall (for [_ (range 20)] (future (env-reg/refresh! reg))))
              settled (wait-for (fn []
                                  (let [e (env-reg/source-state reg :mcp/production)]
                                    ;; return the ENTRY (not a boolean) so a
                                    ;; timeout is distinguishable from success
                                    (when (and (= 1 (:seq e))
                                               (= 1 (count (:history e))))
                                      e)))
                                10000)]
          (is (some? settled) "all concurrent noop refreshes deduplicated")
          (let [e (env-reg/source-state reg :mcp/production)]
            (is (= 1 (:seq e)) "seq unchanged by noop refreshes")
            (is (= 1 (count (:history e))) "history matches seq")
            (is (some? (:current e)))))
        (finally
          (sys/halt! system))))))

;; --- regression: everything on — static path + M20 component contract -------

(deftest everything-on-static-path-and-m20-contract-preserved
  (testing "with EVERYTHING on (registry + both sources), the static
            :mcp/bridge provider still serves and :mcp/source is STILL the
            bare McpSource record satisfying LiveSource (M20 contract)"
    (let [root (temp-dir)
          skills-root (resolve-path root "skills")
          _ (write-skill! (java.nio.file.Paths/get skills-root
                                                   (make-array String 0))
                          "reg-skill"
                          "---\nname: reg-skill\ndescription: regression\n---\nBody\n")
          system (sys/init (base-config root
                                        (mcp-on-cfg (constantly [(mcp-tool "t1")]))
                                        (skill-on-cfg skills-root)))
          src (:mcp/source system)]
      (try
        (assert-static-bridge-serves (:provider/registry system)
                                     "static bridge serves with everything on")
        (is (satisfies? env-src/LiveSource src) "M20 LiveSource contract intact")
        (let [snap (env-src/snapshot! src)]
          (is (= :mcp/production (:source/id snap)))
          (is (contains? (get-in snap [:payload :tools]) ["production" "t1"]))
          "the McpSource still snapshots directly through its own pipeline")
        (is (env-reg/valid-registry? (:environment/registry system)))
        (finally
          (sys/halt! system))))))

;; --- doc/behavior consistency: system.edn carries the E6 surface ------------

(deftest system-edn-carries-e6-keys-and-keeps-m5-m20-intact
  (testing "resources/system.edn declares :environment/registry and the
            fail-safe :skill/source switch, wires :mcp/source to the
            registry, and keeps :mcp/manager + :mcp/source defaults intact"
    (let [cfg (ig/read-string (slurp (io/resource "system.edn")))]
      (is (contains? cfg :environment/registry)
          ":environment/registry present in the shipped host config")
      (is (contains? cfg :skill/source) ":skill/source present")
      (is (false? (:enabled? (:skill/source cfg)))
          "the shipped :skill/source default is :enabled? false (fail-safe)")
      (is (contains? cfg :mcp/source) "M20 key intact")
      (is (false? (:enabled? (:mcp/source cfg)))
          "the shipped :mcp/source default stays :enabled? false")
      (is (contains? (:mcp/source cfg) :environment/registry)
          ":mcp/source carries the :environment/registry injection")
      (is (instance? integrant.core.Ref
                     (get (:mcp/source cfg) :environment/registry))
          "the injection is an #ig/ref to the environment registry")
      (is (contains? cfg :mcp/manager) "M5 key intact"))))

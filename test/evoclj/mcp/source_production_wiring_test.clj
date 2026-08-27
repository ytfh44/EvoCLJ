(ns evoclj.mcp.source-production-wiring-test
  "M20 (P0, e2e#1) — McpSource PRODUCTION WIRING.

   Verifies the host actually instantiates and uses an `McpSource` LiveSource
   through the real Integrant path (resources/system.edn switch -> sys/init ->
   ig/init-key :mcp/source -> evoclj.mcp.source/make-mcp-source), and that
   enabling the new wiring does NOT break the EXISTING static MCP path (the
   shipped :mcp/bridge provider in :provider/registry).

   The wiring is a SWITCH: :mcp/source {:enabled? <bool> ...}. The default
   shipped value is :enabled? false (fail-safe: the system starts and the
   legacy MCP path is untouched). Flipping it to true makes the production
   system build a real McpSource.

   The six required path classes:
     - happy: switch ON builds a WORKING McpSource (real production
       make-mcp-source + snapshot! pipeline) AND the static :mcp/bridge
       provider still serves;
     - each new branch: ON builds / OFF does not build;
     - fault >=2: enabled but missing :source/id / :transport-config fails
       closed with a typed error (and the wired path runs the fail-closed
       descriptor pipeline when a discovered tool lacks a schema); the
       default (key absent / :enabled? false) is safe;
     - concurrency: two inits build independent source records (init is not
       sharing mutable production state);
     - regression: the old static MCP path (:mcp/bridge) is intact when the
       switch is ON;
     - doc/behavior consistency: resources/system.edn carries the :mcp/source
       switch key.

   INV-09: the working-source proof drives the REAL production pipeline —
   sys/init -> ig/init-key :mcp/source -> make-mcp-source -> snapshot! ->
   stable-descriptor (M11 fail-closed) -> detect-collisions! (M12) ->
   payload->sorted. The :discover-fn used here is a documented, first-class
   production field of make-mcp-source (see evoclj.mcp.source ns docstring and
   refresh_schema_test.clj), standing in only for the network I/O, NOT
   bypassing any production component. The descriptor fail-closed test proves
   the real stable-descriptor runs in the wired path."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.environment.source :as env-src]
            [evoclj.kernel.system :as sys]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.source :as mcp-source]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as provider-registry]
            [integrant.core :as ig])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; --- temp helpers -------------------------------------------------------------

(defn- temp-dir
  "A fresh throwaway directory; the host never touches real stores."
  []
  (Files/createTempDirectory "evoclj-m20-" (make-array FileAttribute 0)))

(defn- resolve-path
  [root name]
  (str (.resolve root name)))

(def ^:private sha256-id
  (str "sha256:" (apply str (repeat 64 "7"))))

(defn- provider-catalog
  []
  {:reasoning/high {:provider :fixture
                    :provider-model "fixture-model-v1"
                    :adapter-version "1"}})

;; a well-formed discovered MCP tool (string-keyed JSON schemas, both present)
(defn- mcp-tool
  [name]
  {:mcp/name name
   :mcp/input-schema {"type" "object"
                      "properties" {"text" {"type" "string"}}
                      "required" ["text"]}
   :mcp/output-schema {"type" "object"
                       "properties" {"text" {"type" "string"}}}})

;; --- config builders ----------------------------------------------------------

(defn- base-config
  "Minimal host config (mirrors resources/system.edn shape) under `root`,
   with the :mcp/source switch set by `source-cfg` and an :mcp/bridge
   provider present in :provider/registry to exercise the STATIC MCP path."
  [root source-cfg]
  (let [db (resolve-path root "evoclj.db")
        cas-root (resolve-path root "cas")
        seed (resolve-path root "seed")]
    {:store/sqlite db
     :store/cas {:root cas-root :verify false}
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
     :mcp/source source-cfg
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
      :event/session-id (str (random-uuid))}}))

(defn- source-on-config
  "base-config with the :mcp/source switch ENABLED, wired through the shared
   host-owned :mcp/manager (ig/ref). `discover-fn` stands in for the network
   I/O (supported production option of make-mcp-source)."
  [root discover-fn]
  (base-config
   root
   {:enabled? true
    :source/id :mcp/production
    :transport-config {:type :stdio :command "echo" :args []}
    :mcp/server-id "production"
    :manager (ig/ref :mcp/manager)
    :discover-fn discover-fn}))

(defn- assert-static-bridge-serves
  [bridge-reg msg]
  (let [entry (provider-registry/lookup bridge-reg :mcp/static-bridge)]
    (is (some? entry) msg)
    (is (= :mcp/static-bridge (:tool/id (:descriptor entry)))
        "the static bridge provider is registered under its tool id")
    (is (= :mcp/static-bridge
           (:tool/id (proto/describe (:provider entry))))
        "the static bridge provider describes and serves via the Provider protocol")))

;; --- helper: unwrap integrant's build exception to the host's typed error ---
(defn- host-error-type
  "sys/init wraps a component build failure in an
   :integrant.core/build-threw-exception ExceptionInfo; the host's own typed
   error is the ex-cause."
  [thrown]
  (:error/type (ex-data (or (ex-cause thrown) thrown))))

;; =============================================================================
;; RED -> GREEN: these tests assert the production wiring. Before the
;; ig/init-key :mcp/source is implemented (and before :mcp/source is in
;; system.edn), they FAIL (ig/init throws on an unknown key / nil source).
;; =============================================================================

(deftest switch-on-builds-working-mcpsource-and-static-path-still-serves
  (testing "switch ON: sys/init builds a WORKING McpSource (real production
             make-mcp-source + snapshot! pipeline) AND the existing static
             :mcp/bridge provider still serves"
    (let [root (temp-dir)
          tools-atom (atom [(mcp-tool "tool-a") (mcp-tool "tool-b")])
          system (sys/init (source-on-config root #(deref tools-atom)))
          source (:mcp/source system)
          bridge-reg (:provider/registry system)]
      (testing "the production system actually instantiated an McpSource"
        (is (some? source) "switch ON -> :mcp/source component present")
        (is (= evoclj.mcp.source.McpSource (class source))
            "it is a real McpSource record, built via the Integrant path")
        (is (satisfies? env-src/LiveSource source)
            "it satisfies the LiveSource protocol (production use)"))
      (testing "the McpSource is WORKING: the real snapshot! pipeline runs
                 stable-descriptor + detect-collisions! and yields the
                 discovered tools under their composite [server remote] ids"
        (let [snap (env-src/snapshot! source)
              tools (get-in snap [:payload :tools])]
          (is (= :mcp/production (:source/id snap)))
          (is (= 2 (count tools)))
          (is (contains? tools ["production" "tool-a"]))
          (is (contains? tools ["production" "tool-b"]))
          ;; the fail-closed descriptor pipeline ran: real input schema present
          (is (not= :any (:input-schema (get tools ["production" "tool-a"])))))
      (testing "REGRESSION: the STATIC MCP path (:mcp/bridge provider)
                 is still intact and serving when the switch is ON"
        (assert-static-bridge-serves bridge-reg "static bridge serves with switch ON"))
      (sys/halt! system)))))

(deftest switch-off-does-not-build-mcpsource
  (testing "switch OFF (default-safe): the system starts and :mcp/source is
             absent/nil - the legacy static MCP path is untouched"
    (let [root (temp-dir)
          system (sys/init (base-config
                            root
                            {:enabled? false
                             :source/id :mcp/off
                             :transport-config {:type :stdio :command "echo" :args []}
                             :mcp/server-id "off"
                             :manager (ig/ref :mcp/manager)}))
          source (:mcp/source system)
          bridge-reg (:provider/registry system)]
      (is (nil? source) "disabled switch yields no source")
      (assert-static-bridge-serves bridge-reg "static bridge serves with switch OFF")
      (sys/halt! system))))

(deftest switch-default-safe-when-key-absent
  (testing "the shipped default (:mcp/source absent / :enabled? false) is safe:
             the system starts and the static MCP path serves"
    (let [root (temp-dir)
          cfg (dissoc (base-config
                       root
                       {:enabled? false
                        :source/id :mcp/x
                        :transport-config {:type :stdio :command "echo" :args []}
                        :mcp/server-id "x"
                        :manager (ig/ref :mcp/manager)})
                      :mcp/source)
          system (sys/init cfg)
          bridge-reg (:provider/registry system)]
      (is (nil? (:mcp/source system)) "no source when the key is absent")
      (assert-static-bridge-serves bridge-reg "static bridge serves with key absent")
      (sys/halt! system))))

(deftest switch-on-missing-source-id-fails-closed
  (testing "switch ON but missing :source/id fails closed with a typed error
             (the system does NOT start with a broken source)"
    (let [root (temp-dir)
          cfg (base-config
               root
               {:enabled? true
                :transport-config {:type :stdio :command "echo" :args []}
                :mcp/server-id "bad"
                :manager (ig/ref :mcp/manager)})
          ex (try (sys/init cfg)
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex) "init must surface the configuration error")
      (is (= :mcp/config-invalid (host-error-type ex))
          "missing :source/id -> typed :mcp/config-invalid"))))

(deftest switch-on-missing-transport-config-fails-closed
  (testing "switch ON but missing :transport-config fails closed with a typed error"
    (let [root (temp-dir)
          cfg (base-config
               root
               {:enabled? true
                :source/id :mcp/bad
                :mcp/server-id "bad"
                :manager (ig/ref :mcp/manager)})
          ex (try (sys/init cfg)
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex))
      (is (= :mcp/config-invalid (host-error-type ex))
          "missing :transport-config -> typed :mcp/config-invalid"))))

(deftest switch-on-discovered-tool-without-schema-fails-closed
  (testing "the wired McpSource runs the REAL fail-closed descriptor pipeline:
             a discovered tool missing its output schema makes snapshot! throw
             :mcp/schema-required (M11), not a silent :any default"
    (let [root (temp-dir)
          bad-tool {:mcp/name "no-output"
                    :mcp/input-schema {"type" "object"
                                       "properties" {"text" {"type" "string"}}
                                       "required" ["text"]}}
          system (sys/init (source-on-config root (fn [] [bad-tool])))
          source (:mcp/source system)]
      (let [ex (try (env-src/snapshot! source)
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex) "snapshot! must not silently accept a schema-less tool")
        (is (= :mcp/schema-required (:error/type (ex-data ex)))
            "missing output schema -> typed :mcp/schema-required (fail-closed)"))
      (sys/halt! system))))

(deftest two-inits-build-independent-sources
  (testing "concurrency/isolation: two sys/init calls with the switch ON build
             INDEPENDENT McpSource records (no shared mutable production state)"
    (let [root (temp-dir)
          tools-atom (atom [(mcp-tool "shared")])
          s1 (sys/init (source-on-config root #(deref tools-atom)))
          s2 (sys/init (source-on-config root #(deref tools-atom)))
          src1 (:mcp/source s1)
          src2 (:mcp/source s2)]
      (is (some? src1))
      (is (some? src2))
      (is (not (identical? src1 src2))
          "two inits produce distinct McpSource records")
      (is (= 1 (count (get-in (env-src/snapshot! src1) [:payload :tools]))))
      (is (= 1 (count (get-in (env-src/snapshot! src2) [:payload :tools]))))
      (sys/halt! s1)
      (sys/halt! s2))))

(deftest system-edn-carries-mcp-source-switch
  (testing "resources/system.edn declares the :mcp/source switch key (doc/
             behavior consistency) with a fail-safe default"
    (let [cfg (ig/read-string (slurp (io/resource "system.edn")))]
      (is (contains? cfg :mcp/source)
          ":mcp/source present in the shipped host config")
      (is (false? (:enabled? (:mcp/source cfg)))
          "the shipped default is :enabled? false (fail-safe)"))))

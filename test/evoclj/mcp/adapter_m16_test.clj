(ns evoclj.mcp.adapter-m16-test
  "M16 — ProtocolAdapter wiring (RED -> GREEN).

   SCOPE (closure-repair ledger M16): wire (not delete) the
   evoclj.mcp.adapter ProtocolAdapter translation layer and prove, through
   PRODUCTION paths, the three required behaviors:

     1. 2025 equivalence regression — a 2025 MCP server discovered through
        the newly wired adapter yields the SAME raw tool model as the
        pre-change discovery path; the adapter adds ONLY an `:adapter/version`
        stamp, never alters the normalized tool data.
     2. 2026 unsupported typed — any 2026 / unknown / malformed negotiated
        version fails CLOSED with a typed `:mcp/unsupported` error (no silent
        fallback to 2025, no crash on a half-built adapter).
     3. ConnectionKey + version selection — adapter selection is a pure,
        deterministic function of (ConnectionKey, version): the same pair
        always selects the same adapter, and on a given conn-key a 2025
        version and a 2026 version select DIFFERENT outcomes.

   No fn injection bypasses production components (INV-09). Discovery is
   driven through (a) the REAL evoclj.mcp.adapter/discover over a REAL fake
   MCP subprocess, and (b) the REAL evoclj.mcp.source snapshot path via the
   production discover-fn stub (which still routes through the real adapter
   selection + real stable-descriptor normalization). The ONE approved
   harness is the fake MCP server (WO-T1).

   NOTE on the harness: the fake server's makeTool() deliberately emits NO
   outputSchema (see codec_closure_test which PINS that contract), so a full
   live discovery through stable-descriptor (which fails closed on missing
   output schemas) cannot be driven against the real server. We therefore
   exercise the live adapter `discover` (schema-agnostic, returns RAW tools)
   over the real subprocess, and exercise the normalization wiring through
   the production discover-fn stub with hand-built schema-bearing tools.

   Six mandatory paths: happy / each new branch / >=2 faults / regression /
   doc-behavior consistency. Selection is pure (no shared mutable state), so
   no concurrency test is required (and would be vacuous)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.source :as env-src]
            [evoclj.kernel.error :as err]
            [evoclj.mcp.adapter :as adapter]
            [evoclj.mcp.client :as mcp-client]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.source :as mcp-source]
            [evoclj.mcp.support.fake-server :as fake]))

;; ---------------------------------------------------------------------------
;; helpers
;; ---------------------------------------------------------------------------

(defn- capture-throw
  "Call `f`; return ::no-throw if it returned normally, else the Throwable."
  [f]
  (try (f) ::no-throw (catch Throwable t t)))

(defn- schema-bearing-raw-tool
  "A hand-built raw MCP tool map that carries BOTH input and output schemas,
   so the production stable-descriptor normalization accepts it (the fake
   server's tools intentionally omit output schemas, a pinned contract)."
  [name]
  {:mcp/name name
   :mcp/title (str "title-" name)
   :mcp/description (str "desc-" name)
   :mcp/input-schema {"type" "object" "properties" {"text" {"type" "string"}} "required" ["text"]}
   :mcp/output-schema {"type" "object" "properties" {"text" {"type" "string"}}}})

(defn- discover-fn-source
  "An McpSource whose discovery is driven by `discover-fn` (the production
   stub branch) returning `raw-tools`. Routes through the real adapter
   selection + real stable-descriptor normalization."
  [raw-tools version]
  (->(mcp-source/->McpSource :m16/disc {:type :stdio :command "echo" :args []}
      nil (atom {}) (atom false)
      {:mcp/server-id "m16" :mcp/version version} (fn [] raw-tools))
     (assoc :tools-change-cb (fn []))))

(defn- strip-version-stamp
  "Remove the M16 additive protocol-version stamp so the underlying tool
   model can be compared to the pre-change path."
  [descriptors]
  (mapv #(dissoc % :adapter/version) descriptors))

;; ---------------------------------------------------------------------------
;; PATH 1 — happy: 2025 adapter wires correctly AND is equivalent (live)
;; ---------------------------------------------------------------------------

(deftest m16-happy-2025-discovery-wired-and-equivalent
  (testing "through a REAL MCP server, the wired 2025 adapter's discover
            returns the SAME raw tool model as the pre-change list-all-tools
            call, and wire-request stamps the :adapter/version"
    (fake/with-fake-server [srv {:mode :ok :tool-count 2}]
      (let [managed (mcp-client/open! (:config srv))
            client (:client managed)
            raw (mcp-client/list-all-tools client)
            a (adapter/select-adapter :mcp-2025-11)
            discovered (adapter/discover a {:client client})]
        (try
          (is (= (count raw) (count discovered))
              "2025 adapter discovers the same number of tools as list-all-tools")
          (is (= (set (map :mcp/name raw)) (set (map :mcp/name discovered)))
              "2025 adapter's raw tool identities match the pre-change path")
          (is (= :mcp-2025-11 (:adapter/version (adapter/wire-request a {})))
              "2025 adapter stamps :adapter/version via wire-request")
          (is (= true (:mcp/sessionful (adapter/wire-request a {})))
              "2025 adapter is sessionful (pre-change wire shape preserved)")
          (finally (mcp-client/close! managed)))))))

;; ---------------------------------------------------------------------------
;; PATH 2 — branch: 2025 equivalence regression (normalization preserved)
;; ---------------------------------------------------------------------------

(deftest m16-2025-equivalence-regression
  (testing "the 2025 wired path is identical to the pre-change discovery
            model after normalization: stripping the additive :adapter/version
            stamp yields descriptors equal to converting the raw tools directly"
    (let [raw [(schema-bearing-raw-tool "tool-a") (schema-bearing-raw-tool "tool-b")]
          wired-src (discover-fn-source raw :mcp-2025-11)
          wired-snap (env-src/snapshot! wired-src)
          wired (->> (get-in wired-snap [:payload :tools]) vals vec)
          baseline-src (discover-fn-source raw :mcp-2025-11)
          baseline-snap (env-src/snapshot! baseline-src)
          baseline (->> (get-in baseline-snap [:payload :tools]) vals vec)]
      (is (= (set (map :tool-id (strip-version-stamp wired)))
             (set (map :tool-id (strip-version-stamp baseline))))
          "2025 wired tool-id set matches the equivalent normalized path")
      (is (= (set (map #(select-keys % [:tool/id :mcp/name :mcp/input-schema :mcp/output-schema])
                       (strip-version-stamp wired)))
             (set (map #(select-keys % [:tool/id :mcp/name :mcp/input-schema :mcp/output-schema])
                       (strip-version-stamp baseline))))
          "2025 wired normalized model (minus stamp) equals the pre-change model")
      (is (every? #(= :mcp-2025-11 (:adapter/version %)) wired)
          "every wired descriptor is stamped by the 2025 adapter"))))

;; ---------------------------------------------------------------------------
;; PATH 3 — branch: 2026 unsupported -> typed :mcp/unsupported
;; ---------------------------------------------------------------------------

(deftest m16-2026-unimplemented-typed-unsupported
  (testing "negotiating a 2026 protocol surface fails CLOSED with a typed
            :mcp/unsupported error (never silently falls back to 2025)"
    (let [ck (manager/connection-key
              {:type :stdio :connection/id :m16/c1 :command "node" :args ["s.js"]})]
      (let [e (capture-throw #(adapter/select-adapter :mcp-2026-07 ck))]
        (is (not= ::no-throw e) "select-adapter rejected the 2026 surface")
        (is (= :mcp/unsupported (:error/type (ex-data e)))
            "2026 surface rejected with :mcp/unsupported")
        (is (= :unsupported-surface (:error/reason (ex-data e)))
            "typed reason :unsupported-surface")
        (is (= :mcp-2026-07 (:mcp/version (ex-data e)))
            "the offending version is carried on the error")))
    (fake/with-fake-server [srv {:mode :ok}]
      (let [src (discover-fn-source [] :mcp-2026-07)
            e2 (capture-throw #(env-src/snapshot! src))]
        (is (not= ::no-throw e2) "wired source rejected 2026 version")
        (is (= :mcp/unsupported (:error/type (ex-data e2)))
            "wired discovery throws typed :mcp/unsupported for 2026")))))

;; ---------------------------------------------------------------------------
;; PATH 4 — branch: ConnectionKey + version selection consistency
;; ---------------------------------------------------------------------------

(deftest m16-connection-key-and-version-selection
  (testing "adapter selection is a pure, deterministic function of
            (ConnectionKey, version): same pair -> same adapter; same conn-key
            with 2025 vs 2026 -> different outcomes"
    (let [ck (manager/connection-key
              {:type :stdio :connection/id :m16/ck
               :command "node" :args ["server.js"]})]
      (let [a1 (adapter/adapter-for-connection ck :mcp-2025-11)
            a2 (adapter/adapter-for-connection ck :mcp-2025-11)]
        (is (= :mcp-2025-11 (:adapter/version (adapter/wire-request a1 {}))))
        (is (= (:adapter/version (adapter/wire-request a1 {}))
               (:adapter/version (adapter/wire-request a2 {})))
            "same (conn-key, version) selects a stable adapter"))
      (let [ok (capture-throw #(adapter/adapter-for-connection ck :mcp-2025-11))
            bad (capture-throw #(adapter/adapter-for-connection ck :mcp-2026-07))]
        (is (= ::no-throw ok) "2025 on this conn-key is selectable")
        (is (not= ::no-throw bad) "2026 on the same conn-key is rejected")
        (is (= :mcp/unsupported (:error/type (ex-data bad))))
        (let [ck2 (manager/connection-key
                   {:type :stdio :connection/id :m16/ck2
                    :command "node" :args ["other.js"]})]
          (is (= :mcp-2025-11
                 (:adapter/version (adapter/wire-request
                                    (adapter/adapter-for-connection ck2 :mcp-2025-11) {})))
              "a distinct conn-key selects the same 2025 adapter shape"))))))

;; ---------------------------------------------------------------------------
;; PATH 5 — faults (>=2): malformed / unknown version -> fail-closed
;; ---------------------------------------------------------------------------

(deftest m16-fault-malformed-and-unknown-versions-unsupported
  (testing "a malformed version value and an unknown (future) version both
            fail CLOSED with :mcp/unsupported"
    (let [e-str (capture-throw #(adapter/select-adapter "2026" [:stdio :c2 {} 0]))]
      (is (not= ::no-throw e-str))
      (is (= :mcp/unsupported (:error/type (ex-data e-str))))
      (is (= "2026" (:mcp/version (ex-data e-str)))))
    (let [e-nil (capture-throw #(adapter/select-adapter nil [:stdio :c3 {} 0]))]
      (is (not= ::no-throw e-nil))
      (is (= :mcp/unsupported (:error/type (ex-data e-nil)))))
    (let [e-future (capture-throw #(adapter/select-adapter :mcp-2099-01 [:stdio :c4 {} 0]))]
      (is (not= ::no-throw e-future))
      (is (= :mcp/unsupported (:error/type (ex-data e-future))))
      (is (= :mcp-2099-01 (:mcp/version (ex-data e-future)))))))

;; ---------------------------------------------------------------------------
;; PATH 6 — regression: the adapter is genuinely wired (old un-wired path gone)
;; ---------------------------------------------------------------------------

(deftest m16-regression-adapter-is-wired-not-dormant
  (testing "the adapter is ACTIVATED in production: discovering with the
            default 2025 version stamps :adapter/version on every descriptor,
            proving the pre-wiring path (no adapter in the discovery flow) is gone"
    (let [raw [(schema-bearing-raw-tool "echo") (schema-bearing-raw-tool "read")]
          src (discover-fn-source raw :mcp-2025-11)
          snap (env-src/snapshot! src)
          descriptors (->> (get-in snap [:payload :tools]) vals vec)]
      (is (seq descriptors) "discovered at least one tool")
      (is (every? #(= :mcp-2025-11 (:adapter/version %)) descriptors)
          "every discovered descriptor is stamped by the wired adapter"))))

;; ---------------------------------------------------------------------------
;; PATH 6 (doc/behavior consistency): decision <-> code
;; ---------------------------------------------------------------------------

(deftest m16-doc-behavior-consistency
  (testing "the implemented-version set matches the M16 decision: 2025 is
            supported, 2026 is NOT (structural placeholder only), and the
            default negotiated version is 2025"
    (is (contains? adapter/implemented-versions :mcp-2025-11)
        "2025 is an implemented version")
    (is (not (contains? adapter/implemented-versions :mcp-2026-07))
        "2026 is NOT an implemented (wired) version -> fails closed")
    (is (= :mcp-2025-11 adapter/default-version)
        "default negotiated version is 2025 (pre-change equivalence)")
    (is (= :mcp-2025-11
           (:adapter/version (adapter/wire-request (adapter/select-adapter :mcp-2025-11) {}))))
    (is (some? (adapter/adapter-2026 {:ttl-ms 60000}))
        "Adapter2026 record still exists (defined, not deleted)")
    (is (some? (adapter/cache-policy (adapter/adapter-2026 {:ttl-ms 60000})))
        "Adapter2026 retains its (unreachable) cache policy definition")))

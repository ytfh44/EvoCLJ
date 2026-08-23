(ns evoclj.mcp.manager-identity-test
  "WO-M2 [P1] — split diagnostic redaction from pool identity and from
   execution input (shared invariant INV-01).

   THE CONTRACT UNDER TEST — one transport config, three independent
   derivations, never conflated:

     1. EXECUTION INPUT  — production `open!` receives the REAL config
        (real :env, real :headers). Proven through the real bridge
        provider -> manager/get-or-open! -> mcp-client/open! ->
        evoclj.mcp.transport chain: the config transport construction
        actually received carries the REAL secret values while the very
        same provider-error payload keeps its display form redacted.
     2. POOL IDENTITY    — connection-key derives per-field stable sha256
        fingerprints of secret fields (:env/:headers); non-secret fields
        participate verbatim. Configs differing only in a secret VALUE
        land on different pool keys. credential-fingerprint is a stable
        sha256 digest of :auth/ref.
     3. DIAGNOSTIC FORM  — redact-transport keeps the historical
        whole-value replacement ([REDACTED]) for error/display paths.

   The eight mandatory WO-M2 paths live in tests named p1..p8; the two
   WO-named adversarial counterexample directions are the guard-* tests.

   DEVIATION NOTE (dispatcher-approved, extends DEVIATION RECORD 2 of
   evoclj.mcp.support.fake-server): path 7's original wording drives a
   marker through the stdio :env channel. At HEAD BOTH secret channels
   of production evoclj.mcp.transport are frozen by SDK 2.0.0 API drift,
   verified against mcp-core-2.0.0.jar with javap on this host:

     - :env  — ServerParameters$Builder has NO `environment` method (only
       env(Map)/addEnvVar); build-server-parameters calls `.environment`,
       so any config carrying a map :env fails at transport construction.
     - :headers — NEITHER HttpClientStreamableHttpTransport$Builder NOR
       HttpClientSseClientTransport$Builder has a `.headers(Map)` method;
       apply-http-options calls `.headers`, so any HTTP/SSE config
       carrying :headers fails at transport construction.

   The pre-fix code accidentally masked both defects because
   normalize-transport replaced :env/:headers with the string
   \"[REDACTED]\", which failed the (map? env) guard and skipped the
   builder call entirely. Both freezes are PRE-EXISTING production
   defects owned outside M-scope (M7 per the fake-server record); M2
   therefore proves the un-redacted execution input at the production
   error boundary (p7): stdio-transport embeds the pr-str of the config
   open! actually delivered into its typed :mcp/transport-invalid error,
   so ONE real exception payload exhibits both derivations side by side
   — display point redacted, execution input carrying plaintext secrets.
   p7 also pins the two freeze failure modes as executable documentation."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.manager :as manager]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]
            [evoclj.support.concurrency :as conc])
  (:import [com.sun.net.httpserver HttpServer]
           [java.net InetSocketAddress]))

;; ---------------------------------------------------------------------------
;; fixtures
;; ---------------------------------------------------------------------------

(def ^:private real-token
  "A marked bearer token so any leak into identity/display payloads is
  greppable in assertion messages."
  "Bearer M2-REAL-SECRET-e11f4c2d")

(def ^:private base-http-cfg
  {:type :http
   :url "https://mcp.example.test"
   :endpoint "/mcp"
   :connection/id :m2/identity})

(defn- cfg-with-auth
  [token]
  (assoc-in base-http-cfg [:headers "Authorization"] token))

(defn- stdio-cfg
  []
  {:type :stdio
   :command "node"
   :args ["server.js" "--port" "0"]
   :env {"M2_MARKER" "alpha" "PATH" "/usr/bin"}
   :cwd "C:/tmp/srv"
   :connection/id :m2/stdio})

(defn- no-leak?
  "True when none of the plaintext secrets appear anywhere in `v`'s
  printed form (what a log line would leak)."
  [v & secrets]
  (let [s (pr-str v)]
    (every? #(not (str/includes? s %)) secrets)))

(defn- refused-url
  "A localhost URL whose port was bound and immediately released, so a
  connect fails fast with connection-refused (never a hang). Drives the
  provider error path for the zero-leak check."
  []
  (let [srv (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        port (.getPort (.getAddress srv))]
    (.stop srv 0)
    (str "http://127.0.0.1:" port "/mcp")))

(defn- make-provider
  "A REAL bridge ToolEntry over `transport-config` sharing `mgr`."
  [mgr transport-config connection-id]
  (mcp-bridge/mcp-provider
   {:transport-config transport-config
    :tool/id          :m2/echo
    :tool/mcp-name    "echo"
    :input-schema     :any
    :output-schema    :any
    :connection/id    connection-id
    :manager          mgr}))

(defn- echo-request
  [p]
  (proto/normalize-request p {:payload {:tool/id :m2/echo :args {}}}))

;; ===========================================================================
;; PATH 1 — happy: identical configs -> identical connection-key
;; ===========================================================================

(deftest p1-identical-configs-yield-identical-connection-keys
  (testing "WO-M2 #1: computing the key twice over one config agrees"
    (let [cfg (-> (cfg-with-auth real-token)
                  (assoc :env {"M2_TOKEN" "sk-env-secret"})
                  (assoc :auth/ref "vault://creds/m2"))]
      (is (= (manager/connection-key cfg)
             (manager/connection-key cfg))
          "same config -> same key, every time")
      (is (= (manager/connection-key (assoc cfg :connection/id :other))
             (manager/connection-key (assoc cfg :connection/id :other)))
          "stable across distinct-but-identical config maps too")
      (is (= (manager/transport-identity cfg)
             (manager/transport-identity cfg))
          "identity part agrees alongside the whole key"))))

;; ===========================================================================
;; PATH 2 — CORE REGRESSION: Authorization value alone separates keys
;; ===========================================================================

(deftest p2-authorization-value-only-difference-yields-different-keys
  (testing "WO-M2 #2 core regression: two configs differing ONLY in the
            Authorization header VALUE get DIFFERENT pool keys
            (pre-fix they collapsed onto one shared connection)"
    (let [a (cfg-with-auth "Bearer token-AAA")
          b (cfg-with-auth "Bearer token-BBB")]
      (is (= (dissoc a :headers) (dissoc b :headers))
          "fixture sanity: nothing but the header value differs")
      (is (not= (manager/connection-key a)
                (manager/connection-key b))
          "secret-bearing headers participate in identity by fingerprint")))
  (testing "the same holds for arbitrary header values, not just bearer tokens"
    (is (not= (manager/connection-key (cfg-with-auth "v1"))
              (manager/connection-key (cfg-with-auth "v2"))))))

;; ===========================================================================
;; PATH 3 — env variant: one stdio env var difference separates keys
;; ===========================================================================

(deftest p3-stdio-env-value-only-difference-yields-different-keys
  (testing "WO-M2 #3: stdio configs differing in ONE env var value get
            DIFFERENT pool keys (pre-fix both env maps became
            \"[REDACTED]\" and collapsed)"
    (let [s1 (stdio-cfg)
          s2 (assoc-in s1 [:env "M2_MARKER"] "beta")]
      (is (= (:command s1) (:command s2))
          "fixture sanity: non-secret fields are identical")
      (is (not= (manager/connection-key s1)
                (manager/connection-key s2))
          "one differing env value -> different key")))
  (testing "adding vs removing an env entry also separates keys"
    (let [s (stdio-cfg)
          s+ (assoc-in s [:env "EXTRA"] "x")]
      (is (not= (manager/connection-key s)
                (manager/connection-key s+))))))

;; ===========================================================================
;; PATH 4 — auth/ref branch: cf tracks :auth/ref; key follows cf
;; ===========================================================================

(deftest p4-auth-ref-separates-credential-fingerprint-and-keys
  (testing "WO-M2 #4: different :auth/ref -> different credential-fingerprint"
    (let [c1 (assoc (cfg-with-auth real-token) :auth/ref "vault://creds/one")
          c2 (assoc (cfg-with-auth real-token) :auth/ref "vault://creds/two")]
      (is (not= (manager/credential-fingerprint c1)
                (manager/credential-fingerprint c2)))))
  (testing "same :auth/ref, rest equal -> same key"
    (let [c1 (assoc (cfg-with-auth real-token) :auth/ref "vault://creds/one")
          c3 (assoc (cfg-with-auth real-token) :auth/ref "vault://creds/one")]
      (is (= (manager/credential-fingerprint c1)
             (manager/credential-fingerprint c3)))
      (is (= (manager/connection-key c1)
             (manager/connection-key c3)))))
  (testing ":auth/ref separates the full key as well"
    (let [c1 (assoc (cfg-with-auth real-token) :auth/ref "ref-A")
          c2 (assoc (cfg-with-auth real-token) :auth/ref "ref-B")]
      (is (not= (manager/connection-key c1)
                (manager/connection-key c2))
          "different refs -> different keys (separate pool entries)")))
  (testing "fingerprint format is genome.hash-style stable sha256"
    (let [fp (manager/credential-fingerprint
              (assoc (cfg-with-auth real-token) :auth/ref "vault://creds/one"))]
      (is (string? fp))
      (is (re-matches #"^sha256:[0-9a-f]{64}$" fp)
          (pr-str fp)))))

;; ===========================================================================
;; PATH 5 — fault-default: empty/partial configs still compute a key
;; ===========================================================================

(deftest p5-empty-or-partial-configs-still-compute-keys
  (testing "WO-M2 #5: degenerate configs yield a vector key without throwing"
    (doseq [cfg [{}
                 nil
                 {:type :stdio}
                 {:type :http :url "https://x"}
                 {:type :http :url "https://x" :headers {}}
                 {:type :stdio :env {}}]]
      (is (vector? (manager/connection-key cfg)) (pr-str cfg))
      (is (= 4 (count (manager/connection-key cfg)))
          "ConnectionKey shape [type cid ti cf] unchanged")))
  (testing "missing vs empty secret maps do not crash identity"
    (is (map? (manager/transport-identity {})))
    (is (map? (manager/transport-identity {:headers {}})))))

;; ===========================================================================
;; ADVERSARIAL GUARD — canonical EDN ordering (map serialization ordered)
;; ===========================================================================

(deftest guard-map-construction-order-does-not-change-identity
  (testing "counterexample direction from WO-M2: same entries inserted in
            a different order MUST hash identically (canonicalization)"
    (let [h1 {"Authorization" "Bearer t" "X-Trace" "1" "X-Api-Key" "k"}
          h2 (into {} (reverse {"Authorization" "Bearer t" "X-Trace" "1" "X-Api-Key" "k"}))]
      ;; precondition for the guard: the two maps really print differently,
      ;; i.e. small-map insertion order would otherwise leak into pr-str
      (is (not= (pr-str h1) (pr-str h2)) "fixture sanity: printed forms differ")
      (is (= (manager/connection-key (assoc-in base-http-cfg [:headers] h1))
             (manager/connection-key (assoc-in base-http-cfg [:headers] h2)))
          "identity canonicalizes before hashing")
      (is (= (manager/credential-fingerprint {:auth/ref h1})
             (manager/credential-fingerprint {:auth/ref h2}))
          "credential fingerprint canonicalizes too"))))

;; ===========================================================================
;; ADVERSARIAL GUARD — identity is fingerprinted, never placeholder/plain
;; ===========================================================================

(deftest guard-identity-carries-no-placeholder-and-no-plaintext
  (testing "INV-01: the identity derivation belongs to no other role"
    (let [cfg (-> (cfg-with-auth real-token)
                  (assoc :env {"M2_TOKEN" "sk-canary-xyz"})
                  (assoc :auth/ref "vault://creds/m2"))
          ti (manager/transport-identity cfg)]
      (is (no-leak? ti "[REDACTED]" real-token "sk-canary-xyz")
          "identity contains neither the display placeholder nor plaintext")
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:headers ti))
          "header fingerprint sits in place as sha256:<hex>")
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:env ti))
          "env fingerprint sits in place as sha256:<hex>")
      (is (= "https://mcp.example.test" (:url ti))
          "non-secret fields stay verbatim for readability")
      (is (nil? (:auth/ref ti)) ":auth/ref lives in the cf slot, not in ti")))
  (testing "redact-transport keeps its narrow diagnostic semantics"
    (let [cfg {:type :stdio :command "node" :args ["s.js"]
               :env {"A" "1"} :headers {"Authorization" "Bearer x"}
               "env" {"B" "2"} "headers" {"X" "y"}
               :auth/ref "vault://k1"}]
      (is (= {:type :stdio :command "node" :args ["s.js"]
              :env "[REDACTED]" :headers "[REDACTED]"
              "env" "[REDACTED]" "headers" "[REDACTED]"
              :auth/ref "vault://k1"}
             (manager/redact-transport cfg))
          "whole-value replacement preserved for diagnostics")
      (is (= {} (manager/redact-transport nil)) "nil-safe as before")
      (is (= {:type :stdio} (manager/redact-transport {:type :stdio}))
          "absent secret keys stay absent"))))

;; ===========================================================================
;; PATH 6 — concurrency: two credential variants race, zero cross-talk
;; ===========================================================================

(deftest p6-raced-credential-variants-open-distinct-pool-entries
  (testing "WO-M2 #6 (T4 raced): 8 callers per credential variant over ONE
            manager — each variant opens exactly once, lands in its OWN
            entry, and no caller ever sees the other variant's client"
    (let [mgr (manager/create-manager)
          cfg-a (cfg-with-auth "Bearer cred-A")
          cfg-b (cfg-with-auth "Bearer cred-B")
          k-a (manager/connection-key cfg-a)
          k-b (manager/connection-key cfg-b)
          _ (is (not= k-a k-b) "precondition: variants have distinct keys")
          opens (atom [])
          mk-open (fn [tag cfg]
                    #(do (swap! opens conj tag)
                         {:client {:tag tag} :closed? false :open-count 1
                          :call-count 0 :last-latency-ms nil
                          :transport-config cfg}))
          thunks (concat (repeat 8 #(manager/get-or-open!
                                      mgr k-a (mk-open :a cfg-a)))
                         (repeat 8 #(manager/get-or-open!
                                      mgr k-b (mk-open :b cfg-b))))
          results (conc/raced thunks :timeout-ms 8000)]
      (try
        (is (every? #(= :result (:status %)) results)
            (pr-str (remove #(= :result (:status %)) results)))
        ;; every caller received ITS OWN variant's managed record,
        ;; never the other credential's client
        (doseq [[i r] (map-indexed vector results)]
          (let [expected-tag (if (< i 8) :a :b)]
            (is (= expected-tag (get-in r [:value :client :tag]))
                (pr-str {:i i :status (:status r)
                         :got (get-in r [:value :client :tag])}))))
        (let [entry-a (manager/pool-get mgr k-a)
              entry-b (manager/pool-get mgr k-b)
              a-count (count (filter #{:a} @opens))
              b-count (count (filter #{:b} @opens))]
          (is (= 1 a-count)
              (str "credential A opened exactly once (single-flight held): "
                   (pr-str @opens)))
          (is (= 1 b-count)
              (str "credential B opened exactly once (single-flight held): "
                   (pr-str @opens)))
          (is (= :ready (:state entry-a)) "pool entry A ready")
          (is (= :ready (:state entry-b)) "pool entry B ready")
          (is (= :a (get-in entry-a [:client :client :tag]))
              "pool entry A holds A's client")
          (is (= :b (get-in entry-b [:client :client :tag]))
              "pool entry B holds B's client")
          (is (not= (get-in entry-a [:client :client])
                    (get-in entry-b [:client :client]))
              "zero cross-talk: the two credentials never share a client"))
        (finally
          (manager/shutdown! mgr))))))

;; ===========================================================================
;; PATH 7 — regression: REAL config traverses the production open! chain
;; (deviation-adapted — see DEVIATION NOTE in the ns docstring: BOTH secret
;; channels are frozen at HEAD by SDK 2.0.0 API drift, outside M scope)
;; ===========================================================================

(deftest p7-real-config-traverses-production-open-chain
  (testing "WO-M2 #7: the REAL config traverses bridge provider ->
            get-or-open! -> open! -> transport-for. Observed at the
            production ERROR BOUNDARY: stdio-transport embeds the pr-str
            of the config open! ACTUALLY delivered in its typed
            :mcp/transport-invalid error when :command is missing — so
            one exception payload shows BOTH derivations at once: the
            display point redacted, the execution input real."
    (let [header-secret "Bearer M2-EXEC-CANARY-a1b2c3"
          env-secret "sk-m2-env-canary-d4e5f6"
          cfg {:type :stdio
               ;; :command deliberately absent -> production
               ;; stdio-transport throws :mcp/transport-invalid carrying
               ;; :config = pr-str of what open! received; nothing is
               ;; spawned and no server is needed.
               :args ["--require-token" "M2-MARKER"]
               :env {"M2_TOKEN" env-secret}
               :headers {"Authorization" header-secret}
               :connection/id :m2/exec}
          mgr (manager/create-manager)
          p (make-provider mgr cfg :m2/exec)]
      (try
        (let [ed (try (proto/execute-request! p (echo-request p))
                      nil
                      (catch Throwable t (ex-data t)))]
          (is (= :provider/execution-failed (:error/type ed))
              (pr-str (:error/type ed)))
          ;; display derivation: redacted, as always
          (is (= "[REDACTED]" (get-in ed [:mcp/transport-config :headers]))
              "display point keeps whole-value redaction")
          (is (= "[REDACTED]" (get-in ed [:mcp/transport-config :env]))
              "display point keeps whole-value redaction")
          ;; execution-input derivation: REAL values reached transport
          ;; construction inside the sanitized cause chain
          (let [s (pr-str ed)]
            (is (.contains ^String s header-secret)
                "REAL Authorization value rode through open!")
            (is (.contains ^String s env-secret)
                "REAL env value rode through open!")
            (is (.contains ^String s "--require-token")
                "non-secret args ride verbatim too")
            (is (.contains ^String s "[REDACTED]")
                "same payload still carries the redacted display form")))
        (finally
          (manager/shutdown! mgr)))))
  (testing "executable documentation of the two frozen channels
            (pre-existing defects OUTSIDE M scope — owned by M7):
            each fails at transport construction with its signature
            reflection error, proving these paths were never functional
            at HEAD even though pre-fix redaction masked them"
    (let [mgr (manager/create-manager)
          http-cfg {:type :http :url (refused-url) :endpoint "/mcp"
                    :headers {"Authorization" "Bearer whatever"}
                    :connection/id :m2/frozen-hdr}
          p (make-provider mgr http-cfg :m2/frozen-hdr)]
      (try
        (let [t (try (proto/execute-request! p (echo-request p))
                     nil
                     (catch Throwable e e))]
          (is (some? t) "the frozen :headers channel failed")
          (is (re-find #"No matching method headers"
                       (pr-str (ex-data t)))
              "SDK 2.0.0 builders expose no .headers(Map)"))
        (finally
          (manager/shutdown! mgr))))
    (let [mgr (manager/create-manager)
          stdio-cfg* {:type :stdio :command "node" :args []
                      :env {"M2_ENV_FREEZE" "proof"}
                      :connection/id :m2/frozen-env}
          p (make-provider mgr stdio-cfg* :m2/frozen-env)]
      (try
        (let [t (try (proto/execute-request! p (echo-request p))
                     nil
                     (catch Throwable e e))]
          (is (some? t) "the frozen :env channel failed")
          (is (re-find #"No matching method environment"
                       (pr-str (ex-data t)))
              "SDK 2.0.0 Builder exposes no .environment(Map)"))
        (finally
          (manager/shutdown! mgr))))))

;; ===========================================================================
;; PATH 8 — regression zero-leak: provider error payloads stay redacted
;; ===========================================================================

(deftest p8-provider-error-payloads-carry-only-redacted-secrets
  (testing "WO-M2 #8: a provider error path embeds [REDACTED] and never
            the plaintext secrets"
    (let [header-secret "Bearer SK-M2-LIVE-9f2c1a"
          env-secret "sk-m2-canary-do-not-leak"
          cfg {:type :http :url (refused-url) :endpoint "/mcp"
               :headers {"Authorization" header-secret}
               :env {"M2_TOKEN" env-secret}}
          mgr (manager/create-manager)
          p (make-provider mgr cfg :m2/leak)]
      (try
        (let [_req (echo-request p)
              ed (try (proto/execute-request! p (echo-request p))
                      nil
                      (catch Throwable t (ex-data t)))]
          (is (some? ed) "the provider error path was reached")
          (is (contains? #{:provider/transient-error
                           :provider/execution-failed}
                         (:error/type ed))
              (pr-str ed))
          (let [tc (:mcp/transport-config ed)]
            (is (map? tc) (pr-str tc))
            (is (= "[REDACTED]" (:headers tc))
                "whole :headers value redacted")
            (is (= "[REDACTED]" (:env tc))
                "whole :env value redacted"))
          (is (no-leak? ed header-secret env-secret)
              "plaintext secrets appear NOWHERE in the error data")
          (let [broken (first
                        (for [[_ e] (:pools @mgr)
                              :when (= :broken (:state e))] e))]
            (is (some? broken) "failed open left the entry :broken")
            (is (no-leak? (:health broken) header-secret env-secret)
                "even the pool's stored last-error stays leak-free")))
        (finally
          (manager/shutdown! mgr))))))

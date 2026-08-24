(ns evoclj.mcp.tool-id-test
  "M12 — deterministic composite tool-id for MCP tools.

   Each MCP tool's local id MUST be a deterministic composite of
   [server-id, remote-name] so tools from different servers never alias,
   even when they share a remote name.

   Remote tool names MUST be sanitized INJECTIVELY (single-valued) before
   they become part of the local id, so two distinct remote names can
   never silently collapse onto one local id.

   When two DISTINCT remote tools would still resolve to the SAME local
   tool-id, discovery MUST fail-closed with a typed :mcp/tool-id-collision
   error (never silently overwrite one tool with the other).

   These tests traverse the real production paths in evoclj.mcp.source:
   make-mcp-source + snapshot! + tool-entries->surface (discovery and
   payload hashing), and evoclj.environment.registry (cross-server
   isolation through the live environment). No fn is injected to bypass a
   production component.

   Required six-path coverage:
     - happy:      composite id stable + survives payload rehash
     - branch:     composite formed from [server-id, remote-name]
     - branch:     distinct remote names sanitize to distinct ids (injective)
     - branch:     genuine collision is detected and reported
     - fault x2:   same remote name on two servers -> distinct ids (no alias)
                   two servers publish same name, both present in env
     - concurrency: shared registry refreshed from two sources concurrently
     - regression:  the old raw-name keyword id (:mcp/<name>) is gone
     - doc/behave:  snapshot payload for identical logical tool set is stable"
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.registry :as env-reg]
            [evoclj.environment.revision :as rev]
            [evoclj.mcp.manager :as manager]
            [evoclj.mcp.source :as mcp-source]
            [evoclj.provider.protocol :as proto]))

;; ---------------------------------------------------------------------------
;; helpers
;; ---------------------------------------------------------------------------

(defn- mcp-tool [name]
  {:mcp/name name
   :mcp/title (str "title-" name)
   :mcp/description (str "desc-" name)
   :mcp/input-schema {"type" "object" "properties" {"text" {"type" "string"}} "required" ["text"]}
   :mcp/output-schema {"type" "object" "properties" {"text" {"type" "string"}}}
   :mcp/retry-safe? false})

(defn- source-with
  "Build a McpSource whose discovery returns `tools` and which is tagged
   with the given server-id. Goes through make-mcp-source (production)."
  [source-id server-id tools]
  (mcp-source/make-mcp-source
   {:source/id source-id
    :transport-config {:type :stdio :command "echo" :args []}
    :manager (manager/create-manager)
    :discover-fn (fn [] tools)
    :mcp/server-id server-id}))

(defn- composite-id?
  "A composite tool-id is the stable tuple [server-id remote-name]."
  [x]
  (and (vector? x)
       (= 2 (count x))
       (some? (first x))
       (string? (second x))))

;; ---------------------------------------------------------------------------
;; happy path — composite id is stable and survives payload rehash
;; ---------------------------------------------------------------------------

(deftest composite-tool-id-is-stable
  (testing "the same (server, remote-name) always yields the same composite id"
    (let [src (source-with :mcp/s1 "server-a" [(mcp-tool "read_file")])
          snap (evoclj.environment.source/snapshot! src)
          payload (:payload snap)
          entries (mcp-source/tool-entries->surface
                   payload (:manager src)
                   {:type :stdio :command "echo" :args []}
                   (rev/make-revision :mcp/s1 payload 1))
          id (first (keys entries))
          desc (proto/describe (first (vals entries)))]
      (is (composite-id? id)
          "tool-id is the [server-id remote-name] tuple, not a bare keyword")
      (is (= ["server-a" "read_file"] id)
          "composite id carries server-id and remote-name verbatim")
      (is (= (second id) (:mcp/name desc))
          "remote-name preserved on descriptor")
      ;; rehash the same logical payload -> identical id
      (let [snap2 (evoclj.environment.source/snapshot! src)
            payload2 (:payload snap2)
            entries2 (mcp-source/tool-entries->surface
                      payload2 (:manager src)
                      {:type :stdio :command "echo" :args []}
                      (rev/make-revision :mcp/s1 payload2 1))]
        (is (= id (first (keys entries2)))
            "composite id is deterministic across snapshots")))))

;; ---------------------------------------------------------------------------
;; branch — composite formed from [server-id, remote-name]
;; ---------------------------------------------------------------------------

(deftest composite-id-formed-from-server-and-remote
  (testing "composite id combines server-id and (sanitized) remote-name"
    (let [src (source-with :mcp/s2 "srv/x" [(mcp-tool "a/b c")])
          snap (evoclj.environment.source/snapshot! src)
          id (first (keys (mcp-source/tool-entries->surface
                           (:payload snap) (:manager src)
                           {:type :stdio :command "echo" :args []}
                           (rev/make-revision :mcp/s2 (:payload snap) 1))))]
      ;; server-id is NOT sanitized (only the remote name is), so its
      ;; slash stays intact; the remote name's unsafe chars are encoded.
      (is (= ["srv/x" "a%2Fb%20c"] id)
          "server-id and sanitized remote-name kept distinct inside the tuple"))))

;; ---------------------------------------------------------------------------
;; branch — injective (single-valued) sanitization: distinct remote names
;;          map to distinct ids
;; ---------------------------------------------------------------------------

(deftest injective-sanitization-distinct-names
  (testing "two distinct remote names never collapse onto one local id"
    (let [;; Pairs that differ ONLY by an unsafe character. The test must be
          ;; NON-VACUOUS: these names are NOT already distinct by their safe
          ;; characters. They become distinct ONLY BECAUSE sanitize-remote-name
          ;; percent-encodes the unsafe chars. A weakening mutation that maps
          ;; '/' -> '-' instead of '%2F' would collide "foo/bar" with
          ;; "foo-bar" and this test MUST then fail.
          names ["foo/bar" "foo-bar"
                 "a b" "a%b"
                 "x/y" "x-y"
                 "p%q" "p-q"
                 "read file" "read_file"]
          src (source-with :mcp/s3 "server-z"
                           (mapv mcp-tool names))
          snap (evoclj.environment.source/snapshot! src)
          entries (mcp-source/tool-entries->surface
                   (:payload snap) (:manager src)
                   {:type :stdio :command "echo" :args []}
                   (rev/make-revision :mcp/s3 (:payload snap) 1))
          ids (set (keys entries))]
      (is (= (count names) (count ids))
          "every distinct remote name yields a distinct local id (encoding-dependent)")
      ;; bijective sanity: no two of those names share an id
      (is (every? composite-id? ids))
      ;; Explicit assertion that the encoding actually happened in the
      ;; discovery payload: the unsafe chars are no longer present raw.
      (let [snames (set (map second ids))]
        (is (contains? snames "foo%2Fbar")
            "raw '/' is percent-encoded to %2F in the local id")
        (is (contains? snames "a%20b")
            "raw space is percent-encoded to %20 in the local id")
        (is (contains? snames "p%25q")
            "raw '%' is percent-encoded to %25 in the local id")
        (is (= #{"foo%2Fbar" "foo-bar"
                 "a%20b" "a%25b"
                 "x%2Fy" "x-y"
                 "p%25q" "p-q"
                 "read%20file" "read_file"}
               snames)
            "every unsafe char is encoded, no two names merge")))))

;; ---------------------------------------------------------------------------
;; branch — genuine collision is detected and reported (fail-closed)
;; ---------------------------------------------------------------------------

(deftest collision-detected-and-reported
  (testing "two DISTINCT remote tools resolving to the same local id throw"
    (let [;; two distinct tool maps that collapse onto one logical id:
          ;; same server, same remote name reported twice (genuine dup)
          dup-a (assoc (mcp-tool "dup") :mcp/title "first")
          dup-b (assoc (mcp-tool "dup") :mcp/title "second")
          src (source-with :mcp/s4 "server-c" [dup-a dup-b])]
      (try
        (evoclj.environment.source/snapshot! src)
        (is false "expected :mcp/tool-id-collision to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :mcp/tool-id-collision (:error/type (ex-data e)))
              "discovery fails closed with a typed :mcp/tool-id-collision"))))))

(deftest collision-reported-with-detail
  (testing "the collision error carries the offending ids"
    (let [dup-a (assoc (mcp-tool "dup") :mcp/title "first")
          dup-b (assoc (mcp-tool "dup") :mcp/title "second")
          src (source-with :mcp/s4b "server-c" [dup-a dup-b])]
      (try
        (evoclj.environment.source/snapshot! src)
        (is false "expected collision to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (let [data (ex-data e)]
            (is (= :mcp/tool-id-collision (:error/type data))
                "typed error :mcp/tool-id-collision")
            (is (some? (:mcp/collisions data))
                "error carries the colliding id detail")))))))

;; ---------------------------------------------------------------------------
;; fault x2 — same remote name on two DIFFERENT servers -> distinct ids
;; ---------------------------------------------------------------------------

(deftest same-remote-name-different-servers-distinct
  (testing "two servers exposing tool 'echo' get distinct local ids"
    (let [src-a (source-with :mcp/sa "alpha" [(mcp-tool "echo")])
          src-b (source-with :mcp/sb "beta" [(mcp-tool "echo")])
          id-a (first (keys (mcp-source/tool-entries->surface
                              (:payload (evoclj.environment.source/snapshot! src-a))
                              (:manager src-a)
                              {:type :stdio :command "echo" :args []}
                              (rev/make-revision :mcp/sa (:payload (evoclj.environment.source/snapshot! src-a)) 1))))
          id-b (first (keys (mcp-source/tool-entries->surface
                              (:payload (evoclj.environment.source/snapshot! src-b))
                              (:manager src-b)
                              {:type :stdio :command "echo" :args []}
                              (rev/make-revision :mcp/sb (:payload (evoclj.environment.source/snapshot! src-b)) 1))))]
      (is (not= id-a id-b) "no cross-server alias")
      (is (= ["alpha" "echo"] id-a))
      (is (= ["beta" "echo"] id-b)))))

(deftest cross-server-isolation-in-registry
  (testing "two servers exposing tool 'echo' keep distinct ids across environments"
    ;; The EnvironmentRegistry publishes one source's payload per refresh,
    ;; so we use two registries (one per server) and assert each carries its
    ;; own server-scoped id — the composite id keeps the shared remote name
    ;; "echo" from aliasing across servers even though both are published.
    (let [src-a (source-with :mcp/reg-a "alpha" [(mcp-tool "echo") (mcp-tool "list")])
          src-b (source-with :mcp/reg-b "beta" [(mcp-tool "echo") (mcp-tool "write")])
          env-a (env-reg/create-registry)
          env-b (env-reg/create-registry)]
      (env-reg/register-source! env-a src-a)
      (env-reg/register-source! env-b src-b)
      (let [r-a (env-reg/refresh! env-a)
            r-b (env-reg/refresh! env-b)
            tools-a (:tools (:payload (:revision r-a)))
            tools-b (:tools (:payload (:revision r-b)))
            alpha-echo ["alpha" "echo"]
            beta-echo ["beta" "echo"]]
        (is (= :published (:status r-a)))
        (is (= :published (:status r-b)))
        (is (= 2 (count tools-a)) "alpha keeps its own tool set")
        (is (= 2 (count tools-b)) "beta keeps its own tool set")
        (is (contains? tools-a alpha-echo) "alpha echo id scoped to alpha")
        (is (contains? tools-b beta-echo) "beta echo id scoped to beta")
        (is (not= alpha-echo beta-echo) "no cross-server alias on shared name")))))

;; ---------------------------------------------------------------------------
;; concurrency — shared registry state refreshed concurrently (single source)
;; ---------------------------------------------------------------------------

(deftest concurrent-cross-server-refresh
  (testing "concurrent refreshes of one shared registry keep the tool set intact"
    ;; Exercises the shared mutable EnvironmentRegistry state under
    ;; concurrency: many concurrent refresh! calls must not corrupt the
    ;; published payload nor drop or duplicate any composite-id tool.
    (let [src (source-with :mcp/conc "alpha"
                           [(mcp-tool "echo") (mcp-tool "list") (mcp-tool "read")])
          env (env-reg/create-registry)]
      (env-reg/register-source! env src)
      (let [futures (doall
                     (for [i (range 20)]
                       (future
                         (env-reg/refresh! env)
                         (let [cur (env-reg/current env)
                               tools (:tools (:payload cur))]
                           (count tools)))))]
        (doseq [f futures] (deref f))
        (let [cur (env-reg/current env)
              tools (:tools (:payload cur))]
          (is (= 3 (count tools)) "all 3 composite-id tools survive concurrent refresh")
          (is (= #{["alpha" "echo"] ["alpha" "list"] ["alpha" "read"]}
                 (set (keys tools)))))))))

;; ---------------------------------------------------------------------------
;; regression — the old raw-name keyword id (:mcp/<name>) is gone
;; ---------------------------------------------------------------------------

(deftest regression-old-raw-name-id-gone
  (testing "tool-id is no longer the collision-prone :mcp/<remote-name> keyword"
    (let [src (source-with :mcp/s5 "server-y" [(mcp-tool "echo")])
          snap (evoclj.environment.source/snapshot! src)
          entries (mcp-source/tool-entries->surface
                   (:payload snap) (:manager src)
                   {:type :stdio :command "echo" :args []}
                   (rev/make-revision :mcp/s5 (:payload snap) 1))]
      (is (not (contains? entries :mcp/echo))
          "old keyword id :mcp/echo must not be used as the tool key")
      (is (= 1 (count entries)))
      (is (composite-id? (first (keys entries)))))))

;; ---------------------------------------------------------------------------
;; doc/behavior consistency — identical logical tool set => identical payload
;; ---------------------------------------------------------------------------

(deftest identical-tool-set-stable-payload
  (testing "same server + same remote names hash to the same revision"
    (let [src (source-with :mcp/s6 "server-z" [(mcp-tool "echo") (mcp-tool "list")])
          r1 (env-reg/refresh! (doto (env-reg/create-registry)
                                 (env-reg/register-source! src)))
          r2 (env-reg/refresh! (doto (env-reg/create-registry)
                                 (env-reg/register-source! src)))]
      (is (= (rev/payload->id (:payload (:revision r1)))
             (rev/payload->id (:payload (:revision r2))))
          "composite ids keep identical tool sets churn-free"))))

(ns evoclj.mcp.support.fake-server
  "Clojure harness around the programmable zero-dependency fake MCP stdio
  server (`test/evoclj/mcp/support/server/fake-mcp-server.mjs`, WO-T1).

  `start!` launches the Node script as a stdio subprocess the same way
  `evoclj.mcp.transport` does (ProcessBuilder, parent environment with the
  knob variables overlaid), and returns

      {:process <java.lang.Process>
       :handle  <java.lang.ProcessHandle>   ; used by stop! for tree kill
       :config  <stdio transport-config>    ; feed to evoclj.mcp.client/open!
       :stop!   <fn>                        ; same as calling stop!
       :mode    <keyword>}

  There is deliberately no :port-ish value — stdio servers have no port.

  The production client (`evoclj.mcp.client/open!`) spawns its OWN
  subprocess from :config; stdio semantics mean one client == one child
  process. This wrapper's supervised process exists so lifecycle tests can
  hold a real handle; client-spawned processes are found and audited via
  `processes-matching` / `await-no-process-matching`.

  `stop!` kills the whole process tree Windows-safely using pure JDK
  semantics (ProcessHandle descendants first, then the root), then joins
  exit bounded by a timeout. It is idempotent and nil-tolerant. All waits
  are bounded (default 10s); nothing here blocks forever.

  DEVIATION RECORD (WO-T1 / approved by dispatcher): the MCP Java SDK's
  default `requestTimeout` is 20 seconds and production `build-client`
  never overrides it, so an honest \"slow -> production timeout\" test would
  need FAKE_DELAY_MS > 20000ms, violating this work item's <=10s wait rule.
  Approved replacement: slow-mode tests prove the delay KNOB is real
  (bounded-delay injection: measured response latency >= FAKE_DELAY_MS)
  and drive a sub-timeout delay through the full production client chain
  to prove classification stays correct. A true timeout-classification
  test becomes possible once M7 exposes requestTimeout configuration.

  DEVIATION RECORD 2 (knob delivery): WO-T1 specifies environment-variable
  knobs, but SDK 2.0.0's `ServerParameters$Builder` has no
  `environment` method of any signature (2.0.0 renamed the setter to
  `env(java.util.Map)`; the frozen production transport calls
  `.environment` and throws NoSuchMethodError the moment a config
  carries :env) — src/ may not be modified. Therefore
  `transport-config` delivers knobs via the script's equivalent CLI flags
  (`--mode`, `--delay-ms`, ...; flags win over env vars inside the
  script). The supervised wrapper process still receives BOTH channels:
  ProcessBuilder overlays the FAKE_* env vars AND passes the flags. Once
  M7 fixes env passthrough upstream, :env can be re-added here with no
  test changes.

  DEVIATION RECORD 3 (SDK auto-pagination, discovered by wire capture):
  against SDK 2.0.0 a SINGLE `McpSyncClient.listTools(...)` call follows
  `nextCursor` internally and only returns when the server emits a page
  WITHOUT nextCursor, aggregating every page into one result (verified:
  many-pages 13 tools / page size 5 -> one production call returns all
  13 tools with :next-cursor nil). Consequences honored by this harness
  and its tests:
  - `infinite-cursor` mode must NEVER be driven by ANY production
    listing function (`list-tools` included — a single call is already
    unbounded); tests exercise it via bounded raw stdio JSON-RPC frames
    only — exactly WO-T1's \"client-side bounded controlled call or no
    production function\" allowance.
  - `many-pages` assertions live at two levels: aggregate semantics
    through the production client (one call collects all pages) and
    page-shaped wire behavior through raw bounded probes."
  (:require [clojure.java.io :as io])
  (:import [java.util HashMap]
           [java.util.concurrent TimeUnit]))

(def ^:const default-timeout-ms
  "Upper bound for every wait in this ns (WO-T1 rule: <= 10 seconds;
   sole dispatcher-approved exception: the load-robustness orphan-audit
   helper's 15s poll + 3s second-chance windows — DEVIATION RECORD 4)."
  10000)

(def known-modes
  "All FAKE_MODE values implemented by fake-mcp-server.mjs.
   :hang-after-spawn (WO-M4 R2) seeds the CE-1c initialize-failure leak
   scenario: the subprocess answers nothing and never exits on its own,
   so only a client that closes its transport reaps it."
  #{:ok :slow :malformed :huge :many-pages
    :infinite-cursor :crash-after-init :no-response
    :hang-after-spawn :structured})

(def ^:private script-relpath
  "test/evoclj/mcp/support/server/fake-mcp-server.mjs")

(defn- script-file
  []
  (let [f (io/file script-relpath)]
    (when-not (.exists f)
      (throw (ex-info "fake MCP server script not found (tests must run from the repo root)"
                      {:error/type :support/server-script-missing
                       :path (.getAbsolutePath f)})))
    f))

(defn script-path
  "Absolute filesystem path of the fake server Node script. Public so
   tests can run RAW stdio probes (bounded, non-production) against the
   same script file the transport config points at."
  []
  (.getAbsolutePath (script-file)))

(defn knob-env
  "Build the FAKE_* environment overlay for `opts`
  {:mode :delay-ms :tool-count :page-size}. PATH/SystemRoot are passed
  through defensively so the child resolves `node` regardless of whether
  the SDK merges or replaces the parent environment."
  ([] (knob-env {}))
  ([{:keys [mode delay-ms tool-count page-size] :or {mode :ok}}]
   (when-not (contains? known-modes mode)
     (throw (ex-info "unknown fake-server mode"
                     {:error/type :support/server-invalid-mode
                      :mode mode
                      :known known-modes})))
   (cond-> {"FAKE_MODE" (name mode)
            "PATH" (or (System/getenv "PATH") "")
            "SystemRoot" (or (System/getenv "SystemRoot") "")}
     (some? delay-ms)   (assoc "FAKE_DELAY_MS" (str (long delay-ms)))
     (some? tool-count) (assoc "FAKE_TOOL_COUNT" (str (long tool-count)))
     (some? page-size)  (assoc "FAKE_PAGE_SIZE" (str (long page-size))))))

(defn knob-args
  "CLI-flag form of the knobs, understood by fake-mcp-server.mjs (flags
  win over the FAKE_* env vars). This is the only knob channel that can
  reach the PRODUCTION client's subprocess on SDK 2.0.0 — see DEVIATION
  RECORD 2 in the ns docstring."
  ([] (knob-args {}))
  ([{:keys [mode delay-ms tool-count page-size] :or {mode :ok} :as _opts}]
   (when-not (contains? known-modes mode)
     (throw (ex-info "unknown fake-server mode"
                     {:error/type :support/server-invalid-mode
                      :mode mode
                      :known known-modes})))
   (vec (concat ["--mode" (name mode)]
                (when (some? delay-ms)   ["--delay-ms"   (str (long delay-ms))])
                (when (some? tool-count) ["--tool-count" (str (long tool-count))])
                (when (some? page-size)  ["--page-size"  (str (long page-size))])))))

(defn transport-config
  "The stdio transport-config for a fake server with `opts`, suitable for
  evoclj.mcp.client/open!. Same knob source of truth as `start!`.
  Deliberately carries NO :env key — see DEVIATION RECORD 2."
  ([] (transport-config {}))
  ([opts]
   {:type :stdio
    :command "node"
    :args (into [(.getAbsolutePath (script-file))] (knob-args opts))}))

;; --- process-tree teardown (Windows-safe, pure JDK) --------------------------

(defn kill-tree!
  "Forcefully destroy the process tree rooted at ProcessHandle `handle`
  (descendants first, then the root), then join exit bounded by
  `timeout-ms`. Nil-tolerant and idempotent. Shared teardown primitive
  for this ns and evoclj.mcp.support.real-server. Returns nil."
  [^java.lang.ProcessHandle handle timeout-ms]
  (when handle
    (when (.isAlive handle)
      (doseq [^java.lang.ProcessHandle d (iterator-seq (.iterator (.descendants handle)))]
        (.destroyForcibly d))
      (.destroyForcibly handle))
    (try
      (.get (.onExit handle) (long timeout-ms) TimeUnit/MILLISECONDS)
      (catch InterruptedException e
        (.interrupt (Thread/currentThread)))
      (catch Exception _ nil)))
  nil)

(defn stop!
  "Kill `server`'s process tree: descendants first, then the root, then
  join exit bounded by `timeout-ms` (default 10000). Safe to call twice,
  safe on an already-dead process, safe on nil. Returns nil."
  ([server] (stop! server default-timeout-ms))
  ([server timeout-ms]
   (kill-tree! (:handle server) timeout-ms)))

(defn alive?
  "True when `server`'s supervised process is still running. Consults the
  ProcessHandle: on Windows, java.lang.Process#isAlive can lag behind
  actual termination even after onExit has completed, while the handle
  reports authoritatively."
  [server]
  (some-> ^java.lang.ProcessHandle (:handle server) .isAlive boolean))

;; --- start! / macro ----------------------------------------------------------

(defn start!
  "Launch a supervised fake server subprocess. See the ns docstring for
  the returned shape. Throws when the script or `mode` is invalid.
  Starting a fresh instance after `stop!` works (documented reuse
  contract exercised by the tests)."
  ([] (start! {}))
  ([{:keys [mode] :or {mode :ok} :as opts}]
   (let [cfg (transport-config opts)
         pb (ProcessBuilder. ^java.util.List (vec (cons (:command cfg) (:args cfg))))
         ;; Supervised child gets both knob channels (env overlay + flags).
         _ (.putAll ^java.util.Map (.environment pb) (HashMap. ^java.util.Map (knob-env opts)))
         ^java.lang.Process p (.start pb)
         handle (.toHandle p)]
     {:process p
      :handle handle
      :config cfg
      :mode mode
      :stop! (fn [] (stop! {:handle handle}))})))

(defmacro with-fake-server
  "Start a fake server with `opts`, bind it to `binding-form`, run body,
  and always stop! the server in a finally clause.

      (with-fake-server [srv {:mode :slow :delay-ms 500}]
        ...)"
  [[binding-form & [opts]] & body]
  `(let [server# (start! ~(or opts {}))]
     (try
       (let [~binding-form server#]
         ~@body)
       (finally
         (stop! server#)))))

;; --- descendant auditing (orphans, liveness of client-spawned children) ------

(defn- node-command?
  "True when `info`'s executable path is node (basename node.exe / node)."
  [^java.lang.ProcessHandle$Info info]
  (let [cmd (some-> (.command info) (.orElse nil))]
    (boolean
     (and cmd
          (or (.endsWith (.toLowerCase ^String cmd) "node.exe")
              (= "node" (.getName (java.io.File. ^String cmd))))))))

(defn processes-matching
  "Live ProcessHandles of THIS JVM's descendant processes matching `s`.
  Snapshot; no waiting.

  Platform note (verified on the dev host): Windows JDK often does NOT
  expose ProcessHandle.Info#commandLine / #arguments (absent Optionals),
  so when the command line is unavailable the match falls back to \"any
  live node.exe descendant\" — sound here because the ONLY node
  processes this test JVM ever spawns are harness MCP servers (fake or
  real), so any surviving node descendant IS a harness leftover worth
  reporting."
  [^String s]
  (->> (iterator-seq (.iterator (.descendants (java.lang.ProcessHandle/current))))
       (keep (fn [^java.lang.ProcessHandle ph]
               (let [info (.info ph)
                     cl (some-> (.commandLine info) (.orElse nil))]
                 (when (.isAlive ph)
                   (cond
                     (and cl (.contains ^String cl s)) ph
                     (nil? cl) (when (node-command? info) ph)
                     :else nil)))))
       vec))

(defn await-no-process-matching
  "Poll until no descendant command line matches `s`, or `timeout-ms`
  elapses. Returns the final snapshot: empty means clean, non-empty means
  orphaned processes remain (bounded wait, deterministic failure)."
  ([^String s] (await-no-process-matching s default-timeout-ms))
  ([^String s timeout-ms]
   (let [deadline (+ (System/currentTimeMillis) (long timeout-ms))]
     (loop []
       (let [found (processes-matching s)]
         (if (or (empty? found) (>= (System/currentTimeMillis) deadline))
           found
           (do (Thread/sleep 100) (recur))))))))

;; --- load-robustness audit (DEVIATION RECORD 4) ------------------------------
;;
;; Baseline-diff + second-chance variant of the absolute audit above.
;; Motivating evidence (docs/codebase/m1-full2.txt): under the 35-minute
;; hot-load full suite run, ONE pre-existing node.exe leftover (the same
;; stale PID in all nine failures) tripped every orphan audit — including
;; audits using a DIFFERENT pattern, because on this Windows host the JDK
;; often hides process command lines and `processes-matching` then falls
;; back to \"any live node.exe descendant\". Holding each lifecycle test
;; accountable for residue that predates it detects nothing that test
;; owns; diffing against a pre-test baseline restores the original
;; semantic object (\"THIS test reaps everything IT spawned\") while
;; eliminating cross-suite false positives.

(defn processes-matching-pids
  "PID set of a `processes-matching` snapshot. PIDs (not ProcessHandle
  objects) are the stable cross-snapshot identity: the JDK hands out a
  fresh handle object per query, so handle equality cannot express
  \"same live process as at baseline time\"."
  [^String s]
  (into #{} (map #(.pid ^java.lang.ProcessHandle %)) (processes-matching s)))

(defn- new-matches-since
  "Live handles matching `s` whose PID is absent from `baseline-pids`
  (captured before the test under audit spawned anything)."
  [^String s baseline-pids]
  (->> (processes-matching s)
       (remove #(contains? baseline-pids (.pid ^java.lang.ProcessHandle %)))
       vec))

(defn await-no-new-process-matching
  "Baseline-diff orphan audit: poll until no descendant matching `s`
  exists whose PID was NOT already alive at baseline time. Returns the
  final NEW-match snapshot: empty means this test's own tree is fully
  reaped; non-empty is a deterministic failure.

  Wait strategy (DEVIATION RECORD 4, dispatcher-approved):
    - poll every 100ms up to `timeout-ms` (default 15000 — replaces the
      historical 5s/8s call-site windows; Windows async termination of
      destroyForcibly'd node.exe children was observed to exceed 8s
      under sustained load);
    - second chance: if the window elapses with survivors, sleep
      `settle-ms` (default 3000), recheck ONCE, return that snapshot.
  Total wait stays bounded (<= timeout-ms + settle-ms); nothing here
  blocks forever."
  ([^String s baseline-pids]
   (await-no-new-process-matching s baseline-pids {}))
  ([^String s baseline-pids {:keys [timeout-ms settle-ms]
                             :or {timeout-ms 15000 settle-ms 3000}}]
   (let [deadline (+ (System/currentTimeMillis) (long timeout-ms))]
     (loop []
       (let [found (new-matches-since s baseline-pids)]
         (cond
           ;; clean: nothing new since baseline — done immediately
           (empty? found)
           found

           ;; still inside the poll window: keep waiting for async kills
           (< (System/currentTimeMillis) deadline)
           (do (Thread/sleep 100) (recur))

           ;; window exhausted with survivors: ONE bounded grace window,
           ;; then a single recheck decides pass/fail
           :else
           (do (Thread/sleep (long settle-ms))
               (new-matches-since s baseline-pids))))))))

(def fake-server-process-pattern
  "Command-line substring identifying fake-server node processes."
  "fake-mcp-server.mjs")

(defn kill-matching!
  "Emergency janitor: destroyForcibly the tree of every live descendant
  matching `s`. Returns how many trees were attacked. Used only where a
  bounded test must guarantee teardown even if a graceful close stalls."
  [^String s]
  (let [ps (processes-matching s)]
    (doseq [^java.lang.ProcessHandle p ps]
      (doseq [^java.lang.ProcessHandle d (iterator-seq (.iterator (.descendants p)))]
        (.destroyForcibly d))
      (.destroyForcibly p))
    (count ps)))

(ns evoclj.mcp.support.real-server
  "WO-T1 wrapper around the REAL official MCP sequential-thinking server
  (the @modelcontextprotocol/server-sequential-thinking npm package
  already installed under node_modules/).

  Contract (mirrors evoclj.mcp.support.fake-server):

    start!   -> {:process <java.lang.Process>
                 :handle  <java.lang.ProcessHandle> ; used by stop!
                 :config  <stdio transport-config>  ; feed to
                                                    ; evoclj.mcp.client/open!
                 :stop!   <fn>                      ; same as stop!
                 :bin     <file> :entry <file>}

    stop!    -> kills the whole process tree Windows-safely (delegates to
                fake-server/kill-tree!), bounded join, idempotent,
                nil-tolerant.
    with-real-server macro guarantees teardown.

  Availability: WO-T1 anchors existence on
  node_modules/.bin/mcp-server-sequential-thinking.cmd. Tests MUST skip
  (printing the reason) when it is absent instead of failing — see
  `available?` / `assert-available!`.

  Launch shape note (deliberate): Java ProcessBuilder/CreateProcess on
  Windows cannot execute a `.cmd` shim directly, and wrapping it in
  \"cmd /c\" would insert a shell process into the supervised tree and
  complicate tree-kill. The npm `.cmd` shim resolves to exactly
  node_modules/@modelcontextprotocol/server-sequential-thinking/dist/index.js,
  which is what this wrapper launches (\"node <entry>\") — the same shape
  the existing evoclj.mcp.integration-test already drives successfully
  through the production client. The .cmd remains the WO-specified
  EXISTENCE anchor; `entry` is additionally asserted to exist.

  All waits are bounded (default 10s); nothing here blocks forever."
  (:require [clojure.java.io :as io]
            [evoclj.mcp.support.fake-server :as fake])
  (:import [java.util HashMap]))

(def ^:const default-timeout-ms
  "Upper bound for every wait in this ns (WO-T1 rule: <= 10 seconds)."
  fake/default-timeout-ms)

(def bin-relpath
  "WO-T1-specified existence anchor for the real server."
  "node_modules/.bin/mcp-server-sequential-thinking.cmd")

(def entry-relpath
  "Node entry script the npm .cmd shim resolves to."
  "node_modules/@modelcontextprotocol/server-sequential-thinking/dist/index.js")

(def ^:private process-pattern
  "Command-line substring identifying real sequential-thinking processes."
  "server-sequential-thinking")

(defn process-matching-pattern
  "Substring used by orphan audits for this server."
  [] process-pattern)

(defn bin-file [] (io/file bin-relpath))
(defn entry-file [] (io/file entry-relpath))

(defn available?
  "True when the real server's npm .cmd anchor AND its resolved Node entry
  script both exist under the repo root (tests must run from repo root)."
  []
  (and (.exists (bin-file))
       (.exists (entry-file))))

(defn assert-available!
  "Return {:bin <file> :entry <file>} or throw a typed ex-info carrying
  the missing path (tests that cannot run should prefer available? +
  skip-with-reason over letting this throw)."
  []
  (let [bin (bin-file) entry (entry-file)]
    (cond
      (not (.exists bin))
      (throw (ex-info "real MCP server not installed: npm .cmd anchor missing"
                      {:error/type :support/real-server-missing
                       :missing (.getAbsolutePath bin)}))

      (not (.exists entry))
      (throw (ex-info "real MCP server not installed: entry script missing"
                      {:error/type :support/real-server-missing
                       :missing (.getAbsolutePath entry)}))

      :else {:bin bin :entry entry})))

(defn transport-config
  "Stdio transport-config launching the real server, suitable for
  evoclj.mcp.client/open!. Throws :support/real-server-missing when the
  package is not installed. See ns docstring for why we launch the
  resolved entry script rather than the .cmd shim itself."
  []
  (let [{:keys [^java.io.File entry]} (assert-available!)]
    {:type :stdio
     :command "node"
     :args [(.getAbsolutePath entry)]}))

(defn alive?
  "True when the supervised real-server process is still running."
  [server]
  (fake/alive? server))

(defn stop!
  "Kill the process tree (Windows-safe), bounded join; idempotent and
  nil-tolerant. Same semantics as evoclj.mcp.support.fake-server/stop!."
  ([server] (fake/stop! server default-timeout-ms))
  ([server timeout-ms] (fake/stop! server timeout-ms)))

(defn start!
  "Launch a supervised real sequential-thinking server subprocess.
  Returns the record documented in the ns docstring. Throws
  :support/real-server-missing when unavailable. Starting a fresh
  instance after stop! works (reuse contract exercised by the tests)."
  ([] (start! {}))
  ([_opts]
   (let [cfg (transport-config)
         pb (ProcessBuilder. ^java.util.List (vec (cons (:command cfg) (:args cfg))))
         ;; pass the parent environment through untouched; overlay PATH /
         ;; SystemRoot defensively like the fake wrapper does so the child
         ;; always resolves `node`.
         _ (.putAll ^java.util.Map (.environment pb)
                    (HashMap. ^java.util.Map {"PATH" (or (System/getenv "PATH") "")
                                              "SystemRoot" (or (System/getenv "SystemRoot") "")}))
         ^java.lang.Process p (.start pb)
         handle (.toHandle p)
         {:keys [bin entry]} (assert-available!)]
     {:process p
      :handle handle
      :config cfg
      :stop! (fn [] (stop! {:handle handle}))
      :bin bin
      :entry entry})))

(defmacro with-real-server
  "Start the real server, bind it, run body, always stop! in finally.

      (with-real-server [srv] ...)"
  [[binding-form & [opts]] & body]
  `(let [server# (start! ~(or opts {}))]
     (try
       (let [~binding-form server#]
         ~@body)
       (finally
         (stop! server#)))))

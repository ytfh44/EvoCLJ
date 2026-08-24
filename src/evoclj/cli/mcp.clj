(ns evoclj.cli.mcp
  "MCP diagnostics CLI — protocol/connection diagnostics only.

  Generic lifecycle (refresh) is owned by the generic source commands
  (evoclj.cli.source); this namespace keeps only diagnostics that are
  specific to the MCP transport/protocol layer: pool status and
  per-connection diagnosis.

  Delegates to evoclj.mcp.manager for pool visibility; does not
  perform generic refresh."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.mcp.client :as client]
            [evoclj.mcp.manager :as manager]))

;; --- manager resolution ------------------------------------------------------

(defn- manager-for
  "Resolve MCP manager atom from opts."
  [opts]
  (or (:mcp/manager opts)
      (:manager opts)
      (:mcp-manager opts)
      (get-in opts [:overrides :mcp/manager])
      (get-in opts [:overrides :manager])
      (get-in opts [:system :mcp/manager])
      (try
        (let [system ((resolve 'evoclj.cli.session/build-system) opts)]
          (:mcp/manager system))
        (catch Exception _ nil))
      ;; fallback: empty ephemeral manager (status will be empty but valid)
      (manager/create-manager)))

;; --- commands ----------------------------------------------------------------

(defn status!
  "evoclj mcp status

  Protocol/connection diagnostics: pool states, health, and metrics.
  Returns {:mcp/status :ok :count n :pools [...]}"
  [opts]
  (let [mgr (manager-for opts)
        pools (:pools @mgr)]
    {:mcp/status :ok
     :count (count pools)
     :pools (mapv (fn [[k v]]
                    {:connection/key (str k)
                     :state (:state v)
                     :health (:health v)
                     :metrics (:metrics v)
                     :owners (count (or (:owners v) #{}))
                     :generation (:generation v)})
                  pools)}))

(defn diagnose!
  "evoclj mcp diagnose <connection-id>

  Diagnose one MCP connection by id substring. Returns health for the
  matching pool entry or :not-found."
  [opts]
  (let [id (first (:positionals opts))]
    (when-not id
      (throw (err/error :cli/usage-invalid
                        "mcp diagnose requires <connection-id>"
                        {:usage "evoclj mcp diagnose <connection-id>"})))
    (let [mgr (manager-for opts)
          pools (:pools @mgr)
          entry (some (fn [[k v]]
                        (when (str/includes? (str k) id)
                          {:key k :entry v}))
                      pools)]
      (if entry
        {:mcp/connection id
         :mcp/key (str (:key entry))
         :state (:state (:entry entry))
         :health (:health (:entry entry))
         :metrics (:metrics (:entry entry))
         :transport-identity (:transport-identity (:entry entry))
         :credential-identity (:credential-identity (:entry entry))}
        {:mcp/connection id
         :state :not-found
         :message "no pool entry matches this connection id"
         :count (count pools)}))))

(defn ping!
  "evoclj mcp ping [<connection-id>] [--transport-config <edn>]

  Real outbound liveness probe (M8).

  When `--transport-config <edn>` is supplied, this opens a managed MCP
  client against that transport config (a stdio/tcp/http config launching or
  reaching a real server), drives the production `evoclj.mcp.client/ping!`
  (which sends a genuine JSON-RPC `ping` over the transport), and reports:

      {:mcp/ping :ok
       :mcp/ping-roundtrip-ms <pos-int>
       :mcp/ping-at <ISO-8601 string>
       :transport-config <sanitized>}

  A connection that cannot be opened OR whose ping fails throws a typed
  :mcp/ping-failed (fail-closed) — it does NOT silently report :unreachable.
  The thrown error carries `:mcp/ping-cause-type` (the underlying classified
  error category, e.g. :mcp/timeout / :mcp/transport-error) so the liveness
  failure is itself diagnosable.

  Without `--transport-config`, the legacy pool behavior is preserved: a
  positional <connection-id> is matched against the live manager pool and
  its :ready state reported as :ok / :unreachable (no outbound probe)."
  [opts]
  (let [raw-tc (some-> opts :options :transport-config)
        tc (when raw-tc (edn/read-string raw-tc))]
    (if tc
      ;; --- real outbound path (open! + ping! both inside the try so any
      ;;     failure of the outbound ping operation is reported as
      ;;     :mcp/ping-failed, fail-closed + typed) ---
      (try
        (let [managed (client/open! tc)
              result (try
                       (client/ping! managed)
                       (finally (client/close! managed)))]
          {:mcp/ping (:mcp/ping result)
           :mcp/ping-roundtrip-ms (:mcp/ping-roundtrip-ms result)
           :mcp/ping-at (:mcp/ping-at result)
           :transport-config (err/sanitize tc)})
        (catch Throwable ex
          ;; A failed outbound ping operation is ALWAYS reported as
          ;; :mcp/ping-failed (fail-closed + typed), with the underlying
          ;; classified cause type preserved so the failure is still
          ;; diagnosable (timeout / transport / protocol / etc).
          (let [data (ex-data ex)
                ;; The failure is ALWAYS typed :mcp/ping-failed, and it
                ;; MUST carry a cause-type so the liveness failure is itself
                ;; diagnosable. Prefer the ping!-classified type
                ;; (:mcp/ping-cause-type); when the failure came from open!
                ;; instead of ping! (e.g. an unspawnable server), the
                ;; underlying :error/type of the thrown error is used as the
                ;; cause-type fallback.
                cause-type (or (:mcp/ping-cause-type data)
                               (:error/type data))
                base {:transport-config (err/sanitize tc)
                      :mcp/ping-cause-type cause-type}
                enriched (cond-> base
                           (:mcp/ping-cause-type data)
                           (assoc :mcp/ping-cause-type (:mcp/ping-cause-type data))

                           (:error/type data)
                           (assoc :mcp/ping-failed-from (:error/type data))

                           (:cause data)
                           (assoc :cause (:cause data)))]
            (throw (err/error :mcp/ping-failed
                              (.getMessage ex)
                              enriched)))))
      ;; --- legacy pool-diagnose path (no outbound) ---
      (let [res (diagnose! opts)]
        (assoc res :mcp/ping (if (= :ready (:state res)) :ok :unreachable))))))

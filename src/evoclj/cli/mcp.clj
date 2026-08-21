(ns evoclj.cli.mcp
  "MCP diagnostics CLI — protocol/connection diagnostics only.

  Generic lifecycle (refresh) is owned by the generic source commands
  (evoclj.cli.source); this namespace keeps only diagnostics that are
  specific to the MCP transport/protocol layer: pool status and
  per-connection diagnosis.

  Delegates to evoclj.mcp.manager for pool visibility; does not
  perform generic refresh."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err]
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
  Returns {:mcp/status :ok :count n :pools [...]}."
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
  "evoclj mcp ping <connection-id>

  Alias for diagnose — checks if a connection is reachable (pool entry exists and is :ready)."
  [opts]
  (let [res (diagnose! opts)]
    (assoc res :mcp/ping (if (= :ready (:state res)) :ok :unreachable))))

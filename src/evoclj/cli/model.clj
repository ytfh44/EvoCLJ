(ns evoclj.cli.model
  "Operator model catalog commands (post-v0 extension 1).

  model list — the models.dev catalog summary: total/status counts
  and the supported models (optionally filtered with --style
  :openai-compatible|:anthropic, --status, --provider). The catalog
  is auto-refreshed at every invocation (startup semantics: fetch,
  cache under the state dir, offline fallback).

  model inspect <id> — one model entry: style, status, base URL,
  API-key environment, capabilities, dialect, limits, and cost.

  Both commands are read-only and emit machine-readable EDN by
  default (the CLI output contract)."
  (:require [evoclj.cli.session :as session]
            [evoclj.kernel.error :as err]
            [evoclj.kernel.system :as kernel]
            [evoclj.provider.modelsdev :as modelsdev]))

(defn- catalog-of
  "Refresh the catalog through the CLI host system and return the
  catalog result map {:catalog/status ... :catalog/data ...}."
  [opts]
  (let [system (session/build-system opts)]
    (try
      (:modelsdev/catalog system)
      (finally (kernel/halt! system)))))

(defn model-list!
  "evoclj model list [--style <kw>] [--status <kw>] [--provider <id>]

  The catalog summary: :catalog/status (fresh|cached|unavailable),
  :models/total, per-status counts, and the filtered model ids with
  their style/status."
  [opts]
  (let [catalog (catalog-of opts)
        data (:catalog/data catalog)
        index (:catalog/models data)
        style (some-> (get-in opts [:options :style])
                      (str)
                      (keyword))
        status (or (some-> (get-in opts [:options :status])
                           (str)
                           (keyword))
                   ;; default: the usable models — listing all 6000+
                   ;; catalog entries by default is noise
                   :supported)
        provider (get-in opts [:options :provider])
        models (modelsdev/list-models index
                                      :style style
                                      :status status
                                      :provider provider)
        counts (frequencies (map :model/status (vals index)))]
    {:catalog/status (:catalog/status catalog)
     :models/total (count index)
     :models/by-status (merge {:supported 0 :needs-config 0 :unsupported 0}
                              counts)
     :models (mapv (fn [m]
                     (select-keys m [:model/id :model/style :model/status
                                     :model/provider :model/base-url]))
                   models)}))

(defn model-inspect!
  "evoclj model inspect <id>

  One model entry (full detail). Unknown ids fail with
  :cli/model-not-found."
  [opts]
  (let [model-id (first (:positionals opts))
        _ (when-not model-id
            (throw (err/error :cli/usage-invalid
                              "model inspect requires a model id"
                              {:usage "evoclj model inspect <id>"})))
        catalog (catalog-of opts)
        data (:catalog/data catalog)
        index (:catalog/models data)
        entry (modelsdev/lookup-model index model-id)]
    (when-not entry
      (throw (err/error :cli/model-not-found
                        (str "unknown model " model-id)
                        {:model/id model-id})))
    (assoc entry :model/dialect (:model/dialect entry))))

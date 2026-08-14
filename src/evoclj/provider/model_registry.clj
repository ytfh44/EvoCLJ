(ns evoclj.provider.model-registry
  "Kernel-owned model registry (post-v0 extension 1).

  Builds the live model providers from the models.dev catalog index:
  one adapter instance per (provider, style, base-url) endpoint,
  shared by all models that endpoint serves, keyed by full model id
  (deepseek/deepseek-v4-flash). The registry is a kernel-owned atom
  (Global Constraint 19) — agents never see or mutate it; the broker
  dispatcher reads it for :intent/model-call intents.

  API KEYS: each provider in the catalog declares the environment
  variable it expects (:model/api-key-env, e.g. DEEPSEEK_API_KEY).
  The host config may override keys explicitly
  (:registry/api-keys {<provider-id-keyword> <key>}). A model whose
  key is absent is registered with :provider nil and
  :reason :api-key-missing so dispatch can fail with an informative
  typed error instead of a silent miss. A model whose endpoint style
  is unknown or needs operator config is :reason :not-supported or
  :needs-config.

  Registry entry:

    {:model/id \"deepseek/deepseek-v4-flash\"
     :provider <Provider instance or nil>
     :reason nil | :api-key-missing | :needs-config | :not-supported
     :style :openai-compatible | :anthropic | nil
     :base-url <str or nil>}

  build-model-registry is pure host-side assembly: it performs no
  network I/O (the catalog arrived at startup) and only reads the
  environment for keys."
  (:require [evoclj.kernel.error :as err]
            [evoclj.provider.anthropic :as anthropic]
            [evoclj.provider.openai :as openai]))

(defn- api-key-for
  "The API key for one provider: explicit config override first, then
  the catalog-declared environment variable. Returns nil when the
  key is unavailable."
  [config provider-id api-key-env]
  (or (get-in config [:registry/api-keys provider-id])
      (when api-key-env
        (System/getenv api-key-env))))

(defn build-model-registry
  "Build the model registry atom from the models.dev index.

  index is the :catalog/models map from
  evoclj.provider.modelsdev/refresh-catalog!; config carries:

    :registry/api-keys {<provider-id-keyword> <api-key-string>}
    :registry/timeout-ms (default 60000)

  Grouping: models of the same provider with the same style and
  base-url share one adapter (the adapter is constructed once with
  the union of their entries). Azure-style endpoints get azure?
  true so the client uses the Azure service-version/path-mode
  defaults."
  [index config]
  (let [timeout-ms (or (:registry/timeout-ms config) 60000)
        supported (filter #(= :supported (:model/status %)) (vals index))
        by-endpoint (group-by (fn [m]
                                [(:model/provider m)
                                 (:model/style m)
                                 (:model/base-url m)])
                              supported)
        adapters (into {}
                       (for [[[provider-id style base-url] models] by-endpoint
                             :let [entries (into {} (map (fn [m] [(:model/id m) m])) models)
                                   key (api-key-for config provider-id
                                                    (:model/api-key-env (first models)))
                                   instance (when key
                                              (case style
                                                :openai-compatible
                                                (openai/openai-compatible-provider
                                                 {:provider/id provider-id
                                                  :base-url base-url
                                                  :api-key key
                                                  :model-entries entries
                                                  :timeout-ms timeout-ms
                                                  :azure? (= provider-id :azure)})
                                                :anthropic
                                                (anthropic/anthropic-provider
                                                 {:provider/id provider-id
                                                  :base-url base-url
                                                  :api-key key
                                                  :model-entries entries
                                                  :timeout-ms timeout-ms})
                                                nil))]]
                         [[provider-id style base-url] instance]))]
    (atom
     (into {}
           (for [m (vals index)
                 :let [provider-id (:model/provider m)
                       style (:model/style m)
                       base-url (:model/base-url m)
                       instance (get adapters [provider-id style base-url])
                       reason (cond
                                (not (contains? #{:supported} (:model/status m)))
                                (:model/status m)
                                (nil? instance) :api-key-missing
                                :else nil)]]
             [(:model/id m)
              {:model/id (:model/id m)
               :provider instance
               :reason reason
               :style style
               :base-url base-url}])))))

(defn lookup
  "The registry entry for one full model id, or nil."
  [registry model-id]
  (get @registry model-id))

(defn configured-models
  "Full model ids with a live provider (for diagnostics)."
  [registry]
  (->> @registry
       (filter (fn [[_ e]] (some? (:provider e))))
       (map key)
       (sort)
       vec))

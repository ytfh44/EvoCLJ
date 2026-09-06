(ns evoclj.cli.deploy
  "The deployment-facing CLI command (component): `deploy <generation-id>`
  and the read-only host poll `deploy current`.

  `deploy <generation-id>` sets the specified generation as the deployment
  target. The command validates that the generation exists in the store,
  records an auditable deploy decision, and returns the deployment target
  map together with a canary-ready `:deployment-state` the host can use
  for session routing.

  `deploy current` is the CLI half of the host polling protocol
  (docs/host-polling-protocol.md, GET /api/deployment/current): a
  read-only poll returning the CURRENT generation as {:generation/id
  :genome/id :canary nil :timestamp}. It records no decision and writes
  nothing — :cli/no-current-generation when no CURRENT row exists.

  Neither command moves the CURRENT pointer (Global Constraint 15);
  `deploy <generation-id>` never writes to the generations table — it
  appends an immutable decision row and returns the deployment envelope."
  (:require [evoclj.cli.session :as session]
            [evoclj.kernel.error :as err]
            [evoclj.store.deployment :as deployment]
            [evoclj.promotion.canary :as canary]
            [evoclj.config :as config]))

;; --- shared helpers ----------------------------------------------------------

(defn- positional
  [opts n]
  (let [pos (:positionals opts)]
    (or (nth pos n nil)
        (throw (err/error :cli/usage-invalid
                          "missing positional argument"
                          {:usage (str "expected " (inc n) " positional argument(s)")})))))

;; --- commands ----------------------------------------------------------------

(defn- deployment-state
  "Build a canary-ready deployment-state map for the deployed generation.
  The current generation becomes the fallback; if the deployed generation
  differs from current and canary is configured, it is exposed as the
  canary target. Returns nil when there is no CURRENT generation yet."
  [system deployed-generation-id]
  (let [current (session/current-generation-info system)
        current-id (when current (:generation/id current))]
    (when current-id
      {:current-generation current-id})))

(defn current!
  "evoclj deploy current — read-only CURRENT generation poll (the CLI half
  of the host polling protocol; the HTTP half is GET /api/deployment/current).

  Returns {:generation/id :genome/id :canary nil :timestamp} with the same
  shape as the HTTP poll. :canary is nil — rollout allocations live in the
  deployment-state envelopes returned by deploy!, not in the generations
  table. Records no decision and writes nothing. Throws
  :cli/no-current-generation when no generation is current."
  [opts]
  (let [system (session/build-system opts)
        info (session/current-generation-info system)]
    (when-not (:generation/id info)
      (throw (err/error :cli/no-current-generation
                        "no CURRENT generation; host falls back to the seed generation"
                        {})))
    {:generation/id (:generation/id info)
     :genome/id (:genome/id info)
     :canary nil
     :timestamp (str (java.time.Instant/now))}))

(defn deploy!
  "evoclj deploy <generation-id> | evoclj deploy current

  With a generation id, validate that it exists in the store, record a deploy
  decision, and return the deployment envelope. The returned map is:

      {:generation/id <str>
       :genome/id <content-address-str>
       :status :deployed
       :deployment-state {...}  ; canary-ready routing envelope}

  With `current`, delegate to current! (read-only poll, no decision
  recorded). Throws :cli/generation-not-found when the generation id is
  unknown, :cli/no-current-generation when polling with no CURRENT row."
  [opts]
  (let [generation (positional opts 0)]
    (if (= "current" generation)
      (current! opts)
      (let [system (session/build-system opts)
            store (session/store-of system)
            row (session/generation-row system generation)]
        (when-not row
          (throw (err/error :cli/generation-not-found
                            "no generation with this id in the store"
                            {:generation/id generation})))
        (deployment/record-decision! store generation :deployed nil)
        (assoc {:generation/id (:id row)
                :genome/id (:genome_id row)
                :status :deployed}
               :deployment-state (deployment-state system (:id row)))))))

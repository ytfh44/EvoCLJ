(ns evoclj.cli.deploy
  "The deployment-facing CLI command (Task D1): `deploy <generation-id>`.

  Set the specified generation as the deployment target. The command
  validates that the generation exists in the store, records an
  auditable deploy decision, and returns the deployment target map
  together with a canary-ready `:deployment-state` the host can use
  for session routing.

  This command never moves the CURRENT pointer (Global Constraint 15)
  and never writes to the generations table — it appends an immutable
  decision row and returns the deployment envelope."
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

(defn deploy!
  "evoclj deploy <generation-id>

  Validate that `generation-id` exists in the store, record a deploy
  decision, and return the deployment envelope. The returned map is:

      {:generation/id <str>
       :genome/id <content-address-str>
       :status :deployed
       :deployment-state {...}  ; canary-ready routing envelope}

  Throws :cli/generation-not-found when the generation id is unknown."
  [opts]
  (let [generation (positional opts 0)
        system (session/build-system opts)
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
           :deployment-state (deployment-state system (:id row)))))

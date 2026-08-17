(ns evoclj.cli.deploy
  "The deployment-facing CLI command (Task D1): `deploy <generation-id>`.

  Set the specified generation as the deployment target. The command
  validates that the generation exists in the store and returns its
  genome id as the deployment target (the host's deploy pipeline can
  then route new sessions to this generation through the canary
  machinery).

  This is the CLI's thinnest mutation: it never moves the CURRENT
  pointer (Global Constraint 15) and never writes to the generations
  table — it reads the generation row and returns the immutable
  deployment target map."
  (:require [evoclj.cli.session :as session]
            [evoclj.kernel.error :as err]))

;; --- shared helpers ----------------------------------------------------------

(defn- positional
  [opts n]
  (let [pos (:positionals opts)]
    (or (nth pos n nil)
        (throw (err/error :cli/usage-invalid
                          "missing positional argument"
                          {:usage (str "expected " (inc n) " positional argument(s)")})))))

;; --- commands ----------------------------------------------------------------

(defn deploy!
  "evoclj deploy <generation-id>

  Validate that `generation-id` exists in the store and return its
  genome id as the deployment target. The returned map is:

      {:generation/id <str>
       :genome/id <content-address-str>
       :status :deployed}

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
    {:generation/id (:id row)
     :genome/id (:genome_id row)
     :status :deployed}))

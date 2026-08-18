(ns evoclj.store.deployment
  "Deployment decision persistence (S1-3).

  A deploy decision records that an operator explicitly marked a
  generation as deployed. This namespace does NOT move the CURRENT
  pointer (Global Constraint 15) and does NOT alter generations state;
  it only appends an auditable decision row."
  (:require [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite]
            [clojure.java.jdbc :as jdbc]
            [malli.core :as m])
  (:import [java.util UUID]))

(def DecisionSchema
  "The deploy/rollback decision contract."
  [:map {:closed true}
   [:id string?]
   [:generation/id string?]
   [:decision #{"deployed" "rolled-back"}]
   [:reason {:optional true} string?]
   [:created-at string?]])

(defn- validate-decision
  [decision]
  (when-not (m/validate DecisionSchema decision)
    (throw (err/error :deployment/invalid
                      "decision does not satisfy the contract"
                      {:decision (err/sanitize decision)})))
  decision)

(defn record-decision!
  "Persist a deploy decision. Returns the recorded decision map.

  `store` is the runtime store map {:sqlite <db> :cas <cas>}.
  `generation-id` is the target generation id.
  `decision` is `:deployed` or `:rolled-back`.
  `reason` is an optional keyword or string."
  [store generation-id decision reason]
  (let [db (or (get-in store [:sqlite])
               (throw (err/error :deployment/store-invalid
                                 "store must contain :sqlite"
                                 {:store (err/sanitize store)})))
        decision-map {:id (str (UUID/randomUUID))
                      :generation_id generation-id
                      :decision (name decision)
                      :reason (when reason (str (name reason)))
                      :created_at (.toString (java.time.Instant/now))}]
    (jdbc/insert! db :deployment_decisions decision-map)
    decision-map))

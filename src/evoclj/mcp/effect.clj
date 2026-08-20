(ns evoclj.mcp.effect)
(def valid-states #{:effect/proposed :effect/authorized :effect/call-started :effect/committed :effect/rejected :effect/ambiguous})
(def transitions {:effect/proposed #{:effect/authorized} :effect/authorized #{:effect/call-started} :effect/call-started #{:effect/committed :effect/rejected :effect/ambiguous}})
(defn journal [idempotency-key generation] {:effect/state :effect/proposed :effect/idempotency-key idempotency-key :effect/generation generation :effect/history [[:effect/proposed (System/currentTimeMillis)]]})
(defn advance [j to & {:keys [remote-idempotent? sent? committed?]}]
  (let [cur (:effect/state j)]
    (cond (= cur :effect/call-started)
          (if (and sent? committed? (not remote-idempotent?))
            (assoc j :effect/state :effect/ambiguous :effect/history (conj (:effect/history j) [:effect/ambiguous (System/currentTimeMillis)]))
            (if (contains? (get transitions cur) to) (assoc j :effect/state to :effect/history (conj (:effect/history j) [to (System/currentTimeMillis)])) j))
          (contains? (get transitions cur) to) (assoc j :effect/state to :effect/history (conj (:effect/history j) [to (System/currentTimeMillis)]))
          :else j)))
(defn ambiguous? [j] (= :effect/ambiguous (:effect/state j)))

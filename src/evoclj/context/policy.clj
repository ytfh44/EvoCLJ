(ns evoclj.context.policy
  "HostPolicy for EffectiveContext materialization.

  A HostPolicy controls which bindings are allowed to be materialized.
  In v0 it is a simple allow/deny filter over logical-ids.

  Policy shape (all keys optional):
    {:policy/allowed #{[:skill \"debugging\"] ...} ; allowlist, if present only these pass
     :policy/denied  #{[:skill \"other\"] ...}    ; denylist, these are rejected
     :policy/max-segments <int>}                  ; max segments to materialize (default unbounded)

  If both allowed and denied are present, denied takes precedence.
  A nil policy means allow all."
  (:require [evoclj.kernel.error :as err]))

(defn policy?
  [x]
  (or (nil? x)
      (and (map? x)
           (or (nil? (:policy/allowed x)) (set? (:policy/allowed x)))
           (or (nil? (:policy/denied x)) (set? (:policy/denied x)))
           (or (nil? (:policy/max-segments x)) (int? (:policy/max-segments x))))))

(defn allowed?
  "True when binding is allowed by policy."
  [policy binding]
  (let [logical-id (:logical/id binding)
        allowed (:policy/allowed policy)
        denied (:policy/denied policy)]
    (cond
      (and denied (contains? denied logical-id)) false
      (and allowed (not (contains? allowed logical-id))) false
      :else true)))

(defn filter-bindings
  "Filter bindings collection by policy and max-segments.
  Returns filtered seq."
  [policy bindings]
  (let [filtered (if policy
                   (filter #(allowed? policy %) bindings)
                   bindings)
        max-seg (:policy/max-segments policy)]
    (if (and max-seg (int? max-seg) (pos? max-seg))
      (take max-seg filtered)
      filtered)))

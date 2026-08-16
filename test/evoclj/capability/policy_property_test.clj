(ns evoclj.capability.policy-property-test
  "Property-based invariants of the pure authorization decision
  (Task E-prop).

  A test.check layer over evoclj.capability.policy/decide (Task 4.4).
  decide is a pure, deterministic function of plain data: leases are
  considered in a deterministic total order (sorted by :cap/id), the
  first lease that passes every check allows, and when nothing allows
  the decision is a deny with one of the six documented reason codes.
  The invariants under test:

    1. determinism — identical inputs always yield the same decision,
       and reordering the lease collection (shuffle, reverse, set)
       never changes it;
    2. first-allow-wins — when several leases allow the same request,
       the decision's :lease-id is the FIRST allowing lease in sorted
       order, i.e. the minimum :cap/id among the allowing leases — a
       later allow can never win;
    3. deny-when-nothing-allows — when no lease allows, the decision
       is {:decision :deny :reason <code>} with a documented code,
       :capability/missing exactly when the collection is empty;
    4. monotone-in-lease-set — removing leases can never turn a deny
       into an allow (the documented monotonicity of the allow
       relation).

  Every generated scenario is schema-valid: the lease generator
  mirrors evoclj.capability.schema/CapabilityLeaseSchema (closed map,
  positive window, EDN-safe values), and the request is sometimes
  coupled to a random lease so allow cases are exercised often — so
  decide never throws :capability/schema-invalid inside a property."
  (:require [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [evoclj.capability.policy :as policy])
  (:import (java.util Date)))

;; --- fixed pools -----------------------------------------------------------

(def ^:private now
  "The fixed decision instant; lease windows are generated around it."
  (Date. 1700000000000))

(def ^:private phenotype-pool
  ["sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
   "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
   "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
   "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"])

(def ^:private subject-pool
  (mapv (fn [id] {:phenotype/id id}) phenotype-pool))

(def ^:private resource-pool
  [{:kind :tool :id :fixture/echo}
   {:kind :tool :id :fixture/noop}
   {:kind :memory :id :mem/notes}
   {:kind :filesystem :path "/work/a"}])

(def ^:private action-pool
  [:invoke :read])

(def ^:private actions-pool
  [#{:invoke} #{:read} #{:invoke :read}])

(def ^:private constraints-pool
  [{} {:max-calls 1} {:max-calls 3} {:max-calls 5}])

;; Window offset pairs from `now`, each keeping issued strictly before
;; expires (the schema's positive-window rule): valid at now / already
;; expired / not yet valid.
(def ^:private window-pool
  [[-3600000 3600000]
   [-3600000 -1800000]
   [1800000 7200000]])

(defn- date-at
  "The Date ms milliseconds after the fixed `now` instant."
  [ms]
  (Date. (+ 1700000000000 ms)))

;; --- generators ------------------------------------------------------------

(def ^:private lease-gen
  (gen/fmap
   (fn [[cap-id subject resource actions constraints [i-ms e-ms]]]
     {:cap/id cap-id
      :subject subject
      :resource resource
      :actions actions
      :constraints constraints
      :issued-at (date-at i-ms)
      :expires-at (date-at e-ms)})
   (gen/tuple gen/uuid
              (gen/elements subject-pool)
              (gen/elements resource-pool)
              (gen/elements actions-pool)
              (gen/elements constraints-pool)
              (gen/elements window-pool))))

(defn- request-gen
  "Generator of [subject resource action now]. With probability 1/2 the
  request is coupled to a random generated lease (its subject,
  resource, one of its actions, and an instant inside its window), so
  allow cases are exercised often; otherwise it is fully independent."
  [leases]
  (if (seq leases)
    (gen/one-of
     [(gen/tuple (gen/elements subject-pool)
                 (gen/elements resource-pool)
                 (gen/elements action-pool)
                 (gen/return now))
      (gen/fmap
       (fn [l]
         (let [issued (:issued-at l)
               expires (:expires-at l)
               mid (Date. (quot (+ (.getTime issued) (.getTime expires)) 2))]
           [(:subject l) (:resource l)
            (first (shuffle (vec (:actions l))))
            mid]))
       (gen/elements leases))])
    (gen/tuple (gen/elements subject-pool)
               (gen/elements resource-pool)
               (gen/elements action-pool)
               (gen/return now))))

(defn- usage-gen
  "Generator of a usage map for `leases`: each lease's :cap/id is
  either absent from usage or carries 0-6 calls already consumed."
  [leases]
  (gen/fmap
   (fn [counts]
     (into {}
           (keep-indexed (fn [i l]
                           (when-let [c (nth counts i)]
                             [(:cap/id l) c]))
                         leases)))
   (gen/vector (gen/one-of [(gen/return nil) (gen/choose 0 6)])
               (count leases))))

(defn- dedupe-by-id
  "Drop leases that repeat a :cap/id already seen (uuid collisions are
  astronomically unlikely, but decide assumes a well-formed lease set)."
  [leases]
  (reduce (fn [acc l]
            (if (some #(= (:cap/id %) (:cap/id l)) acc)
              acc
              (conj acc l)))
          []
          leases))

(defn- scenario-gen
  "Generator of [leases usage subject resource action now] scenarios."
  []
  (gen/let [leases (gen/fmap dedupe-by-id
                             (gen/vector lease-gen 0 5))
            usage (usage-gen leases)
            [subject resource action now'] (request-gen leases)]
    (gen/tuple (gen/return leases) (gen/return usage)
               (gen/return subject) (gen/return resource)
               (gen/return action) (gen/return now'))))

;; --- properties ------------------------------------------------------------

(defspec decide-deterministic-and-order-independent 200
  (prop/for-all [[leases usage subject resource action now] (scenario-gen)]
    (let [d (policy/decide leases subject resource action now usage)]
      (and (= d (policy/decide leases subject resource action now usage))
           (= d (policy/decide (shuffle leases) subject resource action now usage))
           (= d (policy/decide (reverse leases) subject resource action now usage))
           (= d (policy/decide (set leases) subject resource action now usage))))))

(def ^:private deny-reasons
  "The documented non-missing deny codes (decide's contract)."
  #{:capability/subject-mismatch :capability/expired :capability/action-denied
    :capability/scope-denied :capability/budget-exceeded})

(defspec first-allow-wins-and-deny-when-nothing-allows 200
  (prop/for-all [[leases usage subject resource action now] (scenario-gen)]
    (let [decision (policy/decide leases subject resource action now usage)
          allowing (filterv (fn [l]
                              (= :allow
                                 (:decision
                                  (policy/decide [l] subject resource action now usage))))
                            leases)
          allowing-ids (mapv :cap/id allowing)]
      (cond
        (seq allowing-ids)
        ;; several leases may allow: the winner is the FIRST in sorted
        ;; order — the minimum :cap/id (by compare, as decide sorts) —
        ;; never a later one
        (and (= :allow (:decision decision))
             (= (first (sort allowing-ids)) (:lease-id decision)))

        (empty? leases)
        (and (= :deny (:decision decision))
             (= :capability/missing (:reason decision)))

        :else
        (and (= :deny (:decision decision))
             (contains? deny-reasons (:reason decision)))))))

(defn- subsets
  "Every subset of coll, as vectors (the powerset)."
  [coll]
  (reduce (fn [acc x] (into acc (map #(conj % x)) acc))
          [[]]
          (vec coll)))

(defspec deny-monotone-in-lease-set 100
  (prop/for-all [[leases usage subject resource action now] (scenario-gen)]
    (let [d (policy/decide leases subject resource action now usage)]
      (or (= :allow (:decision d))
          ;; a deny stays a deny under every subset of the leases
          (every? (fn [sub]
                    (= :deny (:decision
                              (policy/decide sub subject resource action now usage))))
                  (subsets leases))))))

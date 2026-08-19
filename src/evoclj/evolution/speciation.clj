(ns evoclj.evolution.speciation
  "Diversity protection via simple threshold-based speciation (S3-1).

  Candidates are grouped into species by a distance proxy derived from
  their mutation IR. For v0 the distance is the fraction of differing
  ops between two candidates' mutation IRs, falling back to genome-id
  prefix similarity when ops are not available."

  (:require [clojure.string :as str]
            [evoclj.genome.hash :as hash]))

(defn- op-distance
  "Return a normalized distance [0,1] between two candidates based on
  their `:ops` vectors."
  [a b]
  (let [ops-a (set (map pr-str (:ops a)))
        ops-b (set (map pr-str (:ops b)))]
    (if (and (seq ops-a) (seq ops-b))
      (let [union (into #{} (concat ops-a ops-b))
            inter (into #{} (filter ops-a ops-b))]
        (if (seq union)
          (- 1.0 (/ (count inter) (count union)))
          1.0))
      1.0)))

(defn- genome-prefix-distance
  "Return a normalized distance [0,1] between two candidates based on
  their `:candidate/genome-id` prefix similarity."
  [a b]
  (let [ga (:candidate/genome-id a)
        gb (:candidate/genome-id b)]
    (if (and (string? ga) (string? gb))
      (let [na (str/replace ga ":" "-")
            nb (str/replace gb ":" "-")
            pref-a (subs na 0 (min 8 (count na)))
            pref-b (subs nb 0 (min 8 (count nb)))]
        (if (= pref-a pref-b)
          0.0
          1.0))
      1.0)))

(defn candidate-distance
  "Return a distance [0,1] between two candidate records."
  [a b]
  (if (and (seq (:ops a)) (seq (:ops b)))
    (op-distance a b)
    (genome-prefix-distance a b)))

(defn speciate
  "Group `candidates` into species. Returns a map of `species-id ->
  [candidate-record ...]`."
  ([candidates]
   (speciate candidates {}))
  ([candidates opts]
   (let [threshold (or (:compatibility-threshold opts) 0.5)]
     (loop [remaining (vec candidates)
            species {}
            counter 0]
       (if (empty? remaining)
         species
         (let [c (first remaining)
               match (some (fn [[sid members]]
                             (when (<= (candidate-distance c (first members)) threshold)
                               sid))
                           species)]
           (if match
             (recur (subvec remaining 1)
                    (update species match (fnil conj []) c)
                    counter)
             (recur (subvec remaining 1)
                    (assoc species (str "species-" counter) [c])
                    (inc counter)))))))))

(defn species-count
  "The number of distinct species in `species-map`."
  [species-map]
  (count species-map))

(defn- boosted-weight
  "Return the selection weight for a candidate from a species of size
  `species-size`. Species smaller than `:min-species-size` (default 2)
  receive a boost so they are not completely lost; all others keep
  weight 1."
  [species-size {:keys [min-species-size]}]
  (if (< species-size (or min-species-size 2))
    (* 2.0 species-size)
    1.0))

(defn protect-small-species
  "Given a `species-map` (species-id -> [candidate ...]) and the
  current `population` candidates, return a sequence of candidates with
  boosted selection probability for small species.

  Each candidate appears `weight` times in the returned sequence,
  where `weight` is derived from its species size (small species are
  boosted). The sequence can be fed directly into `rand-nth` or
  `sample` for parent selection.

  `opts` supports:
    :min-species-size — species below this size are boosted (default 2)."
  ([species-map candidates]
   (protect-small-species species-map candidates {}))
  ([species-map candidates opts]
   (let [weights (into {}
                       (mapcat (fn [[sid members]]
                                 (let [w (boosted-weight (count members) opts)]
                                   (map (fn [c] [(:candidate/id c) w])
                                        members)))
                       species-map))]
     (mapcat (fn [c]
               (let [w (get weights (:candidate/id c) 1)]
                 (repeat (int (max w 1)) c)))
             candidates))))

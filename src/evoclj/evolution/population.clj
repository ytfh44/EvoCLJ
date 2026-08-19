(ns evoclj.evolution.population
  "In-memory multi-candidate population management (S3-1).

  A Population is a plain map:

      {:id           <uuid>
       :generation/id <str>
       :candidates   [<candidate-record> ...]
       :evaluations  {<candidate-id> <eval-summary> ...}}

  The namespace is pure-functional: every fn returns a new population
  map. The caller holds the population value and threads it through the
  cycle. No store, no IO, no randomness by default."

  (:require [clojure.set :as set]
            [evoclj.genome.types :as types]))

;; --- construction -------------------------------------------------------------

(def PopulationSchema
  "The closed Population map contract."
  [:map {:closed true}
   [:id uuid?]
   [:generation/id string?]
   [:candidates vector?]
   [:evaluations {:optional true} map?]])

;; --- construction -------------------------------------------------------------

(defn create-population
  "Build a new empty Population for `generation-id`.

      (create-population \"generation-1\")
      ;; => {:id <uuid> :generation/id \"generation-1\"
      ;;     :candidates [] :evaluations {}}"
  [generation-id]
  {:id (java.util.UUID/randomUUID)
   :generation/id generation-id
   :candidates []
   :evaluations {}})

;; --- mutation ----------------------------------------------------------------

(defn add-candidate!
  "Add a `candidate-record` to `population`, returning the updated
  population map. When `population` is an atom, swap! it and return
  the atom (side-effecting). The candidate's `:candidate/id` is used
  as the key in the `:evaluations` map when an evaluation summary is
  supplied via the optional `eval-summary` argument.

      (add-candidate! pop candidate)
      (add-candidate! pop candidate eval-summary)"
  ([population candidate]
   (add-candidate! population candidate nil))
  ([population candidate eval-summary]
   (let [update (fn [pop]
                  (let [cid (:candidate/id candidate)]
                    (-> pop
                        (update :candidates conj candidate)
                        (cond-> eval-summary
                          (assoc-in [:evaluations cid] eval-summary)))))]
     (if (instance? clojure.lang.IAtom population)
       (do (swap! population update) population)
       (update population)))))

;; --- queries ------------------------------------------------------------------

(defn size
  "The number of candidates currently in the population."
  [population]
  (count (:candidates population)))

(defn- candidate-fitness
  "A deterministic fitness proxy for tournament selection: the lower
  this value, the 'better' the candidate. We combine the candidate id
  and the presence of an evaluation summary into a single sortable
  key — candidates WITH an evaluation are ranked above those without,
  breaking ties by the id's numeric value."
  [candidate population]
  (if-let [eval (get-in population [:evaluations (:candidate/id candidate)])]
    (let [utility (get-in eval [:summary :utility :task/success :candidate] 0.0)]
      (- utility))
    0.0))

(defn- tournament-select
  "Run one tournament of size `tournament-size` among `candidates` and
  return the winner (the candidate with the best deterministic fitness).
  Pure, deterministic, no randomness — the first `tournament-size`
  candidates are used as the tournament pool."
  [candidates population tournament-size]
  (let [pool (vec candidates)
        k (min tournament-size (count pool))
        tournament-pool (subvec pool 0 k)]
    (reduce (fn [best contender]
              (if (< (candidate-fitness contender population)
                     (candidate-fitness best population))
                contender
                best))
            (first tournament-pool)
            (rest tournament-pool))))

(defn candidates-for-breeding
  "Return the candidates selected for the next generation's parent pool.

  When `:tournament-size` is supplied in `opts` (default 3), run that
  many tournament selections and return the winners as a vector. When
  `:count` is supplied, run exactly that many tournaments; default is
  the current population size.

      (candidates-for-breeding population)
      (candidates-for-breeding population {:tournament-size 5
                                            :count 2})"
  ([population]
   (candidates-for-breeding population {}))
  ([population opts]
   (let [cands (:candidates population)
         tournament-size (or (:tournament-size opts) 3)
         n (or (:count opts) (count cands))]
     (if (empty? cands)
       []
       (vec (take n
                  (repeatedly n
                              #(tournament-select cands population tournament-size))))))))

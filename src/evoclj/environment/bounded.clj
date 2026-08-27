(ns evoclj.environment.bounded
  "Bounded retainment helper shared by the environment registry and the bundle
  publication path (E5).

  Both evoclj.environment.registry and evoclj.environment.bundle grow a
  history of revision/bundle values (per-source :history, aggregate :history
  and :bundle-history). To keep that history BOUNDED (no unbounded growth) we
  need one shared keep-recent implementation — having a copy in each caller
  would be a single-implementation violation (INV-05). This namespace exists
  to host that one implementation without creating a registry <-> bundle
  require cycle.")

(defn keep-recent
  "Return a vector holding at most the most recent `n` entries of `coll`.

  The newest entries are retained and the oldest are evicted first. `coll`
  may be nil (=> []). When `n` is nil or not positive, the whole collection
  is retained (no bound). Deterministic: for a fixed `coll` and `n` the result
  is identical regardless of call order or thread."
  [coll n]
  (let [v (vec (or coll []))]
    (if (and (some? n) (pos? n) (> (count v) n))
      (subvec v (- (count v) n))
      v)))

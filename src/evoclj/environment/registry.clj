(ns evoclj.environment.registry
  "EnvironmentRegistry - minimal in-memory registry for LiveSource.

  Pipeline: invalidate -> mark dirty -> single-flight snapshot ->
  validate -> derive candidate -> atomic swap. Generation (seq) only
  increments on successful publish. Failure keeps last-good, marks
  degraded and dirty. Identical content returns noop without new seq.
  Listeners receive publication diff, not raw file events.

  E1 (per-source registry): state is held PER SOURCE. Each registered
  source owns its own {:current :last-good :seq :history :status
  :last-refresh-error} entry under :per-source, keyed by source id.
  A parameterless refresh! (no source-id) re-syncs EVERY registered
  source; an explicit refresh! with a source-id updates only that source.
  The top-level :current/:last-good/:seq/:history/:status remain a derived
  aggregate (latest published revision across all sources) so the
  single-source contract and bundle.clj publication keep working. The
  refresh! return value preserves the historical top-level :revision and
  :error-data keys (single-source) for existing callers; per-source detail
  lives under :per-source."
  (:require [evoclj.environment.revision :as rev]
            [evoclj.environment.source :as src]
            [evoclj.kernel.error :as err]
            [evoclj.support.failpoint :as fault]))

(declare refresh! refresh-async!)

(defn create-registry []
  (let [lock (Object.)]
    (atom {:sources {}
           :per-source {}
           :source-subs {}
           :current nil
           :last-good nil
           :seq 0
           :status :ok
           :dirty? false
           :last-refresh-error nil
           :listeners {}
           :lock lock
           :history []})))

(defn- registry-lock [registry]
  (:lock @registry))

(defn- source-id-of [source]
  (or (:source/id source)
      (:source-id source)
      (try (:source/id (src/snapshot! source))
           (catch Exception _ nil))))

(defn register-source!
  [registry source]
  (when-not (satisfies? src/LiveSource source)
    (throw (err/error :environment/invalid-source "source must satisfy LiveSource" {:source source})))
  (let [cn (.getName (class source))]
    (when-not (or (.contains cn "FakeSource") (.contains cn "StaticSource") (.contains cn "McpSource")
                  (.contains cn "SkillSource") (.contains cn "Skill") (.contains cn "skill"))
      (throw (err/error :environment/unsupported-source "only FakeSource, StaticSource, McpSource and SkillSource are supported" {:source-type cn}))))
  (let [sid (source-id-of source)]
    (when-not sid
      (throw (err/error :environment/invalid-source "source snapshot must contain :source/id" {})))
    (swap! registry (fn [s]
                      (-> s
                          (assoc-in [:sources sid] source)
                          (assoc-in [:per-source sid] {:current nil
                                                        :last-good nil
                                                        :seq 0
                                                        :history []
                                                        :status :ok
                                                        :last-refresh-error nil}))))
    (let [handle (src/subscribe! source (fn [] (swap! registry assoc :dirty? true) (refresh-async! registry sid)))]
      (swap! registry assoc-in [:source-subs sid] handle))
    sid))

;; --- top-level aggregate accessors (single-source contract preserved) -------

(defn current
  ([registry] (:current @registry))
  ([registry _source-id] (:current @registry)))

(defn last-good
  ([registry] (:last-good @registry))
  ([registry _source-id] (:last-good @registry)))

(defn status [registry]
  (let [s @registry]
    {:status (:status s) :dirty? (:dirty? s) :last-refresh-error (:last-refresh-error s) :seq (:seq s)}))

;; --- per-source accessors (E1) ----------------------------------------------

(defn source-state
  "Return the per-source state map for sid, or nil if not registered."
  [registry sid]
  (get-in @registry [:per-source sid]))

(defn source-current
  "Per-source current revision, or nil."
  [registry sid]
  (:current (source-state registry sid)))

(defn source-last-good
  "Per-source last-good revision, or nil."
  [registry sid]
  (:last-good (source-state registry sid)))

(defn source-seq
  "Per-source monotonic seq, or nil if not registered."
  [registry sid]
  (:seq (source-state registry sid)))

(defn source-status
  "Per-source status, or nil if not registered."
  [registry sid]
  (:status (source-state registry sid)))

(defn subscribe [registry listener-fn]
  (let [id (random-uuid)
        handle {:subscription/id id :close! (fn [] (swap! registry update :listeners dissoc id))}]
    (swap! registry assoc-in [:listeners id] listener-fn)
    handle))

(defn subscribe! [registry listener-fn]
  (subscribe registry listener-fn))

(defn- validate-snapshot [snapshot]
  (when-not (map? snapshot)
    (throw (err/error :environment/invalid-snapshot "snapshot must be a map" {:snapshot snapshot})))
  (when-not (:source/id snapshot)
    (throw (err/error :environment/invalid-snapshot "snapshot missing :source/id" {:snapshot snapshot})))
  (when-not (contains? snapshot :payload)
    (throw (err/error :environment/invalid-snapshot "snapshot missing :payload" {:snapshot snapshot})))
  snapshot)

;; Compute the new per-source entry from the current per-source entry and a
;; captured snapshot (or a captured error). Fail-closed: on error the entry's
;; current/last-good/seq are preserved and only :status/:last-refresh-error
;; change. Returns {:status :published|:noop|:error :entry ...}.
(defn- plan-source
  [entry snapshot-or-error]
  (if (:error snapshot-or-error)
    {:status :error
     :error (:error snapshot-or-error)
     :error-data (:error-data snapshot-or-error)
     :entry (assoc entry :status :degraded :last-refresh-error (:error-data snapshot-or-error))}
    (let [snapshot (:snapshot snapshot-or-error)]
      (try
        (validate-snapshot snapshot)
        (let [sid (:source/id snapshot)
              payload (:payload snapshot)
              candidate-id (rev/payload->id payload)
              cur (:current entry)
              cur-id (:revision/id cur)]
          (if (and cur-id (= candidate-id cur-id))
            {:status :noop :entry entry}
            (let [prev-seq (:seq entry)
                  next-seq (inc prev-seq)
                  new-rev (rev/make-revision sid payload next-seq)]
              {:status :published
               :revision new-rev
               :entry (-> entry
                          (assoc :current new-rev :last-good new-rev :seq next-seq
                                 :status :ok :last-refresh-error nil)
                          (update :history (fnil conj []) new-rev))})))
        (catch Throwable e
          {:status :error
           :error e
           :error-data (err/error-data e)
           :entry (assoc entry :status :degraded :last-refresh-error (err/error-data e))})))))

(defn refresh!
  ([registry]
   (refresh! registry nil))
  ([registry source-id]
   (refresh! registry source-id nil))
  ([registry source-id {:as opts}]
   (let [lock (registry-lock registry)]
     (locking lock
       (swap! registry assoc :dirty? true)
       (let [state @registry
             sources (:sources state)
             per-src (:per-source state)
             target-ids (if source-id
                          (if (contains? sources source-id)
                            [source-id]
                            (throw (err/error :environment/no-source "no such source registered" {:source-id source-id})))
                          (vec (keys sources)))]
         (when (empty? target-ids)
           (throw (err/error :environment/no-source "no source registered" {:source-id source-id})))
         (let [plans (reduce
                      (fn [m sid]
                        (let [src (get sources sid)
                              entry (get per-src sid)]
                          (assoc m sid
                                 (try
                                   (let [snapshot (do (fault/trigger! opts :after-snapshot)
                                                      (src/snapshot! src))]
                                     (validate-snapshot snapshot)
                                     (fault/trigger! opts :after-validate)
                                     (plan-source entry {:snapshot snapshot}))
                                   (catch Throwable e
                                     (plan-source entry {:error e :error-data (err/error-data e)}))))))
                      {} target-ids)
               any-error (some #(= :error (:status %)) (vals plans))
               any-published (some #(= :published (:status %)) (vals plans))
               published (keep (fn [[_ p]] (when (= :published (:status p)) (:revision p))) plans)
               new-per-src (into per-src (for [[sid p] plans] [sid (:entry p)]))]
           (swap! registry (fn [s]
                             (let [s (assoc s :per-source new-per-src)
                                   s (if (seq published)
                                       (update s :history (fnil into []) published)
                                       s)
                                   top (reduce (fn [acc [sid e]]
                                                 (let [sq (:seq e)]
                                                   (if (> sq (:seq acc))
                                                     {:current (:current e) :last-good (:last-good e) :seq sq}
                                                     acc)))
                                               {:current nil :last-good nil :seq -1}
                                               new-per-src)
                                   s (assoc s
                                            :current (:current top)
                                            :last-good (:last-good top)
                                            :seq (max (:seq s) (max 0 (:seq top)))
                                            :status (if any-error :degraded :ok)
                                            :dirty? (boolean any-error)
                                            :last-refresh-error (when any-error
                                                                  (some #(:last-refresh-error (:entry %)) (vals plans))))]
                               s)))
           (let [post-error
                 (try
                   (fault/trigger! opts :mid-publish)
                   (doseq [[sid p] plans
                           :when (= :published (:status p))]
                     (let [prev (:current (get per-src sid))
                           curr (:revision p)]
                       (doseq [[_ listener] (:listeners state)]
                         (try (listener {:prev prev :curr curr}) (catch Exception _ nil)))))
                   nil
                   (catch Throwable e e))]
             (when post-error
               (swap! registry assoc :status :degraded :last-refresh-error (err/error-data post-error)))
             (let [single? (= 1 (count target-ids))
                   single-plan (when single? (val (first plans)))
                   status (if post-error
                            :error
                            (if single?
                              (let [p (val (first plans))]
                                (cond (= :error (:status p)) :error
                                      (= :published (:status p)) :published
                                      :else :noop))
                              (cond any-error :partial
                                    any-published :published-all
                                    :else :noop-all)))
                   error-ex (or post-error
                                (when single? (:error single-plan))
                                nil)]
               (let [result
                     {:status status
                      :error error-ex
                      :revision (if single?
                                  (or (:revision single-plan)
                                      (:current (:entry single-plan)))
                                  (some :revision (vals plans)))
                      :error-data (if single?
                                    (:error-data single-plan)
                                    (some :error-data (vals plans)))
                      :per-source (reduce (fn [m [sid p]]
                                            (assoc m sid (cond-> {:status (:status p)}
                                                            (= :published (:status p)) (assoc :revision (:revision p))
                                                            (= :error (:status p)) (assoc :error-data (:error-data p)))))
                                          {} plans)}]
                 result)))))))))

(defn refresh-async!
  ([registry] (refresh-async! registry nil))
  ([registry source-id] (future (refresh! registry source-id))))

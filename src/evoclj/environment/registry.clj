(ns evoclj.environment.registry
  "EnvironmentRegistry - minimal in-memory registry for LiveSource.
  Pipeline: invalidate -> mark dirty -> single-flight snapshot ->
  validate -> derive candidate -> atomic swap. Generation (seq) only
  increments on successful publish. Failure keeps last-good, marks
  degraded and dirty. Identical content returns noop without new seq.
  Listeners receive publication diff, not raw file events.
  Currently supports FakeSource and StaticSource only."
  (:require [evoclj.environment.revision :as rev]
            [evoclj.environment.source :as src]
            [evoclj.kernel.error :as err]
            [evoclj.support.failpoint :as fault]))

(declare refresh! refresh-async!)

(defn create-registry []
  (let [lock (Object.)]
    (atom {:sources {}
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

(defn register-source! [registry source]
  (when-not (satisfies? src/LiveSource source)
    (throw (err/error :environment/invalid-source "source must satisfy LiveSource" {:source source})))
  (let [cn (.getName (class source))]
    (when-not (or (.contains cn "FakeSource") (.contains cn "StaticSource") (.contains cn "McpSource")
                  (.contains cn "SkillSource") (.contains cn "Skill") (.contains cn "skill"))
      (throw (err/error :environment/unsupported-source "only FakeSource, StaticSource, McpSource and SkillSource are supported" {:source-type cn}))))
  (let [sid (source-id-of source)]
    (when-not sid
      (throw (err/error :environment/invalid-source "source snapshot must contain :source/id" {})))
    (swap! registry assoc-in [:sources sid] source)
    (let [handle (src/subscribe! source (fn [] (swap! registry assoc :dirty? true) (refresh-async! registry sid)))]
      (swap! registry assoc-in [:source-subs sid] handle))
    sid))

(defn current
  ([registry] (:current @registry))
  ([registry _source-id] (:current @registry)))

(defn last-good
  ([registry] (:last-good @registry))
  ([registry _source-id] (:last-good @registry)))

(defn status [registry]
  (let [s @registry]
    {:status (:status s) :dirty? (:dirty? s) :last-refresh-error (:last-refresh-error s) :seq (:seq s)}))

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

(defn refresh!
  ([registry]
   (refresh! registry nil))
  ([registry source-id]
   ;; T2 failpoint seams: pass an opts map, e.g.
   ;; {:failpoints {:after-snapshot (fn [] ...)}} — see
   ;; evoclj.support.failpoint. The pipeline below deliberately stays
   ;; inside this fn's own degradation catch (unchanged by T2), so an
   ;; injected fault surfaces as {:status :error ...} with the registry
   ;; marked :degraded rather than as a thrown exception.
   (refresh! registry source-id nil))
  ([registry source-id {:as opts}]
   (let [lock (registry-lock registry)]
     (locking lock
       (swap! registry assoc :dirty? true)
       (let [source (if source-id
                      (get-in @registry [:sources source-id])
                      (first (vals (:sources @registry))))]
         (when-not source
           (throw (err/error :environment/no-source "no source registered" {:source-id source-id})))
         (try
           (let [snapshot (src/snapshot! source)]
             ;; T2 seam: snapshot captured, not yet validated
             (fault/trigger! opts :after-snapshot)
             (let [_ (validate-snapshot snapshot)]
               ;; T2 seam: snapshot valid; candidate not yet derived/published
               (fault/trigger! opts :after-validate)
               (let [sid (:source/id snapshot)
                     payload (:payload snapshot)
                     candidate-id (rev/payload->id payload)
                     cur (:current @registry)
                     cur-id (:revision/id cur)]
                 (if (and cur-id (= candidate-id cur-id))
                   (do
                     (swap! registry assoc :status :ok :dirty? false :last-refresh-error nil)
                     {:status :noop :revision cur})
                   (let [prev @registry
                         prev-current (:current prev)
                         prev-seq (:seq prev)
                         next-seq (inc prev-seq)
                         new-rev (rev/make-revision sid payload next-seq)]
                     (loop []
                       (let [cur-state @registry
                             cur-seq (:seq cur-state)]
                         (if (not= cur-seq prev-seq)
                           (let [new-cur (:current cur-state)
                                 new-id (:revision/id new-cur)]
                             (if (= candidate-id new-id)
                               (do
                                 (swap! registry assoc :status :ok :dirty? false :last-refresh-error nil)
                                 {:status :noop :revision new-cur})
                               {:status :noop :revision new-cur}))
                           (let [new-state (-> cur-state
                                               (assoc :current new-rev :last-good new-rev :seq next-seq :status :ok :dirty? false :last-refresh-error nil)
                                               (update :history (fnil conj []) new-rev))]
                             (if (compare-and-set! registry cur-state new-state)
                               (do
                                 ;; T2 seam: CAS swap done; listeners not yet notified
                                 (fault/trigger! opts :mid-publish)
                                 (doseq [[_ listener] (:listeners cur-state)]
                                   (try (listener {:prev prev-current :curr new-rev}) (catch Exception _ nil)))
                                 {:status :published :revision new-rev})
                               (recur)))))))))))
           (catch Throwable e
             (let [ed (err/error-data e)]
               (swap! registry assoc :status :degraded :dirty? true :last-refresh-error ed)
               {:status :error :error e :error-data ed :revision (:current @registry)}))))))))

(defn refresh-async!
  ([registry] (refresh-async! registry nil))
  ([registry source-id] (future (refresh! registry source-id))))

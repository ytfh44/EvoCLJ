(ns evoclj.evolution.diagnostic
  "Construction and persistence of evidence-plus-diagnosis bundles.

  A bundle carries provenance and structured findings for one subject.
  Persistence and model projection remain separate concerns."
  (:require [evoclj.evolution.diagnosis-schema :as diagnosis]
            [evoclj.evolution.diagnostic-schema :as schema]
            [evoclj.evolution.evidence-schema :as evidence]
            [evoclj.genome.hash :as hash]
            [evoclj.kernel.error :as err]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)))

(def ^:private media-type "application/edn")

(defn- canonical
  "Deterministic EDN form used for the bundle content address."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn- digest
  [data]
  (hash/text-digest (pr-str (canonical data))))

(defn- utf8-bytes
  [s]
  (.getBytes ^String s StandardCharsets/UTF_8))

(defn build-bundle
  "Build a validated DiagnosticBundle from a frozen evidence pack,
  diagnosis, and capture details.

  `details` supplies :producer, :subject, :findings, :exit-status,
  :status, and :captured-at. The evidence and diagnosis ids are copied
  from their validated inputs and must refer to the same evidence pack."
  [evidence-pack diagnosis-result details]
  (evidence/validate-pack evidence-pack)
  (diagnosis/validate-diagnosis diagnosis-result)
  (when-not (= (:evidence/id evidence-pack)
               (:evidence/id diagnosis-result))
    (throw (err/error :diagnostic/provenance-mismatch
                      "diagnosis must reference the supplied evidence pack"
                      {:evidence/id (:evidence/id evidence-pack)
                       :diagnosis/evidence-id (:evidence/id diagnosis-result)})))
  (let [body (merge details
                    {:evidence/id (:evidence/id evidence-pack)
                     :diagnosis/id (:diagnosis/id diagnosis-result)})
        bundle (assoc body :diagnostic/id (digest body))]
    (schema/validate-bundle bundle)
    bundle))

(defn- validate-store!
  [store]
  (when-not (and (map? store)
                 (contains? store :sqlite)
                 (contains? store :cas))
    (throw (err/error :diagnostic/store-invalid
                      "store must carry :sqlite and :cas handles"
                      {:store/keys (when (map? store) (keys store))})))
  store)

(defn persist-bundle!
  "Persist a validated bundle as one content-addressed EDN artifact.

  CAS bytes are written before the idempotent catalog row, so a catalog
  entry never points at a body that has not been installed."
  [store bundle]
  (validate-store! store)
  (schema/validate-bundle bundle)
  (let [body (dissoc bundle :diagnostic/id)
        id (digest body)]
    (when-not (= id (:diagnostic/id bundle))
      (throw (err/error :diagnostic/id-mismatch
                        "diagnostic id must be the content hash of the bundle body"
                        {:diagnostic/id (:diagnostic/id bundle)
                         :expected id})))
    (let [put-result (cas/put-bytes! (:cas store)
                                     (utf8-bytes (pr-str (canonical body)))
                                     {:media-type media-type})]
      (artifact/ensure-artifact! (:sqlite store)
                                 id
                                 media-type
                                 (:size put-result))
      bundle)))

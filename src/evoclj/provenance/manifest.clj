(ns evoclj.provenance.manifest
  "ContextManifest provenance for model requests.

  A manifest captures the immutable inputs that produced a PreparedModelCall:
  active bindings, tool catalog, and history envelope. It is written to CAS
  as deterministic EDN; events store only the manifest-ref (artifact id),
  never the full prompt. Identical immutable inputs yield identical bytes
  and identical artifact id."
  (:require [clojure.edn :as edn]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)))

(defn- deep-sort
  "Recursively convert maps to sorted-maps for deterministic pr-str."
  [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [k (deep-sort v)]) x))
    (vector? x) (mapv deep-sort x)
    (sequential? x) (mapv deep-sort x)
    (set? x) (into (sorted-set) (map deep-sort x))
    :else x))

(defn- normalize-bindings
  [bindings]
  (->> (or bindings [])
       (map (fn [b]
              (select-keys b [:binding/id :logical/id :revision/id])))
       (sort-by (fn [b] (str (:binding/id b) "/" (:logical/id b) "/" (:revision/id b))))
       vec))

(defn- normalize-tool-catalog
  [tc]
  (if (nil? tc)
    {}
    (let [bid (:binding/id tc)
          rids (or (:revision-ids tc) (:revision/id tc) (:tool-catalog/revision-ids tc))]
      (cond-> {}
        bid (assoc :binding/id bid)
        rids (assoc :revision-ids (if (map? rids)
                                    (into (sorted-map) (map (fn [[k v]] [k v]) rids))
                                    rids))))))

(defn make-manifest
  "Build a deterministic ContextManifest.

  opts map keys:
  :bindings - seq of {:binding/id :logical/id :revision/id}
  :tool-catalog - {:binding/id :revision-ids {id -> revision-id}}
  :history - {:compression-envelope/ref artifact-id} or any map
  :context/manifest-version - defaults to 1

  Also supports positional arity: (make-manifest bindings tool-catalog history)."
  ([opts]
   (let [version (or (:context/manifest-version opts)
                     (:manifest-version opts)
                     1)
         bindings (normalize-bindings (:bindings opts))
         tool-catalog (normalize-tool-catalog (:tool-catalog opts))
         history (let [h (:history opts)]
                   (cond
                     (nil? h) {}
                     (map? h) (deep-sort h)
                     :else h))]
     (deep-sort {:context/manifest-version version
                 :bindings bindings
                 :tool-catalog tool-catalog
                 :history history})))
  ([bindings tool-catalog history]
   (make-manifest {:bindings bindings
                   :tool-catalog tool-catalog
                   :history history})))

(defn- manifest->bytes
  ^bytes [manifest]
  (.getBytes (pr-str (deep-sort manifest)) StandardCharsets/UTF_8))

(defn put-manifest!
  "Write manifest to CAS and return manifest-ref (artifact id string).

  cas - CAS root path, Path, File, or config map {:root ...}
  manifest - map from make-manifest

  Uses deterministic EDN bytes so identical immutable inputs produce
  the same artifact id."
  [cas manifest]
  (let [m (deep-sort manifest)
        ba (manifest->bytes m)
        res (cas/put-bytes! cas ba {:media-type "application/edn"})]
    (:artifact/id res)))

(defn load-manifest
  "Load manifest from CAS by manifest-ref (artifact id). Returns map."
  [cas manifest-ref]
  (let [ba (cas/get-bytes cas manifest-ref)
        s (String. ^bytes ba StandardCharsets/UTF_8)]
    (edn/read-string s)))

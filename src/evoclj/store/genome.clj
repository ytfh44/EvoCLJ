(ns evoclj.store.genome
  "Genome bundle persistence & round-trip for H1 hydration.
  
  The canonical source of a Genome bundle is the on-disk directory
  (evoclj.genome.load/load-genome). For hydration without a bundle root,
  this namespace provides a durable store representation: the full loaded
  genome (:manifest + :files + :programs) plus the provider catalog are
  serialized as EDN and stored in the CAS. A meta-table pointer maps
  genome-id -> bundle CAS address so the bundle can be retrieved from
  the store without a filesystem path.
  
  This is a helper for hydration and tests; the canonical load path for
  production execution remains the on-disk bundle directory."
  (:require [clojure.edn :as edn]
            [evoclj.genome.hash :as hash]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.artifact :as artifact]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)))

(def ^:private meta-prefix "genome:bundle:")

(defn- bundle-meta-key
  [genome-id]
  (str meta-prefix genome-id))

(defn register-loaded-genome!
  "Persist a loaded-genome-for-execution (with :programs attached) and
  its provider catalog so they can be re-compiled by id later.
  
  Returns the bundle's CAS content address.
  
  `loaded-for-exec` must contain :genome/id, :manifest, :files, and
  :programs. The :genome/root and :genome/id are removed from the
  persisted :loaded map (the genome-id is carried by the meta key).
  
  `provider-catalog` is the map of model alias -> provider entry used
  by evoclj.compiler.resolution/resolve-models.
  
  Side effects:
  - Writes bundle EDN to CAS (media-type application/edn)
  - Registers the artifact row for the bundle
  - Upserts meta key genome:bundle:<gid> -> bundle-cas-address
  - Ensures artifact rows for genome-id, resolution-id, phenotype-id
  - Ensures genome row for genome-id"
  [cas-store db loaded-for-exec provider-catalog]
  (let [gid (:genome/id loaded-for-exec)
        rid (:resolution/id loaded-for-exec)
        pid (or (:phenotype/id loaded-for-exec)
                (:code/id loaded-for-exec))
        body (.getBytes (pr-str {:catalog provider-catalog
                                 :loaded (-> loaded-for-exec
                                             (dissoc :genome/root :genome/id))})
                        StandardCharsets/UTF_8)
        bundle-addr (:artifact/id (cas/put-bytes! cas-store body {:media-type "application/edn"}))
        _ (artifact/ensure-artifact! db bundle-addr "application/edn" (alength body))
        _ (sqlite/exec! db ["INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)"
                            (bundle-meta-key gid) bundle-addr])
        _ (artifact/ensure-artifact! db gid "application/octet-stream" 0)
        _ (artifact/ensure-artifact! db rid "application/edn" 0)
        _ (artifact/ensure-artifact! db pid "application/edn" 0)
        _ (artifact/ensure-genome! db gid)]
    bundle-addr))

(defn loaded-genome
  "Reconstruct the persisted loaded-genome-for-execution + catalog for
  `genome-id`, or nil when no bundle was registered.
  
  Returns {:catalog <provider-catalog> :loaded {:manifest ... :files ... :programs ...}}
  with :genome/id re-attached to :loaded, or nil if no bundle exists
  or the CAS/DB is unavailable."
  [cas-store db genome-id]
  (when cas-store
    (try
      (let [addr (some-> (first (sqlite/query db ["SELECT value FROM meta WHERE key = ?"
                                                  (bundle-meta-key genome-id)]))
                         :value)]
        (when addr
          (let [{:keys [catalog loaded]} (edn/read-string
                                          (String. (cas/get-bytes cas-store addr)
                                                   StandardCharsets/UTF_8))]
            {:catalog catalog
             :loaded (assoc loaded :genome/id genome-id)})))
      (catch Exception _ nil))))
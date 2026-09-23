(ns evoclj.support.genome-fixtures
  "Shared fixtures for tests that HYDRATE a session.

  Hydration is fail-closed (evoclj.runtime.hydrate): it needs a REGISTERED
  genome bundle (evoclj.store.genome/register-loaded-genome!) and the I1
  identity rows (evoclj.store.identity) — a synthetic fallback no longer
  exists. These helpers write a real, tiny echo bundle
  (:tool :fixture/echo → :emit, no model, no SCI program) and register it,
  so a child/eval session executes a REAL compiled topology."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [evoclj.compiler.core :as compiler]
            [evoclj.genome.load :as load]
            [evoclj.store.genome :as store-genome]
            [evoclj.store.identity :as identity])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def provider-catalog
  "The component fixture provider catalog (fixtures/resolution/provider-catalog.edn)."
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(def ^:private echo-manifest
  {:genome/format 1
   :agent/id :main
   :agent/entry :graph/echo
   :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
   :modules {:topology "topology.edn"
             :models "models.edn"
             :memory "memory.edn"
             :evolution "evolution.edn"}
   :capabilities/requested #{:tool/call}
   :evolution {:max-risk :behavioral
               :mutable #{:parameters :prompts :skills :programs}}
   :metadata {:name "echo-agent"
              :description "test fixture: tool -> emit"}})

(def ^:private echo-topology
  {:graph/id :graph/echo
   :entry :node/tool
   :nodes {:node/tool {:node/type :tool :tool :fixture/echo :next :node/emit}
           :node/emit {:node/type :emit}}
   :limits {:max-steps 64}})

(defn- write-file!
  [dir rel content]
  (let [p (.resolve (.toPath (io/file dir)) rel)]
    (Files/createDirectories (.getParent p) (make-array FileAttribute 0))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

(defn echo-fixture
  "Write the echo bundle into a fresh temp directory and return

    {:bundle-root <dir>      ; the on-disk bundle
     :loaded <loaded genome + :programs []>
     :genome/id .. :resolution/id .. :code/id .. :abi ..
     :compiled <CompiledGenome>}

  The bundle's topology is :tool :fixture/echo → :emit, so a session that
  runs it needs only the fixture echo provider and a :tool/call lease."
  []
  (let [dir (str (Files/createTempDirectory "evoclj-echo-genome-"
                                            (make-array FileAttribute 0)))]
    (write-file! dir "manifest.edn" (pr-str echo-manifest))
    (write-file! dir "topology.edn" (pr-str echo-topology))
    (write-file! dir "models.edn" (pr-str {:models {}}))
    (write-file! dir "memory.edn" (pr-str {:memory {}}))
    (write-file! dir "evolution.edn" (pr-str {:evolution {}}))
    (let [loaded (assoc (load/load-genome dir) :programs [])
          compiled (compiler/compile-genome loaded provider-catalog)]
      {:bundle-root dir
       :loaded loaded
       :compiled compiled
       :genome/id (:code/genome-id compiled)
       :resolution/id (:code/resolution-id compiled)
       :code/id (:code/id compiled)
       :abi (:abi compiled)})))

(defn register-echo-genome!
  "Register `echo-fixture`'s bundle (H1) and its I1 identity rows (I1) in
  `db` (a sqlite path/spec) with `cas-store`. Returns the fixture map."
  [db cas-store fixture]
  (store-genome/register-loaded-genome! cas-store db
                                        (assoc (:loaded fixture)
                                               :resolution/id (:resolution/id fixture)
                                               :code/id (:code/id fixture))
                                        provider-catalog)
  (identity/record-code-image! db {:code/id (:code/id fixture)
                                   :code/genome-id (:genome/id fixture)
                                   :code/resolution-id (:resolution/id fixture)
                                   :abi (:abi fixture)})
  fixture)

(defn delete-tree!
  "Recursively delete a temp path (bundle dirs, CAS roots)."
  [path]
  (let [p (.toPath (io/file (str path)))]
    (when (Files/exists p (make-array java.nio.file.LinkOption 0))
      (with-open [stream (Files/walk p (make-array java.nio.file.FileVisitOption 0))]
        (doseq [f (reverse (iterator-seq (.iterator stream)))]
          (Files/deleteIfExists f))))))

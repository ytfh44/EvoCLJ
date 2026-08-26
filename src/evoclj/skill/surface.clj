(ns evoclj.skill.surface
  "Projectors for Skill -> SurfaceBundle.

  One skill directory yields one SurfaceBundle with two peer surfaces:

  - ContextSurface  : descriptor = catalog metadata (name, description, allowed-tools)
                    materializer reads full SKILL.md bytes for that exact tree revision via CAS
  - DirectorySurface: RO mount over the immutable CAS tree (files + scripts/ + references/)

  Sibling surfaces are co-versioned (same revision/id = tree id).
  Bundles are published atomically via environment.bundle."
  (:require [clojure.string :as str]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.surface :as surf]
            [evoclj.kernel.error :as err]
            [evoclj.mount.backend :as backend]
            [evoclj.store.cas :as cas]
            [evoclj.fs.snapshot :as snapshot])
  (:import (java.nio.charset StandardCharsets)))

(defn skill-name->logical-id
  [skill-name]
  (when-not (and (string? skill-name) (not (str/blank? skill-name)))
    (throw (err/error :skill/invalid-descriptor "skill name must be non-empty string" {:skill/name skill-name})))
  [:skill skill-name])

(defn- descriptor-for
  "Catalog descriptor (small, for Offer). Progressive disclosure: catalog only shows name+description.
  Carries the :materializer descriptor (WO-S1) telling the generic materializer
  to read SKILL.md from the CAS tree named by the surface's :revision/id."
  [skill-name frontmatter]
  {:name skill-name
   :description (or (:description frontmatter) "")
   :allowed-tools (:allowed-tools frontmatter)
   :allowed-tools-parsed (:allowed-tools-parsed frontmatter)
   :skill/name skill-name
   :materializer {:type :cas-tree-file :path "SKILL.md"}})

(defn- materializer-fn
  "Build the canonical single-arity ContextSurface materializer (INV-05):
   (materializer cas revision-id) -> SKILL.md content string for the CAS tree
   named by revision-id (defaulting to tree-id). Fail-closed (INV-04): a nil
   CAS resolver, an unreadable/missing tree, or a missing SKILL.md throws a
   typed error; it NEVER substitutes cached or degraded content."
  [cas tree-id]
  (fn [cas-arg revision-id]
    (let [c (or cas-arg cas)
          tid (or revision-id tree-id)]
      (when-not c
        (throw (err/error :skill/materializer-missing-cas "CAS required to materialize skill" {:tree/id tid})))
      (let [manifest (snapshot/load-tree c tid)]
        (when-not (map? manifest)
          (throw (err/error :skill/missing-skill-md "invalid tree manifest for skill" {:tree/id tid})))
        (let [ba (snapshot/get-file-bytes c manifest "SKILL.md")]
          (when-not ba
            (throw (err/error :skill/missing-skill-md "SKILL.md missing in tree" {:tree/id tid})))
          (String. ^bytes ba StandardCharsets/UTF_8))))))

(defn make-context-surface
  "Create ContextSurface for a skill."
  [{:keys [skill-name frontmatter cas tree-id]}]
  (let [logical-id (skill-name->logical-id skill-name)
        descriptor (descriptor-for skill-name frontmatter)
        mat (materializer-fn cas tree-id)]
    (surf/make-context-surface {:id (keyword "skill" (str skill-name "-ctx"))
                                :descriptor descriptor
                                :materializer mat
                                :revision/id tree-id})))

(defn make-directory-surface
  "Create DirectorySurface RO for a skill (CAS tree backend)."
  [{:keys [skill-name cas tree-id]}]
  (let [backend (backend/cas-tree-backend cas tree-id)]
    (surf/make-directory-surface {:id (keyword "skill" (str skill-name "-dir"))
                                  :backend backend
                                  :access-max #{:read :list :stat}
                                  :revision/id tree-id})))

(defn skill->bundle
  "Derive one SurfaceBundle for a skill.

  Args:
    {:skill/name string
     :tree/id \"sha256:...\"  manifest id from snapshot
     :frontmatter map from parser
     :body string markdown body (unused for materialization — the context
            surface reads the pinned SKILL.md from the CAS tree, never a
            cached body; kept for call-site compatibility)
     :cas CAS handle}

  Returns bundle where both surfaces share tree/id. Throws on validation."
  [{:keys [skill/name tree/id frontmatter body cas] :as opts}]
  (let [skill-name (or name (throw (err/error :skill/invalid-descriptor "skill name required" {:opts opts})))
        tid (or id (throw (err/error :skill/invalid-descriptor "tree/id required" {:opts opts})))]
    (when-not (and (string? tid) (re-matches #"^sha256:[0-9a-f]{64}$" tid))
      (throw (err/error :skill/invalid-descriptor "tree/id must be sha256" {:tree/id tid})))
    (let [ctx (make-context-surface {:skill-name skill-name :frontmatter frontmatter :cas cas :tree-id tid})
          dir (make-directory-surface {:skill-name skill-name :cas cas :tree-id tid})
          logical-id (skill-name->logical-id skill-name)
          bundle-id (str "bundle:skill:" skill-name ":" tid)]
      (bundle/make-bundle {:bundle-id bundle-id :revision-id tid :logical-id logical-id :surfaces [ctx dir]}))))


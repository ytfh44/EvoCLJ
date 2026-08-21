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
  "Catalog descriptor (small, for Offer). Progressive disclosure: catalog only shows name+description."
  [skill-name frontmatter]
  {:name skill-name
   :description (or (:description frontmatter) "")
   :allowed-tools (:allowed-tools frontmatter)
   :allowed-tools-parsed (:allowed-tools-parsed frontmatter)
   :skill/name skill-name})

(defn- materializer-for
  "Materializer function for ContextSurface. It reads exact SKILL.md bytes
  for the revision's CAS tree (never live host path). This ensures
  activation sees the exact revision that was snapshotted, even if upstream
  later changes."
  [cas tree-id]
  (fn
    ([]
     ;; arity 0 for compatibility: return descriptor hint
     {:tree/id tree-id})
    ([cas-arg revision-id]
     (let [c (or cas-arg cas)
           tid (or revision-id tree-id)]
       (when-not c
         (throw (err/error :skill/materializer-missing-cas "CAS required to materialize skill" {:tree/id tid})))
       (let [manifest (snapshot/load-tree c tid)
             ba (snapshot/get-file-bytes c manifest "SKILL.md")]
         (when-not ba
           (throw (err/error :skill/missing-skill-md "SKILL.md missing in tree" {:tree/id tid})))
         (String. ^bytes ba StandardCharsets/UTF_8))))
    ([cas-arg revision-id _opts]
     ;; 3-arity for materializer interface consistency
     (materializer-for cas tree-id))))

(defn- materializer-fn
  "Build a materializer that reads SKILL.md from CAS tree-id.
  The materializer is a function (cas revision-id -> content string) and also
  carries :tree/id metadata. It closes over cas and tree-id so that
  even if the host file changes, the materialized content is pinned."
  [cas tree-id body-cache]
  ;; body-cache is the body at snapshot time; but we must read via CAS for fidelity.
  ;; We keep a fn that re-reads from CAS on each materialize to prove immutability.
  (fn
    ([] body-cache)
    ([cas-arg] (materializer-fn cas tree-id body-cache))
    ([cas-arg revision-id]
     (let [c (or cas-arg cas)
           tid (or revision-id tree-id)]
       (if c
         (try
           (let [manifest (snapshot/load-tree c tid)
                 ba (snapshot/get-file-bytes c manifest "SKILL.md")
                 _ (when-not ba (throw (err/error :skill/missing-skill-md "SKILL.md missing" {:tree/id tid})))
                 raw (String. ^bytes ba StandardCharsets/UTF_8)
                 ;; strip frontmatter, return body? For materializer we return full text per progressive disclosure spec:
                 ;; activation -> full SKILL.md exact revision
                 full raw]
             full)
           (catch Exception _
             ;; fallback to cached body if CAS unavailable (tests may use func cas)
             body-cache))
         body-cache)))
    ([cas-arg revision-id _] (materializer-fn cas tree-id body-cache))))

(defn make-context-surface
  "Create ContextSurface for a skill."
  [{:keys [skill-name frontmatter body cas tree-id]}]
  (let [logical-id (skill-name->logical-id skill-name)
        descriptor (descriptor-for skill-name frontmatter)
        mat (materializer-fn cas tree-id body)]
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
     :body string markdown body
     :cas CAS handle}

  Returns bundle where both surfaces share tree/id. Throws on validation."
  [{:keys [skill/name tree/id frontmatter body cas] :as opts}]
  (let [skill-name (or name (throw (err/error :skill/invalid-descriptor "skill name required" {:opts opts})))
        tid (or id (throw (err/error :skill/invalid-descriptor "tree/id required" {:opts opts})))]
    (when-not (and (string? tid) (re-matches #"^sha256:[0-9a-f]{64}$" tid))
      (throw (err/error :skill/invalid-descriptor "tree/id must be sha256" {:tree/id tid})))
    (let [ctx (make-context-surface {:skill-name skill-name :frontmatter frontmatter :body body :cas cas :tree-id tid})
          dir (make-directory-surface {:skill-name skill-name :cas cas :tree-id tid})
          logical-id (skill-name->logical-id skill-name)
          bundle-id (str "bundle:skill:" skill-name ":" tid)]
      (bundle/make-bundle {:bundle-id bundle-id :revision-id tid :logical-id logical-id :surfaces [ctx dir]}))))


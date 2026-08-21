(ns evoclj.cli.skill
  "Skill UX CLI: skill list/inspect/validate/vendor, delegating to EnvironmentRegistry and Skill adapter."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.skill.adapter :as adapter]
            [evoclj.skill.parser :as parser]
            [evoclj.skill.vendor :as vendor]
            [evoclj.store.cas :as cas])
  (:import (java.nio.file Files Path Paths LinkOption)
           (java.nio.charset StandardCharsets)))

;; --- helpers ----------------------------------------------------------------

(defn- registry-for [opts]
  (or (:registry opts)
      (:environment/registry opts)
      (get-in opts [:overrides :environment/registry])
      (get-in opts [:system :environment/registry])
      (try
        (let [system ((resolve 'evoclj.cli.session/build-system) opts)]
          (:environment/registry system))
        (catch Exception _ nil))
      nil))

(defn- cas-for [opts]
  (or (:cas opts)
      (:store/cas opts)
      (get-in opts [:overrides :store/cas])
      (get-in opts [:system :store/cas])
      (try
        (let [system ((resolve 'evoclj.cli.session/build-system) opts)]
          (:store/cas system))
        (catch Exception _ nil))
      ;; fallback to state-dir cas
      (when-let [sd (:state-dir opts)]
        (try (cas/->cas (str sd "/cas")) (catch Exception _ nil)))
      nil))

(defn- genome-root-for [opts]
  (or (:genome/root opts)
      (:genome-root opts)
      (get-in opts [:options :genome])
      (get-in opts [:options :genome-root])
      (when-let [sd (:state-dir opts)]
        (let [p (Paths/get (str sd) (into-array String ["genomes" "seed"]))]
          (when (Files/exists p (make-array LinkOption 0))
            (str p))))
      (when-let [sd (:state-dir opts)]
        (str sd "/genomes/seed"))
      "./genomes/seed"))

;; --- commands ----------------------------------------------------------------

(defn list!
  "evoclj skill list

  List current skill offers (catalog projection). Delegates to Skill adapter's registry."
  [opts]
  (let [registry (registry-for opts)]
    (if (nil? registry)
      {:skills [] :count 0}
      (let [offers (adapter/list-offers registry)]
        {:skills (mapv (fn [o]
                         {:skill/name (:offer/name o)
                          :skill/logical-id (:offer/logical-id o)
                          :skill/description (:offer/description o)
                          :revision/id (:offer/revision-id o)
                          :bundle/id (:offer/bundle-id o)})
                       offers)
         :count (count offers)}))))

(defn inspect!
  "evoclj skill inspect <name>

  Inspect one skill's bundle and offer."
  [opts]
  (let [name (first (:positionals opts))]
    (when-not name
      (throw (err/error :cli/usage-invalid
                        "skill inspect requires <name>"
                        {:usage "evoclj skill inspect <name>"})))
    (let [registry (registry-for opts)]
      (when-not registry
        (throw (err/error :cli/skill-not-found "no skill registry available" {:skill/name name})))
      (let [bundle (adapter/get-skill-bundle registry name)]
        (when-not bundle
          (throw (err/error :cli/skill-not-found "no skill with this name" {:skill/name name})))
        (let [offer (adapter/current-offer-for registry name)]
          {:skill/name name
           :logical/id (:logical/id bundle)
           :bundle/id (:bundle/id bundle)
           :revision/id (:revision/id bundle)
           :bundle bundle
           :offer offer
           :surfaces (:surfaces bundle)})))))

(defn validate!
  "evoclj skill validate <path>

  Validate SKILL.md at path with strict YAML/frontmatter rules. Delegates to skill parser."
  [opts]
  (let [path (first (:positionals opts))]
    (when-not path
      (throw (err/error :cli/usage-invalid
                        "skill validate requires <path>"
                        {:usage "evoclj skill validate <path>"})))
    (let [f (io/file path)
          skill-md (if (.isDirectory f)
                     (io/file f "SKILL.md")
                     f)]
      (when-not (.exists skill-md)
        (throw (err/error :cli/skill-not-found "SKILL.md not found at path" {:path (str path)})))
      (let [content (slurp skill-md)
            parsed (parser/parse-skill-content content :strict)]
        {:valid? true
         :path (str (.getAbsolutePath skill-md))
         :frontmatter (:frontmatter parsed)
         :body (:body parsed)
         :skill/name (or (:name (:frontmatter parsed)) (.getName (.getParentFile skill-md)))}))))

(defn vendor!
  "evoclj skill vendor <name>

  Vendor an external Skill snapshot revision into the Genome. Delegates to skill vendor (CAS snapshot revision, not live path)."
  [opts]
  (let [name (first (:positionals opts))]
    (when-not name
      (throw (err/error :cli/usage-invalid
                        "skill vendor requires <name>"
                        {:usage "evoclj skill vendor <name>"})))
    (let [registry (registry-for opts)
          cas-handle (cas-for opts)
          genome-root (genome-root-for opts)]
      (when-not registry
        (throw (err/error :cli/skill-not-found "no skill registry available for vendor" {:skill/name name})))
      (when-not cas-handle
        (throw (err/error :skill/vendor-invalid-args "vendor requires :cas" {:skill/name name})))
      (when-not genome-root
        (throw (err/error :skill/vendor-invalid-args "vendor requires :genome/root" {:skill/name name})))
      (let [bundle (adapter/get-skill-bundle registry name)]
        (when-not bundle
          (throw (err/error :cli/skill-not-found "no skill with this name to vendor" {:skill/name name})))
        (let [tree-id (:revision/id bundle)]
          (when-not tree-id
            (throw (err/error :skill/vendor-invalid-args "bundle missing :revision/id" {:bundle bundle})))
          (vendor/vendor-skill! {:genome/root genome-root :cas cas-handle :skill/name name :tree/id tree-id}))))))

;; --- generic aliases (required function names) -------------------------------

(defn skill-list
  "Generic skill list — alias for list!."
  [opts] (list! opts))

(defn skill-inspect
  "Generic skill inspect — alias for inspect!."
  [opts] (inspect! opts))

(defn skill-validate
  "Generic skill validate — alias for validate!."
  [opts] (validate! opts))

(defn skill-vendor
  "Generic skill vendor — alias for vendor!."
  [opts] (vendor! opts))

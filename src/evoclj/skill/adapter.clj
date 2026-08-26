(ns evoclj.skill.adapter
  "AgentSkillAdapter — FileTree LiveSource + SKILL.md parser + SurfaceBundle projectors.

  Thin adapter: discovery -> snapshot whole skill dir to CAS -> parse FROM SNAPSHOT -> validate -> derive SurfaceBundle -> atomic publish.

  Must not parse live then snapshot. Parsing always reads SKILL.md bytes via CAS manifest.

  Discovery roots:
  - ~/.agents/skills (user home)
  - <project>/.agents/skills (project root, defaults to user.dir)
  - extra roots supplied via :extra-roots opts (vector of paths)

  Each skill is a directory containing SKILL.md. Name defaults to directory name when frontmatter :name missing (lenient mode).

  S12 (same-name collision): when the same skill name is discoverable from more
  than one scope (:project, :user, :extra), it resolves by the FIXED precedence
  project > user > extra — never by registration order and never by an arbitrary
  tie-break. The scope of each root is explicit (see :root-scopes below); when
  the legacy :roots / :extra-roots options are used, a root matching
  <project>/.agents/skills is :project, a root matching ~/.agents/skills is
  :user, all other :roots are :user, and :extra-roots are :extra. An OPTIONAL
  `:on-collision :error` mode turns a cross-scope same-name collision into a
  typed :skill/name-collision error (fail-closed) instead of a precedence pick;
  the default :precedence resolves deterministically. Scope and collision-mode
  validation is fail-closed (typed :skill/invalid-root-scope /
  :skill/invalid-collision-mode).

  ContextSurface progressive disclosure:
  - catalog projection -> Offer with name+description (descriptor)
  - activation -> full SKILL.md exact revision via CAS materializer
  - resource read -> generic mounted directory via mount/filesystem provider (RO)

  activate_skill is a thin facade: skill name -> current Offer -> bundle binding -> durable session binding + RO mount. Returns {:activated true ...} not full body.
  reload_skill atomic A->B for both surfaces.

  Source refresh: catalog shows B but existing binding A unchanged.
  Source removal: catalog disappears but binding A still works via CAS tree.

  S11 (lenient diagnostic): the SkillSource snapshot! payload reports skills
  that were rejected during lenient capture/parse (invalid manifest, unknown
  frontmatter key, disallowed YAML tag, missing SKILL.md, ...) under
  :rejected-skills — a list of {:skill/name <dir> :reason <typed reason>} —
  plus :skill/rejected-count. In :strict mode the first rejection still
  aborts (fail-closed); in :lenient mode rejected skills are skipped for
  publication but never silently dropped (they are observable via the report)."
  (:require [clojure.string :as str]
            [evoclj.environment.source :as src]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.surface :as surf]
            [evoclj.fs.snapshot :as snapshot]
            [evoclj.fs.walk :as walk]
            [evoclj.kernel.error :as err]
            [evoclj.skill.parser :as parser]
            [evoclj.skill.collect :as collect]
            [evoclj.skill.surface :as surface]
            [evoclj.context.offer :as offer]
            [evoclj.context.binding :as ctx-binding]
            [evoclj.store.binding :as store-binding]
            [evoclj.store.cas :as cas]
            [evoclj.support.failpoint :as fault])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files LinkOption Path Paths)
           (java.nio.file.attribute FileAttribute)))

;; ---------------------------------------------------------------------------
;; Discovery
;; ---------------------------------------------------------------------------

(defn- path-of
  [p]
  (cond
    (instance? Path p) p
    (string? p) (Paths/get p (make-array String 0))
    :else (throw (err/error :skill/invalid-path "root must be string or Path" {:path p}))))

(defn- home-skills-root
  []
  (let [home (or (System/getProperty "user.home") (System/getenv "HOME") ".")]
    (Paths/get home (into-array String [".agents" "skills"]))))

(defn- project-skills-root
  []
  (let [cwd (or (System/getProperty "user.dir") ".")]
    (Paths/get cwd (into-array String [".agents" "skills"]))))

(defn default-roots
  "Return default discovery roots: [user-home/.agents/skills project/.agents/skills]. Only those that exist are considered, but we return both for caller to filter."
  []
  [(home-skills-root) (project-skills-root)])

(defn discover-skill-dirs
  "Find skill directories under roots. Each skill is an immediate child dir containing SKILL.md.
  Returns vector of Path objects. Skips non-existent roots gracefully (lenient)."
  [roots]
  (let [roots (or roots (default-roots))]
    (reduce (fn [acc root]
              (let [^Path r (try (path-of root) (catch Exception _ nil))]
                (if (or (nil? r) (not (Files/exists r (make-array LinkOption 0))) (not (Files/isDirectory r (make-array LinkOption 0))))
                  acc
                  (let [children (try
                                   (with-open [stream (Files/list r)]
                                     (vec (iterator-seq (.iterator stream))))
                                   (catch Exception _ []))]
                    (reduce (fn [a ^Path child]
                              (if (Files/isDirectory child (make-array LinkOption 0))
                                (let [skill-md (.resolve child "SKILL.md")]
                                  (if (Files/exists skill-md (make-array LinkOption 0))
                                    (conj a child)
                                    a))
                                a))
                            acc
                            children)))))
            []
            roots)))

(defn skill-name-for-dir
  "Derive skill name from directory name (fallback)."
  [^Path dir]
  (str (.getFileName dir)))

;; ---------------------------------------------------------------------------
;; Scope assignment (S12): project > user > extra precedence
;; ---------------------------------------------------------------------------

(defn- scoped-roots-for
  "Build the vector of {:path <Path> :scope <keyword>} discovery roots from the
   source's configured roots.

   When :root-scopes is supplied (map scope -> [paths]) it is authoritative and
   order-independent: each root's scope is explicit, so reordering the map (or
   the paths within a scope) never changes which skill resolves.

   Otherwise the legacy flat :roots / :extra-roots are used and each root gets a
   deterministic scope by path identity: a root equal to the project
   .agents/skills root -> :project; every other :root -> :user; every
   :extra-root -> :extra. This is path-based (never order-based)."
  [roots extra-roots root-scopes]
  (if (seq root-scopes)
    (vec (for [[scope paths] root-scopes
               p paths]
           {:path (path-of p) :scope scope}))
    (let [roots (or roots [])
          project-roots (filter #(= (str (path-of %)) (str (project-skills-root))) roots)
          user-roots (remove #(= (str (path-of %)) (str (project-skills-root))) roots)]
      (vec (concat
            (map (fn [p] {:path (path-of p) :scope :project}) project-roots)
            (map (fn [p] {:path (path-of p) :scope :user}) user-roots)
            (map (fn [p] {:path (path-of p) :scope :extra}) (or extra-roots [])))))))

(defn- scoped-skill-dirs
  "Discover skill dirs per scoped root. `scoped-roots` is a vector of
   {:path <Path> :scope <keyword>}; returns a vector of
   {:dir <Path> :scope <keyword>}."
  [scoped-roots]
  (vec (mapcat (fn [{:keys [path scope]}]
                 (map (fn [dir] {:dir dir :scope scope})
                      (discover-skill-dirs [path])))
               scoped-roots)))

;; ---------------------------------------------------------------------------
;; Snapshot + parse FROM SNAPSHOT (must not parse live)
;; ---------------------------------------------------------------------------

(def ^:private snapshot-limits
  {:max-depth 20
   :max-files 2000
   :max-total-bytes (* 20 1024 1024)
   :max-file-bytes (* 5 1024 1024)})

(defn snapshot-skill-dir!
  "Snapshot whole skill directory to CAS. Returns {:tree/id ... :manifest ... :entries ...}."
  [^Path skill-dir cas]
  (when-not (Files/isDirectory skill-dir (make-array LinkOption 0))
    (throw (err/error :skill/invalid-path "skill dir must be existing directory" {:path (str skill-dir)})))
  (snapshot/snapshot-tree! skill-dir cas snapshot-limits))

(defn parse-skill-from-snapshot
  "Parse SKILL.md FROM SNAPSHOT manifest (not live file). Returns parsed map.
   mode :lenient or :strict."
  [cas manifest mode]
  (let [ba (snapshot/get-file-bytes cas manifest "SKILL.md")]
    (when-not ba
      (throw (err/error :skill/missing-skill-md "SKILL.md missing in snapshot manifest" {:manifest manifest})))
    (let [content (String. ^bytes ba StandardCharsets/UTF_8)]
      (parser/parse-skill-content content mode))))

(defn- rejection-reason
  "Typed, fully serializable reason for a rejected skill (S11 contract).

   Produced by the production error contract (`err/error-data`) so every
   rejected skill carries a machine-readable `:error/type` plus the error
   message/class/cause data. Never a raw Throwable, never a silently empty
   reason: the fail-closed report contract is that a rejected skill ALWAYS
   appears in `:rejected-skills` WITH a reason."
  [t]
  (err/error-data t))

;; ---------------------------------------------------------------------------
;; Bundle derivation + publish
;; ---------------------------------------------------------------------------

(defn derive-and-publish!
  "Snapshot skill-dir to CAS, parse SKILL.md from snapshot, validate, derive bundle, publish atomically.
  Returns {:skill/name ... :tree/id ... :bundle bundle :frontmatter ... :body ...}
  Throws on strict validation failure. In lenient mode, caller decides to skip or throw.

  Optional 5th arg opts may carry {:failpoints {...}} seams
  (:after-snapshot-tree / :after-parse / :after-bundle-publish) — see
  evoclj.support.failpoint. A hook throw propagates to the caller."
  ([skill-dir cas registry mode]
   (derive-and-publish! skill-dir cas registry mode nil))
  ([skill-dir cas registry mode {:as opts}]
   (let [dir-name (skill-name-for-dir skill-dir)
         {:keys [tree/id manifest] :as snap} (snapshot-skill-dir! skill-dir cas)]
     ;; T2 seam: tree snapshotted to CAS, not yet parsed
     (fault/trigger! opts :after-snapshot-tree)
     (let [parsed (parse-skill-from-snapshot cas manifest mode)]
       ;; T2 seam: SKILL.md parsed from snapshot; bundle not yet derived/published
       (fault/trigger! opts :after-parse)
       (let [fm (:frontmatter parsed)
             skill-name (or (:name fm) dir-name)
             ;; normalize frontmatter for surface: ensure name present
             fm-with-name (if (:name fm) fm (assoc fm :name skill-name))
             bundle (surface/skill->bundle {:skill/name skill-name :tree/id id :frontmatter fm-with-name :body (:body parsed) :cas cas})]
         ;; atomic publish via environment registry
         (bundle/publish-bundle! registry bundle)
         ;; T2 seam: bundle published into the registry
         (fault/trigger! opts :after-bundle-publish)
         {:skill/name skill-name :tree/id id :manifest manifest :parsed parsed :frontmatter fm-with-name :body (:body parsed) :bundle bundle :snapshot snap})))))

;; ---------------------------------------------------------------------------
;; SkillSource LiveSource
;; ---------------------------------------------------------------------------

(defrecord SkillSource [source-id roots cas registry subs closed? strict? extra-roots root-scopes collision]
  src/LiveSource
  (snapshot! [this]
    ;; PURE capture (INV-06): discover skill dirs and snapshot each to CAS +
    ;; parse SKILL.md FROM SNAPSHOT to read its raw data. This performs NO
    ;; publication, NO registry mutation, and NO counter advance. The
    ;; downstream Source -> Revision -> Projector -> Bundle transaction
    ;; (evoclj.environment.registry/refresh!) owns all publication; the
    ;; projector (project, below) turns this captured payload into bundles.
    ;; A strict-mode parse failure still throws (fail-closed) but does not
    ;; partial-publish.
    ;;
    ;; S11 (lenient diagnostic): in :lenient mode a skill whose capture/parse
    ;; throws is NOT published, but it is never silently dropped — every such
    ;; skill is reported in the snapshot payload under :rejected-skills
    ;; ({:skill/name <dir> :reason <typed reason>}) with a matching
    ;; :skill/rejected-count. :strict mode still aborts on the first rejection.
    ;; S12 (same-name collision): each discovered skill is tagged with the scope
    ;; of the root it came from, then same-name skills across scopes are
    ;; resolved deterministically (project > user > extra) — or, when
    ;; :on-collision :error, the whole snapshot fails closed with a typed
    ;; :skill/name-collision instead of picking a winner.
    (when @closed?
      (throw (err/error :skill/source-closed "SkillSource is closed" {:source/id source-id})))
    (let [mode (if strict? :strict :lenient)
          scoped-roots (scoped-roots-for roots extra-roots root-scopes)
          scoped-dirs (scoped-skill-dirs scoped-roots)
          ;; Per-skill capture + parse, tagged with the root's scope. Each entry
          ;; is either a well-formed skill (accepted) or, in lenient mode, a
          ;; rejected skill with its reason.
          {:keys [results rejected]}
          (reduce (fn [acc {:keys [dir scope]}]
                    (try
                      (let [{:keys [tree/id manifest]} (snapshot-skill-dir! dir cas)
                            parsed (parse-skill-from-snapshot cas manifest mode)
                            fm (:frontmatter parsed)
                            skill-name (or (:name fm) (skill-name-for-dir dir))]
                        (update acc :results conj {:skill/name skill-name
                                                   :scope scope
                                                   :tree/id id
                                                   :frontmatter (if (:name fm) fm (assoc fm :name skill-name))
                                                   :body (:body parsed)}))
                      (catch Exception e
                        (if strict?
                          (throw e)
                          ;; lenient: report the rejected skill + its typed
                          ;; reason, and continue. Never silently drop (S11).
                          (update acc :rejected conj {:skill/name (skill-name-for-dir dir)
                                                      :reason (rejection-reason e)})))))
                  {:results [] :rejected []}
                  scoped-dirs)
          results (vec (or results []))
          rejected (vec (or rejected []))
          ;; S12: collapse same-name across scopes by fixed precedence, or
          ;; fail closed on a cross-scope collision when :on-collision :error.
          resolved (collect/resolve-skills results collision)
          payload {:skills (into {} (map (fn [[n entry]]
                                           [n {:tree/id (:tree/id entry)
                                               :revision/id (:tree/id entry)
                                               :frontmatter (:frontmatter entry)}])
                                         resolved))
                   :skill/count (count resolved)
                   :rejected-skills rejected
                   :skill/rejected-count (count rejected)
                   :roots (mapv #(str (:path %)) scoped-roots)
                   :mode (name mode)}]
      {:source/id source-id
       :payload payload
       ;; captured-at is still recorded as a pure observation timestamp of the
       ;; capture (the snapshot value), not stamped into published state.
       :captured-at (System/currentTimeMillis)}))
  (project [this snapshot]
    ;; PURE projector (INV-06): turn the captured snapshot payload into a
    ;; vector of candidate bundle-opts maps, one per discovered skill, ready
    ;; for evoclj.environment.bundle/prepare-bundle + publish. Performs NO
    ;; mutation; throwing here is the fail-closed signal for a mid-chain
    ;; failure. Returns an empty vector when there are no skills.
    (let [skills (get-in snapshot [:payload :skills])]
      (if (empty? skills)
        []
        (mapv (fn [[skill-name {:keys [tree/id frontmatter body]}]]
                (surface/skill->bundle {:skill/name skill-name
                                        :tree/id id
                                        :frontmatter frontmatter
                                        :body body
                                        :cas cas}))
              skills))))
  (subscribe! [this invalidate-fn]
    (when @closed?
      (throw (err/error :skill/source-closed "SkillSource is closed" {:source/id source-id})))
    (let [id (random-uuid)
          close-fn (fn [] (swap! subs dissoc id))]
      (swap! subs assoc id invalidate-fn)
      {:subscription/id id :close! close-fn}))
  (close! [this]
    (when-not @closed?
      (reset! closed? true)
      (reset! subs {}))
    nil))

(defn make-skill-source
  "Create a Skill LiveSource.

  Opts:
  :source/id — keyword id, e.g. :skills/user or :skills/project (default :skills/all)
  :roots — vector of root paths (strings or Paths) to discover skills in. Defaults to [home, project].
  :extra-roots — additional roots supplied by caller (others as extra sources)
  :cas — CAS handle (required, string path or {:root ...} map)
  :registry — environment registry atom (optional, created if not supplied)
  :strict? — boolean, false = lenient external discovery, true = strict vendored/evolution compile
  :root-scopes — map scope -> [paths] making each root's precedence scope
    explicit (:project / :user / :extra). Order-independent and authoritative.
    When omitted, scopes derive from the legacy :roots / :extra-roots by path
    identity (see scoped-roots-for).
  :on-collision — :precedence (default; same-name resolves project > user >
    extra deterministically) or :error (a cross-scope same-name collision
    raises a typed :skill/name-collision and the snapshot fails closed).

  The source's snapshot! will:
   filesystem event -> mark dirty -> snapshot whole skill dir to CAS -> parse SKILL.md FROM SNAPSHOT -> validate -> derive SurfaceBundle -> atomic publish.
  It must not parse live then snapshot."
  [{:keys [source/id roots cas registry strict? extra-roots root-scopes on-collision] :as opts}]
  (when-not cas
    (throw (err/error :skill/invalid-opts "SkillSource requires :cas" {:opts opts})))
  (let [sid (or id :skills/all)
        roots (or roots (default-roots))
        registry (or registry (reg/create-registry))
        subs (atom {})
        closed? (atom false)
        strict? (boolean strict?)
        collision (collect/validate-collision-mode on-collision)
        root-scopes (collect/validate-root-scopes (or root-scopes {}))
        source (->SkillSource sid roots cas registry subs closed? strict? (or extra-roots []) root-scopes collision)]
    ;; attach registry for external access
    (assoc source :registry registry)))

(defn trigger-invalidate!
  "Test helper: simulate filesystem event that marks dirty and triggers subscribers."
  [source]
  (doseq [f (vals @(:subs source))]
    (try (f) (catch Exception _ nil)))
  nil)

;; ---------------------------------------------------------------------------
;; Catalog helpers (progressive disclosure)
;; ---------------------------------------------------------------------------

(defn- find-bundle
  "Find latest bundle for skill-name in registry via logical-index."
  [registry skill-name]
  (let [logical-id (surface/skill-name->logical-id skill-name)
        state @registry
        logical-index (:logical-index state)
        bundles (:bundles state)
        bundle-id (get logical-index logical-id)]
    (if bundle-id
      (get bundles bundle-id)
      ;; fallback: scan all bundles and pick latest by bundle id (most recent)
      (let [bundles (bundle/list-bundles registry)
            matches (filter #(= (:logical/id %) logical-id) bundles)]
        (last (sort-by #(:bundle/id %) matches))))))

(defn list-offers
  "List current ContextOffers (catalog projection: name+description only) from registry's logical-index.
  Progressive disclosure: catalog only exposes small descriptor, not full body. Returns only latest per logical-id."
  [registry]
  (let [state @registry
        logical-index (:logical-index state)
        bundles (:bundles state)]
    (if (seq logical-index)
      (mapv (fn [[logical-id bundle-id]]
              (let [b (get bundles bundle-id)
                    rev (:revision/id b)
                    bid (:bundle/id b)
                    surfaces (:surfaces b)
                    ctx (first (filter #(= :context (:surface/type %)) surfaces))
                    desc (:descriptor ctx)
                    name (or (:name desc) (second logical-id))]
                (offer/make-offer {:logical-id logical-id :revision-id rev :bundle-id bid :name (or name (str logical-id)) :description (or (:description desc) "") :descriptor (when (map? desc) (:materializer desc))}))) 
            logical-index)
      ;; fallback when logical-index empty (e.g., registry created without bundle publish): scan all bundles and deduplicate by logical-id
      (let [bundles (bundle/list-bundles registry)
            by-logical (group-by :logical/id bundles)]
        (mapv (fn [[logical-id bs]]
                (let [b (last (sort-by :bundle/id bs))
                      rev (:revision/id b)
                      bid (:bundle/id b)
                      surfaces (:surfaces b)
                      ctx (first (filter #(= :context (:surface/type %)) surfaces))
                      desc (:descriptor ctx)
                      name (or (:name desc) (second logical-id))]
                  (offer/make-offer {:logical-id logical-id :revision-id rev :bundle-id bid :name (or name (str logical-id)) :description (or (:description desc) "") :descriptor (when (map? desc) (:materializer desc))})))
              by-logical)))))

(defn current-offer-for
  "Lookup current Offer for skill-name."
  [registry skill-name]
  (let [logical-id (surface/skill-name->logical-id skill-name)
        offers (list-offers registry)
        proj (offer/catalog-projection offers)]
    (offer/current-offer proj logical-id)))

(defn get-skill-bundle
  "Get bundle for skill-name via registry."
  [registry skill-name]
  (find-bundle registry skill-name))

;; ---------------------------------------------------------------------------
;; activate_skill / reload_skill thin facades
;; ---------------------------------------------------------------------------

(defn activate-skill!
  "Thin facade: skill name -> current ContextOffer -> bundle binding -> durable session ContextBinding + durable RO directory mount.

  Returns {:activated true :binding/id uuid :revision/id sha256 :bundle/id string :logical/id vector}
  Does NOT return full SKILL.md; next round assembler materializes via CAS.

  Args:
   db — SQLite spec (for store/binding durable table)
   session-id — UUID of session (must exist with :session/created root event)
   skill-name — string name
   opts map:
     :registry — environment registry atom
     :cas — CAS handle
     :mount-registry — atom map mount-id -> mount (optional, for in-memory mounts)
     :context-store — atom from evoclj.context.binding/create-store (optional)

  Throws :skill/not-found if no bundle/offer for skill-name."
  ([db session-id skill-name] (activate-skill! db session-id skill-name {}))
  ([db session-id skill-name {:keys [registry cas mount-registry context-store]}]
   (when-not (and (string? skill-name) (not (str/blank? skill-name)))
     (throw (err/error :skill/invalid-args "skill-name must be non-empty string" {:skill/name skill-name})))
   (when-not registry
     (throw (err/error :skill/missing-registry "registry required for activate" {:skill/name skill-name})))
   (let [bundle (get-skill-bundle registry skill-name)]
     (when-not bundle
       (throw (err/error :skill/not-found "no bundle for skill" {:skill/name skill-name :logical/id (surface/skill-name->logical-id skill-name)})))
     (let [binding (store-binding/activate! db session-id bundle {:registry registry :cas cas :mount-registry mount-registry :context-store context-store})]
       {:activated true
        :binding/id (:binding/id binding)
        :revision/id (:revision/id binding)
        :bundle/id (:bundle/id binding)
        :logical/id (:logical/id binding)
        :binding binding}))))

(defn reload-skill!
  "Atomic A->B reload for both surfaces in one transaction.

  Uses store/binding/reload! which updates durable row and runtime state atomically.
  Returns updated binding map.

  Args same as activate-skill! but logically does A->B."
  ([db session-id skill-name] (reload-skill! db session-id skill-name {}))
  ([db session-id skill-name {:keys [registry cas mount-registry context-store]}]
   (when-not (and (string? skill-name) (not (str/blank? skill-name)))
     (throw (err/error :skill/invalid-args "skill-name must be non-empty string" {:skill/name skill-name})))
   (when-not registry
     (throw (err/error :skill/missing-registry "registry required for reload" {:skill/name skill-name})))
   (let [logical-id (surface/skill-name->logical-id skill-name)
         bundle (get-skill-bundle registry skill-name)]
     (when-not bundle
       (throw (err/error :skill/not-found "no bundle for skill to reload" {:skill/name skill-name})))
     (let [binding (store-binding/reload! db session-id logical-id bundle {:registry registry :cas cas :mount-registry mount-registry :context-store context-store})]
       {:reloaded true
        :binding/id (:binding/id binding)
        :revision/id (:revision/id binding)
        :bundle/id (:bundle/id binding)
        :logical/id (:logical/id binding)
        :binding binding}))))

(defn deactivate-skill!
  "Deactivate skill binding for session."
  ([db session-id skill-name] (deactivate-skill! db session-id skill-name {}))
  ([db session-id skill-name {:keys [mount-registry context-store]}]
   (let [logical-id (surface/skill-name->logical-id skill-name)]
     (store-binding/deactivate! db session-id logical-id {:mount-registry mount-registry :context-store context-store}))))

;; ---------------------------------------------------------------------------
;; Helpers for tests: refresh flow simulation
;; ---------------------------------------------------------------------------

(defn refresh-skills!
  "Explicit refresh: snapshot all discovered skill dirs -> parse from snapshot -> publish bundles.
  Returns {:status :published/:noop :results [...] }.

  This is the normative refresh flow:
  filesystem event -> mark dirty -> snapshot whole skill dir to CAS -> parse FROM SNAPSHOT -> validate -> derive SurfaceBundle -> atomic publish.
  Must not parse live then snapshot."
  [source]
  (when-not (satisfies? src/LiveSource source)
    (throw (err/error :skill/invalid-source "source must satisfy LiveSource" {:source source})))
  (let [snap (src/snapshot! source)]
    {:status :published :snapshot snap :results (:skill/results snap)}))

(defn catalog-snapshot
  "Return catalog projection map logical-id -> offer for current registry state."
  [registry]
  (offer/catalog-projection (list-offers registry)))
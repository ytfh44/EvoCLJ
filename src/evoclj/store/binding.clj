(ns evoclj.store.binding
  "Durable session bindings for the dynamic environment.

  A session binding pins a logical identifier (e.g. [:skill \"debugging\"])
  to an exact immutable bundle revision for the lifetime of the session,
  until an explicit reload moves it to a new revision. Bindings are
  runtime environment — they never touch PhenotypeID or Resolution
  (stored in generations/sessions) and installing an unused Skill does
  not change PhenotypeID.

  Refresh vs reload (normative):
  - refresh changes the registry's current revision (the catalog
    projection) but leaves session_bindings untouched; an active session
    stays at revision A.
  - reload explicitly moves an active binding A -> B, updates the
    durable row (revision_id, bundle_id) and appends an auditable
    event. Only reload writes session_bindings.

  Storage:
  - SQLite table session_bindings (006-session-bindings.sql) via
    clojure.java.jdbc + evoclj.store.sqlite
  - revision_id is a CAS key (\"sha256:<64 hex>\"); lookup is via CAS,
    never via the current catalog, so a binding survives source removal
    and process restarts (the immutable tree A still exists in CAS).
  - Sibling surfaces (Context, Directory, Tools) in one bundle are
    co-versioned (same revision_id) and published atomically; restoring
    a binding restores all siblings at the same revision.

  Runtime publishing:
  - activate!/reload! validate the bundle exists (registry or CAS),
    validate every sibling surface via evoclj.environment.surface,
    insert/update the durable row, publish mount/context state into
    the supplied in-memory registries (if any), and append an event.
  - active-bindings reads only the durable table (CAS fallback, not
    catalog) so it works after source deletion or a restart.

  Phenotype invariant: this namespace never reads or writes
  generations.genome_id, generations.resolution_id or sessions.phenotype_id
  except to fetch the session's pinned generation/phenotype for the
  event's causal chain; it never mutates them."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.environment.surface :as surf]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.time.format DateTimeFormatter)
           (java.util Date UUID)))

;; ---------------------------------------------------------------------------
;; Timestamp helper (copied from session.clj for consistency)
;; ---------------------------------------------------------------------------

(def ^:private timestamp-fmt DateTimeFormatter/ISO_INSTANT)

(defn- canonical-timestamp
  [ts]
  (let [inst (cond
               (nil? ts) (Instant/now)
               (instance? Instant ts) ts
               (instance? Date ts) (.toInstant ^Date ts)
               (string? ts) (Instant/parse ts)
               :else (throw (err/error :store/binding-invalid
                                       "timestamp must be an inst, Instant, or ISO-8601 string"
                                       {:timestamp ts})))]
    (.format timestamp-fmt inst)))

;; ---------------------------------------------------------------------------
;; Row mapping
;; ---------------------------------------------------------------------------

(defn- logical->text
  "Serialize logical-id vector to TEXT for storage."
  [logical-id]
  (pr-str logical-id))

(defn- text->logical
  "Deserialize logical-id TEXT back to vector."
  [s]
  (try
    (edn/read-string s)
    (catch Exception _
      (throw (err/error :store/binding-invalid "logical_id is not readable EDN" {:logical-id s})))))

(defn- row->binding
  "Convert a session_bindings DB row to a public binding map."
  [row]
  {:binding/id (UUID/fromString (:id row))
   :session/id (UUID/fromString (:session_id row))
   :binding/type (:binding_type row)
   :logical/id (text->logical (:logical_id row))
   :revision/id (:revision_id row)
   :bundle/id (:bundle_id row)
   :state (keyword (:state row))
   :activated-at (Date/from (Instant/parse (:activated_at row)))
   :deactivated-at (when (:deactivated_at row)
                     (Date/from (Instant/parse (:deactivated_at row))))
   :metadata (when (:metadata_edn row)
               (try (edn/read-string (:metadata_edn row))
                    (catch Exception _ {})))})

(defn- binding-type-for
  "Derive binding_type string from logical-id or bundle."
  [logical-id bundle]
  (cond
    (and (vector? logical-id) (keyword? (first logical-id)))
    (name (first logical-id))
    (and (vector? logical-id) (string? (first logical-id)))
    (first logical-id)
    (:binding/type bundle) (str (:binding/type bundle))
    :else "skill"))

(defn- bundle->logical
  "Extract logical-id vector from a bundle or offer map."
  [bundle]
  (or (:logical/id bundle)
      (:offer/logical-id bundle)
      (:logical-id bundle)
      (throw (err/error :store/binding-invalid "bundle/offer missing logical id" {:bundle bundle}))))

(defn- bundle->revision
  [bundle]
  (or (:revision/id bundle)
      (:offer/revision-id bundle)
      (:revision-id bundle)
      (throw (err/error :store/binding-invalid "bundle/offer missing revision_id" {:bundle bundle}))))

(defn- bundle->bundle-id
  [bundle]
  (or (:bundle/id bundle)
      (:offer/bundle-id bundle)
      (:bundle-id bundle)
      (throw (err/error :store/binding-invalid "bundle/offer missing bundle_id" {:bundle bundle}))))

(defn- bundle->surfaces
  [bundle]
  (or (:surfaces bundle) []))

(defn- sanitize-surface
  "Return an EDN-safe surface (strip non-serializable fns like :materializer).
  Keeps only keys that round-trip via pr-str/edn/read-string.
  Backend records are converted to plain serializable descriptors."
  [s]
  (let [a (dissoc s :materializer)
        b (if-let [backend (:backend a)]
            (let [tree-id (or (:tree-id backend) (:tree/id backend) (:revision/id a))
                  plain (cond
                          (and (map? backend) (:tree/id backend)) backend
                          (instance? clojure.lang.IRecord backend) {:type :cas-tree :tree/id (or (:tree-id backend) (:revision/id a))}
                          :else {:type :cas-tree :tree/id (or tree-id (:revision/id a))})]
              (assoc a :backend plain))
            a)]
    (try
      (edn/read-string (pr-str b))
      b
      (catch Exception _
        (select-keys b [:surface/id :surface/type :revision/id :bundle/id :logical/id :descriptor :entries :access/max :backend])))))

(defn- bundle->metadata
  "Build metadata_edn value for storage — keeps sanitized surfaces for restore."
  [bundle]
  (let [surfaces (bundle->surfaces bundle)
        safe-surfaces (mapv sanitize-surface surfaces)]
    (pr-str {:bundle/id (bundle->bundle-id bundle)
             :logical/id (bundle->logical bundle)
             :revision/id (bundle->revision bundle)
             :surfaces safe-surfaces
             :bundle {:bundle/id (bundle->bundle-id bundle)
                      :revision/id (bundle->revision bundle)
                      :logical/id (bundle->logical bundle)
                      :surfaces safe-surfaces}})))

;; ---------------------------------------------------------------------------
;; Helpers: session + event cause
;; ---------------------------------------------------------------------------

(defn- fetch-session
  "Return {:generation_id :phenotype_id} for session-id, or throw."
  [db session-id]
  (let [sid (str (types/session-id session-id))
        row (first (sqlite/query db ["SELECT generation_id, phenotype_id FROM sessions WHERE id = ?" sid]))]
    (when-not row
      (throw (err/error :store/session-not-found "no session with this id" {:session/id (types/session-id session-id)})))
    row))

(defn- latest-cause
  "Return the latest event id for session, or nil if none (caller must handle root)."
  [db session-id]
  (let [sid (str (types/session-id session-id))
        row (first (sqlite/query db ["SELECT id FROM events WHERE session_id = ? ORDER BY event_seq DESC LIMIT 1" sid]))]
    (:id row)))

(defn- append-binding-event!
  "Append an auditable binding event (:binding/activated, :binding/reloaded, :binding/deactivated).
  Uses the session's pinned generation/phenotype and chains to the latest event."
  [db session-id event-type metadata]
  (let [{:keys [generation_id phenotype_id]} (fetch-session db session-id)
        cause (latest-cause db session-id)
        req {:session/id (types/session-id session-id)
             :generation/id generation_id
             :phenotype/id phenotype_id
             :event/type event-type
             :cause/event-id cause
             :payload-ref nil
             :metadata (or metadata {})}]
    (if (nil? cause)
      (throw (err/error :store/binding-invalid
                        "session has no events; expected :session/created root before binding activation"
                        {:session/id (types/session-id session-id) :event/type event-type}))
      (event/append-event! db req))))

;; ---------------------------------------------------------------------------
;; Validation helpers
;; ---------------------------------------------------------------------------

(defn- validate-bundle-exists
  "Validate bundle exists via registry or CAS. If registry supplied,
  look up bundle; if cas supplied, verify revision exists in CAS.
  If neither supplied, just validate structure."
  [bundle registry cas]
  (let [bid (bundle->bundle-id bundle)
        rev (bundle->revision bundle)]
    (when-not (types/artifact-id? rev)
      (throw (err/error :store/binding-invalid "revision_id must be sha256:<64 hex>" {:revision/id rev})))
    (when-not (and (string? bid) (seq bid))
      (throw (err/error :store/binding-invalid "bundle_id must be non-empty string" {:bundle/id bid})))
    (when registry
      (let [found (try
                    (let [f (requiring-resolve 'evoclj.environment.bundle/get-bundle)]
                      (when f (f registry bid)))
                    (catch Exception _ nil))]
        (when (and (not found) (nil? cas))
          (let [has? (try
                       (contains? (or (:bundles @registry) {}) bid)
                       (catch Exception _ false))]
            (when-not has?
              nil)))))
    (when cas
      (try
        (let [exists? (try (cas/exists? cas rev) (catch Exception _ false))]
          (when-not exists? nil))
        (catch Exception _ nil)))
    bundle))

(defn- validate-sibling-surfaces
  "Validate every sibling surface in bundle is bindable (descriptor valid,
  access/max valid, co-versioned). Throws on failure."
  [bundle]
  (let [surfaces (bundle->surfaces bundle)
        rev (bundle->revision bundle)]
    (doseq [s surfaces]
      (surf/validate-surface s))
    (when (seq surfaces)
      (let [revs (set (map :revision/id surfaces))]
        (when (> (count revs) 1)
          (throw (err/error :bundle/co-version-violation
                            "sibling surfaces must share same revision_id"
                            {:revision-ids revs :bundle bundle})))
        (when (and (= 1 (count revs)) (not= (first revs) rev))
          (throw (err/error :bundle/co-version-violation
                            "bundle revision_id must match surfaces revision_id"
                            {:bundle-rev rev :surface-revs revs})))))
    bundle))

;; ---------------------------------------------------------------------------
;; Runtime publishing helpers
;; ---------------------------------------------------------------------------

(defn- publish-runtime!
  "Publish bundle's surfaces into mount-registry and context-store.
  Both are optional atoms. No-op if nil."
  [bundle {:keys [mount-registry context-store cas] :as opts}]
  (let [surfaces (bundle->surfaces bundle)
        logical-id (bundle->logical bundle)
        rev (bundle->revision bundle)
        bid (bundle->bundle-id bundle)
        cas-handle cas]
    (when context-store
      (doseq [s surfaces
              :when (= :context (:surface/type s))]
        (try
          (let [offer {:offer/logical-id logical-id
                       :offer/revision-id rev
                       :offer/bundle-id bid
                       :offer/name (str logical-id)
                       :offer/description (str "binding " logical-id)}]
            (let [f (requiring-resolve 'evoclj.context.binding/activate!)]
              (when f (f context-store offer))))
          (catch Exception _ nil))))
    (when mount-registry
      (doseq [s surfaces
              :when (= :directory (:surface/type s))]
        (try
          (let [mount-id (or (:surface/id s) logical-id)]
            (let [existing (try (let [f (requiring-resolve 'evoclj.mount.backend/get-mount)]
                                  (when f (f mount-registry mount-id)))
                                (catch Exception _ nil))]
              (when-not existing
                (let [raw (:backend s)
                      backend (cond
                                (and raw
                                     (try
                                       (let [proto @(requiring-resolve 'evoclj.mount.backend/Backend)]
                                         (satisfies? proto raw))
                                       (catch Exception _ false))) raw
                                (and raw (map? raw) (:tree/id raw) cas-handle)
                                (try
                                  (let [f (requiring-resolve 'evoclj.mount.backend/cas-tree-backend)]
                                    (when f (f cas-handle (:tree/id raw))))
                                  (catch Exception _ raw))
                                (and raw (map? raw) (:tree-id raw) cas-handle)
                                (try
                                  (let [f (requiring-resolve 'evoclj.mount.backend/cas-tree-backend)]
                                    (when f (f cas-handle (:tree-id raw))))
                                  (catch Exception _ raw))
                                cas-handle
                                (try
                                  (let [f (requiring-resolve 'evoclj.mount.backend/cas-tree-backend)]
                                    (when f (f cas-handle rev)))
                                  (catch Exception _ nil))
                                :else raw)
                      backend (or backend {:type :cas-tree :tree/id rev :bundle/id bid})
                      mount (try
                              (let [mk (requiring-resolve 'evoclj.mount.backend/make-mount)]
                                (if mk
                                  (mk {:mount-id mount-id :backend backend :access-max (:access/max s)})
                                  {:mount/id mount-id :backend backend :access/max (:access/max s)}))
                              (catch Exception _ {:mount/id mount-id :backend backend :access/max (:access/max s)}))]
                  (swap! mount-registry assoc mount-id mount)))))
          (catch Exception _ nil))))
    nil))

(defn- unpublish-runtime!
  "Remove binding's runtime state from mount-registry and context-store.
  If surfaces-or-ids is provided, remove those specific mount ids; otherwise
  remove by logical-id."
  ([logical-id opts] (unpublish-runtime! logical-id opts nil))
  ([logical-id {:keys [mount-registry context-store]} surfaces-or-ids]
   (when context-store
     (try
       (let [f (requiring-resolve 'evoclj.context.binding/deactivate!)]
         (when f (f context-store logical-id)))
       (catch Exception _ nil)))
   (when mount-registry
     (try
       (if (seq surfaces-or-ids)
         (let [ids (set (map #(or (:surface/id %) %) surfaces-or-ids))]
           (swap! mount-registry
                  (fn [m]
                    (into {} (remove (fn [[k _]] (contains? ids k)) m)))))
         (swap! mount-registry
                (fn [m]
                  (into {} (remove (fn [[k _]] (= k logical-id)) m)))))
       (catch Exception _ nil)))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn active-bindings
  "Return active bindings for session-id as a vector of binding maps.
  Reads only the durable table (CAS-surviving, not catalog)."
  [db session-id]
  (let [sid (str (types/session-id session-id))
        rows (sqlite/query db ["SELECT * FROM session_bindings WHERE session_id = ? AND state = 'active' ORDER BY activated_at ASC" sid])]
    (mapv row->binding rows)))

(defn list-active-bindings
  "Alias for active-bindings."
  [db session-id]
  (active-bindings db session-id))

(defn get-binding
  "Return the active binding for session-id + logical-id, or nil."
  [db session-id logical-id]
  (let [sid (str (types/session-id session-id))
        lid (logical->text logical-id)
        row (first (sqlite/query db ["SELECT * FROM session_bindings WHERE session_id = ? AND logical_id = ? AND state = 'active'" sid lid]))]
    (when row (row->binding row))))

(defn- insert-binding-row!
  "Insert a new active binding row; throws on unique violation (already active)."
  [db session-id logical-id rev bid binding-type metadata-edn]
  (let [id (str (random-uuid))
        sid (str (types/session-id session-id))
        lid (logical->text logical-id)
        now (canonical-timestamp nil)]
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :session_bindings
                    {:id id
                     :session_id sid
                     :binding_type binding-type
                     :logical_id lid
                     :revision_id rev
                     :bundle_id bid
                     :state "active"
                     :activated_at now
                     :deactivated_at nil
                     :metadata_edn metadata-edn}))
    id))

(defn activate!
  "Activate a bundle for session-id.

  Validates bundle existence (registry or CAS) and that all sibling
  surfaces are bindable, creates a durable row in session_bindings,
  publishes runtime mount/context state, and appends an auditable event.

  Signatures:
    (activate! db session-id bundle)
    (activate! db session-id bundle opts)
  where bundle is a SurfaceBundle map or an Offer map, and opts is an
  optional map with keys:
    :registry       — environment registry atom (for bundle existence check)
    :cas            — CAS handle (string/path or {:root ...}) for CAS check
    :mount-registry — atom map mount-id -> mount
    :context-store  — atom from evoclj.context.binding/create-store

  Returns the persisted binding map (as from active-bindings).
  Throws :store/binding-invalid, :bundle/co-version-violation,
  :store/session-not-found, or :store/binding-already-active (unique violation)."
  ([db session-id bundle]
   (activate! db session-id bundle {}))
  ([db session-id bundle opts]
   (when-not bundle
     (throw (err/error :store/binding-invalid "bundle/offer required" {:bundle bundle})))
   (let [opts (if (and (map? opts) (not (contains? opts :registry)) (not (contains? opts :cas))
                       (not (contains? opts :mount-registry)) (not (contains? opts :context-store)))
                (if (instance? clojure.lang.Atom opts) {:registry opts} {})
                opts)
         registry (:registry opts)
         cas-handle (:cas opts)
         mount-registry (:mount-registry opts)
         context-store (:context-store opts)
         bundle (if (and (map? bundle) (contains? bundle :bundle))
                  (:bundle bundle)
                  bundle)
         logical-id (bundle->logical bundle)
         rev (bundle->revision bundle)
         bid (bundle->bundle-id bundle)
         binding-type (binding-type-for logical-id bundle)
         metadata-edn (bundle->metadata bundle)]
     (fetch-session db session-id)
     (validate-bundle-exists bundle registry cas-handle)
     (validate-sibling-surfaces bundle)
     (let [id (try
                (insert-binding-row! db session-id logical-id rev bid binding-type metadata-edn)
                (catch java.sql.SQLException e
                  (let [msg (.getMessage e)]
                    (if (or (str/includes? (str msg) "UNIQUE") (str/includes? (str msg) "unique"))
                      (throw (err/error :store/binding-already-active
                                        "binding already active for this session + logical_id"
                                        {:session/id (types/session-id session-id) :logical/id logical-id}))
                      (throw e))))
                (catch clojure.lang.ExceptionInfo e
                  (throw e))
                (catch Exception e
                  (let [msg (str (ex-message e) " " (ex-data e))]
                    (if (str/includes? msg "UNIQUE")
                      (throw (err/error :store/binding-already-active
                                        "binding already active for this session + logical_id"
                                        {:session/id (types/session-id session-id) :logical/id logical-id}))
                      (throw e)))))]
       (publish-runtime! bundle {:mount-registry mount-registry :context-store context-store :cas cas-handle})
       (try
         (append-binding-event! db session-id :binding/activated
                                {:logical/id logical-id :revision/id rev :bundle/id bid :binding/id id})
         (catch Exception e
           (throw e)))
       (get-binding db session-id logical-id)))))

(defn reload!
  "Reload an active binding to a new revision (A -> B).

  Updates the durable row's revision_id, bundle_id and appends an
  auditable event. Only reload writes session_bindings; refresh never does.

  Signatures:
    (reload! db session-id logical-id new-bundle)
    (reload! db session-id logical-id new-bundle opts)
  where new-bundle is a SurfaceBundle/Offer for the same logical-id at a
  new revision, and opts supports :registry :cas :mount-registry :context-store.

  Returns the updated binding map.
  Throws :store/binding-not-found if no active binding for logical-id."
  ([db session-id logical-id new-bundle]
   (reload! db session-id logical-id new-bundle {}))
  ([db session-id logical-id new-bundle opts]
   (when-not new-bundle
     (throw (err/error :store/binding-invalid "new bundle/offer required" {:new-bundle new-bundle})))
   (let [opts (if (instance? clojure.lang.Atom opts) {:registry opts} opts)
         registry (:registry opts)
         cas-handle (:cas opts)
         mount-registry (:mount-registry opts)
         context-store (:context-store opts)
         new-bundle (if (and (map? new-bundle) (contains? new-bundle :bundle))
                      (:bundle new-bundle) new-bundle)
         new-rev (bundle->revision new-bundle)
         new-bid (bundle->bundle-id new-bundle)
         target-logical (try (bundle->logical new-bundle) (catch Exception _ logical-id))
         sid (str (types/session-id session-id))
         lid (logical->text logical-id)
         now (canonical-timestamp nil)]
     (when (and target-logical (not= target-logical logical-id))
       (throw (err/error :store/binding-invalid "new bundle logical_id must match requested logical_id"
                         {:requested logical-id :new-logical target-logical})))
     (fetch-session db session-id)
     ;; fetch old binding metadata to know old mount ids to remove
     (let [old-binding (get-binding db session-id logical-id)
           old-surfaces (when old-binding (-> old-binding :metadata :surfaces))]
       (validate-bundle-exists new-bundle registry cas-handle)
       (validate-sibling-surfaces new-bundle)
       (let [cnt (sqlite/with-db [conn db]
                   (first (jdbc/execute! conn
                                         ["UPDATE session_bindings SET revision_id = ?, bundle_id = ?, activated_at = ?, metadata_edn = ? WHERE session_id = ? AND logical_id = ? AND state = 'active'"
                                          new-rev new-bid now (bundle->metadata new-bundle) sid lid])))]
         (when-not (= 1 cnt)
           (throw (err/error :store/binding-not-found "no active binding for this session + logical_id"
                             {:session/id (types/session-id session-id) :logical/id logical-id})))
         (unpublish-runtime! logical-id {:mount-registry mount-registry :context-store context-store} old-surfaces)
         (publish-runtime! new-bundle {:mount-registry mount-registry :context-store context-store :cas cas-handle})
         (append-binding-event! db session-id :binding/reloaded
                                {:logical/id logical-id :from-revision (:revision/id old-binding) :to-revision new-rev :bundle/id new-bid})
         (get-binding db session-id logical-id))))))

(defn deactivate!
  "Deactivate the active binding for session-id + logical-id.

  Sets state to 'inactive', stamps deactivated_at, removes runtime
  state, and appends an event.

  Signatures:
    (deactivate! db session-id logical-id)
    (deactivate! db session-id logical-id opts)
  where opts may contain :mount-registry :context-store.

  Returns the deactivated binding (now inactive) or throws :store/binding-not-found."
  ([db session-id logical-id]
   (deactivate! db session-id logical-id {}))
  ([db session-id logical-id opts]
   (let [opts (if (instance? clojure.lang.Atom opts) {:mount-registry opts} opts)
         mount-registry (:mount-registry opts)
         context-store (:context-store opts)
         sid (str (types/session-id session-id))
         lid (logical->text logical-id)
         now (canonical-timestamp nil)
         ;; fetch before update to know mount ids to clean
         old-binding (get-binding db session-id logical-id)
         old-surfaces (when old-binding (-> old-binding :metadata :surfaces))
         cnt (sqlite/with-db [conn db]
               (first (jdbc/execute! conn
                                     ["UPDATE session_bindings SET state = 'inactive', deactivated_at = ? WHERE session_id = ? AND logical_id = ? AND state = 'active'"
                                      now sid lid])))]
     (when-not (= 1 cnt)
       (throw (err/error :store/binding-not-found "no active binding to deactivate"
                         {:session/id (types/session-id session-id) :logical/id logical-id})))
     (unpublish-runtime! logical-id {:mount-registry mount-registry :context-store context-store} old-surfaces)
     (append-binding-event! db session-id :binding/deactivated
                            {:logical/id logical-id})
     (let [row (first (sqlite/query db ["SELECT * FROM session_bindings WHERE session_id = ? AND logical_id = ? ORDER BY activated_at DESC LIMIT 1" sid lid]))]
       (when row (row->binding row))))))

(defn restore!
  "Restore active bindings for session-id into runtime registries.

  Reads durable rows (via CAS, not catalog) and republishes each
  bundle's sibling surfaces into mount-registry and context-store.
  This is the restart path: a new process creates fresh registries and
  reloads them from DB even if the original source was deleted.

  Args:
    db          — SQLite spec/path
    session-id  — UUID
    opts map    — {:cas :mount-registry :context-store :registry}

  Returns the restored bindings (vector)."
  [db session-id opts]
  (let [bindings (active-bindings db session-id)]
    (doseq [b bindings]
      (let [meta (:metadata b)
            bundle (or (:bundle meta) {:bundle/id (:bundle/id b)
                                       :revision/id (:revision/id b)
                                       :logical/id (:logical/id b)
                                       :surfaces (:surfaces meta [])})]
        (publish-runtime! bundle opts)))
    bindings))

(defn restore-active-bindings!
  "Alias for restore!."
  [db session-id opts]
  (restore! db session-id opts))

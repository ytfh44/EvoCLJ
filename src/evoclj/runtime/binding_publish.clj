(ns evoclj.runtime.binding-publish
  "Runtime publication of a binding's surfaces (component).

  This namespace owns the RUNTIME half of session bindings: publishing a
  bundle's Context surfaces into a context-store and its Directory
  surfaces into a mount-registry, removing them again, and the
  pre-state capture / compensation the WO-B1 two-phase transaction needs
  to undo exactly its own deltas.

  Why it lives here and not in evoclj.store.binding: the store layer
  owns the DURABLE half (the session_bindings rows and the two-phase
  orchestration). Publication touches the runtime registries only, so
  the store must not require the context / environment / mount layers —
  it receives these four operations as injected functions
  (:publish-fn :unpublish-fn :capture-fn :compensate-fn; see
  `publisher`). The mount-id formula and the bundle accessors stay with
  the binding data model in evoclj.store.binding (ONE formula, INV-05)
  and are consumed from there.

  Collaborators (evoclj.context.binding, evoclj.mount.backend) are
  referenced statically via top-level requires (B2: resolution proven
  acyclic) — no runtime symbol lookup.

  WO-B1: the per-surface catch-and-continue swallows are GONE. A failing
  publication throws typed :store/binding-publish-failed naming the
  phase; the caller's staged transaction compensates back to the
  pre-activation state."
  (:require [evoclj.context.binding :as context-binding]
            [evoclj.kernel.error :as err]
            [evoclj.mount.backend :as mount-backend]
            [evoclj.store.binding :as binding-store]))

;; ---------------------------------------------------------------------------
;; Typed failures
;; ---------------------------------------------------------------------------

(defn- publish-failure
  "The typed publication failure (WO-B1): a stable :error/type, the
  failing phase (:context | :directory | :unpublish), the offending
  surface (sanitized), and the fully sanitized original error. Never
  thrown-and-continued away by this namespace."
  [phase surface ^Exception cause]
  (err/error :store/binding-publish-failed
             (str "binding runtime publication failed during " (name phase) " publication")
             {:phase phase
              :surface (err/sanitize surface)
              :error/original (err/error-data cause)}))

;; ---------------------------------------------------------------------------
;; Publication
;; ---------------------------------------------------------------------------

(defn publish-runtime!
  "Publish bundle's surfaces into mount-registry and context-store.
   Both are optional atoms. No-op if nil.

   Collaborators (evoclj.context.binding, evoclj.mount.backend) are
   referenced statically via top-level requires (B2: resolution proven
   acyclic) — no runtime symbol lookup.

   WO-B1: the per-surface catch-and-continue swallows are GONE. A
   failing publication throws typed :store/binding-publish-failed
   naming the phase; the caller's staged transaction compensates back
   to the pre-activation state."
  [bundle {:keys [mount-registry context-store cas] :as opts}]
  (let [surfaces (binding-store/bundle->surfaces bundle)
        logical-id (binding-store/bundle->logical bundle)
        rev (binding-store/bundle->revision bundle)
        bid (binding-store/bundle->bundle-id bundle)
        cas-handle cas]
    (when context-store
      (doseq [s surfaces
              :when (= :context (:surface/type s))]
        ;; WO-B1: no catch-and-continue. A failing publication surfaces
        ;; typed; the caller's staged transaction compensates.
        (let [desc (:descriptor s)
              mat (when (map? desc) (:materializer desc))
              offer (cond-> {:offer/logical-id logical-id
                             :offer/revision-id rev
                             :offer/bundle-id bid
                             :offer/name (str logical-id)
                             :offer/description (str "binding " logical-id)}
                      ;; WO-S1: carry the materializer descriptor (e.g.
                      ;; {:type :cas-tree-file :path \"SKILL.md\"}) so a
                      ;; binding created from it routes tree->file correctly.
                      (some? mat) (assoc :offer/descriptor mat))]
          (try
            (context-binding/activate! context-store offer)
            (catch Exception e
              (throw (publish-failure :context s e)))))))
    (when mount-registry
      (doseq [s surfaces
              :when (= :directory (:surface/type s))]
        ;; WO-B1: the OUTER boundary is typed. WO-B3: the mount-id is a
        ;; canonical vector (directory-mount-id — logical-id + revision,
        ;; never a bare scalar :surface/id) and registration goes through
        ;; the single canonical register-mount! — no ad-hoc
        ;; (swap! assoc) mutation of the registry (INV-05). The backend
        ;; realization fallbacks below stay: a real Backend is preferred,
        ;; else a plain descriptor mount is built (register-mount!
        ;; accepts a descriptor backend) and the filesystem provider
        ;; fails-closed at operation time.
        (try
          (let [mount-id (binding-store/directory-mount-id logical-id s)]
            (when-not (mount-backend/get-mount mount-registry mount-id)
              (let [raw (:backend s)
                    backend (cond
                              (and raw
                                   (try
                                     (satisfies? mount-backend/Backend raw)
                                     (catch Exception _ false))) raw
                              (and raw (map? raw) (:tree/id raw) cas-handle)
                              (try
                                (mount-backend/cas-tree-backend cas-handle (:tree/id raw))
                                (catch Exception _ raw))
                              (and raw (map? raw) (:tree-id raw) cas-handle)
                              (try
                                (mount-backend/cas-tree-backend cas-handle (:tree-id raw))
                                (catch Exception _ raw))
                              cas-handle
                              (try
                                (mount-backend/cas-tree-backend cas-handle rev)
                                (catch Exception _ nil))
                              :else raw)
                    backend (or backend {:type :cas-tree :tree/id rev :bundle/id bid})
                    mount (mount-backend/make-mount {:mount-id mount-id :backend backend :access-max (:access/max s)})]
                (mount-backend/register-mount! mount-registry mount))))
          (catch Exception e
            (throw (publish-failure :directory s e))))))
    nil))

(defn unpublish-runtime!
  "Remove binding's runtime state from mount-registry and context-store.
  If surfaces-or-ids is provided, remove those specific mount ids; otherwise
  remove by logical-id.

  WO-B1: failures here are typed (:store/binding-publish-failed with
  :phase :unpublish) — never caught-and-continued."
  ([logical-id opts] (unpublish-runtime! logical-id opts nil))
  ([logical-id {:keys [mount-registry context-store]} surfaces-or-ids]
   (when context-store
     (try
       (context-binding/deactivate! context-store logical-id)
       (catch Exception e
         (throw (publish-failure :unpublish {:surface/type :context :logical/id logical-id} e)))))
   (when mount-registry
     (try
       (if (seq surfaces-or-ids)
         (let [ids (set (map #(binding-store/mount-key-for logical-id %) surfaces-or-ids))]
           (swap! mount-registry
                  (fn [m]
                    (into {} (remove (fn [[k _]] (contains? ids k)) m)))))
         (swap! mount-registry
                (fn [m]
                  (into {} (remove (fn [[k _]] (= k logical-id)) m)))))
       (catch Exception e
         (throw (publish-failure :unpublish {:surface/type :directory :logical/id logical-id} e)))))))

;; ---------------------------------------------------------------------------
;; Pre-state capture + compensation (the runtime half of WO-B1)
;; ---------------------------------------------------------------------------

(defn- read-prestate!
  "Best-effort pre-state read. A store we cannot even READ is marked
  ::unreadable — compensation will SKIP it (we never claim to restore
  state we could not observe) while publication against it will still
  fail typed."
  [f]
  (try (f) (catch Exception _ ::unreadable)))

(defn capture-runtime-prestate
  "Snapshot the runtime state a staged transaction may mutate: the
  context binding currently pinned under logical-id and the full mount
  registry image. Small atoms; the snapshot is what makes rollback
  byte-comparable. Unreadable stores are marked ::unreadable rather
  than exploding here — they fail typed at publication time instead."
  [logical-id {:keys [mount-registry context-store]}]
  {:ctx-prev (when context-store (read-prestate! #(context-binding/get-binding context-store logical-id)))
   :mounts (if mount-registry (read-prestate! #(clojure.core/deref mount-registry)) {})})

(defn compensate-runtime!
  "Undo EXACTLY this transaction's runtime deltas: every touched mount
  key returns to its pre-transaction value (present -> restored, absent
  -> removed); the context binding under logical-id returns to its
  pre-transaction value. swap!-based, so concurrent activations touching
  OTHER keys are untouched (last-writer-wins only on the same key).

  Returns a status map {:mounts ... :context ...} where each entry is
  :ok | :skipped-unreadable | :failed — callers decide whether a
  :failed constitutes an unrunnable rollback."
  [{:keys [prestate added-mount-ids removed-mount-ids opts logical-id]}]
  (let [{:keys [mounts ctx-prev]} prestate
        touched (into added-mount-ids removed-mount-ids)
        mounts-status
        (cond
          (= ::unreadable mounts) :skipped-unreadable
          (not (:mount-registry opts)) :ok
          :else
          (try
            (swap! (:mount-registry opts)
                   (fn [m]
                     (reduce (fn [acc k]
                               (if (contains? mounts k)
                                 (assoc acc k (get mounts k))
                                 (dissoc acc k)))
                             m touched)))
            :ok
            (catch Exception _ :failed)))
        context-status
        (cond
          (= ::unreadable ctx-prev) :skipped-unreadable
          (not (:context-store opts)) :ok
          :else
          (try
            (swap! (:context-store opts)
                   (fn [st]
                     (let [installed (get-in st [:by-logical logical-id])]
                       (cond-> (if ctx-prev
                                 (assoc-in st [:by-logical logical-id] ctx-prev)
                                 (update st :by-logical dissoc logical-id))
                         ctx-prev (assoc-in [:by-id (:binding/id ctx-prev)] ctx-prev)
                         installed (update :by-id dissoc (:binding/id installed))))))
            :ok
            (catch Exception _ :failed)))]
    {:mounts mounts-status :context context-status}))

;; ---------------------------------------------------------------------------
;; Injection point
;; ---------------------------------------------------------------------------

(defn publisher
  "The four runtime operations evoclj.store.binding's two-phase engine
  needs, ready to merge into its opts map:

      (binding/activate! db sid bundle (merge opts (binding-publish/publisher)))

  Injection keeps the store layer free of context/mount dependencies;
  supplying runtime registries WITHOUT a publisher is a typed failure
  (:store/binding-invalid :reason :publisher-required), never a silent
  no-publication."
  []
  {:publish-fn publish-runtime!
   :unpublish-fn unpublish-runtime!
   :capture-fn capture-runtime-prestate
   :compensate-fn compensate-runtime!})

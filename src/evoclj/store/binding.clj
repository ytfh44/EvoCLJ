(ns evoclj.store.binding
  "Durable session bindings for the dynamic environment.

  A session binding pins a logical identifier (e.g. [:skill \"debugging\"])
  to an exact immutable bundle revision for the lifetime of the session,
  until an explicit reload moves it to a new revision. Bindings are
  runtime environment — they never touch CodeImageId or Resolution
  (stored in generations/sessions) and installing an unused Skill does
  not change CodeImageId.

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

  Layer split (WO-B1 follow-up): this namespace owns the DURABLE half
  of a binding — the session_bindings rows, the fail-closed existence /
  sibling-surface / fs-lease validation, and the two-phase orchestration
  (staging + compensating rollback). The RUNTIME half — publishing a
  bundle's Context surfaces into a context-store and its Directory
  surfaces into a mount-registry, plus the pre-state capture and
  compensation the staged transaction needs — is owned by
  evoclj.runtime.binding-publish and injected as four functions
  (:publish-fn :unpublish-fn :capture-fn :compensate-fn; see
  binding-publish/publisher). Supplying runtime registries WITHOUT the
  publisher is a typed :store/binding-invalid (:reason
  :publisher-required), never a silent no-publication.

  Runtime publishing:
  - activate!/reload! validate the bundle exists FAIL-CLOSED (registry
    hit or CAS artifact required; unverifiable bundles throw typed
    :store/binding-invalid, INV-02), validate every sibling surface via
    evoclj.environment.surface,
    insert/update the durable row, publish mount/context state through
    the injected publisher (if any), and append an event.
  - WO-B1: activation is a TWO-PHASE transaction. Phase 1 stages every
    mutation (durable row write + runtime publication); phase 2 commits
    by appending the auditable event. Any failure inside the staged
    region — a typed publish failure or an injected seam fault — runs
    the COMPENSATING rollback (delete the staged row / restore the
    prior row bytes, undo exactly this transaction's runtime deltas)
    and rethrows the original exception: a failed activation leaves the
    system byte-comparable to its pre-activation state, never torn.
    A fault after the commit point (:after-event-append) never rolls
    back. If the rollback itself cannot run, the caller gets typed
    :store/binding-rollback-failed carrying the original error.
  - WO-B1: publisher failures are TYPED (:store/binding-publish-failed,
    thrown by evoclj.runtime.binding-publish), never
    caught-and-continued.
  - WO-B1: persisted metadata is serialized through a strict KEY
    ALLOWLIST per surface (:materializer is stripped as operational;
    backend records flatten to plain descriptors); unknown keys or
    values that do not round-trip strict EDN are rejected typed
    :store/binding-metadata-invalid before anything is written.
  - active-bindings reads only the durable table (CAS fallback, not
    catalog) so it works after source deletion or a restart.
  - WO-B1: restore! verifies EVERY durable binding (phase 1) before
    republishing ANY of them (phase 2); the scheduler's run-session!
    calls it before the session's Work is dispatched (production restart
    wiring).
  - WO-B4: when the caller supplies a filesystem lease (`:fs-lease`,
    plus an optional `:fs-lease-registry` atom for revocation/recording
    checks), activate!/reload!/restore! RE-verify it fail-closed against
    the session's pinned code image BEFORE any runtime state is
    published — a stale/expired/revoked lease is rejected with a typed
    error, never silently honored. Without an `:fs-lease` the engine
    grants no filesystem access (no lease, no grant).

  Phenotype invariant: this namespace never reads or writes
  generations.genome_id, generations.resolution_id or sessions.phenotype_id
  except to fetch the session's pinned generation/code-image for the
  event's causal chain; it never mutates them."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.environment.bundle :as env-bundle]
            [evoclj.environment.surface :as surf]
            [evoclj.genome.types :as types]
            [evoclj.kernel.error :as err]
            [evoclj.mount.filesystem :as mount-fs]
            [evoclj.sci.boundary :as boundary]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.support.failpoint :as fault])
  (:import (java.time Instant)
           (java.util Date UUID)))

;; ---------------------------------------------------------------------------
;; Timestamp failures (shared coercion + ISO formatting: store.sqlite)
;; ---------------------------------------------------------------------------

(defn- invalid-timestamp
  [ts]
  (err/error :store/binding-invalid
             "timestamp must be an inst, Instant, or ISO-8601 string"
             {:timestamp ts}))

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
                    (catch Exception _
                      ;; WO-B1: a corrupt persisted payload is a typed
                      ;; failure, never a silent {}
                      (throw (err/error :store/binding-invalid
                                        "persisted metadata_edn is not readable EDN"
                                        {:binding/id (:id row)})))))})

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

(defn bundle->logical
  "Extract logical-id vector from a bundle or offer map. Public: the
  binding data model is shared with evoclj.runtime.binding-publish (the
  runtime publisher), which consumes the same accessors."
  [bundle]
  (or (:logical/id bundle)
      (:offer/logical-id bundle)
      (:logical-id bundle)
      (throw (err/error :store/binding-invalid "bundle/offer missing logical id" {:bundle bundle}))))

(defn bundle->revision
  [bundle]
  (or (:revision/id bundle)
      (:offer/revision-id bundle)
      (:revision-id bundle)
      (throw (err/error :store/binding-invalid "bundle/offer missing revision_id" {:bundle bundle}))))

(defn bundle->bundle-id
  [bundle]
  (or (:bundle/id bundle)
      (:offer/bundle-id bundle)
      (:bundle-id bundle)
      (throw (err/error :store/binding-invalid "bundle/offer missing bundle_id" {:bundle bundle}))))

(defn bundle->surfaces
  [bundle]
  (or (:surfaces bundle) []))

;; WO-B3: mount-id shape unification. A mount id is ALWAYS a canonical
;; vector ([:skill "name" "sha256:..."], [:workspace "id"]) — never a
;; bare scalar :surface/id. The canonical id for a directory surface is
;; the bundle's logical-id vector extended with the surface's
;; :revision/id when that revision is not already part of the logical id.
;; Used by publish/unpublish/publish-mount-ids/removed-mount-ids-for so
;; every mount registry key derives from ONE formula (INV-05).

(defn directory-mount-id
  "Canonical vector mount-id for a directory surface.

  Returns logical-id (a vector) extended with the surface's :revision/id
  when that revision is not already a component of the logical id.
  Throws typed :store/binding-invalid when logical-id is not a usable
  vector base (fail-closed)."
  [logical-id surface]
  (when-not (and (vector? logical-id) (seq logical-id) (keyword? (first logical-id)))
    (throw (err/error :store/binding-invalid
                      "directory surface mount id requires a vector logical-id"
                      {:logical/id logical-id :surface/id (:surface/id surface)})))
  (let [lid (vec logical-id)
        rev (:revision/id surface)]
    (cond-> lid
      (and rev (not (some #{rev} lid))) (conj rev))))

(defn mount-key-for
  "Given an element of surfaces-or-ids (a surface map OR an
  already-canonical mount-id), return its canonical mount registry key.
  WO-B3: surfaces are normalized via directory-mount-id; pre-computed
  ids pass through unchanged."
  [logical-id elt]
  (if (map? elt)
    (directory-mount-id logical-id elt)
    elt))

;; WO-B1 metadata allowlist: the ONLY surface keys that may cross the
;; persistence boundary. :materializer is operational (stripped before
;; the check); anything else is an attempt to persist uncontrolled
;; shape and is rejected typed.
(def ^:private metadata-surface-allowlist
  #{:surface/id :surface/type :revision/id :bundle/id :logical/id
    :descriptor :entries :access/max :backend})

;; The plain backend descriptor carries exactly these keys.
(def ^:private metadata-backend-allowlist
  #{:type :tree/id :root})

(defn- metadata-invalid!
  "Throw typed :store/binding-metadata-invalid."
  [message data]
  (throw (err/error :store/binding-metadata-invalid message data)))

(defn- sanitize-surface
  "WO-B1 allowlist serialization of one surface for persisted metadata.

  Policy (fail-closed):
  - :materializer is operational state and is stripped silently;
  - every REMAINING key must be in metadata-surface-allowlist — any
    other key rejects the whole activation typed
    :store/binding-metadata-invalid naming the rejected keys;
  - a map :backend with a :tree/id passes through its allowlisted keys
    only; record backends flatten to the plain
    {:type :cas-tree :tree/id ...} descriptor; any other backend
    becomes the same plain descriptor keyed by revision;
  - finally the result MUST be plain EDN-safe data (Global Constraint
    22, checked recursively WITHOUT realizing anything) — a poisoned
    value under an allowed key is rejected typed, never silently
    sanitized away."
  [s]
  (let [stripped (dissoc s :materializer)
        rejected (remove metadata-surface-allowlist (keys stripped))]
    (when (seq rejected)
      (metadata-invalid! "surface carries keys outside the metadata allowlist"
                         {:rejected-keys (vec (sort rejected))
                          :surface/type (:surface/type stripped)}))
    (let [b (if (contains? stripped :backend)
              (let [backend (:backend stripped)
                    tree-id (or (:tree-id backend) (:tree/id backend) (:revision/id stripped))
                    plain (cond
                            (and (map? backend) (:tree/id backend))
                            (select-keys backend metadata-backend-allowlist)

                            (instance? clojure.lang.IRecord backend)
                            {:type :cas-tree :tree/id tree-id}

                            :else
                            {:type :cas-tree :tree/id (or tree-id (:revision/id stripped))})]
                (assoc stripped :backend plain))
              stripped)]
      (when-not (boundary/edn-safe? b)
        (metadata-invalid! "surface values must be plain EDN-safe data (non-data value under an allowed key)"
                           {:surface/type (:surface/type b) :surface/id (:surface/id b)}))
      b)))

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

(def ^:private event-append-max-attempts
  "Bounded retries for the auditable event append. Each
  :store/prev-not-immediate rejection implies a peer committed to the
  session tip first, so re-reading the tip converges; 10 attempts bounds
  worst-case spinning far above realistic racer counts."
  10)

(defn- append-binding-event!
  "Append an auditable binding event (:binding/activated, :binding/reloaded, :binding/deactivated).
  Uses the session's pinned generation/code-image and chains to the latest event.

  Why retry (not deny) on a lost tip race: concurrent binding lifecycle
  intents for DISTINCT logical-ids are independent — event order between
  them carries no semantic conflict, since the events only audit state
  rows that are already committed under their own UNIQUE guard (same
  logical-id contention still fails closed via
  :store/binding-already-active at row insert, before this append). A
  failed append attempt writes nothing (the store throws before INSERT,
  inside its own rolled-back transaction), so re-reading the tip and
  re-appending the identical event is side-effect-free and the final
  chain still satisfies the strict predecessor invariant on every row.
  Only :store/prev-not-immediate is retried; every other error
  propagates immediately. Exhaustion throws
  :store/binding-event-contention."
  [db session-id event-type metadata]
  (let [{:keys [generation_id phenotype_id]} (fetch-session db session-id)
        sid (types/session-id session-id)]
    (loop [attempt 1]
      (let [cause (event/latest-event-id db session-id)]
        (when (nil? cause)
          (throw (err/error :store/binding-invalid
                            "session has no events; expected :session/created root before binding activation"
                            {:session/id sid :event/type event-type})))
        (let [req {:session/id sid
                   :generation/id generation_id
                   :code/id phenotype_id
                   :event/type event-type
                   :prev/event-id cause
                   :payload-ref nil
                   :metadata (or metadata {})}
              outcome (try
                        (event/append-event! db req)
                        (catch clojure.lang.ExceptionInfo rejected rejected))]
          (cond
            (not (instance? clojure.lang.ExceptionInfo outcome))
            outcome
            (not (= :store/prev-not-immediate (:error/type (ex-data outcome))))
            (throw outcome)
            (< attempt event-append-max-attempts)
            (recur (inc attempt))
            :else
            (throw (err/error :store/binding-event-contention
                              "binding event append lost too many races for the session tip"
                              {:session/id sid
                               :event/type event-type
                               :attempts attempt
                               :last-error (ex-data outcome)}))))))))

;; ---------------------------------------------------------------------------
;; Validation helpers
;; ---------------------------------------------------------------------------

(defn- validate-bundle-exists
  "Fail-closed existence check (INV-02): a binding may only be
  activated/reloaded/restored when its referenced bundle can be shown
  to exist. Verification sources, either of which confirming passes:
    - registry (EnvironmentRegistry atom): bundle present under its
      :bundle/id via evoclj.environment.bundle/get-bundle;
    - cas (CAS handle/root/path): artifact present for :revision/id.
  If every supplied source misses — or none is supplied, so existence
  cannot be established at all — throws typed :store/binding-invalid
  naming :bundle/id and :revision/id. Structural checks (canonical
  sha256 revision id, non-empty string bundle id) always run first and
  throw the same type. A source lookup ERROR counts as a miss, never as
  proof of existence. The registry reader is referenced statically via
  a top-level require (B2: resolution proven acyclic) — no runtime
  symbol lookup."
  [bundle registry cas]
  (let [bid (bundle->bundle-id bundle)
        rev (bundle->revision bundle)]
    (when-not (types/artifact-id? rev)
      (throw (err/error :store/binding-invalid "revision_id must be sha256:<64 hex>" {:revision/id rev})))
    (when-not (and (string? bid) (seq bid))
      (throw (err/error :store/binding-invalid "bundle_id must be non-empty string" {:bundle/id bid})))
    (let [in-registry? (when registry
                         ;; a lookup error is not evidence of existence:
                         ;; treat it as a miss and let the verdict decide
                         (try
                           (some? (env-bundle/get-bundle registry bid))
                           (catch Exception _ false)))
          in-cas? (when cas
                    (try
                      (cas/exists? cas rev)
                      (catch Exception _ false)))]
      ;; INV-02 fail-closed verdict: existence must be positively
      ;; confirmed by at least one supplied source; a miss or a source
      ;; error under every supplied source — or no source at all —
      ;; refuses activation/reload/restore.
      (when-not (or in-registry? in-cas?)
        (throw (err/error :store/binding-invalid
                          "bundle cannot be verified to exist (absent from registry and CAS)"
                          {:bundle/id bid :revision/id rev}))))
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

(defn- verify-binding-fs-lease!
  "B4 fail-closed re-verification: when the caller supplies a filesystem
  lease, it is re-verified (case R: requester :: revoked) so only a
  valid lease is activation-worthy. Verify-FS-lease's registry path
  ensures the granted lease is genuine and the window covers :now; an
  expired / revoked lease is rejected with a typed error, never silently
  honored. When no `:fs-lease` is supplied this is a no-op (the engine
  grants no filesystem access without a lease). Reuses
  evoclj.mount.filesystem/verify-fs-lease! (single implementation, INV-05).
  Returns the lease when present. I2: verification principal is
  SessionPrincipal(session-id), principal equality."
  [db session-id fs-lease opts]
  (when fs-lease
    (mount-fs/verify-fs-lease! fs-lease
                                 {:principal {:principal/type :session :session/id (types/session-id session-id)}
                                  :now (:now opts)
                                  :registry (:fs-lease-registry opts)}))
  fs-lease)

;; ---------------------------------------------------------------------------
;; Injected runtime publisher (the store owns persistence, not publication)
;; ---------------------------------------------------------------------------
;;
;; The RUNTIME half of a binding — publishing Context surfaces into a
;; context-store and Directory surfaces into a mount-registry, plus the
;; pre-state capture and compensation the staged transaction needs — is
;; owned by evoclj.runtime.binding-publish. The store receives those four
;; operations as injected functions (see binding-publish/publisher), so
;; this namespace requires neither the context nor the mount layer.

(def ^:private publisher-keys
  [:publish-fn :unpublish-fn :capture-fn :compensate-fn])

(defn- publisher-of
  "The injected runtime publisher for one call: the four operations when
  the caller supplied runtime registries, or an all-nil map when there is
  nothing to publish.

  Fail-closed: runtime registries WITHOUT the publisher is a typed
  :store/binding-invalid — a binding must never be activated while its
  mount/context state silently goes unpublished."
  [opts]
  (let [runtime? (or (:mount-registry opts) (:context-store opts))
        missing (vec (remove #(fn? (get opts %)) publisher-keys))]
    (if runtime?
      (if (seq missing)
        (throw (err/error :store/binding-invalid
                          "runtime registries require an injected publisher"
                          {:reason :publisher-required
                           :missing missing}))
        (select-keys opts publisher-keys))
      {})))

;; ---------------------------------------------------------------------------
;; WO-B1 two-phase activation: staging + compensating rollback
;; ---------------------------------------------------------------------------

(defn- publish-mount-ids
  "The mount registry keys the runtime publisher would use for bundle's
  directory surfaces (the exact same id formula — canonical vector
  mount-id via directory-mount-id)."
  [bundle]
  (let [logical-id (bundle->logical bundle)]
    (set (for [s (bundle->surfaces bundle)
               :when (= :directory (:surface/type s))]
           (directory-mount-id logical-id s)))))

(defn- removed-mount-ids-for
  "The mount registry keys the runtime unpublisher removes for these
  surfaces-or-ids (mirrors its id logic exactly — canonical vector
  mount-id via mount-key-for)."
  [surfaces-or-ids logical-id]
  (if (seq surfaces-or-ids)
    (set (map #(mount-key-for logical-id %) surfaces-or-ids))
    #{logical-id}))

(defn- compensate!
  "Run the COMPENSATING actions for one failed staged transaction.

  Durable first (the row is the source of truth):
    - inserted-row-id present -> the staged INSERT is deleted;
    - otherwise old-row present -> EVERY mutated column is written back
      from the captured row image (byte-comparable restore).
  Then the runtime deltas. If any compensation step itself throws, the
  caller gets typed :store/binding-rollback-failed carrying the original
  sanitized error — never silence, never an unexplained torn state.

  txn keys:
    :db :session-id :logical-id
    :inserted-row-id   (activate!: the staged row's id)
    :old-row           (reload!/deactivate!: full pre-image of the row)
    :prestate          (the injected :capture-fn's value)
    :added-mount-ids   (mount keys this txn published)
    :removed-mount-ids (mount keys this txn removed)
    :opts              {:mount-registry :context-store} runtime atoms only
    :compensate-fn     the injected runtime compensation (nil when no
                       runtime registries were supplied)"
  [{:keys [db session-id logical-id inserted-row-id old-row prestate
           added-mount-ids removed-mount-ids opts compensate-fn
           original-throwable]}]
  (let [durable-ok?
        (try
          (sqlite/with-db [conn db]
            (if inserted-row-id
              (jdbc/delete! conn :session_bindings
                            ["id = ? AND session_id = ?" inserted-row-id (str (types/session-id session-id))])
              (when old-row
                (jdbc/execute! conn
                               ["UPDATE session_bindings SET revision_id = ?, bundle_id = ?, activated_at = ?, metadata_edn = ?, state = ?, deactivated_at = ? WHERE id = ?"
                                (:revision_id old-row) (:bundle_id old-row) (:activated_at old-row)
                                (:metadata_edn old-row) (:state old-row) (:deactivated_at old-row)
                                (:id old-row)]))))
          true
          (catch Exception _ false))
        ;; stores marked ::unreadable at capture time are SKIPPED, not
        ;; failures: we never promised to restore state we could not read.
        ;; An ATTEMPTED step that FAILED means the rollback could not run.
        statuses (if compensate-fn
                   (try
                     (let [s (compensate-fn
                              {:prestate prestate
                               :added-mount-ids added-mount-ids
                               :removed-mount-ids removed-mount-ids
                               :opts opts
                               :logical-id logical-id})]
                       (if (map? s) s {:mounts :failed :context :failed}))
                     (catch Exception _ {:mounts :failed :context :failed}))
                   {:mounts :ok :context :ok})]
    (when (or (false? durable-ok?)
              (some #(= :failed %) (vals statuses)))
      (throw (err/error :store/binding-rollback-failed
                        "compensating rollback itself failed; staged state may be retained"
                        {:durable-compensated durable-ok?
                         :runtime-status statuses
                         :error/original (some-> original-throwable err/error-data)})))
    nil))

(defn- run-staged!
  "Run ONE WO-B1 staged two-phase transaction — the single implementation
  behind activate!/reload!/deactivate!.

  `txn` carries compensate!'s collaborators and this transaction's
  identity (:db :session-id :logical-id :mount-registry :context-store),
  the durable-compensation pair (:inserted-row-id / :old-row), the
  runtime deltas it applies (:added-mount-ids / :removed-mount-ids), and
  the injected runtime pair (:capture-fn / :compensate-fn). The engine
  captures the runtime pre-state (the :capture-fn) BEFORE running
  `stage-fn`, so a failed transaction restores exactly what it touched.

  `stage-fn` performs the staged steps — the durable row write already
  happened in the caller — and is handed a zero-arg `commit!` it MUST call
  once, past the point where the transaction is final (WO-B1 published ->
  durable ordering: the event is appended before commit!). Its value is
  the caller's return value. A Throwable before commit! runs the SINGLE
  compensation engine (compensate!) and is rethrown unchanged; nothing
  compensates after commit!. compensate! may itself throw
  :store/binding-rollback-failed."
  [{:keys [db session-id logical-id mount-registry context-store
           inserted-row-id old-row added-mount-ids removed-mount-ids
           capture-fn compensate-fn]}
   stage-fn]
  (let [runtime-opts {:mount-registry mount-registry :context-store context-store}
        prestate (when capture-fn (capture-fn logical-id runtime-opts))
        committed? (atom false)]
    (try
      (stage-fn #(reset! committed? true))
      (catch Throwable t
        (when-not @committed?
          (compensate! {:db db :session-id session-id :logical-id logical-id
                        :inserted-row-id inserted-row-id :old-row old-row
                        :prestate prestate :added-mount-ids added-mount-ids
                        :removed-mount-ids removed-mount-ids :opts runtime-opts
                        :compensate-fn compensate-fn
                        :original-throwable t}))
        (throw t)))))

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
        now (sqlite/canonical-timestamp nil invalid-timestamp)]
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

  Validates bundle existence fail-closed (INV-02: a registry hit or a
  CAS artifact for the revision must confirm the bundle exists; an
  unverifiable bundle throws instead of passing through) and that all
  sibling surfaces are bindable, creates a durable row in
  session_bindings, publishes runtime mount/context state, and appends
  an auditable event.

  Signatures:
    (activate! db session-id bundle)
    (activate! db session-id bundle opts)
  where bundle is a SurfaceBundle map or an Offer map, and opts is an
  optional map with keys:
    :registry       — environment registry atom (for bundle existence check)
    :cas            — CAS handle (string/path or {:root ...}) for CAS check
    :mount-registry — atom map mount-id -> mount
    :context-store  — atom from evoclj.context.binding/create-store
    :fs-lease       — optional filesystem CapabilityLease (B4): RE-verified
                      fail-closed against the session's pinned code image
                      before any runtime state is published
    :fs-lease-registry — optional atom from
                      evoclj.mount.filesystem/create-lease-registry for
                      revocation/recording checks (B4)

  Returns the persisted binding map (as from active-bindings).
  Throws :store/binding-invalid, :bundle/co-version-violation,
  :store/session-not-found, or :store/binding-already-active (unique violation)."
  ([db session-id bundle]
   (activate! db session-id bundle {}))
  ([db session-id bundle opts]
   (when-not bundle
     (throw (err/error :store/binding-invalid "bundle/offer required" {:bundle bundle})))
   (let [opts (if (and (map? opts) (not (contains? opts :registry)) (not (contains? opts :cas))
                       (not (contains? opts :mount-registry)) (not (contains? opts :context-store))
                       ;; T2: an opts map carrying failpoint seams is a
                       ;; legitimate opts map — do not normalize it away
                       (not (contains? opts :failpoints)))
                (if (instance? clojure.lang.Atom opts) {:registry opts} {})
                opts)
         registry (:registry opts)
         cas-handle (:cas opts)
         mount-registry (:mount-registry opts)
         context-store (:context-store opts)
         {:keys [publish-fn capture-fn compensate-fn]} (publisher-of opts)
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
     ;; B4: if the caller granted a filesystem lease, RE-verify it
     ;; fail-closed against the session's pinned code image before any
     ;; runtime state is published.
     (verify-binding-fs-lease! db session-id (:fs-lease opts) opts)
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
       ;; ---- WO-B1 staged region: everything below is compensatable ----
       (let [added-ids (publish-mount-ids bundle)]
         (run-staged! {:db db
                       :session-id session-id
                       :logical-id logical-id
                       :mount-registry mount-registry
                       :context-store context-store
                       :inserted-row-id id
                       :old-row nil
                       :added-mount-ids added-ids
                       :removed-mount-ids #{}
                       :capture-fn capture-fn
                       :compensate-fn compensate-fn}
                      (fn [commit!]
                        ;; T2 seam: durable row inserted; sits outside every catch around
                        ;; the insert, so a hook throw propagates to the caller unchanged
                        (fault/trigger! opts :after-db-insert)
                        (when publish-fn
                          (publish-fn bundle {:mount-registry mount-registry :context-store context-store :cas cas-handle}))
                        ;; T2 seam: runtime mount/context state published
                        (fault/trigger! opts :after-publish-runtime)
                        ;; T2 seam: last point before the auditable event append
                        (fault/trigger! opts :before-event-append)
                        (append-binding-event! db session-id :binding/activated
                                               {:logical/id logical-id :revision/id rev :bundle/id bid :binding/id id})
                        ;; ---- commit point: past here the activation is final ----
                        (commit!)
                        ;; T2 seam: event appended successfully
                        (fault/trigger! opts :after-event-append)
                        (get-binding db session-id logical-id))))))))

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
  Throws :store/binding-invalid if the new bundle cannot be verified to
  exist (fail-closed, INV-02 — the durable row is left untouched), and
  :store/binding-not-found if no active binding for logical-id."
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
         {:keys [publish-fn unpublish-fn capture-fn compensate-fn]} (publisher-of opts)
         new-bundle (if (and (map? new-bundle) (contains? new-bundle :bundle))
                      (:bundle new-bundle) new-bundle)
         new-rev (bundle->revision new-bundle)
         new-bid (bundle->bundle-id new-bundle)
         target-logical (try (bundle->logical new-bundle) (catch Exception _ logical-id))
         sid (str (types/session-id session-id))
         lid (logical->text logical-id)
         now (sqlite/canonical-timestamp nil invalid-timestamp)]
     (when (and target-logical (not= target-logical logical-id))
       (throw (err/error :store/binding-invalid "new bundle logical_id must match requested logical_id"
                         {:requested logical-id :new-logical target-logical})))
     (fetch-session db session-id)
     ;; fetch old binding metadata to know old mount ids to remove;
     ;; WO-B1: also capture the FULL pre-image row so a failed staged
     ;; reload can restore every column byte-comparably
     (let [old-binding (get-binding db session-id logical-id)
           old-surfaces (when old-binding (-> old-binding :metadata :surfaces))
           old-row (first (sqlite/query db ["SELECT * FROM session_bindings WHERE session_id = ? AND logical_id = ? AND state = 'active'" sid lid]))]
       (validate-bundle-exists new-bundle registry cas-handle)
       (validate-sibling-surfaces new-bundle)
       ;; B4: RE-verify the fs lease fail-closed before republishing runtime
       ;; state at the new revision (a stale/expired/revoked lease aborts
       ;; the reload rather than being silently honored).
       (verify-binding-fs-lease! db session-id (:fs-lease opts) opts)
       (let [cnt (sqlite/with-db [conn db]
                   (first (jdbc/execute! conn
                                         ["UPDATE session_bindings SET revision_id = ?, bundle_id = ?, activated_at = ?, metadata_edn = ? WHERE session_id = ? AND logical_id = ? AND state = 'active'"
                                          new-rev new-bid now (bundle->metadata new-bundle) sid lid])))]
         (when-not (= 1 cnt)
           (throw (err/error :store/binding-not-found "no active binding for this session + logical_id"
                             {:session/id (types/session-id session-id) :logical/id logical-id})))
         ;; ---- WO-B1 staged region: everything below is compensatable ----
         (let [added-ids (publish-mount-ids new-bundle)
               removed-ids (removed-mount-ids-for old-surfaces logical-id)]
           (run-staged! {:db db
                         :session-id session-id
                         :logical-id logical-id
                         :mount-registry mount-registry
                         :context-store context-store
                         :inserted-row-id nil
                         :old-row old-row
                         :added-mount-ids added-ids
                         :removed-mount-ids removed-ids
                         :capture-fn capture-fn
                         :compensate-fn compensate-fn}
                        (fn [commit!]
                          ;; T2 seam: durable row updated (revision/bundle/metadata)
                          (fault/trigger! opts :after-db-insert)
                          (when unpublish-fn
                            (unpublish-fn logical-id {:mount-registry mount-registry :context-store context-store} old-surfaces))
                          (when publish-fn
                            (publish-fn new-bundle {:mount-registry mount-registry :context-store context-store :cas cas-handle}))
                          ;; T2 seam: runtime state republished at the new revision
                          (fault/trigger! opts :after-publish-runtime)
                          ;; T2 seam: last point before the auditable event append
                          (fault/trigger! opts :before-event-append)
                          (append-binding-event! db session-id :binding/reloaded
                                                 {:logical/id logical-id :from-revision (:revision/id old-binding) :to-revision new-rev :bundle/id new-bid})
                          ;; ---- commit point: past here the reload is final ----
                          (commit!)
                          ;; T2 seam: reloaded event appended successfully
                          (fault/trigger! opts :after-event-append)
                          (get-binding db session-id logical-id)))))))))

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
         {:keys [unpublish-fn capture-fn compensate-fn]} (publisher-of opts)
         sid (str (types/session-id session-id))
         lid (logical->text logical-id)
         now (sqlite/canonical-timestamp nil invalid-timestamp)
         ;; fetch before update to know mount ids to clean; WO-B1 also
         ;; captures the FULL pre-image row for byte-comparable rollback
         old-binding (get-binding db session-id logical-id)
         old-surfaces (when old-binding (-> old-binding :metadata :surfaces))
         old-row (first (sqlite/query db ["SELECT * FROM session_bindings WHERE session_id = ? AND logical_id = ? AND state = 'active'" sid lid]))
         cnt (sqlite/with-db [conn db]
               (first (jdbc/execute! conn
                                     ["UPDATE session_bindings SET state = 'inactive', deactivated_at = ? WHERE session_id = ? AND logical_id = ? AND state = 'active'"
                                      now sid lid])))]
     (when-not (= 1 cnt)
       (throw (err/error :store/binding-not-found "no active binding to deactivate"
                         {:session/id (types/session-id session-id) :logical/id logical-id})))
     ;; ---- WO-B1 staged region: unpublish + commit event compensatable ----
     (let [removed-ids (removed-mount-ids-for old-surfaces logical-id)]
       (run-staged! {:db db
                     :session-id session-id
                     :logical-id logical-id
                     :mount-registry mount-registry
                     :context-store context-store
                     :inserted-row-id nil
                     :old-row old-row
                     :added-mount-ids #{}
                     :removed-mount-ids removed-ids
                     :capture-fn capture-fn
                     :compensate-fn compensate-fn}
                    (fn [commit!]
                      (when unpublish-fn
                        (unpublish-fn logical-id {:mount-registry mount-registry :context-store context-store} old-surfaces))
                      ;; T2 seam: runtime state removed (row already flipped to inactive)
                      (fault/trigger! opts :after-unpublish)
                      (append-binding-event! db session-id :binding/deactivated
                                             {:logical/id logical-id})
                      ;; ---- commit point ----
                      (commit!)
                      (let [row (first (sqlite/query db ["SELECT * FROM session_bindings WHERE session_id = ? AND logical_id = ? ORDER BY activated_at DESC LIMIT 1" sid lid]))]
                        (when row (row->binding row)))))))))

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

  Fail-closed (INV-02): every durable binding is re-verified via
  validate-bundle-exists against the supplied sources BEFORE any
  runtime state is republished; a binding whose revision no longer
  resolves (e.g. garbage-collected CAS artifact) aborts the whole
  restore with typed :store/binding-invalid instead of republishing
  content that cannot exist.

  WO-B1 two-phase: verification of ALL bindings is phase 1; phase 2
  republishes them while journaling compensations. A publication
  failure mid-restore unwinds every already-republished binding back to
  the pre-restore state, then rethrows (or throws typed
  :store/binding-rollback-failed if the unwind itself cannot run).
  With zero active bindings this is a no-op returning [].

  Returns the restored bindings (vector)."
  [db session-id opts]
  (let [bindings (active-bindings db session-id)
        bundles (mapv (fn [b]
                        (let [meta (:metadata b)]
                          (or (:bundle meta)
                              {:bundle/id (:bundle/id b)
                               :revision/id (:revision/id b)
                               :logical/id (:logical/id b)
                               :surfaces (:surfaces meta [])})))
                      bindings)]
    ;; ---- phase 1: verify EVERYTHING before publishing ANYTHING ----
    ;; B4: if the caller granted a filesystem lease, RE-verify it once
    ;; fail-closed against the session's pinned code image before any
    ;; binding is republished (a stale/expired/revoked lease aborts the
    ;; restore rather than being silently honored).
    (verify-binding-fs-lease! db session-id (:fs-lease opts) opts)
    (doseq [bundle bundles]
      ;; B2 / INV-02: refuse to republish a binding whose bundle no
      ;; longer exists in registry/CAS — no partial runtime state.
      (validate-bundle-exists bundle (:registry opts) (:cas opts)))
    ;; ---- phase 2: publish all, with a compensation journal ----
    (if-not (seq bundles)
      ;; zero active bindings: nothing to republish
      bindings
      (let [{:keys [publish-fn capture-fn compensate-fn]} (publisher-of opts)
            runtime-opts {:mount-registry (:mount-registry opts)
                          :context-store (:context-store opts)}
            publish-all
            (fn publish-all*
              [pending undos]
              (if-not (seq pending)
                ;; committed everything: the restore is final
                bindings
                (let [bundle (first pending)
                      logical-id (:logical/id bundle)
                      prestate (when capture-fn (capture-fn logical-id runtime-opts))
                      undo {:logical-id logical-id
                            :prestate prestate
                            :added-mount-ids (publish-mount-ids bundle)}
                      outcome (try
                                (when publish-fn (publish-fn bundle opts))
                                ::published
                                (catch Throwable t {::failure t}))
                      failure (::failure outcome)]
                  (if failure
                    ;; unwind in reverse, then surface honestly
                    (let [unwound
                          (try
                            (let [statuses (mapv #(compensate-fn
                                                   {:prestate (:prestate %)
                                                    :added-mount-ids (:added-mount-ids %)
                                                    :removed-mount-ids #{}
                                                    :opts runtime-opts
                                                    :logical-id (:logical-id %)})
                                                 (reverse undos))]
                              ;; ::unreadable pre-states are skipped by
                              ;; compensation; only an ATTEMPTED step that
                              ;; failed counts as an unrunnable rollback
                              (if (some #(= :failed %) (flatten statuses))
                                ::step-failed
                                ::ok))
                            (catch Throwable t'
                              {::rollback-failure t'}))]
                      (cond
                        (= ::ok unwound)
                        (throw failure)

                        (= ::step-failed unwound)
                        (throw (err/error :store/binding-rollback-failed
                                          "restore rollback itself failed; staged state may be retained"
                                          {:error/original (err/error-data failure)}))

                        :else
                        (throw (err/error :store/binding-rollback-failed
                                          "restore rollback itself failed; staged state may be retained"
                                          {:error/original (err/error-data failure)
                                           :error/rollback-failure (err/error-data (::rollback-failure unwound))}))))
                    (recur (rest pending) (conj undos undo))))))]
        (publish-all bundles [])))))

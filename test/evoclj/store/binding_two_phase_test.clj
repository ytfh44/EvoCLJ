(ns evoclj.store.binding-two-phase-test
  "WO-B1 — two-phase / compensating transaction for binding activation,
  typed publish-runtime! failures, and allowlisted metadata serialization.

  Contracts pinned here (all through the production activate!/reload!/
  deactivate!/restore! paths — no injected stand-ins for any production
  component):

  1. TWO-PHASE ACTIVATION. Phase 1 stages every mutation (durable row
     write + runtime mount/context publication); phase 2 commits by
     appending the auditable event. Any failure inside the staged region
     — including an injected seam fault (:after-db-insert,
     :after-publish-runtime, :before-event-append) or a typed publish
     failure — runs the COMPENSATING rollback and leaves the system at
     the pre-activation state: no durable row, no runtime residue, no
     commit event. A fault AFTER the commit point (:after-event-append)
     never rolls back. The original exception still reaches the caller
     unchanged (T2 contract preserved).
  2. TYPED PUBLISH. publish-runtime! never catches-and-continues: a
     failing publication surfaces as typed
     :store/binding-publish-failed and triggers the rollback above.
     If the compensation itself cannot run, the caller gets typed
     :store/binding-rollback-failed naming the original error — never
     silence, never a torn half-published binding.
  3. METADATA ALLOWLIST. Persisted binding metadata serializes each
     surface through a strict key ALLOWLIST: operational keys
     (:materializer) are stripped, backend records become plain
     descriptors, and any surface carrying a key outside the allowlist
     — or a value that does not round-trip strict EDN — is rejected
     typed :store/binding-metadata-invalid before anything is written.
     Reading back a corrupt metadata_edn row fails typed instead of
     silently yielding {}.
  4. RESTORE ATOMICITY. restore! verifies EVERY durable binding
     (phase 1) BEFORE republishing any of them (phase 2): one
     unverifiable binding republishes nothing."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.context.binding :as ctx-binding]
            [evoclj.environment.surface :as surf]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.revision :as rev]
            [evoclj.mount.backend :as mount-backend]
            [evoclj.store.binding :as binding]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

;; A record-shaped backend value: exercises the metadata serializer's
;; record -> plain descriptor flattening (production path, real IRecord).
(defrecord FakeTreeBackend [])

;; ---------------------------------------------------------------------------
;; Fixtures (same pattern as store/binding_test)
;; ---------------------------------------------------------------------------

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-b1-" ".db" (make-array FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- temp-cas-root []
  (let [p (Files/createTempDirectory "evoclj-b1-cas-" (make-array FileAttribute 0))]
    (swap! cas-roots conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (Files/deleteIfExists (java.nio.file.Paths/get p (make-array String 0))))
  (reset! db-paths [])
  (doseq [^java.nio.file.Path r @cas-roots]
    (when (Files/exists r (make-array java.nio.file.LinkOption 0))
      (doseq [f (reverse (file-seq (.toFile r)))]
        (Files/deleteIfExists (.toPath f)))))
  (reset! cas-roots []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private gen "generation-1")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))

(defn- seed-session!
  "Generation + session + :session/created root event; returns sid."
  [db]
  (sqlite/with-db [conn db]
    (when-not (first (jdbc/query conn ["SELECT id FROM generations WHERE id = ?" gen]))
      (jdbc/insert! conn :generations
                    {:id gen :genome_id genome :resolution_id resolution
                     :parent_id nil :state "active" :current 0 :created_at now})))
  (let [sid (:session/id
             (session/create-session! db {:genome/id genome
                                          :resolution/id resolution
                                          :phenotype/id phenotype
                                          :generation/id gen}))]
    (event/append-event! db {:session/id sid
                             :generation/id gen
                             :phenotype/id phenotype
                             :event/type :session/created
                             :cause/event-id nil
                             :payload-ref nil
                             :metadata {}})
    sid))

(defn- fresh-cas-with!
  "Fresh CAS containing the raw bytes of each given string, so a bundle
  built from payload s verifies against it (revision id == text digest)."
  [& contents]
  (let [c (cas/->cas (str (temp-cas-root)))]
    (doseq [s contents]
      (cas/put-bytes! c (.getBytes ^String s StandardCharsets/UTF_8) {:media-type "text/plain"}))
    c))

(defn- make-skill-bundle
  "Skill bundle with context+directory sibling surfaces sharing one rev.
  Surface ids are unique per skill NAME (namespace = logical tag) so two
  different skills never collide on mount keys."
  ([logical payload] (make-skill-bundle logical payload {}))
  ([logical payload {:keys [surface-overrides]}]
   (let [rev (rev/payload->id payload)
         bid (str "bundle:" rev ":" (pr-str logical))
         nm (second logical)
         surfaces [(surf/make-context-surface {:id (keyword (str (name (first logical)) nm "-ctx"))
                                               :descriptor {:prompt payload}
                                               :materializer identity
                                               :revision/id rev})
                   (surf/make-directory-surface {:id (keyword (str (name (first logical)) nm "-dir"))
                                                 :backend {:type :memory :root "/tmp"}
                                                 :access-max #{:read :list :stat}
                                                 :revision/id rev})]
         surfaces (if surface-overrides
                    (mapv #(merge % (get surface-overrides (:surface/type %))) surfaces)
                    surfaces)]
     (bundle/make-bundle {:bundle-id bid :revision-id rev :logical-id logical :surfaces surfaces}))))

(defn- capture-ex
  "Run f; return the thrown Throwable, or nil."
  [f]
  (try (f) nil (catch Throwable t t)))

(defn- activated-events
  [db sid event-type]
  (filter #(= event-type (:event/type %)) (event/events-for-session db sid)))

(defn- any-row
  [db sid]
  (first (jdbc/query db ["SELECT * FROM session_bindings WHERE session_id = ?" (str sid)])))

(def ^:private sentinel-data {:b1/seam :test})

(defn- sentinel []
  (ex-info "b1-injected-phase2-fault" sentinel-data))

;; ---------------------------------------------------------------------------
;; 1. Happy path — staged transaction preserves baseline behavior end to end
;; ---------------------------------------------------------------------------

(deftest b1-happy-two-phase-activation-end-to-end
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (fresh-cas-with! "content-A")
        mounts (mount-backend/create-registry)
        ctx (ctx-binding/create-store)
        logical [:skill "debugging"]
        b (make-skill-bundle logical "content-A")
        res (binding/activate! db sid b {:cas cas-handle
                                         :mount-registry mounts
                                         :context-store ctx})]
    (testing "durable stage committed"
      (is (= logical (:logical/id res)))
      (is (= :active (:state res)))
      (is (= 1 (count (binding/active-bindings db sid)))))
    (testing "commit marker appended exactly once"
      (is (= 1 (count (activated-events db sid :binding/activated)))))
    (testing "runtime state published"
      (is (= 1 (count (ctx-binding/list-active ctx))))
      (is (= 1 (count (mount-backend/list-mounts mounts)))))))

;; ---------------------------------------------------------------------------
;; 2. Phase-2 fault => compensating rollback to byte-comparable pre-state
;; ---------------------------------------------------------------------------

(deftest b1-activate-phase2-fault-rolls-back-to-pre-state
  (doseq [stage [:after-db-insert :after-publish-runtime :before-event-append]]
    (testing (str "activate! fault at :" (name stage) " leaves zero trace")
      (let [db (fresh-db)
            sid (seed-session! db)
            cas-handle (fresh-cas-with! "content-A")
            mounts (mount-backend/create-registry)
            ctx (ctx-binding/create-store)
            logical [:skill "debugging"]
            b (make-skill-bundle logical "content-A")
            sent (sentinel)
            thrown (capture-ex #(binding/activate! db sid b
                                                   {:cas cas-handle
                                                    :mount-registry mounts
                                                    :context-store ctx
                                                    :failpoints {stage (fn [] (throw sent))}}))]
        (testing "original exception reaches the caller unchanged"
          (is (identical? sent thrown)))
        (testing "no durable row survives"
          (is (nil? (any-row db sid))))
        (testing "runtime registries byte-comparable to pre-state"
          (is (= {} @mounts) "no mount residue")
          (is (empty? (ctx-binding/list-active ctx)) "no context residue"))
        (testing "commit marker never appended"
          (is (zero? (count (activated-events db sid :binding/activated)))))))))

(deftest b1-activate-fault-after-commit-point-never-rolls-back
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (fresh-cas-with! "content-A")
        mounts (mount-backend/create-registry)
        ctx (ctx-binding/create-store)
        b (make-skill-bundle [:skill "debugging"] "content-A")
        sent (sentinel)
        thrown (capture-ex #(binding/activate! db sid b
                                               {:cas cas-handle
                                                :mount-registry mounts
                                                :context-store ctx
                                                :failpoints {:after-event-append (fn [] (throw sent))}}))]
    (testing "fault still propagates"
      (is (identical? sent thrown)))
    (testing "activation stays committed (no compensating undo past the commit point)"
      (is (some? (any-row db sid)) "row intact")
      (is (= 1 (count (activated-events db sid :binding/activated))) "event intact")
      (is (= 1 (count (mount-backend/list-mounts mounts))))
      (is (= 1 (count (ctx-binding/list-active ctx)))))))

(deftest b1-reload-phase2-fault-restores-old-revision-byte-comparably
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (fresh-cas-with! "content-A" "content-B")
        mounts (mount-backend/create-registry)
        ctx (ctx-binding/create-store)
        logical [:skill "debugging"]
        a (make-skill-bundle logical "content-A")]
    (binding/activate! db sid a {:cas cas-handle
                                 :mount-registry mounts
                                 :context-store ctx})
    ;; pre-reload snapshots: raw durable row + full runtime atom state
    (let [row-a (any-row db sid)
          mounts-a @mounts
          ctx-a (vec (sort-by (comp str :logical/id) (ctx-binding/list-active ctx)))
          b (make-skill-bundle logical "content-B")
          sent (sentinel)
          thrown (capture-ex #(binding/reload! db sid logical b
                                               {:cas cas-handle
                                                :mount-registry mounts
                                                :context-store ctx
                                                :failpoints {:after-publish-runtime (fn [] (throw sent))}}))]
      (testing "original exception reaches the caller unchanged"
        (is (identical? sent thrown)))
      (testing "durable row restored byte-comparable to revision A"
        (is (= row-a (any-row db sid)) "every column identical to the pre-reload row"))
      (testing "runtime registries restored byte-comparable"
        (is (= mounts-a @mounts))
        (is (= ctx-a (vec (sort-by (comp str :logical/id) (ctx-binding/list-active ctx)))))
        (is (= (:revision/id a) (:revision/id (ctx-binding/get-binding ctx logical)))
            "context binding pinned back to revision A"))
      (testing "no reloaded commit marker"
        (is (zero? (count (activated-events db sid :binding/reloaded))))))))

(deftest b1-deactivate-phase2-fault-restores-active-pre-state
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (fresh-cas-with! "content-A")
        mounts (mount-backend/create-registry)
        ctx (ctx-binding/create-store)
        logical [:skill "debugging"]
        a (make-skill-bundle logical "content-A")]
    (binding/activate! db sid a {:cas cas-handle
                                 :mount-registry mounts
                                 :context-store ctx})
    (let [row-active (any-row db sid)
          mounts-a @mounts
          ctx-a (ctx-binding/get-binding ctx logical)
          sent (sentinel)
          thrown (capture-ex #(binding/deactivate! db sid logical
                                                   {:mount-registry mounts
                                                    :context-store ctx
                                                    :failpoints {:after-unpublish (fn [] (throw sent))}}))]
      (testing "original exception reaches the caller unchanged"
        (is (identical? sent thrown)))
      (testing "row flipped back to active, byte-comparable"
        (is (= row-active (any-row db sid))))
      (testing "runtime state republished (pre-state restored)"
        (is (= mounts-a @mounts))
        (is (= ctx-a (ctx-binding/get-binding ctx logical))))
      (testing "no deactivated commit marker"
        (is (zero? (count (activated-events db sid :binding/deactivated))))))))

;; ---------------------------------------------------------------------------
;; 3. Typed publish failures — swallowed exceptions become typed errors
;; ---------------------------------------------------------------------------

(deftest b1-publish-failure-is-typed-and-compensated
  (testing "hostile context store -> typed :store/binding-publish-failed, nothing persisted"
    (let [db (fresh-db)
          sid (seed-session! db)
          cas-handle (fresh-cas-with! "content-C")
          b (make-skill-bundle [:skill "ctx-boom"] "content-C")
          thrown (capture-ex #(binding/activate! db sid b
                                                 {:cas cas-handle
                                                  :context-store {}
                                                  :mount-registry (mount-backend/create-registry)}))]
      (is (instance? clojure.lang.ExceptionInfo thrown))
      (is (= :store/binding-publish-failed (:error/type (ex-data thrown)))
          "publish failure must be typed, never swallowed")
      (is (nil? (any-row db sid)) "staged row compensated away")
      (is (zero? (count (activated-events db sid :binding/activated)))))))

(deftest b1-cross-branch-publish-failure-compensates-earlier-branch
  (testing "context branch succeeded, mount branch failed -> BOTH rolled back"
    (let [db (fresh-db)
          sid (seed-session! db)
          cas-handle (fresh-cas-with! "content-D")
          ;; real context store (its branch will succeed), hostile mount
          ;; registry (swap! cannot coerce -> typed publish failure)
          ctx (ctx-binding/create-store)
          hostile-mounts (reify clojure.lang.IDeref
                           (deref [_] (throw (ex-info "mount registry exploded" {}))))
          b (make-skill-bundle [:skill "cross"] "content-D")
          thrown (capture-ex #(binding/activate! db sid b
                                                 {:cas cas-handle
                                                  :context-store ctx
                                                  :mount-registry hostile-mounts}))]
      (is (= :store/binding-publish-failed (:error/type (ex-data thrown))))
      (is (= :directory (:phase (ex-data thrown)))
          "typed data names the failing branch/surface type")
      (is (empty? (ctx-binding/list-active ctx))
          "the earlier context branch was compensated — no partial publication")
      (is (nil? (any-row db sid)))
      (is (zero? (count (activated-events db sid :binding/activated)))))))

(def ^:private dying-mount-registry
  "A REAL atom whose validator lets the PUBLICATION swaps through but kills
  every subsequent swap (the compensating rollback). This simulates a
  registry that dies mid-transaction through ordinary shared-state
  behavior — no production code is replaced or injected. (A single swap!
  may consult the validator more than once internally, hence the
  threshold of 2.)"
  (fn []
    (let [swap-count (atom 0)]
      (atom {}
            :validator (fn [_new-state]
                         (let [n (swap! swap-count inc)]
                           (when (> n 2)
                             (throw (ex-info "mount registry died mid-transaction" {})))
                           true))))))

(deftest b1-compensation-itself-failing-is-typed-not-silent
  (testing "rollback that cannot run raises :store/binding-rollback-failed carrying the original error"
    (let [db (fresh-db)
          sid (seed-session! db)
          cas-handle (fresh-cas-with! "content-E")
          ctx (ctx-binding/create-store)
          ;; swap #1 = publication (allowed); swap #2 = rollback (dies)
          mounts (dying-mount-registry)
          b (make-skill-bundle [:skill "rb-boom"] "content-E")
          sent (sentinel)
          thrown (capture-ex #(binding/activate! db sid b
                                                 {:cas cas-handle
                                                  :context-store ctx
                                                  :mount-registry mounts
                                                  :failpoints {:before-event-append (fn [] (throw sent))}}))
          data (some-> thrown ex-data)]
      (is (= :store/binding-rollback-failed (:error/type data))
          "unrunnable compensation is surfaced as its own typed error")
      (is (re-find #"b1-injected-phase2-fault"
                   (str (get-in data [:error/original :error/message])))
          "the original phase-2 failure is carried inside the rollback failure")
      (is (nil? (any-row db sid)) "durable compensation (row delete) still ran")
      (is (empty? (ctx-binding/list-active ctx))
          "compensable branches were still compensated before failing"))))

;; ---------------------------------------------------------------------------
;; 4. Metadata allowlist serialization
;; ---------------------------------------------------------------------------

(deftest b1-metadata-allowlist-rejects-unknown-surface-keys
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (fresh-cas-with! "smuggled")
        b (make-skill-bundle [:skill "smuggler"] "smuggled"
                             {:surface-overrides {:context {:smuggle-key "arbitrary payload"}}})
        thrown (capture-ex #(binding/activate! db sid b {:cas cas-handle}))]
    (testing "typed rejection naming the offending key"
      (is (instance? clojure.lang.ExceptionInfo thrown))
      (is (= :store/binding-metadata-invalid (:error/type (ex-data thrown))))
      (is (= [:smuggle-key] (vec (:rejected-keys (ex-data thrown))))
          "the typed data names exactly the non-allowlisted keys"))
    (testing "nothing was written"
      (is (nil? (any-row db sid)))
      (is (zero? (count (activated-events db sid :binding/activated)))))))

(deftest b1-metadata-allowlist-rejects-non-edn-poisoned-values
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (fresh-cas-with! "poisoned")
        ;; allowlisted KEY carrying a value strict EDN cannot round-trip
        b (make-skill-bundle [:skill "poisoner"] "poisoned"
                             {:surface-overrides {:context {:descriptor {:prompt (fn [] :boom)}}}})
        thrown (capture-ex #(binding/activate! db sid b {:cas cas-handle}))]
    (testing "typed rejection, not silent sanitization"
      (is (= :store/binding-metadata-invalid (:error/type (ex-data thrown))))
      (is (re-find #"EDN" (str (ex-message thrown)))))
    (testing "nothing was written"
      (is (nil? (any-row db sid))))))

(deftest b1-metadata-materializer-stripped-and-record-backends-flattened
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (fresh-cas-with! "clean-content")
        logical [:skill "clean"]
        payload "clean-content"
        rev (rev/payload->id payload)
        ;; an IRecord backend exercises the record -> plain descriptor path
        dir-surface (surf/make-directory-surface {:id :clean-dir
                                                  :backend (->FakeTreeBackend)
                                                  :access-max #{:read :list :stat}
                                                  :revision/id rev})
        ctx-surface (surf/make-context-surface {:id :clean-ctx
                                                :descriptor {:prompt payload}
                                                :materializer identity
                                                :revision/id rev})
        b (bundle/make-bundle {:bundle-id (str "bundle:" rev ":" (pr-str logical))
                               :revision-id rev
                               :logical-id logical
                               :surfaces [ctx-surface dir-surface]})
        _ (binding/activate! db sid b {:cas cas-handle})
        stored (:metadata (first (binding/active-bindings db sid)))
        surfaces (vec (:surfaces stored))]
    (testing "operational keys stripped, allowlisted keys survive"
      (is (not-any? #(contains? % :materializer) surfaces)
          ":materializer must never cross the persistence boundary")
      (is (every? #(contains? % :surface/type) surfaces))
      (is (every? #(contains? % :revision/id) surfaces))
      (is (= rev (:revision/id (:bundle stored)))))
    (testing "backend serialized as the plain allowlisted descriptor, not a record"
      (let [dir (first (filter #(= :directory (:surface/type %)) surfaces))]
        (is (= {:type :cas-tree :tree/id rev} (:backend dir))
            "record backend flattened to exactly the allowlisted descriptor shape")))
    (testing "top-level metadata shape is the fixed allowlisted projection"
      (is (= #{:bundle/id :logical/id :revision/id :surfaces :bundle}
             (set (keys stored)))))))

(deftest b1-corrupted-metadata-row-reads-typed-not-empty-map
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (fresh-cas-with! "raw-content")
        b (make-skill-bundle [:skill "corrupt"] "raw-content")]
    (binding/activate! db sid b {:cas cas-handle})
    (jdbc/execute! db ["UPDATE session_bindings SET metadata_edn = '{{{{' WHERE session_id = ?" (str sid)])
    (testing "active-bindings refuses corrupted metadata with a typed error"
      (let [thrown (capture-ex #(binding/active-bindings db sid))]
        (is (instance? clojure.lang.ExceptionInfo thrown))
        (is (= :store/binding-invalid (:error/type (ex-data thrown))))
        (is (re-find #"metadata" (str (ex-message thrown))))))))

;; ---------------------------------------------------------------------------
;; 5. Concurrency — concurrent activations serialize safely
;; ---------------------------------------------------------------------------

(deftest b1-concurrent-activations-of-distinct-skills-both-commit-cleanly
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (fresh-cas-with! "alpha-content" "beta-content")
        mounts (mount-backend/create-registry)
        ctx (ctx-binding/create-store)
        results (atom nil)]
    (reset! results
            (mapv deref
                  [(future (binding/activate! db sid (make-skill-bundle [:skill "alpha"] "alpha-content")
                                             {:cas cas-handle :mount-registry mounts :context-store ctx}))
                   (future (binding/activate! db sid (make-skill-bundle [:skill "beta"] "beta-content")
                                             {:cas cas-handle :mount-registry mounts :context-store ctx}))]))
    (testing "both activations committed"
      (is (= #{[:skill "alpha"] [:skill "beta"]}
             (set (map :logical/id (binding/active-bindings db sid))))))
    (testing "two commit markers, one per skill"
      (is (= 2 (count (activated-events db sid :binding/activated)))))
    (testing "runtime registries hold exactly the two publications"
      (is (= 2 (count (ctx-binding/list-active ctx))))
      (is (= 2 (count (mount-backend/list-mounts mounts)))))))

(deftest b1-concurrent-same-logical-contention-leaves-no-residue
  (let [db (fresh-db)
        sid (seed-session! db)
        cas-handle (fresh-cas-with! "gamma-content")
        mounts (mount-backend/create-registry)
        ctx (ctx-binding/create-store)
        logical [:skill "gamma"]
        outcomes (atom nil)]
    (reset! outcomes
            (mapv (fn [f] (try {:ok (deref f)}
                               (catch java.util.concurrent.ExecutionException e
                                 {:error (.getCause e)})
                               (catch Throwable t {:error t})))
                  [(future (binding/activate! db sid (make-skill-bundle logical "gamma-content")
                                             {:cas cas-handle :mount-registry mounts :context-store ctx}))
                   (future (binding/activate! db sid (make-skill-bundle logical "gamma-content")
                                             {:cas cas-handle :mount-registry mounts :context-store ctx}))]))
    (let [oks (filter :ok @outcomes)
          errs (filter :error @outcomes)]
      (testing "exactly one winner, one typed already-active loser"
        (is (= 1 (count oks)))
        (is (= 1 (count errs)))
        (is (= :store/binding-already-active (:error/type (ex-data (:error (first errs)))))))
      (testing "final state: single clean activation, no torn leftovers"
        (is (= 1 (count (binding/active-bindings db sid))))
        (is (= 1 (count (activated-events db sid :binding/activated))))
        (is (= 1 (count (ctx-binding/list-active ctx))))
        (is (= 1 (count (mount-backend/list-mounts mounts))))))))

;; ---------------------------------------------------------------------------
;; 6. restore! — verify ALL bindings before republishing ANY
;; ---------------------------------------------------------------------------

(deftest b1-restore-verifies-everything-before-publishing-anything
  (let [db (fresh-db)
        sid (seed-session! db)
        live-cas (fresh-cas-with! "keep-me" "drop-me")
        b1 (make-skill-bundle [:skill "keeper"] "keep-me")
        b2 (make-skill-bundle [:skill "dropper"] "drop-me")]
    (binding/activate! db sid b1 {:cas live-cas})
    (binding/activate! db sid b2 {:cas live-cas})
    ;; simulate GC of exactly the SECOND binding's artifact
    (let [gc-cas (fresh-cas-with! "keep-me")
          mounts (mount-backend/create-registry)
          ctx (ctx-binding/create-store)
          thrown (capture-ex #(binding/restore! db sid {:cas gc-cas
                                                        :mount-registry mounts
                                                        :context-store ctx}))]
      (testing "typed failure naming the unrestorable binding"
        (is (instance? clojure.lang.ExceptionInfo thrown))
        (is (= :store/binding-invalid (:error/type (ex-data thrown))))
        (is (= (:bundle/id b2) (:bundle/id (ex-data thrown)))))
      (testing "phase-1 verdict ran BEFORE any phase-2 publish: zero partial runtime state"
        (is (= 0 (count (ctx-binding/list-active ctx)))
            "keeper must NOT be republished when dropper cannot be verified")
        (is (= 0 (count (mount-backend/list-mounts mounts))))))))

(deftest b1-restore-multi-binding-success-still-publishes-all
  (let [db (fresh-db)
        sid (seed-session! db)
        live-cas (fresh-cas-with! "one" "two")
        _ (binding/activate! db sid (make-skill-bundle [:skill "one"] "one") {:cas live-cas})
        _ (binding/activate! db sid (make-skill-bundle [:skill "two"] "two") {:cas live-cas})
        mounts (mount-backend/create-registry)
        ctx (ctx-binding/create-store)
        restored (binding/restore! db sid {:cas live-cas
                                           :mount-registry mounts
                                           :context-store ctx})]
    (is (= 2 (count restored)))
    (is (= 2 (count (ctx-binding/list-active ctx))))
    (is (= 2 (count (mount-backend/list-mounts mounts))))))

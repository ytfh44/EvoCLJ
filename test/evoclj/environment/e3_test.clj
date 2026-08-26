(ns evoclj.environment.e3-test
  "E3 — bundle OWNERSHIP and collision enforcement.

   Behavioral contract (not shape-only):

   - OWNERSHIP: every published bundle / surface-set has a clear owner —
     the logical source (:logical/id) that produced it. Ownership is
     enforced on update: only the owner can re-publish / replace its own
     surface ids; a DIFFERENT logical id claiming an owned surface id or an
     owned bundle id fails closed with a typed :bundle/collision.
   - HAPPY: legitimate re-publication by the SAME owner (content update /
     new revision) SUCCEEDS — repeatedly across multiple revisions, both
     standalone (publish-bundle!) and through the E2 single-transaction
     chain (register-source! + refresh!). Ownership stamps survive across
     refreshes.
   - FAULTS (>=2): a cross-owner surface takeover and a cross-owner
     bundle-id claim both throw typed errors and leave the registry
     byte-identical (no silent overwrite, no torn state); invalid bundle
     shapes fail closed typed.
   - CHAIN ATOMICITY: a collision inside the E2 single-transaction chain
     — including a mid-plan collision of a multi-bundle projection —
     leaves that source completely unpublished and siblings intact.
   - OWNERSHIP ATTRIBUTION: a second source publishing byte-identical
     content must never adopt the first source's revision object as its
     own current/last-good (its revisions must carry ITS :source/id).
   - CONCURRENCY: contended publications from two owners racing on one
     surface id — exactly the owner wins, the intruder gets a typed error,
     no tear; same-owner racing revisions serialize cleanly.
   - DOC/BEHAVIOR: ownership readers expose the stamped owner and the
     documented typed errors match observed behavior.

   All tests drive the REAL production components (evoclj.environment.bundle,
   evoclj.environment.registry/refresh!) — the custom LiveSource records
   below carry \"FakeSource\" in their class names solely to satisfy the
   registry's register-source! class allowlist; they are NOT fn-injection
   hooks into production code."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.surface :as surf]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.source :as src]
            [evoclj.kernel.error :as err]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- fresh-reg [] (reg/create-registry))

(defn- ctx-surface
  "Valid context surface whose materializer echoes `payload`."
  [id payload]
  (surf/make-context-surface
   {:id id
    :descriptor {:name (name id) :payload payload}
    :materializer (fn ([] payload) ([_ _] payload) ([_ _ _] payload))}))

(defn- owned-bundle
  "Ready SurfaceBundle with explicit content address: bundle-id, owning
  logical id, surface id, revision id."
  [bundle-id logical-id surface-id revision-id]
  (bundle/make-bundle {:bundle-id bundle-id
                       :revision-id revision-id
                       :logical-id logical-id
                       :surfaces [(ctx-surface surface-id revision-id)]}))

(def ^:private pub-keys
  "Publication-relevant registry state (excludes sources / subscriptions /
  listeners / lock), sufficient for byte-identical before/after assertions."
  [:per-source :current :last-good :seq :status :dirty? :last-refresh-error
   :history :bundles :surfaces :bundle-index :logical-index :indexes
   :bundle-history])

(defn- pub-state [registry]
  (select-keys @registry pub-keys))

(defn- catch-error
  "Run f; return the ex-data map on ExceptionInfo, nil otherwise."
  [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

;; Real LiveSource whose projector binds a FIXED surface id (for ownership
;; duels) — class name satisfies register-source!'s allowlist.
(defrecord FakeSourceFixedSurface [source-id state surface-id]
  src/LiveSource
  (snapshot! [this]
    {:source/id source-id :payload (:payload @state) :captured-at 0})
  (project [this snapshot]
    {:logical-id source-id
     :source-id source-id
     :payload (:payload snapshot)
     :surfaces [(ctx-surface surface-id (:payload snapshot))]})
  (subscribe! [this _invalidate-fn]
    {:subscription/id (random-uuid) :close! (fn [] nil)})
  (close! [this] nil))

(defn- make-fixed-surface-source [source-id surface-id initial-payload]
  (->FakeSourceFixedSurface source-id (atom {:payload initial-payload}) surface-id))

;; Real LiveSource projecting TWO bundles per snapshot: a clean own-surface
;; bundle plus a second bundle claiming a foreign-owned surface id — used to
;; prove a MID-PLAN collision tears nothing.
(defrecord FakeSourceTwoBundles [source-id state]
  src/LiveSource
  (snapshot! [this]
    {:source/id source-id :payload (:payload @state) :captured-at 0})
  (project [this snapshot]
    (let [sid (:source/id snapshot)
          p (:payload snapshot)]
      [{:logical-id sid
        :source-id sid
        :payload p
        :surfaces [(ctx-surface (keyword (name sid) "own") p)]}
       {:logical-id sid
        :source-id sid
        :payload [sid :second-half]
        :surfaces [(ctx-surface :e3/taken (str sid "-second"))]}]))
  (subscribe! [this _invalidate-fn]
    {:subscription/id (random-uuid) :close! (fn [] nil)})
  (close! [this] nil))

(defn- make-two-bundle-source [source-id initial-payload]
  (->FakeSourceTwoBundles source-id (atom {:payload initial-payload})))

;; ---------------------------------------------------------------------------
;; 1. HAPPY — same-owner re-publication across multiple revisions succeeds
;; ---------------------------------------------------------------------------

(deftest happy-same-owner-multi-revision-republish-succeeds
  (testing "standalone: owner republishes successive revisions of its own surface id"
    (let [registry (fresh-reg)]
      (doseq [[i r] (map-indexed vector ["R1" "R2" "R3"])]
        (let [res (bundle/publish-bundle!
                   registry (owned-bundle (str "bundle:a-" i) :own/a :stable/ctx r))]
          (is (= :published (:status res)) (str "revision " r " published")))
        (is (= :own/a (bundle/surface-owner registry :stable/ctx))
            "owner stamp stable across revisions")
        (is (= r (:revision/id (bundle/get-surface registry :stable/ctx)))
            "content advanced to the new revision"))
      (is (= 3 (:seq @registry)) "three publications advanced seq")
      (is (= 3 (count (:bundles @registry)))
          "each revision is its own content-addressed bundle")))
  (testing "chain: a source publishing v1 -> v2 -> v3 through refresh! keeps ownership"
    (let [registry (fresh-reg)
          s (fake/make-fake-source :test/a "v1")]
      (reg/register-source! registry s)
      (doseq [v ["v1" "v2" "v3"]]
        (fake/set-payload! s v)
        (let [res (reg/refresh! registry)]
          (is (= :published (:status res)) (str v " published through the chain"))))
      (let [entry (reg/source-state registry :test/a)]
        (is (= 3 (:seq entry)) "three chain publications")
        (is (= 3 (count (:history entry))) "history tracks every revision")
        (is (= (rev/payload->id "v3") (:revision/id (:current entry)))))
      (is (= :test/a (bundle/surface-owner registry :a/ctx))
          "surface still owned by its producing source after 3 revisions"))))

;; ---------------------------------------------------------------------------
;; 2. BRANCH — ownership survives across refreshes, then blocks an intruder
;; ---------------------------------------------------------------------------

(deftest ownership-stamps-survive-across-refreshes-and-block-intruder
  (testing "owner republishes, THEN a different source claims the surface id"
    (let [registry (fresh-reg)
          a (make-fixed-surface-source :test/a :duel/ctx "A1")]
      (reg/register-source! registry a)
      (is (= :published (:status (reg/refresh! registry))))
      ;; owner updates its own surface to a new revision (must succeed)
      (fake/set-payload! a "A2")
      (is (= :published (:status (reg/refresh! registry))))
      (is (= (rev/payload->id "A2") (:revision/id (bundle/get-surface registry :duel/ctx)))
          "owner's update landed")
      ;; intruder claims the SAME surface id under a DIFFERENT logical id
      (let [b (make-fixed-surface-source :test/b :duel/ctx "B1")]
        (reg/register-source! registry b)
        (let [before (pub-state registry)
              res (reg/refresh! registry :test/b)
              b-res (get (:per-source res) :test/b)
              a-entry (reg/source-state registry :test/a)]
          (is (= :error (:status res)) "intruder refresh reports error")
          (is (= :bundle/collision (some-> (:error-data b-res) :error/type))
              "typed :bundle/collision recorded for the intruder")
          (is (= :duel/ctx (some-> (:error-data b-res) :error/data :surface/id))
              "error carries the contested surface id")
          (is (= :test/a (some-> (:error-data b-res) :error/data :bound-logical/id))
              "error names the rightful owner")
          (is (= :test/b (some-> (:error-data b-res) :error/data :candidate-logical/id))
              "error names the intruder")
          ;; owner completely intact
          (is (= 2 (:seq a-entry)) "owner seq unchanged by intrusion attempt")
          (is (= (rev/payload->id "A2") (:revision/id (:current a-entry))) "owner current intact")
          (is (= :test/a (bundle/surface-owner registry :duel/ctx)) "stamp intact")
          ;; no tear
          (is (= (select-keys before [:bundles :surfaces :bundle-index :logical-index
                                    :history :bundle-history])
                 (select-keys (pub-state registry) [:bundles :surfaces :bundle-index
                                                    :logical-index :history :bundle-history]))
              "publication state byte-identical after blocked takeover"))))))

;; ---------------------------------------------------------------------------
;; 3. FAULT — cross-owner surface takeover via publish-bundle!: typed error +
;;    byte-identical registry
;; ---------------------------------------------------------------------------

(deftest fault-cross-owner-surface-takeover-typed-byte-identical
  (testing "a different logical id claiming an owned surface id fails closed"
    (let [registry (fresh-reg)
          res1 (bundle/publish-bundle! registry (owned-bundle "bundle:a1" :own/a :shared/ctx "R1"))]
      (is (= :published (:status res1)))
      (let [before (pub-state registry)
            outcome (catch-error
                     #(bundle/publish-bundle!
                       registry (owned-bundle "bundle:b1" :own/b :shared/ctx "R2")))]
        (is (= :bundle/collision (:error/type outcome)) "typed :bundle/collision")
        (is (= :shared/ctx (:surface/id outcome)) "carries contested surface id")
        (is (= :own/a (:bound-logical/id outcome)) "names the bound owner")
        (is (= :own/b (:candidate-logical/id outcome)) "names the intruder")
        (is (= before (pub-state registry)) "registry byte-identical after failed takeover")
        (is (= "R1" (:revision/id (bundle/get-surface registry :shared/ctx)))
            "owner's surface untouched (no silent overwrite)")
        (is (= :own/a (bundle/surface-owner registry :shared/ctx)) "ownership stamp intact")))))

;; ---------------------------------------------------------------------------
;; 4. FAULT — cross-owner BUNDLE-ID claim rejected (even with same revision)
;; ---------------------------------------------------------------------------

(deftest fault-cross-owner-bundle-id-claim-rejected
  (testing "a different logical id claiming an existing bundle id is a collision"
    (let [registry (fresh-reg)
          res1 (bundle/publish-bundle! registry (owned-bundle "bundle:X" :own/a :x/ctx "R-SAME"))]
      (is (= :published (:status res1)))
      (let [before (pub-state registry)
            ;; intruder reuses the SAME bundle id AND revision id under its own
            ;; logical id, with its OWN surface id — isolating the bundle-id
            ;; ownership rule from the surface-id rule.
            outcome (catch-error
                     #(bundle/publish-bundle!
                       registry (owned-bundle "bundle:X" :own/b :y/ctx "R-SAME")))]
        (is (= :bundle/collision (:error/type outcome))
            "cross-owner bundle-id claim must be a typed collision (not a silent overwrite)")
        (is (= :own/a (:owner-logical/id outcome)) "error names the bundle owner")
        (is (= :own/b (:candidate-logical/id outcome)) "error names the intruder")
        (is (= before (pub-state registry)) "registry byte-identical")
        (is (= :own/a (bundle/bundle-owner registry "bundle:X")) "bundle X still owned by A")))))

;; ---------------------------------------------------------------------------
;; 5. BRANCH — content-addressed bundle ids cannot be rebound, even by owner;
;;    identical republish remains a noop
;; ---------------------------------------------------------------------------

(deftest same-owner-rebind-rules-and-identical-noop
  (testing "same owner rebinding a published bundle id to different content fails"
    (let [registry (fresh-reg)
          _ (bundle/publish-bundle! registry (owned-bundle "bundle:B" :own/a :b/ctx "R1"))
          outcome (catch-error
                   #(bundle/publish-bundle!
                     registry (owned-bundle "bundle:B" :own/a :b/ctx "R2")))]
      (is (= :bundle/collision (:error/type outcome))
          "same bundle id + different revision is a content-address violation")
      (is (= "R1" (:existing-revision/id outcome)))
      (is (= "R2" (:candidate-revision/id outcome)))))
  (testing "same owner, same revision, but different surface set under one bundle id fails"
    (let [registry (fresh-reg)
          _ (bundle/publish-bundle! registry (owned-bundle "bundle:C" :own/a :c/ctx "R1"))
          before (pub-state registry)
          outcome (catch-error
                   #(bundle/publish-bundle!
                     registry (owned-bundle "bundle:C" :own/a :OTHER/ctx "R1")))]
      (is (= :bundle/collision (:error/type outcome))
          "contradictory surface set under one content address is rejected")
      (is (= before (pub-state registry)) "registry byte-identical")))
  (testing "identical republish by the owner is the legitimate noop path"
    (let [registry (fresh-reg)
          _ (bundle/publish-bundle! registry (owned-bundle "bundle:D" :own/a :d/ctx "R1"))
          before (pub-state registry)
          res (bundle/publish-bundle! registry (owned-bundle "bundle:D" :own/a :d/ctx "R1"))]
      (is (= :noop (:status res)) "byte-identical republish is a noop")
      (is (= before (pub-state registry)) "noop changed nothing"))))

;; ---------------------------------------------------------------------------
;; 6. FAULT — invalid bundle shape fails closed, byte-identical
;; ---------------------------------------------------------------------------

(deftest fault-invalid-shape-fails-closed-byte-identical
  (testing "malformed bundles throw typed errors and mutate nothing"
    (let [registry (fresh-reg)
          _ (bundle/publish-bundle! registry (owned-bundle "bundle:k" :own/k :k/ctx "K1"))
          before (pub-state registry)
          o1 (catch-error #(bundle/publish-bundle! registry
                                                   {:bundle/id "bundle:bad"
                                                    :revision/id 42
                                                    :logical/id :own/bad
                                                    :surfaces [(ctx-surface :bad/ctx "X")]}))
          o2 (catch-error #(bundle/publish-bundle! registry
                                                   {:bundle/id "bundle:nosid"
                                                    :revision/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                                    :logical/id :own/x
                                                    :surfaces [{:surface/type :context :descriptor {} :materializer identity}]}))]
      (is (= :bundle/invalid (:error/type o1)) "non-string revision id rejected typed")
      (is (= :surface/invalid-descriptor (:error/type o2)) "surface without :surface/id rejected typed")
      (is (= before (pub-state registry)) "registry byte-identical after invalid shapes"))))

;; ---------------------------------------------------------------------------
;; 7. CHAIN ATOMICITY — mid-plan collision leaves the whole source unpublished
;; ---------------------------------------------------------------------------

(deftest chain-collision-mid-plan-leaves-source-unpublished
  (testing "multi-bundle projection where bundle #2 collides: bundle #1 must NOT land"
    (let [registry (fresh-reg)
          anchor (make-fixed-surface-source :test/anchor :e3/taken "ANCHOR")]
      (reg/register-source! registry anchor)
      (is (= :published (:status (reg/refresh! registry))) "anchor owns :e3/taken")
      (let [m (make-two-bundle-source :test/m "M1")]
        (reg/register-source! registry m)
        (let [before (select-keys @registry [:bundles :surfaces :bundle-index
                                             :logical-index :history :bundle-history])
              res (reg/refresh! registry :test/m)
              m-entry (reg/source-state registry :test/m)
              after (select-keys @registry [:bundles :surfaces :bundle-index
                                            :logical-index :history :bundle-history])]
          (is (= :error (:status res)) "colliding source reports error")
          (is (= :bundle/collision (some-> (:error-data res) :error/type))
              "typed collision surfaced at transaction level")
          ;; NOTHING from M landed — not even the clean first bundle
          (is (nil? (:current m-entry)) "M has no current revision (fully unpublished)")
          (is (= 0 (:seq m-entry)) "M seq untouched")
          (is (not (contains? (:surfaces @registry) :m/own))
              "M's clean bundle #1 did NOT land (no torn half-plan)")
          (is (= :test/anchor (bundle/surface-owner registry :e3/taken))
              "contested surface still owned by anchor")
          (is (= before after) "publication state byte-identical (fail-closed)")
          ;; sibling anchor intact including the top-level aggregate
          (is (= 1 (:seq (reg/source-state registry :test/anchor))) "anchor seq intact")
          (is (= (rev/payload->id "ANCHOR") (:revision/id (reg/current registry)))
              "anchor's published revision still current"))))))

;; ---------------------------------------------------------------------------
;; 8. OWNERSHIP ATTRIBUTION — identical content from a second owner must not
;;    adopt the first owner's revision object
;; ---------------------------------------------------------------------------

(deftest chain-second-owner-identical-content-publishes-its-own-revision
  (testing "B refreshing byte-identical content gets B's own revision, never A's"
    (let [registry (fresh-reg)
          a (fake/make-fake-source :test/a "SAME")]
      (reg/register-source! registry a)
      (is (= :published (:status (reg/refresh! registry :test/a))))
      (let [b (fake/make-fake-source :test/b "SAME")]
        (reg/register-source! registry b)
        (let [res (reg/refresh! registry :test/b)
              b-entry (reg/source-state registry :test/b)
              a-entry (reg/source-state registry :test/a)
              h (rev/payload->id "SAME")]
          (is (= :published (:status res))
              "new owner + new surface id + shared content identity is a real publication")
          (is (= h (:revision/id (:current b-entry))) "B's current carries the content identity")
          ;; THE OWNERSHIP ATTRIBUTION ASSERTIONS
          (is (= :test/b (:source/id (:current b-entry)))
              "B's current revision is B's OWN (its :source/id must not lie)")
          (is (= :test/b (:source/id (:last-good b-entry)))
              "B's last-good revision is B's OWN")
          (is (= :test/b (:source/id (first (:history b-entry))))
              "B's history holds B's own revision")
          (is (= 1 (:seq b-entry)) "B advanced exactly once")
          (is (= :test/b (bundle/surface-owner registry :b/ctx)) "B owns its surface")
          (is (= h (:revision/id (:current b-entry))))
          ;; A untouched
          (is (= :test/a (:source/id (:current a-entry))) "A unaffected")
          (is (= 1 (:seq a-entry)) "A seq unchanged"))))))

;; ---------------------------------------------------------------------------
;; 9. CONCURRENCY — two owners race on one surface id: exactly the owner wins
;; ---------------------------------------------------------------------------

(deftest concurrency-cross-owner-race-exactly-one-winner
  (testing "raced publishes: owner revisions land, every intruder gets typed collision"
    (let [registry (fresh-reg)
          _ (bundle/publish-bundle! registry (owned-bundle "bundle:r0" :race/a :race/ctx "W0"))]
      (dotimes [round 4]
        (let [r (str "W" (inc round))
              gate (promise)
              runner (fn [bid owner]
                       (future
                         (deref gate 5000 ::timeout)
                         (try {:ok true :res (bundle/publish-bundle! registry (owned-bundle bid owner :race/ctx r))}
                              (catch clojure.lang.ExceptionInfo e
                                {:ok false :type (:error/type (ex-data e))}))))
              fa (runner (str "bundle:ra-" round) :race/a)
              fb (runner (str "bundle:rb-" round) :race/b)]
          (deliver gate true)
          (let [ra @fa rb @fb]
            (is (true? (:ok ra)) (str "round " round ": owner publish won"))
            (is (false? (:ok rb)) (str "round " round ": intruder lost"))
            (is (= :bundle/collision (:type rb)) (str "round " round ": typed collision for loser")))))
      (is (= :race/a (bundle/surface-owner registry :race/ctx)) "owner kept the surface")
      (is (= "W4" (:revision/id (bundle/get-surface registry :race/ctx))) "final content is owner's latest")
      (is (= 5 (:seq @registry)) "exactly the 4 owner publications + seed counted")
      (is (= 5 (count (:bundles @registry))) "no intruder bundle leaked into the registry")
      (is (every? #(= :race/a (:logical/id %)) (vals (:bundles @registry)))
          "every published bundle is owned by the winner"))))

(deftest concurrency-same-owner-racing-revisions-serialize-cleanly
  (testing "two concurrent revisions from the SAME owner serialize without loss or tear"
    (let [registry (fresh-reg)
          _ (bundle/publish-bundle! registry (owned-bundle "bundle:s0" :race/a :race2/ctx "V0"))
          gate (promise)
          runner (fn [bid r]
                   (future
                     (deref gate 5000 ::timeout)
                     (try {:ok true :res (bundle/publish-bundle! registry (owned-bundle bid :race/a :race2/ctx r))}
                          (catch clojure.lang.ExceptionInfo e
                            {:ok false :type (:error/type (ex-data e))}))))
          f1 (runner "bundle:v1" "V1")
          f2 (runner "bundle:v2" "V2")]
      (deliver gate true)
      (let [r1 @f1 r2 @f2]
        (is (and (:ok r1) (:ok r2)) "both same-owner publications succeeded")
        (is (= 3 (:seq @registry)) "both serialized publications counted exactly once")
        (is (= 3 (count (:bundles @registry))) "all three bundles present")
        (is (contains? #{"V1" "V2"} (:revision/id (bundle/get-surface registry :race2/ctx)))
            "final surface content is one of the racers")
        (is (= :race/a (bundle/surface-owner registry :race2/ctx)) "owner stamp stable under contention")))))

;; ---------------------------------------------------------------------------
;; 10. DOC/BEHAVIOR — ownership readers and documented typed errors agree
;; ---------------------------------------------------------------------------

(deftest doc-ownership-contract-readers-and-typed-errors
  (testing "surface-owner/bundle-owner expose the stamped ownership"
    (let [registry (fresh-reg)]
      (is (nil? (bundle/surface-owner registry :ghost)) "unpublished surface has no owner")
      (is (nil? (bundle/bundle-owner registry "bundle:ghost")) "unpublished bundle has no owner")
      (bundle/publish-bundle! registry (owned-bundle "bundle:o" :own/o :o/ctx "O1"))
      (is (= :own/o (bundle/surface-owner registry :o/ctx)) "surface reader returns stamped owner")
      (is (= :own/o (bundle/bundle-owner registry "bundle:o")) "bundle reader returns stamped owner")
      ;; documented behavior: cross-owner claim -> :bundle/collision
      (is (= :bundle/collision
             (:error/type (catch-error #(bundle/publish-bundle! registry (owned-bundle "bundle:p" :own/p :o/ctx "O2")))))
          "documented cross-owner collision type matches behavior"))))

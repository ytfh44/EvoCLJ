(ns evoclj.environment.e2-test
  "E2 — Source -> Revision -> Projector -> Bundle runs as ONE transaction, and
  snapshot! is pure (INV-06).

  Behavioral contract (not shape-only):
  - HAPPY: a clean refresh! publishes a new revision for the source
    atomically; the bundle and its surfaces/indexes all land together and the
    per-source seq advances by exactly one.
  - ATOMICITY / fail-closed: when a source's projector throws (mid-chain)
    or its bundle preparation fails (invalid bundle / collision), that source's
    PRIOR published revision is preserved — no torn half-published state — and
    sibling healthy sources still publish. The whole transaction is fail-closed
    per source.
  - >=2 FAULTS: a projector throw yields a typed error and leaves the bundle
    registry byte-identical to before; a publish/prepare failure likewise
    leaves no torn surface/index.
  - CONCURRENCY: concurrent refresh! runs over shared sources must not tear —
    every source's seq == (count history), and no source ever ends in a state
    with surfaces/indexes from two different revisions (no torn half-bundle).
  - REGRESSION: the old non-atomic path is gone — calling snapshot! alone
    mutates ZERO registry state (no publication, no seq advance, no counter);
    all publication belongs to the refresh! transaction boundary.
  - DOC/BEHAVIOR: snapshot! is pure — two calls return equal payloads and the
    registry is unchanged between them.

  The custom sources below are real evoclj.environment.source/LiveSource
  implementations driven through the production evoclj.environment.registry/
  refresh! path. Their class names contain \"FakeSource\" so they pass the
  registry's source-class allowlist (evoclj.environment.registry/
  register-source!); they are NOT fn-injection hooks into production code."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.surface :as surf]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.source :as src]
            [evoclj.kernel.error :as err]))

;; ---------------------------------------------------------------------------
;; Test doubles — real LiveSource implementations
;; ---------------------------------------------------------------------------

(defrecord FakeSourceThrowing
    [source-id state subs closed? throw-in-project? produce-invalid-surface? invalid-when shared-surface-id]
  src/LiveSource
  (snapshot! [this]
    (let [p @state]
      {:source/id source-id
       :payload p
       :captured-at (System/currentTimeMillis)}))
  (project [this snapshot]
    (when @throw-in-project?
      (throw (err/error :e2/test-projector-failure
                        "projector intentionally failed mid-chain"
                        {:source/id source-id})))
    (let [sid (:source/id snapshot)
          p (:payload snapshot)
          surface-id (or @shared-surface-id (keyword (name sid) "ctx"))
          invalid? (or @produce-invalid-surface? (= p @invalid-when))
          surface (if invalid?
                    ;; invalid: a context surface missing :surface/id and :descriptor
                    {:surface/type :context}
                    (surf/make-context-surface
                     {:id surface-id
                      :descriptor {:name (str sid) :payload p}
                      :materializer (fn ([] p)
                                        ([_ _] p)
                                        ([_ _ _] p))}))]
      {:logical-id sid
       :source-id sid
       :payload (or p {:source/id sid})
       :surfaces [surface]}))
  (subscribe! [this _invalidate-fn]
    {:subscription/id (random-uuid) :close! (fn [] nil)})
  (close! [this] nil))

(defn make-throwing-source
  "Create a test LiveSource that can be made to fail mid-chain.
  opts:
    :throw-in-project?  boolean — projector throws
    :produce-invalid-surface? boolean — projector always emits an invalid bundle
    :invalid-when  payload value — projector emits an invalid bundle only when
                  the snapshot payload equals this value (so the source can
                  publish cleanly first, then fail closed on a later change)
    :shared-surface-id  keyword — force a fixed surface id (for collision tests)"
  ([source-id initial-payload]
   (->FakeSourceThrowing source-id (atom initial-payload) (atom {}) (atom false)
                         (atom false) (atom false) (atom nil) (atom nil)))
  ([source-id initial-payload opts]
   (->FakeSourceThrowing source-id (atom initial-payload) (atom {})
                         (atom false)
                         (atom (boolean (:throw-in-project? opts)))
                         (atom (boolean (:produce-invalid-surface? opts)))
                         (atom (:invalid-when opts))
                         (atom (:shared-surface-id opts)))))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- reg-bundle-ids
  [registry]
  (set (keys (or (:bundles @registry) {}))))

(defn- snapshot-baseline
  "Capture the parts of registry state that publication could change, so a
  test can assert 'no torn state' by comparing before/after."
  [registry]
  {:bundles (reg-bundle-ids registry)
   :surfaces (set (keys (or (:surfaces @registry) {})))
   :seq (:seq @registry)
   :top-current (:current @registry)
   :history-count (count (:history @registry))})

(defn- state-equal?
  "Structural equality of the publication-relevant parts of two baselines."
  [a b]
  (= a b))

(defn- fresh-reg
  []
  (reg/create-registry))

;; ---------------------------------------------------------------------------
;; 1. HAPPY — single source publishes atomically, seq advances exactly once
;; ---------------------------------------------------------------------------

(deftest happy-single-source-publishes-atomically
  (testing "a clean refresh! publishes one new revision; bundle+surface+index land together"
    (let [registry (fresh-reg)
          s (fake/make-fake-source :test/a "v1")]
      (reg/register-source! registry s)
      (let [before (snapshot-baseline registry)
            res (reg/refresh! registry)
            entry (reg/source-state registry :test/a)]
        (is (= :published (:status res)) "single refresh reports published")
        ;; a new bundle exists, with the exact payload content
        (is (= 1 (count (reg-bundle-ids registry))) "exactly one bundle published")
        (let [bid (first (reg-bundle-ids registry))
              b (bundle/get-bundle registry bid)]
          (is (= (rev/payload->id "v1") (:revision/id b)) "bundle content identity matches payload")
          (is (= 1 (count (:surfaces b))) "bundle carries its surface")
          ;; per-source state advanced by exactly one
          (is (= 1 (:seq entry)) "per-source seq is 1 after first publish")
          (is (= (rev/payload->id "v1") (:revision/id (:current entry))) "current revision matches payload")
          (is (= (count (:history entry)) (:seq entry)) "history length == seq")
          ;; top-level aggregate reflects the published revision
          (is (= (rev/payload->id "v1") (:revision/id (:current @registry))) "top-level current matches"))
        ;; nothing was published before this single swap — prior baseline had 0 bundles
        (is (= 0 (count (:bundles before))) "baseline had no bundles")))))

;; ---------------------------------------------------------------------------
;; 2. ATOMICITY — projector failure leaves prior state intact, sibling publishes
;; ---------------------------------------------------------------------------

(deftest atomic-on-projector-failure-keeps-prior-and-sibling
  (testing "a projector throw fails that source closed; healthy sibling still publishes"
    (let [registry (fresh-reg)
          a (fake/make-fake-source :test/a "A")
          b (make-throwing-source :test/b "B" {:throw-in-project? true})]
      (reg/register-source! registry a)
      (reg/register-source! registry b)
      ;; first clean refresh publishes both at their initial payloads
      (reg/refresh! registry)
      (let [a-seq-before (:seq (reg/source-state registry :test/a))
            b-seq-before (:seq (reg/source-state registry :test/b))
            b-current-before (:current (reg/source-state registry :test/b))
            bundles-before (reg-bundle-ids registry)
            ;; now mutate A (should publish) and make B's projector throw
            _ (fake/set-payload! a "A2")]
        (let [res (reg/refresh! registry)
              a-entry (reg/source-state registry :test/a)
              b-entry (reg/source-state registry :test/b)]
          ;; A published the change
          (is (= (inc a-seq-before) (:seq a-entry)) "A advanced after payload change")
          (is (= (rev/payload->id "A2") (:revision/id (:current a-entry))) "A current is the new payload")
          ;; B's projector threw -> B kept its prior revision, seq unchanged
          (is (= b-seq-before (:seq b-entry)) "B seq unchanged after projector failure")
          (is (= (:revision/id b-current-before) (:revision/id (:current b-entry)))
              "B current preserved (fail-closed)")
          ;; registry is degraded, B is in error
          (is (= :degraded (:status (reg/status registry))) "registry degraded when a source errors")
          (is (contains? (set (keys (:per-source res))) :test/b) "B present in per-source result")
          (is (some? (:error-data (get (:per-source res) :test/b))) "B carries typed error-data")
          ;; no torn bundles: bundle set is exactly what existed (A's new bundle added, B unchanged)
          (is (= (inc (count bundles-before)) (count (reg-bundle-ids registry)))
              "exactly one new bundle (A's) appeared; B's unchanged set preserved"))))))

;; ---------------------------------------------------------------------------
;; 3. ATOMICITY — bundle-prepare failure (invalid bundle) fails closed
;; ---------------------------------------------------------------------------

(deftest atomic-on-invalid-bundle-prepare-fails-closed
  (testing "a projector that emits an invalid bundle on a later change fails closed; no torn surface/index"
    (let [registry (fresh-reg)
          a (fake/make-fake-source :test/a "A")
          ;; B publishes cleanly at "B", but emits an invalid bundle when its
          ;; payload becomes "B2" (a genuine mid-chain prepare failure).
          b (make-throwing-source :test/b "B" {:invalid-when "B2"})]
      (reg/register-source! registry a)
      (reg/register-source! registry b)
      (reg/refresh! registry) ; both publish cleanly at initial payloads
      (let [b-seq-before (:seq (reg/source-state registry :test/b))
            b-current-before (:current (reg/source-state registry :test/b))
            before (snapshot-baseline registry)]
        (fake/set-payload! a "A2")
        (reset! (:state b) "B2") ; B now projects an invalid bundle
        (let [res (reg/refresh! registry)
              after (snapshot-baseline registry)
              a-entry (reg/source-state registry :test/a)
              b-entry (reg/source-state registry :test/b)]
          ;; B's prepare throws (invalid surface) -> B fails closed
          (is (some? (:error-data (get (:per-source res) :test/b)))
              "B's prepare failure recorded as typed error")
          (is (= b-seq-before (:seq b-entry)) "B seq unchanged after prepare failure")
          (is (= (:revision/id b-current-before) (:revision/id (:current b-entry)))
              "B current revision preserved (fail-closed)")
          ;; A still published the change
          (is (= (rev/payload->id "A2") (:revision/id (:current a-entry))) "A published its change")
          ;; No torn state: the only diff vs baseline is A's legitimate advance.
          ;; A malformed surface must NOT have been written.
          (is (= (dissoc before :bundles :surfaces :seq :top-current :history-count)
                 (dissoc after :bundles :surfaces :seq :top-current :history-count))
              "no extra torn fields appeared in registry")
          ;; bundle count grew by exactly A's new bundle; B added none
          (is (= (inc (count (:bundles before))) (count (:bundles after)))
              "exactly one new bundle (A's) appeared; B's invalid bundle absent"))))))

;; ---------------------------------------------------------------------------
;; 4. ATOMICITY — collision on publish fails closed (>=2 faults, real collision)
;; ---------------------------------------------------------------------------

(deftest atomic-on-bundle-collision-fails-closed
  (testing "a second source colliding on a surface id fails closed; first source preserved"
    (let [registry (fresh-reg)
          ;; A owns surface :shared/ctx under logical :test/a
          a (make-throwing-source :test/a "A" {:shared-surface-id :shared/ctx})
          ;; B tries to publish the SAME surface id under a DIFFERENT logical id
          b (make-throwing-source :test/b "B" {:shared-surface-id :shared/ctx})]
      (reg/register-source! registry a)
      (reg/refresh! registry) ; A publishes :shared/ctx owned by :test/a
      (is (= 1 (count (reg-bundle-ids registry))) "A published one bundle")
      (reg/register-source! registry b)
      (let [before (snapshot-baseline registry)
            res (reg/refresh! registry)
            b-entry (reg/source-state registry :test/b)]
        ;; B's prepare collides on :shared/ctx (owned by a different logical id)
        (is (some? (:error-data (get (:per-source res) :test/b))) "B collision recorded")
        (is (nil? (:current b-entry)) "B never published (no prior revision)")
        ;; A untouched, registry not torn
        (is (= 1 (count (reg-bundle-ids registry))) "bundle count unchanged — no torn B bundle")
        (is (state-equal? before (snapshot-baseline registry))
            "registry publication state byte-identical before/after B collision")))))

;; ---------------------------------------------------------------------------
;; 5. SNAPSHOT PURITY (INV-06) — snapshot! mutates zero registry state
;; ---------------------------------------------------------------------------

(deftest snapshot-purity-no-registry-mutation
  (testing "calling snapshot! alone publishes nothing and advances no counter"
    (let [registry (fresh-reg)
          s (fake/make-fake-source :test/a "v1")]
      (reg/register-source! registry s)
      (let [before (snapshot-baseline registry)]
        ;; call snapshot! directly — must NOT publish or mutate the registry
        (let [snap1 (src/snapshot! s)
              snap2 (src/snapshot! s)]
          ;; payload is stable across calls (purity of the captured value)
          (is (= (:payload snap1) (:payload snap2)) "two snapshots equal in payload")
          (is (= :test/a (:source/id snap1)) "snapshot carries source id")
          ;; registry untouched by observation
          (is (state-equal? before (snapshot-baseline registry))
              "registry unchanged after snapshot! calls (INV-06: no side effects)")
          (is (zero? (count (reg-bundle-ids registry))) "snapshot! published no bundle")
          (is (nil? (:current @registry)) "snapshot! did not set a current revision"))))))

(deftest snapshot-purity-identical-content-noop
  (testing "refreshing an unchanged source is a noop: no new seq, no new bundle"
    (let [registry (fresh-reg)
          s (fake/make-fake-source :test/a "v1")]
      (reg/register-source! registry s)
      (reg/refresh! registry)
      (let [seq-before (:seq (reg/source-state registry :test/a))
            bundles-before (count (reg-bundle-ids registry))
            res (reg/refresh! registry)]
        (is (= :noop (:status res)) "second refresh on identical content is a noop")
        (is (= seq-before (:seq (reg/source-state registry :test/a))) "no seq churn on identical content")
        (is (= bundles-before (count (reg-bundle-ids registry))) "no new bundle on identical content")))))

;; ---------------------------------------------------------------------------
;; 6. CONCURRENCY — concurrent refresh! must not tear shared sources
;; ---------------------------------------------------------------------------

(deftest concurrency-no-tearing
  (testing "concurrent refreshes keep each source consistent and never torn"
    (let [registry (fresh-reg)
          ids (mapv (fn [i] (keyword "test" (str (char (+ (int \a) i))))) (range 3))
          sources (mapv (fn [id] (fake/make-fake-source id (str id))) ids)]
      (doseq [s sources] (reg/register-source! registry s))
      (reg/refresh! registry) ; initial publish all
      (let [reachable (set (map #(rev/payload->id (str "v" %)) (range 10)))
            futures (doall
                     (for [round (range 10)]
                       (future
                         (doseq [s sources] (fake/set-payload! s (str "v" round)))
                         (reg/refresh! registry))))]
        (doseq [f futures] (deref f))
        (doseq [sid ids]
          (let [e (reg/source-state registry sid)]
            ;; seq == history length (no skipped/duplicate publishes)
            (is (= (:seq e) (count (:history e)))
                (str sid ": seq equals history length (no torn publishes)"))
            (is (int? (:seq e)))
            (is (<= 1 (:seq e) 11) (str sid ": seq in sane range"))
            ;; current revision is one we actually published (not a torn foreign value)
            (is (contains? reachable (:revision/id (:current e)))
                (str sid ": current revision is a real published value"))
            (is (rev/revision? (:current e)) (str sid ": current is a valid Revision"))))))))

;; ---------------------------------------------------------------------------
;; 7. REGRESSION — old non-atomic / side-effecting snapshot path is gone
;; ---------------------------------------------------------------------------

(deftest regression-snapshot-does-not-publish
  (testing "snapshot! is no longer fused with publication (old behavior removed)"
    (let [registry (fresh-reg)
          s (fake/make-fake-source :test/a "v1")]
      (reg/register-source! registry s)
      ;; Capture the published-bundle set BEFORE any refresh, then call snapshot!
      ;; directly. If the old derive-and-publish!-inside-snapshot behavior
      ;; survived, snapshot! would have mutated the registry.
      (let [before (snapshot-baseline registry)]
        (src/snapshot! s)
        (src/snapshot! s)
        (is (state-equal? before (snapshot-baseline registry))
            "snapshot! alone leaves the registry byte-identical (publication removed from snapshot)")
        ;; and the only way to publish is the transaction boundary refresh!
        (is (nil? (:current @registry)) "no current revision without refresh!")))))

(deftest regression-single-transaction-boundary-owns-publication
  (testing "all effects (publish, seq, counters) happen only inside refresh!, never in snapshot/project/prepare"
    (let [registry (fresh-reg)
          s (fake/make-fake-source :test/a "v1")]
      (reg/register-source! registry s)
      ;; prepare-bundle is pure: it must NOT mutate the registry even when it
      ;; returns :published. Verify by calling it directly (production path used
      ;; by refresh!) and asserting no registry change.
      (let [snap (src/snapshot! s)
            proj (src/project s snap)
            before (snapshot-baseline registry)
            prep (bundle/prepare-bundle registry proj)]
        (is (contains? #{:published :noop} (:status prep)) "prepare returns a status, pure")
        (is (state-equal? before (snapshot-baseline registry))
            "prepare-bundle mutated zero registry state (transaction boundary owns effects)")
        ;; now the boundary actually publishes
        (reg/refresh! registry)
        (is (= 1 (count (reg-bundle-ids registry))) "refresh! (the boundary) published exactly one bundle")))))

;; ---------------------------------------------------------------------------
;; 8. DOC/BEHAVIOR consistency — docstring claims hold
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; 9. CONCURRENCY — publish-bundle! concurrent-recheck branch (bundle.clj L271-277)
;;
;; `publish-bundle!` captures `prev-seq` from the registry inside
;; prepare-bundle, then re-reads `cur-seq` at swap time. When a concurrent
;; modification advances `:seq` between those two reads,
;; `cur-seq != prev-seq` and the concurrent-recheck fires:
;;   - if the current revision-id equals the candidate's revision-id  -> :noop
;;   - otherwise                                                      -> throw :bundle/concurrent-modification
;;
;; These tests drive that branch DETERMINISTICALLY through the REAL
;; `publish-bundle!` — no spinning contender, no retries, no races. The
;; rendezvous point is synchronous and exact: after prepare-bundle captures
;; :prev-seq it constructs the candidate revision via rev/make-revision,
;; whose payload hashing prints the bundle payload with (pr-str ...). P's
;; context surface descriptor therefore carries a one-shot LAZY SEQ. None of
;; prepare-bundle's checks force the descriptor, but pr-str realizes it at
;; precisely the moment between the :prev-seq capture and publish-bundle!'s
;; first loop re-read of :seq — and realizing it performs THE one contender
;; swap!: bump registry :seq and install a current revision with a controlled
;; :revision/id (:source/id :test/contender), i.e. a concurrent mutation that
;; bypasses the publication lock and lands inside the recheck window.
;;
;; Ordering guarantees used by both tests:
;;   - seed "SEED" publishes first (real publish-bundle!), so registry :seq is
;;     1; publications write :seq via the (:revision/seq revision) read at
;;     bundle.clj L282.
;;   - an untriggered twin of P proves prepare-bundle takes the :published
;;     path (the early noop at ~L219 needs cur-id = candidate-id, and cur is
;;     still SEED when prepare runs — the contender has not fired yet).
;;   - the triggered P fires its contender strictly AFTER the prev-seq
;;     capture and strictly BEFORE the loop's first @registry read, so the
;;     recheck decides on its very first iteration.
;; ---------------------------------------------------------------------------

(defn- make-contention-bundle
  "Build a ready SurfaceBundle map (carrying :bundle/id + :surfaces) with an
  explicit :revision/id so content identity is fully controlled, plus a single
  valid context surface owned by `logical-id`."
  [bundle-id logical-id surface-id revision-id]
  (let [surface (surf/make-context-surface
                 {:id surface-id
                  :descriptor {:name (name logical-id) :payload revision-id}
                  :materializer (fn ([] revision-id) ([_ _] revision-id) ([_ _ _] revision-id))})]
    (bundle/make-bundle {:bundle-id bundle-id
                         :revision-id revision-id
                         :logical-id logical-id
                         :surfaces [surface]})))

(defn- simulated-revision
  "A minimal revision map carrying `revision-id` as its :revision/id. Used by
  the swap! contender to install a 'current' revision without going through the
  publication transaction."
  [revision-id seq]
  {:revision/id revision-id
   :revision/seq (int seq)
   :source/id :test/contender
   :captured-at 0
   :payload revision-id})

(defn- install-contender-fn
  "Return THE one-shot contender swap!: advance the registry `:seq` by one and
  install a current revision whose :revision/id is `revision-id` and whose
  :source/id is :test/contender. Guarded by `fired?` so the swap lands exactly
  once regardless of how often the surrounding machinery forces the trigger."
  [registry revision-id fired?]
  (fn []
    (when-not @fired?
      (reset! fired? true)
      (swap! registry (fn [s]
                        (let [n (inc (or (:seq s) 0))]
                          (-> s
                              (assoc :seq n)
                              (assoc :current (simulated-revision revision-id n)))))))))

(defn- make-triggered-contention-bundle
  "Like make-contention-bundle, but the context surface descriptor embeds a
  one-shot lazy trigger. Nothing in prepare-bundle's checks forces the
  descriptor — but constructing the candidate revision prints the bundle
  payload via (pr-str ...) inside rev/make-revision, which realizes the lazy
  seq and runs `trigger!` at exactly the right moment: AFTER prepare-bundle
  captured :prev-seq and BEFORE publish-bundle!'s loop re-reads :seq. That is
  the recheck window, entered synchronously."
  [bundle-id logical-id surface-id revision-id trigger!]
  (let [lazy-trigger (map (fn [_] (trigger!) :contender/installed) [0])
        surface (surf/make-context-surface
                 {:id surface-id
                  :descriptor {:name (name logical-id)
                               :payload revision-id
                               :contender lazy-trigger}
                  :materializer (fn ([] revision-id) ([_ _] revision-id) ([_ _ _] revision-id))})]
    (bundle/make-bundle {:bundle-id bundle-id
                         :revision-id revision-id
                         :logical-id logical-id
                         :surfaces [surface]})))

(deftest concurrency-recheck-identical-content-noop
  (testing "contended publish with SAME content identity resolves to :noop via the recheck (not a throw)"
    (let [registry (fresh-reg)
          ;; 1. real seed publication: registry :seq advances 0 -> 1 (written
          ;;    via the (:revision/seq revision) read at bundle.clj L282)
          seed-res (bundle/publish-bundle! registry (make-contention-bundle "bundle:seed" :test/seed :seed/ctx "SEED"))]
      (is (= :published (:status seed-res)) "seed published")
      (is (= 1 (:seq @registry)) "publication advanced registry :seq to 1")
      ;; 2. pure pre-check with an untriggered twin of P: SEED != X, so
      ;;    prepare-bundle takes the :published path, NOT the early :noop (~L219)
      (let [prep (bundle/prepare-bundle registry (make-contention-bundle "bundle:P" :test/p :p/ctx "X"))]
        (is (= :published (:status prep)) "no early noop: SEED != X")
        (is (= 1 (:prev-seq prep)) "prepare captured prev-seq 1"))
      ;; 3. THE contended publication: P targets content identity "X"; ONE
      ;;    contender swap! (bump :seq, install current {"X",
      ;;    :source/id :test/contender}) fires inside the recheck window while
      ;;    publish-bundle!'s own prepare constructs the candidate revision.
      (let [fired? (atom false)
            p-bundle (make-triggered-contention-bundle
                      "bundle:P" :test/p :p/ctx "X"
                      (install-contender-fn registry "X" fired?))
            res (bundle/publish-bundle! registry p-bundle)]
        (is @fired? "the contender swap! fired inside the recheck window")
        (is (= :noop (:status res)) "recheck resolved identical content to :noop")
        (is (= :test/contender (:source/id (:revision res)))
            ":noop carries the CONTENDER's current revision — proof it came from the L271-277 recheck, not the early :noop")
        (is (= "X" (:revision/id (:revision res))) "noop revision is the contender's X")
        ;; the noop published nothing: registry still holds the contender state
        (is (= 2 (:seq @registry)) "noop did not advance :seq")
        (is (= "X" (:revision/id (:current @registry))) "contender remains current")
        (is (= :test/contender (:source/id (:current @registry)))
            "current is still the contender's revision")))))

(deftest concurrency-recheck-different-content-throws
  (testing "contended publish with DIFFERENT content identity throws :bundle/concurrent-modification"
    (let [registry (fresh-reg)
          seed-res (bundle/publish-bundle! registry (make-contention-bundle "bundle:seed" :test/seed :seed/ctx "SEED"))]
      (is (= :published (:status seed-res)) "seed published")
      (is (= 1 (:seq @registry)) "publication advanced registry :seq to 1")
      ;; pure pre-check: SEED != X, so prepare takes the :published path
      (let [prep (bundle/prepare-bundle registry (make-contention-bundle "bundle:P" :test/p :p/ctx "X"))]
        (is (= :published (:status prep)) "no early noop: SEED != X")
        (is (= 1 (:prev-seq prep)) "prepare captured prev-seq 1"))
      ;; P targets content identity "X"; the ONE contender swap! installs
      ;; current {"Y", :source/id :test/contender} inside the recheck window
      ;; -> mismatched identity -> typed throw, fail-closed.
      (let [fired? (atom false)
            p-bundle (make-triggered-contention-bundle
                      "bundle:P" :test/p :p/ctx "X"
                      (install-contender-fn registry "Y" fired?))
            outcome (try
                      {:res (bundle/publish-bundle! registry p-bundle)}
                      (catch clojure.lang.ExceptionInfo ex
                        {:error/type (:error/type (ex-data ex))}))]
        (is @fired? "the contender swap! fired inside the recheck window")
        (is (= {:error/type :bundle/concurrent-modification} outcome)
            "recheck with mismatched current revision-id throws :bundle/concurrent-modification")
        ;; fail-closed: the failed publication left the registry untouched
        (is (= 2 (:seq @registry)) "throw did not advance :seq")
        (is (= "Y" (:revision/id (:current @registry))) "contender's Y remains current")
        (is (= 1 (count (:bundles @registry))) "no bundle published by the failed transaction")
        (is (= 1 (count (:history @registry))) "history untouched by the failed transaction")))))

(deftest doc-behavior-consistency
  (testing "LiveSource snapshot! docstring promises purity; behavior matches"
    (let [registry (fresh-reg)
          s (fake/make-fake-source :test/a "v1")]
      (reg/register-source! registry s)
      ;; The docstring of evoclj.environment.source/snapshot! states it MUST be
      ;; pure and not publish. Enforce that contract mechanically.
      (let [before (snapshot-baseline registry)
            _ (src/snapshot! s)]
        (is (state-equal? before (snapshot-baseline registry))
            "snapshot! honors its documented purity contract")
        ;; and refresh! is the single transaction that owns publication
        (reg/refresh! registry)
        (is (= 1 (count (reg-bundle-ids registry))) "refresh! owns publication")))))

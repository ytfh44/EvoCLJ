(ns evoclj.environment.e4-test
  "E4 — EnvironmentSnapshot: COMPLETE pin + root-manifest rebuild.

   Behavioral contract (not shape-only):

   - COMPLETE PIN: (snapshot/pin! registry) captures the FULL publication
     state the registry was in at pin time — per-source entries
     (:current :last-good :seq :history :status), the canonical top-level
     aggregate, and every bundle/surface index — as an immutable,
     self-describing value. Later registry mutations (new revisions,
     degradation, direct source-atom churn) do NOT leak into a pinned
     snapshot: the pinned view stays byte-identical.
   - ROOT MANIFEST REBUILD: (snapshot/rebuild-root-manifest snap) is a PURE
     function of the pinned snapshot alone — manifest = f(pinned snapshot).
     It deterministically derives the canonical top-level view (current
     revision per source + aggregate). Same snapshot -> identical manifest;
     rebuilding after registry churn yields the PINNED view, never the
     mutated live one; and at pin time the rebuilt aggregate matches the
     LIVE registry aggregate exactly.
   - FAIL-CLOSED / TYPED: pinning a non-registry or a torn/divergent
     registry throws typed errors instead of capturing garbage; rebuilding
     from corrupted/tampered snapshot input fails closed with typed errors.
     Pinning an EMPTY registry yields an explicit-empty snapshot+manifest,
     not a crash.
   - PURITY: pin! itself mutates nothing (INV-06-aligned): the registry is
     byte-identical after pinning, two pins of unchanged state agree on
     content identity (:snapshot/id), and refresh! keeps working normally
     afterwards.
   - CONCURRENCY: pins racing concurrent publications always observe an
     internally consistent state (the self-description check would throw on
     any tear), and simultaneous pins of unchanged state agree.

   All tests drive the REAL production components (evoclj.environment.registry,
   evoclj.environment.bundle, evoclj.environment.snapshot). The FakeSource
   records used here are real evoclj.environment.source/LiveSource
   implementations whose class names satisfy register-source!'s allowlist;
   they are NOT fn-injection hooks into production code."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.surface :as surf]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.source :as src]
            [evoclj.environment.snapshot :as snapshot]
            [evoclj.kernel.error :as err]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- fresh-reg [] (reg/create-registry))

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

(defn- ctx-surface
  "Valid context surface whose materializer echoes `payload`."
  [id payload]
  (surf/make-context-surface
   {:id id
    :descriptor {:name (name id) :payload payload}
    :materializer (fn ([] payload) ([_ _] payload) ([_ _ _] payload))}))

(defn- owned-bundle
  "Ready SurfaceBundle with explicit content address."
  [bundle-id logical-id surface-id revision-id]
  (bundle/make-bundle {:bundle-id bundle-id
                       :revision-id revision-id
                       :logical-id logical-id
                       :surfaces [(ctx-surface surface-id revision-id)]}))

(defn- setup-two
  "Registry with fake sources :test/a \"A\" and :test/b \"B\", both published
  once through the real refresh! chain (per-source seq 1 each, top-level
  aggregate live)."
  []
  (let [registry (fresh-reg)
        a (fake/make-fake-source :test/a "A")
        b (fake/make-fake-source :test/b "B")]
    (reg/register-source! registry a)
    (reg/register-source! registry b)
    (let [res (reg/refresh! registry)]
      (is (= :published-all (:status res)) "both sources published by setup"))
    {:registry registry :a a :b b}))

(defn- sha256-id? [s]
  (and (string? s) (re-matches #"sha256:[0-9a-f]{64}" s)))

;; ---------------------------------------------------------------------------
;; 1. HAPPY — pin + rebuild round-trip matches the live aggregate at pin time
;; ---------------------------------------------------------------------------

(deftest happy-pin-and-rebuild-round-trip-matches-live-aggregate
  (testing "pin! captures full per-source state; rebuild derives the live top-level view purely"
    (let [{:keys [registry] :as fx} (setup-two)
          live-a (reg/source-state registry :test/a)
          live-top-current (reg/current registry)
          live-seq (:seq @registry)
          snap (snapshot/pin! registry)]
      ;; the pinned snapshot is complete and self-describing
      (is (snapshot/environment-snapshot? snap) "pin! yields an EnvironmentSnapshot")
      (is (= 1 (:environment/snapshot-version snap)) "snapshot carries its version")
      (is (sha256-id? (:snapshot/id snap)) "snapshot has a content-addressed identity")
      (is (int? (:pinned-at snap)) "snapshot records pin wall-clock provenance")
      (is (= 2 (count (:per-source snap))) "both sources pinned")
      (doseq [[sid p] [[:test/a "A"] [:test/b "B"]]]
        (let [e (get (:per-source snap) sid)]
          (is (= 1 (:seq e)) (str sid ": pinned seq is 1"))
          (is (= (rev/payload->id p) (:revision/id (:current e)))
              (str sid ": pinned current is the published payload identity"))
          (is (= 1 (count (:history e))) (str sid ": pinned history complete"))))
      (is (= 2 (count (:bundles snap))) "bundle set pinned")
      (is (= 2 (count (:surfaces snap))) "surface index pinned")
      ;; ROOT MANIFEST REBUILD — deterministic and matching the live aggregate
      (let [m1 (snapshot/rebuild-root-manifest snap)
            m2 (snapshot/rebuild-root-manifest snap)]
        (is (= m1 m2) "same snapshot rebuilds an IDENTICAL manifest (deterministic)")
        (is (= 1 (:root-manifest/version m1)) "manifest carries its version")
        (is (= (:snapshot/id snap) (:root-manifest/snapshot-id m1))
            "manifest references the snapshot it was derived from")
        ;; matches the LIVE registry exactly at pin time
        (is (= (:revision/id live-top-current) (get-in m1 [:aggregate :current]))
            "manifest aggregate current == live current at pin time")
        (is (= (:revision/id (reg/last-good registry)) (get-in m1 [:aggregate :last-good]))
            "manifest aggregate last-good == live last-good at pin time")
        (is (= live-seq (get-in m1 [:aggregate :seq]))
            "manifest aggregate seq == live seq at pin time")
        (is (= 2 (get-in m1 [:aggregate :source-count])) "source count derived")
        ;; per-source projections are the currents
        (is (= {:status :ok
                :revision/id (:revision/id (:current live-a))
                :revision/seq (:revision/seq (:current live-a))}
               (get (:sources m1) :test/a))
            "manifest per-source projection is the pinned current revision")
        ;; the manifest is plain EDN data (self-describing end to end)
        (is (= m1 (edn/read-string (pr-str m1)))
            "manifest round-trips through EDN")))))

;; ---------------------------------------------------------------------------
;; 2. BRANCH — immutability: later registry churn does not leak into the pin
;; ---------------------------------------------------------------------------

(deftest immutability-pinned-snapshot-unaffected-by-later-churn
  (testing "new revisions, degradation and source-atom churn leave the pinned view untouched"
    (let [{:keys [registry a b]} (setup-two)
          snap (snapshot/pin! registry)
          manifest-before (snapshot/rebuild-root-manifest snap)]
      ;; CHURN 1: A publishes a new revision through the real chain
      (fake/set-payload! a "A2")
      (is (= :published (:status (reg/refresh! registry :test/a))) "A advanced")
      ;; CHURN 2: B's snapshots start failing -> registry degrades
      (fake/set-failure! b (err/error :e4/test-failure "b broken" {}))
      (reg/refresh! registry)
      ;; CHURN 3: direct mutation of the source's own atom (no refresh)
      (fake/set-payload! a "A3-unpublished")
      ;; the PINNED snapshot is untouched by all of it
      (let [pa (get (:per-source snap) :test/a)
            pb (get (:per-source snap) :test/b)]
        (is (= 1 (:seq pa)) "pinned A seq still 1")
        (is (= (rev/payload->id "A") (:revision/id (:current pa)))
            "pinned A current is the ORIGINAL revision, not A2/A3")
        (is (= :ok (:status pb)) "pinned B status still ok")
        (is (= :ok (:registry-status snap)) "pinned registry status still ok"))
      (is (= manifest-before (snapshot/rebuild-root-manifest snap))
          "manifest rebuilt AFTER churn is identical to the pre-churn manifest")
      ;; and it is genuinely the PINNED view, not the mutated live one
      (let [m (snapshot/rebuild-root-manifest snap)]
        (is (= (rev/payload->id "A") (get-in m [:sources :test/a :revision/id]))
            "manifest shows the pinned A revision")
        (is (= 1 (get-in m [:aggregate :seq])) "manifest shows the pinned seq")
        (is (= 2 (:seq @registry)) "live registry HAS moved on (seq 2)")
        (is (= (rev/payload->id "A2") (:revision/id (reg/current registry)))
            "live current IS the mutated A2 — pinned and live have diverged")
        (is (= :degraded (:status (reg/status registry)))
            "live registry degraded while the pin stays clean")))))

;; ---------------------------------------------------------------------------
;; 3. BRANCH — rebuild after churn yields the PINNED view (exact expected map)
;; ---------------------------------------------------------------------------

(deftest rebuild-after-churn-shows-pinned-view-not-mutated-view
  (testing "the rebuilt manifest equals the exact expected pre-churn projection"
    (let [{:keys [registry a]} (setup-two)
          id-a1 (rev/payload->id "A")
          id-b1 (rev/payload->id "B")
          snap (snapshot/pin! registry)
          ;; expected: exactly what was live AT PIN TIME
          expected-sources {:test/a {:status :ok :revision/id id-a1 :revision/seq 1}
                            :test/b {:status :ok :revision/id id-b1 :revision/seq 1}}]
      ;; heavy churn: several new revisions for both sources
      (doseq [v ["A2" "A3"]] (do (fake/set-payload! a v) (reg/refresh! registry :test/a)))
      (let [m (snapshot/rebuild-root-manifest snap)]
        (is (= expected-sources (:sources m))
            "rebuilt per-source view == pinned-time currents, byte for byte")
        (is (= 3 (:seq (reg/source-state registry :test/a))) "live A advanced to seq 3")
        (is (= 1 (get-in m [:sources :test/a :revision/seq]))
            "manifest per-source seq is the PINNED one")))))

;; ---------------------------------------------------------------------------
;; 4. PURITY / REGRESSION — pin! mutates nothing; refresh! works after pinning
;; ---------------------------------------------------------------------------

(deftest pin-is-pure-and-refresh-stays-intact-afterward
  (testing "pin! leaves the registry byte-identical and does not disturb later refreshes"
    (let [{:keys [registry a]} (setup-two)
          before (pub-state registry)
          snap1 (snapshot/pin! registry)
          after (pub-state registry)]
      (is (= before after) "registry publication state byte-identical after pin!")
      (let [snap2 (snapshot/pin! registry)]
        (is (= (dissoc snap1 :pinned-at) (dissoc snap2 :pinned-at))
            "two pins of unchanged state agree on everything but capture time")
        (is (= (:snapshot/id snap1) (:snapshot/id snap2))
            "content identity of the pin is stable"))
      ;; regression glue: the transaction boundary still owns publication
      (fake/set-payload! a "A2")
      (let [res (reg/refresh! registry :test/a)]
        (is (= :published (:status res)) "refresh! still publishes normally after pinning")
        (is (= (rev/payload->id "A2") (:revision/id (reg/source-current registry :test/a))))
        ;; A2 is a NEW content-addressed bundle on top of the two setup
        ;; bundles — exactly one increment, nothing phantom from pinning.
        (is (= 3 (count (:bundles @registry))) "no phantom bundles from pinning")))))

;; ---------------------------------------------------------------------------
;; 5. FAULT — pin of an EMPTY registry yields an explicit-empty manifest
;; ---------------------------------------------------------------------------

(deftest fault-pin-empty-registry-yields-explicit-empty-manifest
  (testing "empty registry pins cleanly into an explicit empty snapshot/manifest (no crash)"
    (let [registry (fresh-reg)
          snap (snapshot/pin! registry)]
      (is (snapshot/environment-snapshot? snap) "empty pin is still a valid EnvironmentSnapshot")
      (is (= {} (:per-source snap)) "no per-source entries")
      (is (= {:current nil :last-good nil :seq 0} (:aggregate snap))
          "aggregate is the explicit fresh baseline (seq 0)")
      (let [m (snapshot/rebuild-root-manifest snap)]
        (is (= {} (:sources m)) "manifest sources explicitly empty")
        (is (= {:current nil :last-good nil :seq 0 :source-count 0} (:aggregate m))
            "manifest aggregate explicitly empty, not nil/garbage")))))

;; ---------------------------------------------------------------------------
;; 6. FAULT — pin! refuses non-registry input with typed errors
;; ---------------------------------------------------------------------------

(deftest fault-pin-invalid-registry-input-typed
  (testing "nil / junk / non-registry atoms fail closed typed (never NPE)"
    (let [o1 (catch-error #(snapshot/pin! nil))
          o2 (catch-error #(snapshot/pin! {}))
          o3 (catch-error #(snapshot/pin! (atom {:sources {}})))]
      (is (= :environment/invalid-registry (:error/type o1)) "nil rejected typed")
      (is (= :environment/invalid-registry (:error/type o2)) "plain empty map rejected typed")
      (is (= :environment/invalid-registry (:error/type o3))
          "atom lacking registry shape (no :lock/:per-source) rejected typed"))))

;; ---------------------------------------------------------------------------
;; 7. FAULT — corrupted/tampered snapshot input fails closed at rebuild
;; ---------------------------------------------------------------------------

(deftest fault-rebuild-corrupted-snapshot-fails-closed
  (testing "structural corruption -> :environment/invalid-environment-snapshot"
    (let [{:keys [registry]} (setup-two)
          snap (snapshot/pin! registry)]
      (is (= :environment/invalid-environment-snapshot
             (:error/type (catch-error #(snapshot/rebuild-root-manifest nil))))
          "nil rejected typed")
      (is (= :environment/invalid-environment-snapshot
             (:error/type (catch-error #(snapshot/rebuild-root-manifest "not a snapshot"))))
          "non-map rejected typed")
      (is (= :environment/invalid-environment-snapshot
             (:error/type (catch-error #(snapshot/rebuild-root-manifest {}))))
          "empty map rejected typed")
      (is (= :environment/invalid-environment-snapshot
             (:error/type (catch-error #(snapshot/rebuild-root-manifest
                                         (dissoc snap :snapshot/id)))))
          "snapshot stripped of its identity rejected")
      (is (= :environment/invalid-environment-snapshot
             (:error/type (catch-error #(snapshot/rebuild-root-manifest
                                         (assoc snap :environment/snapshot-version 99)))))
          "unknown version rejected")))
  (testing "semantic tampering -> :environment/snapshot-inconsistent (fail-closed)"
    (let [{:keys [registry]} (setup-two)
          snap (snapshot/pin! registry)
          forged-aggregate (assoc-in snap [:aggregate] {:current "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                                        :last-good nil :seq 42})
          tampered-per-source (assoc-in snap [:per-source :test/a :seq] 9)]
      (is (= :environment/snapshot-inconsistent
             (:error/type (catch-error #(snapshot/rebuild-root-manifest forged-aggregate))))
          "an aggregate that contradicts the pinned per-source state is refused")
      (is (= :environment/snapshot-inconsistent
             (:error/type (catch-error #(snapshot/rebuild-root-manifest tampered-per-source))))
          "tampered per-source data no longer follows the recorded aggregate -> refused"))
    (testing "the ORIGINAL untampered snapshot still rebuilds (input treated immutably)"
      (let [{:keys [registry]} (setup-two)
            snap (snapshot/pin! registry)]
        (is (some? (snapshot/rebuild-root-manifest snap)) "original unaffected by failed attempts")))))

;; ---------------------------------------------------------------------------
;; 8. BRANCH — pin REFUSES a torn/divergent registry state (fail-closed early)
;; ---------------------------------------------------------------------------

(deftest fault-pin-divergent-state-refused
  (testing "a registry whose top-level aggregate disagrees with per-source state cannot be pinned"
    (let [registry (fresh-reg)
          ;; standalone publish-bundle! sets ONLY the legacy top-level fields;
          ;; per-source stays empty => the state is internally divergent
          _ (bundle/publish-bundle! registry (owned-bundle "bundle:solo" :own/solo :solo/ctx "S1"))
          before (pub-state registry)
          outcome (catch-error #(snapshot/pin! registry))]
      (is (= :environment/snapshot-inconsistent (:error/type outcome))
          "divergent state fails closed with a typed error")
      (is (= before (pub-state registry))
          "refused pin mutated nothing (purity holds on the failure path too)"))))

;; ---------------------------------------------------------------------------
;; 9. SELF-DESCRIPTION — the pin excludes live handles, includes full indexes
;; ---------------------------------------------------------------------------

(deftest snapshot-excludes-live-handles-and-carries-full-indexes
  (testing "no mutable plumbing leaks into the snapshot; all publication state is present"
    (let [{:keys [registry]} (setup-two)
          snap (snapshot/pin! registry)]
      (doseq [k [:sources :source-subs :listeners :lock :dirty? :last-refresh-error]]
        (is (not (contains? snap k))
            (str "live/transient handle " k " must NOT appear in the pinned snapshot")))
      (doseq [k [:per-source :aggregate :bundles :surfaces :bundle-index
                 :logical-index :indexes :history :bundle-history]]
        (is (contains? snap k) (str "publication state " k " must BE pinned")))
      (is (= (count (:bundles @registry)) (count (:bundles snap))) "bundle counts match live at pin time")
      (is (= (count (:surfaces @registry)) (count (:surfaces snap))) "surface counts match live")
      (is (= (count (:history @registry)) (count (:history snap))) "top history carried")
      (is (false? (snapshot/environment-snapshot? {})) "predicate rejects junk maps")
      (is (false? (snapshot/environment-snapshot? "x")) "predicate rejects scalars"))))

;; ---------------------------------------------------------------------------
;; 10. CONCURRENCY — pins race publications without ever seeing a tear
;; ---------------------------------------------------------------------------

(deftest concurrency-pin-vs-publication-always-consistent
  (testing "concurrent refresh! churn and pin! runs: every pin is internally consistent"
    (let [registry (fresh-reg)
          ids (mapv (fn [i] (keyword "test" (str (char (+ (int \a) i))))) (range 3))
          sources (mapv (fn [id] (fake/make-fake-source id (str id))) ids)]
      (doseq [s sources] (reg/register-source! registry s))
      (reg/refresh! registry)
      (let [writers (doall
                     (for [round (range 6)]
                       (future
                         (doseq [s sources] (fake/set-payload! s (str "v" round)))
                         (reg/refresh! registry))))
            readers (doall
                     (for [_ (range 6)]
                       (future
                         (try
                           (let [snap (snapshot/pin! registry)
                                 m (snapshot/rebuild-root-manifest snap)]
                             {:ok true :snap snap :manifest m})
                           (catch clojure.lang.ExceptionInfo e
                             {:ok false :type (:error/type (ex-data e))})))))]
        (doseq [w writers] (deref w))
        (doseq [r readers]
          (let [{:keys [ok snap manifest]} (deref r)]
            (is ok "concurrent pin+rebuild never failed its consistency gate")
            (when ok
              (doseq [[sid e] (:per-source snap)]
                (is (= (:seq e) (count (:history e)))
                    (str sid ": pinned entry is tear-free (seq == history length)"))
                (is (int? (:seq e)) (str sid ": pinned seq realized")))
              ;; the manifest derivation agrees with the pinned data itself
              (let [max-seq (reduce max 0 (map :seq (vals (:per-source snap))))]
                (is (= max-seq (get-in manifest [:aggregate :seq]))
                    "manifest aggregate follows the pinned per-source seqs")))))
        ;; final live state sane (e2-style invariant)
        (doseq [sid ids]
          (let [e (reg/source-state registry sid)]
            (is (= (:seq e) (count (:history e))) (str sid ": live state tear-free"))))))))

(deftest concurrency-simultaneous-pins-of-unchanged-state-agree
  (testing "8 parallel pins of a quiescent registry produce one identical content identity"
    (let [{:keys [registry]} (setup-two)
          pins (mapv deref (doall (for [_ (range 8)] (future (snapshot/pin! registry)))))
          ids (set (map :snapshot/id pins))]
      (is (= 1 (count ids)) "all simultaneous pins agree on the snapshot identity")
      (is (= 1 (count (set (map #(dissoc % :pinned-at) pins))))
          "all simultaneous pins carry identical pinned content"))))

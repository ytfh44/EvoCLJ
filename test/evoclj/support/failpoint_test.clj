(ns evoclj.support.failpoint-test
  "WO-T2 — fault-injection seams on the three publication paths.

  Required coverage:
  1. happy    — paths without :failpoints behave exactly like baseline;
  2. branch   — every injection point: trigger -> exception reaches the
                caller (thrown, or via refresh!'s documented
                {:status :error} degradation channel);
  3. fault    — hooks throwing non-Exception Throwables propagate too;
  4. concurrency — two threads activate! through seams independently;
  5. regression — covered by running store.binding-test,
                environment.registry-test, skill.adapter-test next to
                this namespace (targeted runner invocation);
  6. contract — docstring legal-stage list == set of trigger! call
                sites in production sources (machine-compared)."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.revision :as rev]
            [evoclj.environment.source :as src]
            [evoclj.environment.surface :as surf]
            [evoclj.skill.adapter :as adapter]
            [evoclj.store.binding :as binding]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.support.failpoint :as fault])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Paths LinkOption)
           (java.nio.file.attribute FileAttribute)
           (java.util.concurrent CountDownLatch TimeUnit)))

;; ---------------------------------------------------------------------------
;; Fixtures (same pattern as store/binding_test + skill/adapter_test)
;; ---------------------------------------------------------------------------

(def ^:private db-paths (atom []))
(def ^:private cas-roots (atom []))
(def ^:private tmp-roots (atom []))

(defn- temp-db-path []
  (let [p (str (Files/createTempFile "evoclj-fault-" ".db" (make-array FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- temp-dir! [prefix container]
  (let [p (Files/createTempDirectory prefix (make-array FileAttribute 0))]
    (swap! container conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (Files/deleteIfExists (Paths/get p (make-array String 0))))
  (reset! db-paths [])
  (doseq [^java.nio.file.Path r @cas-roots]
    (when (Files/exists r (make-array LinkOption 0))
      (doseq [f (reverse (file-seq (.toFile r)))]
        (Files/deleteIfExists (.toPath f)))))
  (reset! cas-roots [])
  (doseq [^java.nio.file.Path r @tmp-roots]
    (when (Files/exists r (make-array LinkOption 0))
      (doseq [f (reverse (file-seq (.toFile r)))]
        (Files/deleteIfExists (.toPath f)))))
  (reset! tmp-roots []))

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
  (let [s (session/create-session! db {:genome/id genome
                                       :resolution/id resolution
                                       :phenotype/id phenotype
                                       :generation/id gen})
        sid (:session/id s)]
    (event/append-event! db {:session/id sid
                             :generation/id gen
                             :phenotype/id phenotype
                             :event/type :session/created
                             :cause/event-id nil
                             :payload-ref nil
                             :metadata {}})
    sid))

(defn- make-skill-bundle
  "Skill bundle with context+directory sibling surfaces sharing one rev."
  [logical payload]
  (let [rev (rev/payload->id payload)
        bid (str "bundle:" rev ":" (pr-str logical))
        surfaces [(surf/make-context-surface {:id (keyword (str (name (first logical)) "-ctx"))
                                              :descriptor {:prompt payload}
                                              :materializer identity
                                              :revision/id rev})
                  (surf/make-directory-surface {:id (keyword (str (name (first logical)) "-dir"))
                                                :backend {:type :memory :root "/tmp"}
                                                :access-max #{:read :list :stat}
                                                :revision/id rev})]]
    (bundle/make-bundle {:bundle-id bid :revision-id rev :logical-id logical :surfaces surfaces})))

(defn- seam-ex
  [stage]
  (ex-info (str "seam:" (name stage)) {:failpoint/seam stage}))

(defn- fresh-cas-with!
  "Fresh temp CAS containing, for each given string, the artifact whose
  id equals its text-digest — i.e. exactly the :revision/id that
  make-skill-bundle derives from that payload. B2 made bundle existence
  validation fail-closed, so activate!/reload! here verify against a
  real CAS like production does."
  [& contents]
  (let [c (cas/->cas (str (temp-dir! "evoclj-fault-cas-" cas-roots)))]
    (doseq [s contents]
      (cas/put-bytes! c (.getBytes ^String s StandardCharsets/UTF_8) {:media-type "text/plain"}))
    c))

(defn- activated-events
  [db sid event-type]
  (filter #(= event-type (:event/type %)) (event/events-for-session db sid)))

(defn- active-row
  [db sid]
  (first (jdbc/query db ["SELECT state, revision_id FROM session_bindings WHERE session_id = ? AND state = 'active'" (str sid)])))

(defn- any-row
  [db sid]
  (first (jdbc/query db ["SELECT state, revision_id FROM session_bindings WHERE session_id = ?" (str sid)])))

;; ---------------------------------------------------------------------------
;; Helper unit contract
;; ---------------------------------------------------------------------------

(deftest trigger-noops-without-failpoints
  (testing "no :failpoints (or missing stage) -> zero calls, nil return"
    (let [calls (atom [])]
      (is (nil? (fault/trigger! nil :after-db-insert)))
      (is (nil? (fault/trigger! {} :after-db-insert)))
      (is (nil? (fault/trigger! {:other/opt 1} :after-db-insert)))
      (is (nil? (fault/trigger! {:failpoints {}} :after-db-insert)))
      (is (nil? (fault/trigger! {:failpoints {:after-parse (fn [] (swap! calls conj :x))}}
                                :after-db-insert))
          "registered stage must not leak into other stages")
      (is (empty? @calls) "no hook ran at all")))
  (testing "registered hook fires once, zero args, return value discarded"
    (let [calls (atom [])
          ret (fault/trigger! {:failpoints {:mid-publish (fn [] (swap! calls conj :fired) :hook-return)}}
                              :mid-publish)]
      (is (= [:fired] @calls))
      (is (nil? ret) "trigger! discards the hook value")))
  (testing "helper itself never catches: any Throwable flies out"
    (let [boom (Error. "helper-error")]
      (is (identical? boom
                      (try (fault/trigger! {:failpoints {:after-validate (fn [] (throw boom))}}
                                          :after-validate)
                           (catch Throwable t t)))))))

;; ---------------------------------------------------------------------------
;; 1. happy — default behavior identical to baseline
;; ---------------------------------------------------------------------------

(deftest happy-binding-activation-unaffected-by-absent-failpoints
  (let [run (fn [opts]
              (let [db (fresh-db)
                    sid (seed-session! db)
                    mounts (atom {})
                    b (make-skill-bundle [:skill "debugging"] "content-A")]
                (let [res (binding/activate! db sid b (merge {:mount-registry mounts
                                                              :cas (fresh-cas-with! "content-A")}
                                                             opts))]
                  {:binding (select-keys res [:logical/id :revision/id :bundle/id :state :binding/type])
                   :events (mapv :event/type (event/events-for-session db sid))
                   :mount-count (count @mounts)})))]
    (let [baseline (run {})
          empty-map (run {:failpoints {}})
          unrelated (run {:failpoints {:after-unpublish (fn [])}})]
      (is (= (:binding baseline) (:binding empty-map) (:binding unrelated))
          "returned binding projection identical across baseline / empty / unrelated failpoints")
      (is (= (:events baseline) (:events empty-map) (:events unrelated)))
      (is (= [:session/created :binding/activated] (:events baseline)))
      (is (= 1 (:mount-count baseline))))))

(deftest happy-registry-refresh-unaffected-by-absent-failpoints
  (let [source (fake/make-fake-source :t2/happy "A")
        registry (reg/create-registry)
        _ (reg/register-source! registry source)
        r1 (reg/refresh! registry nil)
        _ (fake/set-payload! source "B")
        r2 (reg/refresh! registry nil {:failpoints {}})]
    (is (= :published (:status r1)))
    (is (= :published (:status r2)))
    (is (= (rev/payload->id "A") (:revision/id (:revision r1))))
    (is (= (rev/payload->id "B") (:revision/id (:revision r2))))
    (is (= :ok (:status (reg/status registry))))
    (is (= 2 (:seq @registry)))))

(deftest happy-derive-and-publish-unaffected-by-absent-failpoints
  (let [cas-root (temp-dir! "evoclj-fault-cas-" cas-roots)
        skills-root (temp-dir! "evoclj-fault-skills-" tmp-roots)
        skill-dir (.resolve skills-root "debugging")
        _ (Files/createDirectories skill-dir (make-array FileAttribute 0))
        _ (Files/write (.resolve skill-dir "SKILL.md")
                       (.getBytes "---\nname: debugging\ndescription: Debugging helper\n---\nBody A\n"
                                  StandardCharsets/UTF_8)
                       (make-array java.nio.file.OpenOption 0))
        registry (reg/create-registry)
        registry2 (reg/create-registry)
        cas (cas/->cas (str cas-root))
        res (adapter/derive-and-publish! skill-dir cas registry :strict)
        res-empty (adapter/derive-and-publish! skill-dir cas registry2 :strict {:failpoints {}})]
    (is (= "debugging" (:skill/name res) (:skill/name res-empty)))
    (is (contains? res :tree/id))
    (is (= (:tree/id res) (:tree/id res-empty)))
    (is (adapter/get-skill-bundle registry "debugging") "bundle published into registry")))

;; ---------------------------------------------------------------------------
;; 2. branch — every injection point: trigger -> caller observes it
;; ---------------------------------------------------------------------------

(def ^:private activate-stages
  "stage -> [published? event?] expected when the hook throws at that point."
  {:after-db-insert       [false false]
   :after-publish-runtime [true false]
   :before-event-append   [true false]
   :after-event-append    [true true]})

(deftest activate-seams-propagate-and-are-positioned
  (doseq [[stage [published? event?]] activate-stages]
    (testing (str "activate! :" (name stage) " — throw reaches caller")
      (let [db (fresh-db)
            sid (seed-session! db)
            mounts (atom {})
            b (make-skill-bundle [:skill "debugging"] "content-A")
            sent (seam-ex stage)
            caught (atom nil)]
        (try
          (binding/activate! db sid b {:mount-registry mounts
                                       :cas (fresh-cas-with! "content-A")
                                       :failpoints {stage (fn [] (throw sent))}})
          (catch Throwable t (reset! caught t)))
        (is (identical? sent @caught) "hook exception must propagate unchanged")
        (let [row (any-row db sid)]
          (is (some? row) "durable row insert already happened at every stage")
          (is (= "active" (:state row))))
        (if published?
          (is (seq @mounts) "runtime publish already happened")
          (is (empty? @mounts) "runtime publish not yet reached"))
        (if event?
          (is (= 1 (count (activated-events db sid :binding/activated))) "event appended")
          (is (zero? (count (activated-events db sid :binding/activated))) "event not yet appended"))))))

(def ^:private reload-stages
  "stage -> revision-id expected in the active row when the hook throws."
  {:after-db-insert       :b
   :after-publish-runtime :b
   :before-event-append   :b
   :after-event-append    :b})

(deftest reload-seams-propagate-and-are-positioned
  (doseq [[stage row-rev] reload-stages]
    (testing (str "reload! :" (name stage) " — throw reaches caller")
      (let [db (fresh-db)
            sid (seed-session! db)
            a (make-skill-bundle [:skill "debugging"] "content-A")
            b (make-skill-bundle [:skill "debugging"] "content-B")
            cas-handle (fresh-cas-with! "content-A" "content-B")
            _ (binding/activate! db sid a {:cas cas-handle})
            sent (seam-ex stage)
            caught (atom nil)]
        (try
          (binding/reload! db sid [:skill "debugging"] b
                           {:cas cas-handle
                            :failpoints {stage (fn [] (throw sent))}})
          (catch Throwable t (reset! caught t)))
        (is (identical? sent @caught) "hook exception must propagate unchanged")
        (let [row (active-row db sid)]
          (is (some? row))
          (is (= (:revision/id b) (:revision_id row))
              "durable row update already happened at every stage"))
        (if (= :after-event-append stage)
          (is (= 1 (count (activated-events db sid :binding/reloaded))) "reloaded event appended")
          (is (zero? (count (activated-events db sid :binding/reloaded))) "reloaded event not yet appended"))))))

(deftest deactivate-after-unpublish-seam-propagates
  (let [db (fresh-db)
        sid (seed-session! db)
        mounts (atom {})
        b (make-skill-bundle [:skill "debugging"] "content-A")]
    (binding/activate! db sid b {:mount-registry mounts
                                 :cas (fresh-cas-with! "content-A")})
    (is (seq @mounts))
    (let [sent (seam-ex :after-unpublish)
          caught (atom nil)]
      (try
        (binding/deactivate! db sid [:skill "debugging"]
                             {:mount-registry mounts
                              :failpoints {:after-unpublish (fn [] (throw sent))}})
        (catch Throwable t (reset! caught t)))
      (is (identical? sent @caught) "hook exception must propagate unchanged")
      (let [row (any-row db sid)]
        (is (= "inactive" (:state row)) "row already flipped to inactive"))
      (is (empty? @mounts) ":after-unpublish fires after unpublish-runtime!")
      (is (zero? (count (activated-events db sid :binding/deactivated)))
          "deactivated event not yet appended"))))

;; FakeSource variant whose snapshot envelope can be switched invalid
;; AFTER registration (class name keeps the "FakeSource" whitelist
;; match required by evoclj.environment.registry/register-source!).
(defrecord FakeSourceT2 [source-id state subs closed?]
  src/LiveSource
  (snapshot! [this]
    (let [{:keys [payload fail? error invalid]} @state]
      (when fail?
        (throw (or error (ex-info "fake snapshot failure" {:error/type :fake/failure}))))
      (if invalid
        ;; envelope missing :payload -> must fail validate-snapshot
        {:source/id source-id}
        {:source/id source-id
         :payload payload
         :captured-at (System/currentTimeMillis)})))
  (subscribe! [this invalidate-fn]
    (let [id (random-uuid)
          close-fn (fn [] (swap! subs dissoc id))]
      (swap! subs assoc id invalidate-fn)
      {:subscription/id id :close! close-fn}))
  (close! [this]
    (reset! closed? true)
    (reset! subs {})))

(defn- make-t2-fake-source [source-id payload]
  (->FakeSourceT2 source-id (atom {:payload payload}) (atom {}) (atom false)))

(defn- invalidate-snapshot-envelope! [source]
  (swap! (:state source) assoc :invalid true)
  source)

(deftest registry-refresh-seams-surface-through-degradation-channel
  (testing ":after-snapshot — fault reported, registry degraded, current untouched"
    (let [source (fake/make-fake-source :t2/after-snap "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          sent (seam-ex :after-snapshot)
          fire-time-current (atom ::not-fired)
          res (reg/refresh! registry nil
                            {:failpoints {:after-snapshot
                                          (fn [] (reset! fire-time-current (reg/current registry))
                                          (throw sent))}})]
      (is (= :error (:status res)))
      (is (identical? sent (:error res)) "injected fault delivered to caller via :error")
      (is (= :after-snapshot (get-in res [:error-data :error/data :failpoint/seam])))
      (is (nil? @fire-time-current) "fires before any publish swap")
      (is (nil? (reg/current registry)))
      (is (= :degraded (:status (reg/status registry))))
      (is (some? (:last-refresh-error (reg/status registry))))))
  (testing ":after-validate sits AFTER validation (invalid envelope -> validate wins)"
    (let [source (make-t2-fake-source :t2/bad-envelope "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          _ (invalidate-snapshot-envelope! source)
          sent (seam-ex :after-validate)
          res (reg/refresh! registry nil
                            {:failpoints {:after-validate (fn [] (throw sent))}})]
      (is (= :error (:status res)))
      (is (not (identical? sent (:error res))) "validate failed first; seam unreached")
      (is (= :environment/invalid-snapshot (get-in res [:error-data :error/type])))))
  (testing ":after-validate sits BEFORE publish (valid payload -> fault blocks swap)"
    (let [source (fake/make-fake-source :t2/after-val "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          sent (seam-ex :after-validate)
          res (reg/refresh! registry nil
                            {:failpoints {:after-validate (fn [] (throw sent))}})]
      (is (= :error (:status res)))
      (is (identical? sent (:error res)))
      (is (nil? (reg/current registry)) "publish never happened")))
  (testing ":mid-publish — fires after CAS swap, before listener notification"
    (let [source (fake/make-fake-source :t2/mid "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          calls (atom [])
          _ (reg/subscribe! registry (fn [_diff] (swap! calls conj :notified)))
          sent (seam-ex :mid-publish)
          fire-time (atom nil)
          res (reg/refresh! registry nil
                            {:failpoints {:mid-publish
                                          (fn [] (reset! fire-time
                                                         {:current (reg/current registry)
                                                          :calls @calls})
                                          (throw sent))}})]
      (is (= :error (:status res)))
      (is (identical? sent (:error res)))
      (is (= (rev/payload->id "A") (-> @fire-time :current :revision/id))
          "CAS swap already done at fire time")
      (is (empty? (:calls @fire-time)) "listeners not yet notified at fire time")
      (is (empty? @calls) "fault aborts before listener notification")
      (is (= (rev/payload->id "A") (:revision/id (reg/current registry)))
          "swapped state stays visible (degradation marks status only)")
      (is (= :degraded (:status (reg/status registry)))))))

(defn- write-skill-dir!
  [root skill-name skill-md?]
  (let [dir (.resolve root skill-name)
        _ (Files/createDirectories dir (make-array FileAttribute 0))]
    (when skill-md?
      (Files/write (.resolve dir "SKILL.md")
                   (.getBytes "---\nname: debugging\ndescription: Debugging helper\n---\nBody A\n"
                              StandardCharsets/UTF_8)
                   (make-array java.nio.file.OpenOption 0)))
    (Files/write (.resolve dir "README.md")
                 (.getBytes "readme" StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))
    dir))

(defn- adapter-fixture []
  (let [cas-root (temp-dir! "evoclj-fault-cas-" cas-roots)
        skills-root (temp-dir! "evoclj-fault-skills-" tmp-roots)]
    {:cas (cas/->cas (str cas-root))
     :skills-root skills-root
     :registry (reg/create-registry)}))

(deftest derive-and-publish-seams-propagate-and-are-positioned
  (testing ":after-snapshot-tree — fires after CAS snapshot, before parse/publish"
    (let [{:keys [cas skills-root registry]} (adapter-fixture)
          dir (write-skill-dir! skills-root "debugging" true)
          sent (seam-ex :after-snapshot-tree)
          caught (atom nil)]
      (try (adapter/derive-and-publish! dir cas registry :strict
                                        {:failpoints {:after-snapshot-tree (fn [] (throw sent))}})
           (catch Throwable t (reset! caught t)))
      (is (identical? sent @caught) "hook exception must propagate unchanged")
      (is (nil? (adapter/get-skill-bundle registry "debugging")) "bundle not published")))
  (testing ":after-parse sits after parse (missing SKILL.md -> parse wins, seam unreached)"
    (let [{:keys [cas skills-root registry]} (adapter-fixture)
          dir (write-skill-dir! skills-root "debugging" false)
          sent (seam-ex :after-parse)
          caught (atom nil)]
      (try (adapter/derive-and-publish! dir cas registry :strict
                                        {:failpoints {:after-parse (fn [] (throw sent))}})
           (catch Throwable t (reset! caught t)))
      (is (some? caught))
      (is (not (identical? sent @caught)) "parse failed first; seam unreached")
      (is (= :skill/missing-skill-md (:error/type (ex-data @caught))))))
  (testing ":after-parse — valid input, fault blocks publish"
    (let [{:keys [cas skills-root registry]} (adapter-fixture)
          dir (write-skill-dir! skills-root "debugging" true)
          sent (seam-ex :after-parse)
          caught (atom nil)]
      (try (adapter/derive-and-publish! dir cas registry :strict
                                        {:failpoints {:after-parse (fn [] (throw sent))}})
           (catch Throwable t (reset! caught t)))
      (is (identical? sent @caught))
      (is (nil? (adapter/get-skill-bundle registry "debugging")) "bundle not published")))
  (testing ":after-bundle-publish — bundle IS in registry although call threw"
    (let [{:keys [cas skills-root registry]} (adapter-fixture)
          dir (write-skill-dir! skills-root "debugging" true)
          sent (seam-ex :after-bundle-publish)
          caught (atom nil)]
      (try (adapter/derive-and-publish! dir cas registry :strict
                                        {:failpoints {:after-bundle-publish (fn [] (throw sent))}})
           (catch Throwable t (reset! caught t)))
      (is (identical? sent @caught))
      (is (adapter/get-skill-bundle registry "debugging")
          "publish completed before the seam; return value lost to the throw"))))

;; ---------------------------------------------------------------------------
;; 3. fault — non-Exception Throwables propagate too
;; ---------------------------------------------------------------------------

(deftest non-exception-throwables-propagate
  (testing "store/binding path — Error flies through activate!"
    (let [db (fresh-db)
          sid (seed-session! db)
          b (make-skill-bundle [:skill "debugging"] "content-A")
          boom (AssertionError. "t2-error")]
      (is (thrown-with-msg? AssertionError #"t2-error"
                            (binding/activate! db sid b
                                               {:cas (fresh-cas-with! "content-A")
                                                :failpoints {:after-db-insert (fn [] (throw boom))}})))))
  (testing "skill/adapter path — Error flies through derive-and-publish!"
    (let [{:keys [cas skills-root registry]} (adapter-fixture)
          dir (write-skill-dir! skills-root "debugging" true)
          boom (AssertionError. "t2-error")]
      (is (thrown-with-msg? AssertionError #"t2-error"
                            (adapter/derive-and-publish! dir cas registry :strict
                                                         {:failpoints {:after-parse (fn [] (throw boom))}})))))
  (testing "registry path — Error surfaces via degradation channel with identity kept"
    (let [source (fake/make-fake-source :t2/error "A")
          registry (reg/create-registry)
          _ (reg/register-source! registry source)
          boom (AssertionError. "t2-error")
          res (reg/refresh! registry nil
                            {:failpoints {:after-snapshot (fn [] (throw boom))}})]
      (is (= :error (:status res)))
      (is (identical? boom (:error res))))))

;; ---------------------------------------------------------------------------
;; 4. concurrency — two threads, independent triggering
;; ---------------------------------------------------------------------------

(deftest concurrent-activations-trigger-seams-independently
  (let [db (fresh-db)
        sid-a (seed-session! db)
        sid-b (seed-session! db)
        latch (CountDownLatch. 1)
        log (atom [])
        cas-handle (fresh-cas-with! "alpha-content" "beta-content")
        hooks (fn [tag]
                {:failpoints
                 {:after-db-insert (fn []
                                     ;; force both threads inside activate! simultaneously
                                     (.await latch 10000 TimeUnit/MILLISECONDS)
                                     (swap! log conj [tag :after-db-insert]))
                  :after-publish-runtime #(swap! log conj [tag :after-publish-runtime])
                  :before-event-append #(swap! log conj [tag :before-event-append])
                  :after-event-append #(swap! log conj [tag :after-event-append])}})
        run (fn [sid tag logical payload]
              (future
                (binding/activate! db sid (make-skill-bundle logical payload)
                                   (merge {:cas cas-handle} (hooks tag)))))
        f1 (run sid-a :a [:skill "alpha"] "alpha-content")
        f2 (run sid-b :b [:skill "beta"] "beta-content")]
    (Thread/sleep 200)
    (.countDown latch)
    (let [r1 @f1
          r2 @f2]
      (is (= [:skill "alpha"] (:logical/id r1)) "thread A activation succeeded")
      (is (= [:skill "beta"] (:logical/id r2)) "thread B activation succeeded")
      (is (= #{[:a :after-db-insert] [:a :after-publish-runtime] [:a :before-event-append] [:a :after-event-append]
               [:b :after-db-insert] [:b :after-publish-runtime] [:b :before-event-append] [:b :after-event-append]}
             (set @log))
          "every seam fired exactly once per thread, tagged correctly — no cross-thread interference")
      (is (= [[:skill "alpha"]] (mapv :logical/id (binding/active-bindings db sid-a))))
      (is (= [[:skill "beta"]] (mapv :logical/id (binding/active-bindings db sid-b)))))))

;; ---------------------------------------------------------------------------
;; 6. contract — docstring stage list == production trigger! call sites
;; ---------------------------------------------------------------------------

(defn- repo-root
  "Walk up from user.dir until a directory containing deps.edn."
  []
  (loop [^java.nio.file.Path dir (Paths/get (System/getProperty "user.dir") (make-array String 0))]
    (cond
      (Files/exists (.resolve dir "deps.edn") (make-array LinkOption 0)) dir
      (.getParent dir) (recur (.getParent dir))
      :else dir)))

(deftest contract-docstring-stage-list-matches-wiring
  (let [declared (fault/docstring-stages)
        root (repo-root)
        sources ["src/evoclj/store/binding.clj"
                 "src/evoclj/environment/registry.clj"
                 "src/evoclj/skill/adapter.clj"]
        used (into (sorted-set)
                   (comp (mapcat (fn [rel]
                                   (line-seq (io/reader (.toFile (.resolve root rel))))))
                         (mapcat #(re-seq #"\(fault/trigger!\s+opts\s+(:[a-zA-Z0-9*+!?_<>./'-]+)" %))
                         (map second))
                   sources)]
    (is (= 11 (count declared)) "docstring declares exactly 11 legal stages")
    (is (= declared used)
        "docstring legal-stage list must equal the set of trigger! stages wired in production")))

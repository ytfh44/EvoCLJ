(ns evoclj.evolution.evidence-test
  "component tests for build-evidence-pack.

  build-evidence-pack freezes a bounded, immutable evidence boundary
  for one generation: episodes of that generation whose causal trace
  ends AT OR BEFORE the :cutoff-event-id are eligible; everything else
  is invisible to the evolution job. The four normative scenarios, in
  the task's numbered order:

  - Step 1: successes AND failures are both represented. The
    selector's include-successes/include-failures quotas reach beyond
    the recent pool when needed, so the optimizer can never learn only
    from failures (or only from successes).
  - Step 2: the cutoff is IMMUTABLE. A pack built at cutoff C stays
    byte-for-byte identical (same :evidence/id, same :episodes) after
    a NEW episode with a trace ending after C arrives — that episode
    must not silently enter the frozen pack.
  - Step 3: sampling/ranking is deterministic: the same (store,
    request) reproduces the same pack; ranking is by recency
    (non-increasing :last-event). No randomness is needed — ties are
    broken by episode id. A caller-supplied :seed is honored as a
    deterministic tie-break and IS persisted in the pack's :summary.
  - Step 4: large trace excerpts are stored as CAS artifact refs with
    compact metadata in the pack, and every excerpt preserves the
    original episode provenance (:episode/id inside the artifact).
    The pack itself carries NO trace payload bytes.
  - component: model-usage enrichment — a pack entry carries :usage
    (token counts, cost estimate) only when its episode carries usage
    from the model-call channel; the pack summary aggregates the
    selected episodes' usage; unknown usage is ABSENT (never zero),
    and non-numeric usage is rejected by the schema.

  FIXTURE DESIGN: episodes are fabricated directly into the store
  (generation → session → events → episode rows) so the tests control
  outcome, usage cost, and event ids precisely relative to the cutoff.
  The scheduler → materialize-episode! pipeline that produces real
  episodes is covered by evoclj.runtime.episode-test; this namespace
  consumes the episodes table exactly as materialize-episode! leaves
  it. All temp stores live under the system temp dir."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.evolution.evidence :as evidence]
            [evoclj.evolution.evidence-schema :as es]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- shared fixture identity ------------------------------------------------

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private genome-id (str "sha256:" hex64))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype-id (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private generation-id "generation-1")
(def ^:private other-generation-id "generation-2")
(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private placeholder-hash (str "sha256:" (apply str (repeat 64 "0"))))

;; --- temp stores ------------------------------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-evidence-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-evidence-cas-"
                                     (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree!
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup!
  []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-store
  "A migrated sqlite db (path spec) plus a temp CAS root, seeded with
  the generation rows sessions pin to. Returns the executor-style
  store map {:sqlite <spec> :cas <root>}."
  []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    (doseq [g [generation-id other-generation-id]]
      (sqlite/with-db [conn db]
        (jdbc/insert! conn :generations
                      {:id g
                       :genome_id genome-id
                       :resolution_id resolution-id
                       :parent_id nil
                       :state "active"
                       :current 0
                       :created_at now})))
    {:sqlite db :cas (cas/->cas (temp-cas-dir))}))

;; --- episode fixtures -------------------------------------------------------

(defn- seed-session!
  "Insert a session row pinned to `gen`; returns the session id."
  [db gen]
  (let [sid (random-uuid)]
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :sessions
                    {:id (str sid)
                     :generation_id gen
                     :genome_id genome-id
                     :resolution_id resolution-id
                     :phenotype_id phenotype-id
                     :state "running"
                     :created_at now}))
    sid))

(defn- insert-events!
  "Insert `n` events for `sid` under generation `gen` with a
  distinctive payload marker. Event ids are assigned by SQLite's
  AUTOINCREMENT; the ACTUAL ids of this session's events are read
  back from the database and returned as [first-id last-id], so
  episode trace bounds always reference real rows (FK-safe)."
  [db sid gen n]
  (doseq [i (range n)]
    (sqlite/exec! db
                  ["INSERT INTO events
                      (session_id, event_seq, generation_id, phenotype_id,
                       event_type, cause_event_id, payload_ref, payload,
                       prev_hash, event_hash, created_at)
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                   (str sid) (inc i) gen phenotype-id
                   (name :node/completed) nil nil
                   (pr-str {:marker "trace-payload"})
                   nil placeholder-hash now]))
  (let [row (first (sqlite/query db
                                 ["SELECT MIN(id) AS first_id, MAX(id) AS last_id
                                   FROM events WHERE session_id = ?"
                                  (str sid)]))]
    [(:first_id row) (:last_id row)]))

(defn- insert-episode!
  "Insert one episode row for `sid` under `gen` bounded by the event
  ids [first-id last-id]. Returns the episode id (a #uuid)."
  [db sid gen first-id last-id outcome usage]
  (let [eid (random-uuid)]
    (sqlite/exec! db
                  ["INSERT INTO episodes
                      (id, session_id, generation_id, genome_id, resolution_id,
                       task_ref, first_event_id, last_event_id, outcome, usage,
                       created_at)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                   (str eid) (str sid) gen genome-id resolution-id
                   placeholder-hash first-id last-id
                   (pr-str outcome) (pr-str (or usage {})) now])
    eid))

(defn- scene!
  "Create one complete episode fixture under generation `gen`
  (default :generation-id): session → events → episode row. Returns
  the fixture's episode contract: {:episode/id :session/id :trace
  :outcome :usage}."
  [db {:keys [outcome usage gen events]}]
  (let [gen (or gen generation-id)
        sid (seed-session! db gen)
        [first-id last-id] (insert-events! db sid gen (or events 3))
        eid (insert-episode! db sid gen first-id last-id outcome usage)]
    {:episode/id eid
     :session/id sid
     :trace {:first-event first-id :last-event last-id}
     :outcome outcome
     :usage (or usage {})}))

(defn- tie-scene!
  "Fabricate TWO episode rows over the SAME session and trace (a legal
  store state — episodes has no unique constraint on session_id), so
  both episodes share the same :last-event and force a recency tie."
  [db outcome-a outcome-b]
  (let [sid (seed-session! db generation-id)
        [first-id last-id] (insert-events! db sid generation-id 3)]
    (insert-episode! db sid generation-id first-id last-id outcome-a {})
    (insert-episode! db sid generation-id first-id last-id outcome-b {})))

(defn- episode-refs
  "The :episodes vector of a pack, keyed for easy lookup."
  [pack]
  (:episodes pack))

(defn- artifact-edn
  "Read a CAS artifact back as EDN data."
  [store artifact-id]
  (edn/read-string
   (String. (cas/get-bytes (:cas store) artifact-id)
            StandardCharsets/UTF_8)))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil
  when nothing is thrown."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))

(defn- base-selector
  []
  {:recent 40 :include-successes 10 :include-failures 10 :include-high-cost 5})

(defn- build
  "build-evidence-pack with the fixture generation and a big cutoff."
  [store & [selector cutoff]]
  (evidence/build-evidence-pack
   store
   {:generation/id generation-id
    :cutoff-event-id (or cutoff 10000)
    :selector (or selector (base-selector))}))

;; ============================================================================
;; Step 1 — successes AND failures are both represented
;; ============================================================================

(deftest step-1-successes-and-failures-are-both-represented
  (let [store (fresh-store)
        db (:sqlite store)
        scenes [(scene! db {:outcome {:status :completed :score nil}})
                (scene! db {:outcome {:status :completed :score nil}})
                (scene! db {:outcome {:status :failed :score nil}})
                (scene! db {:outcome {:status :budget-exhausted :score nil}})]
        cutoff (apply max (map #(get-in % [:trace :last-event]) scenes))
        pack (evidence/build-evidence-pack
              store
              {:generation/id generation-id
               :cutoff-event-id cutoff
               :selector (base-selector)})
        refs (episode-refs pack)
        statuses (mapv #(get-in % [:outcome :status]) refs)
        summary (:summary pack)]
    (testing "all cutoff-bounded episodes of the generation are selected"
      (is (= 4 (:selected summary)))
      (is (= 4 (count refs)))
      (is (= 4 (:eligible summary))))
    (testing "successes are represented — the optimizer never learns only from failures"
      (is (= 2 (:successes summary)))
      (is (some #{:completed} statuses)))
    (testing "failures are represented — failures are evidence, not discarded traces"
      (is (= 2 (:failures summary)))
      (is (some #(not= :completed %) statuses))
      (is (some #{:failed} statuses))
      (is (some #{:budget-exhausted} statuses)))
    (testing "no episode beyond the cutoff ever enters"
      (is (every? #(<= (:last-event (:trace %)) cutoff) refs)))
    (testing "the pack is content-addressed with the Genome hashing convention"
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (:evidence/id pack)))
      (is (= generation-id (:generation/id pack)))
      (is (= cutoff (:cutoff-event-id pack))))
    (testing "the evidence id IS the content address of the frozen pack body"
      (is (cas/exists? (:cas store) (:evidence/id pack)))
      (is (= (dissoc pack :evidence/id)
             (artifact-edn store (:evidence/id pack)))))))

(deftest step-1-representation-quotas-reach-beyond-the-recent-pool
  (let [store (fresh-store)
        db (:sqlite store)
        ;; a failure-skewed recent window: 1 success then 6 failures,
        ;; with :recent 3 so the pool alone would be all failures
        _ (scene! db {:outcome {:status :completed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        pack (build store {:recent 3 :include-successes 10
                           :include-failures 10 :include-high-cost 5})
        refs (episode-refs pack)
        statuses (mapv #(get-in % [:outcome :status]) refs)
        summary (:summary pack)]
    (testing "include-successes backfills the success into a failure-skewed pack"
      (is (some #{:completed} statuses))
      (is (= 1 (:successes summary))))
    (testing "the recent quota bounds the recent component; the include-*
              quotas add only their own classes, so the pack stays bounded"
      (is (= 7 (:selected summary)))
      (is (= 7 (count refs))))
    (testing "ranking is by recency"
      (let [ls (mapv #(get-in % [:trace :last-event]) refs)]
        (is (= (sort > ls) ls))))))

;; ============================================================================
;; Step 2 — the cutoff is immutable; a pack never changes after creation
;; ============================================================================

(deftest step-2-cutoff-is-immutable-and-new-episodes-stay-out
  (let [store (fresh-store)
        db (:sqlite store)
        _ (scene! db {:outcome {:status :completed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        cutoff (get-in (scene! db {:outcome {:status :completed :score nil}})
                       [:trace :last-event])
        request {:generation/id generation-id
                 :cutoff-event-id cutoff
                 :selector (base-selector)}
        p1 (evidence/build-evidence-pack store request)]
    (testing "the first pack sees exactly the three cutoff-bounded episodes"
      (is (= 3 (count (episode-refs p1)))))
    ;; a NEW episode arrives AFTER the pack was created, with a trace
    ;; ending strictly after the frozen cutoff
    (let [late (scene! db {:outcome {:status :failed :score nil}})
          _ (is (> (get-in late [:trace :last-event]) cutoff))
          p2 (evidence/build-evidence-pack store request)
          p3 (evidence/build-evidence-pack
              store (assoc request :cutoff-event-id
                           (get-in late [:trace :last-event])))]
      (testing "the frozen pack is byte-for-byte unchanged: same evidence id,
                same episodes, no late episode ever enters"
        (is (= (:evidence/id p1) (:evidence/id p2)))
        (is (= (:episodes p1) (:episodes p2)))
        (is (= (:summary p1) (:summary p2)))
        (is (= 3 (count (episode-refs p2))))
        (is (not-any? #(= (:episode/id late) (:episode/id %))
                      (episode-refs p2))))
      (testing "the late episode IS eligible under a raised cutoff — the
                boundary, not the data, is what froze"
        (is (= 4 (count (episode-refs p3))))
        (is (some #(= (:episode/id late) (:episode/id %))
                  (episode-refs p3)))))))

;; ============================================================================
;; Step 3 — deterministic sampling/ranking; persisted seed policy
;; ============================================================================

(deftest step-3-selection-is-deterministic-and-ranking-is-by-recency
  (let [store (fresh-store)
        db (:sqlite store)
        _ (scene! db {:outcome {:status :completed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        _ (scene! db {:outcome {:status :completed :score nil}})
        p1 (build store)
        p2 (build store)]
    (testing "the same (store, request) reproduces the same pack"
      (is (= (:evidence/id p1) (:evidence/id p2)))
      (is (= (:episodes p1) (:episodes p2)))
      (is (= (:summary p1) (:summary p2))))
    (testing "episodes are ranked by recency: non-increasing :last-event"
      (let [ls (mapv #(get-in % [:trace :last-event]) (:episodes p1))]
        (is (= (sort > ls) ls))))))

(deftest step-3-seeded-selection-is-deterministic-and-the-seed-is-persisted
  (let [store (fresh-store)
        db (:sqlite store)
        _ (scene! db {:outcome {:status :completed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        _ (scene! db {:outcome {:status :completed :score nil}})
        selector (assoc (base-selector) :seed 42)
        p1 (build store selector)
        p2 (build store selector)]
    (testing "the seed is persisted in the pack's summary (and inside :selector)"
      (is (= 42 (get-in p1 [:summary :seed])))
      (is (= 42 (get-in p1 [:summary :selector :seed]))))
    (testing "the seeded selection is deterministic: same seed, same pack"
      (is (= (:evidence/id p1) (:evidence/id p2)))
      (is (= (:episodes p1) (:episodes p2))))
    (testing "the unseeded selection (episode-id tie-break) is also deterministic"
      (is (= (:evidence/id (build store))
             (:evidence/id (build store)))))))

(deftest step-3-recency-ties-are-broken-deterministically
  (let [store (fresh-store)
        db (:sqlite store)
        _ (tie-scene! db {:status :completed :score nil}
                      {:status :failed :score nil})
        u1 (build store)
        u2 (build store)
        s1 (build store (assoc (base-selector) :seed 7))
        s2 (build store (assoc (base-selector) :seed 7))]
    (testing "recency ties are broken deterministically (by episode id)"
      (is (= 2 (count (:episodes u1))))
      (is (= (:episodes u1) (:episodes u2)))
      (is (= (:evidence/id u1) (:evidence/id u2))))
    (testing "a supplied seed breaks ties deterministically per seed"
      (is (= (:episodes s1) (:episodes s2)))
      (is (= (:evidence/id s1) (:evidence/id s2))))))

;; ============================================================================
;; Step 4 — large trace excerpts as CAS artifact refs with provenance
;; ============================================================================

(deftest step-4-trace-excerpts-are-cas-refs-not-copies
  (let [store (fresh-store)
        db (:sqlite store)
        _ (scene! db {:outcome {:status :completed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        _ (scene! db {:outcome {:status :completed :score nil}
                      :usage {:total-cost 55}})
        pack (build store)
        pack-edn (pr-str pack)]
    (testing "the pack carries only compact metadata — trace payload bytes
              never cross into the pack itself"
      (is (not (str/includes? pack-edn "trace-payload"))))
    (testing "the excerpt artifact DOES carry the trace payload — the body
              lives in the CAS, only the ref lives in the pack"
      (let [excerpt (artifact-edn store (:excerpt-ref (first (:episodes pack))))]
        (is (str/includes? (pr-str (:events excerpt)) "trace-payload"))))
    (testing "every episode is a compact ref: id, provenance, outcome,
              trace bounds — and a resolvable :excerpt-ref"
      (doseq [e (:episodes pack)]
        (let [expected-keys (cond-> #{:episode/id :session/id :generation/id
                                     :excerpt-ref :outcome :trace}
                              (seq (:usage e)) (conj :usage))]
          (is (= expected-keys (set (keys e)))
              "component: :usage appears only when the episode carries usage
               (unknown usage is omitted, never zero)"))
        (is (uuid? (:episode/id e)))
        (is (re-matches #"^sha256:[0-9a-f]{64}$" (:excerpt-ref e)))
        (is (cas/exists? (:cas store) (:excerpt-ref e)))
        (let [excerpt (artifact-edn store (:excerpt-ref e))]
          (testing "the excerpt preserves episode provenance"
            (is (= 1 (:excerpt/version excerpt)))
            (is (= (:episode/id e) (:episode/id excerpt)))
            (is (= (:session/id e) (:session/id excerpt)))
            (is (= (:trace e) (:trace excerpt))))
          (testing "the excerpt carries the full causal trace as events"
            (is (vector? (:events excerpt)))
            (is (= 3 (count (:events excerpt))))
            (is (= (get-in e [:trace :first-event])
                   (:event/id (first (:events excerpt)))))
            (is (= (get-in e [:trace :last-event])
                   (:event/id (last (:events excerpt)))))))))
    (testing "high-cost episodes are represented alongside successes/failures"
      (is (pos? (:high-cost (:summary pack))))
      (is (some #(= {:total-cost 55} (:usage %)) (:episodes pack))))))

;; ============================================================================
;; component — model-usage enrichment (optional :usage, absent when unknown)
;; ============================================================================

(deftest e5-usage-round-trips-through-the-pack
  (let [store (fresh-store)
        db (:sqlite store)
        usage {:model-input-tokens 100 :model-output-tokens 50
               :model-cost-units 0.75}
        _ (scene! db {:outcome {:status :completed :score nil}
                      :usage usage})
        _ (scene! db {:outcome {:status :failed :score nil}})
        pack (build store)]
    (testing "the episode's model-call usage (token counts + cost estimate)
              round-trips into its pack entry"
      (is (some #(= usage (:usage %)) (:episodes pack)))
      (is (= usage (:usage (first (filter #(contains? % :usage)
                                          (:episodes pack)))))))
    (testing "the pack summary carries the selected episodes' model usage"
      (is (= usage (get-in pack [:summary :usage]))))
    (testing "the frozen pack body round-trips through the CAS with the
              usage intact"
      (is (= (dissoc pack :evidence/id)
             (artifact-edn store (:evidence/id pack)))))))

(deftest e5-usage-is-absent-when-unknown
  (let [store (fresh-store)
        db (:sqlite store)
        _ (scene! db {:outcome {:status :completed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}})
        pack (build store)]
    (testing "episodes without usage omit the :usage key entirely — unknown
              usage is never fabricated as zeros (honest accounting)"
      (is (every? #(not (contains? % :usage)) (:episodes pack))))
    (testing "the pack summary omits :usage when no selected episode carries
              usage"
      (is (not (contains? (:summary pack) :usage)))
      (is (not (str/includes? (pr-str pack) ":model-input-tokens"))))))

(deftest e5-usage-accumulates-across-selected-episodes
  (let [store (fresh-store)
        db (:sqlite store)
        _ (scene! db {:outcome {:status :completed :score nil}
                      :usage {:model-input-tokens 100 :model-output-tokens 50
                              :model-cost-units 0.25}})
        _ (scene! db {:outcome {:status :failed :score nil}
                      :usage {:model-input-tokens 200 :model-output-tokens 60
                              :model-cost-units 0.5}})
        pack (build store)
        s-usage (get-in pack [:summary :usage])]
    (testing "token counters and cost estimates accumulate over the selected
              episodes"
      (is (= 300 (:model-input-tokens s-usage)))
      (is (= 110 (:model-output-tokens s-usage)))
      (is (= 0.75 (:model-cost-units s-usage))))))

(deftest e5-schema-rejects-non-numeric-usage
  (let [store (fresh-store)
        db (:sqlite store)
        _ (scene! db {:outcome {:status :completed :score nil}
                      :usage {:model-input-tokens "one hundred"}})]
    (testing "non-numeric usage is rejected at the store trust boundary"
      (is (= :evidence/episode-invalid
             (thrown-error-type #(build store)))))
    (testing "non-numeric usage in a pack is rejected at the pack boundary"
      (is (= :evidence/pack-invalid
             (thrown-error-type
              #(es/validate-pack
                {:evidence/id placeholder-hash
                 :generation/id generation-id
                 :cutoff-event-id 1000
                 :episodes [{:episode/id (random-uuid)
                             :session/id (random-uuid)
                             :generation/id generation-id
                             :excerpt-ref placeholder-hash
                             :outcome {:status :completed :score nil}
                             :trace {:first-event 1 :last-event 1}
                             :usage {:model-cost-units :unknown}}]
                 :summary {:selector (base-selector) :seed nil
                           :eligible 1 :selected 1 :successes 1
                           :failures 0 :high-cost 0}})))))))

;; ============================================================================
;; Error contract and generation scoping
;; ============================================================================

(deftest invalid-store-and-request-are-rejected
  (let [store (fresh-store)
        request {:generation/id generation-id
                 :cutoff-event-id 10000
                 :selector (base-selector)}]
    (testing "a malformed store is rejected"
      (is (= :evidence/store-invalid
             (thrown-error-type #(evidence/build-evidence-pack {} request))))
      (is (= :evidence/store-invalid
             (thrown-error-type #(evidence/build-evidence-pack nil request)))))
    (testing "unknown selector keys and invalid cutoff/selector shapes are rejected"
      (is (= :evidence/request-invalid
             (thrown-error-type
              #(evidence/build-evidence-pack
                store (assoc-in request [:selector :bogus] 1)))))
      (is (= :evidence/request-invalid
             (thrown-error-type
              #(evidence/build-evidence-pack
                store (assoc request :cutoff-event-id 0)))))
      (is (= :evidence/request-invalid
             (thrown-error-type
              #(evidence/build-evidence-pack
                store (assoc request :cutoff-event-id -5)))))
      (is (= :evidence/request-invalid
             (thrown-error-type
              #(evidence/build-evidence-pack
                store (dissoc request :generation/id))))))))

(deftest episodes-from-other-generations-are-excluded
  (let [store (fresh-store)
        db (:sqlite store)
        _ (scene! db {:outcome {:status :completed :score nil}})
        _ (scene! db {:outcome {:status :failed :score nil}
                      :gen other-generation-id})
        pack (build store)
        summary (:summary pack)]
    (testing "only episodes pinned to the request's generation enter the pack"
      (is (= 1 (count (:episodes pack))))
      (is (= 1 (:selected summary)))
      (is (= 1 (:eligible summary)))
      (is (= generation-id
             (get-in (first (:episodes pack)) [:generation/id]))))))

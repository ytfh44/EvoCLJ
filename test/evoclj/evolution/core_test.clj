(ns evoclj.evolution.core-test
  "component tests — orchestrate one evolution proposal cycle.

  propose-candidates! (evoclj.evolution.core) is the Milestone 7
  orchestrator: it runs the REAL pipeline (freeze evidence → diagnose →
  load negative history → propose mutation → validate risk/budget →
  apply patch → compile candidate → persist Candidate) and returns the
  persisted Candidate records. The only adapters are the deterministic
  fakes sanctioned by the task (Step 1): a FakeDiagnostician
  implementing the component Diagnostician protocol and a FakeMutator
  implementing the Mutator protocol defined in evoclj.evolution.core.

  The four normative scenarios, in the task's numbered order:

  - Step 1/2: the exact phase order is asserted through a spy
    :phase-hook (an ordered phase log) AND through the evolution
    taxonomy events (:evolution/evidence-frozen →
    :evolution/diagnosis-created → :evolution/mutation-proposed →
    :evolution/candidate-materialized) captured by a spy :event-sink.
  - Step 3: a failing mutation (stale :expect/hash → the patch
    preimage gate, and a budget-exceeding :meta declaration) leaves the
    current Genome directory and the current-generation pointer
    untouched — no candidate row, no leftover candidate directory, no
    new generation row — while a healthy sibling mutation still
    materializes.
  - Step 4: the cycle is capped at THREE candidates per cycle even
    when the mutator proposes five (the v0 cap is absolute).
  - Milestone 7 exit: with fixture Episodes, the REAL
    pattern-diagnostician and a deterministic fixture Mutator produce
    a deterministic immutable G2 Candidate from the seed G1; the
    candidate records why it exists (mutation + evidence + hypothesis
    linkage, resolvable in the CAS), re-running the cycle is
    idempotent, and there is NO mechanism yet to promote it (state
    stays :evaluation-pending, no promotion namespace exists).

  FIXTURE DESIGN: the parent generation row is seeded with the REAL
  seed Genome's content address (current = 1, Database Invariant 6),
  so the candidate lineage FK (Database Invariant 8) and the
  orchestrator's parent-genome integrity check run against the true
  G1. The current Genome bundle is the real genomes/seed directory —
  the orchestrator never writes into it (candidates land in a temp
  :candidates-dir), so \"current Genome directory untouched\" is
  asserted by reloading G1 and by the leftover-directory count."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.evolution.budget :as budget]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.evolution.core :as core]
            [evoclj.evolution.diagnose :as diagnose]
            [evoclj.genome.hash :as hash]
            [evoclj.genome.load :as load]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Path Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; --- shared fixture identity --------------------------------------------------

(def ^:private hex64
  "64 hex chars for the canonical content-addressed ids."
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private fixture-resolution-id
  "A canonical ResolutionId (64 hex chars) for the seeded generation
  row and the fixture episodes."
  (str "sha256:" (apply str (repeat 64 "f"))))

(def ^:private fixture-phenotype-id
  "A canonical phenotype id (64 hex chars) for the fixture
  session/event rows."
  (str "sha256:" (apply str (repeat 64 "a"))))

(def ^:private generation-id
  "The parent generation's stable id (the seeded CURRENT row)."
  "generation-1")

(defn- uuid
  "A fixed, readable UUID for fixture ids."
  [n]
  (java.util.UUID/fromString (format "00000000-0000-0000-0000-%012d" n)))

(defn- deterministic-uuid
  "A deterministic name-based (v3) UUID over a string — the fixture
  Mutator's deterministic :mutation/id convention."
  [s]
  (UUID/nameUUIDFromBytes (.getBytes s StandardCharsets/UTF_8)))

(defn- thrown-error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))

;; --- the real seed Genome (G1) ------------------------------------------------

(defn- seed-root
  "The real seed Genome bundle directory (genomes/seed)."
  []
  (let [p (.toPath (io/file "genomes/seed"))]
    (when-not (Files/isDirectory p (make-array LinkOption 0))
      (throw (ex-info "genomes/seed bundle not found (run from the repo root)"
                      {:path (str p)})))
    p))

(defn- route-descriptor
  "The seed route program descriptor (component choice (a): an in-memory
  descriptor list riding on the loaded-genome value under :programs)."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- seed-loaded-genome
  "The REAL genomes/seed bundle loaded from disk with its program
  registry attached (G1)."
  []
  (assoc (load/load-genome (seed-root))
         :programs [(route-descriptor)]))

(defn- fixture-catalog
  "The on-disk provider catalog fixture (component Resolution)."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

;; --- the deterministic fake adapters (Step 1) ---------------------------------

(defn- canonical
  "The repo's canonical EDN form for hashing — the same convention as
  evoclj.evolution.diagnose/canonical (maps sorted by their pr-str key
  form, sets by their pr-str element form, collections realized
  eagerly). Replicated here so the fake diagnosis carries the TRUE
  content hash (persist-diagnosis! rejects a forged :diagnosis/id)."
  [x]
  (cond
    (map? x) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map (fn [[k v]] [k (canonical v)])) x)
    (set? x) (into (sorted-set-by (fn [a b] (compare (pr-str a) (pr-str b))))
                   (map canonical) x)
    (vector? x) (mapv canonical x)
    (seq? x) (mapv canonical x)
    :else x))

(defn- fake-diagnosis
  "A fixed, schema-valid Diagnosis for the frozen pack: it copies the
  pack's :evidence/id (provenance) and carries one bounded hypothesis,
  so the orchestrator's evidence-id agreement check and the component
  persistence path both pass."
  [pack]
  (let [body {:evidence/id (:evidence/id pack)
              :hypotheses [{:hypothesis/id (uuid 2)
                            :pattern :task/success
                            :claim "fixture diagnosis for the orchestration test"
                            :support [{:episode/id (uuid 9) :event-ids [1]}]
                            :counterevidence []
                            :target {:kind :workflow :id :task}
                            :expected-effect {:metric :task/success :direction :increase}
                            :confidence-band :medium}]}]
    (assoc body :diagnosis/id (hash/text-digest (pr-str (canonical body))))))

(defrecord FakeDiagnostician [builder]
  diagnose/Diagnostician
  (diagnose [_ evidence-pack]
    (builder evidence-pack)))

(defrecord FakeMutator [producer]
  core/Mutator
  (propose-mutations [_ context]
    (producer context)))

(defn- case-form
  "The replacement :case form for the route program: the same decision
  contract (the :echo and :finish branches are unchanged), but the
  fallback branch normalizes a missing :value to `fallback`. Pure,
  allowlist-clean, and deterministic (Global Constraint 6)."
  [fallback]
  (list 'case 'op
        :echo {:action (list 'tool-call-intent :fixture/echo
                             {:text (list 'get 'input :text)})}
        :finish {:action (list 'finish-intent (list 'get 'input :value))}
        {:action (list 'finish-intent
                       (list 'or (list 'get 'input :value) fallback))}))

(defn- route-replacement-op
  "The deterministic :replace-form op on the seed's mutable
  programs/route.clj: replace the :case form (the only top-level form
  starting with `case`) with the case-form variant. The :expect/hash
  is the parent's own file digest (a stale digest fails the patch
  preimage gate)."
  [parent-genome & [op-overrides]]
  (merge {:op :replace-form
          :file "programs/route.clj"
          :selector ['case]
          :expect/hash (get-in parent-genome [:files "programs/route.clj" :digest])
          :form (case-form "done")}
         op-overrides))

(defn- route-mutation
  "A schema-plausible Mutation IR template for the seed parent: one
  :program-risk form op on the mutable route program. :mutation/id,
  :parent/genome-id, :evidence/id, and :hypothesis/id are completed by
  the orchestrator unless supplied; an optional override map wins."
  [parent-genome & [overrides]]
  (merge {:risk :program
          :ops [(route-replacement-op parent-genome)]
          :expected-effect {:primary-metric :task/success :direction :increase}}
         overrides))

(defn- fallback-mutation
  "A :replace-form mutation on the seed's route program that changes
  the :case fallback string to `fallback` — one distinct mutation per
  string (distinct content → distinct candidate), all schema-valid
  and all compiling against the mutable route program."
  [parent-genome fallback]
  (route-mutation parent-genome
                  {:ops [(route-replacement-op
                          parent-genome
                          {:form (case-form fallback)})]}))

(defn- fixture-producer
  "The deterministic fixture Mutator used by the Milestone 7 exit test:
  when the (real) diagnosis carries a :task/success hypothesis, it
  proposes ONE fully-determined Mutation IR (name-based :mutation/id,
  the hypothesis's own :hypothesis/id, the frozen pack's
  :evidence/id) targeting the mutable route program. nil when there is
  nothing to act on."
  [ctx]
  (when-let [hypothesis (first (:hypotheses (:diagnosis ctx)))]
    (let [parent (:parent-genome ctx)
          content {:parent/genome-id (:genome/id parent)
                   :hypothesis/id (:hypothesis/id hypothesis)
                   :evidence/id (:evidence/id (:diagnosis ctx))
                   :risk :program
                   :ops [(route-replacement-op parent)]
                   :expected-effect {:primary-metric :task/success
                                     :direction :increase}}]
      [(assoc content :mutation/id (deterministic-uuid (pr-str content)))])))

;; --- temp stores (test temp dirs only) ---------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-evolution-core-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-cas-dir
  []
  (let [d (Files/createTempDirectory "evoclj-evolution-core-cas-"
                                     (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- temp-candidates-dir
  []
  (let [d (Files/createTempDirectory "evoclj-evolution-core-candidates-"
                                     (make-array FileAttribute 0))]
    (swap! temp-paths conj (str d))
    d))

(defn- delete-tree!
  "Recursively delete a temp path."
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
  "A migrated temp database seeded with the parent generation row whose
  genome_id is the REAL seed Genome's content address (current = 1,
  Database Invariant 6) plus a temp CAS root. Returns the executor
  :stores map {:sqlite ... :cas ...}."
  []
  (let [path (temp-db-path)
        db (sqlite/spec path)
        cas-root (temp-cas-dir)]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id (:genome/id (seed-loaded-genome))
                     :resolution_id fixture-resolution-id
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    {:sqlite db :cas (cas/->cas cas-root)}))

(defn- seed-episode-fixtures!
  "Seed two executed-session fixtures for the exit test: two sessions
  pinned to the generation, six append-only events (ids 1-6), and two
  episodes — one :budget-exhausted failure (trace 1-3) and one
  :completed success (trace 4-6). Failures and successes are both
  evidence (component)."
  [store]
  (let [db (:sqlite store)
        genome-id (:genome/id (seed-loaded-genome))
        sid-a (random-uuid)
        sid-b (random-uuid)
        events [[1 sid-a ":session/created"]
                [2 sid-a ":session/started"]
                [3 sid-a ":session/budget-exhausted"]
                [4 sid-b ":session/created"]
                [5 sid-b ":session/started"]
                [6 sid-b ":session/completed"]]]
    (sqlite/with-db [conn db]
      (doseq [[sid state] [[sid-a "budget-exhausted"] [sid-b "completed"]]]
        (jdbc/insert! conn :sessions
                      {:id (str sid)
                       :generation_id generation-id
                       :genome_id genome-id
                       :resolution_id fixture-resolution-id
                       :phenotype_id fixture-phenotype-id
                       :state state
                       :created_at "2025-01-02T00:00:00Z"}))
      (doseq [[id sid type] events]
        (jdbc/insert! conn :events
                      {:id id
                       :session_id (str sid)
                       :event_seq id
                       :generation_id generation-id
                       :phenotype_id fixture-phenotype-id
                       :event_type type
                       :cause_event_id (when (> id 1) (dec id))
                       :payload_ref nil
                       :payload "{}"
                       :prev_hash "fixture"
                       :event_hash (str "fixture-" id)
                       :created_at "2025-01-02T00:00:00Z"}))
      (doseq [[sid first-event last-event outcome]
              [[sid-a 1 3 {:status :budget-exhausted}]
               [sid-b 4 6 {:status :completed}]]]
        (jdbc/insert! conn :episodes
                      {:id (str (random-uuid))
                       :session_id (str sid)
                       :generation_id generation-id
                       :genome_id genome-id
                       :resolution_id fixture-resolution-id
                       :task_ref (str "sha256:" hex64)
                       :first_event_id first-event
                       :last_event_id last-event
                       :outcome (pr-str outcome)
                       :usage (pr-str {})
                       :created_at "2025-01-02T00:00:00Z"})))
    store))

(defn- base-system
  "The evolution-system map under test: the real store, the fixture
  provider catalog, the real seed Genome root as :genome-root, a temp
  :candidates-dir, the given adapters, the v0 budget profile, and the
  spy :event-sink / :phase-hook. Optional overrides win."
  [store & [overrides]]
  (merge {:store store
          :provider-catalog (fixture-catalog)
          :genome-root (seed-root)
          :candidates-dir (temp-candidates-dir)
          :diagnostician (->FakeDiagnostician fake-diagnosis)
          :mutator (->FakeMutator (constantly nil))
          :budget-profile budget/v0-profile
          :programs-registry [(route-descriptor)]}
         overrides))

(defn- base-request
  "A valid cycle request for the fixture generation; overrides win."
  [& [overrides]]
  (merge {:generation/id generation-id
          :evidence-selector {:recent 1 :include-successes 1
                              :include-failures 1 :include-high-cost 1}
          :max-candidates 3}
         overrides))

(defn- candidate-dir-count
  "The number of candidate bundle directories under the system's
  :candidates-dir (each finalized candidate is one directory)."
  [system]
  (let [root (.toFile (Path/of (str (:candidates-dir system))
                               (make-array String 0)))]
    (count (filter #(.isDirectory %) (or (.listFiles root) (make-array java.io.File 0))))))

(defn- event-types
  "The :event/type sequence captured by the spy :event-sink."
  [events]
  (mapv :event/type @events))

(defn- read-artifact
  "Read a CAS artifact back as EDN data."
  [store artifact-id]
  (edn/read-string
   (String. (cas/get-bytes (:cas store) artifact-id)
            StandardCharsets/UTF_8)))

;; ============================================================================
;; Step 1/2 — the exact phase order (spy log + taxonomy events)
;; ============================================================================

(deftest step-1-2-orchestration-runs-the-normative-phase-order
  (let [store (fresh-store)
        events (atom [])
        phases (atom [])
        system (base-system store
                             {:event-sink (fn [event] (swap! events conj event))
                              :phase-hook (fn [phase] (swap! phases conj phase))
                              :mutator (->FakeMutator
                                        (fn [ctx] [(route-mutation (:parent-genome ctx))]))})
        result (core/propose-candidates! system (base-request))]
    (testing "Step 2 — the exact normative phase order"
      (is (= [:freeze-evidence :diagnose :load-history :propose-mutation
              :validate-risk-budget :apply-patch :compile-candidate
              :persist-candidate]
             @phases)))
    (testing "the evolution taxonomy events fire in cycle order"
      (is (= [:evolution/evidence-frozen :evolution/diagnosis-created
              :evolution/mutation-proposed :evolution/candidate-materialized]
             (event-types events))))
    (testing "one persisted candidate comes back"
      (is (= 1 (count result)))
      (let [c (first result)]
        (is (= :evaluation-pending (:state c)))
        (is (= generation-id (:parent/generation-id c)))
        (is (= (:genome/id (seed-loaded-genome)) (:parent/genome-id c)))
        (is (= :program (:risk c)))
        (is (uuid? (:mutation/id c)))
        (is (re-matches #"^sha256:[0-9a-f]{64}$" (:candidate/genome-id c)))
        (is (not= (:genome/id (seed-loaded-genome)) (:candidate/genome-id c))
            "the candidate G2 differs from the parent G1")
        (is (= (:candidate/id c)
               (:candidate/id (candidate/find-candidate store (:candidate/id c))))
            "the persisted row resolves by id")))
    (testing "the frozen evidence and the diagnosis are durable artifacts"
      (let [frozen (first (filter #(= :evolution/evidence-frozen (:event/type %)) @events))
            diag (first (filter #(= :evolution/diagnosis-created (:event/type %)) @events))
            pack-id (get-in frozen [:metadata :evidence/id])
            pack (read-artifact store pack-id)
            diagnosis (read-artifact store (get-in diag [:metadata :diagnosis/id]))]
        (is (= generation-id (:generation/id pack)))
        (is (= pack-id (:evidence/id diagnosis))
            "the persisted diagnosis is self-provenancing to the frozen pack")))))

(deftest step-1-nothing-to-propose-returns-an-empty-cycle
  (let [store (fresh-store)
        events (atom [])
        system (base-system store
                             {:event-sink (fn [event] (swap! events conj event))
                              :mutator (->FakeMutator (constantly nil))})
        result (core/propose-candidates! system (base-request))]
    (is (= [] result))
    (is (= [:evolution/evidence-frozen :evolution/diagnosis-created]
           (event-types events))
        "the cycle stops at the diagnosis when the Mutator proposes nothing")
    (is (= 0 (count (sqlite/query (:sqlite store)
                                  ["SELECT * FROM candidates"]))))))

;; ============================================================================
;; Step 3 — a failure before materialization cannot touch the current
;; Genome directory or the current-generation pointer
;; ============================================================================

(deftest step-3-a-failing-mutation-leaves-no-trace-and-a-healthy-sibling-still-materializes
  (let [store (fresh-store)
        events (atom [])
        phases (atom [])
        system (base-system store
                             {:event-sink (fn [event] (swap! events conj event))
                              :phase-hook (fn [phase] (swap! phases conj phase))
                              :mutator (->FakeMutator
                                        (fn [ctx]
                                          (let [parent (:parent-genome ctx)]
                                            [(route-mutation parent
                                                             {:ops [(route-replacement-op
                                                                     parent
                                                                     {:expect/hash
                                                                      (str "sha256:" (apply str (repeat 64 "0")))})]})
                                             (route-mutation parent)])))})
        result (core/propose-candidates! system (base-request))]
    (testing "only the healthy sibling materializes"
      (is (= 1 (count result)))
      (is (= 1 (count (sqlite/query (:sqlite store)
                                    ["SELECT * FROM candidates"]))))
      (is (= 1 (count (sqlite/query (:sqlite store)
                                    ["SELECT * FROM mutations"])))
          "the failing mutation never reached persistence"))
    (testing "the failing mutation is recorded as candidate-invalid, the cycle continues"
      (let [invalid (first (filter #(= :evolution/candidate-invalid (:event/type %))
                                   @events))
            order (event-types events)]
        (is (= :evolution/candidate-invalid (:event/type invalid)))
        (is (= :patch-failed (get-in invalid [:metadata :reason])))
        (is (= :patch/preimage-mismatch (get-in invalid [:metadata :error/type])))
        (is (= 2 (count (filter #(= :evolution/mutation-proposed (:event/type %))
                                @events)))
            "both mutations were proposed; one was invalid")
        (is (< (.indexOf order :evolution/candidate-invalid)
               (.indexOf order :evolution/candidate-materialized))
            "the invalid candidate is recorded before the healthy one materializes")))
    (testing "the current Genome directory is untouched"
      (is (= (:genome/id (seed-loaded-genome))
             (:genome/id (load/load-genome (seed-root))))
          "reloading G1 yields the same content address"))
    (testing "the current-generation pointer is untouched"
      (let [rows (sqlite/query (:sqlite store)
                               ["SELECT id, genome_id, current FROM generations"])]
        (is (= 1 (count rows)))
        (is (= generation-id (:id (first rows))))
        (is (= 1 (:current (first rows))))
        (is (= (:genome/id (seed-loaded-genome)) (:genome_id (first rows))))))
    (testing "no partial candidate: the failed mutation left no bundle directory"
      (is (= 1 (candidate-dir-count system))
          "exactly the healthy candidate's bundle exists, nothing from the failure"))))

(deftest step-3-a-cycle-where-every-proposal-fails-leaves-nothing
  (let [store (fresh-store)
        events (atom [])
        system (base-system store
                             {:event-sink (fn [event] (swap! events conj event))
                              :mutator (->FakeMutator
                                        (fn [ctx]
                                          [(route-mutation (:parent-genome ctx)
                                                           {:risk :meta})]))})
        result (core/propose-candidates! system (base-request))]
    (testing "the budget gate rejects the :meta declaration (R4 not enabled in v0)"
      (is (= [] result))
      (let [invalid (first (filter #(= :evolution/candidate-invalid (:event/type %))
                                   @events))]
        (is (= :budget-exceeded (get-in invalid [:metadata :reason])))
        (is (= :evolution/risk-not-enabled (get-in invalid [:metadata :error/type])))))
    (testing "nothing was persisted and the current generation is untouched"
      (is (= 0 (count (sqlite/query (:sqlite store)
                                    ["SELECT * FROM candidates"]))))
      (is (= 0 (candidate-dir-count system)))
      (let [rows (sqlite/query (:sqlite store)
                               ["SELECT id, current FROM generations"])]
        (is (= 1 (count rows)))
        (is (= 1 (:current (first rows))))))))

;; ============================================================================
;; Step 4 — v0 limits the cycle to max THREE candidates
;; ============================================================================

(deftest step-4-the-cycle-is-capped-at-three-candidates
  (let [store (fresh-store)
        events (atom [])
        fallbacks ["done" "ok" "finished" "complete" "exit"]
        system (base-system store
                             {:event-sink (fn [event] (swap! events conj event))
                              :mutator (->FakeMutator
                                        (fn [ctx]
                                          (mapv #(fallback-mutation
                                                  (:parent-genome ctx) %)
                                                fallbacks)))})
        result (core/propose-candidates! system (base-request))]
    (testing "five distinct proposals, three materialized candidates"
      (is (= 3 (count result)))
      (is (= 3 (count (distinct (map :candidate/genome-id result))))
          "each candidate is a distinct G2")
      (is (= 3 (count (sqlite/query (:sqlite store)
                                    ["SELECT * FROM candidates"]))))
      (is (= 3 (count (sqlite/query (:sqlite store)
                                    ["SELECT * FROM mutations"]))))
      (is (= 3 (count (filter #(= :evolution/mutation-proposed (:event/type %))
                              @events)))
          "only the adopted three were proposed to the pipeline")
      (is (= 3 (count (filter #(= :evolution/candidate-materialized (:event/type %))
                              @events))))
      (is (= 3 (candidate-dir-count system))))))

(deftest step-4-the-three-candidate-cap-is-absolute-in-v0
  (let [store (fresh-store)
        system (base-system store
                             {:mutator (->FakeMutator
                                        (fn [ctx]
                                          (mapv #(fallback-mutation
                                                  (:parent-genome ctx)
                                                  (str "fallback-" %))
                                                (range 8))))})
        result (core/propose-candidates! system
                                         (base-request {:max-candidates 10}))]
    (testing "even a request for ten is capped at three in v0"
      (is (= 3 (count result)))
      (is (= 3 (count (sqlite/query (:sqlite store)
                                    ["SELECT * FROM candidates"])))))))

;; ============================================================================
;; Milestone 7 exit — a deterministic immutable G2 from G1, with no
;; promotion mechanism yet
;; ============================================================================

(deftest milestone-7-exit-deterministic-g2-candidate-with-no-promotion-mechanism
  (let [store (seed-episode-fixtures! (fresh-store))
        events (atom [])
        system (base-system store
                             {:event-sink (fn [event] (swap! events conj event))
                              :diagnostician (diagnose/pattern-diagnostician
                                              {:task/success-threshold 1.0
                                               :max-hypotheses 3
                                               :confidence-band :medium})
                              :mutator (->FakeMutator fixture-producer)})
        request (base-request {:evidence-selector {:recent 2 :include-successes 1
                                                   :include-failures 1 :include-high-cost 1}})
        first-run (core/propose-candidates! system request)
        c1 (first first-run)]
    (testing "the cycle produced exactly one G2 from the fixture Episodes"
      (is (= 1 (count first-run)))
      (is (= (:genome/id (seed-loaded-genome)) (:parent/genome-id c1)))
      (is (not= (:parent/genome-id c1) (:candidate/genome-id c1))
          "G2 is a new content address, not G1")
      (is (= :program (:risk c1)))
      (is (= :evaluation-pending (:state c1)))
      (is (= [:evolution/evidence-frozen :evolution/diagnosis-created
              :evolution/mutation-proposed :evolution/candidate-materialized]
             (event-types events))
          "the whole real pipeline ran clean — no candidate-invalid"))
    (testing "the candidate records why it exists: mutation + evidence + hypothesis lineage"
      (let [cand-row (first (sqlite/query (:sqlite store)
                                          ["SELECT * FROM candidates WHERE id = ?"
                                           (str (:candidate/id c1))]))
            mut-row (first (sqlite/query (:sqlite store)
                                         ["SELECT * FROM mutations WHERE id = ?"
                                          (str (:mutation/id c1))]))
            diag-event (first (filter #(= :evolution/diagnosis-created (:event/type %))
                                      @events))
            diagnosis-id (get-in diag-event [:metadata :diagnosis/id])
            diagnosis (read-artifact store diagnosis-id)
            pack (read-artifact store (:evidence/id c1))]
        (is (= generation-id (:parent_generation_id cand-row)))
        (is (= (:parent/genome-id c1) (:parent_genome_id cand-row)))
        (is (= (:candidate/genome-id c1) (:genome_id cand-row)))
        (is (= (:mutation/id c1) (UUID/fromString (:mutation_id cand-row))))
        (is (= (:evidence/id c1) (:evidence_id cand-row)))
        (testing "the mutation row links hypothesis and evidence durably"
          (is (= (:parent/genome-id c1) (:parent_genome_id mut-row)))
          (is (= (:evidence/id c1) (:evidence_id mut-row)))
          (is (uuid? (UUID/fromString (:hypothesis_id mut-row))))
          (is (= "program" (:risk mut-row)))
          (is (= (:hypothesis/id (first (:hypotheses diagnosis)))
                 (UUID/fromString (:hypothesis_id mut-row)))
              "the materialized mutation answers the persisted hypothesis"))
        (testing "the frozen pack and the diagnosis resolve in the CAS"
          (is (= generation-id (:generation/id pack)))
          (is (= 2 (count (:episodes pack))))
          (is (= (:evidence/id c1) (:evidence/id diagnosis)))
          (is (= 1 (count (:hypotheses diagnosis)))))))
    (testing "the G2 bundle on disk reloads to its content address"
      (let [candidate-dir (clojure.string/replace (:candidate/genome-id c1) ":" "-")
            root (Paths/get (str (:candidates-dir system)) (make-array String 0))
            reloaded (load/load-genome (.resolve root candidate-dir))]
        (is (= (:candidate/genome-id c1) (:genome/id reloaded)))))
    (testing "re-running the cycle is deterministic and idempotent"
      (let [second-run (core/propose-candidates! system request)]
        (is (= 1 (count second-run)))
        (is (= (:candidate/id c1) (:candidate/id (first second-run))))
        (is (= (:candidate/genome-id c1) (:candidate/genome-id (first second-run))))
        (is (= 1 (count (sqlite/query (:sqlite store)
                                      ["SELECT * FROM candidates"])))
            "the same parent+mutation content dedupes to the same auditable candidate")))
    (testing "there is NO mechanism yet to call G2 'better' (promotion is M9)"
      (is (nil? (find-ns 'evoclj.promotion.core))
          "no promotion namespace exists anywhere in the codebase")
      (is (= candidate/states evoclj.evolution.candidate-states/candidate-states)
          "the candidate state machine is the canonical closed machine")
      (is (= candidate/transitions evoclj.evolution.candidate-states/candidate-transitions))
      (is (= 1 (count (sqlite/query (:sqlite store)
                                    ["SELECT * FROM generations"])))
          "no new generation row was created")
      (is (= 1 (:current (first (sqlite/query (:sqlite store)
                                              ["SELECT current FROM generations
                                                WHERE id = ?" generation-id]))))
          "the CURRENT pointer still points at G1")
      (is (nil? (some #(re-find #"(?i)promot|activ|deploy|rollback|canary" (name %))
                      (map name (keys (ns-publics 'evoclj.evolution.core))))))
      (is (not-any? #(re-find #"(?i)promotion|current" (str %))
                    (map str (keys (ns-aliases 'evoclj.evolution.core))))))))

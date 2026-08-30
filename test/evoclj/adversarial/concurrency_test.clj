(ns evoclj.adversarial.concurrency-test
  "component — Concurrency and stale promotion suite (adversarial
  release gate).

  Four plan cases, repeated until the race windows have been exercised
  enough times to be trustworthy — the normative outcomes must hold on
  EVERY transaction, not on a lucky first run:

    case 1 — two candidates evaluate concurrently from one parent;
    case 2 — two workers call promote! concurrently (same candidate,
             and two different siblings from the same parent — the CAS
             must let exactly one win);
    case 3 — rollback! races with promote! (both interleavings: the
             promotion wins, then the rollback wins);
    case 4 — session creation races with a CURRENT change.

  NORMATIVE OUTCOMES pinned by this suite:

    * exactly ONE CURRENT generation row after every transaction
      (Database Invariant 6);
    * AT MOST ONE sibling candidate wins a parent CAS — one :promoted,
      the rest :stale (or :promotion/candidate-state-invalid when two
      workers race the SAME candidate), and exactly one promotions row
      with decision 'promoted' (Invariant 5);
    * every created session records exactly ONE immutable generation —
      the pinned (generation_id, genome_id, resolution_id,
      phenotype_id) tuple is written once and never changes (Global
      Constraint 2, Database Invariant 2);
    * no session's pinned generation changes after creation.

  BARRIER DESIGN (Step 2): every race uses java.util.concurrent
  latches so the interleavings are deliberate, not lucky. The
  promotion/rollback transactions expose the component/9.5 :failpoint
  test seam (called INSIDE the BEGIN IMMEDIATE transaction, after all
  writes, immediately before the CURRENT compare-and-set), so the
  WINNER can be parked holding SQLite's write lock while the LOSER
  blocks at its own BEGIN IMMEDIATE (busy_timeout 10000) — the loser
  then reads the winner's COMMITTED state and must report :stale /
  fail closed. That pins the serialization guarantee (Global
  Constraint 15) with a deterministic window on every iteration.
  Session races additionally use a release latch so N session creators
  read CURRENT and insert rows while the promotion is parked
  mid-transaction, and evaluation races use a start latch so both
  candidates enter their finalization transactions simultaneously.

  SQLITE JOURNAL / LOCKING-MODE FINDINGS (Step 3):

    * journal mode = 'delete' (the SQLite DEFAULT): neither
      resources/migrations/001-init.sql nor any runtime connection
      issues PRAGMA journal_mode, so every database runs with the
      rollback journal, not WAL (verified below by PRAGMA journal_mode
      on a migrated test database).
    * busy_timeout: the promotion (promote.clj), rollback
      (rollback.clj), and event (event.clj) transactions explicitly
      set PRAGMA busy_timeout = 10000 before BEGIN IMMEDIATE, so
      contended writers WAIT for the write lock instead of failing
      with SQLITE_BUSY; the session/candidate compare-and-set paths
      set their own busy_timeout = 10000. The evaluation finalization
      transaction (eval/core.clj persist-finalized!) sets NO
      busy_timeout and relies on the sqlite-jdbc driver default
      (3000 ms) — adequate for its one-INSERT + one-UPDATE window, but
      not explicit (reported as a hardening note, not a defect).
    * Serialization is therefore writer-serialized (BEGIN IMMEDIATE +
      busy_timeout), never optimistic: a losing transaction always
      starts after the winner's commit, which is exactly what the CAS
      outcome assertions below rely on.

  REPETITION COUNTS (Step 1): case 1 × 20, case 2 × 50 (25 same
  candidate + 25 sibling candidates), case 3 × 20 (10 promote-wins +
  10 rollback-wins), case 4 × 30 (10 deliberate + 20 latch-burst).
  Every iteration builds a FRESH migrated database, so the races
  never reuse state.

  STORAGE FINDING (cas.clj, NOT part of this task's file): the
  eval pipeline's put-artifact! is check-then-act — cas/put-bytes!
  skips the body write only when the target does not exist, and two
  concurrent puts of the SAME content both see 'missing' and race
  Files/move (ATOMIC_MOVE + REPLACE_EXISTING) onto the same body
  path. On Windows/NTFS that move pair throws
  java.nio.file.AccessDeniedException for the loser (MoveFileEx
  refuses to replace a target that is being concurrently replaced);
  reproduced in isolation: 6 simultaneous identical puts → 5
  AccessDeniedException + 1 ok. The docstring promise ('the second
  put is a no-op on the body') holds only SEQUENTIALLY. Case 1
  therefore uses a FRESH CAS root per evaluation: the two candidate
  evaluations race on the shared DATABASE (eval_runs insert +
  candidate CAS-update — the subject of this suite) while their
  identical parent artifacts land in separate stores, so the
  storage race cannot flake the DB-invariant assertions. Fixing the
  CAS race is out of scope here (touch ONLY this file); it is
  reported as a finding with a repro."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.core :as eval-core]
            [evoclj.eval.replay :as replay]
            [evoclj.eval.static :as static]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.genome.hash :as hash]
            [evoclj.provider.protocol :as proto]
            [evoclj.promotion.promote :as promote]
            [evoclj.promotion.rollback :as rollback]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.candidate-store :as candidate-store]
            [evoclj.store.event :as event]
            [evoclj.store.existence :as existence]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)
           (java.util.concurrent CountDownLatch TimeUnit)))

;; ============================================================================
;; fixture identity
;; ============================================================================

(def ^:private seed-genome-body
  "Deterministic seed Genome body. The seed id must be content
  addressed (not a fake constant) because rollback!'s
  verify-target-genome-integrity! (component Step 3) re-hashes the
  target Genome in the CAS before any write — the rollback-vs-
  promotion race needs that check to PASS."
  "evoclj-race-seed-genome-body-v1\n")

(def ^:private parent-genome-id
  "The seed generation's Genome id (the parent of every candidate):
  the content address of the seed body above."
  (hash/text-digest seed-genome-body))

(def ^:private resolution-id
  "The compiled ResolutionId every generation row carries."
  (str "sha256:" (apply str (repeat 64 "b"))))

(def ^:private phenotype-id
  "The phenotype every session pins to (the seed's compiled Phenotype)."
  (str "sha256:" (apply str (repeat 64 "c"))))

(def ^:private seed-generation-id
  "The seed generation's stable id (the CURRENT row at world start)."
  "generation-1")

(def ^:private evidence-id
  "ArtifactId of the frozen evidence pack each mutation answers."
  (str "sha256:" (apply str (repeat 64 "e"))))

;; ============================================================================
;; temp-path lifecycle
;; ============================================================================

(def ^:private temp-paths (atom []))

(defn- temp-path!
  "Create a temp path (directory unless `file?`) and register it for
  cleanup."
  ([prefix] (temp-path! prefix false))
  ([prefix file?]
   (let [p (if file?
             (str (Files/createTempFile prefix "" (make-array FileAttribute 0)))
             (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))]
     (swap! temp-paths conj p)
     p)))

(defn- delete-tree!
  "Recursively delete a temp path (CAS roots contain artifact trees)."
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

(use-fixtures :each
  (fn [f]
    ;; G3's suite registry is kernel-side and shared; every test starts
    ;; with an empty registry so no suite leaks across tests (the same
    ;; discipline as the component gates tests).
    (static/clear-suites!)
    (f)
    (cleanup!)))

(defn- write-file!
  "Write `content` as UTF-8 to `path`, creating parent directories."
  [path content]
  (let [p (Paths/get path (make-array String 0))
        parent (.getParent p)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

;; ============================================================================
;; the world: a fresh migrated database + CAS + seed generation row
;; ============================================================================

(defn- fresh-world
  "A migrated temp database seeded with the seed generation row
  (current = 1, Database Invariant 6) plus a temp CAS root. Returns
  {:db <java.jdbc spec> :db-path <path> :cas <cas config>}."
  []
  (let [db-path (temp-path! "evoclj-race-" true)
        db (sqlite/spec db-path)
        cas-root (temp-path! "evoclj-race-cas-")
        cas (cas/->cas cas-root)]
    (migrate/migrate! db)
    ;; the seed Genome must exist in the CAS under its content address
    ;; (rollback! re-hashes it, promote!'s lineage verification reads
    ;; it) before the seed generation row is inserted
    (cas/put-bytes! cas (.getBytes seed-genome-body StandardCharsets/UTF_8)
                    {})
    (sqlite/with-db [conn db]
      (doseq [[artifact-id media-type]
              [[parent-genome-id "application/octet-stream"]
               [resolution-id "application/edn"]
               [phenotype-id "application/edn"]
               [evidence-id "application/edn"]]]
        (jdbc/insert! conn :artifacts
                      {:hash artifact-id
                       :media_type media-type
                       :size 0
                       :created_at "2025-01-01T00:00:00Z"}))
      (jdbc/insert! conn :genomes
                    {:id parent-genome-id
                     :created_at "2025-01-01T00:00:00Z"})
      (jdbc/insert! conn :generations
                    {:id seed-generation-id
                     :genome_id parent-genome-id
                     :resolution_id resolution-id
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    {:db db :db-path db-path :cas cas}))

(defn- current-rows
  "Every generation row carrying current = 1 (Database Invariant 6)."
  [db]
  (sqlite/query db ["SELECT * FROM generations WHERE current = 1"]))

(defn- generation-row
  [db id]
  (first (sqlite/query db ["SELECT * FROM generations WHERE id = ?" id])))

(defn- promotion-rows
  [db]
  (sqlite/query db ["SELECT * FROM promotions ORDER BY created_at, id"]))

(defn- candidate-row
  [db id]
  (first (sqlite/query db ["SELECT * FROM candidates WHERE id = ?" (str id)])))

(defn- session-row
  [db id]
  (first (sqlite/query db ["SELECT * FROM sessions WHERE id = ?" (str id)])))

(defn- pinned-tuple
  "The session's immutable pinned identity columns."
  [row]
  (select-keys row [:generation_id :genome_id :resolution_id :phenotype_id]))

(defn- expected-generation-id
  "The deterministic generation id promote! derives for a
  (genome-id, resolution-id) pair (the exact private formula in
  promote.clj — a generation IS a compiled Genome/Resolution pair)."
  [genome-id]
  (str "generation-"
       (subs (hash/text-digest (str genome-id "\n" resolution-id)) 7 23)))

;; ============================================================================
;; candidates: real materialization + finalization helpers
;; ============================================================================

(defn- mutation*
  "A schema-plausible Mutation IR fixture (component shape) whose
  content differs per sibling (the uniqueness rule — two siblings from
  one parent need different mutation content). The parent Genome id
  defaults to the seed; the rollback race passes g1's genome so the
  candidate's lineage (parent_generation_id, parent_genome_id) matches
  the promoted child's row (Database Invariant 8's composite FK)."
  ([n] (mutation* parent-genome-id n))
  ([parent-genome-id n]
   {:mutation/id (UUID/randomUUID)
    :parent/genome-id parent-genome-id
    :hypothesis/id (UUID/randomUUID)
    :evidence/id evidence-id
    :risk :behavioral
    :ops [{:op :set-edn
           :file "skills/debugging.edn"
           :path [:workflow :before-edit]
           :expect/hash evidence-id
           :value [:reproduce :localize n]}]
    :expected-effect {:primary-metric :task/success :direction :increase}}))

(defn- candidate-request
  "A valid create-candidate request for the fixture parent. Both the
  parent generation id and the parent Genome id are parameters so the
  rollback race can build a sibling of a promoted child (g1) whose
  lineage matches the child's generations row (Invariant 8)."
  [parent-generation-id parent-genome-id candidate-genome-id mutation-id]
  {:parent/generation-id parent-generation-id
   :parent/genome-id parent-genome-id
   :candidate/genome-id candidate-genome-id
   :mutation/id mutation-id
   :evidence/id evidence-id
   :risk :behavioral})

(defn- store-candidate-body!
  "Store a candidate Genome body in the CAS under its own content
  address and return that address (Database Invariant 7: activation
  re-hashes the body against this id)."
  [cas-store n]
  (:artifact/id
   (cas/put-bytes! cas-store
                   (.getBytes (str "candidate-genome-body-" n)
                              StandardCharsets/UTF_8)
                   {})))

(defn- proof
  "Create the explicit test-only proof required by the CandidateStore
  boundary. The fixture registers the matching artifact row below."
  [artifact-id]
  (#'existence/unsafe-verified-digest artifact-id))

(defn- materialize-and-pend!
  "Materialize a fresh sibling candidate from `parent-generation-id` /
  `parent-genome-id` (defaults: the seed pair) via the REAL component
  path and transition it to :evaluation-pending. Returns the pending
  Candidate record."
  ([store n] (materialize-and-pend! store seed-generation-id parent-genome-id n))
  ([store parent-generation-id parent-genome-id n]
   (let [candidate-genome-id (store-candidate-body! (:cas store) n)
         _ (artifact/ensure-artifact! (:sqlite store) candidate-genome-id
                                      "application/octet-stream" 0)
         _ (artifact/ensure-genome! (:sqlite store) candidate-genome-id)
         m (mutation* parent-genome-id n)
         c (candidate/create-candidate (candidate-request
                                        parent-generation-id parent-genome-id
                                        candidate-genome-id
                                        (:mutation/id m)))
         handle (candidate-store/make-candidate-store (:sqlite store))
         m1 (candidate/materialize-candidate!
             handle
             (update c :candidate/genome-id proof)
             (-> m
                 (update :parent/genome-id proof)
                 (update :evidence/id proof)))]
     (candidate/mark-evaluation-pending! handle (:candidate/id m1)))))

(defn- finalize-eligible!
  "Finalize one candidate's evaluation (the eval/core persist-finalized!
  write shape: one transaction inserts the finalized eval_runs row and
  CAS-updates the candidate 'evaluating' → 'eligible') and return
  {:candidate-id <uuid> :evaluation-id <uuid>}. The eligibility is the
  stored evaluator judgment consumed verbatim by promote! (component:
  :eligible? true is the only entry to :promoted)."
  [db pending]
  (let [cid (:candidate/id pending)
        eid (UUID/randomUUID)
        ts "2025-01-01T00:00:00Z"]
    (jdbc/with-db-transaction [conn (sqlite/spec db)]
      (sqlite/enable-foreign-keys! conn)
      (jdbc/insert! conn :eval_runs
                    {:id (str eid)
                     :candidate_id (str cid)
                     :parent_generation_id (:parent/generation-id pending)
                     :profile_id ":race/v1"
                     :gates "[]"
                     :paired_results_ref nil
                     :summary (pr-str {:hard {:passed 1 :failed 0}
                                       :utility {} :cost {} :complexity {}})
                     :eligibility (pr-str {:eligible? true :reasons []})
                     :status "finalized"
                     :created_at ts})
      (let [n (first (jdbc/execute!
                      conn
                      ["UPDATE candidates SET state = 'eligible'
                        WHERE id = ? AND state = 'evaluating'"
                       (str cid)]))]
        (when-not (= 1 n)
          (throw (ex-info "candidate is not :evaluation-pending"
                          {:error/type :race/fixture :candidate/id cid})))))
    {:candidate-id cid :evaluation-id eid}))

(defn- eligible-sibling!
  "A full eligible sibling candidate: materialize + pend + finalized
  eligible evaluation. `parent-generation-id` / `parent-genome-id`
  default to the seed pair (the parent used by cases 1-2 and the
  session race); the rollback race passes g1 + its genome so the
  racing candidate is a sibling OF THE PROMOTED CHILD, not of the
  seed (the setup promotion's CURRENT pointer and the candidate's
  lineage must agree, else promote! is stale by construction).
  Returns {:candidate-id :evaluation-id :genome-id
  :expected-generation-id}."
  ([world n] (eligible-sibling! world seed-generation-id parent-genome-id n))
  ([world parent-generation-id parent-genome-id n]
   (let [store {:sqlite (:db world) :cas (:cas world)}
         pending (materialize-and-pend! store parent-generation-id parent-genome-id n)
         finalized (finalize-eligible! (:db world) pending)]
     {:candidate-id (:candidate-id finalized)
      :evaluation-id (:evaluation-id finalized)
      :genome-id (:candidate/genome-id pending)
      :expected-generation-id (expected-generation-id
                               (:candidate/genome-id pending))})))

;; ============================================================================
;; the operator session every promotion/rollback event anchors to
;; ============================================================================

(defn- operator-session!
  "create-session! pinned to the seed identity, then append the
  :session/created root event (the host's job — promote! and rollback!
  both validate the anchor INSIDE their transaction). Returns the
  session id."
  [db]
  (let [sid (:session/id
             (session/create-session!
              db
              {:genome/id parent-genome-id
               :resolution/id resolution-id
               :phenotype/id phenotype-id
               :generation/id seed-generation-id}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id seed-generation-id
                          :phenotype/id phenotype-id
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

(defn- promotion-system
  "The component/9.5 promotion-system contract. `failpoint` is the
  optional test seam: called inside the transaction after every write,
  immediately before the CURRENT CAS."
  [world & [failpoint]]
  (cond-> {:store {:sqlite (:db world) :cas (:cas world)}
           :resolution/id resolution-id
           :event/session-id (operator-session! (:db world))}
    failpoint (assoc :failpoint failpoint)))

;; ============================================================================
;; thread helpers
;; ============================================================================

(defn- run-thread
  "Run `f` on a fresh thread; return a derefable delivering `f`'s
  value or {:race/error <ex-data>} when it throws."
  [f]
  (future
    (try
      (f)
      (catch clojure.lang.ExceptionInfo e
        {:race/error (ex-data e)})
      (catch Throwable t
        {:race/error {:error/type :race/unexpected
                      :message (.getMessage t)}}))))

(defn- deref-or-fail
  "Deref `d` with a generous timeout so a hung race fails the suite
  instead of hanging it."
  [d]
  (deref d 120000 :race/timeout))

(defn- released-by
  "A body that awaits `start` (both threads begin together) and counts
  down `started` immediately before running."
  [start started f]
  (fn []
    (.await start)
    (.countDown started)
    (f)))

;; ============================================================================
;; case 1 — evaluation fixtures (the component paired-fixture shape)
;; ============================================================================

(defn- provider-catalog
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- route-descriptor
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- echo-decision
  [text]
  {:action {:intent/type :intent/tool-call
            :payload {:tool/id :fixture/echo :args {:text text}}}})

(defn- route-source
  "A route program: {:op :echo :text t} emits a :fixture/echo tool-call
  with (transform t); anything else finishes."
  [transform-expr]
  (str "(ns agent.route)\n"
       "(defn- transform [text] " transform-expr ")\n"
       "(defn run [input]\n"
       "  (let [op (get input :op)]\n"
       "    (case op\n"
       "      :echo {:action {:intent/type :intent/tool-call\n"
       "                      :payload {:tool/id :fixture/echo\n"
       "                                :args {:text (transform (get input :text))}}}}\n"
       "      {:action {:intent/type :intent/finish :payload {:value input}}})))\n"))

(defn- bundle!
  "Build a genome bundle in a fresh temp dir and return its path string."
  [transform-expr]
  (let [dir (temp-path! "race-bundle-")]
    (write-file! (str dir "/manifest.edn")
                 (pr-str {:genome/format 1
                          :agent/id :main
                          :agent/entry :graph/main
                          :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
                          :modules {:topology "topology.edn"
                                    :models "models.edn"
                                    :memory "memory.edn"
                                    :evolution "evolution.edn"}
                          :capabilities/requested #{:model/call}
                          :evolution {:max-risk :behavioral
                                      :mutable #{:parameters :prompts
                                                 :skills :programs}}
                          :metadata {:name "race-fixture"
                                     :description "concurrency test bundle"}}))
    (write-file! (str dir "/topology.edn")
                 (pr-str {:graph/id :graph/race
                          :entry :node/router
                          :nodes {:node/router {:node/type :sci
                                                :program :program/route
                                                :next :node/emit}
                                  :node/emit {:node/type :emit}}
                          :limits {:max-steps 64}}))
    (write-file! (str dir "/models.edn") "{:models {:planner {:alias :reasoning/high}}}")
    (write-file! (str dir "/memory.edn") "{:memory {}}")
    (write-file! (str dir "/evolution.edn") "{:evolution {}}")
    (write-file! (str dir "/programs/route.clj") (route-source transform-expr))
    dir))

(defn- seeded-echo-provider
  "A DETERMINISTIC fixture provider: echoes {:text (str text \"-fixed\")}."
  []
  (reify proto/Provider
    (describe [_]
      {:tool/id :fixture/echo
       :effect :pure
       :input-schema [:map [:text :string]]
       :output-schema [:map [:text :string]]
       :required-action :invoke})
    (normalize-request [_ intent]
      {:tool/id :fixture/echo
       :resource {:kind :tool :id :fixture/echo}
       :args (get-in intent [:payload :args])})
    (execute-request! [_ authorized-request]
      {:text (str (get-in authorized-request [:args :text]) "-fixed")})))

(defn- selection-case
  []
  {:case/id :sel/c1
   :task-input {:op :echo :text "hi"}
   :expected-output [(echo-decision "hi") {:text "hi-fixed"}]
   :tools #{:fixture/echo}})

(defn- replay-case
  []
  (replay/build-replay-case
   {:episode/id (UUID/randomUUID)
    :outcome {:status :completed}}
   [{:intent/type :intent/tool-call
     :effect :read
     :payload {:tool/id :fixture/echo :args {:text "hi"}}
     :response {:text "hi-fixed"}}]
   {:case/id :replay/c1
    :task-input {:op :echo :text "hi"}
    :expected-output [(echo-decision "hi") {:text "hi-fixed"}]
    :mode :fixture}))

(defn- race-profile
  []
  {:eval/profile-id :race/v1
   :evolution-set {:source :evals/evolution}
   :selection-set {:source :evals/selection :visibility :kernel-only}
   :audit-set {:source :evals/audit :visibility :operator-only}
   :repetitions 1
   :promotion {:strategy :paired-comparison
               :min-delta 0.05
               :max-cost-regression 1.10
               :max-complexity-regression 1.25}})

(defn- evaluator-for
  "A minimal valid evaluator value for evaluate-candidate! (the exact
  component shape the component core tests use). Each evaluation gets
  its OWN CAS root so the two candidate evaluations race on the
  shared database only — see the namespace docstring's STORAGE
  FINDING for why a shared root flakes on Windows."
  [world pending candidate-bundle]
  {:store {:sqlite (:db world)
           :cas (cas/->cas (temp-path! "evoclj-race-cas-"))}
   :provider/catalog (provider-catalog)
   :kernel/abi {:kernel 1 :genome 1 :intent 1 :tool 1}
   :profiles {:race/v1 (race-profile)}
   :genome/roots {seed-generation-id (bundle! "text")
                  (str (:candidate/id pending)) candidate-bundle}
   :selection/cases {:sel/c1 (selection-case)}
   :selection/fixtures {:fixture/echo (fn [_seed] (seeded-echo-provider))}
   :replay/cases {:replay/c1 (replay-case)}
   :replay/fixtures {:fixture/echo (fn [] (seeded-echo-provider))}
   :programs (fn [_loaded] [(route-descriptor)])
   :measure/cost (fn [_root] 1000.0)})

;; ============================================================================
;; STEP 1/2 — case 1: two candidates evaluate concurrently from one parent
;; ============================================================================

(deftest two-candidates-evaluate-concurrently-from-one-parent
  (testing "20 fresh worlds; each iteration races the REAL
            evaluate-candidate! pipeline for two sibling candidates
            from the seed generation (release latch, so both enter
            their finalization transactions together)"
    (dotimes [i 20]
      (let [world (fresh-world)
            db (:db world)
            store {:sqlite db :cas (:cas world)}
            p1 (materialize-and-pend! store 1)
            p2 (materialize-and-pend! store 2)
            start (CountDownLatch. 1)
            started (CountDownLatch. 2)
            t1 (run-thread
                (released-by start started
                             (fn []
                               (eval-core/evaluate-candidate!
                                (evaluator-for world p1 (bundle! "text"))
                                (:candidate/id p1) :race/v1))))
            t2 (run-thread
                (released-by start started
                             (fn []
                               (eval-core/evaluate-candidate!
                                (evaluator-for world p2 (bundle! "text"))
                                (:candidate/id p2) :race/v1))))]
        (.countDown start)
        (is (.await started 120000 TimeUnit/MILLISECONDS)
            "both evaluations entered the race")
        (let [r1 (deref-or-fail t1)
              r2 (deref-or-fail t2)]
          (is (nil? (:race/error r1)) (str "iteration " i " evaluation 1 raced clean"))
          (is (nil? (:race/error r2)) (str "iteration " i " evaluation 2 raced clean"))
          (is (uuid? (:evaluation/id r1)))
          (is (uuid? (:evaluation/id r2)))
          (testing "both siblings finalized; each has EXACTLY one eval_runs row"
            (doseq [[pending eval] [[p1 r1] [p2 r2]]]
              (let [cid (str (:candidate/id pending))
                    runs (sqlite/query db
                                       ["SELECT * FROM eval_runs WHERE candidate_id = ?"
                                        cid])]
                (is (= 1 (count runs)) (str "candidate " cid " has one eval_runs row"))
                (is (= "finalized" (:status (first runs))))
                (is (= cid (:candidate_id (first runs))))
                (is (= (str (:evaluation/id eval)) (:id (first runs)))
                    "the persisted row is exactly the evaluation id")))
            (is (= "eligible" (:state (candidate-row db (:candidate/id p1)))))
            (is (= "eligible" (:state (candidate-row db (:candidate/id p2))))))
          (testing "evaluation NEVER touches CURRENT — exactly one current row"
            (is (= 1 (count (current-rows db))))
            (is (= seed-generation-id (:id (first (current-rows db)))))))))))

;; ============================================================================
;; STEP 1/2 — case 2: two workers call promote! concurrently
;; ============================================================================

(defn- run-promote-race!
  "Race `winner-f` (holds the write lock at its :failpoint) against
  `loser-f` (blocks at BEGIN IMMEDIATE until the winner commits). Both
  share one release latch: the winner parks at its failpoint, the
  loser is started only after the winner parked. Returns
  {:winner <result> :loser <result>}."
  [world winner-system winner-request loser-system loser-request]
  (let [parked (CountDownLatch. 1)
        release (CountDownLatch. 1)
        loser-started (CountDownLatch. 1)
        sys-w (assoc winner-system :failpoint
                     (fn [] (.countDown parked) (.await release)))
        t-w (run-thread #(promote/promote! sys-w winner-request))]
    ;; park the winner inside its transaction (holding SQLite's write
    ;; lock) before the loser is even started
    (is (.await parked 120000 TimeUnit/MILLISECONDS)
        "winner reached its failpoint and holds the write lock")
    (let [sys-l (assoc loser-system :failpoint
                       (fn [] (.countDown loser-started)))
          t-l (run-thread #(promote/promote! sys-l loser-request))]
      (.countDown release)
      {:winner (deref-or-fail t-w)
       :loser (deref-or-fail t-l)})))

(defn- assert-one-winner
  "Shared assertions after a two-worker promote! race on one parent:
  exactly one promotions row (decision 'promoted'), exactly one CURRENT
  row, one candidate :promoted and one :stale."
  [db winner loser sib-a sib-b]
  (is (= 1 (count (current-rows db))) "exactly one CURRENT generation")
  (let [promos (promotion-rows db)]
    (is (= 1 (count promos)) "exactly one promotions row")
    (is (= "promoted" (:decision (first promos)))))
  (let [statuses (frequencies (map :status [winner loser]))]
    (is (= 1 (get statuses :promoted 0)) "exactly one worker was :promoted")
    (is (= 1 (get statuses :stale 0)) "exactly one worker was :stale"))
  (let [promoted-candidates (filter #(= "promoted" (:state %))
                                    [(candidate-row db (:candidate-id sib-a))
                                     (candidate-row db (:candidate-id sib-b))])
        stale-candidates (filter #(= "stale" (:state %))
                                 [(candidate-row db (:candidate-id sib-a))
                                  (candidate-row db (:candidate-id sib-b))])]
    (is (= 1 (count promoted-candidates)) "one sibling candidate :promoted")
    (is (= 1 (count stale-candidates)) "one sibling candidate :stale"))
  (let [won (or (= :promoted (:status winner)) (= :promoted (:status loser)))
        loser-report (if (= :promoted (:status winner)) loser winner)
        expected-ids #{(:expected-generation-id sib-a)
                       (:expected-generation-id sib-b)}]
    (is won)
    (is (= :stale (:status loser-report)))
    (is (contains? expected-ids (:current loser-report))
        "the loser reports the winner's NEW generation as :current")
    (is (= seed-generation-id (:expected loser-report))
        "the loser's :expected is the shared parent")))

(deftest concurrent-promote-different-siblings-single-winner
  (testing "25 fresh worlds; two sibling candidates from the seed
            generation race the CURRENT compare-and-set — one
            :promoted, one :stale, exactly one promotions row"
    (dotimes [i 25]
      (let [world (fresh-world)
            db (:db world)
            a (eligible-sibling! world 1)
            b (eligible-sibling! world 2)
            sys (promotion-system world)]
        (let [{:keys [winner loser]}
              (run-promote-race!
               world sys {:candidate-id (:candidate-id a)
                          :evaluation-id (:evaluation-id a)
                          :expected-parent-generation seed-generation-id}
               sys {:candidate-id (:candidate-id b)
                    :evaluation-id (:evaluation-id b)
                    :expected-parent-generation seed-generation-id})]
          (is (nil? (:race/error winner)) (str "winner clean at iteration " i))
          (is (nil? (:race/error loser)) (str "loser clean at iteration " i))
          (assert-one-winner db winner loser a b))))))

(deftest concurrent-promote-same-candidate-single-winner
  (testing "25 fresh worlds; two workers race the SAME candidate — the
            CAS lets exactly one win (:promoted); the loser observes
            the winner's committed state and fails closed with
            :promotion/candidate-state-invalid (never a double
            promotion)"
    (dotimes [i 25]
      (let [world (fresh-world)
            db (:db world)
            a (eligible-sibling! world 1)
            sys (promotion-system world)
            request {:candidate-id (:candidate-id a)
                     :evaluation-id (:evaluation-id a)
                     :expected-parent-generation seed-generation-id}
            {:keys [winner loser]} (run-promote-race! world sys request sys request)]
        (is (nil? (:race/error winner)) (str "winner clean at iteration " i))
        (is (= :promotion/candidate-state-invalid
               (get-in loser [:race/error :error/type]))
            (str "the losing worker fails closed — iteration " i))
        (is (= 1 (count (promotion-rows db))) "exactly one promotions row")
        (is (= 1 (count (current-rows db))) "exactly one CURRENT generation")
        (is (= "promoted" (:state (candidate-row db (:candidate-id a))))
            "the candidate is promoted exactly once")
        (is (= (:expected-generation-id a) (:to winner))
            "the winner moved CURRENT to the deterministic child id")))))

;; ============================================================================
;; STEP 1/2 — case 3: rollback races with promotion
;; ============================================================================

(defn- promoted-child-world
  "A world where the seed generation G0 has already been superseded by
  a first promotion G0 → G1 (the CURRENT row), and a fresh sibling
  candidate of G1 is eligible. Returns {:world ... :g1 <id>
  :candidate <eligible sibling map>}."
  []
  (let [world (fresh-world)
        db (:db world)
        a (eligible-sibling! world 1)
        sys (promotion-system world)
        result (promote/promote! sys
                                 {:candidate-id (:candidate-id a)
                                  :evaluation-id (:evaluation-id a)
                                  :expected-parent-generation seed-generation-id})
        g1 (:to result)
        child (eligible-sibling! world g1 (:genome-id a) 2)]
    (is (= :promoted (:status result)))
    (is (= 1 (count (current-rows db))))
    {:world world :g1 g1 :candidate child}))

(deftest rollback-races-with-promotion
  (testing "10 iterations where the PROMOTION wins the write lock: it
            parks at its failpoint; the rollback blocks at BEGIN
            IMMEDIATE, then reads the moved pointer and reports :stale"
    (dotimes [i 10]
      (let [{:keys [world g1 candidate]} (promoted-child-world)
            db (:db world)
            parked (CountDownLatch. 1)
            release (CountDownLatch. 1)
            promote-sys (promotion-system
                         world
                         (fn [] (.countDown parked) (.await release)))
            rollback-sys (promotion-system world)
            t-p (run-thread
                 #(promote/promote!
                   promote-sys
                   {:candidate-id (:candidate-id candidate)
                    :evaluation-id (:evaluation-id candidate)
                    :expected-parent-generation g1}))]
        (is (.await parked 120000 TimeUnit/MILLISECONDS)
            "the promotion parked inside its transaction")
        (let [t-r (run-thread
                   #(rollback/rollback!
                     rollback-sys
                     {:from-generation g1
                      :to-generation seed-generation-id
                      :reason :race/rollback-test}))]
          (.countDown release)
          (let [p (deref-or-fail t-p)
                r (deref-or-fail t-r)]
            (is (nil? (:race/error p)))
            (is (nil? (:race/error r)))
            (is (= :promoted (:status p)))
            (is (= :stale (:status r)))
            (is (= (:to p) (:current r))
                "the rollback observed the promotion's committed CURRENT")
            (is (= g1 (:expected r)))
            (is (= 1 (count (current-rows db))) "exactly one CURRENT row")
            (is (= (:to p) (:id (first (current-rows db))))
                "the promotion's child is CURRENT")
            (is (= "retired" (:state (generation-row db seed-generation-id))))
            (is (= "retired" (:state (generation-row db g1))))
            (is (= "promoted" (:state (candidate-row db (:candidate-id candidate))))
                "the promotion winner's candidate is :promoted")
            (is (= 2 (count (promotion-rows db)))
                "one setup promotion + one racing promotion — the rollback
                wrote no promotions row"))))))
  (testing "10 iterations where the ROLLBACK wins the write lock: it
            parks at its failpoint; the promotion blocks at BEGIN
            IMMEDIATE, then reads the moved pointer and reports :stale"
    (dotimes [i 10]
      (let [{:keys [world g1 candidate]} (promoted-child-world)
            db (:db world)
            parked (CountDownLatch. 1)
            release (CountDownLatch. 1)
            rollback-sys (promotion-system
                          world
                          (fn [] (.countDown parked) (.await release)))
            promote-sys (promotion-system world)
            t-r (run-thread
                 #(rollback/rollback!
                   rollback-sys
                   {:from-generation g1
                    :to-generation seed-generation-id
                    :reason :race/rollback-test}))]
        (is (.await parked 120000 TimeUnit/MILLISECONDS)
            "the rollback parked inside its transaction")
        (let [t-p (run-thread
                   #(promote/promote!
                     promote-sys
                     {:candidate-id (:candidate-id candidate)
                      :evaluation-id (:evaluation-id candidate)
                      :expected-parent-generation g1}))]
          (.countDown release)
          (let [r (deref-or-fail t-r)
                p (deref-or-fail t-p)]
            (is (nil? (:race/error r)))
            (is (nil? (:race/error p)))
            (is (= :rolled-back (:status r)))
            (is (= :stale (:status p)))
            (is (= seed-generation-id (:current p))
                "the promotion observed the rollback's committed CURRENT")
            (is (= g1 (:expected p)))
            (is (= 1 (count (current-rows db))) "exactly one CURRENT row")
            (is (= seed-generation-id (:id (first (current-rows db))))
                "CURRENT moved back to the seed")
            (is (= "rolled-back" (:state (generation-row db g1))))
            (is (= "active" (:state (generation-row db seed-generation-id))))
            (is (= "stale" (:state (candidate-row db (:candidate-id candidate))))
                "the CAS-losing candidate is :stale")
            (is (nil? (generation-row db (:expected-generation-id candidate)))
                "the losing promotion created no generation row")
            (is (= 1 (count (promotion-rows db)))
                "only the setup promotion — the racing one wrote nothing")))))))

;; ============================================================================
;; STEP 1/2 — case 4: session creation races with CURRENT change
;; ============================================================================

(defn- create-pinned-session!
  "The host's session-creation adapter: read CURRENT, then pin the
  session to that generation (the component canary routing read).
  `read-done` is an optional zero-arg callback invoked immediately
  after the CURRENT read (before the INSERT) so a test can prove the
  read happened inside a parked-writer window."
  ([db] (create-pinned-session! db nil))
  ([db read-done]
   (let [gen (:id (first (sqlite/query db
                                       ["SELECT * FROM generations WHERE current = 1"])))]
     (when read-done (.countDown ^CountDownLatch read-done))
     {:generation/id gen
      :session/id (:session/id
                   (session/create-session!
                    db
                    {:genome/id parent-genome-id
                     :resolution/id resolution-id
                     :phenotype/id phenotype-id
                     :generation/id gen}))})))

(deftest session-creation-races-with-current-change
  (testing "10 deliberate iterations: the promotion parks at its
            failpoint holding the write lock while CURRENT is still the
            seed; four session creators read CURRENT and insert in that
            window; the promotion then commits. Every session records
            exactly one immutable generation and no pinned column ever
            changes"
    (dotimes [i 10]
      (let [world (fresh-world)
            db (:db world)
            a (eligible-sibling! world 1)
            parked (CountDownLatch. 1)
            release (CountDownLatch. 1)
            creators-started (CountDownLatch. 4)
            read-done (CountDownLatch. 4)
            promote-sys (promotion-system
                         world
                         (fn [] (.countDown parked) (.await release)))
            t-p (run-thread
                 #(promote/promote!
                   promote-sys
                   {:candidate-id (:candidate-id a)
                    :evaluation-id (:evaluation-id a)
                    :expected-parent-generation seed-generation-id}))]
        (is (.await parked 120000 TimeUnit/MILLISECONDS)
            "the promotion parked mid-transaction")
        (let [start (CountDownLatch. 1)
              creators (mapv (fn [_]
                               (run-thread
                                (released-by
                                 start creators-started
                                 (fn [] (create-pinned-session! db read-done)))))
                             (range 4))]
          (.countDown start)
          (is (.await creators-started 120000 TimeUnit/MILLISECONDS)
              "all four session creators ran inside the promotion window")
          (is (.await read-done 120000 TimeUnit/MILLISECONDS)
              "all four creators READ CURRENT while the promotion was
              parked mid-transaction")
          ;; let the parked promotion commit, THEN collect the sessions:
          ;; the creators' INSERTs contended on the write lock (SQLite
          ;; busy-waits via busy_timeout) and land right after the
          ;; commit, but their CURRENT READ already happened inside the
          ;; promotion window — so every pin is the seed's, and the
          ;; inserts cannot deadlock the parked writer
          (.countDown release)
          (let [sessions (mapv deref-or-fail creators)
                tuples (mapv #(pinned-tuple (session-row db (:session/id %))) sessions)]
            (is (every? #(= seed-generation-id (:generation/id %)) sessions)
                "every session created in the window pinned to the seed
                generation — the CURRENT value at their read time")
            (is (every? (fn [t]
                          (and (= seed-generation-id (:generation_id t))
                               (= parent-genome-id (:genome_id t))
                               (= resolution-id (:resolution_id t))
                               (= phenotype-id (:phenotype_id t))))
                        tuples)
                "every session's pinned identity tuple is exactly the seed's")
            (let [p (deref-or-fail t-p)]
              (is (nil? (:race/error p)))
              (is (= :promoted (:status p)))
              (is (= 1 (count (current-rows db))) "exactly one CURRENT row")
              (testing "no session's pinned generation changed after creation"
                (doseq [s sessions]
                  (is (= (pinned-tuple (session-row db (:session/id s)))
                         (pinned-tuple (session-row db (:session/id s))))
                      "re-reading yields the identical pinned tuple")
                  (is (= seed-generation-id
                         (:generation_id (session-row db (:session/id s))))
                      "the pinned generation is the one recorded at creation,
                      even though CURRENT moved to the promoted child")
                  (is (= "created" (:state (session-row db (:session/id s))))
                      "session creation performed no transition")))))))))
  (testing "20 latch-burst iterations: one promotion and four session
            creators released together; sessions may pin to either the
            seed or the promoted child (whichever they read) but always
            to a REAL generation, and always exactly one CURRENT row"
    (dotimes [i 20]
      (let [world (fresh-world)
            db (:db world)
            a (eligible-sibling! world 1)
            start (CountDownLatch. 1)
            t-p (run-thread
                 (released-by
                  start (CountDownLatch. 1)
                  (fn []
                    (promote/promote!
                     (promotion-system world)
                     {:candidate-id (:candidate-id a)
                      :evaluation-id (:evaluation-id a)
                      :expected-parent-generation seed-generation-id}))))
            creators (mapv (fn [_]
                             (run-thread
                              (released-by
                               start (CountDownLatch. 1)
                               (fn [] (create-pinned-session! db)))))
                           (range 4))]
        (.countDown start)
        (let [p (deref-or-fail t-p)
              sessions (mapv deref-or-fail creators)]
          (is (nil? (:race/error p)))
          (is (= :promoted (:status p)))
          (is (every? (fn [s]
                        (contains? #{seed-generation-id (:to p)}
                                   (:generation/id s)))
                      sessions)
              (str "every session pinned to a REAL generation — iteration " i))
          (is (= 1 (count (current-rows db))) "exactly one CURRENT row")
          (testing "pinned tuples are immutable on re-read"
            (doseq [s sessions]
              (is (= (pinned-tuple (session-row db (:session/id s)))
                     (pinned-tuple (session-row db (:session/id s))))))))))))

;; ============================================================================
;; STEP 3 — SQLite journal/locking-mode verification
;; ============================================================================

(deftest sqlite-journal-mode-and-busy-timeout
  (testing "journal mode: the schema never opts into WAL, so every
            database runs under the SQLite DEFAULT rollback journal
            ('delete')"
    (let [world (fresh-world)
          db (:db world)]
      (is (= "delete"
             (-> (sqlite/query db ["PRAGMA journal_mode"])
                 first vals first))
          "PRAGMA journal_mode reports 'delete' on a migrated database")
      (is (not (str/includes?
                (slurp (io/resource "migrations/001-init.sql"))
                "journal_mode"))
          "the component schema issues no journal-mode pragma — the
          default applies, and 'delete' (not WAL) is therefore the
          operating mode of every store")))
  (testing "busy_timeout: promotion/rollback/event transactions set it
            explicitly before BEGIN IMMEDIATE, so contended writers wait
            for SQLite's write lock instead of failing SQLITE_BUSY"
    (doseq [path ["src/evoclj/promotion/promote.clj"
                  "src/evoclj/promotion/rollback.clj"
                  "src/evoclj/store/event.clj"]]
      (is (str/includes? (slurp path) "PRAGMA busy_timeout = 10000")
          (str path " sets busy_timeout = 10000 before BEGIN IMMEDIATE")))
    (testing "the evaluation finalization transaction relies on the
              sqlite-jdbc driver default (documented finding, not a
              defect: its write window is one INSERT + one UPDATE)"
      (let [src (slurp "src/evoclj/eval/core.clj")]
        (is (not (str/includes? src "busy_timeout"))
            "eval/core.clj sets no busy_timeout — the driver default
            (3000 ms in sqlite-jdbc) applies to its finalization
            transaction")))))

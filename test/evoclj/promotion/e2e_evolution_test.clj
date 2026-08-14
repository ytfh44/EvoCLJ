(ns evoclj.promotion.e2e-evolution-test
  "Task 9.7 — Milestone 9 exit: the complete end-to-end evolutionary
  promotion test.

  ONE black-box test (Step 1) walks the NORMATIVE scenario through
  ONLY public subsystem interfaces:

      load G1 (test/fixtures/evolution-e2e/route-a)
      → compile G1
      → scheduler runs Evolution-set episodes (temp store/CAS):
        an :echo-a request COMPLETES under tool A; two :echo-b
        requests FAIL (the A-for-everything router raises, the
        sessions end :failed, the episodes are failures)
      → build-evidence-pack freezes the episodes
      → the REAL pattern-diagnostician proposes hypothesis H
        (:task/success — the pack's success rate is below 1.0)
      → a deterministic Mutator produces Δ (a :replace-form op on the
        mutable route program: class B is now served by tool B)
      → apply-mutation materializes the candidate bundle
      → compile → propose-candidates! persists Candidate G2
      → evaluate-candidate! runs G0-G6: the replay proves the old A
        request still passes, and the PAIRED hidden Selection (both
        A and B cases, loaded through the evaluator-only
        dataset/selection-loader) shows G2 beats G1 above the
        profile's :min-delta with no hard/cost regression
      → promote! CAS-changes CURRENT from G1's generation to G2's
      → the already-running G1 session stays pinned to G1; a new
        session receives G2 (canary routing + a real G2 run)
      → lineage reconstructs G1 + evidence + Δ + evaluation → G2

  The steps then assert the isolation firewall (Step 2 — the hidden
  Selection fixture is unreachable from the Diagnostician/Mutator
  adapters and never mounted into candidate workspaces), the durable
  events/artifacts (Step 3), the stale promotion branch (Step 4 — a
  second candidate from G1 promoted after G2's success returns :stale
  without touching the winning branch), and the store restart
  (Step 5 — close + reopen the sqlite/CAS and re-run the lineage /
  CURRENT checks).

  FIXTURE GENOMES: test/fixtures/evolution-e2e/route-a is the G1
  bundle; programs/route.clj chooses tool A (:fixture/echo) for every
  request — class A (:echo-a) is served, class B (:echo-b) FAILS under
  that policy. test/fixtures/evolution-e2e/route-b is the REFERENCE G2
  bundle whose decision table the deterministic mutation must
  reproduce (:echo-a → tool A, :echo-b → tool B). The hidden Selection
  dataset lives in test/fixtures/evolution-e2e/selection (two cases:
  :sel/a exercises tool A, :sel/b — marked :critical? — exercises
  tool B). The Evolution/Audit dataset roots are the sibling
  evolution/ and audit/ directories.

  THE CASE CONTRACT (documented here, minimal by design):

      {:case/id <keyword>
       :task-input <EDN>            ; fed to the session
       :expected-output <EDN>       ; the oracle — byte-identical by
                                    ;   default; the session's
                                    ;   accumulated outputs
                                    ;   [<route decision> <provider
                                    ;   result>]
       :tools #{<tool/id> ...}      ; the tools the case exercises
       :critical? <bool>}           ; optional; a lost critical case
                                    ;   fails G5"

  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.compiler.core :as compiler]
            [evoclj.eval.core :as eval-core]
            [evoclj.eval.dataset :as dataset]
            [evoclj.eval.replay :as replay]
            [evoclj.evolution.budget :as budget]
            [evoclj.evolution.core :as core]
            [evoclj.evolution.diagnose :as diagnose]
            [evoclj.genome.load :as load]
            [evoclj.genome.path :as gpath]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [evoclj.promotion.canary :as canary]
            [evoclj.promotion.current :as current]
            [evoclj.promotion.lineage :as lineage]
            [evoclj.promotion.promote :as promote]
            [evoclj.runtime.episode :as episode]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.sci.execute :as execute]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util UUID)))

;; ============================================================================
;; fixture identity
;; ============================================================================

(def ^:private generation-id
  "G1's stable generation id (the CURRENT seed row)."
  "generation-1")

(defn- route-a-root
  "The G1 Genome bundle directory (test/fixtures/evolution-e2e/route-a)."
  []
  (str (io/file "test" "fixtures" "evolution-e2e" "route-a")))

(defn- route-b-root
  "The reference G2 Genome bundle directory
  (test/fixtures/evolution-e2e/route-b)."
  []
  (str (io/file "test" "fixtures" "evolution-e2e" "route-b")))

(defn- selection-root
  "The hidden Selection dataset root
  (test/fixtures/evolution-e2e/selection)."
  []
  (str (io/file "test" "fixtures" "evolution-e2e" "selection")))

(defn- evolution-root
  "The Evolution dataset root (test/fixtures/evolution-e2e/evolution)."
  []
  (str (io/file "test" "fixtures" "evolution-e2e" "evolution")))

(defn- audit-root
  "The Audit dataset root (test/fixtures/evolution-e2e/audit)."
  []
  (str (io/file "test" "fixtures" "evolution-e2e" "audit")))

(defn- route-descriptor
  "The route program descriptor (Task 2.3 choice (a): an in-memory
  descriptor list riding on the loaded-genome value under :programs)."
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- fixture-catalog
  "The on-disk provider catalog fixture (Task 2.1 Resolution)."
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- loaded-route-a
  "G1 loaded from disk with its program registry attached."
  []
  (assoc (load/load-genome (route-a-root))
         :programs [(route-descriptor)]))

(defn- deterministic-uuid
  "A deterministic name-based (v3) UUID over a string — the fixture
  Mutator's deterministic :mutation/id convention."
  [s]
  (UUID/nameUUIDFromBytes (.getBytes s StandardCharsets/UTF_8)))

(defn- program-sources
  "Decode every compiled program's source text from the immutable
  loaded bundle :files (Global Constraint 22)."
  [loaded compiled]
  (into {}
        (map (fn [[program-id descriptor]]
               [program-id
                (String. ^bytes (byte-array
                                 (get-in loaded
                                         [:files (:file descriptor) :bytes]))
                         StandardCharsets/UTF_8)]))
        (:programs compiled)))

(defn- compile-bundle
  "Load a bundle root and compile it with the fixture catalog and the
  route program registry; returns {:loaded ... :compiled ...}."
  [bundle-root]
  (let [loaded (assoc (load/load-genome bundle-root)
                      :programs [(route-descriptor)])]
    {:loaded loaded
     :compiled (compiler/compile-genome loaded (fixture-catalog))}))

;; ============================================================================
;; the deterministic :fixture/echo-b provider (tool B)
;; ============================================================================

(defn- echo-b-provider
  "The deterministic tool-B fixture provider: a pure echo of :text
  under the distinct tool id :fixture/echo-b."
  []
  (reify proto/Provider
    (describe [_]
      {:tool/id :fixture/echo-b
       :effect :pure
       :input-schema [:map [:text :string]]
       :output-schema [:map [:text :string]]
       :required-action :invoke
       :retry {:safe? true}})
    (normalize-request [_ intent]
      (let [args (get-in intent [:payload :args])]
        (when-not (map? args)
          (throw (ex-info "tool-call payload must carry an :args map"
                          {:error/type :provider/input-invalid})))
        {:tool/id :fixture/echo-b
         :resource {:kind :tool :id :fixture/echo-b}
         :args args}))
    (execute-request! [_ authorized-request]
      {:text (get-in authorized-request [:args :text])})))

;; ============================================================================
;; the deterministic Mutator (reads ONLY the orchestration context)
;; ============================================================================

(defn- g2-case-form
  "The replacement :case form the winning mutation writes into
  programs/route.clj: :echo-a is still served by tool A, :echo-b is
  now served by tool B, anything else finishes. Pure,
  allowlist-clean, deterministic (Global Constraint 6)."
  []
  (list 'case 'op
        :echo-a {:action (list 'tool-call-intent :fixture/echo
                               {:text (list 'get 'input :text)})}
        :echo-b {:action (list 'tool-call-intent :fixture/echo-b
                               {:text (list 'get 'input :text)})}
        {:action (list 'finish-intent 'input)}))

(defn- g2c-case-form
  "The stale-branch variant: like G2 plus :echo-c → tool B (distinct
  mutation content → a distinct candidate from the same parent)."
  []
  (list 'case 'op
        :echo-a {:action (list 'tool-call-intent :fixture/echo
                               {:text (list 'get 'input :text)})}
        :echo-b {:action (list 'tool-call-intent :fixture/echo-b
                               {:text (list 'get 'input :text)})}
        :echo-c {:action (list 'tool-call-intent :fixture/echo-b
                               {:text (list 'get 'input :text)})}
        {:action (list 'finish-intent 'input)}))

(defn- route-replacement-op
  "The deterministic :replace-form op on the mutable programs/route.clj:
  replace the :case form (the only top-level `case` form) with `form`.
  :expect/hash is the parent's own file digest (a stale digest fails
  the patch preimage gate)."
  [parent-genome form]
  {:op :replace-form
   :file "programs/route.clj"
   :selector ['case]
   :expect/hash (get-in parent-genome [:files "programs/route.clj" :digest])
   :form form})

(defn- delta-mutation
  "One fully-determined Mutation IR for one case-form variant: parent,
  the diagnosis's :task/success hypothesis, the frozen pack's
  :evidence/id, the :program risk class, and the expected effect.
  The :mutation/id is a deterministic name-based UUID over the content
  plus the variant suffix."
  [parent diagnosis hypothesis form suffix]
  (let [content {:parent/genome-id (:genome/id parent)
                 :hypothesis/id (:hypothesis/id hypothesis)
                 :evidence/id (:evidence/id diagnosis)
                 :risk :program
                 :ops [(route-replacement-op parent form)]
                 :expected-effect {:primary-metric :task/success
                                   :direction :increase}}]
    (assoc content
           :mutation/id (deterministic-uuid (pr-str [content suffix])))))

(defn- propose-deltas
  "The deterministic Mutator body: when the diagnosis carries a
  :task/success hypothesis, propose TWO fully-determined mutations —
  the winning G2 (A→A, B→B) and the stale-branch variant (A→A, B→B,
  C→B). Proposes nothing otherwise. Reads ONLY the context it is
  handed."
  [ctx]
  (when-let [hypothesis (some #(when (= :task/success (:pattern %)) %)
                              (:hypotheses (:diagnosis ctx)))]
    (let [parent (:parent-genome ctx)
          diagnosis (:diagnosis ctx)]
      [(delta-mutation parent diagnosis hypothesis (g2-case-form) "g2")
       (delta-mutation parent diagnosis hypothesis (g2c-case-form) "g2c")])))

(defn- recording-mutator
  "The Mutator adapter under test: captures the exact context the
  orchestrator hands it (Step 2 isolation evidence), then proposes the
  deterministic deltas."
  [captured]
  (reify core/Mutator
    (propose-mutations [_ context]
      (reset! captured context)
      (propose-deltas context))))

(defn- recording-diagnostician
  "The Diagnostician adapter under test: the REAL deterministic
  pattern-diagnostician (Task 7.2), wrapped only to capture the exact
  evidence pack it receives (Step 2 isolation evidence)."
  [captured]
  (let [inner (diagnose/pattern-diagnostician
               {:task/success-threshold 1.0
                :max-hypotheses 3
                :confidence-band :medium})]
    (reify diagnose/Diagnostician
      (diagnose [_ pack]
        (reset! captured pack)
        (diagnose/diagnose inner pack)))))

;; ============================================================================
;; temp stores (test temp dirs only — Global Constraint 23)
;; ============================================================================

(def ^:private temp-paths (atom []))

(defn- temp-db-path
  []
  (let [p (str (Files/createTempFile "evoclj-e2e-evolution-" ".db"
                                     (make-array FileAttribute 0)))]
    (swap! temp-paths conj p)
    p))

(defn- temp-dir
  [prefix]
  (let [d (str (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (swap! temp-paths conj d)
    d))

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

(use-fixtures :each (fn [f] (f) (cleanup!)))

;; ============================================================================
;; host glue: stores, executors, sessions
;; ============================================================================

(defn- fresh-store
  "A migrated temp database seeded with G1's generation row
  (current = 1, Database Invariant 6) plus a temp CAS root. Returns
  {:store {:sqlite ... :cas ...} :db-path ... :cas-root ...}."
  []
  (let [db-path (temp-db-path)
        db (sqlite/spec db-path)
        cas-root (temp-dir "evoclj-e2e-evolution-cas-")
        cas-store (cas/->cas cas-root)
        g1 (loaded-route-a)]
    (migrate/migrate! db)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id
                     :genome_id (:genome/id g1)
                     :resolution_id (str "sha256:" (apply str (repeat 64 "f")))
                     :parent_id nil
                     :state "active"
                     :current 1
                     :created_at "2025-01-01T00:00:00Z"}))
    {:store {:sqlite db :cas cas-store}
     :db-path db-path
     :cas-root cas-root}))

(defn- genome-index-bytes
  "The canonical index bytes whose SHA-256 is the genome's content
  address — the exact serialization of evoclj.genome.hash/tree-digest
  (per bytewise-sorted path: path + NUL + digest + LF). Storing these
  bytes under the genome id is the HOST's job before any promotion or
  lineage integrity check (Database Invariant 7)."
  [loaded]
  (apply str
         (map (fn [[p {:keys [digest]}]]
                (str p "\u0000" digest "\n"))
              (sort-by (fn [[p _]] p) gpath/bytewise-compare (:files loaded)))))

(defn- store-genome-body!
  "Store a loaded Genome's canonical body in the CAS under its content
  address; returns the artifact id (which must equal the genome id)."
  [cas-store loaded]
  (:artifact/id
   (cas/put-bytes! cas-store
                   (.getBytes (genome-index-bytes loaded)
                              StandardCharsets/UTF_8)
                   {})))

(defn- leases-for
  "One CapabilityLease per tool id, granting the exact phenotype id
  the tool's :invoke action."
  [phenotype-id tool-ids]
  (let [now (java.util.Date.)
        expires (java.util.Date. (+ (.getTime now) 60000))]
    (mapv (fn [tool-id]
            {:cap/id (random-uuid)
             :subject {:phenotype/id phenotype-id}
             :resource {:kind :tool :id tool-id}
             :actions #{:invoke}
             :constraints {:max-calls 10000}
             :issued-at now
             :expires-at expires})
          tool-ids)))

(defn- build-executor
  "Assemble a scheduler executor for one Genome bundle root: compile
  from scratch, instantiate a fresh Phenotype, register both fixture
  tools, grant both leases. Returns {:executor ... :compiled ...}."
  [store bundle-root]
  (let [{:keys [loaded compiled]} (compile-bundle bundle-root)
        reg (registry/create-registry)
        _ (registry/register! reg (fixture/echo-provider {}))
        _ (registry/register! reg (echo-b-provider))
        usage (atom {})
        leases (leases-for (:compiled/phenotype-id compiled)
                           [:fixture/echo :fixture/echo-b])
        ph (phenotype/instantiate
            compiled
            {:stores {:sqlite :poison :cas {:root :poison}}
             :providers {:registry reg}
             :capabilities {:leases leases :usage usage}
             :program-sources (program-sources loaded compiled)})]
    {:executor {:phenotype ph
                :stores store
                :dispatch (dispatch/make-broker-context
                           {:registry reg :leases leases :usage usage})}
     :compiled compiled}))

(defn- create-pinned-session!
  "create-session! pinned to `compiled`'s identity under `gen`, then
  append the :session/created root event (the host's job). Returns the
  session id."
  [db compiled gen]
  (let [sid (:session/id
             (session/create-session!
              db
              {:genome/id (:compiled/genome-id compiled)
               :resolution/id (:compiled/resolution-id compiled)
               :phenotype/id (:compiled/phenotype-id compiled)
               :generation/id gen}))]
    (event/append-event! db
                         {:session/id sid
                          :generation/id gen
                          :phenotype/id (:compiled/phenotype-id compiled)
                          :event/type :session/created
                          :cause/event-id nil
                          :payload-ref nil
                          :metadata {}})
    sid))

(defn- run-episode!
  "Run one G1 session through the scheduler and materialize its
  Episode. Returns {:result ... :session/id ... :episode ...}."
  [executor db compiled task]
  (let [sid (create-pinned-session! db compiled generation-id)
        result (scheduler/run-session! executor sid task)
        ep (episode/materialize-episode! {:sqlite db :cas (:cas (:stores executor))}
                                         sid)]
    {:result result :session/id sid :episode ep}))

(defn- candidate-bundle-root
  "The finalized candidate bundle directory under :candidates-dir
  (the same name rule as Task 7.4 finalize: the content address with
  ':' replaced)."
  [candidates-dir genome-id]
  (str candidates-dir java.io.File/separator (str/replace genome-id ":" "-")))

(defn- read-artifact
  "Read a CAS artifact back as EDN data."
  [store artifact-id]
  (edn/read-string
   (String. (cas/get-bytes (:cas store) artifact-id)
            StandardCharsets/UTF_8)))

(defn- replay-case-from-session
  "Build the G4 replay case from a REAL recorded G1 session: the
  task input, the recorded route decision (the session's accumulated
  outputs), and the recorded provider response read back from the CAS
  :provider/call-completed artifact."
  [store sid task expected-output]
  (let [events (event/events-for-session (:sqlite store) sid)
        completed (first (filter #(= :provider/call-completed (:event/type %))
                                 events))
        response (read-artifact store (:payload-ref completed))
        decision (first expected-output)
        payload (get-in decision [:action :payload])]
    (replay/build-replay-case
     {:episode/id (random-uuid)
      :outcome {:status :completed}}
     [{:intent/type :intent/tool-call
       :effect :read
       :payload payload
       :response response}]
     {:case/id :replay/a
      :task-input task
      :expected-output expected-output
      :mode :fixture})))

(defn- e2e-profile
  "The Task 8.1 profile carrying the Task 8.5 thresholds. The three
  dataset :source keywords resolve — via the evaluator's :dataset/roots
  — to the fixture roots; the selection set is :kernel-only (Global
  Constraint 11) and the audit set :operator-only."
  []
  {:eval/profile-id :e2e/v1
   :evolution-set {:source :evals/evolution}
   :selection-set {:source :evals/selection :visibility :kernel-only}
   :audit-set {:source :evals/audit :visibility :operator-only}
   :repetitions 1
   :promotion {:strategy :paired-comparison
               :min-delta 0.05
               :max-cost-regression 1.10
               :max-complexity-regression 1.25}})

(defn- selection-cases
  "Resolve the hidden Selection case bodies through the EVALUATOR-ONLY
  dataset/selection-loader (the sanctioned loader surface for case
  bodies; the host assembles the evaluator with the resolved cases, so
  the Diagnostician/Mutator adapters never see them)."
  []
  (let [loader (dataset/selection-loader
                (e2e-profile)
                {:evals/evolution (evolution-root)
                 :evals/selection (selection-root)
                 :evals/audit (audit-root)})]
    (into {} (map (fn [c] [(:case/id c) c])) (loader))))

(defn- build-evaluator
  "The Task 8.7 evaluator value for one candidate: the selection set
  is resolved ONLY through dataset/selection-loader over the fixture
  :dataset/roots (never mounted into any workspace); the replay case
  is derived from the recorded G1 A-class session; both sides are
  re-evaluated from their bundle roots."
  [store candidates-dir candidate-record replay-case]
  (let [cid (:candidate/id candidate-record)
        candidate-root (candidate-bundle-root candidates-dir
                                              (:candidate/genome-id candidate-record))]
    {:store store
     :provider/catalog (fixture-catalog)
     :kernel/abi {:kernel 1 :genome 1 :intent 1 :tool 1}
     :profiles {:e2e/v1 (e2e-profile)}
     :genome/roots {generation-id (route-a-root)
                    (str cid) candidate-root}
     :selection/cases (selection-cases)
     :selection/fixtures {:fixture/echo (fn [_seed] (fixture/echo-provider {}))
                          :fixture/echo-b (fn [_seed] (echo-b-provider))}
     :replay/cases {:replay/a replay-case}
     :replay/fixtures {:fixture/echo (fn [] (fixture/echo-provider {}))
                       :fixture/echo-b (fn [] (echo-b-provider))}
     :programs (fn [_loaded] [(route-descriptor)])
     :measure/cost (fn [_root] 1000.0)}))

;; ============================================================================
;; THE SCENARIO — one black-box test (Step 1)
;; ============================================================================

(deftest e2e-evolutionary-promotion
  (let [fx (fresh-store)
        store (:store fx)
        db (:sqlite store)
        cas-store (:cas store)
        g1-loaded (loaded-route-a)
        g1-id (:genome/id g1-loaded)
        g1-ctx (build-executor store (route-a-root))
        g1-compiled (:compiled g1-ctx)
        g1-executor (:executor g1-ctx)
        _ (is (re-matches #"^sha256:[0-9a-f]{64}$" g1-id)
            "G1 is content-addressed")
        _ (is (= g1-id (store-genome-body! cas-store g1-loaded))
            "the host stores G1's canonical body under its own content address")
        _ (is (= g1-id (:compiled/genome-id g1-compiled))
            "the compiled G1 names the loaded bundle's address")
        ;; ---------------- the Evolution-set episodes (G1 runs) ------------
        task-a {:op :echo-a :text "hi"}
        task-b1 {:op :echo-b :text "bo"}
        task-b2 {:op :echo-b :text "go"}
        run-a (run-episode! g1-executor db g1-compiled task-a)
        run-b1 (run-episode! g1-executor db g1-compiled task-b1)
        run-b2 (run-episode! g1-executor db g1-compiled task-b2)]

    (testing "the scenario preamble — G1 chooses tool A for every request;
              class-B requests FAIL under that policy"
      (is (= :completed (:status (:result run-a))))
      (is (= :failed (:status (:result run-b1))))
      (is (= :failed (:status (:result run-b2))))
      (is (= :completed (get-in (:episode run-a) [:outcome :status])))
      (is (= :failed (get-in (:episode run-b1) [:outcome :status])))
      (is (= :failed (get-in (:episode run-b2) [:outcome :status])))
      (let [a-output (read-artifact store (:output-ref (:result run-a)))]
        (is (= {:tool/id :fixture/echo}
               (select-keys (get-in a-output [0 :action :payload]) [:tool/id]))
            "the G1 router chooses tool A")
        (is (= [{:action {:intent/type :intent/tool-call
                          :payload {:tool/id :fixture/echo
                                    :args {:text "hi"}}}}
                {:text "hi"}]
               a-output))))

    (testing "STEP 1 — the deterministic Diagnostician proposes hypothesis H
              from the Evolution-set evidence pack; the deterministic
              Mutator produces Δ changing the SCI router"
      (let [mutator-context (atom nil)
            diag-pack (atom nil)
            candidates-dir (temp-dir "evoclj-e2e-evolution-candidates-")
            events (atom [])
            phases (atom [])
            system {:store store
                    :provider-catalog (fixture-catalog)
                    :genome-root (route-a-root)
                    :candidates-dir candidates-dir
                    :diagnostician (recording-diagnostician diag-pack)
                    :mutator (recording-mutator mutator-context)
                    :budget-profile budget/v0-profile
                    :programs-registry [(route-descriptor)]
                    :event-sink (fn [event] (swap! events conj event))
                    :phase-hook (fn [phase] (swap! phases conj phase))}
            request {:generation/id generation-id
                     :evidence-selector {:recent 3 :include-successes 1
                                         :include-failures 2 :include-high-cost 1}
                     :max-candidates 2}
            candidates (core/propose-candidates! system request)
            [g2 g2c] candidates
            pack @diag-pack
            context @mutator-context
            diag-event (first (filter #(= :evolution/diagnosis-created (:event/type %))
                                      @events))
            diagnosis (read-artifact store (get-in diag-event [:metadata :diagnosis/id]))]
        (is (= 2 (count candidates))
            "one cycle produces the winning G2 and the stale-branch candidate")
        (is (every? #(= :evaluation-pending (:state %)) candidates))
        (is (= g1-id (:parent/genome-id g2)))
        (is (not= g1-id (:candidate/genome-id g2)) "G2 is a new content address")
        (is (not= (:candidate/genome-id g2) (:candidate/genome-id g2c))
            "the two candidates are distinct content")
        (testing "the evidence pack shows the class-B failures (successes 1, failures 2)"
          (is (= generation-id (:generation/id pack)))
          (is (= 3 (count (:episodes pack))))
          (is (= 1 (get-in pack [:summary :successes])))
          (is (= 2 (get-in pack [:summary :failures]))))
        (testing "the hypothesis is the deterministic :task/success pattern"
          (is (= 1 (count (:hypotheses diagnosis))))
          (is (= :task/success (get-in diagnosis [:hypotheses 0 :pattern])))
          (is (= (:evidence/id pack) (:evidence/id diagnosis))
              "the diagnosis is self-provenancing to the frozen pack")
          (is (= (:evidence/id pack) (:evidence/id g2))
              "the candidate's mutation answers the frozen pack"))
        (testing "the Δ changes the SCI router — G2 uses tool B only for
                  class-B requests"
          (let [ops (-> (first (sqlite/query db
                                             ["SELECT ops FROM mutations WHERE id = ?"
                                              (str (:mutation/id g2))]))
                        :ops
                        edn/read-string)]
            (is (= :replace-form (get-in ops [0 :op])))
            (is (= "programs/route.clj" (get-in ops [0 :file])))
            (is (= :echo-b (nth (get-in ops [0 :form]) 4))
                "the new case form still names the :echo-b class")
            (is (= :fixture/echo-b (second (:action (nth (get-in ops [0 :form]) 5))))
                "the new case form routes :echo-b to tool B"))
          (let [g2-bundle-root (candidate-bundle-root candidates-dir
                                                      (:candidate/genome-id g2))
                {:keys [loaded compiled]} (compile-bundle g2-bundle-root)
                runtime (:sci-runtime
                         (phenotype/instantiate
                          compiled
                          {:stores {:sqlite :poison :cas {:root :poison}}
                           :providers {:registry (registry/create-registry)}
                           :capabilities {:leases [] :usage (atom {})}
                           :program-sources (program-sources loaded compiled)}))]
            (is (= {:status :ok
                    :value {:action {:intent/type :intent/tool-call
                                     :payload {:tool/id :fixture/echo-b
                                               :args {:text "bo"}}}}}
                   (select-keys (execute/invoke! runtime :program/route
                                                 task-b1 nil)
                                [:status :value]))
                "the mutated candidate routes class B to tool B")
            (is (= {:tool/id :fixture/echo}
                   (-> (execute/invoke! runtime :program/route task-a nil)
                       :value :action :payload (select-keys [:tool/id])))
                "class A still routes to tool A"))
          (testing "the mutated route program reproduces the reference G2
                    decision table (route-b)"
            (let [g2-bundle-root (candidate-bundle-root candidates-dir
                                                        (:candidate/genome-id g2))
                  invoke-decision (fn [bundle-root input]
                                    (let [{:keys [loaded compiled]}
                                          (compile-bundle bundle-root)
                                          runtime (:sci-runtime
                                                   (phenotype/instantiate
                                                    compiled
                                                    {:stores {:sqlite :poison
                                                              :cas {:root :poison}}
                                                     :providers {:registry
                                                                 (registry/create-registry)}
                                                     :capabilities {:leases []
                                                                    :usage (atom {})}
                                                     :program-sources
                                                     (program-sources loaded compiled)}))]
                                      (select-keys (execute/invoke! runtime
                                                                    :program/route
                                                                    input nil)
                                                   [:status :value])))]
              (doseq [input [task-a task-b1 {:op :unknown :x 1}]]
                (is (= (invoke-decision (route-b-root) input)
                       (invoke-decision g2-bundle-root input))
                    (str "patched G2 == route-b on " (pr-str input)))))))
        (testing "STEP 2 — the hidden Selection fixture is NOT reachable
                  from the Diagnostician/Mutator adapters"
          (testing "the Diagnostician received ONLY the evidence pack"
            (is (map? pack))
            (is (= #{:evidence/id :generation/id :cutoff-event-id :episodes
                     :summary}
                   (set (keys pack)))
                "the pack is store-derived metadata + excerpt refs, nothing else")
            (is (not (str/includes? (pr-str pack) (selection-root)))
                "the selection dataset path never appears in the pack")
            (is (empty? (filter #(contains? #{:selection :case/body
                                              :expected-output :prompt} %)
                                (mapcat keys (:episodes pack))))
                "no selection case bodies/keys leak into the pack"))
          (testing "the Mutator received ONLY the documented context"
            (is (= #{:generation/id :parent/genome-id :parent-genome
                     :diagnosis :history :budget-profile}
                   (set (keys context)))
                "the closed Mutator context carries no selection/audit surface")
            (is (not (str/includes? (pr-str context) (selection-root)))
                "the selection dataset path never reaches the Mutator")
            (is (not-any? fn? (vals context))
                "no loader handles or fns cross into the Mutator (GC 22)"))
          (testing "the candidate evaluation workspace never mounts the
                    Selection dataset (Global Constraints 11, 23)"
            (let [staging (temp-dir "evoclj-e2e-evolution-workspace-")
                  ws (dataset/build-candidate-workspace!
                      staging
                      {:evals/evolution (evolution-root)
                       :evals/selection (selection-root)
                       :evals/audit (audit-root)})]
              (is (not-any? #(str/includes? % "selection")
                            (:workspace/entries ws))
                  "no evals/selection path is staged into the workspace")
              (is (not-any? #(str/includes? % (selection-root))
                            (:workspace/entries ws))))))
        (testing "STEP 3 — all expected events and artifacts exist"
          (is (= [:freeze-evidence :diagnose :load-history :propose-mutation]
                 (take 4 @phases))
              "the cycle ran the normative front phase order")
          (is (= (concat [:validate-risk-budget :apply-patch :compile-candidate
                          :persist-candidate]
                         [:validate-risk-budget :apply-patch :compile-candidate
                          :persist-candidate])
                 (drop 4 @phases))
              "the per-candidate phases repeat once per adopted mutation")
          (is (= [:evolution/evidence-frozen :evolution/diagnosis-created
                  :evolution/mutation-proposed :evolution/candidate-materialized
                  :evolution/mutation-proposed :evolution/candidate-materialized]
                 (mapv :event/type @events))
              "the evolution taxonomy events fire in cycle order")
          (is (= 2 (count (sqlite/query db ["SELECT * FROM candidates"]))))
          (is (= 2 (count (sqlite/query db ["SELECT * FROM mutations"]))))
          (is (= 2 (count (filter #(.isDirectory %)
                                  (or (.listFiles (io/file candidates-dir))
                                      (make-array java.io.File 0)))))
              "both candidate bundles are finalized under :candidates-dir")
          (is (= g1-id (:genome/id (load/load-genome (route-a-root))))
              "the current Genome directory is untouched by the cycle")
          (is (= 1 (:current (first (sqlite/query db
                                                  ["SELECT current FROM generations
                                                    WHERE id = ?" generation-id]))))
              "the CURRENT pointer still names G1 after the cycle")
          (is (= (:candidate/genome-id g2)
                 (store-genome-body!
                  cas-store
                  (load/load-genome (candidate-bundle-root
                                     candidates-dir (:candidate/genome-id g2)))))
              "the host stores G2's canonical body (Database Invariant 7)")
          (let [pack-artifact (read-artifact store (:evidence/id g2))]
            (is (= generation-id (:generation/id pack-artifact))
                "the frozen evidence pack resolves in the CAS"))
          (testing "G4 replay — G2 still passes the old A request; G5 paired
                    hidden Selection — G2 beats G1 above :min-delta with no
                    hard/cost regression"
            (let [a-output (read-artifact store (:output-ref (:result run-a)))
                  replay-case (replay-case-from-session store (:session/id run-a)
                                                        task-a a-output)
                  eval-a (eval-core/evaluate-candidate!
                          (build-evaluator store candidates-dir g2 replay-case)
                          (:candidate/id g2) :e2e/v1)
                  eval-c (eval-core/evaluate-candidate!
                          (build-evaluator store candidates-dir g2c replay-case)
                          (:candidate/id g2c) :e2e/v1)]
              (is (every? #(= :pass (:status %)) (:gates eval-a))
                  "all seven phases pass for the winning candidate")
              (is (true? (:eligible? (:eligibility eval-a))))
              (is (true? (:eligible? (:eligibility eval-c)))
                  "the stale-branch candidate is also eligible (it fixes B too)")
              (let [summary (:summary eval-a)]
                (is (= 0.5 (get-in summary [:utility :task/success :parent]))
                    "G1 wins only the A case; B fails")
                (is (= 1.0 (get-in summary [:utility :task/success :candidate]))
                    "G2 passes both A and B")
                (is (>= (- (get-in summary [:utility :task/success :candidate])
                           (get-in summary [:utility :task/success :parent]))
                        0.05)
                    "G2's task/success beats G1's above the profile :min-delta")
                (is (empty? (get-in summary [:hard :gates :violations])))
                (is (empty? (get-in summary [:hard :replay :violations])))
                (is (empty? (get-in summary [:hard :paired :violations])))
                (is (<= (get-in summary [:cost :cost/units :candidate])
                        (* 1.10 (get-in summary [:cost :cost/units :parent])))
                    "no cost regression beyond :max-cost-regression")
                (is (re-matches #"^sha256:[0-9a-f]{64}$"
                                (:paired-results-ref eval-a))
                    "the paired observations artifact is a durable content address"))
              (is (= 2 (count (sqlite/query db ["SELECT * FROM eval_runs"]))))
              (is (every? #(= "finalized" (:status %))
                          (sqlite/query db ["SELECT status FROM eval_runs"])))
              (testing "STEP 4 — the winner promotes; a second candidate from
                        G1 promoted after G2's success returns :stale
                        without corrupting the winning branch"
                (let [op-session-a (create-pinned-session! db g1-compiled generation-id)
                      op-session-c (create-pinned-session! db g1-compiled generation-id)
                      g2-resolution (:compiled/resolution-id
                                     (compiler/compile-genome
                                      (assoc (load/load-genome
                                              (candidate-bundle-root
                                               candidates-dir
                                               (:candidate/genome-id g2)))
                                             :programs [(route-descriptor)])
                                      (fixture-catalog)))
                      promote-result (promote/promote!
                                      {:store store
                                       :resolution/id g2-resolution
                                       :event/session-id op-session-a}
                                      {:candidate-id (:candidate/id g2)
                                       :evaluation-id (:evaluation/id eval-a)
                                       :expected-parent-generation generation-id})
                      g2-gen (:to promote-result)
                      stale-result (promote/promote!
                                    {:store store
                                     :resolution/id g2-resolution
                                     :event/session-id op-session-c}
                                    {:candidate-id (:candidate/id g2c)
                                     :evaluation-id (:evaluation/id eval-c)
                                     :expected-parent-generation generation-id})
                      ;; the new G2 session (created after promotion) —
                      ;; hoisted so STEP 5 can re-verify its pin after
                      ;; the store restart
                      g2-ctx (build-executor store
                                             (candidate-bundle-root
                                              candidates-dir
                                              (:candidate/genome-id g2)))
                      new-sid (create-pinned-session! db (:compiled g2-ctx)
                                                      g2-gen)
                      new-result (scheduler/run-session! (:executor g2-ctx)
                                                         new-sid task-b1)]
                  (testing "the promotion CAS changed CURRENT G1 → G2"
                    (is (= :promoted (:status promote-result)))
                    (is (= generation-id (:from promote-result)))
                    (is (string? (:to promote-result)))
                    (is (not= generation-id (:to promote-result)))
                    (is (= g2-gen (:id (current/current-generation db))))
                    (is (= "active" (:state (current/current-generation db))))
                    (is (= 1 (count (sqlite/query db
                                                  ["SELECT * FROM generations
                                                    WHERE current = 1"])))))
                  (testing "the stale branch returns :stale and changes nothing"
                    (is (= :stale (:status stale-result)))
                    (is (= g2-gen (:current stale-result)))
                    (is (= generation-id (:expected stale-result)))
                    (is (= g2-gen (:id (current/current-generation db)))
                        "CURRENT is untouched by the loser")
                    (is (= "stale" (:state (first (sqlite/query db
                                                                ["SELECT state FROM candidates
                                                                  WHERE id = ?"
                                                                 (str (:candidate/id g2c))]))))
                        "the CAS-loser candidate is marked :stale (Task 9.1)")
                    (is (= "promoted" (:state (first (sqlite/query db
                                                                   ["SELECT state FROM candidates
                                                                     WHERE id = ?"
                                                                    (str (:candidate/id g2))]))))
                        "the winning candidate is :promoted")
                    (let [promo-rows (sqlite/query db ["SELECT * FROM promotions"])]
                      (is (= 1 (count promo-rows))
                          "exactly the winning promotion has a promotions row")
                      (is (= "promoted" (:decision (first promo-rows))))
                      (is (= generation-id (:from_generation_id (first promo-rows))))
                      (is (= g2-gen (:to_generation_id (first promo-rows))))))
                  (testing "the promotion events are anchored and durable"
                    (is (= 1 (count (event/events-by-type db op-session-a
                                                          :promotion/promoted))))
                    (is (= 1 (count (event/events-by-type db op-session-c
                                                          :promotion/stale))))
                    (is (= g2-gen (get-in (first (event/events-by-type
                                                  db op-session-a
                                                  :promotion/promoted))
                                          [:metadata :to]))))
                  (testing "an already-running G1 session remains on G1; a new
                            session receives G2 (canary routing + a real G2 run)"
                    (let [g1-session (session/get-session db (:session/id run-a))]
                      (is (= :completed (:state g1-session)))
                      (is (= generation-id (:generation/id g1-session)))
                      (is (= g1-id (:genome/id g1-session)))
                      (is (= (:session/id run-a) (:session/id g1-session)))
                      (is (= :failed (:state (session/get-session db
                                                                  (:session/id run-b1))))
                          "the failed G1 session stays pinned too"))
                    (let [ds {:current-generation g2-gen
                              :canary {:generation g2-gen :allocation 1.0
                                       :ladder [0.10 0.25 0.50 1.0]
                                       :version "v1"}
                              :active? true}]
                      (is (= g2-gen (canary/select-generation-for-new-session
                                     ds "new-session-key-1"))
                          "an active 100% canary routes new sessions to G2")
                      (is (= g2-gen (canary/select-generation-for-new-session
                                     {:current-generation g2-gen :active? false}
                                     "new-session-key-2"))
                          "without a canary, CURRENT (G2) is the fallback"))
                    (let [new-session (session/get-session db new-sid)]
                      (is (= :completed (:status new-result))
                          "the new G2 session serves the class-B request")
                      (is (= g2-gen (:generation/id new-session)))
                      (is (= (:candidate/genome-id g2) (:genome/id new-session)))
                      (is (= {:tool/id :fixture/echo-b}
                             (-> (read-artifact store (:output-ref new-result))
                                 first :action :payload (select-keys [:tool/id])))
                          "G2 routes class B to tool B in the live run")))
                  (testing "the lineage query explains G1 + evidence + Δ +
                            evaluation → G2"
                    (let [root-lineage (lineage/lineage store generation-id)
                          child (first (:children root-lineage))]
                      (is (= generation-id (get-in root-lineage
                                                   [:generation :generation/id])))
                      (is (= g1-id (get-in root-lineage [:generation :genome/id])))
                      (is (nil? (get-in root-lineage [:mutation]))
                          "the seed generation has no mutation")
                      (is (= 1 (count (:children root-lineage)))
                          "exactly one promoted child (the stale branch records
                          no promotion row)")
                      (is (= g2-gen (get-in child [:generation :generation/id])))
                      (is (= (:candidate/genome-id g2)
                             (get-in child [:generation :genome/id])))
                      (is (= :promoted (get-in child [:promotion :decision])))
                      (is (= (:evidence/id g2)
                             (get-in child [:evidence :evidence/id]))
                          "the child carries the frozen evidence reference")
                      (is (= (:mutation/id g2)
                             (get-in child [:mutation :mutation/id]))
                          "the child carries the exact Δ mutation")
                      (is (= :program (get-in child [:mutation :risk])))
                      (is (= "programs/route.clj"
                             (get-in child [:mutation :ops 0 :file]))
                          "the Δ targets the SCI router")
                      (is (= :fixture/echo-b
                             (second (:action (nth (get-in child
                                                          [:mutation :ops 0 :form])
                                                   5))))
                          "the Δ switches class B to tool B")
                      (is (= (:evaluation/id eval-a)
                             (get-in child [:evaluation :evaluation/id]))
                          "the child carries the finalized evaluation")
                      (is (true? (get-in child [:evaluation :eligibility :eligible?])))
                      (is (= generation-id (get-in child [:promotion :from-generation])))
                      (is (= g2-gen (get-in child [:promotion :to-generation])))))
                  (testing "STEP 5 — close and REOPEN the store from disk;
                            lineage/current checks re-run cleanly"
                    (let [reopened-db (sqlite/spec (:db-path fx))
                          _ (is (= {:status :noop :version 2}
                                   (migrate/migrate! reopened-db)))
                          reopened-store {:sqlite reopened-db
                                          :cas (cas/->cas (:cas-root fx))}]
                      (is (= g2-gen (:id (current/current-generation reopened-db)))
                          "CURRENT survives the restart")
                      (let [reopened-lineage (lineage/lineage reopened-store
                                                              generation-id)
                            reopened-child (first (:children reopened-lineage))]
                        (is (= g2-gen (get-in reopened-child
                                              [:generation :generation/id])))
                        (is (= (:mutation/id g2)
                               (get-in reopened-child [:mutation :mutation/id])))
                        (is (= (:evaluation/id eval-a)
                               (get-in reopened-child [:evaluation :evaluation/id])))
                        (is (= :promoted
                               (get-in reopened-child [:promotion :decision]))))
                      (let [g2-lineage (lineage/lineage reopened-store g2-gen)]
                        (is (= g2-gen (get-in g2-lineage [:generation :generation/id])))
                        (is (= generation-id (get-in g2-lineage [:parent :generation/id])))
                        (is (empty? (:children g2-lineage))))
                      (is (= :completed
                             (:state (session/get-session reopened-db
                                                          (:session/id run-a))))
                          "the G1 session pin survives the restart")
                      (is (= g2-gen
                             (:generation/id (session/get-session reopened-db
                                                                  new-sid)))
                          "the G2 session pin survives the restart")
                      (is (:valid? (event/verify-event-chain reopened-db
                                                             (:session/id run-a)))
                          "the append-only chain re-verifies after the restart"))))))))))))

;; ============================================================================
;; the fixture route programs' decision tables (deterministic contracts)
;; ============================================================================

(deftest route-program-contracts
  (let [invoke (fn [bundle-root input]
                 (let [{:keys [loaded compiled]} (compile-bundle bundle-root)
                       runtime (:sci-runtime
                                (phenotype/instantiate
                                 compiled
                                 {:stores {:sqlite :poison :cas {:root :poison}}
                                  :providers {:registry (registry/create-registry)}
                                  :capabilities {:leases [] :usage (atom {})}
                                  :program-sources (program-sources loaded compiled)}))]
                   (select-keys (execute/invoke! runtime :program/route input nil)
                                [:status :value])))]
    (testing "G1 (route-a): tool A for every request; class B fails"
      (is (= {:status :ok
              :value {:action {:intent/type :intent/tool-call
                               :payload {:tool/id :fixture/echo
                                         :args {:text "hi"}}}}}
             (invoke (route-a-root) {:op :echo-a :text "hi"})))
      (is (= :error (:status (invoke (route-a-root) {:op :echo-b :text "bo"})))
          "class-B requests fail under the A-for-everything router"))
    (testing "the reference G2 program (route-b): A → tool A, B → tool B"
      (is (= {:status :ok
              :value {:action {:intent/type :intent/tool-call
                               :payload {:tool/id :fixture/echo
                                         :args {:text "hi"}}}}}
             (invoke (route-b-root) {:op :echo-a :text "hi"})))
      (is (= {:status :ok
              :value {:action {:intent/type :intent/tool-call
                               :payload {:tool/id :fixture/echo-b
                                         :args {:text "bo"}}}}}
             (invoke (route-b-root) {:op :echo-b :text "bo"}))))))

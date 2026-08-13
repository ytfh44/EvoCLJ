(ns evoclj.evolution.core
  "Orchestrate one evolution proposal cycle (Task 7.8, Milestone 7).

  propose-candidates! runs the normative phase order over the REAL
  pipeline — only the Diagnostician (Task 7.2 protocol) and the
  Mutator (protocol defined below) are adapters:

      freeze evidence      → evoclj.evolution.evidence/build-evidence-pack
      diagnose             → Diagnostician + diagnose/persist-diagnosis!
      load negative history→ evoclj.evolution.history/recent-mutation-history
      propose mutation     → Mutator (protocol below)
      validate risk/budget → mutation/validate-mutation +
                             budget/check-budget (v0 profile, Task 7.5)
      apply patch          → evoclj.genome.patch/apply-mutation (Task 7.4)
      compile candidate    → evoclj.compiler.core/compile-genome (Task 2.4)
      persist Candidate    → evoclj.evolution.candidate (Task 7.6)

  Per-cycle and per-candidate semantics:

  - THE EVOLUTION-SYSTEM MAP (documented contract). The system map
    carries everything the cycle needs:

        {:store            {:sqlite <db> :cas <CAS root>}  ; executor :stores
         :provider-catalog {alias -> provider-entry}        ; Task 2.1 Resolution
         :genome-loader    (fn [] -> loaded Genome map)     ; exactly one of
         :genome-root      <path|string>                    ;   these two
         :candidates-dir   <path|string>  ; Task 7.4 output dir — the
                                          ;   staging area; finalized
                                          ;   candidate bundles land here
         :diagnostician    <Diagnostician impl>   ; Task 7.2 protocol
         :mutator          <Mutator impl>         ; protocol below
         :budget-profile   map?          ; default budget/v0-profile
         :programs-registry sequential?  ; Task 2.3 descriptors, used
                                         ; only when the parent Genome
                                         ; carries no :programs
         :event-sink       (fn [event])  ; taxonomy events, default no-op
         :phase-hook       (fn [phase])  ; TEST SEAM: called once per
                                         ; phase with its keyword, in
                                         ; cycle order; default nil}

    The cycle request is the task's normative interface plus one
    optional override:

        {:generation/id     \"generation-1\"
         :evidence-selector {:recent n :include-successes n
                             :include-failures n :include-high-cost n
                             :seed (optional)}
         :max-candidates    pos-int?      ; default 3, capped at 3 (v0)
         :cutoff-event-id   pos-int?      ; optional immutable evidence
                                          ; boundary; default = the
                                          ; newest event id appended by
                                          ; any session pinned to the
                                          ; generation}

  - THE MUTATOR CONTRACT (defined here, normative for this task):
    (propose-mutations mutator context) returns a FINITE vector of
    Mutation IR maps, or nil when nothing to propose. The context is
    the exact input the Mutator is allowed to see:

        {:generation/id      \"generation-1\"
         :parent/genome-id   \"sha256:...\"    ; the loaded parent G1
         :parent-genome      <loaded parent Genome map> ; for choosing
                                        ; targets and :expect/hash digests
         :diagnosis          <validated Diagnosis>      ; Task 7.2
         :history            <Task 7.7 history entries> ; negative evidence
         :budget-profile     <profile map>}

    The Mutator must return mutations whose :ops stay inside the Task
    7.3 op language; the orchestrator completes any MISSING lineage
    fields before validation: :mutation/id (fresh uuid),
    :parent/genome-id (the loaded parent), :evidence/id (the frozen
    pack), and :hypothesis/id (the diagnosis's first hypothesis). A
    returned value that is not nil/sequential/map-shaped fails the
    cycle with :evolution/mutator-invalid.

  - FAILURE ISOLATION (Global Constraints 22, 23, 24; Step 3). The
    current generation is never written: the cycle only READS the
    current Genome bundle (through :genome-loader/:genome-root) and
    never touches the generations CURRENT pointer — candidate bundles
    are finalized into :candidates-dir and candidate rows are written
    by evoclj.evolution.candidate, which has no activation rights.
    A per-candidate failure (validation, budget, patch, compile, or
    materialize) NEVER aborts the cycle and NEVER affects the current
    Genome directory: the mutation is skipped, an
    :evolution/candidate-invalid event is appended with the typed
    :reason (:invalid-mutation :budget-exceeded :patch-failed
    :compile-failed :materialize-failed) and the :error/type, and a
    bundle already finalized into :candidates-dir is removed
    (best-effort) so no partial candidate survives. System/request
    contract errors (:evolution/system-invalid, :evolution/request-
    invalid, :evolution/generation-not-found, :evolution/lineage-
    invalid, :evolution/genome-mismatch, :evolution/evidence-mismatch,
    :evolution/mutator-invalid) fail loudly — they are caller bugs.

  - THE V0 CAP (Step 4): at most THREE mutations are adopted per
    cycle. The request's :max-candidates is honored only below the
    hard v0 ceiling of three (a request for ten still materializes
    three).

  - EVENTS (Event Taxonomy). The cycle appends exactly the evolution
    taxonomy events through the :event-sink:
    :evolution/evidence-frozen, :evolution/diagnosis-created,
    :evolution/mutation-proposed (per adopted mutation),
    :evolution/candidate-materialized (per persisted candidate), and
    :evolution/candidate-invalid (per skipped mutation). The sink is a
    no-op by default: wiring a durable sink (e.g. an operator session
    in the store's append-only event log) is the host's job — the
    orchestrator itself owns no session. Global Constraint 22 is
    preserved: every event payload is plain EDN-safe Clojure data.

  - THE CANDIDATE (Step 5 persistence). Each adopted mutation is
    materialized through evoclj.evolution.candidate (the uniqueness
    rule dedupes same parent + same mutation content to one auditable
    row) and immediately transitioned to :evaluation-pending — the
    terminal state of Milestone 7. There is NO promotion mechanism
    here: this namespace never reads or writes the CURRENT pointer and
    never depends on a promotion namespace (Global Constraint 15 keeps
    activation in M9). The candidate Genome BODY is the finalized
    bundle directory under :candidates-dir (content-addressed by its
    :genome/id — Task 7.4 already made the finalize atomic and
    deterministic); the orchestrator stores no additional CAS copy in
    v0 (YAGNI, Global Constraint 24)."
  (:require [clojure.string :as str]
            [evoclj.compiler.core :as compiler]
            [evoclj.evolution.budget :as budget]
            [evoclj.evolution.candidate :as candidate]
            [evoclj.evolution.diagnose :as diagnose]
            [evoclj.evolution.evidence :as evidence]
            [evoclj.evolution.evidence-schema :as es]
            [evoclj.evolution.history :as history]
            [evoclj.evolution.mutation :as mutation]
            [evoclj.genome.load :as load]
            [evoclj.genome.patch :as patch]
            [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite]
            [malli.core :as m]
            [malli.error :as me])
  (:import (java.nio.file FileVisitOption Files LinkOption Path Paths)
           (java.util UUID)))

;; --- the Mutator contract (normative for this task) ---------------------------

(defprotocol Mutator
  "The Mutator contract (Task 7.8). An adapter implements exactly one
  method:

      (propose-mutations mutator context)

  `context` is the full, closed input the Mutator is allowed to see:
  the generation and its loaded parent Genome, the validated
  Diagnosis, the Task 7.7 negative-history entries, and the budget
  profile. It returns a FINITE vector of Mutation IR maps (Task 7.3)
  to propose this cycle, or nil when nothing to propose. The result
  must be finite and data-only (Global Constraint 22); the
  orchestrator completes missing lineage fields (:mutation/id,
  :parent/genome-id, :evidence/id, :hypothesis/id) and validates every
  proposal against the parent and the budget profile before anything
  is staged."
  (propose-mutations [mutator context]
    "Return a finite vector of Mutation IR maps for this cycle, or nil
     when nothing to propose."))

;; --- the v0 cap and the normative phase order ---------------------------------

(def v0-max-candidates
  "The hard v0 ceiling: at most three candidates per cycle (Task 7.8
  Step 4). A request's :max-candidates is honored only below this."
  3)

(def cycle-phases
  "The normative phase order of one cycle (Task 7.8 Step 2), reported
  through the :phase-hook test seam in this exact order."
  [:freeze-evidence :diagnose :load-history :propose-mutation
   :validate-risk-budget :apply-patch :compile-candidate
   :persist-candidate])

(def ^:private default-history-limit
  "The recent-mutation-history window the cycle loads (Task 7.7
  default)."
  50)

;; --- contract validation -------------------------------------------------------

(def CycleRequestSchema
  "The propose-candidates! request contract (closed). :generation/id
  names the current generation; :evidence-selector is the Task 7.1
  selection quota map; :max-candidates is optional and v0-capped at
  three; :cutoff-event-id is an optional immutable evidence boundary
  (default: the newest event id of the generation's sessions)."
  [:map {:closed true}
   [:generation/id string?]
   [:evidence-selector es/SelectorSchema]
   [:max-candidates {:optional true} pos-int?]
   [:cutoff-event-id {:optional true} pos-int?]])

(defn- schema-error!
  [error-type kind expl]
  (throw (err/error error-type
                    (str kind " does not satisfy the evolution-system contract")
                    {:errors (me/humanize expl)})))

(defn- validate-request!
  [request]
  (when-let [expl (m/explain CycleRequestSchema request)]
    (schema-error! :evolution/request-invalid "cycle request" expl))
  request)

(defn- system-error!
  [reason message data]
  (throw (err/error :evolution/system-invalid message (assoc data :reason reason))))

(defn- validate-store!
  [store]
  (when-not (map? store)
    (system-error! :store-invalid
                   "store must be the executor :stores map {:sqlite ... :cas ...}"
                   {:value (err/sanitize store)}))
  (when-not (contains? store :sqlite)
    (system-error! :sqlite-missing "store must carry the :sqlite handle" {}))
  (when-not (contains? store :cas)
    (system-error! :cas-missing "store must carry the :cas handle" {}))
  store)

(defn- validate-system!
  "Validate the evolution-system map (see the namespace docstring for
  the closed contract). Typed error :evolution/system-invalid with
  :reason distinguishing the violation."
  [system]
  (when-not (map? system)
    (system-error! :not-a-map "evolution-system must be a map"
                   {:value (err/sanitize system)}))
  (doseq [k [:store :provider-catalog :diagnostician :mutator
             :candidates-dir]]
    (when-not (contains? system k)
      (system-error! (keyword (name k) "missing")
                     (str "evolution-system is missing required key " k)
                     {})))
  (validate-store! (:store system))
  (when-not (map? (:provider-catalog system))
    (system-error! :provider-catalog-invalid
                   ":provider-catalog must be a map (alias -> provider entry)"
                   {:value (err/sanitize (:provider-catalog system))}))
  (when-not (satisfies? diagnose/Diagnostician (:diagnostician system))
    (system-error! :diagnostician-invalid
                   ":diagnostician must implement the Task 7.2 Diagnostician protocol"
                   {:value (err/sanitize (:diagnostician system))}))
  (when-not (satisfies? Mutator (:mutator system))
    (system-error! :mutator-invalid
                   ":mutator must implement the Mutator protocol"
                   {:value (err/sanitize (:mutator system))}))
  (let [has-loader (contains? system :genome-loader)
        has-root (contains? system :genome-root)]
    (when (and has-loader has-root)
      (system-error! :ambiguous-genome-source
                     "supply exactly one of :genome-loader or :genome-root" {}))
    (when-not (or has-loader has-root)
      (system-error! :genome-source-missing
                     "supply exactly one of :genome-loader or :genome-root" {})))
  (let [candidates-dir (:candidates-dir system)]
    (when-not (or (string? candidates-dir)
                  (instance? Path candidates-dir))
      (system-error! :candidates-dir-invalid
                     ":candidates-dir must be a path or string"
                     {:value (err/sanitize candidates-dir)})))
  (when-let [profile (:budget-profile system)]
    (when-not (map? profile)
      (system-error! :budget-profile-invalid
                     ":budget-profile must be a map"
                     {:value (err/sanitize profile)})))
  (when-let [registry (:programs-registry system)]
    (when-not (sequential? registry)
      (system-error! :programs-registry-invalid
                     ":programs-registry must be a sequential collection of descriptors"
                     {:value (err/sanitize registry)})))
  (when-let [sink (:event-sink system)]
    (when-not (ifn? sink)
      (system-error! :event-sink-invalid
                     ":event-sink must be a function (fn [event])"
                     {:value (err/sanitize sink)})))
  (when-let [hook (:phase-hook system)]
    (when-not (ifn? hook)
      (system-error! :phase-hook-invalid
                     ":phase-hook must be a function (fn [phase]) or nil"
                     {:value (err/sanitize hook)})))
  system)

;; --- store glue: lineage, evidence boundary, parent integrity -----------------

(defn- generation-row
  [store generation-id]
  (first (sqlite/query (:sqlite store)
                       ["SELECT id, genome_id, parent_id FROM generations
                         WHERE id = ?"
                        generation-id])))

(defn- generation-lineage
  "The generation lineage from the current generation back to the
  root, derived by walking parent links (the shape Task 7.7's
  recent-mutation-history expects). A missing current row or a
  dangling parent link fails loudly (:evolution/generation-not-found,
  :evolution/lineage-invalid)."
  [store generation-id]
  (when-not (generation-row store generation-id)
    (throw (err/error :evolution/generation-not-found
                      "the cycle's generation is not in the store"
                      {:generation/id generation-id})))
  (loop [id generation-id seen #{} acc []]
    (cond
      (contains? seen id)
      (throw (err/error :evolution/lineage-invalid
                        "cycle in the generation lineage"
                        {:generation/id id}))

      (nil? id) (vec acc)

      :else
      (let [row (first (sqlite/query (:sqlite store)
                                     ["SELECT id, parent_id FROM generations
                                       WHERE id = ?"
                                      id]))]
        (when-not row
          (throw (err/error :evolution/lineage-invalid
                            "generation lineage contains a dangling parent link"
                            {:generation/id id})))
        (recur (:parent_id row) (conj seen id) (conj acc id))))))

(defn- generation-cutoff
  "The immutable evidence boundary for the cycle: the newest event id
  appended by any session pinned to the generation (the join keeps the
  bound generation-local), or 0 when the generation has no events yet."
  [store generation-id]
  (or (-> (first (sqlite/query (:sqlite store)
                               ["SELECT COALESCE(MAX(e.id), 0) AS max_id
                                 FROM events e
                                 JOIN sessions s ON s.id = e.session_id
                                 WHERE s.generation_id = ?"
                                generation-id]))
          :max_id)
      0))

(defn- load-parent-genome
  "Load the current generation's Genome through the system's
  :genome-loader fn or :genome-root path, and verify its content
  address against the stored generation row — a loader that yields a
  different Genome than the generation record claims is a broken
  system map (:evolution/genome-mismatch)."
  [system generation-id row]
  (let [loaded (if (contains? system :genome-loader)
                 ((:genome-loader system))
                 (load/load-genome (:genome-root system)))]
    (when-not (= (:genome/id loaded) (:genome_id row))
      (throw (err/error :evolution/genome-mismatch
                        "the loaded parent Genome does not match the generation record"
                        {:generation/id generation-id
                         :generation/genome-id (:genome_id row)
                         :loaded/genome-id (:genome/id loaded)})))
    loaded))

;; --- event and phase plumbing --------------------------------------------------

(defn- emit-event!
  "Append one evolution taxonomy event through the system's :event-sink
  (a no-op when no sink is configured). The event is plain EDN-safe
  data: {:event/type <:evolution/*> :metadata <map>}."
  [system event]
  (when-let [sink (:event-sink system)]
    (sink event)))

(defn- phase!
  "Report one phase through the :phase-hook test seam (a no-op when no
  hook is configured). Called once per phase, in cycle order, before
  the phase's work."
  [system phase]
  (when-let [hook (:phase-hook system)]
    (hook phase)))

;; --- per-candidate pipeline -----------------------------------------------------

(defn- complete-mutation!
  "Complete a Mutator-returned Mutation IR with the lineage fields the
  adapter may omit: :parent/genome-id (the loaded parent), :evidence/id
  (the frozen pack), :mutation/id (a fresh uuid), and :hypothesis/id
  (the diagnosis's first hypothesis). An adapter value that is not a
  map, or a mutation with no referencable hypothesis, fails the cycle
  with :evolution/mutator-invalid — the adapter violated its contract."
  [parent pack diagnosis mutation]
  (when-not (map? mutation)
    (throw (err/error :evolution/mutator-invalid
                      "the Mutator returned a non-map proposal"
                      {:value (err/sanitize mutation)})))
  (let [hypothesis-id (or (:hypothesis/id mutation)
                          (some-> (:hypotheses diagnosis) first :hypothesis/id))]
    (when-not (uuid? hypothesis-id)
      (throw (err/error :evolution/mutator-invalid
                        "a mutation needs a :hypothesis/id (own or the diagnosis's first)"
                        {:mutation (err/sanitize (dissoc mutation :ops))})))
    (merge {:parent/genome-id (:genome/id parent)
            :evidence/id (:evidence/id pack)
            :mutation/id (UUID/randomUUID)}
           mutation
           (when (nil? (:hypothesis/id mutation))
             {:hypothesis/id hypothesis-id}))))

(defn- attach-programs
  "Attach the program-descriptor registry to the candidate loaded
  Genome before compilation (Task 2.3 choice (a)): the parent's own
  :programs when present (non-empty), else the system's
  :programs-registry. compile-genome reads the registry from the
  loaded value, so the candidate must carry it explicitly."
  [candidate-genome system parent]
  (let [registry (if (seq (:programs parent))
                   (:programs parent)
                   (:programs-registry system))]
    (assoc candidate-genome :programs (or registry []))))

(defn- candidate-dir
  "The finalized bundle directory of a candidate genome under the
  system's :candidates-dir (the same name rule as Task 7.4 finalize:
  the content address with ':' replaced so it is legal on every host)."
  [system genome-id]
  (.resolve (Path/of (str (:candidates-dir system)) (make-array String 0))
            (str/replace genome-id ":" "-")))

(defn- delete-candidate-dir!
  "Best-effort removal of a finalized candidate bundle that failed
  compilation or materialization, so :candidates-dir never holds a
  bundle that has no persisted candidate row (no partial candidate).
  Failures to delete are ignored — the :candidate-invalid event is the
  authoritative signal."
  [system genome-id]
  (let [dir (candidate-dir system genome-id)]
    (try
      (when (Files/exists dir (make-array LinkOption 0))
        (with-open [stream (Files/walk dir (make-array FileVisitOption 0))]
          (doseq [p (reverse (iterator-seq (.iterator stream)))]
            (Files/deleteIfExists p))))
      (catch Throwable _ nil))))

(defn- error-type
  "The :error/type of a thrown value when it is a typed ExceptionInfo,
  else nil."
  [t]
  (:error/type (ex-data t)))

(defn- failure-reason
  "The :candidate-invalid :reason for a thrown per-candidate error,
  classified by the :error/type keyword's namespace (name alone would
  strip it: (name :patch/preimage-mismatch) is preimage-mismatch)."
  [t]
  (let [et (error-type t)
        ns (some-> et namespace)]
    (cond
      (contains? #{:evolution/budget-exceeded :evolution/risk-not-enabled
                   :evolution/under-declared-risk} et) :budget-exceeded
      (= ns "mutation") :invalid-mutation
      (contains? #{"patch" "genome"} ns) :patch-failed
      (= ns "compiler") :compile-failed
      (= ns "candidate") :materialize-failed
      :else :unknown)))

(defn- mutation-proposed-event
  [mutation]
  {:event/type :evolution/mutation-proposed
   :metadata {:mutation/id (:mutation/id mutation)
              :risk (:risk mutation)}})

(defn- candidate-invalid-event
  [mutation reason t]
  {:event/type :evolution/candidate-invalid
   :metadata {:mutation/id (:mutation/id mutation)
              :reason reason
              :error/type (error-type t)
              :message (some-> t .getMessage)}})

(defn- candidate-materialized-event
  [record]
  {:event/type :evolution/candidate-materialized
   :metadata {:candidate/id (:candidate/id record)
              :candidate/genome-id (:candidate/genome-id record)
              :mutation/id (:mutation/id record)
              :evidence/id (:evidence/id record)}})

(defn- materialize-one!
  "Run the per-candidate pipeline for one adopted mutation:
  validate risk/budget → apply patch → compile candidate → persist
  Candidate. Returns {:candidate <persisted record>} on success, or
  nil when the mutation was skipped. A failure never propagates: the
  mutation is recorded as :evolution/candidate-invalid and the cycle
  continues (Step 3 isolation — the current Genome directory and the
  CURRENT pointer are never touched; a bundle already finalized into
  :candidates-dir is removed)."
  [system parent pack diagnosis generation-id budget-profile mutation]
  (let [mutation (complete-mutation! parent pack diagnosis mutation)]
    (try
      (phase! system :validate-risk-budget)
      ;; schema + path + protected-path + mutable-class gates, then the
      ;; hard budget gates (Task 7.5) — everything before any staging
      (mutation/validate-mutation mutation parent)
      (budget/check-budget mutation budget-profile)
      (emit-event! system (mutation-proposed-event mutation))

      (phase! system :apply-patch)
      (let [candidate-genome (patch/apply-mutation parent mutation
                                                   (:candidates-dir system))]
        (try
          (phase! system :compile-candidate)
          (let [compiled (compiler/compile-genome
                          (attach-programs candidate-genome system parent)
                          (:provider-catalog system))]
            (phase! system :persist-candidate)
            (let [proposed (candidate/create-candidate
                            {:parent/generation-id generation-id
                             :parent/genome-id (:genome/id parent)
                             :candidate/genome-id (:compiled/genome-id compiled)
                             :mutation/id (:mutation/id mutation)
                             :evidence/id (:evidence/id mutation)
                             :risk (:risk mutation)})
                  materialized (candidate/materialize-candidate!
                                (:store system) proposed mutation)
                  pending (if (= :materialized (:state materialized))
                            (candidate/mark-evaluation-pending!
                             (:store system) (:candidate/id materialized))
                            ;; dedupe: an earlier cycle already moved
                            ;; this candidate to :evaluation-pending
                            materialized)]
              (emit-event! system (candidate-materialized-event pending))
              {:candidate pending}))
          (catch Throwable t
            (delete-candidate-dir! system (:genome/id candidate-genome))
            (emit-event! system (candidate-invalid-event mutation
                                                         :compile-failed t))
            nil)))
      (catch Throwable t
        (emit-event! system (candidate-invalid-event mutation
                                                     (failure-reason t) t))
        nil))))

;; --- the public entry point -----------------------------------------------------

(defn propose-candidates!
  "Orchestrate one evolution proposal cycle (Task 7.8) and return the
  persisted Candidate records — a vector of at most three
  :evaluation-pending records in mutation order.

  Phase order (normative, Step 2): freeze evidence → diagnose → load
  negative history → propose mutation → validate risk/budget → apply
  patch → compile candidate → persist Candidate. Only the Diagnostician
  and the Mutator are adapters; everything else is the REAL pipeline
  (see the namespace docstring for the evolution-system map contract,
  the Mutator contract, the failure-isolation semantics of Step 3, and
  the v0 three-candidate cap of Step 4).

  Typed errors (caller bugs, fail loudly): :evolution/system-invalid,
  :evolution/request-invalid, :evolution/generation-not-found,
  :evolution/lineage-invalid, :evolution/genome-mismatch,
  :evolution/evidence-mismatch, :evolution/mutator-invalid. Per-
  candidate failures are skipped and reported as
  :evolution/candidate-invalid events, never thrown."
  [system request]
  (validate-system! system)
  (validate-request! request)
  (let [store (:store system)
        generation-id (:generation/id request)
        budget-profile (or (:budget-profile system) budget/v0-profile)
        ;; --- 1. freeze evidence
        _ (phase! system :freeze-evidence)
        row (generation-row store generation-id)
        _ (when-not row
            (throw (err/error :evolution/generation-not-found
                              "the cycle's generation is not in the store"
                              {:generation/id generation-id})))
        cutoff (or (:cutoff-event-id request)
                   ;; the evidence boundary must be a positive int; with
                   ;; no events the eligible set is empty either way
                   (max 1 (generation-cutoff store generation-id)))
        pack (evidence/build-evidence-pack
              store {:generation/id generation-id
                     :cutoff-event-id cutoff
                     :selector (:evidence-selector request)})
        _ (emit-event! system
                       {:event/type :evolution/evidence-frozen
                        :metadata {:evidence/id (:evidence/id pack)
                                   :generation/id generation-id
                                   :cutoff-event-id cutoff
                                   :selected (get-in pack [:summary :selected])}})
        ;; --- 2. diagnose
        _ (phase! system :diagnose)
        diagnosis (diagnose/diagnose (:diagnostician system) pack)
        _ (when-not (= (:evidence/id diagnosis) (:evidence/id pack))
            (throw (err/error :evolution/evidence-mismatch
                              "the diagnosis answers a different evidence pack than the one frozen this cycle"
                              {:diagnosis/evidence-id (:evidence/id diagnosis)
                               :pack/evidence-id (:evidence/id pack)})))
        _ (diagnose/persist-diagnosis! store diagnosis)
        _ (emit-event! system
                       {:event/type :evolution/diagnosis-created
                        :metadata {:diagnosis/id (:diagnosis/id diagnosis)
                                   :evidence/id (:evidence/id diagnosis)
                                   :hypotheses (count (:hypotheses diagnosis))}})
        ;; --- 3. load negative history
        _ (phase! system :load-history)
        lineage (generation-lineage store generation-id)
        history-entries (history/recent-mutation-history
                         store lineage {:limit default-history-limit})
        ;; --- 4. propose mutation (parent loaded for the adapter + patches)
        _ (phase! system :propose-mutation)
        parent (load-parent-genome system generation-id row)
        context {:generation/id generation-id
                 :parent/genome-id (:genome/id parent)
                 :parent-genome parent
                 :diagnosis diagnosis
                 :history history-entries
                 :budget-profile budget-profile}
        proposed (propose-mutations (:mutator system) context)
        _ (when-not (or (nil? proposed) (sequential? proposed))
            (throw (err/error :evolution/mutator-invalid
                              "the Mutator must return a finite vector of mutations or nil"
                              {:value (err/sanitize proposed)})))
        adopted (take (min (or (:max-candidates request)
                               v0-max-candidates)
                           v0-max-candidates)
                      (vec proposed))
        ;; --- 5-8. per adopted mutation: validate → apply → compile → persist
        results (mapv (fn [mutation]
                        (materialize-one! system parent pack diagnosis
                                          generation-id budget-profile mutation))
                      adopted)]
    (vec (keep :candidate results))))

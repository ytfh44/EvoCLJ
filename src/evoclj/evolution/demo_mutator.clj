(ns evoclj.evolution.demo-mutator
  "The built-in heuristic Mutator adapter for the :demo profile (Task
  D1).

  This adapter conforms to the Mutator protocol of
  evoclj.evolution.core and mirrors the LLM adapter's shape — the
  difference is that it is NON-LLM: a deterministic-heuristic
  template/function-swap mutator over the seed genome's OWN mutable
  program file (programs/route.clj). It consumes the same closed
  per-cycle context the LLM adapter sees (the generation id, the
  loaded parent Genome, the validated Diagnosis, the negative-history
  entries, and the budget profile) and returns a finite vector of
  Mutation IR maps, or nil when nothing is proposed.

  THE TEMPLATE-SWAP MECHANISM: the seed route program's routing
  decision is one top-level `case` form (route one task input map to
  a typed Intent decision). The demo mutator swaps that `case` for
  template variants drawn from a fixed, deterministic catalog:

      :routing/echo-b    — the reference improvement: adds an
                           :echo-b branch routing to the
                           :fixture/echo-b tool (the deterministic
                           full-cycle fixture), so the candidate wins
                           the demo's hidden :echo-b selection case
                           the seed parent cannot pass.
      :fallback/ok       — a semantically-neutral template swap of
                           the fallback branch (finish carries \"ok\").
      :fallback/finished — same, with \"finished\".

  Every variant keeps the decision contract for the seed's :echo and
  :finish inputs, is allowlist-clean (only forms the seed already
  uses), and touches ONLY the mutable programs/ route program — never
  topology.edn, so a candidate always passes the compiler topology
  validation (compile-genome) that the orchestrator runs per
  candidate. All three are proposed every cycle in catalog order; the
  :routing/echo-b variant is the one the demo's hidden selection
  cases promote.

  THE KERNEL-COMPUTES-HASH RULE (the same security property as the
  LLM adapter): every op carries :expect/hash — the \"sha256:<64
  hex>\" preimage digest of the op's target file's CURRENT content,
  taken from the parent genome's :files digest. The demo mutator
  computes and attaches it itself (there is no model to name a
  preimage), so a stale patch is impossible.

  SELF-SUFFICIENT PROVENANCE: a fresh demo state dir has no evidence
  yet, so the pattern Diagnostician may produce a diagnosis with ZERO
  hypotheses. The demo mutator is heuristic and proposes regardless,
  supplying its own deterministic :hypothesis/id when the diagnosis
  carries none (the orchestrator's complete-mutation! accepts either
  — the adapter's own id wins, the diagnosis's first hypothesis is
  preferred for provenance when present).

  DETERMINISM (Global Constraint 6): the proposed ops are a pure
  function of the parent genome bytes — identical context always
  yields the identical Mutation IR vector, and identical parent bytes
  plus the same mutation value yield the same applied candidate hash.

  The demo mutator holds NO store handle and calls NO provider
  directly (Global Constraints 8, 11): propose-mutations is pure over
  its context. It is injected by the CLI host (evoclj.cli.session)
  under the :demo profile through the SAME :overrides seam any host
  uses, together with the demo's hidden selection cases and fixture
  providers (demo-selection-cases / demo-selection-fixtures).

  Error contract (Global Constraint 22 — plain serializable data):
    :mutation/context-invalid — the handed context is not the closed
                                Mutator context map."
  (:require [evoclj.evolution.core :refer [Mutator]]
            [evoclj.kernel.error :as err]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto])
  (:import (java.nio.charset StandardCharsets)))

;; --- the deterministic template/function-swap library --------------------------

(defn- tool-call-form
  "The route decision form for one tool-call intent: the same shape the
  seed's :echo branch uses (allowlist-clean by construction)."
  [tool-id]
  (list 'tool-call-intent tool-id {:text (list 'get 'input :text)}))

(defn- seed-case-form
  "The seed route's routing `case` (the exact decision contract of
  genomes/seed/programs/route.clj), kept in the library so every
  variant below is a bounded swap of the SAME form."
  []
  (list 'case 'op
        :echo {:action (tool-call-form :fixture/echo)}
        :finish {:action (list 'finish-intent (list 'get 'input :value))}
        {:action (list 'finish-intent 'input)}))

(defn- echo-b-case-form
  "The :routing/echo-b template variant: the seed's `case` plus a new
  :echo-b branch routing to the :fixture/echo-b tool (the reference
  improvement the demo's hidden selection cases promote)."
  []
  (list 'case 'op
        :echo {:action (tool-call-form :fixture/echo)}
        :finish {:action (list 'finish-intent (list 'get 'input :value))}
        :echo-b {:action (tool-call-form :fixture/echo-b)}
        {:action (list 'finish-intent 'input)}))

(defn- fallback-case-form
  "A semantically-neutral template swap: the seed's `case` with the
  fallback branch carrying `fallback` instead of the whole input."
  [fallback]
  (list 'case 'op
        :echo {:action (tool-call-form :fixture/echo)}
        :finish {:action (list 'finish-intent (list 'get 'input :value))}
        {:action (list 'finish-intent fallback)}))

(def demo-templates
  "The deterministic template/function-swap catalog, in proposal order:
  the reference :routing/echo-b improvement first, then two
  semantically-neutral fallback swaps. Every template is a pure
  function of nothing (constant forms), so the proposal order is fixed."
  [{:template/id :routing/echo-b
    :form (echo-b-case-form)
    :expected-effect {:primary-metric :task/success :direction :increase}}
   {:template/id :fallback/ok
    :form (fallback-case-form "ok")
    :expected-effect {:primary-metric :task/success :direction :increase}}
   {:template/id :fallback/finished
    :form (fallback-case-form "finished")
    :expected-effect {:primary-metric :task/success :direction :increase}}])

(defn- demo-hypothesis-id
  "The deterministic name-based hypothesis the demo mutator supplies
  when the diagnosis carries none (a fresh demo state dir has no
  evidence, so the pattern Diagnostician proposes nothing). Fixed for
  the adapter, so proposals stay deterministic."
  []
  (java.util.UUID/nameUUIDFromBytes
   (.getBytes "evoclj/demo-mutator" StandardCharsets/UTF_8)))

;; --- context trust boundary -----------------------------------------------------

(defn- validate-context!
  "Validate the Mutator context at the trust boundary: a map carrying
  a :parent-genome with a :files map (the source of every op's
  :expect/hash) and a :diagnosis map. Throws
  :mutation/context-invalid otherwise, so a cryptic downstream failure
  becomes a typed error."
  [context]
  (when-not (map? context)
    (throw (err/error :mutation/context-invalid
                      "the Mutator context must be a map"
                      {:value (err/sanitize context)})))
  (let [parent (:parent-genome context)
        files (:files parent)]
    (when-not (and (map? parent) (map? files))
      (throw (err/error :mutation/context-invalid
                        "the Mutator context must carry a :parent-genome map with a :files map"
                        {:value (err/sanitize context)}))))
  (when-not (map? (:diagnosis context))
    (throw (err/error :mutation/context-invalid
                      "the Mutator context must carry a :diagnosis map"
                      {:value (err/sanitize context)})))
  context)

;; --- mutation construction ------------------------------------------------------

(def ^:private route-file
  "The seed route program, the demo mutator's ONLY target: a declared
  mutable asset (:programs) inside the genome's own program files."
  "programs/route.clj")

(defn- route-swap-op
  "One :replace-form op swapping the route program's `case` for
  `form`, with the kernel-computed :expect/hash — the parent's own
  route.clj digest (the same preimage convention the patch runtime
  verifies against)."
  [parent form]
  {:op :replace-form
   :file route-file
   :selector ['case]
   :expect/hash (get-in parent [:files route-file :digest])
   :form form})

(defn- template-mutation
  "One Mutation IR map for `template` from the parent: the op, the
  :program risk class (R2 — a form op on a programs/ asset), the
  template's expected effect, and the hypothesis id — the diagnosis's
  first hypothesis when present, else the demo's own deterministic
  id. Only the keys the adapter owns are returned; the orchestrator
  completes the lineage fields itself."
  [parent diagnosis template]
  (let [hypothesis-id (or (some-> (:hypotheses diagnosis) first :hypothesis/id)
                          (demo-hypothesis-id))]
    {:risk :program
     :hypothesis/id hypothesis-id
     :ops [(route-swap-op parent (:form template))]
     :expected-effect (:expected-effect template)}))

;; --- the adapter -----------------------------------------------------------------

(defrecord DemoMutator []
  Mutator
  (propose-mutations [_ context]
    ;; 1. validate the closed context at the trust boundary
    (validate-context! context)
    ;; 2. propose the full deterministic template-swap catalog — a
    ;;    pure function of the parent genome (Global Constraint 6)
    (let [parent (:parent-genome context)
          diagnosis (:diagnosis context)]
      (mapv (partial template-mutation parent diagnosis)
            demo-templates))))

(defn demo-mutator
  "Construct the built-in heuristic Mutator adapter (Task D1). Zero
  configuration: the adapter is stateless and deterministic, holding
  no store handle (Global Constraint 11) and calling no provider
  (Global Constraint 8). The CLI host injects it under the :demo
  profile through the same :overrides seam hosts use."
  []
  (->DemoMutator))

;; ============================================================================
;; the demo's hidden evaluation surface (host-injected with the mutator)
;;
;; The :demo profile also needs the evaluator's hidden selection cases
;; and fixture providers to run headless: G5 paired selection must be
;; able to show the candidate BEATING the parent. The demo's selection
;; set is the seed route's decision contract: the :echo case both sides
;; pass, and the :echo-b case only the template-swapped candidate can
;; pass (the seed parent falls back to finish), so the utility delta is
;; positive and promotion is honest.
;; ============================================================================

(defn- echo-b-provider
  "The :fixture/echo-b provider the demo's selection fixtures serve
  (mirrors the deterministic full-cycle fixture): a pure echo under
  the distinct tool id the demo mutator's :routing/echo-b branch
  calls."
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
          (throw (err/error :provider/input-invalid
                            "tool-call payload must carry an :args map"
                            {:value (err/sanitize args)})))
        {:tool/id :fixture/echo-b
         :resource {:kind :tool :id :fixture/echo-b}
         :args args}))
    (execute-request! [_ authorized-request]
      {:text (get-in authorized-request [:args :text])})))

(defn demo-selection-cases
  "The demo's hidden selection cases (Task D1), keyed by :case/id as
  the G5 paired runner resolves them: the :echo case both sides pass,
  and the :echo-b case ONLY the demo-mutated candidate passes (the
  seed falls back to finish), so a promoted candidate must show the
  utility improvement the :routing/echo-b template swap claims."
  []
  {:sel/demo-echo
   {:case/id :sel/demo-echo
    :task-input {:op :echo :text "hi"}
    :expected-output [{:action {:intent/type :intent/tool-call
                                :payload {:tool/id :fixture/echo
                                          :args {:text "hi"}}}}
                      {:text "hi"}]
    :tools #{:fixture/echo}
    :critical? false}
   :sel/demo-echo-b
   {:case/id :sel/demo-echo-b
    :task-input {:op :echo-b :text "ho"}
    :expected-output [{:action {:intent/type :intent/tool-call
                                :payload {:tool/id :fixture/echo-b
                                          :args {:text "ho"}}}}
                      {:text "ho"}]
    :tools #{:fixture/echo-b}
    :critical? true}})

(defn demo-selection-fixtures
  "The demo's hidden selection fixtures (Task D1): 1-ary fns the G5
  runner invokes with the derived per-case seed — :fixture/echo is the
  standard seed provider, :fixture/echo-b the demo's own pure echo."
  []
  {:fixture/echo (fn [_seed] (fixture/echo-provider {}))
   :fixture/echo-b (fn [_seed] (echo-b-provider))})

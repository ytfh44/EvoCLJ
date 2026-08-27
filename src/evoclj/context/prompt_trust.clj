(ns evoclj.context.prompt-trust
  "Prompt trust model + provenance header for assembled model requests.

  The trusted RequestAssembler produces, alongside the final :messages, a
  structured provenance header (:prompt/provenance) that attributes every
  message to a source/trust level:

    :kernel  — kernel-supplied instructions (maximal trust)
    :extra   — injected environment/skill segments (extra content)
    :user    — user/task messages
    :model   — model/provider-generated assistant & tool messages
               (informational, lowest trust)

  Contract (S13):
    1. PROVENANCE HEADER — every assembled prompt carries the block
       (:prompt/provenance), a structured, trusted attribution of each
       message to its source/trust level.
    2. KERNEL PRIORITY — kernel instructions are maximal trust and are
       always emitted first; a lower-trust segment (extra/user) can never
       precede or override a kernel instruction.
    3. FAIL-CLOSED & TYPED — a message that cannot be attributed to a known
       trust level, a provenance block that is missing/malformed, or an
       ordering that would place a lower-trust segment before a kernel
       instruction throws a typed error instead of emitting an untrusted
       prompt.

  This is the single implementation of the prompt trust model (INV-05); it
  carries no test-only injection and drives no external effect (INV-09)."
  (:require [evoclj.kernel.error :as err]))

(def ^:const trust-order
  "Descending trust priority. :kernel is maximal trust."
  [:kernel :extra :user :model])

(def ^:const trust-rank
  "Numeric trust rank per level (higher = more trusted).
  Kernel instructions are maximal trust."
  {:kernel 100 :extra 60 :user 40 :model 20})

(defn trust-level?
  "True when `x` is a known trust level."
  [x]
  (boolean (contains? trust-rank x)))

(defn max-trust
  "The highest-trust level among `levels` (a seq of trust keywords).
  Kernel is maximal trust. Throws :prompt/trust-invalid when a level is
  unknown. Returns nil when `levels` is empty."
  [levels]
  (when (seq levels)
    (when-not (every? trust-level? levels)
      (throw (err/error :prompt/trust-invalid
                        "an unknown trust level appears in the set"
                        {:levels (vec levels)})))
    (first (sort-by (fn [l] (- (trust-rank l))) levels))))

(defn source-for-role
  "Map a message :role (keyword or string) to its trust level.

  Unknown roles throw :prompt/trust-unknown-role (fail-closed): a prompt
  whose message cannot be attributed has no provenance and is refused."
  [role]
  (let [r (cond (keyword? role) role
                (string? role) (keyword role)
                :else (throw (err/error :prompt/trust-unknown-role
                                        "message role must be a keyword or string"
                                        {:role role})))]
    (case r
      :system    :kernel
      :user      :user
      :assistant :model
      :tool      :model
      (throw (err/error :prompt/trust-unknown-role
                        (str "cannot attribute message to a trust level; role " r)
                        {:role r})))))

(defn split-base-messages
  "Classify base-call messages into trust groups.

  Returns {:kernel [...] :user [...] :model [...]} partitioning `messages`
  by :role (kernel/system, user, model/assistant+tool). An unclassifiable
  role throws :prompt/trust-unknown-role (fail-closed)."
  [messages]
  (reduce (fn [acc m]
            (update acc (source-for-role (:role m)) (fnil conj []) m))
          {:kernel [] :user [] :model []}
          messages))

(defn- build-provenance
  "Build the provenance header block from an ordered seq of [level message]
  pairs. `segments` is a vector of message maps already in final order."
  [ordered kernel-present?]
  {:prompt/provenance-version 1
   :prompt/segments (mapv (fn [i [level m]]
                            {:segment/trust level
                             :segment/role (:role m)
                             :segment/position i})
                          (range) ordered)
   :prompt/kernel-max-trust (boolean kernel-present?)})

(defn validate-kernel-priority!
  "Assert a provenance header (+ message ordering) upholds kernel priority.

  Returns truthy when valid; throws typed otherwise:
   - :prompt/provenance-missing    when no provenance is supplied
   - :prompt/provenance-invalid    when the block is malformed, carries an
                                   unknown trust level, or omits messages
                                   that are actually present
   - :prompt/kernel-overridden     when a kernel instruction is recorded but
                                   a lower-trust segment precedes it (an
                                   override / precedence inversion)

  The provenance's ordered :prompt/segments list is the trust model's source
  of truth, so an extra/skill segment carrying a :system role is correctly
  classified as :extra and can never masquerade as a kernel instruction."
  [messages provenance]
  (when-not (map? provenance)
    (throw (err/error :prompt/provenance-missing
                      "assembled prompt is missing its provenance header"
                      {})))
  (when-not (= 1 (:prompt/provenance-version provenance))
    (throw (err/error :prompt/provenance-invalid
                      "unsupported provenance header version"
                      {:version (:prompt/provenance-version provenance)})))
  (let [segments (:prompt/segments provenance)]
    (when-not (vector? segments)
      (throw (err/error :prompt/provenance-invalid
                        "provenance header must carry a :prompt/segments vector"
                        {:segments segments})))
    (when (and (seq messages) (empty? segments))
      (throw (err/error :prompt/provenance-invalid
                        "messages are present but none are attributed in the provenance header"
                        {:message-count (count messages)})))
    (when (and (seq segments) (seq messages)
               (not= (count segments) (count messages)))
      (throw (err/error :prompt/provenance-invalid
                        "provenance segment count does not match message count"
                        {:segments (count segments) :messages (count messages)})))
    (doseq [s segments]
      (when-not (trust-level? (:segment/trust s))
        (throw (err/error :prompt/provenance-invalid
                          "provenance segment has an unknown trust level"
                          {:segment s}))))
    (let [trusts (mapv :segment/trust segments)
          kernel-present? (boolean (some #(= :kernel %) trusts))]
      (when (and kernel-present?
                 (not= :kernel (:segment/trust (first segments))))
        (throw (err/error :prompt/kernel-overridden
                          "a lower-trust segment precedes or overrides a kernel instruction"
                          {:first-trust (:segment/trust (first segments))})))
      (when (and (:prompt/kernel-max-trust provenance) (not kernel-present?))
        (throw (err/error :prompt/provenance-invalid
                          "provenance claims kernel instruction presence but none is recorded"
                          {})))))
  true)

(defn prioritized-prompt
  "Order trust-classified groups kernel-first and build the provenance header.

  groups — map with :kernel :extra :user :model keys, each a vector of message
           maps (map order preserved within a level). Missing keys default to
           [].

  Returns {:messages [...] :prompt/provenance {...}}.

  Ordering follows `trust-order` (kernel > extra > user > model): a lower-trust
  segment can never precede a kernel instruction. Messages are emitted in
  provenance trust order so the assembler's wire payload and the header always
  agree. Fail-closed (INV-04/09 style): an unclassifiable role already throws on
  split; a provenance block that violates kernel priority throws here."
  [groups]
  (let [groups (merge {:kernel [] :extra [] :user [] :model []} groups)
        ordered (mapcat (fn [level] (map (fn [m] [level m]) (get groups level))) trust-order)
        messages (vec (map second ordered))
        provenance (build-provenance ordered (boolean (seq (:kernel groups))))]
    (validate-kernel-priority! messages provenance)
    {:messages messages :prompt/provenance provenance}))

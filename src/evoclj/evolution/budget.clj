(ns evoclj.evolution.budget
  "Mutation budgets and risk classes (Task 7.5).

  Every mutation op belongs to exactly one of the plan's risk classes
  (R0-R4; the op language can express only R0-R3):

      R0 :parameter  — the EDN value ops (:set-edn :delete-edn) whose
                       target file is a parameters/* asset
      R1 :behavioral — the text ops (:insert-text :replace-text
                       :delete-text), plus the EDN value ops on
                       text/rule assets (skills/, prompts/)
      R2 :program    — the Clojure form ops (:replace-form
                       :insert-form :delete-form), plus the EDN value
                       ops on programs/* assets
      R3 :topology   — the graph ops (:add-node :remove-node :add-edge
                       :remove-edge :update-node), plus the EDN value
                       ops on a topology asset
      R4 :meta       — evolution policy; the op language cannot express
                       it, so a mutation may only DECLARE it (and v0
                       rejects even the declaration)

  The EDN value ops take their class from the target file's asset
  class (first path component, root-level extension stripped —
  evoclj.evolution.mutation's :file-class rule). Unlisted asset
  classes default to :behavioral (rules/text).

  `mutation-cost` aggregates each op's resource cost by class, and
  `check-budget` enforces three gates IN ORDER:

  1. Enabled-class gate — a mutation DECLARING R4 (:meta) is not
     enabled in the v0 profile: :evolution/risk-not-enabled.
  2. Declared-risk coverage gate — the mutation's declared :risk must
     cover ALL of its ops' classes (an op's class may be at most the
     declared class in the order :parameter < :behavioral < :program <
     :topology < :meta): :evolution/under-declared-risk.
  3. Aggregate-limit gate — per-class resource totals must fit the
     budget profile: :evolution/budget-exceeded.

  This is a HARD gate, not a soft metric (Global Constraint 14): a
  mutation that fails any gate is rejected outright — nothing is
  traded off against utility or cost.

  Byte accounting is derived from op data alone (deterministic and
  content-independent, because the budget gate runs BEFORE patch
  application): an :insert-text op adds the UTF-8 byte length of its
  :text; a :replace-text op adds the length of its :text and deletes
  the length of its string anchor; a :delete-text op deletes the
  length of its string anchor. A line-offset anchor has no computable
  preimage size from the op alone, so it contributes 0 deleted bytes.

  Task E4 (roadmap E4) adds `adapt-budget`, which adapts a profile
  from rejection history (per mutation-class success rate): allowance
  shrinks after consecutive rejections, grows after successes, with
  floor/ceiling caps; deterministic and check-budget-compatible."
  (:require [clojure.string :as str]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)))

;; --- the normative v0 budget profile ----------------------------------------

(def v0-profile
  "The normative v0 budget profile: R0-R3 enabled, R4 (:meta) absent —
  a profile entry is what ENABLES a class, and :meta is not enabled in
  v0. Limits are per mutation: :parameter counts ops; :behavioral
  bounds distinct files and added/deleted bytes; :program bounds
  distinct files and top-level forms touched; :topology bounds
  new/removed nodes and edge changes (an edge add, edge remove, or
  node update)."
  {:parameter {:max-ops 8}
   :behavioral {:max-files 2 :max-added-bytes 8192 :max-deleted-bytes 8192}
   :program {:max-files 2 :max-top-level-forms 3}
   :topology {:max-new-nodes 2 :max-removed-nodes 1 :max-edge-changes 4}})

;; --- op → risk class --------------------------------------------------------

(def ^:private risk-order
  "The plan's R0-R4 ordering, used to decide whether a declared :risk
  covers an op's class."
  {:parameter 0
   :behavioral 1
   :program 2
   :topology 3
   :meta 4})

(def ^:private file-class->risk-class
  "Risk class of an EDN value op (:set-edn :delete-edn) by target file
  asset class. Asset class = first path component with a root-level
  extension stripped. Anything unlisted falls back to :behavioral
  (rules/text)."
  {:parameters :parameter
   :prompts :behavioral
   :skills :behavioral
   :programs :program
   :topology :topology})

(defn- file-class
  "The asset class of a target file: first path component with a
  root-level extension stripped (\"skills/debugging.edn\" → :skills,
  \"topology.edn\" → :topology)."
  [file]
  (keyword (str/replace (first (str/split file #"/")) #"\.[^.]+$" "")))

(defn- utf8-bytes
  "The UTF-8 byte length of a string — the :added-bytes /
  :deleted-bytes unit."
  [^String s]
  (alength (.getBytes s StandardCharsets/UTF_8)))

(defn- anchor-bytes
  "The byte length a text anchor accounts for: a string anchor is its
  own UTF-8 length; a line-offset anchor has no computable preimage
  size from the op alone, so it contributes 0."
  [anchor]
  (if (string? anchor)
    (utf8-bytes anchor)
    0))

(defn op-risk-class
  "The risk class of a single op (R0-R3):

      :topology   — the graph ops :add-node :remove-node :add-edge
                    :remove-edge :update-node
      :program    — the form ops :replace-form :insert-form
                    :delete-form
      :behavioral — the text ops :insert-text :replace-text
                    :delete-text
      file-based  — the EDN ops :set-edn :delete-edn follow their
                    target file's asset class: parameters/* →
                    :parameter; skills/, prompts/ (text/rules) →
                    :behavioral; programs/* → :program; topology →
                    :topology.

  Throws :evolution/unknown-op-class for anything the op language
  cannot express (fail-closed: an unclassifiable op is unbudgetable)."
  [op]
  (case (:op op)
    (:add-node :remove-node :add-edge :remove-edge :update-node) :topology
    (:replace-form :insert-form :delete-form) :program
    (:insert-text :replace-text :delete-text) :behavioral
    (:set-edn :delete-edn) (get file-class->risk-class
                                (file-class (:file op))
                                :behavioral)
    (throw (err/error :evolution/unknown-op-class
                      "cannot budget an op outside the op language"
                      {:op (:op op)}))))

;; --- per-op and aggregate cost ----------------------------------------------

(defn op-cost
  "The budget cost of one op: a single-entry map from its risk class
  to the resources it consumes:

      :parameter   — :ops 1
      :behavioral  — :files #{file}, :added-bytes, :deleted-bytes
      :program     — :files #{file}, :top-level-forms 1
      :topology    — :new-nodes 1 (:add-node), :removed-nodes 1
                     (:remove-node), :edge-changes 1 (:add-edge
                     :remove-edge :update-node)

  Byte deltas come from the op alone (see the namespace docstring):
  an :insert-text adds the UTF-8 length of :text; a :replace-text
  adds the length of :text and deletes the length of its string
  anchor; a :delete-text deletes the length of its string anchor."
  [op]
  (case (:op op)
    :add-node {:topology {:new-nodes 1}}
    :remove-node {:topology {:removed-nodes 1}}
    (:add-edge :remove-edge :update-node) {:topology {:edge-changes 1}}

    (:replace-form :insert-form :delete-form)
    {:program {:files #{(:file op)} :top-level-forms 1}}

    :insert-text
    {:behavioral {:files #{(:file op)}
                  :added-bytes (utf8-bytes (:text op))}}
    :replace-text
    {:behavioral {:files #{(:file op)}
                  :added-bytes (utf8-bytes (:text op))
                  :deleted-bytes (anchor-bytes (:anchor op))}}
    :delete-text
    {:behavioral {:files #{(:file op)}
                  :deleted-bytes (anchor-bytes (:anchor op))}}

    (:set-edn :delete-edn)
    {(get file-class->risk-class (file-class (:file op)) :behavioral)
     {:ops 1}}

    (throw (err/error :evolution/unknown-op-class
                      "cannot budget an op outside the op language"
                      {:op (:op op)}))))

(defn- merge-class-costs
  "Merge two class-cost maps: :files sets union, numeric units add."
  [a b]
  (merge-with (fn [x y]
                (cond
                  (set? x) (into x y)
                  (number? x) (+ x y)
                  :else (throw (err/error :evolution/cost-invalid
                                          "unexpected budget cost value"
                                          {:left x :right y}))))
              a b))

(defn- merge-costs
  [a b]
  (merge-with merge-class-costs a b))

(def ^:private class-cost-defaults
  "The canonical cost shape of each class; a class that appears in a
  cost report carries ALL of its resource keys, missing ones filled
  with these defaults."
  {:parameter {:ops 0}
   :behavioral {:files #{} :added-bytes 0 :deleted-bytes 0}
   :program {:files #{} :top-level-forms 0}
   :topology {:new-nodes 0 :removed-nodes 0 :edge-changes 0}})

(defn mutation-cost
  "Aggregate a mutation's ops into a per-class cost map; classes with
  no ops are absent, and every present class carries its full
  canonical shape:

      {:parameter  {:ops n}
       :behavioral {:files #{...} :added-bytes n :deleted-bytes n}
       :program    {:files #{...} :top-level-forms n}
       :topology   {:new-nodes n :removed-nodes n :edge-changes n}}"
  [mutation]
  (let [merged (reduce (fn [acc op] (merge-costs acc (op-cost op)))
                       {}
                       (:ops mutation))]
    (into {}
          (map (fn [[class cost]]
                 [class (merge (get class-cost-defaults class) cost)]))
          merged)))

;; --- the v0 gates -----------------------------------------------------------

(defn- risk-covers?
  "Does the declared :risk cover an op of class `class`? Yes when the
  op's class is at most the declared class in the R0-R4 order
  (:parameter < :behavioral < :program < :topology < :meta). An
  unknown declared class covers nothing (fail-closed)."
  [declared class]
  (and (contains? risk-order declared)
       (<= (risk-order class) (risk-order declared))))

(def ^:private limit-keys
  "For each risk class, the cost keys to compare and the profile key
  each is bounded by. :files is compared by count."
  {:parameter [{:cost-key :ops :limit-key :max-ops}]
   :behavioral [{:cost-key :files :limit-key :max-files}
                {:cost-key :added-bytes :limit-key :max-added-bytes}
                {:cost-key :deleted-bytes :limit-key :max-deleted-bytes}]
   :program [{:cost-key :files :limit-key :max-files}
             {:cost-key :top-level-forms :limit-key :max-top-level-forms}]
   :topology [{:cost-key :new-nodes :limit-key :max-new-nodes}
              {:cost-key :removed-nodes :limit-key :max-removed-nodes}
              {:cost-key :edge-changes :limit-key :max-edge-changes}]})

(defn- limit-failures
  "The violations of `profile` by `cost` for one class: a vector of
  {:class :limit :actual :max} maps. An absent profile limit means no
  bound."
  [class cost profile]
  (keep (fn [{:keys [cost-key limit-key]}]
          (let [v (get-in cost [class cost-key])
                actual (if (set? v) (count v) (or v 0))
                max (get-in profile [class limit-key])]
            (when (and max (> actual max))
              {:class class :limit limit-key :actual actual :max max})))
        (get limit-keys class)))

(defn check-budget
  "Validate a mutation against a budget profile (v0-profile by
  default), enforcing the three gates IN ORDER:

  1. Enabled-class gate — a mutation DECLARING R4 (:meta) is not
     enabled when the profile carries no :meta entry (v0): throws
     :evolution/risk-not-enabled.
  2. Declared-risk coverage gate — the declared :risk must cover ALL
     of the mutation's op classes (op class ≤ declared class in the
     R0-R4 order): throws :evolution/under-declared-risk with
     :declared, :classes, and :uncovered.
  3. Aggregate-limit gate — each class's aggregated resource cost must
     fit the profile's limits: throws :evolution/budget-exceeded with
     :cost and :failures ({:class :limit :actual :max} each).

  Returns the mutation unchanged when it passes. A HARD gate (Global
  Constraint 14): a failure is never traded off against utility."
  ([mutation] (check-budget mutation v0-profile))
  ([mutation profile]
   (let [declared (:risk mutation)]
     (when (and (= :meta declared) (not (contains? profile :meta)))
       (throw (err/error :evolution/risk-not-enabled
                         "risk class R4 (:meta) is not enabled in this budget profile"
                         {:risk declared})))
     (let [classes (into #{} (map op-risk-class) (:ops mutation))
           uncovered (into #{}
                           (remove #(risk-covers? declared %))
                           classes)]
       (when (seq uncovered)
         (throw (err/error :evolution/under-declared-risk
                           "the mutation's declared :risk does not cover all of its ops' risk classes"
                           {:declared declared
                            :classes (vec (sort-by risk-order classes))
                            :uncovered (vec (sort-by risk-order uncovered))})))
       (let [cost (mutation-cost mutation)
             failures (vec (mapcat #(limit-failures % cost profile)
                                   (keys limit-keys)))]
         (when (seq failures)
           (throw (err/error :evolution/budget-exceeded
                             "mutation exceeds the budget profile limits"
                             {:cost cost :failures failures})))))  
     mutation)))

;; --- Task E4: budget adaptation from rejection history ------------------------

(def adapt-defaults
  "Task E4 adaptation knobs. The allowance multiplier of a risk class
  is clamped into [:floor :ceiling] — the caps that prevent both
  explosion and zeroing — and combines two deterministic signals from
  the class's own history:

      streak signal — the consecutive run of the most recent verdicts:
                      each trailing acceptance grows the allowance by
                      :grow-step, each trailing rejection shrinks it
                      by :shrink-step
      rate signal   — the class's window success rate (accepted /
                      accepted+rejected — the per-mutation-class
                      success rate) pulls the multiplier toward the
                      ceiling (all successes) or the floor (all
                      failures), weighted by :rate-weight over the
                      distance (:ceiling - :floor)"
  {:floor 0.5
   :ceiling 2.0
   :grow-step 0.25
   :shrink-step 0.25
   :rate-weight 0.5})

(defn- verdict-entries
  "The verdict-carrying (:accepted/:rejected) history entries of one
  risk class in CHRONOLOGICAL order. `history` is the Task 7.7
  newest-first vector; a :pending entry carries no verdict, so it
  neither breaks nor extends a streak."
  [history risk]
  (->> history
       (filter #(= risk (:risk %)))
       (filter #(contains? #{:accepted :rejected} (:state %)))
       reverse
       vec))

(defn- trailing-streak
  "The consecutive run of equal verdicts at the NEWEST end of a
  chronological entry vector: {:state ... :length n}."
  [entries]
  (let [newest-state (:state (peek entries))]
    {:state newest-state
     :length (count (take-while #(= newest-state (:state %))
                                (reverse entries)))}))

(defn- class-multiplier
  "The Task E4 allowance multiplier of one risk class: the streak
  signal plus the success-rate pull, clamped into [:floor :ceiling].
  A class with no verdicts has no signal and keeps its base allowance
  (multiplier 1)."
  [entries {:keys [floor ceiling grow-step shrink-step rate-weight]}]
  (if (empty? entries)
    1.0
    (let [n (count entries)
          accepted (count (filter #(= :accepted (:state %)) entries))
          rate (/ accepted n)
          {:keys [state length]} (trailing-streak entries)
          streak-mult (if (= :accepted state)
                        (+ 1.0 (* grow-step length))
                        (- 1.0 (* shrink-step length)))
          rate-pull (* rate-weight (- rate 0.5) (- ceiling floor))]
      (max floor (min ceiling (+ streak-mult rate-pull))))))

(defn- adapt-class
  "Scale every numeric limit of one class's profile entry by the
  class's allowance multiplier; non-numeric values pass through. A
  limit never drops below 1 — the cap that prevents zeroing."
  [limit-map multiplier]
  (into {}
        (map (fn [[k v]]
               [k (if (number? v)
                    (max 1 (Math/round (* v multiplier)))
                    v)]))
        limit-map))

(defn adapt-budget
  "Task E4: adapt a budget profile from rejection history — pure and
  deterministic.

  `history` is a vector of Task 7.7 history entries, NEWEST FIRST (the
  recent-mutation-history contract); each entry contributes its :risk
  class and its resolved verdict :state (:accepted / :rejected /
  :pending — pending carries no verdict). Every risk class present in
  the profile adapts INDEPENDENTLY from its own verdicts (the
  per-mutation-class success rate); a class with no verdicts keeps
  its base allowance unchanged, so an unchanged history yields an
  unchanged profile.

  The allowance of a class is its profile entry's numeric limits,
  scaled by a multiplier m clamped into [:floor :ceiling] (defaults
  in adapt-defaults):

      m = clamp(streak-mult + rate-pull, floor, ceiling)

      streak-mult = 1 + :grow-step * k   the k most recent verdicts
                   1 - :shrink-step * k  are all accepted/rejected
      rate-pull   = :rate-weight * (r - 0.5) * (ceiling - floor)
                    r = accepted / (accepted + rejected)

  Consecutive rejections shrink the allowance, successes grow it, and
  the caps keep the result bounded: limits are rounded to integers
  and never drop below 1, so the profile can neither explode nor
  reach zero. The result has the same shape as the base profile
  (v0-profile by default) and is safe to pass to check-budget — the
  propose loop's budget check consumes it wherever the host supplies
  an adapted :budget-profile.

  Typed error: :evolution/adapt-invalid when history is not a
  sequential collection (fail-closed)."
  ([history] (adapt-budget history v0-profile))
  ([history profile] (adapt-budget history profile adapt-defaults))
  ([history profile opts]
   (when-not (or (nil? history) (sequential? history))
     (throw (err/error :evolution/adapt-invalid
                       "adapt-budget history must be a sequential collection of Task 7.7 entries"
                       {:history (err/sanitize history)})))
   (reduce (fn [acc [risk limit-map]]
             (let [m (class-multiplier (verdict-entries (vec history) risk)
                                       (merge adapt-defaults opts))]
               (if (= 1.0 m)
                 acc
                 (assoc acc risk (adapt-class limit-map m)))))
           profile
           profile)))

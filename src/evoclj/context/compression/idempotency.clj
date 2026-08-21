(ns evoclj.context.compression.idempotency
  "Idempotency guarantees for the context-compression re-compression path.

  The core invariant: re-compression takes an OLD envelope A (from a
  previous compression) plus a NEW envelope B (freshly produced from
  A + the turns since). The result must be semantically equivalent to
  what a single compression of the whole span would have produced.

  Idempotency guards (all throw `:context/idempotency-violation`):
  - Version match: B must not bump :envelope/version without explicit
    caller-side migration handling.
  - Core field pass-through: B must not drop a core field that A had.
  - Residue accumulation: every :residue/text present in A must appear
    in the result after merge.
  - Evidence accumulation: every :evidence/text present in A must appear
    in the result after merge.

  Provenance is preserved automatically: merge-envelopes accumulates
  residue by :residue/text identity, so every traceable claim that was
  present in A remains traceable in the result."
  (:require [evoclj.context.compression.error :as err]
            [evoclj.context.compression.envelope :as envelope]
            [clojure.set :as set]))

;; ----------------------------------------------------------------------
;; Constants
;; ----------------------------------------------------------------------

(def ^:private ^:const core-envelope-keys
  "Core envelope fields (excludes :envelope/task which is optional).
   If A has one of these, B must have it too."
  #{:envelope/version
    :envelope/created-at
    :envelope/window
    :envelope/tokens-before
    :envelope/tokens-after
    :envelope/compressor})

;; ----------------------------------------------------------------------
;; Core field preservation
;; ----------------------------------------------------------------------

(defn core-fields-preserved?
  "True when B carries every core field A carried.

  The check is PRESENCE, not value equality: B is the newer envelope,
  so its core fields legitimately differ (window advances, token
  counts change). What must hold is that B did not DROP a field A
  had. Task may be nil in both — but if A had a task, B must too.
  Returns false (never throws) when a core field is missing in B."
  [a b]
  (let [a-core (select-keys a core-envelope-keys)
        a-task (:envelope/task a)
        b-task (:envelope/task b)]
    (and (every? (fn [k] (contains? b k)) (keys a-core))
         (or (nil? a-task) (some? b-task)))))

;; ----------------------------------------------------------------------
;; Residue and evidence preservation
;; ----------------------------------------------------------------------

(defn residue-preserved?
  "True when every :residue/text in A's residue appears in RESULT's
  residue.

  RESULT is the output of a merge operation. This is a post-merge
  diagnostic — the merge itself accumulates by text, so this
  should always return true unless the merge is buggy."
  [a result]
  (let [a-texts   (set (map :residue/text (:envelope/residue a [])))
        r-texts   (set (map :residue/text (:envelope/residue result [])))]
    (every? #(contains? r-texts %) a-texts)))

(defn evidence-preserved?
  "True when every :evidence/text in A's evidence appears in RESULT's
  evidence.

  RESULT is the output of a merge operation. This is a post-merge
  diagnostic — the merge itself accumulates by text, so this
  should always return true unless the merge is buggy."
  [a result]
  (let [a-texts   (set (map :evidence/text (:envelope/evidence a [])))
        r-texts   (set (map :evidence/text (:envelope/evidence result [])))]
    (every? #(contains? r-texts %) a-texts)))

;; ----------------------------------------------------------------------
;; Idempotency report
;; ----------------------------------------------------------------------

(defn idempotency-report
  "Produce a diagnostic report of idempotency between two envelopes.

  Does NOT throw. All seven keys are always present:
  - :idempotency/valid?               — true only when all four
                                        sub-checks pass
  - :idempotency/residue-preserved?   — every :residue/text in A
                                        survives in RESULT
  - :idempotency/evidence-preserved?   — every :evidence/text in A
                                        survives in RESULT
  - :idempotency/core-fields-preserved? — B carries every core field
                                         A carried
  - :idempotency/version-match?        — :envelope/version is identical
  - :idempotency/lost-residue-texts   — texts in A not in RESULT
  - :idempotency/lost-evidence-texts  — texts in A not in RESULT

  Pass RESULT (the merged output) when available to get accurate
  lost-* lists. If RESULT is nil the lost lists will be empty."
  ([a b]
   (idempotency-report a b nil))
  ([a b result]
   (let [version-match?           (= (:envelope/version a) (:envelope/version b))
         cf-preserved?             (core-fields-preserved? a b)
         resid-preserved?          (residue-preserved? a (or result a))
         evid-preserved?          (evidence-preserved? a (or result a))

         a-residue-texts          (set (map :residue/text (:envelope/residue a [])))
         r-residue-texts           (set (map :residue/text (:envelope/residue (or result a) [])))
         lost-residue-texts        (vec (sort (set/difference a-residue-texts r-residue-texts)))

         a-evidence-texts          (set (map :evidence/text (:envelope/evidence a [])))
         r-evidence-texts          (set (map :evidence/text (:envelope/evidence (or result a) [])))
         lost-evidence-texts       (vec (sort (set/difference a-evidence-texts r-evidence-texts)))]

     {:idempotency/valid?                (and version-match?
                                               cf-preserved?
                                               resid-preserved?
                                               evid-preserved?)
      :idempotency/version-match?        version-match?
      :idempotency/core-fields-preserved? cf-preserved?
      :idempotency/residue-preserved?     resid-preserved?
      :idempotency/evidence-preserved?    evid-preserved?
      :idempotency/lost-residue-texts    lost-residue-texts
      :idempotency/lost-evidence-texts   lost-evidence-texts})))

;; ----------------------------------------------------------------------
;; Core merge operation
;; ----------------------------------------------------------------------

(defn idempotent-merge
  "Merge two envelopes A (older) and B (newer) into one validated
  envelope, guaranteeing that re-compression is idempotent.

  Delegated to `evoclj.context.compression.envelope/merge-envelopes` which
  handles accumulation of residue and evidence by text identity,
  subgoal merging by id, and core field pass-through from B.

  Before delegating, performs three idempotency guards and throws
  `:context/idempotency-violation` on any violation:

  1. Version match — B's :envelope/version must equal A's.  A version
     bump is a schema change requiring explicit caller-side migration;
     reject it so the caller handles it explicitly.

  2. Core field pass-through — B must not drop a core field that A
     had. Delegated to merge-envelopes which enforces this.

  3. Residue accumulation — after merge, every :residue/text present
     in A must appear in the result.  Merge-envelopes accumulates by
     text, so this is verified post-merge.

  4. Evidence accumulation — after merge, every :evidence/text
     present in A must appear in the result.  Verified post-merge.

  Returns the validated merged envelope on success."
  [a b]
  ;; Guard 1: version must match
  (when (and (contains? a :envelope/version)
             (contains? b :envelope/version)
             (not= (:envelope/version a) (:envelope/version b)))
    (throw (err/error :context/idempotency-violation
                      "B's :envelope/version differs from A's — schema version bump requires explicit caller-side migration"
                      {:a-version (:envelope/version a)
                       :b-version (:envelope/version b)})))

  ;; Guard 2: core field pass-through — merge-envelopes throws
  ;; :context/idempotency-violation if violated
  (let [result (envelope/merge-envelopes a b)]

    ;; Guard 3: residue accumulation
    (when-not (residue-preserved? a result)
      (let [a-texts   (set (map :residue/text (:envelope/residue a [])))
            r-texts    (set (map :residue/text (:envelope/residue result [])))
            lost       (vec (sort (set/difference a-texts r-texts)))]
        (throw (err/error :context/idempotency-violation
                          "Post-merge residue check failed — texts from A are absent in result"
                          {:lost-residue-texts lost}))))

    ;; Guard 4: evidence accumulation
    (when-not (evidence-preserved? a result)
      (let [a-texts   (set (map :evidence/text (:envelope/evidence a [])))
            r-texts    (set (map :evidence/text (:envelope/evidence result [])))
            lost       (vec (sort (set/difference a-texts r-texts)))]
        (throw (err/error :context/idempotency-violation
                          "Post-merge evidence check failed — texts from A are absent in result"
                          {:lost-evidence-texts lost}))))

    result))

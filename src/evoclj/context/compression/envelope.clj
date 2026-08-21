(ns evoclj.context.compression.envelope
  "The self-contained data structure produced by context compression.

  An envelope is both a summary of the past and the prefix of the
  future. It has four sections — task, subgoals, residue, evidence —
  plus compression metadata. The envelope is closed: no field may be
  added or removed without a schema version bump.

  Error contract: every validation failure throws a typed
  :context/compression-invalid ExceptionInfo (or
  :context/idempotency-violation for merge errors). All ex-data is
  plain, sanitized, serializable Clojure data that round-trips through
  pr-str / clojure.edn read-string."
  (:require [evoclj.context.compression.error :as err]
            [evoclj.sci.boundary :as boundary]
            [evoclj.genome.types :as types]
            [malli.core :as m]
            [malli.error :as me]))

;; --------------------------------------------------------------------
;; Schemas
;; --------------------------------------------------------------------

(def TaskStatusSchema
  "The lifecycle status of a task."
  [:enum :pending :in-progress :completed :blocked :cancelled])

(def ResidueKindSchema
  "The kind of a residue entry."
  [:enum :constraint :decision :discovery :open :state])

(def EvidenceKindSchema
  "The kind of an evidence entry."
  [:enum :test-pass :test-fail :command :observation])

(def TaskSchema
  "A structured task summary."
  [:map {:closed true}
   [:task/id string?]
   [:task/status TaskStatusSchema]
   [:task/description string?]
   [:task/owner {:optional true} string?]])

(def SubgoalSchema
  "A structured subgoal."
  [:map {:closed true}
   [:subgoal/id string?]
   [:subgoal/status TaskStatusSchema]
   [:subgoal/description string?]
   [:subgoal/parent {:optional true} string?]])

(def ResidueEntrySchema
  "A single residue entry: non-structured memory that structured tools
  cannot cover."
  [:map {:closed true}
   [:residue/id int?]
   [:residue/kind ResidueKindSchema]
   [:residue/text string?]
   [:residue/source string?]
   [:residue/at [:or string? inst?]]])

(def EvidenceEntrySchema
  "A single evidence entry: structured, non-discardable facts."
  [:map {:closed true}
   [:evidence/id int?]
   [:evidence/kind EvidenceKindSchema]
   [:evidence/text string?]
   [:evidence/at [:or string? inst?]]])

(def CompressorInfoSchema
  "Metadata about the compressor that produced this envelope."
  [:map {:closed true}
   [:compressor/model string?]
   [:compressor/prompt string?]])

(def WindowSchema
  "The turn window this envelope covers."
  [:map {:closed true}
   [:window/from int?]
   [:window/to int?]])

(def EnvelopeSchema
  "The closed envelope map schema. All four sections (task, subgoals,
  residue, evidence) plus compression metadata."
  [:map {:closed true}
   [:envelope/version int?]
   [:envelope/created-at [:or string? inst?]]
   [:envelope/window WindowSchema]
   [:envelope/tokens-before int?]
   [:envelope/tokens-after int?]
   [:envelope/compressor CompressorInfoSchema]
   [:envelope/task {:optional true} TaskSchema]
   [:envelope/subgoals {:optional true} [:vector SubgoalSchema]]
   [:envelope/residue [:vector ResidueEntrySchema]]
   [:envelope/evidence [:vector EvidenceEntrySchema]]])

;; --------------------------------------------------------------------
;; Validation helpers
;; --------------------------------------------------------------------

(defn- validate-schema
  "Validate `x` against `schema`. Returns `x` unchanged on success.
  Throws :context/compression-invalid on failure with sanitized value
  and Malli explanation."
  [schema x]
  (if-let [expl (m/explain schema x)]
    (throw (err/error :context/compression-invalid
                      "envelope does not satisfy EnvelopeSchema"
                      {:value (err/sanitize x)
                       :explanation (err/sanitize expl)}))
    x))

;; --------------------------------------------------------------------
;; Public API
;; --------------------------------------------------------------------

(defn make-envelope
  "Construct a validated envelope from its parts.

  Takes named arguments for each section and metadata. Rejects
  malformed input with :context/compression-invalid. Never coerces."
  [{:keys [task subgoals residue evidence version created-at window
           tokens-before tokens-after compressor]
    :or {version 1
         subgoals []
         residue []
         evidence []}}]
  (let [envelope {:envelope/version version
                  :envelope/created-at created-at
                  :envelope/window window
                  :envelope/tokens-before tokens-before
                  :envelope/tokens-after tokens-after
                  :envelope/compressor compressor
                  :envelope/task task
                  :envelope/subgoals subgoals
                  :envelope/residue residue
                  :envelope/evidence evidence}]
    (validate-schema EnvelopeSchema envelope)
    envelope))

(defn validate-envelope
  "Validate `x` against EnvelopeSchema.

  First checks EDN-safety via boundary/edn-safe?. Returns `x`
  unchanged on success. Throws :context/compression-invalid on
  failure with a sanitized :value and a serializable Malli
  :explanation."
  [x]
  (when-not (boundary/edn-safe? x)
    (throw (err/error :context/compression-invalid
                      "envelope is not EDN-safe"
                      {:value (err/sanitize x)})))
  (validate-schema EnvelopeSchema x))

(defn envelope->edn
  "Serialize an envelope to a string via pr-str."
  [envelope]
  (pr-str envelope))

(defn edn->envelope
  "Read an envelope from a string via clojure.edn/read-string,
  then validate it. Throws :context/compression-invalid on failure."
  [s]
  (let [x (clojure.edn/read-string s)]
    (validate-envelope x)))

(defn envelope-tokens
  "Return the :envelope/tokens-after value of an envelope, the
  size of the envelope itself as a proxy for its cost."
  [envelope]
  (:envelope/tokens-after envelope))

(defn merge-envelopes
  "Merge two envelopes `a` and `b` (b is newer) into one validated
  envelope.

  Core fields (task, window, compressor, version, created-at,
  tokens-before, tokens-after) are taken from `b`, overriding `a`.
  Residue entries are accumulated: the union by :residue/text content
  is taken, preserving first-seen order (entries from `a` come first,
  then new entries from `b` that have no text match in `a`).
  Evidence is accumulated the same way (union by :evidence/text).
  Subgoals are merged by :subgoal/id: if both have the same id, the
  one with the newer status wins (b's status wins ties).

  Throws :context/idempotency-violation if `b` is missing a core
  field that `a` has and `b` does not (e.g. b has no :task/id but a
  does)."
  [a b]
  (when-not (map? a)
    (throw (err/error :context/compression-invalid "a must be a map" {:value (err/sanitize a)})))
  (when-not (map? b)
    (throw (err/error :context/compression-invalid "b must be a map" {:value (err/sanitize b)})))
  (let [;; --- idempotency checks first --------------------------------
        core-keys [:envelope/version :envelope/created-at :envelope/window
                   :envelope/tokens-before :envelope/tokens-after
                   :envelope/compressor]
        lost-core (seq (for [k core-keys
                            :when (and (contains? a k) (not (contains? b k)))]
                        k))
        _ (when lost-core
            (throw (err/error :context/idempotency-violation
                              "b is missing core fields that a had"
                              {:lost-fields (err/sanitize (vec lost-core))})))
        _ (when (and (:envelope/task a) (nil? (:envelope/task b)))
            (throw (err/error :context/idempotency-violation
                              "b dropped :envelope/task that a had"
                              {:lost-fields [:envelope/task]})))

        ;; --- task ----------------------------------------------------
        task (or (:envelope/task b) (:envelope/task a))

        ;; --- subgoals: merge by id, newer status wins ----------------
        sg-map (reduce
                (fn [m sg]
                  (let [existing (get m (:subgoal/id sg))]
                    (if (or (nil? existing)
                            (neg? (compare (:subgoal/status sg)
                                           (:subgoal/status existing))))
                      (assoc m (:subgoal/id sg) sg)
                      m)))
                (into {} (map (fn [sg] [(:subgoal/id sg) sg]))
                      (:envelope/subgoals a []))
                (:envelope/subgoals b []))
        subgoals (vec (vals sg-map))

        ;; --- residue: accumulate by :residue/text --------------------
        residue-a (:envelope/residue a [])
        residue-b (:envelope/residue b [])
        seen-residue-texts (set (map :residue/text residue-a))
        new-residue (filterv #(not (contains? seen-residue-texts (:residue/text %)))
                             residue-b)
        residue (vec (concat residue-a new-residue))

        ;; --- evidence: accumulate by :evidence/text -------------------
        evidence-a (:envelope/evidence a [])
        evidence-b (:envelope/evidence b [])
        seen-evidence-texts (set (map :evidence/text evidence-a))
        new-evidence (filterv #(not (contains? seen-evidence-texts (:evidence/text %)))
                              evidence-b)
        evidence (vec (concat evidence-a new-evidence))

        ;; --- assemble -------------------------------------------------
        merged {:envelope/version (or (:envelope/version b) (:envelope/version a))
                :envelope/created-at (or (:envelope/created-at b) (:envelope/created-at a))
                :envelope/window (or (:envelope/window b) (:envelope/window a))
                :envelope/tokens-before (or (:envelope/tokens-before b) (:envelope/tokens-before a))
                :envelope/tokens-after (or (:envelope/tokens-after b) (:envelope/tokens-after a))
                :envelope/compressor (or (:envelope/compressor b) (:envelope/compressor a))
                :envelope/task task
                :envelope/subgoals subgoals
                :envelope/residue residue
                :envelope/evidence evidence}]
    (validate-schema EnvelopeSchema merged)
    merged))

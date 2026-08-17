(ns evoclj.context.residue
  "Residue manager — the non-structured, load-bearing memory of the
  context-compression subsystem.

  A residue entry captures the kind of thing structured tools (todo,
  goal trackers) CANNOT cover: user constraints, rejected方案 with
  reasons, discovered file paths / API shapes / dependency facts,
  open questions, and temporary bad states. It is the agent's
  autobiography.

  Every residue entry carries a stable identity key (:residue/text):
  two entries with the same text are the same entry, regardless of
  kind. This makes deduplication cheap and enforces that a constraint
  restated is still the same constraint."
  (:require [evoclj.context.error :as err]))

;; ----------------------------------------------------------------------
;; Kind vocabulary
;; ----------------------------------------------------------------------

(def ^:private valid-residue-kinds
  #{:constraint :decision :discovery :open :state})

(defn residue-kind?
  "Return true when `k` is one of the five valid residue kinds:
  :constraint, :decision, :discovery, :open, :state."
  [k]
  (contains? valid-residue-kinds k))

;; ----------------------------------------------------------------------
;; Field validators (private, throwing)
;; ----------------------------------------------------------------------

(defn- validate-id!
  [id]
  (when-not (and (integer? id) (>= id 0))
    (throw (err/error
            :context/residue-invalid
            (str ":context/residue-invalid — :residue/id must be a "
                 "non-negative integer, got: " (pr-str id))
            {:field :residue/id :value id}))))

(defn- validate-kind!
  [kind]
  (when-not (residue-kind? kind)
    (throw (err/error
            :context/residue-invalid
            (str ":context/residue-invalid — :residue/kind must be one of: "
                 valid-residue-kinds ", got: " (pr-str kind))
            {:field :residue/kind :value kind}))))

(defn- validate-text!
  [text]
  (when-not (string? text)
    (throw (err/error
            :context/residue-invalid
            (str ":context/residue-invalid — :residue/text must be a string, "
                 "got: " (pr-str (type text)))
            {:field :residue/text :value text})))
  (when (empty? text)
    (throw (err/error
            :context/residue-invalid
            (str ":context/residue-invalid — :residue/text must be a "
                 "non-empty string")
            {:field :residue/text :value text}))))

(defn- validate-source!
  [source]
  (when-not (string? source)
    (throw (err/error
            :context/residue-invalid
            (str ":context/residue-invalid — :residue/source must be a "
                 "non-empty string, got: " (pr-str (type source)))
            {:field :residue/source :value source})))
  (when (empty? source)
    (throw (err/error
            :context/residue-invalid
            (str ":context/residue-invalid — :residue/source must be a "
                 "non-empty string")
            {:field :residue/source :value source}))))

(defn- validate-at!
  [at]
  (when-not (or (string? at) (instance? java.util.Date at))
    (throw (err/error
            :context/residue-invalid
            (str ":context/residue-invalid — :residue/at must be a string "
                 "or java.util.Date, got: " (pr-str (type at)))
            {:field :residue/at :value at}))))

;; ----------------------------------------------------------------------
;; Construction and validation
;; ----------------------------------------------------------------------

(defn make-residue
  "Construct and validate a residue entry.

   `id`    — non-negative integer
   `kind`  — one of: :constraint, :decision, :discovery, :open, :state
   `text`  — non-empty string
   `source` — non-empty string describing the origin
   `at`    — ISO-8601 string or java.util.Date

   Returns a validated residue map on success.
   Throws `:context/residue-invalid` on any validation failure.
   Never coerces inputs."
  [id kind text source at]
  (validate-id! id)
  (validate-kind! kind)
  (validate-text! text)
  (validate-source! source)
  (validate-at! at)
  {:residue/id      id
   :residue/kind    kind
   :residue/text    text
   :residue/source  source
   :residue/at      at})

(defn validate-residue
  "Validate an arbitrary value `x` as a residue entry.

   Returns `x` unchanged on success.
   Throws `:context/residue-invalid` with sanitized `:value` on failure."
  [x]
  (when-not (map? x)
    (throw (err/error
            :context/residue-invalid
            (str ":context/residue-invalid — residue must be a map, got: "
                 (pr-str (type x)))
            {:value (err/sanitize x)})))
  (when-not (contains? x :residue/id)
    (throw (err/error
            :context/residue-invalid
            ":context/residue-invalid — residue missing required :residue/id"
            {:value (err/sanitize x)})))
  (when-not (contains? x :residue/kind)
    (throw (err/error
            :context/residue-invalid
            ":context/residue-invalid — residue missing required :residue/kind"
            {:value (err/sanitize x)})))
  (when-not (contains? x :residue/text)
    (throw (err/error
            :context/residue-invalid
            ":context/residue-invalid — residue missing required :residue/text"
            {:value (err/sanitize x)})))
  (when-not (contains? x :residue/source)
    (throw (err/error
            :context/residue-invalid
            ":context/residue-invalid — residue missing required :residue/source"
            {:value (err/sanitize x)})))
  (when-not (contains? x :residue/at)
    (throw (err/error
            :context/residue-invalid
            ":context/residue-invalid — residue missing required :residue/at"
            {:value (err/sanitize x)})))
  ;; Re-validate individual fields to surface precise errors
  (validate-id! (:residue/id x))
  (validate-kind! (:residue/kind x))
  (validate-text! (:residue/text x))
  (validate-source! (:residue/source x))
  (validate-at! (:residue/at x))
  x)

;; ----------------------------------------------------------------------
;; Accessors
;; ----------------------------------------------------------------------

(defn residue-text
  "Return the :residue/text string from a residue entry."
  [r]
  (:residue/text r))

;; ----------------------------------------------------------------------
;; Collection operations
;; ----------------------------------------------------------------------

(defn append-residue
  "Append `new` to the residue vector `residues`, deduplicating by
  :residue/text.

   `new` may be a single residue entry or a vector of entries.
   Entries whose text already appears in `residues` are NOT duplicated;
   new entries are appended preserving order.

   Returns a vector."
  [residues new]
  {:pre [(vector? residues)]}
  (let [items (if (vector? new) new [new])
        existing (set (map :residue/text residues))]
    (into residues (filter #(not (contains? existing (:residue/text %)))) items)))

(defn residues-by-kind
  "Return the vector of residue entries whose :residue/kind equals `kind`.

   `kind` must be one of the five valid residue kinds.
   Throws `:context/residue-invalid` if `kind` is not valid."
  [residues kind]
  {:pre [(vector? residues)]}
  (validate-kind! kind)
  (filterv #(= (:residue/kind %) kind) residues))

(defn residue-merge
  "Merge two residue vectors `a` and `b`, deduplicating by :residue/text.

   First-seen order is preserved: all entries from `a` appear first,
   then new entries from `b` that are not already in `a`.

   Returns a vector."
  [a b]
  {:pre [(vector? a) (vector? b)]}
  (let [existing (set (map :residue/text a))]
    (into a (filter #(not (contains? existing (:residue/text %)))) b)))

;; ----------------------------------------------------------------------
;; EDN round-trip
;; ----------------------------------------------------------------------

(defn residue->edn
  "Serialize a single residue entry to an EDN string via pr-str."
  [r]
  {:pre [(map? r)]}
  (pr-str r))

(defn edn->residue
  "Read a single residue entry from an EDN string.
   Validates the entry after reading.
   Throws `:context/residue-invalid` on malformed or invalid input."
  [s]
  {:pre [(string? s)]}
  (let [parsed (clojure.edn/read-string s)]
    (validate-residue parsed)))

(defn residues->edn
  "Serialize a vector of residue entries to an EDN string via pr-str."
  [rs]
  {:pre [(vector? rs)]}
  (pr-str rs))

(defn edn->residues
  "Read a vector of residue entries from an EDN string.
   Validates every entry after reading.
   Throws `:context/residue-invalid` on malformed or invalid input."
  [s]
  {:pre [(string? s)]}
  (let [parsed (clojure.edn/read-string s)]
    (when-not (vector? parsed)
      (throw (err/error
              :context/residue-invalid
              (str ":context/residue-invalid — edn->residues expected a "
                   "vector, got: " (pr-str (type parsed)))
              {:value (err/sanitize parsed)})))
    (mapv validate-residue parsed)))

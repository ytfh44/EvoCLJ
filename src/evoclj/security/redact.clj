(ns evoclj.security.redact
  "Write-path secret redaction for evolved events (Trust & Hygiene F7).

  Event metadata is validated as EDN-safe by the store, but nothing
  guarantees that secrets keyed or embedded in that metadata are removed
  before persistence. This module is the pure redaction layer: given a
  value (always EDN-safe) and a collection of declarative redaction
  specs, it rewrites the value so secret-bearing sub-values are replaced
  by the literal \"[REDACTED]\" before anything is appended to the event
  log.

  A spec is one of two kinds:

      :pattern  — a regular expression; every match in every STRING value
                  encountered during the walk is replaced with
                  \"[REDACTED]\" (used for token/credential lexical
                  shapes, e.g. a bearer-token pattern).
      :key-path — a non-empty vector of keyword-vectors; when a map
                  contains the key path (via get-in), the value at that
                  path is replaced with \"[REDACTED]\" — whether it is a
                  map, vector, string, or any other EDN value.

  Contract guarantees:

  - IDEMPOTENT — redacting an already-redacted value yields the same
    value: pattern matches that have already become \"[REDACTED]\" no
    longer match the spec's pattern, and a :key-path value already equal
    to \"[REDACTED]\" is replaced by the same literal again.
  - KEYS ARE NEVER REWRITTEN — only values are transformed; map keys and
    the key path components themselves pass through untouched, so
    authorization- and navigation-relevant structure is preserved.
  - Only values are transformed; everything else passes through.

  `redact-event` is the write-path hookup target: it redacts ONLY the
  event's :metadata and leaves :payload-ref/:cause/ids byte-identical.
  The write path MUST call this BEFORE append-event!.

  Error contract (Global Constraint 22 — plain serializable data):
  :security/redact-invalid with :reason (:not-sequential when specs is
  not a sequential collection, :spec-invalid when a spec fails
  RedactSpecSchema)."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [evoclj.kernel.error :as err]))

(def ^:private redaction-marker
  "The literal replacement inserted wherever a secret is removed."
  "[REDACTED]")

(defn regex-like?
  "True for a value usable as a :pattern redaction source: a
  compiled java.util.regex.Pattern (a Clojure `#\"\"` regex literal) or
  a plain string regex source."
  [v]
  (or (instance? java.util.regex.Pattern v)
      (string? v)))

(def RedactSpecSchema
  "The closed contract for a single redaction spec: exactly one kind
  (:pattern or :key-path) and, for :pattern specs, a regex source
  (:redact/pattern — a regex literal or string regex). The schema is
  closed — unknown keys at a trust boundary are rejected. (The :pattern
  field is a `regex-like?` value, not Malli's [:re ...] string schema,
  because a Clojure regex literal is a compiled Pattern object.)"
  [:map {:closed true}
   [:redact/kind [:enum :pattern :key-path]]
   [:redact/pattern {:optional true} [:fn regex-like?]]
   [:redact/paths {:optional true} [:sequential [:sequential keyword?]]]])

;; --- validation --------------------------------------------------------------

(defn validate-specs!
  "Validate a specs collection against RedactSpecSchema. Returns the
  specs unchanged, or throws :security/redact-invalid (:reason
  :not-sequential when specs is not sequential, :spec-invalid with a
  humanized Malli explanation when any spec violates the contract)."
  [specs]
  (when-not (sequential? specs)
    (throw (err/error :security/redact-invalid
                      "redact specs must be a sequential collection"
                      {:reason :not-sequential})))
  (doseq [spec specs]
    (when-let [expl (m/explain RedactSpecSchema spec)]
      (throw (err/error :security/redact-invalid
                        "redact spec does not satisfy the redact contract"
                        {:reason :spec-invalid
                         :errors (me/humanize expl)}))))
  specs)

;; --- the redaction walk (specs are applied left-to-right) --------------------

(defn- redact-pattern
  "Replace every match of a :pattern spec's regex in every STRING value
  of `v` with the redaction marker, recursing through maps (keys kept,
  values redacted), vectors, and seqs. Non-string values pass through."
  [v spec]
  (let [pat (:redact/pattern spec)]
    (cond
      (string? v) (str/replace v pat redaction-marker)
      (map? v) (into (empty v)
                     (map (fn [[k val]] [k (redact-pattern val spec)]))
                     v)
      (vector? v) (mapv #(redact-pattern % spec) v)
      (seq? v) (map #(redact-pattern % spec) v)
      :else v)))

(defn- redact-key-path*
  "Replace the value at `path` in `v` when `v` (or any map reachable
  through its maps/vectors/seqs) contains the key path. A map that
  contains the path has that value replaced (assoc-in), and every map is
  recursed into, so a path nested under a vector or a nested map is found
  too. Non-map values at the leaf are replaced as well. Keys and the key
  path components are never rewritten."
  [v path]
  (cond
    (map? v)
    (let [v' (if (get-in v path) (assoc-in v path redaction-marker) v)]
      (into (empty v')
            (map (fn [[k val]] [k (redact-key-path* val path)]))
            v'))
    (vector? v) (mapv #(redact-key-path* % path) v)
    (seq? v) (map #(redact-key-path* % path) v)
    :else v))

(defn- redact-key-path
  "For each key path in a :key-path spec, replace the value at that path
  wherever a reachable map contains it."
  [v spec]
  (reduce (fn [acc path] (redact-key-path* acc path))
          v
          (:redact/paths spec)))

(defn- apply-specs
  "Apply every spec in `specs` in order to the value `v`."
  [v specs]
  (reduce (fn [acc spec]
            (case (:redact/kind spec)
              :pattern (redact-pattern acc spec)
              :key-path (redact-key-path acc spec)))
          v
          specs))

(defn redact
  "Redact `value` (EDN-safe) against `specs`, returning a new value with
  every secret-bearing sub-value replaced by \"[REDACTED]\" and all other
  values touched. Idempotent: redacting an already-redacted value yields
  the same value. Keys are never rewritten, only values. `specs` is
  validated (see validate-specs!)."
  [value specs]
  (validate-specs! specs)
  (apply-specs value specs))

(defn redact-event
  "Return an event map with ONLY its :metadata redacted against `specs`.
  :payload-ref, :cause, ids, :event/type and every other key pass through
  byte-identical; :metadata itself, when present, is replaced by its
  redacted counterpart (an absent :metadata is left absent).

  The write path MUST call this BEFORE append-event! so secrets embedded
  in metadata never reach persistent storage. `specs` is validated."
  [event specs]
  (validate-specs! specs)
  (if (contains? event :metadata)
    (assoc event :metadata (apply-specs (:metadata event) specs))
    event))

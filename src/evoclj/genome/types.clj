(ns evoclj.genome.types
  "ID conventions and validated value helpers for EvoCLJ.

  Content-addressed IDs — genome, resolution, artifact — are canonical
  strings of the form \"sha256:<64 lowercase hex>\". Session and intent
  IDs are UUIDs, accepted either as #uuid values or their canonical
  string representation. IDs are kept as plain values in validated maps
  so they stay easy to persist and print; records and protocols are
  deliberately deferred."
  (:require [evoclj.kernel.error :as err]))

(def ^:private sha256-id-re #"^sha256:[0-9a-f]{64}$")

(defn- sha256-id? [x]
  (and (string? x) (boolean (re-matches sha256-id-re x))))

(defn genome-id?
  "True when x is a canonical \"sha256:<64 hex>\" Genome ID string."
  [x]
  (sha256-id? x))

(defn resolution-id?
  "True when x is a canonical \"sha256:<64 hex>\" Resolution ID string."
  [x]
  (sha256-id? x))

(defn artifact-id?
  "True when x is a canonical \"sha256:<64 hex>\" Artifact ID string."
  [x]
  (sha256-id? x))

(defn- uuid-string? [x]
  (boolean (try (java.util.UUID/fromString x)
                (catch Exception _ nil))))

(defn session-id?
  "True when x is a UUID value or a valid UUID string."
  [x]
  (or (uuid? x)
      (and (string? x) (uuid-string? x))))

(defn intent-id?
  "True when x is a UUID value or a valid UUID string."
  [x]
  (session-id? x))

(defn- invalid-id!
  "Throw a typed :id/invalid error; the raw value is kept in ex-data and
  sanitized by evoclj.kernel.error/error-data at any serialization
  boundary."
  [kind expected x]
  (throw (err/error :id/invalid
                    (str "invalid " (name kind) ": expected " expected)
                    {:id/kind kind :value x})))

(defn genome-id
  "Validate x as a Genome ID, returning the canonical string unchanged."
  [x]
  (if (genome-id? x)
    x
    (invalid-id! :genome/id "sha256:<64 hex> string" x)))

(defn resolution-id
  "Validate x as a Resolution ID, returning the canonical string unchanged."
  [x]
  (if (resolution-id? x)
    x
    (invalid-id! :resolution/id "sha256:<64 hex> string" x)))

(defn artifact-id
  "Validate x as an Artifact ID, returning the canonical string unchanged."
  [x]
  (if (artifact-id? x)
    x
    (invalid-id! :artifact/id "sha256:<64 hex> string" x)))

(defn- uuid-value
  "Validate x as a UUID, returning the #uuid value (strings are
  canonicalized through java.util.UUID/fromString)."
  [kind x]
  (cond
    (uuid? x) x
    (and (string? x) (uuid-string? x)) (java.util.UUID/fromString x)
    :else (invalid-id! kind "UUID value or canonical UUID string" x)))

(defn session-id
  "Validate x as a Session ID, returning it as a #uuid value."
  [x]
  (uuid-value :session/id x))

(defn intent-id
  "Validate x as an Intent ID, returning it as a #uuid value."
  [x]
  (uuid-value :intent/id x))

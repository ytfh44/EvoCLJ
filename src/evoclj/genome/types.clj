(ns evoclj.genome.types
  "ID conventions and validated value helpers for EvoCLJ.

  Content-addressed IDs — genome, resolution, artifact, code-image,
  deployment — are canonical strings of the form \"sha256:<64 lowercase hex>\".
  Session, intent, and execution IDs are UUIDs, accepted either as #uuid
  values or their canonical string representation. IDs are kept as plain
  values in validated maps so they stay easy to persist and print; records
  and protocols are deliberately deferred.

  I1 identity split:
    CodeImageId  = H(kernel ABI, Genome, Resolution) — pure code identity
    DeploymentId = H(CodeImage, bindings, authority) — bound deployment
    ExecutionId  = UUID per activation — distinct execution identity
  PhenotypeId legacy alias is removed (one-time break compat)."
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

(defn code-id?
  "True when x is a canonical \"sha256:<64 hex>\" CodeImage ID string (I1)."
  [x]
  (sha256-id? x))

(defn code-image-id?
  "Alias for code-id? — CodeImageId is H(kernel ABI, Genome, Resolution)."
  [x]
  (code-id? x))

(defn deployment-id?
  "True when x is a canonical \"sha256:<64 hex>\" Deployment ID string (I1)."
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

(defn execution-id?
  "True when x is a UUID value or a valid UUID string (I1 ExecutionId)."
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

(defn code-id
  "Validate x as a CodeImage ID, returning the canonical string unchanged."
  [x]
  (if (code-id? x)
    x
    (invalid-id! :code/id "sha256:<64 hex> string" x)))

(defn code-image-id
  "Alias for code-id — validate CodeImageId."
  [x]
  (code-id x))

(defn deployment-id
  "Validate x as a Deployment ID, returning the canonical string unchanged."
  [x]
  (if (deployment-id? x)
    x
    (invalid-id! :deployment/id "sha256:<64 hex> string" x)))
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

(defn execution-id
  "Validate x as an Execution ID, returning it as a #uuid value (I1)."
  [x]
  (uuid-value :execution/id x))

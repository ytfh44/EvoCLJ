(ns evoclj.context.compression.provenance
  "Traceability backbone for the context-compression subsystem.
  Every claim in a compression envelope's RESIDUE section must be traceable
  to a source. This namespace provides the source/claim data structures,
  validation, traceability verification, EDN serialization, and reporting."
  (:require [evoclj.context.compression.error :as err]))

;; ----------------------------------------------------------------------
;; Source kinds
;; ----------------------------------------------------------------------

(def ^:private valid-source-kinds
  #{:user-message
    :tool-output
    :decision
    :observation
    :compression-output})

;; ----------------------------------------------------------------------
;; Source construction and validation
;; ----------------------------------------------------------------------

(defn- validate-source-kind!
  [kind]
  (when-not (contains? valid-source-kinds kind)
    (throw (err/error
            :context/provenance-invalid
            (str ":context/provenance-invalid — Invalid source kind: " kind ". Must be one of: " valid-source-kinds)
            {:source/kind kind}))))

(defn- validate-non-empty-string!
  [v field-name]
  (when-not (string? v)
    (throw (err/error
            :context/provenance-invalid
            (str ":context/provenance-invalid — " field-name " must be a non-empty string")
            {:field field-name :value v})))
  (when (empty? v)
    (throw (err/error
            :context/provenance-invalid
            (str ":context/provenance-invalid — " field-name " must be a non-empty string")
            {:field field-name :value v}))))

(defn make-source
  "Construct and validate a source map.

   `kind` must be one of: :user-message, :tool-output, :decision,
   :observation, :compression-output.

   `where` and `summary` must be non-empty strings.

   Optional kwargs:
   - :turn <int or nil>
   - :hash <string or nil>
   - :id <keyword or string>

   Throws `:context/provenance-invalid` when validation fails."
  [kind where summary & {:keys [turn hash id]}]
  (validate-source-kind! kind)
  (validate-non-empty-string! where "where")
  (validate-non-empty-string! summary "summary")
  (let [source {:source/kind kind
                :source/where where
                :source/summary summary}]
    (cond-> source
      (some? turn)  (assoc :source/turn turn)
      (some? hash)  (assoc :source/hash hash)
      (some? id)    (assoc :source/id id))))

;; ----------------------------------------------------------------------
;; Claim construction and validation
;; ----------------------------------------------------------------------

(defn- validate-confidence!
  [confidence]
  (when (some? confidence)
    (when-not (float? confidence)
      (throw (err/error
              :context/provenance-invalid
              (str ":context/provenance-invalid — confidence must be a float, got: " (type confidence))
              {:confidence confidence})))
    (when-not (<= 0.0 confidence 1.0)
      (throw (err/error
              :context/provenance-invalid
              (str ":context/provenance-invalid — confidence must be in [0, 1], got: " confidence)
              {:confidence confidence})))))

(defn make-claim
  "Construct and validate a claim map.

   `text` must be a non-empty string.

   `source` may be either a raw {:source/kind ...} map (which will be
   validated via make-source) or an already-constructed source map
   (which is assumed valid).

   Optional kwargs:
   - :id <int>
   - :confidence <float in [0,1] or nil>

   Throws `:context/provenance-invalid` when validation fails."
  [text source & {:keys [id confidence]}]
  (when-not (string? text)
    (throw (err/error
            :context/provenance-invalid
            (str ":context/provenance-invalid — claim text must be a non-empty string, got: " (type text))
            {:text text})))
  (when (empty? text)
    (throw (err/error
            :context/provenance-invalid
            ":context/provenance-invalid — claim text must be a non-empty string"
            {:text text})))
  (when-not (map? source)
    (throw (err/error
            :context/provenance-invalid
            (str ":context/provenance-invalid — source must be a map, got: " (type source))
            {:source source})))
  (validate-confidence! confidence)
  (let [validated-source (if (contains? source :source/where)
                           source
                           (let [{:source/keys [kind where summary]} source]
                             (make-source kind where summary
                                          :turn  (:turn source)
                                          :hash   (:hash source)
                                          :id     (:id source))))]
    (cond-> {:claim/id      id
             :claim/text    text
             :claim/source  validated-source}
      (some? confidence) (assoc :claim/confidence confidence))))

;; ----------------------------------------------------------------------
;; Claim accessors
;; ----------------------------------------------------------------------

(defn claim-source
  "Return the source map from a claim."
  [claim]
  (:claim/source claim))

;; ----------------------------------------------------------------------
;; Traceability verification
;; ----------------------------------------------------------------------

(defn- source-matches?
  "Return true when `candidate` matches `known`:
   - by :source/kind + :source/where equality, or
   - by :source/hash equality when candidate has a hash."
  [candidate known]
  (or (and (:source/hash candidate)
           (= (:source/hash candidate) (:source/hash known)))
      (and (= (:source/kind candidate) (:source/kind known))
           (= (:source/where candidate) (:source/where known)))))

(defn trace-claim
  "Verify that `claim` is traceable against `known-sources`.

   A claim is traced when its source matches one of the known sources.
   Match is either kind+where equality, or hash equality when the
   claim's source carries a hash.

   Returns the claim unchanged when traceable.

   Throws `:context/provenance-invalid` with :claim/id, :claim/text,
   and reason :untraceable when no match is found."
  [claim known-sources]
  {:pre [(map? claim) (sequential? known-sources)]}
  (let [source (:claim/source claim)]
    (if (some #(source-matches? source %) known-sources)
      claim
      (throw (err/error
              :context/provenance-invalid
              (str ":context/provenance-invalid — Claim " (:claim/id claim) " is untraceable: "
                   "source " (:source/kind source)
                   " where " (:source/where source)
                   " does not match any known source")
              {:claim/id   (:claim/id claim)
               :claim/text (:claim/text claim)
               :reason     :untraceable})))))

(defn trace-claims
  "Trace every claim in `claims` against `known-sources`.

   Returns a vector of the traced claims, or throws on the first
   untraceable claim (see trace-claim)."
  [claims known-sources]
  {:pre [(sequential? claims)]}
  (reduce (fn [acc claim]
            (conj acc (trace-claim claim known-sources)))
          []
          claims))

;; ----------------------------------------------------------------------
;; EDN round-trip
;; ----------------------------------------------------------------------

(defn source->edn
  "Serialize a source to an EDN string via pr-str."
  [source]
  {:pre [(map? source)
         (contains? source :source/kind)]}
  (pr-str source))

(defn edn->source
  "Read a source from an EDN string, validating required fields.
   Throws `:context/provenance-invalid` on malformed input."
  [s]
  {:pre [(string? s)]}
  (let [parsed (clojure.edn/read-string s)]
    {:pre [(map? parsed)
           (contains? parsed :source/kind)
           (contains? parsed :source/where)
           (contains? parsed :source/summary)]}
    parsed))

(defn claim->edn
  "Serialize a claim to an EDN string via pr-str."
  [claim]
  {:pre [(map? claim)
         (contains? claim :claim/text)
         (contains? claim :claim/source)]}
  (pr-str claim))

(defn edn->claim
  "Read a claim from an EDN string, validating required fields.
   Throws `:context/provenance-invalid` on malformed input."
  [s]
  {:pre [(string? s)]}
  (let [parsed (clojure.edn/read-string s)]
    {:pre [(map? parsed)
           (contains? parsed :claim/text)
           (contains? parsed :claim/source)]}
    parsed))

;; ----------------------------------------------------------------------
;; Provenance reporting
;; ----------------------------------------------------------------------

(defn ^:private traced?
  "A claim is traced when its source is non-nil and has a non-empty
   :source/where."
  [claim]
  (let [src (:claim/source claim)]
    (and (some? src)
         (not (empty? (:source/where src))))))

(defn provenance-report
  "Produce a report map from a sequence of claims.

   Report structure:
   {:provenance/total           <int>
    :provenance/traced           <int>
    :provenance/untraceable      <int>
    :provenance/by-kind          {:user-message <n>
                                   :tool-output  <n>
                                   :decision     <n>
                                   :observation  <n>
                                   :compression-output <n>}
    :provenance/untraceable-ids  [<claim-id> ...]}"
  [claims]
  {:pre [(sequential? claims)]}
  (let [total     (count claims)
        traced    (filter traced? claims)
        untraced  (remove traced? claims)
        traced-n  (count traced)
        untraced-n (count untraced)]
    {:provenance/total      total
     :provenance/traced     traced-n
     :provenance/untraceable untraced-n
     :provenance/by-kind
     (reduce (fn [acc c]
               (let [k (:source/kind (:claim/source c))]
                 (update acc k (fnil inc 0))))
             {:user-message 0
              :tool-output   0
              :decision      0
              :observation   0
              :compression-output 0}
             claims)
     :provenance/untraceable-ids
     (vec (map :claim/id untraced))}))

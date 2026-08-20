(ns evoclj.mcp.json-schema
  "Minimal, bounded native JSON-Schema validator (subset).

   `validate` returns true iff `value` satisfies `schema`, where
   `schema` is a plain Clojure map with STRING keys (the shape produced
   by evoclj.mcp.client/java-schema->clj).

   Supported keywords: type (object/string/integer/number/boolean/
   array/null), properties, required, additionalProperties (bool), items,
   enum, const, minimum/maximum, minLength/maxLength, nullable.

   Unsupported keywords (oneOf/anyOf/allOf/$ref/$defs/pattern/format/
   dependentSchemas/unevaluatedProperties/...) are intentionally ignored
   and treated as unconstrained. This is PERMISSIVE ON THE UNSUPPORTED
   PART ONLY: supported constraints such as a wrong `type` or a missing
   `required` property still reject clearly invalid data.

   This is not a full 2020-12 validator; its purpose is to remove the
   `:any` fail-open gap, not to be spec-complete.")

(declare validate)

(defn- validate-object
  [schema value]
  (let [props (get schema "properties" {})
        required (set (get schema "required" []))
        additional? (get schema "additionalProperties" true)]
    (and (every? #(contains? value %) required)
         (every? (fn [[k subschema]]
                   (or (not (contains? value k))
                       (validate subschema (get value k))))
                 props)
         (if (false? additional?)
           (every? #(contains? props %) (keys value))
           true))))

(defn- validate-string
  [schema value]
  (let [min-l (get schema "minLength")
        max-l (get schema "maxLength")
        len (count (str value))]
    (and (if (some? min-l) (>= len min-l) true)
         (if (some? max-l) (<= len max-l) true))))

(defn- validate-number
  [schema value]
  (let [min-v (get schema "minimum")
        max-v (get schema "maximum")]
    (and (if (some? min-v) (>= (double value) (double min-v)) true)
         (if (some? max-v) (<= (double value) (double max-v)) true))))

(defn- validate-array
  [schema value]
  (let [items (get schema "items")]
    (if (map? items)
      (every? #(validate items %) (vec value))
      true)))

(defn validate
  "Return true iff `value` satisfies the string-keyed JSON `schema`.
   Unsupported keywords are ignored (permissive on that part only)."
  [schema value]
  (let [t (get schema "type")
        nullable? (true? (get schema "nullable"))]
    (cond
      (nil? value)
      (or nullable? (= "null" t))

      (contains? schema "const")
      (= value (get schema "const"))

      (contains? schema "enum")
      (some #(= value %) (get schema "enum"))

      :else
      (let [type-ok?
            (case t
              "object"  (map? value)
              "string"  (string? value)
              "boolean" (boolean? value)
              "array"   (or (vector? value) (seq? value) (set? value))
              "null"    (nil? value)
              "integer" (and (number? value) (integer? value))
              "number"  (number? value)
              ;; unknown or absent type -> permissive on type only
              true)]
        (and type-ok?
             (case t
               "object"  (validate-object schema value)
               "string"  (validate-string schema value)
               "array"   (validate-array schema value)
               "integer" (validate-number schema value)
               "number"  (validate-number schema value)
               true))))))

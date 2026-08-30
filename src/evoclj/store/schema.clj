(ns evoclj.store.schema
  "Central Malli schema registry for program descriptor phantom checks (PLT3) and topology compositional typing (PLT4).

  Definition > validation: only registered schema keywords are representable.
  compile-program-descriptor resolves :input-schema / :output-schema via this
  registry and fails closed on phantom keywords (e.g. :schema/unicorn).
  PLT4 extends the same closed registry to topology nodes: each node carries
  :input-schema/:output-schema keywords that resolve to Malli values, and edge
  typing checks output(A) <: input(B) via malli. The compiled descriptor/node
  carries the resolved Malli schema values, not just the keyword, so a phantom
  keyword or a mismatched edge is unrepresentable in compiled form.

  Global Constraint 22: registry values are plain EDN Malli schemas round-tripping
  through pr-str / clojure.edn read-string."
  (:require [evoclj.kernel.error :as err]
            [malli.core :as m]))

;; --- canonical registered schemas ------------------------------------------

(def route-input-schema
  "Malli schema for :schema/route-input — the seed routing program input.
  EDN-serializable (uses :keyword :string :any, not fn objects) so compiled
  descriptors round-trip via pr-str / clojure.edn/read-string (Global Constraint 22)."
  [:map {:closed false}
   [:op {:optional true} :keyword]
   [:text {:optional true} :string]
   [:value {:optional true} :any]])

(def intent-or-route-schema
  "Malli schema for :schema/intent-or-route — a routing decision map
  carrying a single :action intent (tool-call or finish).
  EDN-serializable (Global Constraint 22)."
  [:map {:closed false}
   [:action [:map
             [:intent/type :keyword]
             [:payload :map]]]])

;; PLT4 primitive schemas for compositional typing
;; Each is a plain Malli value (Global Constraint 22) and enables
;; edge-type tests like A:JSON->String -> B:Int->Intent mismatch.

(def string-schema
  "Malli schema for :schema/string — primitive string."
  :string)

(def int-schema
  "Malli schema for :schema/int — primitive int."
  :int)

(def any-schema
  "Malli schema for :schema/any — top type, accepts anything."
  :any)

(def number-schema
  "Malli schema for :schema/number."
  :double)

(def boolean-schema
  "Malli schema for :schema/boolean."
  :boolean)

(def json-schema
  "Malli schema for :schema/json — generic JSON value (closed false map)."
  [:map {:closed false}
   [:data {:optional true} :any]])

(def ^:private registry
  "Closed registry: keyword -> Malli schema value.
  Definition > validation: only these keywords are representable.
  Phantom keywords (e.g. :schema/unicorn) are absent by definition."
  {:schema/route-input route-input-schema
   :schema/intent-or-route intent-or-route-schema
   :schema/string string-schema
   :schema/int int-schema
   :schema/any any-schema
   :schema/number number-schema
   :schema/boolean boolean-schema
   :schema/json json-schema})

;; Validate registry itself at load time: every value must be a valid Malli schema
(doseq [[k v] registry]
  (try (m/schema v)
       (catch Exception e
         (throw (ex-info (str "schema registry contains invalid Malli schema for " k)
                         {:schema/kw k :value v} e)))))

(defn schema-registry
  "Return the closed schema registry map: keyword -> Malli schema value.
  The map itself is EDN-serializable (Global Constraint 22)."
  []
  registry)

(defn registered?
  "True when kw is a registered schema keyword."
  [kw]
  (contains? registry kw))

(defn resolve-schema
  "Resolve kw via the closed registry. Returns the Malli schema value
  when registered, nil otherwise (caller decides failure policy).
  Never throws for unknown keywords — use resolve-schema! for fail-closed."
  [kw]
  (get registry kw))

(defn resolve-schema!
  "Resolve kw via the closed registry, fail-closed.
  Returns the Malli schema value when registered.
  Throws :program/invalid with :reason :unknown-schema when kw is not
  registered, so a phantom keyword (e.g. :schema/unicorn) is unrepresentable
  at compile time (Definition > validation)."
  [kw]
  (if-let [s (get registry kw)]
    s
    (throw (err/error :program/invalid
                      (str "unknown schema keyword " kw " — not registered in schema registry")
                      {:reason :unknown-schema
                       :schema kw
                       :registered (vec (sort (keys registry)))}))))

(defn ensure-valid-schema!
  "Validate that s is a valid Malli schema value (via m/schema).
  Throws :program/schema-invalid when s is not a valid Malli schema."
  [s]
  (try (m/schema s)
       s
       (catch Exception _
         (throw (err/error :program/schema-invalid
                           "resolved schema is not a valid Malli schema"
                           {:reason :invalid-schema
                            :value (err/sanitize s)})))))

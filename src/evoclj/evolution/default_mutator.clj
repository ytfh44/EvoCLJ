(ns evoclj.evolution.default-mutator
  "The built-in default Mutator adapter (S2-1).

  When no host injects a custom mutator, this adapter proposes a single
  trivial mutation so that loop/cycle have something to evaluate. The
  mutation is a no-op form swap in the route program: it replaces the
  seed's `case` with an identical `case`, so the compiled candidate is
  byte-identical to the parent. This guarantees the loop/cycle pipeline
  runs end-to-end without depending on the :demo profile or external
  injection.

  The adapter is stateless, deterministic, holds no store handle
  (Global Constraint 11), and calls no provider (Global Constraint 8)."

  (:require [evoclj.evolution.core :refer [Mutator]]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)))

;; --- the trivial no-op template ----------------------------------------------

(defn- route-case-form
  "The seed route's routing `case` — identical to the parent's form."
  []
  (list 'case 'op
        :echo {:action (list 'tool-call-intent :fixture/echo
                             {:text (list 'get 'input :text)})}
        :finish {:action (list 'finish-intent (list 'get 'input :value))}
        {:action (list 'finish-intent 'input)}))

(def default-template
  "The single trivial template: a no-op swap of the route program's
  `case` form. The compiled candidate is byte-identical to the parent."
  {:template/id :default/noop
   :form (route-case-form)
   :expected-effect {:primary-metric :task/success :direction :neutral}})

(defn- default-hypothesis-id
  "Deterministic name-based hypothesis id for the default adapter
   (mirrors the demo mutator's `UUID/nameUUIDFromBytes` convention).
   complete-mutation! gates on `(uuid? hypothesis-id)`, so a bare
   string id would fail the cycle as :evolution/mutator-invalid; the
   name-based UUID keeps proposals deterministic and satisfies that gate."
  []
  (java.util.UUID/nameUUIDFromBytes
   (.getBytes "default/noop" StandardCharsets/UTF_8)))

;; --- the adapter --------------------------------------------------------------

(defrecord DefaultMutator []
  Mutator
  (propose-mutations [_ context]
    ;; The closed context validation mirrors the demo adapter.
    (when-not (map? context)
      (throw (err/error :mutation/context-invalid
                        "the Mutator context must be a map"
                        {:value (err/sanitize context)})))
    (let [parent (:parent-genome context)]
      (when-not (and (map? parent) (map? (:files parent)))
        (throw (err/error :mutation/context-invalid
                          "the Mutator context must carry a :parent-genome map with a :files map"
                          {:value (err/sanitize context)})))
    ;; Propose exactly one trivial mutation.
    [{:risk :program
      :hypothesis/id (default-hypothesis-id)
      :ops [{:op :replace-form
             :file "programs/route.clj"
             :selector ['case]
             :expect/hash (get-in parent [:files "programs/route.clj" :digest])
             :form (:form default-template)}]
      :expected-effect (:expected-effect default-template)}])))

(defn default-mutator
  "Construct the built-in default Mutator adapter (S2-1). Zero
  configuration: the adapter is stateless and deterministic."
  []
  (->DefaultMutator))

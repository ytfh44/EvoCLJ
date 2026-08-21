(ns evoclj.context
  "EffectiveContext — top-level Context subsystem.

  EffectiveContext = Materialize(History, ActiveBindings, CatalogProjection, HostPolicy)

  History is the compressed conversation history produced by the compression
  subsystem (evoclj.context.compression.*). History -> compressed History
  only; compression never stores Skill bindings.

  ActiveBindings are ContextBindings pinned to exact revisions via CAS.
  CatalogProjection is the current catalog view (for discovery, not for
  materialization). HostPolicy filters which bindings are injected.

  The materializer reads exact content from each binding's immutable
  artifact/tree via CAS, not from the current registry. So if a binding
  points to revision A and the catalog has moved to B, materialization
  still returns A.

  This namespace re-exports the main Context APIs for convenience:
   - offer/make-offer, catalog-projection
   - binding/make-binding, activate!, list-active
   - segment/make-segment
   - materializer/materialize, effective-context"
  (:require [evoclj.context.offer :as offer]
            [evoclj.context.binding :as binding]
            [evoclj.context.segment :as segment]
            [evoclj.context.policy :as policy]
            [evoclj.context.materializer :as materializer]))

;; Re-export offer
(def make-offer offer/make-offer)
(def offer? offer/offer?)
(def catalog-projection offer/catalog-projection)
(def current-offer offer/current-offer)

;; Re-export binding
(def make-binding binding/make-binding)
(def binding? binding/binding?)
(def binding-from-offer binding/binding-from-offer)
(def validate-binding binding/validate-binding)
(def create-binding-store binding/create-store)
(def activate! binding/activate!)
(def deactivate! binding/deactivate!)
(def get-binding binding/get-binding)
(def list-active binding/list-active)
(def get-by-id binding/get-by-id)
(def clear-bindings! binding/clear!)

;; Re-export segment
(def make-segment segment/make-segment)
(def segment? segment/segment?)
(def segment-from-binding segment/segment-from-binding)

;; Re-export policy
(def allowed? policy/allowed?)
(def filter-bindings policy/filter-bindings)

;; Re-export materializer
(def materialize materializer/materialize)
(def effective-context materializer/effective-context)

;; ---------------------------------------------------------------------------
;; Convenience: EffectiveContext materialization that ensures history is
;; compressed independently of bindings.
;; ---------------------------------------------------------------------------

(defn effective-context-string
  "Build the effective context string for a model request.

  Takes history (already compressed via compression loop), active bindings,
  catalog, policy, and CAS, and returns the injected context string.
  See materializer/materialize for full return map."
  [history bindings catalog policy cas]
  (:effective/context-string (materialize {:history history
                                           :bindings bindings
                                           :catalog catalog
                                           :policy policy
                                           :cas cas})))

(ns evoclj.support.failpoint
  "Centralized fault-injection seams for the publication paths (WO-T2).

  Same seam shape as the promotion system's optional `:failpoint` fn
  (evoclj.promotion.promote/evoclj.promotion.rollback), generalized to
  a data-driven multi-stage form: a publication function takes an opts
  map that may carry

    :failpoints — {<stage-kw> (fn [] ...), ...}

  Each call site invokes (trigger! opts :stage-kw) at a fixed point in
  the path. When a hook is registered for that stage it is called with
  zero arguments, for effect only; its return value is discarded. The
  seam is purely data-driven: no global atom, no dynamic binding, so
  two concurrent callers with different opts maps trigger their hooks
  independently.

  Production default: opts without :failpoints (or without the stage
  key) makes trigger! an immediate no-op — zero behavior change.

  A hook may throw ANY Throwable. The helper itself never catches,
  wraps or suppresses; the exception propagates to the caller of the
  enclosing publication function. One path is documented to surface
  the fault differently: evoclj.environment.registry/refresh!
  intentionally wraps its whole pipeline in the registry's own
  degradation catch, which converts any Throwable into
  {:status :error :error e :error-data ed} and marks the registry
  :degraded (recorded in :last-refresh-error). An injected fault there
  reaches the caller through that documented channel — reported and
  recorded, never silently swallowed. WO-T2 does not modify that
  catch. In evoclj.store.binding and evoclj.skill.adapter paths the
  seams sit outside every existing catch, so a hook throw reaches the
  caller as a thrown exception.

  Injection points (stage -> paths):
  - evoclj.store.binding/activate!   :after-db-insert
                                     :after-publish-runtime
                                     :before-event-append
                                     :after-event-append
  - evoclj.store.binding/reload!     same four stages as activate!
                                     (:after-db-insert fires after the
                                     durable row UPDATE commits)
  - evoclj.store.binding/deactivate! :after-unpublish
  - evoclj.environment.registry/refresh! — opts is the trailing arg of
                                     the new arity ([registry source-id
                                     opts]); stages:
                                     :after-snapshot :after-validate
                                     :after-project :after-bundle-publish
                                     :mid-publish
  - evoclj.skill.adapter/derive-and-publish! — opts is the trailing
                                     arg of the new arity ([skill-dir
                                     cas registry mode opts]); stages:
                                     :after-snapshot-tree :after-parse
                                     :after-bundle-publish

  Legal stages — exactly these 12 keywords (machine-checkable list;
  this block is flush-left: its lines are exactly those docstring
  lines that START at column zero with a keyword token):
:after-db-insert :after-publish-runtime :before-event-append
:after-event-append :after-unpublish :after-snapshot :after-validate
:after-project :mid-publish :after-snapshot-tree :after-parse :after-bundle-publish"
  (:require [clojure.string :as str]))

(defn trigger!
  "Fire the optional fault-injection hook registered in opts for
  stage-kw.

  opts — the publication call's opts map (may be nil)
  stage-kw — one of the legal stages listed in the namespace
             docstring

  No-op (returns nil) unless opts carries {:failpoints {stage-kw f}}.
  Stage keys not present in the map — including misspellings of legal
  stages — are silently ignored. A nil value is treated as absent; a
  non-fn truthy value throws at the seam (arity error).
  Calls f with zero arguments, discards its return value and returns
  nil. Any Throwable thrown by f propagates unchanged to the caller —
  this helper performs no catching of its own."
  [opts stage-kw]
  (when-some [fps (:failpoints opts)]
    (when-some [f (get fps stage-kw)]
      (do (f) nil))))

(defn docstring-stages
  "Return the sorted set of legal stage keywords declared in this
  namespace's docstring (the machine-checkable block: lines whose
  first non-whitespace character is a colon). Used by the contract
  test to keep the docstring list and the actual trigger! call sites
  in lockstep."
  []
  (let [doc (-> (the-ns 'evoclj.support.failpoint) meta :doc)]
    (into (sorted-set)
          (comp (filter #(re-find #"^:" %))
                (mapcat #(re-seq #":[a-zA-Z0-9*+!?_<>./'-]+" %)))
          (str/split-lines doc))))

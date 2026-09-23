(ns evoclj.kernel.store-contract
  "The executor-store trust boundary RULE shared by the evolution and
  runtime entry points: the executor :stores map must be a map carrying
  :sqlite and :cas, checked in that order, failing with :not-a-map,
  :sqlite-missing, and :cas-missing respectively.

  What is SHARED is the rule (which shapes fail, in which order, under
  which reason keyword) plus the canonical message/reason-data text for
  the sites that report the contract verbatim. What stays OWNED BY THE
  SITE is its error contract: `error-fn` is the site's own closure,
  called as (error-fn reason store), that throws the site's typed
  failure — its :error/type, and its message/data when they deviate
  (the A′ site in evoclj.evolution.core carries {} data and renames
  :not-a-map to :store-invalid; the diagnostic bundle site merges the
  three checks into one message).

  NOT for the store-HANDLE contracts (CandidateStore /
  EnrichmentStore / MemoryStore): those validate a constructed handle,
  a genuinely different contract, and keep their own validation."
  (:require [evoclj.kernel.error :as err]))

(def messages
  "The canonical executor-store boundary messages, keyed by the reason
  validate-executor-stores! reports."
  {:not-a-map "store must be the executor :stores map {:sqlite ... :cas ...}"
   :sqlite-missing "store must carry the :sqlite handle"
   :cas-missing "store must carry the :cas handle"})

(defn error-data
  "The canonical executor-store boundary error data for `reason`: the
  :not-a-map branch carries the sanitized value, the missing-handle
  branches carry only the reason."
  [reason store]
  (if (= :not-a-map reason)
    {:reason :not-a-map :value (err/sanitize store)}
    {:reason reason}))

(defn validate-executor-stores!
  "Validate the executor store map against the shared rule: a map
  carrying :sqlite and :cas. Calls (error-fn reason store) on the first
  violation — the site's closure throws its own typed error — and
  returns `store` unchanged when valid.

  `reason` is one of :not-a-map, :sqlite-missing, :cas-missing."
  [error-fn store]
  (when-not (map? store)
    (throw (error-fn :not-a-map store)))
  (when-not (contains? store :sqlite)
    (throw (error-fn :sqlite-missing store)))
  (when-not (contains? store :cas)
    (throw (error-fn :cas-missing store)))
  store)

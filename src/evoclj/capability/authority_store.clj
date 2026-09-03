(ns evoclj.capability.authority-store
  "AuthorityStore: the explicit durable authority for the CapabilityLease
  lifecycle (P1 structural fix).

  Before this abstraction, `evoclj.capability.mint` reached the persistent
  capabilities table through `requiring-resolve` wrapped in a swallow:
  when the persistence fn could not be resolved the durable write was
  silently skipped while the in-memory LeaseRegistry (versioned cache) was
  still updated — the exact opposite of the \"DB is truth\" invariant.

  `AuthorityStore` makes the durable authority a concrete, fail-closed
  participant: `mint` funnels every write through an `AuthorityStore`
  instance, so an unavailable/invalid DB *throws* (typed
  `:capability/authority-unavailable`) and the memory cache is only updated
  after the durable write succeeds.

  Implementations:
    - [[ProductionAuthorityStore]] wraps a raw sqlite db spec/handle and
      delegates to `evoclj.store.capability-store`. Every operation throws
      on DB error (fail-closed); nothing is swallowed.
    - [[MemoryAuthorityStore]] wraps an atom and is the TYPE-visible
      memory-only authority used by unit tests that want no DB. It makes
      the memory-only path explicit rather than an arity side-effect."
  (:require [evoclj.kernel.error :as err]
            [evoclj.store.capability-store :as cap-store]))

;; ---------------------------------------------------------------------------
;; Protocol
;; ---------------------------------------------------------------------------

(defprotocol AuthorityStore
  (insert-lease! [this lease]
    "Durably record `lease` (a sealed CapabilityLease). Throws on failure
    (fail-closed); the caller must NOT update the cache on failure. Returns
    the recorded lease.")
  (revoke! [this cap-id]
    "Durably revoke `cap-id`. Idempotent. Throws on failure (fail-closed).
    Returns the revoked row or nil when no such row exists.")
  (hydrate! [this registry]
    "Load currently-active rows into `registry` (an atom LeaseRegistry).
    Returns the count of hydrated entries. Throws on failure.")
  (active-by-principal [this principal]
    "Return the authority's active (non-revoked) records for `principal`
    (a tagged principal map). Read-only."))

;; ---------------------------------------------------------------------------
;; Fail-closed helper
;; ---------------------------------------------------------------------------

(defn- durable!
  "Invoke `f`; translate any failure into a typed `:capability/authority-unavailable`
  (fail-closed). Already-typed `clojure.lang.ExceptionInfo` carrying an
  `:error/type` is rethrown unchanged so schema/validation errors are not
  masked."
  [f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (if (:error/type (ex-data e))
        (throw e)
        (throw (err/error :capability/authority-unavailable
                          "durable authority (DB) unavailable"
                          {:underlying (ex-message e)}))))
    (catch Exception e
      (throw (err/error :capability/authority-unavailable
                        (str "durable authority (DB) unavailable: " (ex-message e))
                        {})))))

;; ---------------------------------------------------------------------------
;; ProductionAuthorityStore — durable SQLite-backed authority
;; ---------------------------------------------------------------------------

(defn- principal->selector
  "Map a tagged principal map to the `list-capabilities` selector
  `{:principal-type … :principal-id …}`. Unknown principal types fall back
  to a type-only selector."
  [principal]
  (case (:principal/type principal)
    :session {:principal-type "session" :principal-id (str (:session/id principal))}
    :job {:principal-type "job" :principal-id (str (:job/id principal))}
    :eval {:principal-type "eval" :principal-id (str (:eval/id principal))}
    :operator {:principal-type "operator" :principal-id "operator"}
    {:principal-type (name (:principal/type principal))}))

(defrecord ProductionAuthorityStore [db]
  AuthorityStore
  (insert-lease! [_ lease]
    (durable! #(cap-store/insert-capability! db lease)))
  (revoke! [_ cap-id]
    (durable! #(cap-store/revoke-capability! db cap-id)))
  (hydrate! [_ registry]
    (durable! #(cap-store/hydrate-registry! db registry)))
  (active-by-principal [_ principal]
    (durable! #(cap-store/list-active-capabilities db (principal->selector principal)))))

(defn production-store
  "Wrap a raw sqlite db spec/handle into a [[ProductionAuthorityStore]]."
  [db]
  (->ProductionAuthorityStore db))

;; ---------------------------------------------------------------------------
;; MemoryAuthorityStore — type-visible memory-only authority (unit tests)
;; ---------------------------------------------------------------------------

(defrecord MemoryAuthorityStore [state]
  AuthorityStore
  (insert-lease! [_ lease]
    (swap! state assoc (:cap/id lease) lease)
    lease)
  (revoke! [_ cap-id]
    (swap! state dissoc cap-id)
    nil)
  (hydrate! [_ registry]
    (let [entries (into {} (map (fn [[id l]] [id {:lease l :revoked? false}]) @state))]
      (swap! registry (fn [m] (merge (select-keys m [:evoclj.capability.mint/version]) entries)))
      (count entries)))
  (active-by-principal [_ principal]
    (->> @state
         vals
         (filterv (fn [l] (= principal (:principal l)))))))

(defn memory-store
  "Return a [[MemoryAuthorityStore]] over `state` (an atom mapping `:cap/id`
  -> sealed lease), allocating a fresh atom when omitted."
  ([] (memory-store (atom {})))
  ([state] (->MemoryAuthorityStore state)))
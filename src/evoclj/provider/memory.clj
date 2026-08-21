(ns evoclj.provider.memory
  "The kernel-owned :memory/kv provider (feature R1): episodic memory as
  a broker-dispatched effect.

  Episodic memory is the per-session accumulation of what a phenotype
  has observed/decided — DISTINCT from procedural Genome changes
  (Global Constraint 10: episodic memory is expressed here, procedural
  evolution is the Genome). The provider closes over the SQLite spec
  (constructor-injected, exactly how evoclj.provider.fixture closes
  over :secret) so the store handle never crosses the protocol
  boundary: describe / normalize-request / execute-request! deal only
  in plain, validated Clojure data (Global Constraint 22).

  The provider is dispatched ONLY through the broker
  (evoclj.intent.dispatch/dispatch! — Global Constraint 8): evolvable
  code emits :intent/memory-read / :intent/memory-write, and the
  kernel's capability broker normalizes, authorizes, executes, and
  validates the output. A :memory-kv lease grants the exact key via
  {:kind :memory :id <key>} (evoclj.capability.lease/resource-covers?).

  SESSION SCOPING: every row is scoped to the requesting session id,
  which the provider threads into the normalized request args from the
  INTENT's own attribution (:session/id — Global Constraint 20). The
  dispatcher passes the full intent to normalize-request, so the
  provider reads :session/id there and carries it into the canonical
  request the lease is decided on and execute-request! consumes. Two
  sessions therefore CANNOT read each other's memory (feature R2:
  per-session isolation).

  The descriptor effect is :episodic (NOT :pure) and deliberately
  declares NO automatic retry: a memory WRITE is not idempotent, so
  the dispatcher must never auto-retry it (mirrors
  evoclj.provider.fixture/non-idempotent-provider's deliberate no-retry).
  Reads are safe but share the single descriptor, so no retry is
  declared at all."
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]
            [evoclj.sci.boundary :as boundary]
            [evoclj.store.sqlite :as sqlite]
            [malli.core :as m]))

;; --- descriptor -------------------------------------------------------------

(def ^:private memory-descriptor
  ;; Deliberately NO :retry block: automatic retries are allowed only
  ;; when a provider declares :retry {:safe? true} (component), so the
  ;; dispatcher must NEVER auto-retry this provider. Memory WRITES are
  ;; not idempotent; reads are safe but share the descriptor.
  {:tool/id :memory/kv
   :effect :episodic
   :input-schema [:map {:closed false}
                  [:memory/key keyword?]
                  [:memory/limit {:optional true} [:and :int [:fn (fn [x] (not (neg? x)))]]]
                  [:memory/content {:optional true} any?]]
   :output-schema [:map {:closed false}
                   [:memory/key keyword?]
                   [:memory/content {:optional true} any?]
                   [:memory/found {:optional true} boolean?]
                   [:memory/written {:optional true} boolean?]]
   :required-action :invoke})

;; --- payload / arg helpers ---------------------------------------------------

(defn- memory-args
  "Extract the :memory/* args from a memory intent payload. A payload
  that is not a map or carries no :memory/key is malformed and is
  rejected with :provider/input-invalid before anything is normalized."
  [intent]
  (let [payload (:payload intent)]
    (when-not (and (map? payload) (contains? payload :memory/key))
      (throw (err/error :provider/input-invalid
                        "memory intent payload must carry a :memory/key"
                        {:value (err/sanitize payload)})))
    payload))

(defn- validate-args!
  "Validate the user-facing args against the descriptor's
  :input-schema: EDN-safety first (Global Constraint 22), then the
  schema. Throws :provider/input-invalid on any failure."
  [descriptor args]
  (when-not (boundary/edn-safe? args)
    (throw (err/error :provider/input-invalid
                      "provider input must be plain EDN-safe data (Global Constraint 22)"
                      {:value (err/sanitize args)})))
  (when-not (m/validate (:input-schema descriptor) args)
    (throw (err/error :provider/input-invalid
                      "provider input failed input-schema validation"
                      {:value (err/sanitize args)
                       :explanation (err/sanitize (m/explain (:input-schema descriptor) args))}))))

(defn- expect-normalized!
  "Guard execute-request!: the authorized-request must be a canonical
  resource descriptor carrying the given key. A request that did not
  come through normalize-request is a kernel-side bug and fails closed
  (mirrors evoclj.provider.fixture/expect-normalized!).
  "
  [authorized-request key]
  (when-not (and (map? authorized-request)
                 (contains? authorized-request key))
    (throw (err/error :provider/request-invalid
                      "execute-request! requires a normalized request"
                      {:value (err/sanitize authorized-request)}))))

(defn- now-utc
  "The current instant as an ISO-8601 UTC string (created_at column)."
  []
  (str (java.time.Instant/now)))

;; --- the provider -----------------------------------------------------------

(defn memory-provider
  "Build the kernel-owned :memory/kv provider (feature R1).

  Required opts:

  - :store — the SQLite db spec (string path or java.jdbc spec map) the
    provider CLOSES OVER. The store handle never crosses the protocol
    boundary: only describe / normalize-request / execute-request! data
    do. The schema (or at least the ephemeral_memory table) is assumed
    migrated before dispatch (the host runs evoclj.store.migrate/migrate!
    at startup).

  Optional opts:

  - :execution-count — an atom bumped once per execute-request! call, so
    tests can assert when the provider REALLY ran: a denied request must
    never bump it (component Step 2).

  normalize-request turns an :intent/memory-read payload
  {:memory/key k :memory/limit n?} into the canonical resource
  descriptor {:tool/id :memory/kv :resource {:kind :memory :id <k>}
  :args {:memory/key k :memory/limit n :memory/op :read}} and an
  :intent/memory-write payload {:memory/key k :memory/content v} into
  {:tool/id :memory/kv :resource {:kind :memory :id <k>}
  :args {... :memory/op :write}} — the canonical resource kind :memory
  with the EXACT key id is what the capability lease grants, and the
  requesting :session/id is threaded into :args so writes are
  session-scoped.

  execute-request! performs the read or write and returns the plain
  result VALUE, which the broker validates against :output-schema."
  [{:keys [store execution-count]}]
  (when-not store
    (throw (err/error :provider/config-invalid
                      "memory provider requires a :store sqlite spec"
                      {:reason :store-missing})))
  (let [count (or execution-count (atom 0))]
    (reify proto/Provider
      (describe [_] memory-descriptor)
      (normalize-request [_ intent]
        (let [args (memory-args intent)
              k (:memory/key args)]
          (validate-args! memory-descriptor args)
          (case (:intent/type intent)
            :intent/memory-read
            {:tool/id :memory/kv
             :resource {:kind :memory :id k}
             :args {:memory/key k
                    :memory/limit (or (:memory/limit args) 1)
                    :memory/op :read
                    :session/id (:session/id intent)}}
            :intent/memory-write
            {:tool/id :memory/kv
             :resource {:kind :memory :id k}
             :args {:memory/key k
                    :memory/content (:memory/content args)
                    :memory/op :write
                    :session/id (:session/id intent)}}
            (throw (err/error :provider/input-invalid
                              "memory provider expects :intent/memory-read or :intent/memory-write"
                              {:intent/type (:intent/type intent)
                               :value (err/sanitize intent)})))))
      (execute-request! [_ authorized-request]
        (expect-normalized! authorized-request :args)
        (swap! count inc)
        (let [args (:args authorized-request)
              session-id (:session/id args)
              op (:memory/op args)]
          (when-not (uuid? session-id)
            (throw (err/error :provider/request-invalid
                              "normalized memory request must carry a uuid :session/id"
                              {:value (err/sanitize args)})))
          (case op
            :read
            (let [row (first (sqlite/query store
                                           ["SELECT content FROM episodic_memory
                                             WHERE session_id = ? AND memory_key = ?"
                                            (str session-id) (name (:memory/key args))]))]
              (if row
                {:memory/key (:memory/key args)
                 :memory/content (clojure.edn/read-string (:content row))
                 :memory/found true}
                {:memory/key (:memory/key args)
                 :memory/content nil
                 :memory/found false}))
            :write
            (do
              (sqlite/exec! store
                            ["INSERT OR REPLACE INTO episodic_memory
                              (session_id, memory_key, content, created_at)
                              VALUES (?, ?, ?, ?)"
                             (str session-id)
                             (name (:memory/key args))
                             (pr-str (:memory/content args))
                             (now-utc)])
              {:memory/key (:memory/key args)
               :memory/written true})
            (throw (err/error :provider/request-invalid
                              "normalized memory request carries an unknown :memory/op"
                              {:value (err/sanitize authorized-request)}))))))))

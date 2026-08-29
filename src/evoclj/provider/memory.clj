(ns evoclj.provider.memory
  "The kernel-owned :memory/kv provider (feature R1): episodic memory as
  a broker-dispatched effect.

  Fleet R horizontal (narrow handle): the provider closes over a
  MemoryStore handle (evoclj.store.memory-store/MemoryStore), not a raw
  sqlite spec. Raw maps are rejected (definition > validation). For
  backward compat a raw sqlite spec (string/path) is auto-wrapped, but
  new code must pass a MemoryStore via :store or :memory-store.
  FK existence (Fleet P5/F): episodic_memory.session_id REFERENCES
  sessions(id) at rest (011); a write for an unknown session fails with
  a foreign-key violation."
  (:require [evoclj.kernel.error :as err]
            [evoclj.provider.protocol :as proto]
            [evoclj.sci.boundary :as boundary]
            [evoclj.store.memory-store :as ms]
            [evoclj.store.sqlite :as sqlite]
            [malli.core :as m]))

(def ^:private memory-descriptor
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

(defn- memory-args
  [intent]
  (let [payload (:payload intent)]
    (when-not (and (map? payload) (contains? payload :memory/key))
      (throw (err/error :provider/input-invalid
                        "memory intent payload must carry a :memory/key"
                        {:value (err/sanitize payload)})))
    payload))

(defn- validate-args!
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
  [authorized-request key]
  (when-not (and (map? authorized-request)
                 (contains? authorized-request key))
    (throw (err/error :provider/request-invalid
                      "execute-request! requires a normalized request"
                      {:value (err/sanitize authorized-request)}))))

(defn- now-utc [] (str (java.time.Instant/now)))

(defn- normalize-store
  "Normalize :store arg to a MemoryStore handle. Accepts a MemoryStore,
  a raw sqlite spec (string/path), or a map with :memory-store. Raw maps
  {:sqlite ...} are rejected."
  [store]
  (cond
    (instance? evoclj.store.memory_store.MemoryStore store) store
    (map? store) (if (contains? store :sqlite)
                   (throw (err/error :provider/config-invalid
                                     "memory provider store must be a MemoryStore, not a raw {:sqlite ...} map"
                                     {:reason :not-a-memory-store}))
                   (ms/make-memory-store store))
    (string? store) (ms/make-memory-store store)
    :else (ms/make-memory-store store)))

(defn memory-provider
  [{:keys [store memory-store execution-count]}]
  (let [raw (or memory-store store)]
    (when-not raw
      (throw (err/error :provider/config-invalid
                        "memory provider requires a :store sqlite spec or :memory-store MemoryStore"
                        {:reason :store-missing})))
    (let [mem-store (normalize-store raw)
          count (or execution-count (atom 0))]
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
                op (:memory/op args)
                db (.-db ^evoclj.store.memory_store.MemoryStore mem-store)]
            (when-not (uuid? session-id)
              (throw (err/error :provider/request-invalid
                                "normalized memory request must carry a uuid :session/id"
                                {:value (err/sanitize args)})))
            (case op
              :read
              (let [row (first (sqlite/query db
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
                (sqlite/exec! db
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
                                {:value (err/sanitize authorized-request)})))))))))
(ns evoclj.store.capability-store
  "Store-backed CapabilityLease persistence (P7, I2).

  Durable table `capabilities` (migration 013/015) mirrors the sealed lease
  shape: principal (tagged union), closed resource_kind, JSON actions,
  positive window, revoked flag. Helpers are thin wrappers over
  sqlite spec — no mandatory DB read in the hot verify path (the in-memory
  LeaseRegistry remains authoritative); this is the additive persistence
  layer.

  Actions are stored as JSON array strings; constraints as JSON string
  (or nil). Timestamps are TEXT ISO-8601 UTC. The helpers validate
  minimally with Malli but rely on DB CHECKs for the closed invariants."
  (:require [cheshire.core :as json]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.store.sqlite :as sqlite]
            [malli.core :as m]))

;; --- Malli schemas (lightweight) -----------------------------------------

(def ^:private allowed-resource-kinds
  #{"tool" "model" "memory" "filesystem" "filesystem/path"})

(def CapabilityRowSchema
  "Minimal row schema for insert validation."
  [:map
   [:id :string]
   [:principal-type :string]
   [:principal-id :string]
   [:resource-kind [:and :string [:fn #(contains? allowed-resource-kinds %)]]]
   [:resource-id :string]
   [:actions [:vector :string]]
   [:issued-at :string]
   [:expires-at :string]
   [:created-at :string]])

(defn- coerce-json [v]
  (cond
    (nil? v) nil
    (string? v) v
    :else (json/generate-string v)))

(defn- parse-json
  "Parse a JSON string to Clojure, or return nil."
  [s]
  (when s (json/parse-string s true)))

(defn- row->capability
  "Map a DB row (keyword keys) to a normalized capability map."
  [row]
  {:id (:id row)
   :principal-type (:principal_type row)
   :principal-id (:principal_id row)
   :subject-session-id (:subject_session_id row)
   :subject-phenotype-id (:subject_phenotype_id row)
   :resource-kind (:resource_kind row)
   :resource-id (:resource_id row)
   :actions (or (when-let [a (:actions row)]
                  (try (json/parse-string a) (catch Exception _ [a])))
                [])
   :actions-raw (:actions row)
   :constraints (:constraints row)
   :constraints-parsed (parse-json (:constraints row))
   :issued-at (:issued_at row)
   :expires-at (:expires_at row)
   :revoked (let [v (:revoked row)] (if (number? v) (pos? v) (boolean v)))
   :revoked-raw (:revoked row)
   :created-at (:created_at row)})

(defn fetch-capability
  "Fetch capability by `cap-id` (TEXT PRIMARY KEY), or nil when absent.
  Returns a normalized map with parsed actions/constraints and
  boolean :revoked."
  [db cap-id]
  (when cap-id
    (when-let [row (first (sqlite/query db ["SELECT * FROM capabilities WHERE id = ?" (str cap-id)]))]
      (row->capability row))))

(defn- capability->row
  "Normalize a lease-like map to DB row params. Accepts either the store
  row shape or a lease map with nested :principal/:resource."
  [lease]
  (let [id (or (:id lease) (:cap/id lease) (str (:capId lease)))
        ;; principal resolution: explicit flat, or nested :principal, or legacy :subject
        ptype (or (:principal-type lease)
                  (:principal_type lease)
                  (get-in lease [:principal :principal/type])
                  (get-in lease [:principal "principal/type"])
                  (when-let [p (or (:principal lease) (:subject lease))]
                    (name (:principal/type p)))
                  "session")
        pid (or (:principal-id lease)
                (:principal_id lease)
                (get-in lease [:principal :session/id])
                (get-in lease [:principal :job/id])
                (get-in lease [:principal :eval/id])
                (when (= ptype "operator") "operator")
                (get-in lease [:subject :session/id])
                (get-in lease [:subject "session/id"])
                (:subject-session-id lease)
                (:subject_session_id lease))
        rkind (or (:resource-kind lease)
                  (get-in lease [:resource :kind])
                  (get-in lease [:resource "kind"])
                  (:resource_kind lease))
        rid (or (:resource-id lease)
                (get-in lease [:resource :id])
                (get-in lease [:resource "id"])
                (:resource_id lease)
                (str rkind "-resource"))
        actions (or (:actions lease) ["invoke"])
        constraints (:constraints lease)
        issued (or (:issued-at lease) (:issued_at lease) (str (java.time.Instant/now)))
        expires (or (:expires-at lease) (:expires_at lease) (str (.plusSeconds (java.time.Instant/now) 3600)))
        created (or (:created-at lease) (:created_at lease) (str (java.time.Instant/now)))
        revoked (:revoked lease 0)]
    {:id (str id)
     :principal_type (str ptype)
     :principal_id (str pid)
     :subject_session_id (str pid)
     :subject_phenotype_id (str (or (:subject-phenotype-id lease) (:subject_phenotype_id lease) pid))
     :resource_kind (str rkind)
     :resource_id (str rid)
     :actions (coerce-json actions)
     :constraints (coerce-json constraints)
     :issued_at (str issued)
     :expires_at (str expires)
     :revoked (if (boolean? revoked) (if revoked 1 0) (if (number? revoked) revoked (if revoked 1 0)))
     :created_at (str created)}))
;; --- public helpers -------------------------------------------------------
(defn insert-capability!
  "Insert a capability row. `lease` may be a flat helper map or a
  capability-like map with nested :principal/:resource. Returns the
  inserted row as a normalized map. Throws on CHECK / FK violation."
  [db lease]
  (let [row (capability->row lease)]
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :capabilities row))
    (fetch-capability db (:id row))))
(defn revoke-capability!
  "Mark capability `cap-id` as revoked (SET revoked = 1). Idempotent:
  revoking an already-revoked row is a no-op. Returns the updated
  normalized map, or nil when no such row exists."
  [db cap-id]
  (sqlite/with-db [conn db]
    (jdbc/update! conn :capabilities {:revoked 1} ["id = ?" (str cap-id)]))
  (fetch-capability db cap-id))

(defn list-capabilities
  "List capabilities, optionally filtered by {:principal-type t :principal-id id :subject-session-id s :resource-kind k :revoked? bool}."
  ([db] (list-capabilities db {}))
  ([db {:keys [principal-type principal-id subject-session-id resource-kind revoked?]}]
   (let [clauses (cond-> []
                   principal-type (conj ["principal_type = ?" principal-type])
                   principal-id (conj ["principal_id = ?" principal-id])
                   subject-session-id (conj ["subject_session_id = ?" subject-session-id])
                   resource-kind (conj ["resource_kind = ?" resource-kind])
                   (some? revoked?) (conj ["revoked = ?" (if revoked? 1 0)]))
         where (when (seq clauses)
                 (str " WHERE " (clojure.string/join " AND " (map first clauses))))
         params (mapcat rest clauses)
         sql (str "SELECT * FROM capabilities" (or where ""))]
     (mapv row->capability (sqlite/query db (into [sql] params))))))

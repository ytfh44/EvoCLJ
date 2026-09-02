(ns evoclj.store.capability-store
  "P1 single-source Authority: DB is source of truth, memory LeaseRegistry is versioned cache.

  Durable table `capabilities` (migrations 013/015/016/019) mirrors the sealed lease
  shape: principal (tagged union), open resource_kind + faithful resource_edn
  (C1, no CHECK), JSON actions, positive window, revoked flag + revoked_at,
  and faithful lease_edn (pr-str of sealed lease). All writes are durable
  before cache: callers must INSERT/UPDATE WHERE revoked=0 then swap! cache.
  Best-effort try/catch dual writes are removed; synthetic fallback on DB miss
  is deny (no synthetic lease).

  Helpers are thin wrappers over sqlite spec and throw on CHECK/FK violation.
  Restart hydrates from DB via list-active-capabilities / hydrate-registry!."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.capability.resource-kind :as rk]
            [evoclj.store.sqlite :as sqlite]
            [malli.core :as m]))

;; --- Malli schemas (lightweight) -----------------------------------------

(def ^:private allowed-resource-kinds
  (set (map name (rk/allowed-kinds))))

(def CapabilityRowSchema
  "Minimal row schema for insert validation."
  [:map
   [:id :string]
   [:principal-type :string]
   [:principal-id :string]
   [:resource-kind :string]
   [:resource-edn :string]
   [:resource-id {:optional true} :string]
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
  "Map a DB row (keyword keys) to a normalized capability map. Parses
  :resource from resource_edn (faithful C1), JSON actions/constraints,
  and exposes :revoked boolean + :revoked-at and :lease-edn (EDN faithful)."
  [row]
  (let [lease-edn-str (:lease_edn row)
        lease-edn (when lease-edn-str
                    (try (edn/read-string lease-edn-str) (catch Exception _ nil)))]
    {:id (:id row)
     :principal-type (:principal_type row)
     :principal-id (:principal_id row)
     :subject-session-id (:subject_session_id row)
     :subject-phenotype-id (:subject_phenotype_id row)
     :resource-kind (:resource_kind row)
     :resource-id (:resource_id row)
     :resource-edn (:resource_edn row)
     :resource (or (:resource lease-edn) (rk/deserialize-resource (:resource_edn row)))
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
     :revoked-at (:revoked_at row)
     :lease-edn lease-edn-str
     :lease lease-edn
     :created-at (:created_at row)}))

(defn fetch-capability
  "Fetch capability by `cap-id` (TEXT PRIMARY KEY), or nil when absent.
  Returns a normalized map with parsed actions/constraints, parsed
  :resource (from resource_edn or lease_edn) and boolean :revoked."
  [db cap-id]
  (when cap-id
    (when-let [row (first (sqlite/query db ["SELECT * FROM capabilities WHERE id = ?" (str cap-id)]))]
      (row->capability row))))

(defn- capability->row
  "Normalize a lease-like map to DB row params. Accepts either the store
  row shape or a lease map with nested :principal/:resource."
  [lease]
  (let [id (or (:id lease) (:cap/id lease) (str (:capId lease)))
        ptype (let [raw (or (:principal-type lease)
                            (:principal_type lease)
                            (get-in lease [:principal :principal/type])
                            (get-in lease [:principal "principal/type"])
                            (when-let [p (:principal lease)]
                              (name (:principal/type p)))
                            "session")]
                (if (keyword? raw) (name raw) (str raw)))
        pid (or (:principal-id lease)
                (:principal_id lease)
                (get-in lease [:principal :session/id])
                (get-in lease [:principal :job/id])
                (when (= ptype "operator") "operator")
                (:subject-session-id lease)
                (:subject_session_id lease))
        resource (or (:resource lease) (get-in lease [:resource "resource"]))
        resource (cond
                   resource resource
                   (:resource_edn lease) (rk/deserialize-resource (:resource_edn lease))
                   :else nil)
        rkind (or (:resource-kind lease)
                  (get-in lease [:resource :kind])
                  (get-in lease [:resource "kind"])
                  (:resource_kind lease)
                  (when resource (name (or (:kind resource) (keyword (:kind resource))))))
        rkind-name (if (keyword? rkind) (name rkind) (str rkind))
        resource-edn (or (:resource_edn lease)
                         (when resource (rk/serialize-resource resource)))
        rid (or (:resource-id lease)
                (:resource_id lease)
                (get-in lease [:resource :id])
                (get-in lease [:resource "id"])
                (when resource (:id resource))
                (str rkind-name "-resource"))
        actions (or (:actions lease) ["invoke"])
        constraints (:constraints lease)
        issued (or (:issued-at lease) (:issued_at lease) (str (java.time.Instant/now)))
        expires (or (:expires-at lease) (:expires_at lease) (str (.plusSeconds (java.time.Instant/now) 3600)))
        created (or (:created-at lease) (:created_at lease) (str (java.time.Instant/now)))
        fmt #(cond
               (instance? java.util.Date %) (str (.toInstant ^java.util.Date %))
               (instance? java.time.Instant %) (str %)
               (string? %) %
               :else (str %))
        issued (fmt issued)
        expires (fmt expires)
        created (fmt created)
        revoked (:revoked lease 0)
        lease-edn (or (:lease_edn lease) (:lease-edn lease)
                      (when (map? lease)
                        (try (pr-str lease) (catch Exception _ nil))))]
    {:id (str id)
     :principal_type (str ptype)
     :principal_id (str pid)
     :subject_session_id (str pid)
     :subject_phenotype_id (str (or (:subject-phenotype-id lease) (:subject_phenotype_id lease) pid))
     :resource_kind rkind-name
     :resource_edn (or resource-edn (pr-str {:kind (keyword rkind-name) :id (str rid)}))
     :resource_id (str rid)
     :actions (coerce-json actions)
     :constraints (coerce-json constraints)
     :issued_at (str issued)
     :expires_at (str expires)
     :revoked (if (boolean? revoked) (if revoked 1 0) (if (number? revoked) revoked (if revoked 1 0)))
     :revoked_at (when (or (= revoked 1) (true? revoked) (= (:revoked lease) true)) (str (java.time.Instant/now)))
     :lease_edn lease-edn
     :created_at (str created)}))

;; --- public helpers -------------------------------------------------------

(defn insert-capability!
  "Insert a capability row. `lease` may be a flat helper map or a
  capability-like map with nested :principal/:resource. Returns the
  inserted row as a normalized map. Throws on CHECK / FK violation.
  P1: caller must swap! cache only after this durable commit succeeds."
  [db lease]
  (let [row (capability->row lease)
        row (assoc row :revoked 0 :revoked_at nil)]
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :capabilities row))
    (fetch-capability db (:id row))))

(defn revoke-capability!
  "Mark capability `cap-id` as revoked: UPDATE WHERE revoked=0.
  Idempotent and durable-first: sets revoked=1 and revoked_at=NOW only
  when currently not revoked. Returns the updated normalized map, or nil
  when no such row exists. Caller must swap! cache only after this succeeds."
  [db cap-id]
  (let [now (str (java.time.Instant/now))]
    (sqlite/with-db [conn db]
      (jdbc/update! conn :capabilities {:revoked 1 :revoked_at now} ["id = ? AND revoked = 0" (str cap-id)]))
    (fetch-capability db cap-id)))

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

(defn list-active-capabilities
  "List non-revoked capabilities, optionally filtered. Shorthand for revoked?=false."
  ([db] (list-active-capabilities db {}))
  ([db opts] (list-capabilities db (assoc opts :revoked? false))))

(defn hydrate-registry!
  "Restart hydration: load all active (revoked=0) capabilities from DB into
  `registry` atom as versioned cache entries {:lease <sealed> :revoked? false}.
  Increments version (::version) after load. Returns count of hydrated entries.
  DB is truth; cache is replaced."
  [db registry]
  (let [rows (list-active-capabilities db)
        leases (keep (fn [row]
                       (or (:lease row)
                           (let [p {:principal/type (keyword (:principal-type row))
                                    (if (= "session" (:principal-type row)) :session/id
                                        (if (= "job" (:principal-type row)) :job/id
                                            (if (= "eval" (:principal-type row)) :eval/id :operator/id)))
                                    (try (java.util.UUID/fromString (:principal-id row))
                                         (catch Exception _ (:principal-id row)))}
                                 res (:resource row)
                                 actions (set (map keyword (:actions row)))
                                 constraints (or (:constraints-parsed row) {})]
                             (try
                               {:cap/id (java.util.UUID/fromString (:id row))
                                :principal p
                                :resource res
                                :actions actions
                                :constraints constraints
                                :issued-at (java.util.Date/from (java.time.Instant/parse (:issued-at row)))
                                :expires-at (java.util.Date/from (java.time.Instant/parse (:expires-at row)))}
                               (catch Exception _ nil)))))
                     rows)
        entries (into {} (map (fn [l] [(:cap/id l) {:lease l :revoked? false}]) leases))]
    (swap! registry (fn [m]
                      (-> (merge (select-keys m [:evoclj.capability.mint/version]) entries)
                          (assoc :evoclj.capability.mint/version (inc (get m :evoclj.capability.mint/version 0))))))
    (count entries)))

(defn registry-version
  "Return the cache version of `registry` (0 if uninitialized)."
  [registry]
  (get @registry :evoclj.capability.mint/version 0))

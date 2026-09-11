(ns evoclj.store.budget-store
  "Durable capability-budget market.

  Every state transition runs in a BEGIN IMMEDIATE transaction. SQLite is the
  authority: callers may cache snapshots, but reserve/settle/release and
  allocation changes always go through this namespace before an effect."
  (:require [cheshire.core :as json]
            [evoclj.capability.budget :as budget]
            [evoclj.kernel.error :as err]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)
           (java.util UUID)))

(defn- now [] (str (Instant/now)))
(defn- id [] (str (UUID/randomUUID)))
(defn- json-write [x] (json/generate-string (or x {})))
(defn- json-read [x]
  (if (or (nil? x) (= "" x)) {} (json/parse-string x true)))
(defn- fail! [type message data]
  (throw (err/error type message data)))
(defn- budget-map [x]
  (budget/canonicalize (or x {})))

(defn- row->budget [row]
  (when row
    {:budget/id (:id row)
     :lease/id (:lease_id row)
     :parent/id (:parent_id row)
     :root/id (:root_id row)
     :budget (budget-map (json-read (:budget_json row)))
     :reserved (budget-map (json-read (:reserved_json row)))
     :consumed (budget-map (json-read (:consumed_json row)))
     :state (keyword (:state row))
     :expires-at (:expires_at row)
     :created-at (:created_at row)
     :updated-at (:updated_at row)}))

(defn- read-budget-on [conn budget-id]
  (row->budget
   (first (sqlite/query-raw! conn
                             "SELECT * FROM capability_budgets WHERE id = ?"
                             [(str budget-id)]))))

(defn get-budget [db budget-id]
  (row->budget
   (first (sqlite/query db
                        ["SELECT * FROM capability_budgets WHERE id = ?"
                         (str budget-id)]))))

(defn budget-for-lease [db lease-id]
  "Return the active allocation attached to a capability lease."
  (row->budget
   (first (sqlite/query db
                        ["SELECT * FROM capability_budgets
                          WHERE lease_id = ? AND state = 'active'
                          ORDER BY created_at DESC LIMIT 1"
                         (str lease-id)]))))

(defn- existing-ledger [conn key]
  (first (sqlite/query-raw! conn
                             "SELECT * FROM capability_budget_ledger
                              WHERE idempotency_key = ?"
                             [key])))

(defn- insert-ledger!
  "Append one accounting event. Repeating the same key is a no-op and
  returns the original row; a different key always appends a new row."
  [conn budget-id reservation-id event amount key]
  (or (existing-ledger conn key)
      (do
        (sqlite/insert-raw!
         conn
         "INSERT INTO capability_budget_ledger
            (id,budget_id,reservation_id,event_type,amount_json,idempotency_key,created_at)
          VALUES (?,?,?,?,?,?,?)"
         [(id) (str budget-id) (some-> reservation-id str) (name event)
          (json-write amount) key (now)])
        (existing-ledger conn key))))

(defn- update-budget! [conn b]
  (sqlite/insert-raw!
   conn
   "UPDATE capability_budgets
       SET budget_json=?,reserved_json=?,consumed_json=?,state=?,updated_at=?
     WHERE id=?"
   [(json-write (:budget b))
    (json-write (:reserved b))
    (json-write (:consumed b))
    (name (:state b))
    (now)
    (str (:budget/id b))]))

(defn- parse-instant [value]
  (cond
    (nil? value) nil
    (instance? Instant value) value
    (instance? java.util.Date value) (.toInstant ^java.util.Date value)
    (string? value) (Instant/parse value)
    :else (fail! :capability/budget-invalid
                 "budget expiry must be an ISO instant, Date, or Instant"
                 {:expires-at (err/sanitize value)})))

(defn create-budget!
  "Create a root allocation or an attenuated child allocation. Child limits
  are reserved from the parent in the same write transaction, preventing
  concurrent siblings from oversubscribing the parent's free balance."
  [db opts]
  (let [requested (budget-map (or (:budget opts) (:limits opts) {}))
        bid (str (or (:budget/id opts) (id)))
        parent-id (some-> (or (:parent/id opts) (:parent-id opts)) str)
        lease-id (some-> (or (:lease/id opts) (:lease-id opts)) str)
        expires (parse-instant (:expires-at opts))
        expires-text (some-> expires str)
        ts (now)]
    (sqlite/with-write-tx [conn db]
      (let [parent (when parent-id (read-budget-on conn parent-id))]
        (when (and parent-id (nil? parent))
          (fail! :capability/budget-missing
                 "parent budget does not exist"
                 {:parent/id parent-id}))
        (let [b (if parent
                  (do
                    (when-not (= :active (:state parent))
                      (fail! :capability/budget-closed
                             "parent budget is not active"
                             {:parent/id parent-id}))
                    (when (and expires (:expires-at parent)
                               (.isAfter expires (parse-instant (:expires-at parent))))
                      (fail! :capability/budget-invalid
                             "child budget expiry exceeds parent"
                             {:parent/id parent-id}))
                    (when-not (budget/le? (:budget parent) requested)
                      (fail! :capability/budget-exceeded
                             "child budget exceeds parent allocation"
                             {:parent/id parent-id
                              :parent-budget (:budget parent)
                              :requested requested}))
                    (let [reserved (budget/reserve (:budget parent)
                                                   (:reserved parent)
                                                   (:consumed parent)
                                                   requested)
                          parent' (assoc parent :reserved reserved)
                          child {:budget/id bid
                                 :lease/id lease-id
                                 :parent/id parent-id
                                 :root/id (:root/id parent)
                                 :budget requested
                                 :reserved {}
                                 :consumed {}
                                 :state :active
                                 :expires-at expires-text
                                 :created-at ts
                                 :updated-at ts}]
                      (update-budget! conn parent')
                      (insert-ledger! conn parent-id nil :allocate requested
                                      (str "allocate-child:" bid))
                      child))
                  {:budget/id bid
                   :lease/id lease-id
                   :parent/id nil
                   :root/id bid
                   :budget requested
                   :reserved {}
                   :consumed {}
                   :state :active
                   :expires-at expires-text
                   :created-at ts
                   :updated-at ts})]
          (sqlite/insert-raw!
           conn
           "INSERT INTO capability_budgets
              (id,lease_id,parent_id,root_id,budget_json,reserved_json,
               consumed_json,state,expires_at,created_at,updated_at)
            VALUES (?,?,?,?,?,?,?,?,?,?,?)"
           [(str (:budget/id b)) (:lease/id b) (:parent/id b) (:root/id b)
            (json-write (:budget b)) (json-write (:reserved b))
            (json-write (:consumed b)) (name (:state b)) (:expires-at b)
            ts ts])
          (insert-ledger! conn (:budget/id b) nil :allocate (:budget b)
                          (str "allocate:" (:budget/id b)))
          b)))))

(defn- reservation-row [conn key]
  (first (sqlite/query-raw! conn
                             "SELECT * FROM capability_budget_reservations
                              WHERE idempotency_key = ?"
                             [key])))

(defn- reservation->map [row]
  (when row
    {:reservation/id (:id row)
     :budget/id (:budget_id row)
     :intent/id (:intent_id row)
     :attempt (:attempt row)
     :idempotency/key (:idempotency_key row)
     :amount (budget-map (json-read (:amount_json row)))
     :status (keyword (:status row))
     :actual (budget-map (json-read (:settled_json row)))}))

(defn reserve!
  "Reserve one intent attempt before its provider call. Repeating a key
  returns the original reservation without mutation. Reusing a key with a
  different budget or amount fails closed."
  [db budget-id opts]
  (let [key-value (or (:idempotency/key opts) (:idempotency-key opts))
        key (when (some? key-value) (str key-value))
        amount (budget-map (or (:amount opts) {}))
        rid (str (or (:reservation/id opts) (:reservation-id opts) (id)))
        intent-id (:intent/id opts)
        attempt (long (or (:attempt opts) 1))]
    (when (or (nil? key) (= "" key))
      (fail! :capability/budget-invalid
             "reservation requires idempotency key"
             {}))
    (sqlite/with-write-tx [conn db]
      (if-let [existing (reservation-row conn key)]
        (let [existing-map (reservation->map existing)]
          (when (or (not= (str budget-id) (str (:budget/id existing-map)))
                    (not= amount (:amount existing-map)))
            (fail! :capability/budget-idempotency-conflict
                   "idempotency key already names a different reservation"
                   {:idempotency/key key}))
          existing-map)
        (let [b (read-budget-on conn budget-id)]
          (when-not b
            (fail! :capability/budget-missing
                   "budget does not exist"
                   {:budget/id budget-id}))
          (when-not (= :active (:state b))
            (fail! :capability/budget-closed
                   "budget is revoked or closed"
                   {:budget/id budget-id}))
          (when (and (:expires-at b)
                     (.isAfter (Instant/now) (parse-instant (:expires-at b))))
            (fail! :capability/budget-expired
                   "budget has expired"
                   {:budget/id budget-id}))
          (let [reserved (try
                           (budget/reserve (:budget b) (:reserved b)
                                           (:consumed b) amount)
                           (catch clojure.lang.ExceptionInfo e
                             (if (= :capability/budget-exceeded
                                    (:error/type (ex-data e)))
                               (fail! :capability/budget-exhausted
                                      "budget has insufficient available balance"
                                      {:budget/id budget-id
                                       :requested amount})
                               (throw e))))
                b' (assoc b :reserved reserved)]
            (update-budget! conn b')
            (sqlite/insert-raw!
             conn
             "INSERT INTO capability_budget_reservations
                (id,budget_id,intent_id,attempt,idempotency_key,amount_json,
                 status,created_at,settled_at)
              VALUES (?,?,?,?,?,?,?,?,NULL)"
             [rid (str budget-id) (some-> intent-id str) attempt key
              (json-write amount) "reserved" (now)])
            (insert-ledger! conn budget-id rid :reserve amount
                            (str key "|reserve"))
            {:reservation/id rid
             :budget/id (str budget-id)
             :intent/id intent-id
             :attempt attempt
             :idempotency/key key
             :amount amount
             :status :reserved
             :actual {}}))))))

(defn settle!
  "Consume actual reserved units and release the unused part. Idempotent for
  a terminal reservation state."
  [db reservation-id actual]
  (let [actual (budget-map actual)]
    (sqlite/with-write-tx [conn db]
      (let [row (first (sqlite/query-raw!
                        conn
                        "SELECT * FROM capability_budget_reservations WHERE id = ?"
                        [(str reservation-id)]))]
        (when-not row
          (fail! :capability/budget-reservation-missing
                 "reservation does not exist"
                 {:reservation/id reservation-id}))
        (if (not= "reserved" (:status row))
          (reservation->map row)
          (let [b (read-budget-on conn (:budget_id row))
                reservation (budget-map (json-read (:amount_json row)))
                {:keys [reserved consumed]} (budget/settle (:reserved b)
                                                           (:consumed b)
                                                           reservation
                                                           actual)
                b' (assoc b :reserved reserved :consumed consumed)]
            (update-budget! conn b')
            (sqlite/insert-raw!
             conn
             "UPDATE capability_budget_reservations
                 SET status='settled',settled_json=?,settled_at=?
               WHERE id=? AND status='reserved'"
             [(json-write actual) (now) (str reservation-id)])
            (insert-ledger! conn (:budget/id b) reservation-id :settle actual
                            (str (:idempotency_key row) "|settle"))
            (reservation->map
             (first (sqlite/query-raw!
                     conn
                     "SELECT * FROM capability_budget_reservations WHERE id = ?"
                     [(str reservation-id)])))))))))

(defn release!
  "Release a reservation without consuming it. Idempotent after settlement or
  release."
  [db reservation-id]
  (sqlite/with-write-tx [conn db]
    (let [row (first (sqlite/query-raw!
                      conn
                      "SELECT * FROM capability_budget_reservations WHERE id = ?"
                      [(str reservation-id)]))]
      (when-not row
        (fail! :capability/budget-reservation-missing
               "reservation does not exist"
               {:reservation/id reservation-id}))
      (if (not= "reserved" (:status row))
        (reservation->map row)
        (let [b (read-budget-on conn (:budget_id row))
              amount (budget-map (json-read (:amount_json row)))
              b' (assoc b :reserved (budget/release (:reserved b) amount))]
          (update-budget! conn b')
          (sqlite/insert-raw!
           conn
           "UPDATE capability_budget_reservations
               SET status='released',settled_at=?
             WHERE id=? AND status='reserved'"
           [(now) (str reservation-id)])
          (insert-ledger! conn (:budget/id b) reservation-id :release amount
                          (str (:idempotency_key row) "|release"))
          (reservation->map
           (first (sqlite/query-raw!
                   conn
                   "SELECT * FROM capability_budget_reservations WHERE id = ?"
                   [(str reservation-id)]))))))))

(defn reallocate!
  "Move only free balance between allocations in one serialized transaction.
  Allocations must be distinct siblings; the optional idempotency key allows
  callers to repeat a requested transfer safely."
  ([db from-id to-id amount]
   (reallocate! db from-id to-id amount nil))
  ([db from-id to-id amount idempotency-key]
   (let [amount (budget-map amount)
         key (str (or idempotency-key
                      (str "reallocate:" from-id ":" to-id ":" (json-write amount))))]
     (sqlite/with-write-tx [conn db]
       (if-let [event (existing-ledger conn key)]
         {:from (read-budget-on conn from-id)
          :to (read-budget-on conn to-id)
          :idempotency/key key
          :ledger event}
         (let [from (read-budget-on conn from-id)
               to (read-budget-on conn to-id)]
           (when (or (nil? from) (nil? to))
             (fail! :capability/budget-missing
                    "allocation does not exist"
                    {:from from-id :to to-id}))
           (when (= (str from-id) (str to-id))
             (fail! :capability/budget-reallocation-invalid
                    "cannot reallocate an allocation to itself"
                    {}))
           (when (not= (:root/id from) (:root/id to))
             (fail! :capability/budget-reallocation-invalid
                    "allocations have different roots"
                    {}))
           (when (not= (:parent/id from) (:parent/id to))
             (fail! :capability/budget-reallocation-invalid
                    "allocations are not siblings"
                    {}))
           (when (or (not= :active (:state from))
                     (not= :active (:state to)))
             (fail! :capability/budget-closed
                    "allocation is not active"
                    {}))
           (let [{from' :from to' :to}
                 (budget/reallocate from to amount)]
             (update-budget! conn from')
             (update-budget! conn to')
             (insert-ledger! conn from-id nil :reallocate amount
                             (str key "|from"))
             (insert-ledger! conn to-id nil :reallocate amount
                             (str key "|to"))
             {:from (read-budget-on conn from-id)
              :to (read-budget-on conn to-id)
              :idempotency/key key})))))))

(defn revoke-budget!
  "Revoke an allocation and its descendants, releasing outstanding
  reservations in one transaction. Repeated revocation is idempotent."
  [db budget-id]
  (sqlite/with-write-tx [conn db]
    (let [target (read-budget-on conn budget-id)]
      (when-not target
        (fail! :capability/budget-missing
               "budget does not exist"
               {:budget/id budget-id}))
      (let [all (mapv row->budget
                      (sqlite/query-raw!
                       conn
                       "SELECT * FROM capability_budgets WHERE root_id = ?"
                       [(str (:root/id target))]))
            descendants (loop [ids #{(str (:budget/id target))}]
                         (let [next-ids (into ids
                                             (map (comp str :budget/id)
                                                  (filter #(and (:parent/id %)
                                                                (contains? ids (str (:parent/id %))))
                                                          all)))]
                           (if (= ids next-ids) ids (recur next-ids))))
            rows (filterv #(contains? descendants (str (:budget/id %))) all)
            ts (now)]
        (doseq [b rows]
          (when (= :active (:state b))
            (let [reservations (sqlite/query-raw!
                                conn
                                "SELECT * FROM capability_budget_reservations
                                 WHERE budget_id = ? AND status='reserved'"
                                [(str (:budget/id b))])]
              (doseq [reservation reservations]
                (let [amount (budget-map (json-read (:amount_json reservation)))]
                  (sqlite/insert-raw!
                   conn
                   "UPDATE capability_budget_reservations
                       SET status='released',settled_at=?
                     WHERE id=? AND status='reserved'"
                   [ts (str (:id reservation))])
                  (insert-ledger! conn (:budget/id b) (:id reservation) :release
                                  amount
                                  (str (:idempotency_key reservation) "|release")))))
            (sqlite/insert-raw!
             conn
             "UPDATE capability_budgets
                 SET state='revoked',reserved_json='{}',updated_at=?
               WHERE id=?"
             [ts (str (:budget/id b))])
            (insert-ledger! conn (:budget/id b) nil :revoke (:reserved b)
                            (str "revoke:" (:budget/id b))))))
        ;; Child allocation reserves its limits in the parent. Return that
        ;; allocation to the parent when the child tree is revoked.
        (when (and (= :active (:state target)) (:parent/id target))
          (let [parent (read-budget-on conn (:parent/id target))]
            (when (and parent (= :active (:state parent)))
              (update-budget! conn
                              (assoc parent
                                     :reserved (budget/release (:reserved parent)
                                                               (:budget target))))
              (insert-ledger! conn (:budget/id parent) nil :release
                              (:budget target)
                              (str "deallocate:" (:budget/id target))))))
        (read-budget-on conn budget-id))))

(defn recover!
  "After restart, release reservations with no terminal settlement. The
  terminal release event is idempotent, so a crash during recovery is safe."
  [db]
  (let [rows (sqlite/query db
                            ["SELECT id FROM capability_budget_reservations
                              WHERE status='reserved'"])]
    (doseq [row rows]
      (release! db (:id row)))
    (count rows)))

(defn ledger [db budget-id]
  (mapv (fn [r]
          (update r :amount_json #(budget-map (json-read %))))
        (sqlite/query db
                      ["SELECT * FROM capability_budget_ledger
                        WHERE budget_id = ? ORDER BY created_at,id"
                       (str budget-id)])))

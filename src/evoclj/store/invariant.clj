(ns evoclj.store.invariant
  "Narrow durable boundary for generated-invariant evidence and decisions.

  CAS is written and verified before any dependent row. Core rows are
  append-only; activation, its event, and outbox are one BEGIN IMMEDIATE
  transaction. This namespace never evaluates generated code."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [evoclj.eval.static :as static]
            [evoclj.evolution.invariant :as invariant]
            [evoclj.kernel.error :as err]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.support.failpoint :as fault])
  (:import (java.nio.charset StandardCharsets)
           (java.time Instant)
           (java.util UUID)))

(defn- now [] (str (Instant/now)))
(defn- id [] (str (UUID/randomUUID)))
(defn- stores [store]
  (when-not (map? store) (throw (err/error :invariant/store-invalid "store must be {:sqlite ... :cas ...}" {:reason :not-a-map})))
  (when-not (contains? store :sqlite) (throw (err/error :invariant/store-invalid "store is missing :sqlite" {:reason :sqlite-missing})))
  (when-not (contains? store :cas) (throw (err/error :invariant/store-invalid "store is missing :cas" {:reason :cas-missing})))
  store)
(defn- encoded-bytes [x] (.getBytes (pr-str x) StandardCharsets/UTF_8))
(defn- cas-put! [store value media-type]
  (let [{:keys [cas]} (stores store)
        out (cas/put-bytes! cas (encoded-bytes value) {:media-type media-type})
        aid (:artifact/id out)]
    (artifact/ensure-artifact! (:sqlite store) aid media-type (:size out))
    aid))
(defn- ensure-ref! [store ref]
  (when (and (string? ref) (str/starts-with? ref "sha256:")
             (not (cas/exists? (:cas store) ref)))
    (throw (err/error :invariant/cas-missing "referenced invariant evidence is absent from CAS" {:artifact/id ref})))
  ref)
(defn- json [x] (pr-str x))
(defn- read-edn [s]
  (try (edn/read-string s) (catch Exception _ nil)))
(defn- row->proposal [r]
  (when r
    (let [p (invariant/proposal
             {:proposal/id (:id r)
              :proposer (:proposer r)
              :reviewer (:reviewer r)
              :scope (keyword (:scope r))
              :risk (keyword (:risk r))
              :version (:version r)
              :registry/revision (:registry_revision r)
              :predicate (or (read-edn (:predicate_json r)) {})
              :evidence/refs (or (read-edn (:evidence_refs r)) [])
              :replay/refs (or (read-edn (:replay_refs r)) [])
              :adversarial/refs (or (read-edn (:adversarial_refs r)) [])})]
      (assoc p :proposal/id (:id r)
               :proposal/digest (:proposal_digest r)
               :predicate/digest (:predicate_digest r)))))
(defn get-proposal [store proposal-id]
  (stores store)
  (row->proposal (first (sqlite/query (:sqlite store) ["SELECT * FROM invariant_proposals WHERE id = ?" (str proposal-id)]))))
(defn list-proposals [store]
  (stores store)
  (mapv row->proposal (sqlite/query (:sqlite store) ["SELECT * FROM invariant_proposals ORDER BY created_at, id"])))

(defn propose!
  "Persist a normalized proposal as immutable evidence. The proposal and its
  predicate are put in CAS before the foreign-keyed proposal row."
  [store p]
  (let [p (invariant/proposal p)
        proposal-id (str (or (:proposal/id p) (:invariant/id p) (id)))
        predicate-id (cas-put! store (dissoc (:predicate p) :predicate/digest) "application/edn")
        predicate (assoc (:predicate p) :predicate/digest predicate-id)
        p (assoc p :proposal/id proposal-id :predicate predicate :predicate/digest predicate-id)
        proposal-id-artifact (cas-put! store p "application/edn")
        refs (concat (:evidence/refs p) (:replay/refs p) (:adversarial/refs p))]
    (doseq [r refs] (ensure-ref! store r))
    (sqlite/with-write-tx [conn (:sqlite store)]
      (if-let [existing (first (sqlite/query-raw! conn "SELECT * FROM invariant_proposals WHERE id = ?" [proposal-id]))]
        (if (= proposal-id-artifact (:proposal_digest existing))
          (row->proposal existing)
          (throw (err/error :invariant/idempotency-conflict "proposal id already names different evidence" {:proposal/id proposal-id})))
        (do
          (sqlite/insert-raw! conn
            "INSERT INTO invariant_proposals
             (id,proposer,reviewer,scope,risk,version,registry_revision,predicate_digest,predicate_json,proposal_digest,evidence_refs,replay_refs,adversarial_refs,status,created_at)
             VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
            [proposal-id (str (:proposer p)) (some-> (:reviewer p) str) (name (:scope p)) (name (:risk p)) (:version p)
             (str (:registry/revision p)) predicate-id (json (:predicate p)) proposal-id-artifact (json (:evidence/refs p))
             (json (:replay/refs p)) (json (:adversarial/refs p)) "proposed" (now)])
          (assoc p :proposal/digest proposal-id-artifact))))))

(defn- row->run [r]
  (when r
    (merge (or (read-edn (:run_json r)) {})
           {:run/id (:id r) :proposal/id (:proposal_id r) :kind (keyword (:kind r))
            :result-ref (:result_ref r) :run/digest (:run_digest r)
            :gate (some-> (:gate r) keyword) :model/policy (some-> (:model_policy r) keyword)
            :deterministic? (= 1 (:deterministic r)) :fresh-model? (= 1 (:fresh_model r))
            :passed? (= 1 (:passed r))})) )
(defn list-runs [store proposal-id]
  (stores store)
  (mapv row->run (sqlite/query (:sqlite store) ["SELECT * FROM invariant_runs WHERE proposal_id = ? ORDER BY created_at, id" (str proposal-id)])))
(defn append-run!
  "Append counterexample/replay/adversarial evidence. Repeating the same run
  id and digest is a no-op; a conflicting reuse fails closed."
  [store proposal-id r]
  (stores store)
  (when-not (get-proposal store proposal-id)
    (throw (err/error :invariant/proposal-missing "proposal does not exist" {:proposal/id proposal-id})))
  (let [raw (assoc (if (map? r) r {:result r}) :proposal/id proposal-id)
        result-id (if (and (string? (:result-ref raw)) (str/starts-with? (:result-ref raw) "sha256:"))
                    (ensure-ref! store (:result-ref raw))
                    (cas-put! store (or (:result raw) raw) "application/edn"))
        run-id (str (or (:run/id raw) (id)))
        r (invariant/run (assoc raw :run/id run-id :result-ref result-id))
        run-id-artifact (cas-put! store r "application/edn")]
    (sqlite/with-write-tx [conn (:sqlite store)]
      (if-let [existing (first (sqlite/query-raw! conn "SELECT * FROM invariant_runs WHERE id = ?" [run-id]))]
        (if (= run-id-artifact (:run_digest existing))
          (row->run existing)
          (throw (err/error :invariant/idempotency-conflict "run id already names different evidence" {:run/id run-id})))
        (do
          (sqlite/insert-raw! conn
            "INSERT INTO invariant_runs
             (id,proposal_id,kind,result_ref,run_digest,gate,model_policy,deterministic,fresh_model,passed,run_json,created_at)
             VALUES (?,?,?,?,?,?,?,?,?,?,?,?)"
            [run-id (str proposal-id) (name (:kind r)) result-id run-id-artifact (some-> (:gate r) name)
             (some-> (:model/policy r) name) (if (:deterministic? r) 1 0) (if (:fresh-model? r) 1 0)
             (if (:passed? r) 1 0) (json r) (now)])
          (assoc r :run/id run-id :result-ref result-id :run/digest run-id-artifact))))))

(defn- decision [store proposal-id decision reviewer opts]
  (let [p (get-proposal store proposal-id)
        runs (list-runs store proposal-id)
        a (merge {:reviewer reviewer :replay/ref (or (:replay/ref opts) (-> runs first :result-ref))
                  :adversarial/ref (or (:adversarial/ref opts) (-> runs second :result-ref))}
                 opts)]
    (when-not p (throw (err/error :invariant/proposal-missing "proposal does not exist" {:proposal/id proposal-id})))
    (if (= :approved decision)
      (invariant/approval p a runs)
      (do (when (= (str reviewer) (str (:proposer p)))
            (throw (err/error :invariant/reviewer-conflict "proposer cannot reject/approve its own proposal" {})))
          (assoc a :proposal/id (str proposal-id) :status :rejected :activation-qualified? false)))))

(defn approve!
  "Append a reviewer approval. Approval never mutates a proposal row."
  [store proposal-id reviewer opts]
  (stores store)
  (let [d (decision store proposal-id :approved reviewer opts)
        did (str (or (:decision/id d) (id)))
        digest (cas-put! store d "application/edn")]
    (sqlite/with-write-tx [conn (:sqlite store)]
      (if-let [existing (first (sqlite/query-raw! conn "SELECT * FROM invariant_decisions WHERE id = ?" [did]))]
        (if (= digest (:decision_digest existing)) existing
            (throw (err/error :invariant/idempotency-conflict "decision id already exists" {:decision/id did})))
        (sqlite/insert-raw! conn
          "INSERT INTO invariant_decisions (id,proposal_id,decision,reviewer,decision_digest,activation_qualified,reason,created_at) VALUES (?,?,?,?,?,?,?,?)"
          [did (str proposal-id) "approved" (str reviewer) digest (if (:activation-qualified? d) 1 0) (:reason d) (now)])))
    (assoc d :decision/id did :decision/digest digest)))

(defn reject!
  [store proposal-id reviewer reason]
  (stores store)
  (let [d (decision store proposal-id :rejected reviewer {:reason (str reason)})
        did (str (id)) digest (cas-put! store d "application/edn")]
    (sqlite/with-write-tx [conn (:sqlite store)]
      (sqlite/insert-raw! conn
        "INSERT INTO invariant_decisions (id,proposal_id,decision,reviewer,decision_digest,activation_qualified,reason,created_at) VALUES (?,?,?,?,?,?,?,?)"
        [did (str proposal-id) "rejected" (str reviewer) digest 0 (:reason d) (now)]))
    (assoc d :decision/id did :decision/digest digest)))

(defn- latest-approval [store proposal-id]
  (first (sqlite/query (:sqlite store)
                       ["SELECT * FROM invariant_decisions WHERE proposal_id = ? AND decision = 'approved' ORDER BY created_at DESC, id DESC LIMIT 1" (str proposal-id)])))

(defn activate!
  "Commit an approved invariant and its event/outbox atomically, then publish
  only after the durable transaction commits."
  ([store proposal-id reviewer]
   (activate! store proposal-id reviewer {}))
  ([store proposal-id reviewer opts]
   (stores store)
   (let [p (get-proposal store proposal-id)
         d (latest-approval store proposal-id)]
     (when-not p (throw (err/error :invariant/proposal-missing "proposal does not exist" {:proposal/id proposal-id})))
     (when-not d (throw (err/error :invariant/approval-missing "activation requires an approval decision" {:proposal/id proposal-id})))
     (when (= (str reviewer) (:proposer p)) (throw (err/error :invariant/reviewer-conflict "proposer cannot activate its own proposal" {})))
     (when-not (= 1 (:activation_qualified d)) (throw (err/error :invariant/not-qualified "activation requires deterministic replay and adversarial G3 evidence" {})))
     (let [aid (str (id))
           activation-digest (cas-put! store {:proposal/id proposal-id :version (:version p) :predicate (:predicate p) :reviewer reviewer} "application/edn")
           desc {:invariant/id (or (:invariant/id p) proposal-id) :proposal/id proposal-id :version (:version p)
                 :predicate (:predicate p) :registry/revision (:registry/revision p)
                 :activation/id aid :activation/committed? true}]
       (let [result (sqlite/with-write-tx [conn (:sqlite store)]
                      (if-let [old (first (sqlite/query-raw! conn "SELECT * FROM invariant_activations WHERE version = ? AND predicate_digest = ?" [(:version p) (:predicate/digest (:predicate p))]))]
                        {:row old :idempotent? true}
                        (do
                          (sqlite/insert-raw! conn
                            "INSERT INTO invariant_activations (id,proposal_id,decision_id,version,predicate_digest,activation_digest,registry_revision,status,committed_at) VALUES (?,?,?,?,?,?,?,?,?)"
                            [aid (str proposal-id) (:id d) (:version p) (:predicate/digest (:predicate p)) activation-digest (str (:registry/revision p)) "active" (now)])
                          (sqlite/insert-raw! conn
                            "INSERT INTO invariant_events (activation_id,event_type,payload_ref,created_at) VALUES (?,?,?,?)"
                            [aid "activated" activation-digest (now)])
                          (let [ev (first (sqlite/query-raw! conn "SELECT id FROM invariant_events WHERE activation_id = ? AND event_type = 'activated'" [aid]))]
                            (sqlite/insert-raw! conn
                              "INSERT INTO invariant_outbox (id,activation_id,event_id,event_type,dispatched,created_at) VALUES (?,?,?,?,0,?)"
                              [(id) aid (:id ev) "activated" (now)]))
                          (fault/trigger! opts :before-event-append)
                          {:id aid :idempotent? false})))]
         (let [desc (if (:idempotent? result)
                      (assoc desc :activation/id (or (:id (:row result)) aid))
                      desc)]
           (static/publish-active-invariant! desc)
           (assoc desc :status :active :idempotent? (:idempotent? result))))))))

(defn disable!
  [store proposal-id reviewer reason]
  (stores store)
  (when (= (str reviewer) (str (:proposer (get-proposal store proposal-id))))
    (throw (err/error :invariant/reviewer-conflict "proposer cannot disable its own proposal" {})))
  (let [d (invariant/disable proposal-id reviewer reason)
        digest (cas-put! store d "application/edn")]
    (sqlite/with-write-tx [conn (:sqlite store)]
      (doseq [row (sqlite/query-raw! conn "SELECT id FROM invariant_activations WHERE proposal_id = ? AND status = 'active'" [(str proposal-id)])]
        (sqlite/insert-raw! conn "INSERT OR IGNORE INTO invariant_disables (id,proposal_id,reviewer,reason,disable_digest,created_at) VALUES (?,?,?,?,?,?)"
                             [(id) (str proposal-id) (str reviewer) (str reason) digest (now)])))
    (static/disable-active-invariant! proposal-id)
    (assoc d :decision/digest digest)))

(defn recover-activations!
  "Publish only activation rows that durably committed. Pending proposals and
  approvals are never auto-activated. Malformed or missing CAS is reported."
  [store]
  (stores store)
  (reduce (fn [out row]
            (try
              (let [p (get-proposal store (:proposal_id row))
                    desc {:invariant/id (or (:invariant/id p) (:proposal_id row))
                          :proposal/id (:proposal_id row) :version (:version row)
                          :predicate (:predicate p) :registry/revision (:registry_revision row)
                          :activation/id (:id row) :activation/committed? true}]
                (static/publish-active-invariant! desc)
                (conj out {:activation/id (:id row) :status :published}))
              (catch Exception e
                (conj out {:activation/id (:id row) :status :quarantined :error (err/error-data e)}))))
          [] (sqlite/query (:sqlite store) ["SELECT a.* FROM invariant_activations a WHERE a.status = 'active' AND NOT EXISTS (SELECT 1 FROM invariant_disables d WHERE d.proposal_id = a.proposal_id) ORDER BY a.committed_at, a.id"])))

;; Stable narrow aliases keep callers on the data-only boundary; no registry
;; or executable-code handles are exposed.
(def propose propose!)
(def append-run append-run!)
(defn append-counterexample!
  [store proposal-id result]
  (append-run! store proposal-id
               (assoc (if (map? result) result {:result result})
                      :kind :counterexample)))
(def approve-invariant! approve!)
(def reject-invariant! reject!)
(def activate-invariant! activate!)
(def disable-invariant! disable!)

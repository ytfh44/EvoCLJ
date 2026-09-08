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
            [evoclj.store.existence :as existence]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.support.failpoint :as fault])
  (:import (java.nio.charset StandardCharsets)
           (java.time Instant)
           (java.util UUID)))

(defn- now [] (str (Instant/now)))
(defn- id [] (str (UUID/randomUUID)))
(declare verify-durable-activation! get-proposal)

(defn- stores [store]
  (when-not (map? store) (throw (err/error :invariant/store-invalid "store must be {:sqlite ... :cas ...}" {:reason :not-a-map})))
  (when-not (contains? store :sqlite) (throw (err/error :invariant/store-invalid "store is missing :sqlite" {:reason :sqlite-missing})))
  (when-not (contains? store :cas) (throw (err/error :invariant/store-invalid "store is missing :cas" {:reason :cas-missing})))
  (static/install-durable-verifier! #(verify-durable-activation! store %1 %2))
  store)
(defn- encoded-bytes [x] (.getBytes (pr-str x) StandardCharsets/UTF_8))
(defn- verifying-cas [store]
  (let [c (:cas store)]
    (cas/->cas (if (map? c) (:root c) c) {:verify true})))
(defn- cas-put! [store value media-type]
  (let [{:keys [cas]} (stores store)
        out (cas/put-bytes! cas (encoded-bytes value) {:media-type media-type})
        aid (:artifact/id out)]
    (artifact/ensure-artifact! (:sqlite store) aid media-type (:size out))
    aid))
(defn- ensure-ref! [store ref]
  "Every persisted external reference must arrive as a sealed existence proof and
  be rehashed before it is accepted."
  (let [vd (existence/ensure-proof ref)
        digest (existence/digest-of vd)]
    (cas/get-bytes (verifying-cas store) digest)
    digest))

(defn- verify-durable-activation!
  [store descriptor _proof]
  (let [aid (:activation/id descriptor)
        digest (:activation/digest descriptor)
        proposal-id (:proposal/id descriptor)
        row (first (sqlite/query (:sqlite store) ["SELECT * FROM invariant_activations WHERE id = ? AND status = 'active'" (str aid)]))
        p (get-proposal store proposal-id)
        approval (when row (first (sqlite/query (:sqlite store) ["SELECT * FROM invariant_decisions WHERE id = ? AND proposal_id = ? AND decision = 'approved' AND activation_qualified = 1" (:decision_id row) (str proposal-id)])))
        event (when row (first (sqlite/query (:sqlite store) ["SELECT e.id FROM invariant_events e JOIN invariant_outbox o ON o.event_id = e.id AND o.activation_id = e.activation_id WHERE e.activation_id = ? AND e.event_type = 'activated' AND o.event_type = 'activated' AND e.payload_ref = ?" (str aid) digest])))]
    (when-not (and row p approval event
                   (= (str (:proposal_id row)) (str proposal-id))
                   (= (:version row) (:version descriptor))
                   (= (:predicate_digest row) (:predicate/digest (:predicate descriptor)))
                   (= (:predicate_digest row) (:predicate/digest (:predicate p)))
                   (= (:registry_revision row) (:registry/revision descriptor))
                   (= (:registry_revision row) (:registry/revision p))
                   (= (:registry_revision row) (static/registry-revision))
                   (= (:activation_digest row) digest)
                   (nil? (first (sqlite/query (:sqlite store) ["SELECT id FROM invariant_disables WHERE proposal_id = ?" (str proposal-id)]))))
      (throw (err/error :invariant/activation-invalid "durable activation facts do not match publication" {:reason :durable-state-mismatch :activation/id aid})))
    (cas/get-bytes (verifying-cas store) digest)
    true))

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
  (when-let [r (first (sqlite/query (:sqlite store) ["SELECT * FROM invariant_proposals WHERE id = ?" (str proposal-id)]))]
    (let [v (verifying-cas store)]
      (doseq [k [:predicate_digest :proposal_digest]]
        (cas/get-bytes v (get r k)))
      (doseq [k [:evidence_refs :replay_refs :adversarial_refs]]
        (doseq [ref (or (read-edn (get r k)) [])]
          (cas/get-bytes v ref)))
      (row->proposal r))))
(defn list-proposals [store]
  (stores store)
  (mapv row->proposal (sqlite/query (:sqlite store) ["SELECT * FROM invariant_proposals ORDER BY created_at, id"])))

(defn propose!
  "Persist a normalized proposal as immutable evidence. The proposal and its
  predicate are put in CAS before the foreign-keyed proposal row."
  [store p]
  (let [p (reduce (fn [p k]
                    (update p k #(mapv (partial ensure-ref! store) (or % []))))
                  p [:evidence/refs :replay/refs :adversarial/refs])
        p (invariant/proposal p)
        proposal-id (str (or (:proposal/id p) (:invariant/id p) (id)))
        predicate-id (cas-put! store (dissoc (:predicate p) :predicate/digest) "application/edn")
        predicate (assoc (:predicate p) :predicate/digest predicate-id)
        p (assoc p :proposal/id proposal-id :predicate predicate :predicate/digest predicate-id)
        proposal-id-artifact (cas-put! store p "application/edn")
        refs (concat (:evidence/refs p) (:replay/refs p) (:adversarial/refs p))]
    ;; refs were sealed while normalizing the proposal
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
            :passed? (= 1 (:passed r))})))
(defn- verify-run-row! [store r]
  (let [v (verifying-cas store)]
    (cas/get-bytes v (:result_ref r))
    (cas/get-bytes v (:run_digest r))
    r))
(defn list-runs [store proposal-id]
  (stores store)
  (mapv (fn [r] (row->run (verify-run-row! store r)))
        (sqlite/query (:sqlite store) ["SELECT * FROM invariant_runs WHERE proposal_id = ? ORDER BY created_at, id" (str proposal-id)])))
(def ^:private caller-claim-keys
  #{:gate :gate/id :model/policy :deterministic? :fresh-model? :passed?})

(defn- result-artifact [store proposal kind raw]
  (when (some #(contains? raw %) caller-claim-keys)
    (throw (err/error :invariant/unattested-result
                      "qualification claims must be inside a verified result artifact"
                      {:reason :caller-only-claim})))
  (when-not (contains? raw :result/ref)
    (throw (err/error :invariant/unattested-result
                      "durable runs require a sealed result artifact reference"
                      {:reason :inline-result-rejected})))
  (let [result-id (ensure-ref! store (:result/ref raw))
        result (read-edn (String. (cas/get-bytes (verifying-cas store) result-id) StandardCharsets/UTF_8))]
    (when-not (map? result)
      (throw (err/error :invariant/result-invalid "result artifact is not a closed EDN envelope" {})))
    (doseq [k [:details-ref :details/ref]]
      (when-let [ref (get result k)]
        (when-not (and (string? ref) (re-matches #"^sha256:[0-9a-f]{64}$" ref))
          (throw (err/error :invariant/result-invalid "nested result reference is not a canonical digest" {:key k})))
        (cas/get-bytes (verifying-cas store) ref)))
    [result-id result]))

(defn append-run!
  "Append evidence backed by a closed result artifact. Raw caller booleans or
  arbitrary result refs never authorize a run; result refs must be sealed
  VerifiedDigest proofs."
  [store proposal-id r]
  (stores store)
  (let [p (get-proposal store proposal-id)]
    (when-not p
      (throw (err/error :invariant/proposal-missing "proposal does not exist" {:proposal/id proposal-id})))
    (when-not (map? r) (throw (err/error :invariant/run-invalid "run must be a map" {})))
    (when-not (contains? invariant/run-kinds (:kind r))
      (throw (err/error :invariant/run-kind-invalid "run kind is required" {:kind (:kind r)})))
    (let [[result-id result] (result-artifact store p (:kind r) r)
          _ (when-not (= (str proposal-id) (str (:proposal/id result)))
              (throw (err/error :invariant/result-proposal-mismatch "result envelope names another proposal" {})))
          _ (when-not (= (:predicate/digest (:predicate p)) (:predicate/digest result))
              (throw (err/error :invariant/result-predicate-mismatch "result envelope names another predicate" {})))
          _ (when-not (= (:registry/revision p) (:registry/revision result))
              (throw (err/error :invariant/result-registry-mismatch "result envelope names another registry revision" {})))
          raw (assoc (select-keys r [:run/id :kind]) :proposal/id proposal-id :result result :result-ref result-id)
          run-id (str (or (:run/id raw) (id)))
          r (invariant/run (assoc raw :run/id run-id))
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
            (assoc r :run/id run-id :result-ref result-id :run/digest run-id-artifact)))))))


(defn- current-state [store proposal-id]
  (if (seq (sqlite/query (:sqlite store) ["SELECT id FROM invariant_disables WHERE proposal_id = ? LIMIT 1" (str proposal-id)]))
    :disabled
    (if (seq (sqlite/query (:sqlite store) ["SELECT id FROM invariant_activations WHERE proposal_id = ? AND status = 'active' LIMIT 1" (str proposal-id)]))
      :active
      (let [row (first (sqlite/query (:sqlite store) ["SELECT decision FROM invariant_decisions WHERE proposal_id = ? ORDER BY created_at DESC, id DESC LIMIT 1" (str proposal-id)]))
            d (some-> row :decision str)]
        (cond (= d "approved") :approved (= d "rejected") :rejected :else nil)))))

(defn- decision [store proposal-id decision reviewer opts]
  (let [p (get-proposal store proposal-id)
        runs (list-runs store proposal-id)
        opts (reduce-kv (fn [m k v]
                          (if (#{:replay/ref :adversarial/ref} k)
                            (assoc m k (ensure-ref! store v))
                            (assoc m k v))) {} opts)
        a (assoc opts :reviewer reviewer)]
    (when-not p (throw (err/error :invariant/proposal-missing "proposal does not exist" {:proposal/id proposal-id})))
    (when (= (str reviewer) (str (:proposer p)))
      (throw (err/error :invariant/reviewer-conflict "proposer cannot reject/approve its own proposal" {})))
    (if (= :approved decision)
      (invariant/approval p a runs)
      (assoc a :proposal/id (str proposal-id) :status :rejected :activation-qualified? false))))

(defn approve!
  "Append a reviewer approval. Approval never mutates a proposal row."
  [store proposal-id reviewer opts]
  (stores store)
  (let [d (decision store proposal-id :approved reviewer opts)
        did (str (or (:decision/id d) (id)))
        digest (cas-put! store d "application/edn")]
    (sqlite/with-write-tx [conn (:sqlite store)]
      (if-let [existing (first (sqlite/query-raw! conn "SELECT * FROM invariant_decisions WHERE proposal_id = ? LIMIT 1" [(str proposal-id)]))]
        (if (= digest (:decision_digest existing))
          existing
          (throw (err/error :invariant/decision-conflict "proposal already has a terminal decision" {:proposal/id proposal-id :decision (:decision existing)})))
        (do
          (sqlite/insert-raw! conn
            "INSERT INTO invariant_decisions (id,proposal_id,decision,reviewer,decision_digest,activation_qualified,reason,created_at) VALUES (?,?,?,?,?,?,?,?)"
            [did (str proposal-id) "approved" (str reviewer) digest (if (:activation-qualified? d) 1 0) (:reason d) (now)])
          (assoc d :decision/id did :decision/digest digest))))))

(defn reject!
  [store proposal-id reviewer reason]
  (stores store)
  (let [d (decision store proposal-id :rejected reviewer {:reason (str reason)})
        did (str (id))
        digest (cas-put! store d "application/edn")]
    (sqlite/with-write-tx [conn (:sqlite store)]
      (if-let [existing (first (sqlite/query-raw! conn "SELECT * FROM invariant_decisions WHERE proposal_id = ? LIMIT 1" [(str proposal-id)]))]
        (if (= digest (:decision_digest existing))
          (assoc d :decision/id (:id existing) :decision/digest (:decision_digest existing))
          (throw (err/error :invariant/decision-conflict "proposal already has a terminal decision" {:proposal/id proposal-id :decision (:decision existing)})))
        (do
          (sqlite/insert-raw! conn
            "INSERT INTO invariant_decisions (id,proposal_id,decision,reviewer,decision_digest,activation_qualified,reason,created_at) VALUES (?,?,?,?,?,?,?,?)"
            [did (str proposal-id) "rejected" (str reviewer) digest 0 (:reason d) (now)])
          (assoc d :decision/id did :decision/digest digest))))))

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
     (when (= (str reviewer) (:proposer p)) (throw (err/error :invariant/reviewer-conflict "proposer cannot activate its own invariant" {})))
     (when-not (= 1 (:activation_qualified d)) (throw (err/error :invariant/not-qualified "activation requires deterministic replay and adversarial G3 evidence" {})))
     (let [aid (str (id))
           activation-digest (cas-put! store {:proposal/id proposal-id :version (:version p) :predicate (:predicate p) :reviewer reviewer} "application/edn")
           desc {:invariant/id (or (:invariant/id p) proposal-id) :proposal/id proposal-id :version (:version p)
                 :predicate (:predicate p) :registry/revision (:registry/revision p)
                 :decision/id (:id d) :activation/id aid :activation/digest activation-digest :activation/committed? true}]
       (let [result (sqlite/with-write-tx [conn (:sqlite store)]
                      (when (seq (sqlite/query-raw! conn "SELECT id FROM invariant_disables WHERE proposal_id = ? LIMIT 1" [(str proposal-id)]))
                        (throw (err/error :invariant/activation-disabled "disabled proposals cannot reactivate" {})))
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
                      (assoc desc :activation/id (:id (:row result)) :activation/digest (:activation_digest (:row result)) :decision/id (:decision_id (:row result)))
                      desc)]
           (when (seq (sqlite/query (:sqlite store) ["SELECT id FROM invariant_disables WHERE proposal_id = ? LIMIT 1" (str proposal-id)]))
             (throw (err/error :invariant/activation-disabled "activation was disabled before publication" {})))
           (static/publish-active-invariant! desc ((var-get #'static/activation-proof) (:activation/digest desc)))
           (assoc desc :status :active :idempotent? (:idempotent? result))))))))
(defn disable!
  [store proposal-id reviewer reason]
  (stores store)
  (let [p (get-proposal store proposal-id)]
    (when-not p (throw (err/error :invariant/proposal-missing "proposal does not exist" {})))
    (when (= (str reviewer) (str (:proposer p)))
      (throw (err/error :invariant/reviewer-conflict "proposer cannot disable its own proposal" {})))
    (let [d (invariant/disable proposal-id reviewer reason)
          digest (cas-put! store d "application/edn")
          result (sqlite/with-write-tx [conn (:sqlite store)]
                   (if-let [old (first (sqlite/query-raw! conn "SELECT * FROM invariant_disables WHERE proposal_id = ? LIMIT 1" [(str proposal-id)]))]
                     (if (= digest (:disable_digest old))
                       {:row old :idempotent? true}
                       (throw (err/error :invariant/idempotency-conflict "proposal is already disabled with different evidence" {:proposal/id proposal-id})))
                     (let [disable-id (id)]
                       (sqlite/insert-raw! conn "INSERT INTO invariant_disables (id,proposal_id,reviewer,reason,disable_digest,created_at) VALUES (?,?,?,?,?,?)" [disable-id (str proposal-id) (str reviewer) (str reason) digest (now)])
                       (doseq [row (sqlite/query-raw! conn "SELECT id FROM invariant_activations WHERE proposal_id = ? AND status = 'active'" [(str proposal-id)])]
                         (let [aid (:id row)]
                           (sqlite/insert-raw! conn "INSERT OR IGNORE INTO invariant_events (activation_id,event_type,payload_ref,created_at) VALUES (?,?,?,?)" [aid "disabled" digest (now)])
                           (when-let [ev (first (sqlite/query-raw! conn "SELECT id FROM invariant_events WHERE activation_id = ? AND event_type = 'disabled'" [aid]))]
                             (sqlite/insert-raw! conn "INSERT OR IGNORE INTO invariant_outbox (id,activation_id,event_id,event_type,dispatched,created_at) VALUES (?,?,?,?,0,?)" [(id) aid (:id ev) "disabled" (now)]))))
                       {:id disable-id :idempotent? false}))) ]
      (static/disable-active-invariant! proposal-id)
      (assoc d :decision/digest (or (:disable_digest (:row result)) digest)
             :status :disabled :idempotent? (:idempotent? result)))))

(defn- quarantine-activation! [store row]
  (try
    (sqlite/with-write-tx [conn (:sqlite store)]
      (let [aid (str (:id row))
            digest (:activation_digest row)]
        (sqlite/insert-raw! conn "INSERT OR IGNORE INTO invariant_events (activation_id,event_type,payload_ref,created_at) VALUES (?,?,?,?)" [aid "quarantined" digest (now)])
        (when-let [ev (first (sqlite/query-raw! conn "SELECT id FROM invariant_events WHERE activation_id = ? AND event_type = 'quarantined'" [aid]))]
          (sqlite/insert-raw! conn "INSERT OR IGNORE INTO invariant_outbox (id,activation_id,event_id,event_type,dispatched,created_at) VALUES (?,?,?,?,0,?)" [(id) aid (:id ev) "quarantined" (now)]))))
    (catch Exception _ nil)))

(defn recover-activations!
  "Publish only rows whose proposal, predicate, approval, CAS, event, and outbox
  evidence are all durable. Any malformed or partial row is quarantined."
  [store]
  (stores store)
  (static/clear-active-invariants!)
  (reduce (fn [out row]
            (try
              (let [p (get-proposal store (:proposal_id row))
                    approval (first (sqlite/query (:sqlite store) ["SELECT * FROM invariant_decisions WHERE id = ? AND proposal_id = ? AND decision = 'approved' AND activation_qualified = 1" (:decision_id row) (str (:proposal_id row))]))
                    activated (first (sqlite/query (:sqlite store) ["SELECT e.id FROM invariant_events e JOIN invariant_outbox o ON o.event_id = e.id AND o.activation_id = e.activation_id WHERE e.activation_id = ? AND e.event_type = 'activated' AND o.event_type = 'activated' AND e.payload_ref = ? LIMIT 1" (str (:id row)) (:activation_digest row)]))
                    pred-digest (get-in p [:predicate :predicate/digest])]
                (cas/get-bytes (verifying-cas store) (:activation_digest row))
                (when-not (and p approval activated
                               (= (:predicate_digest row) pred-digest)
                               (= (:registry_revision row) (:registry/revision p)))
                  (throw (err/error :invariant/recovery-invalid "activation evidence is incomplete or inconsistent" {:activation/id (:id row)})))
                (let [desc {:invariant/id (or (:invariant/id p) (:proposal_id row))
                            :proposal/id (:proposal_id row) :version (:version row)
                            :predicate (:predicate p) :registry/revision (:registry_revision row)
                            :decision/id (:decision_id row)
                            :activation/id (:id row) :activation/digest (:activation_digest row)}]
                  (static/publish-active-invariant! desc ((var-get #'static/activation-proof) (:activation/digest desc)))
                  (conj out {:activation/id (:id row) :status :published})))
              (catch Exception e
                (static/clear-active-invariants!)
                (quarantine-activation! store row)
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
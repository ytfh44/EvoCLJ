(ns evoclj.store.identity
  "The I1 identity tables — code_images / deployments / executions — with
  their ONE writer and the readers the hydration pin check resolves
  against.

  Identity model (migration 014):

    CodeImageId  = H(ABI, Genome, Resolution) — pure code identity,
                   shared by every deployment of the same program.
    DeploymentId = H(CodeImageId, bindings, authority) — the program bound
                   to concrete host bindings/authority.
    ExecutionId  = a fresh UUID per activation.

  WRITE DISCIPLINE: rows are written ONCE per instantiation (at the
  identity-registration point, never inside a step loop) and the writes
  are idempotent — INSERT OR IGNORE, because a content-addressed identity
  re-registered is the same identity. A FOREIGN-KEY violation is NOT
  swallowed: SQLite's ON CONFLICT clause does not apply to FOREIGN KEY
  constraints, so an execution naming an unregistered deployment raises
  and the caller fails closed."
  (:require [evoclj.store.sqlite :as sqlite])
  (:import (java.time Instant)))

(defn- now-string
  "The canonical created_at timestamp (the same ISO-8601 form every store
  column uses)."
  []
  (str (Instant/now)))

(defn- abi-text
  "The ABI as canonical EDN text (sorted top-level keys, so the same ABI
  always persists byte-identically)."
  [abi]
  (pr-str (into (sorted-map) (or abi {}))))

(defn- canonical-collection-text
  "A collection persisted as canonical EDN text (sorted by pr-str), so
  bindings/authority round-trip deterministically."
  [x]
  (pr-str (vec (sort-by pr-str (or x [])))))

(defn record-code-image!
  "Persist one CodeImage row (idempotent). `identity` keys: :code/id,
  :code/genome-id, :code/resolution-id, and the optional :abi. Returns
  the CodeImageId string."
  [db identity]
  (let [code-id (:code/id identity)]
    (sqlite/exec! (sqlite/db-spec db)
                  ["INSERT OR IGNORE INTO code_images
                      (id, abi, genome_id, resolution_id, created_at)
                    VALUES (?, ?, ?, ?, ?)"
                   (str code-id)
                   (abi-text (:abi identity))
                   (str (:code/genome-id identity))
                   (str (:code/resolution-id identity))
                   (now-string)])
    (str code-id)))

(defn record-deployment!
  "Persist one Deployment row (idempotent). `identity` keys:
  :deployment/id, :code/id, and the optional :bindings/:authority (stored
  as canonical EDN text). Returns the DeploymentId string. The
  code_images row must exist first (FK)."
  [db identity]
  (let [deployment-id (:deployment/id identity)]
    (sqlite/exec! (sqlite/db-spec db)
                  ["INSERT OR IGNORE INTO deployments
                      (id, code_image_id, bindings, authority, created_at)
                    VALUES (?, ?, ?, ?, ?)"
                   (str deployment-id)
                   (str (:code/id identity))
                   (canonical-collection-text (:bindings identity))
                   (canonical-collection-text (:authority identity))
                   (now-string)])
    (str deployment-id)))

(defn record-execution!
  "Persist one Execution row (idempotent). `identity` keys:
  :execution/id, :deployment/id, :code/id. Returns the ExecutionId
  string. The deployments and code_images rows must exist first (FK)."
  [db identity]
  (let [execution-id (:execution/id identity)]
    (sqlite/exec! (sqlite/db-spec db)
                  ["INSERT OR IGNORE INTO executions
                      (id, deployment_id, code_image_id, created_at)
                    VALUES (?, ?, ?, ?)"
                   (str execution-id)
                   (str (:deployment/id identity))
                   (str (:code/id identity))
                   (now-string)])
    (str execution-id)))

;; --- readers (the rows a session pin names) ---------------------------------

(defn code-image-for-pin
  "The code_images row named by the pin's :code/id, or nil:
  {:code/id :code/genome-id :code/resolution-id :abi :created-at}."
  [db pin]
  (when-let [code-id (:code/id pin)]
    (when-let [row (first (sqlite/query (sqlite/db-spec db)
                                        ["SELECT id, abi, genome_id, resolution_id, created_at
                                            FROM code_images WHERE id = ?"
                                         (str code-id)]))]
      {:code/id (:id row)
       :code/genome-id (:genome_id row)
       :code/resolution-id (:resolution_id row)
       :abi (:abi row)
       :created-at (:created_at row)})))

(defn deployment-for-pin
  "The deployments row named by the pin's :deployment/id, or nil:
  {:deployment/id :code/id :bindings :authority :created-at}."
  [db pin]
  (when-let [deployment-id (:deployment/id pin)]
    (when-let [row (first (sqlite/query (sqlite/db-spec db)
                                        ["SELECT id, code_image_id, bindings, authority, created_at
                                            FROM deployments WHERE id = ?"
                                         (str deployment-id)]))]
      {:deployment/id (:id row)
       :code/id (:code_image_id row)
       :bindings (:bindings row)
       :authority (:authority row)
       :created-at (:created_at row)})))

(defn execution-for-pin
  "The executions row named by the pin's :execution/id, or nil:
  {:execution/id :deployment/id :code/id :created-at}."
  [db pin]
  (when-let [execution-id (:execution/id pin)]
    (when-let [row (first (sqlite/query (sqlite/db-spec db)
                                        ["SELECT id, deployment_id, code_image_id, created_at
                                            FROM executions WHERE id = ?"
                                         (str execution-id)]))]
      {:execution/id (:id row)
       :deployment/id (:deployment_id row)
       :code/id (:code_image_id row)
       :created-at (:created_at row)})))

;; --- the fail-closed pin rule -------------------------------------------------

(defn pin-failure
  "The FAIL-CLOSED identity check for one session pin: nil when every
  identity id the pin carries has its row and each row names the pin's
  :code/id; otherwise the FIRST finding:

    {:reason :execution-row-missing | :deployment-row-missing
             | :code-image-row-missing | :code-image-mismatch
     :session/id <uuid> ... }

  A missing row is a finding, never a silent pass: a session whose
  identity was never registered cannot be executed. Read-only — the
  caller owns the error type (hydrate throws :hydrate/pin-mismatch) and
  the response (the startup scan reports)."
  [db pin]
  (let [pin-code (:code/id pin)]
    (or
     (when-let [eid (:execution/id pin)]
       (let [row (execution-for-pin db pin)]
         (cond
           (nil? row)
           {:reason :execution-row-missing
            :session/id (:session/id pin)
            :execution/id eid}
           (and pin-code (not= (:code/id row) pin-code))
           {:reason :code-image-mismatch
            :session/id (:session/id pin)
            :session/code-id pin-code
            :execution/id eid
            :execution/code-image-id (:code/id row)})))
     (when-let [did (:deployment/id pin)]
       (let [row (deployment-for-pin db pin)]
         (cond
           (nil? row)
           {:reason :deployment-row-missing
            :session/id (:session/id pin)
            :deployment/id did}
           (and pin-code (not= (:code/id row) pin-code))
           {:reason :code-image-mismatch
            :session/id (:session/id pin)
            :session/code-id pin-code
            :deployment/id did
            :deployment/code-image-id (:code/id row)})))
     (when pin-code
       (let [row (code-image-for-pin db pin)]
         (cond
           (nil? row)
           {:reason :code-image-row-missing
            :session/id (:session/id pin)
            :session/code-id pin-code}
           (and (:genome/id pin) (not= (:code/genome-id row) (:genome/id pin)))
           {:reason :code-image-mismatch
            :session/id (:session/id pin)
            :pin pin :row row}
           (and (:resolution/id pin)
                (not= (:code/resolution-id row) (:resolution/id pin)))
           {:reason :code-image-mismatch
            :session/id (:session/id pin)
            :pin pin :row row}))))))

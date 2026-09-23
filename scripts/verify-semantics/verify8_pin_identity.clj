(ns verify8-pin-identity
  "Semantic verification #8 — the I1 identity tables and the fail-closed pin.

  Model: a session pin is {session/id, code/id, deployment/id, execution/id}
  and the identity relation is a conjunction of existence and agreement
  constraints over three tables:

      executions.deployment_id = pin.deployment/id
      executions.code_image_id = pin.code/id          (agreement)
      deployments.code_image_id = pin.code/id         (agreement)
      code_images.id           = pin.code/id          (agreement)
      every named row EXISTS                          (existence)

  The admission decision is `accept  <=>  all conjuncts hold`. The model is
  finite (four ids, three tables, one row each), so the script enumerates the
  two failure modes that matter — a MISSING row and a DISAGREEING row — and
  proves both are rejected rather than silently admitted (fail-closed).

  Real code: evoclj.store.identity (record-*! writers + pin-failure rule),
  evoclj.store.session-store/insert-session! (the identity columns on the
  sessions row), evoclj.runtime.hydrate/verify-pin! (the admission gate)."
  (:require [clojure.java.jdbc :as jdbc]
            [evoclj.runtime.hydrate :as hydrate]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.identity :as identity]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.session-store :as ss]
            [evoclj.store.sqlite :as sqlite]))

(defn check! [label ok detail]
  (println (if ok "PASS" "FAIL") "|" label "|" detail)
  (when-not ok (System/exit 1)))

(def hex64 (apply str (repeat 64 "a")))
(def hex64b (apply str (repeat 64 "b")))
(def hex64c (apply str (repeat 64 "c")))
(def genome-id (str "sha256:" hex64))
(def resolution-id (str "sha256:" hex64b))
(def code-id (str "sha256:" hex64c))
(def code-id-2 (str "sha256:" (apply str (repeat 64 "d"))))
(def deployment-id (str "sha256:" (apply str (repeat 64 "e"))))
(def deployment-id-2 (str "sha256:" (apply str (repeat 64 "f"))))
(def generation-id "G1")
(def now "2025-01-01T00:00:00Z")

(defn- error-type
  "The :error/type of the typed ExceptionInfo thrown by `f`, or nil."
  [f]
  (:error/type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo e e)))))

(defn- count-rows
  [db sql & params]
  (:n (first (sqlite/query db (into [sql] params)))))

(let [p (str (java.nio.file.Files/createTempFile "verify-identity-" ".db"
                                                 (make-array java.nio.file.attribute.FileAttribute 0)))
      db (sqlite/spec p)]
  (try
    (migrate/migrate! db)
    ;; FK targets: generations.genome_id -> genomes(id) -> artifacts(hash),
    ;; generations.resolution_id -> artifacts(hash), sessions.code_image_id ->
    ;; code_images(id) -> artifacts(hash) (migrations 009/011/014).
    (doseq [h [hex64 hex64b hex64c (apply str (repeat 64 "d"))
               (apply str (repeat 64 "e")) (apply str (repeat 64 "f"))]]
      (artifact/ensure-artifact! db (str "sha256:" h) "application/octet-stream" 0))
    (artifact/ensure-genome! db genome-id)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations
                    {:id generation-id :genome_id genome-id
                     :resolution_id resolution-id :parent_id nil
                     :state "active" :current 1 :created_at now}))

    ;; --- (a) the three writers persist and are idempotent --------------------
    (let [code-image {:code/id code-id :code/genome-id genome-id
                      :code/resolution-id resolution-id
                      :abi {:kernel 1 :genome 1 :intent 1 :tool 1}}
          deployment {:deployment/id deployment-id :code/id code-id
                      :bindings [{:binding/id (random-uuid) :logical/id [:skill "x"]
                                  :revision/id resolution-id}]
                      :authority [{:kind :tool :id :fixture/echo}]}
          execution {:execution/id (random-uuid) :deployment/id (:deployment/id deployment)
                     :code/id code-id}]
      (identity/record-code-image! db code-image)
      (identity/record-deployment! db deployment)
      (identity/record-execution! db execution)
      (doseq [_ (range 2)]                       ; replay: idempotent
        (identity/record-code-image! db code-image)
        (identity/record-deployment! db deployment)
        (identity/record-execution! db execution))
      (check! "code_images row written once"
              (= 1 (count-rows db "SELECT COUNT(*) AS n FROM code_images WHERE id = ?" code-id))
              "record-code-image! is idempotent (INSERT OR IGNORE)")
      (check! "deployments row written once"
              (= 1 (count-rows db "SELECT COUNT(*) AS n FROM deployments WHERE id = ?"
                               (str (:deployment/id deployment))))
              "record-deployment! is idempotent")
      (check! "executions row written once"
              (= 1 (count-rows db "SELECT COUNT(*) AS n FROM executions WHERE id = ?"
                               (str (:execution/id execution))))
              "record-execution! is idempotent")
      (check! "readers round-trip the rows the pin names"
              (and (= code-id (:code/id (identity/code-image-for-pin db {:code/id code-id})))
                   (= code-id (:code/id (identity/deployment-for-pin db deployment)))
                   (= code-id (:code/id (identity/execution-for-pin db execution))))
              "code-image-for-pin / deployment-for-pin / execution-for-pin")

      ;; --- (b) the session row carries the identity triple -------------------
      (let [store (ss/make-session-store db)
            created (session/create-session!
                     store {:generation/id generation-id
                            :genome/id genome-id
                            :resolution/id resolution-id
                            :code/id code-id
                            :deployment/id (:deployment/id deployment)
                            :execution/id (:execution/id execution)})
            sid (:session/id created)
            row (first (sqlite/query db ["SELECT code_image_id, deployment_id, execution_id
                                           FROM sessions WHERE id = ?" (str sid)]))
            found (session/find-session store sid)]
        (check! "sessions row carries code_image_id"
                (= code-id (:code_image_id row))
                "the pinned CodeImageId is persisted on the session row")
        (check! "sessions row carries deployment_id + execution_id"
                (and (= (str (:deployment/id deployment)) (:deployment_id row))
                     (= (str (:execution/id execution)) (:execution_id row)))
                "the I1 identity triple is persisted, not just the code id")
        (check! "row->session exposes the identity triple"
                (and (= code-id (:code/id found))
                     (= (str (:deployment/id deployment)) (:deployment/id found))
                     (= (:execution/id execution) (:execution/id found)))
                "the session contract reads back :code/id/:deployment/id/:execution/id")

        ;; --- (c) a consistent pin is admitted, a disagreeing one rejected ----
        (let [pin {:session/id sid :code/id code-id
                   :deployment/id (:deployment/id deployment)
                   :execution/id (:execution/id execution)}
              verified (hydrate/verify-pin! db pin)]
          (check! "a consistent pin is admitted"
                  (= code-id (:code/id verified))
                  "hydrate/verify-pin! returns the resolved pin"))

        ;; A second CodeImage + Deployment + Execution whose execution row
        ;; names a DIFFERENT code image than the pin does.
        (let [code-image-2 {:code/id code-id-2 :code/genome-id genome-id
                            :code/resolution-id resolution-id :abi {}}
              deployment-2 {:deployment/id deployment-id-2 :code/id code-id-2
                            :bindings [] :authority []}
              execution-2 {:execution/id (random-uuid)
                           :deployment/id (:deployment/id deployment-2)
                           :code/id code-id-2}]
          (identity/record-code-image! db code-image-2)
          (identity/record-deployment! db deployment-2)
          (identity/record-execution! db execution-2)
          (let [disagreeing {:session/id sid :code/id code-id
                             :deployment/id (:deployment/id deployment-2)
                             :execution/id (:execution/id execution-2)}
                t (error-type #(hydrate/verify-pin! db disagreeing))]
            (check! "a pin whose rows disagree is rejected with :hydrate/pin-mismatch"
                    (= :hydrate/pin-mismatch t)
                    (str "deployments/executions name code image " code-id-2
                         " while the pin names " code-id)))

          ;; --- (d) a MISSING row is rejected too (fail-closed) ---------------
          (let [missing-execution {:session/id sid :code/id code-id
                                   :deployment/id (:deployment/id deployment)
                                   :execution/id (random-uuid)}
                t (error-type #(hydrate/verify-pin! db missing-execution))]
            (check! "a pin naming an unregistered execution is rejected"
                    (= :hydrate/pin-mismatch t)
                    "a session whose identity was never registered cannot run"))

          (let [mismatched-code {:session/id sid :code/id code-id
                                 :deployment/id (:deployment/id deployment-2)
                                 :execution/id (:execution/id execution-2)}
                t (error-type #(hydrate/verify-pin! db mismatched-code))]
            (check! "agreement is checked in both directions"
                    (= :hydrate/pin-mismatch t)
                    "the second identity cannot stand in for the pinned code image")))))

    (finally
      (java.nio.file.Files/deleteIfExists
       (java.nio.file.Paths/get p (make-array String 0))))))
(println "VERIFY8 DONE")

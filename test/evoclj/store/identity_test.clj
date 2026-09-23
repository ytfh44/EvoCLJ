(ns evoclj.store.identity-test
  "I1 identity-table tests: code_images / deployments / executions are
  written ONCE per instantiation, idempotently, and the session row
  carries the identity triple the hydrate pin check authenticates."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.cli.session :as cli-session]
            [evoclj.compiler.core :as compiler]
            [evoclj.genome.load :as load]
            [evoclj.runtime.hydrate :as hydrate]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.genome :as genome-store]
            [evoclj.store.identity :as identity]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.recovery :as recovery]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite]))

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private gen "generation-1")
(def ^:private genome (str "sha256:" (apply str (repeat 64 "a"))))
(def ^:private resolution (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private code-id (str "sha256:" (apply str (repeat 64 "d"))))
(def ^:private deployment-id (str "sha256:" (apply str (repeat 64 "e"))))
(def ^:private execution-id #uuid "00000000-0000-0000-0000-0000000000ff")

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [f (java.io.File/createTempFile "evoclj-identity-test-" ".db")
        p (.getAbsolutePath f)]
    (.delete f)
    (swap! db-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (try
      (java.nio.file.Files/deleteIfExists
       (java.nio.file.Paths/get p (make-array String 0)))
      (catch Exception _ nil)))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (try (f) (finally (cleanup!)))))

(defn- fresh-db []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- seed-generation! [db]
  (artifact/ensure-artifact! db genome "application/octet-stream" 0)
  (artifact/ensure-artifact! db resolution "application/edn" 0)
  (artifact/ensure-artifact! db phenotype "application/edn" 0)
  ;; the sessions.phenotype_id column (the frozen CodeImageId column, FK to
  ;; artifacts) must resolve for every pinned code id the fixtures use
  (artifact/ensure-artifact! db code-id "application/edn" 0)
  (artifact/ensure-genome! db genome)
  (sqlite/with-db [conn db]
    (jdbc/insert! conn :generations
                  {:id gen
                   :genome_id genome
                   :resolution_id resolution
                   :parent_id nil
                   :state "active"
                   :current 1
                   :created_at now})))

(defn- code-identity []
  {:code/id code-id
   :code/genome-id genome
   :code/resolution-id resolution
   :abi {:kernel 1 :genome 1 :intent 1 :tool 1}})

(defn- deployment-identity []
  {:deployment/id deployment-id
   :code/id code-id
   :bindings [{:binding/id "b"} {:binding/id "a"}]
   :authority [{:cap/id "c"}]})

(defn- execution-identity []
  {:execution/id execution-id
   :deployment/id deployment-id
   :code/id code-id})

(defn- count-rows [db table]
  (:n (first (sqlite/query db [(str "SELECT COUNT(*) AS n FROM " table)]))))

(deftest identity-rows-are-written-once-and-idempotently
  (let [db (fresh-db)
        _ (seed-generation! db)]
    (testing "the three rows are written by the identity writer"
      (is (= code-id (identity/record-code-image! db (code-identity))))
      (is (= deployment-id (identity/record-deployment! db (deployment-identity))))
      (is (= (str execution-id) (identity/record-execution! db (execution-identity))))
      (is (= 1 (count-rows db "code_images")))
      (is (= 1 (count-rows db "deployments")))
      (is (= 1 (count-rows db "executions"))))
    (testing "re-registering the SAME identity is a no-op (content-addressed)"
      (identity/record-code-image! db (code-identity))
      (identity/record-deployment! db (deployment-identity))
      (identity/record-execution! db (execution-identity))
      (is (= 1 (count-rows db "code_images")))
      (is (= 1 (count-rows db "deployments")))
      (is (= 1 (count-rows db "executions"))))
    (testing "the persisted columns carry the identity"
      (let [row (first (sqlite/query db ["SELECT * FROM code_images WHERE id = ?"
                                         code-id]))]
        (is (= genome (:genome_id row)))
        (is (= resolution (:resolution_id row)))
        (is (= "{:genome 1, :intent 1, :kernel 1, :tool 1}" (:abi row))))
      (let [row (first (sqlite/query db ["SELECT * FROM deployments WHERE id = ?"
                                         deployment-id]))]
        (is (= code-id (:code_image_id row)))
        (is (= (vec (sort-by pr-str [{:binding/id "b"} {:binding/id "a"}]))
               (edn/read-string (:bindings row)))
            "bindings round-trip as EDN data")
        (is (= (pr-str (vec (sort-by pr-str [{:binding/id "b"} {:binding/id "a"}])))
               (:bindings row))
            "bindings persist in canonical (sorted) form")
        (is (= (pr-str (vec (sort-by pr-str [{:cap/id "c"}])))
               (:authority row))))
      (let [row (first (sqlite/query db ["SELECT * FROM executions WHERE id = ?"
                                         (str execution-id)]))]
        (is (= deployment-id (:deployment_id row)))
        (is (= code-id (:code_image_id row)))))))

(deftest readers-resolve-the-rows-a-pin-names
  (let [db (fresh-db)
        _ (seed-generation! db)]
    (identity/record-code-image! db (code-identity))
    (identity/record-deployment! db (deployment-identity))
    (identity/record-execution! db (execution-identity))
    (testing "the rows come back by the pin's ids"
      (is (= code-id (:code/id (identity/code-image-for-pin db {:code/id code-id}))))
      (is (= genome (:code/genome-id (identity/code-image-for-pin db {:code/id code-id}))))
      (is (= deployment-id (:deployment/id
                            (identity/deployment-for-pin db {:deployment/id deployment-id}))))
      (is (= code-id (:code/id
                      (identity/deployment-for-pin db {:deployment/id deployment-id}))))
      (is (= (str execution-id)
             (:execution/id (identity/execution-for-pin db {:execution/id execution-id}))))
      (is (= code-id (:code/id
                      (identity/execution-for-pin db {:execution/id execution-id})))))
    (testing "an unknown id is nil (never a fabricated row)"
      (is (nil? (identity/code-image-for-pin db {:code/id (str "sha256:" (apply str (repeat 64 "9")))})))
      (is (nil? (identity/deployment-for-pin db {:deployment/id (str "sha256:" (apply str (repeat 64 "9")))})))
      (is (nil? (identity/execution-for-pin db {:execution/id (java.util.UUID/randomUUID)})))
      (is (nil? (identity/execution-for-pin db {}))))))

(deftest execution-without-its-deployment-fails-closed
  ;; ON CONFLICT (INSERT OR IGNORE) does NOT apply to FOREIGN KEY
  ;; constraints: an execution naming an unregistered deployment must
  ;; raise, never be silently skipped.
  (let [db (fresh-db)
        _ (seed-generation! db)]
    (is (thrown? Exception
                 (identity/record-execution!
                  db {:execution/id (java.util.UUID/randomUUID)
                      :deployment/id deployment-id
                      :code/id code-id})))
    (is (zero? (count-rows db "executions")))))

(defn- fixture-loaded
  "The minimal-valid bundle with the seed route program attached (the
  same shape evoclj.cli.session/load-genome-for-execution produces)."
  []
  (assoc (load/load-genome
          (.toPath (io/file (io/resource "fixtures/genomes/minimal-valid"))))
         :programs [{:program/id :program/route
                     :file "programs/route.clj"
                     :entry 'agent.route/run
                     :input-schema :schema/route-input
                     :output-schema :schema/intent-or-route}]))

(deftest cli-registration-satisfies-the-hydration-preconditions
  ;; The CLI identity-registration point must leave the store in the state
  ;; hydration needs: the loaded bundle registered (H1) and the I1 identity
  ;; rows written — so hydrate re-compiles the REAL program (never the
  ;; synthetic fallback) and its pin check has rows to authenticate.
  (let [db (fresh-db)
        cas-dir (str (java.nio.file.Files/createTempDirectory
                      "evoclj-identity-cas-"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
        cas-store (cas/->cas cas-dir)
        loaded (fixture-loaded)
        compiled (compiler/compile-genome loaded cli-session/provider-catalog)
        gid (:code/genome-id compiled)
        rid (:code/resolution-id compiled)
        cid (:code/id compiled)
        did (:deployment/id compiled)
        eid (:execution/id compiled)
        identity {:generation/id gen
                  :genome/id gid
                  :resolution/id rid
                  :code/id cid
                  :deployment/id did
                  :execution/id eid
                  :abi (:abi compiled)}]
    (cli-session/ensure-identity-artifacts! {:store/sqlite db :store/cas cas-store}
                                            identity loaded)
    (testing "the loaded bundle is durable (the H1 precondition)"
      (let [persisted (genome-store/loaded-genome cas-store db gid)]
        (is (some? persisted))
        (is (= [gid] (mapv :genome/id [(:loaded persisted)])))
        (is (contains? (:loaded persisted) :programs))))
    (testing "the identity rows exist for the compiled identity"
      (is (= cid (:code/id (identity/code-image-for-pin db {:code/id cid}))))
      (is (= did (:deployment/id (identity/deployment-for-pin db {:deployment/id did}))))
      (is (= cid (:code/id (identity/execution-for-pin db {:execution/id eid})))))
    (testing "hydration re-compiles the REAL program through the written rows"
      (sqlite/with-db [conn db]
        (jdbc/insert! conn :generations
                      {:id gen
                       :genome_id gid
                       :resolution_id rid
                       :parent_id nil
                       :state "active"
                       :current 1
                       :created_at now}))
      (let [s (session/create-session! db
                                       {:generation/id gen
                                        :genome/id gid
                                        :resolution/id rid
                                        :code/id cid
                                        :deployment/id did
                                        :execution/id eid})
            handle (hydrate/hydrate {:sqlite db :cas cas-store} (:session/id s))
            hc (:compiled handle)]
        (is (= gid (:code/genome-id hc)))
        (is (= cid (:code/id hc)))
        (is (= :graph/main (:graph/id (:topology hc)))
            "the real compiled topology, not the fallback echo")
        (is (not= :graph/subagent-echo (:graph/id (:topology hc))))))))

(deftest recovery-scan-reports-identity-failures
  ;; The startup scan classifies unauthenticated pins by reason (read-only;
  ;; it never repairs) so an operator can tell a never-registered identity
  ;; from a real mismatch.
  (let [db (fresh-db)
        _ (seed-generation! db)
        cas-store (cas/->cas (str (java.nio.file.Files/createTempDirectory
                                   "evoclj-identity-cas-"
                                   (make-array java.nio.file.attribute.FileAttribute 0))))
        unregistered-code (str "sha256:" (apply str (repeat 64 "7")))
        ;; the artifacts row exists (the sessions.phenotype_id FK) but the
        ;; code_images row does NOT — that is the failure the scan reports
        _ (artifact/ensure-artifact! db unregistered-code "application/edn" 0)
        bad (session/create-session! db {:generation/id gen
                                         :genome/id genome
                                         :resolution/id resolution
                                         :code/id unregistered-code})
        _ (identity/record-code-image! db (code-identity))
        _ (identity/record-deployment! db (deployment-identity))
        _ (identity/record-execution! db (execution-identity))
        good (session/create-session! db {:generation/id gen
                                          :genome/id genome
                                          :resolution/id resolution
                                          :code/id code-id
                                          :deployment/id deployment-id
                                          :execution/id execution-id})
        report (recovery/scan-recovery-state db cas-store)
        failures (:identity-failures report)]
    (testing "the unauthenticated session is reported with its reason"
      (is (= [{:session/id (str (:session/id bad))
               :session/code-id unregistered-code
               :reason :code-image-row-missing}]
             failures)))
    (testing "the authenticated session is not reported"
      (is (not-any? #(= (str (:session/id good)) (:session/id %)) failures)))
    (testing "the scan stays read-only (the historical categories survive)"
      (is (contains? report :missing-artifacts))
      (is (contains? report :invalid-event-chains))
      (is (contains? report :stale-candidates)))))

(deftest session-row-carries-the-identity-triple
  (let [db (fresh-db)
        _ (seed-generation! db)
        _ (identity/record-code-image! db (code-identity))
        _ (identity/record-deployment! db (deployment-identity))
        _ (identity/record-execution! db (execution-identity))
        s (session/create-session! db
                                   {:generation/id gen
                                    :genome/id genome
                                    :resolution/id resolution
                                    :code/id code-id
                                    :deployment/id deployment-id
                                    :execution/id execution-id})
        row (first (sqlite/query db ["SELECT * FROM sessions WHERE id = ?"
                                     (str (:session/id s))]))]
    (testing "the columns are written"
      (is (= code-id (:code_image_id row)))
      (is (= deployment-id (:deployment_id row)))
      (is (= (str execution-id) (:execution_id row))))
    (testing "the public Session contract exposes the identity triple"
      (is (= code-id (:code/id s)))
      (is (= deployment-id (:deployment/id s)))
      (is (= execution-id (:execution/id s)))
      (is (= s (session/get-session db (:session/id s)))))
    (testing "a session pinned only to its code id omits the optional ids"
      (let [s2 (session/create-session! db
                                        {:generation/id gen
                                         :genome/id genome
                                         :resolution/id resolution
                                         :code/id phenotype})
            row2 (first (sqlite/query db ["SELECT * FROM sessions WHERE id = ?"
                                          (str (:session/id s2))]))]
        (is (= phenotype (:code_image_id row2)))
        (is (nil? (:deployment_id row2)))
        (is (nil? (:execution_id row2)))
        (is (= phenotype (:code/id s2)))
        (is (not (contains? s2 :deployment/id)))))))

(ns evoclj.store.capability-table-test
  "P7 — capabilities table (migration 013) and store helpers.

  Three normative checks:
    1. insert valid lease row succeeds and fetch returns same
    2. insert with expires <= issued fails CHECK
    3. revoked update flips flag and fetch shows revoked

  Plus FK / CHECK membership / index coverage."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as jdbc]
            [evoclj.store.capability-store :as cap-store]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

(def ^:private now "2025-01-01T00:00:00Z")
(def ^:private later "2025-01-01T01:00:00Z")
(def ^:private earlier "2024-12-31T23:00:00Z")

(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [p (str (java.nio.file.Files/createTempFile
                "evoclj-cap-" ".db"
                (make-array java.nio.file.attribute.FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (java.nio.file.Files/deleteIfExists
     (java.nio.file.Paths/get p (make-array String 0))))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- fresh-db []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    db))

(defn- insert! [db table row]
  (sqlite/with-db [conn db]
    (jdbc/insert! conn table row)))

(defn- insert-generation! [db gid]
  (let [gid-hash (str "sha256:" (apply str (repeat 64 "a")))
        rid "resolution-1"]
    (try (insert! db :artifacts {:hash gid-hash :media_type "application/octet-stream" :size 64 :created_at now}) (catch Exception _ nil))
    (try (insert! db :artifacts {:hash rid :media_type "application/edn" :size 64 :created_at now}) (catch Exception _ nil))
    (try (insert! db :genomes {:id gid-hash :created_at now}) (catch Exception _ nil))
    (insert! db :generations
             {:id gid
              :genome_id gid-hash
              :resolution_id rid
              :parent_id nil
              :state "active"
              :current 0
              :created_at now})))

(defn- insert-session! [db sid gid]
  (try (insert! db :artifacts {:hash (str "sha256:" (apply str (repeat 64 "a"))) :media_type "application/octet-stream" :size 0 :created_at now}) (catch Exception _ nil))
  (try (insert! db :artifacts {:hash "resolution-1" :media_type "application/octet-stream" :size 0 :created_at now}) (catch Exception _ nil))
  (try (insert! db :artifacts {:hash "phenotype-1" :media_type "application/octet-stream" :size 0 :created_at now}) (catch Exception _ nil))
  (try (insert! db :genomes {:id (str "sha256:" (apply str (repeat 64 "a"))) :created_at now}) (catch Exception _ nil))
  (insert! db :sessions
           {:id sid
            :generation_id gid
            :genome_id (str "sha256:" (apply str (repeat 64 "a")))
            :resolution_id "resolution-1"
            :phenotype_id "phenotype-1"
            :state "created"
            :created_at now}))

(defn- valid-cap [id sess-id]
  {:id id
   :subject-session-id sess-id
   :subject-phenotype-id (str "sha256:" (apply str (repeat 64 "e")))
   :resource-kind "tool"
   :resource-id "tool-1"
   :actions ["invoke" "read"]
   :constraints {:maxCalls 3}
   :issued-at now
   :expires-at later
   :created-at now})

;; ===========================================================================
;; 1. insert valid lease row succeeds and fetch returns same
;; ===========================================================================

(deftest insert-valid-lease-succeeds-and-fetch-roundtrips
  (let [db (fresh-db)
        gid "gen-cap-1"
        sid "sess-cap-1"]
    (insert-generation! db gid)
    (insert-session! db sid gid)
    (let [cap-id "cap-1"
          row (cap-store/insert-capability! db (valid-cap cap-id sid))
          fetched (cap-store/fetch-capability db cap-id)]
      (is (some? row))
      (is (some? fetched))
      (is (= cap-id (:id fetched)))
      (is (= sid (:subject-session-id fetched)))
      (is (= "tool" (:resource-kind fetched)))
      (is (= ["invoke" "read"] (:actions fetched)))
      (is (= false (:revoked fetched)))
      (is (= now (:issued-at fetched)))
      (is (= later (:expires-at fetched))))))

;; ===========================================================================
;; 2. insert with expires <= issued fails CHECK
;; ===========================================================================

(deftest expires-not-after-issued-fails-check
  (let [db (fresh-db)
        gid "gen-cap-2"
        sid "sess-cap-2"]
    (insert-generation! db gid)
    (insert-session! db sid gid)
    (testing "expires == issued violates CHECK(expires_at > issued_at)"
      (is (thrown? Exception
                   (cap-store/insert-capability! db
                                                 (assoc (valid-cap "cap-2" sid)
                                                        :issued-at now
                                                        :expires-at now)))))
    (testing "expires < issued violates CHECK"
      (is (thrown? Exception
                   (cap-store/insert-capability! db
                                                 (assoc (valid-cap "cap-3" sid)
                                                        :issued-at later
                                                        :expires-at earlier)))))))

;; ===========================================================================
;; 3. revoked update flips flag and fetch shows revoked
;; ===========================================================================

(deftest revoke-flips-flag
  (let [db (fresh-db)
        gid "gen-cap-3"
        sid "sess-cap-3"]
    (insert-generation! db gid)
    (insert-session! db sid gid)
    (cap-store/insert-capability! db (valid-cap "cap-4" sid))
    (is (= false (:revoked (cap-store/fetch-capability db "cap-4"))))
    (cap-store/revoke-capability! db "cap-4")
    (let [fetched (cap-store/fetch-capability db "cap-4")]
      (is (= true (:revoked fetched)))
      (is (= 1 (:revoked-raw fetched))))
    (testing "revoke is idempotent"
      (cap-store/revoke-capability! db "cap-4")
      (is (= true (:revoked (cap-store/fetch-capability db "cap-4")))))))

;; ===========================================================================
;; Additional P7 invariants
;; ===========================================================================

;; C1: the resource_kind vocabulary is OPEN — the DB CHECK was removed
;; (definition lives in the ResourceKindDescriptor registry). An arbitrary
;; kind persists and round-trips; authorization fail-closure is the broker's
;; job (unknown kind -> deny), never the store's.
(deftest resource-kind-open-vocabulary-not-closed-set
  (let [db (fresh-db)
        gid "gen-cap-4"
        sid "sess-cap-4"]
    (insert-generation! db gid)
    (insert-session! db sid gid)
    (testing "membership is enforced in the Descriptor registry, not the DB"
      (let [row (cap-store/insert-capability! db
                                               (assoc (valid-cap "cap-custom-kind" sid)
                                                      :resource-kind "unicorn"
                                                      :resource-id "horn-1"))
            fetched (cap-store/fetch-capability db "cap-custom-kind")]
        (is (some? row))
        (is (= "unicorn" (:resource-kind fetched)))
        (is (some? (:resource-edn fetched)))))))

(deftest actions-non-empty-enforced
  (let [db (fresh-db)
        gid "gen-cap-5"
        sid "sess-cap-5"]
    (insert-generation! db gid)
    (insert-session! db sid gid)
    (is (thrown? Exception
                 (cap-store/insert-capability! db
                                               (assoc (valid-cap "cap-empty-actions" sid)
                                                      :actions []))))))

;; I2/C1: the capabilities table has no FK to sessions — the subject is a
;; Principal tagged union (job/eval/operator need no session), so a lease for
;; a non-existent session persists. Fail-closed authorization is decided at
;; verify time, not by the store.
(deftest capabilities-are-principal-scoped-not-fk-bound
  (let [db (fresh-db)
        gid "gen-cap-6"
        sid "sess-cap-6"]
    (insert-generation! db gid)
    (insert-session! db sid gid)
    (testing "a lease with an unknown session principal persists (no FK)"
      (let [row (cap-store/insert-capability! db
                                               (assoc (valid-cap "cap-fk" "no-such-session")
                                                      :subject-session-id "no-such-session"))]
        (is (some? row))
        (is (= "no-such-session" (:subject-session-id
                                  (cap-store/fetch-capability db "cap-fk"))))))))

(deftest indexes-exist
  (let [db (fresh-db)]
    (let [idx-names (set (map :name (sqlite/query db ["SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='capabilities'"])))]
      (is (contains? idx-names "capabilities_subject_session_idx"))
      (is (contains? idx-names "capabilities_resource_kind_idx"))
      (is (contains? idx-names "capabilities_revoked_idx")))))

(deftest table-has-expected-columns
  (let [db (fresh-db)
        cols (set (map :name (sqlite/query db ["SELECT name FROM pragma_table_info('capabilities')"])))]
    (is (contains? cols "id"))
    (is (contains? cols "subject_session_id"))
    (is (contains? cols "subject_phenotype_id"))
    (is (contains? cols "resource_kind"))
    (is (contains? cols "resource_id"))
    (is (contains? cols "resource_edn"))
    (is (contains? cols "actions"))
    (is (contains? cols "constraints"))
    (is (contains? cols "issued_at"))
    (is (contains? cols "expires_at"))
    (is (contains? cols "revoked"))
    (is (contains? cols "created_at"))))

(ns evoclj.genome.types-test
  "Tests for ID conventions and validated value helpers (component).

  Content-addressed IDs (genome, resolution, artifact) are canonical
  strings of the form \"sha256:<64 lowercase hex>\". Session and intent
  IDs are UUIDs, accepted as #uuid values or their canonical string
  representation."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.genome.types :as types]))

(def ^:private hex64 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(deftest genome-id-format
  (is (types/genome-id? (str "sha256:" hex64)))
  (is (not (types/genome-id? "G42"))))

(deftest sha256-id-predicates
  (testing "valid canonical sha256 ids are accepted"
    (is (types/genome-id? (str "sha256:" hex64)))
    (is (types/resolution-id? (str "sha256:" hex64)))
    (is (types/artifact-id? (str "sha256:" hex64))))
  (testing "wrong prefix, length, hex, and non-strings are rejected"
    (is (not (types/genome-id? (str "md5:" hex64))))
    (is (not (types/genome-id? (str "sha256:" (subs hex64 1)))))
    (is (not (types/genome-id? (str "sha256:" (apply str (repeat 64 \g))))))
    (is (not (types/genome-id? "")))
    (is (not (types/genome-id? 42)))
    (is (not (types/genome-id? nil)))))

(deftest uuid-id-predicates
  (let [uuid-str "00112233-4455-6677-8899-aabbccddeeff"]
    (testing "uuid strings and #uuid values are accepted"
      (is (types/session-id? uuid-str))
      (is (types/intent-id? uuid-str))
      (is (types/session-id? #uuid "00112233-4455-6677-8899-aabbccddeeff"))
      (is (types/intent-id? #uuid "00112233-4455-6677-8899-aabbccddeeff")))
    (testing "non-uuid values are rejected"
      (is (not (types/session-id? "not-a-uuid")))
      (is (not (types/session-id? "")))
      (is (not (types/session-id? 42)))
      (is (not (types/session-id? nil)))
      (is (not (types/session-id? (str "sha256:" hex64))))
      (is (not (types/intent-id? "not-a-uuid"))))))

(deftest id-constructors-validate-and-normalize
  (testing "sha256 constructors return the canonical string unchanged"
    (let [id (str "sha256:" hex64)]
      (is (= id (types/genome-id id)))
      (is (= id (types/resolution-id id)))
      (is (= id (types/artifact-id id)))))
  (testing "uuid constructors accept #uuid and canonicalize strings"
    (is (= #uuid "00112233-4455-6677-8899-aabbccddeeff"
           (types/session-id "00112233-4455-6677-8899-aabbccddeeff")))
    (is (= #uuid "00112233-4455-6677-8899-aabbccddeeff"
           (types/intent-id #uuid "00112233-4455-6677-8899-aabbccddeeff")))))

(deftest id-constructors-throw-typed-errors
  (doseq [[f kind] [[types/genome-id :genome/id]
                    [types/resolution-id :resolution/id]
                    [types/artifact-id :artifact/id]
                    [types/session-id :session/id]
                    [types/intent-id :intent/id]]]
    (let [e (try (f "nope")
                 nil
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (= :id/invalid (:error/type (ex-data e))))
      (is (= kind (:id/kind (ex-data e)))))))

(ns evoclj.evolution.diagnostic-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.evolution.diagnostic :as diagnostic]
            [evoclj.evolution.diagnosis-schema :as diagnosis]
            [evoclj.evolution.evidence-schema :as evidence]
            [evoclj.store.cas :as cas]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path)))

(def ^:private hash64
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")

(def ^:private artifact-id (str "sha256:" hash64))

(def ^:private evidence-pack
  {:evidence/id artifact-id
   :generation/id "generation-1"
   :cutoff-event-id 12
   :episodes []
   :summary {}})

(def ^:private diagnosis-result
  {:diagnosis/id artifact-id
   :evidence/id artifact-id
   :hypotheses []})

(def ^:private details
  {:producer {:kind :clj-kondo :version "2026.09"}
   :subject {:artifact/revision artifact-id
             :workspace/id "workspace-1"}
   :findings []
   :exit-status 0
   :status :complete
   :captured-at (java.util.Date. 0)})

(defn- thrown-error-type
  [f]
  (:error/type
   (ex-data
    (try
      (f)
      nil
      (catch clojure.lang.ExceptionInfo e e)))))

(defn- delete-tree!
  [^Path root]
  (when (Files/exists root (make-array java.nio.file.LinkOption 0))
    (with-open [paths (Files/walk root
                                  (make-array java.nio.file.FileVisitOption 0))]
      (doseq [path (reverse (sort-by str (iterator-seq (.iterator paths))))]
        (Files/deleteIfExists ^Path path)))))

(deftest build-bundle-carries-provenance
  (let [bundle (diagnostic/build-bundle evidence-pack diagnosis-result details)]
    (is (map? (diagnosis/validate-diagnosis diagnosis-result)))
    (is (map? (evidence/validate-pack evidence-pack)))
    (is (= artifact-id (:evidence/id bundle)))
    (is (= artifact-id (:diagnosis/id bundle)))
    (is (re-matches #"^sha256:[0-9a-f]{64}$" (:diagnostic/id bundle)))))

(deftest build-bundle-is-deterministic
  (is (= (diagnostic/build-bundle evidence-pack diagnosis-result details)
         (diagnostic/build-bundle evidence-pack diagnosis-result details))))

(deftest build-bundle-rejects-provenance-mismatch
  (let [other-diagnosis (assoc diagnosis-result
                               :evidence/id
                               (str "sha256:" (apply str (repeat 64 "b"))))]
    (is (= :diagnostic/provenance-mismatch
           (thrown-error-type
            #(diagnostic/build-bundle evidence-pack other-diagnosis details))))))

(deftest build-bundle-validates-capture-details
  (is (= :diagnostic/bundle-invalid
         (thrown-error-type
          #(diagnostic/build-bundle evidence-pack diagnosis-result
                                    (assoc details :status :running))))))

(deftest persist-bundle-writes-cas-before-catalog
  (let [root (Files/createTempDirectory "evoclj-diagnostic-test" (make-array java.nio.file.attribute.FileAttribute 0))
        db (.toString (.resolve root "store.sqlite"))
        cas-root (.resolve root "cas")
        store {:sqlite db :cas (cas/->cas cas-root)}]
    (try
      (is (= :applied (:status (migrate/migrate! db))))
      (let [bundle (diagnostic/build-bundle evidence-pack diagnosis-result details)
            id (:diagnostic/id bundle)]
        (is (= bundle (diagnostic/persist-bundle! store bundle)))
        (is (= bundle (diagnostic/persist-bundle! store bundle)))
        (is (cas/exists? (:cas store) id))
        (is (= {:artifact/id id
                :size (alength (cas/get-bytes (:cas store) id))
                :media-type "application/edn"}
               (cas/get-meta (:cas store) id)))
        (is (= (:evidence/id bundle)
               (:evidence/id
                (edn/read-string
                 (String. ^bytes (cas/get-bytes (:cas store) id)
                          StandardCharsets/UTF_8)))))
        (is (= 1 (count (sqlite/query db
                                      ["SELECT hash FROM artifacts WHERE hash = ?" id])))))
      (finally
        (delete-tree! root)))))

(deftest persist-bundle-rejects-forged-id
  (let [root (Files/createTempDirectory "evoclj-diagnostic-id-test"
                                        (make-array java.nio.file.attribute.FileAttribute 0))
        db (.toString (.resolve root "store.sqlite"))
        store {:sqlite db :cas (cas/->cas (.resolve root "cas"))}]
    (try
      (migrate/migrate! db)
      (let [bundle (diagnostic/build-bundle evidence-pack diagnosis-result details)
            forged (assoc bundle :diagnostic/id (str "sha256:" (apply str (repeat 64 "c"))))]
        (is (= :diagnostic/id-mismatch
               (thrown-error-type #(diagnostic/persist-bundle! store forged)))))
      (finally
        (delete-tree! root)))))

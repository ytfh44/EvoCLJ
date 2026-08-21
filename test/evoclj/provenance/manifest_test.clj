(ns evoclj.provenance.manifest-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.provenance.manifest :as m]
            [evoclj.store.cas :as cas])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-root []
  (Files/createTempDirectory "evoclj-prov-manifest-"
                             (make-array FileAttribute 0)))

(defn- delete-tree! [^java.nio.file.Path p]
  (when (Files/exists p (make-array java.nio.file.LinkOption 0))
    (doseq [f (reverse (file-seq (.toFile p)))]
      (Files/deleteIfExists (.toPath f)))))

(deftest deterministic
  (testing "identical immutable inputs produce deterministic manifest and same CAS ref"
    (let [b1 {:binding/id #uuid "11111111-1111-1111-1111-111111111111"
              :logical/id "skill-a"
              :revision/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
          b2 {:binding/id #uuid "22222222-2222-2222-2222-222222222222"
              :logical/id "skill-b"
              :revision/id "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}
          tc {:binding/id #uuid "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
              :revision-ids {"src-b" "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                             "src-a" "sha256:1111111111111111111111111111111111111111111111111111111111111111"}}
          tc-shuffled {:binding/id #uuid "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
                       :revision-ids {"src-a" "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                                      "src-b" "sha256:2222222222222222222222222222222222222222222222222222222222222222"}}
          hist {:compression-envelope/ref "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"}
          m1 (m/make-manifest {:bindings [b1 b2] :tool-catalog tc :history hist})
          m2 (m/make-manifest {:bindings [b2 b1] :tool-catalog tc-shuffled :history hist})
          root (temp-root)]
      (try
        (is (= m1 m2) "sorted bindings and sorted tool-catalog keys yield equal manifest")
        (is (= 1 (:context/manifest-version m1)))
        (let [r1 (m/put-manifest! root m1)
              r2 (m/put-manifest! root m2)]
          (is (= r1 r2) "same bytes -> same artifact id")
          (is (string? r1))
          (is (re-matches #"sha256:[0-9a-f]{64}" r1)))
        (finally (delete-tree! root))))))

(deftest cas-round-trip
  (testing "manifest round-trips through CAS"
    (let [bindings [{:binding/id #uuid "33333333-3333-3333-3333-333333333333"
                     :logical/id "skill-x"
                     :revision/id "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"}]
          tool-catalog {:binding/id #uuid "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                        :revision-ids {"a" "sha256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"}}
          history {:compression-envelope/ref "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"}
          manifest (m/make-manifest {:bindings bindings
                                     :tool-catalog tool-catalog
                                     :history history})
          root (temp-root)]
      (try
        (let [ref (m/put-manifest! root manifest)
              loaded (m/load-manifest root ref)]
          (is (= manifest loaded))
          (is (= 1 (:context/manifest-version loaded)))
          (is (= bindings (:bindings loaded)))
          (is (= history (:history loaded))))
        (finally (delete-tree! root))))))

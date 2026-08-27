(ns evoclj.context.materializer-test
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [evoclj.context.offer :as offer]
            [evoclj.context.binding :as binding]
            [evoclj.context.materializer :as mat]
            [evoclj.context.policy :as policy]
            [evoclj.genome.hash :as hash]
            [evoclj.store.cas :as cas])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.nio.charset StandardCharsets)))

(defn- rev [s] (hash/text-digest s))
(defn- txt-bytes [s] (.getBytes ^String s StandardCharsets/UTF_8))

;; ---------------------------------------------------------------------------
;; Helpers for CAS
;; ---------------------------------------------------------------------------

(defn- temp-cas-root []
  (Files/createTempDirectory "evoclj-context-test-" (make-array FileAttribute 0)))

(defn- delete-tree [^java.nio.file.Path root]
  (when (Files/exists root (make-array java.nio.file.LinkOption 0))
    (doseq [f (reverse (file-seq (.toFile root)))]
      (Files/deleteIfExists (.toPath f)))))

(defn- cas-put [root content]
  (:artifact/id (cas/put-bytes! (str root) (txt-bytes content) {:media-type "text/markdown"})))

;; ---------------------------------------------------------------------------
;; Basic materialization
;; ---------------------------------------------------------------------------

(t/deftest materialize-with-in-memory-cas
  (let [content-a "# SKILL A\noriginal"
        content-b "# SKILL B\nupdated"
        rev-a (rev content-a)
        rev-b (rev content-b)
        offer-a (offer/make-offer {:logical-id [:skill "debugging"] :revision-id rev-a :bundle-id "bundle:a"})
        cas {rev-a content-a rev-b content-b}
        store (binding/create-store)
        _ (binding/activate! store offer-a)
        history "compressed history"
        result (mat/materialize {:history history
                                 :bindings (binding/list-active store)
                                 :catalog (offer/catalog-projection [offer-a])
                                 :policy nil
                                 :cas cas})]
    (t/is (= 1 (count (:effective/segments result))))
    (t/is (= content-a (:segment/content (first (:effective/segments result)))))
    (t/is (str/includes? (:effective/context-string result) content-a))
    (t/is (str/includes? (:effective/context-string result) history))))

(t/deftest materialize-with-filesystem-cas
  (let [root (temp-cas-root)]
    (try
      (let [content-a "# SKILL A file"
            content-b "# SKILL B file"
            ;; put both artifacts, but we'll pin to A
            id-a (cas-put root content-a)
            id-b (cas-put root content-b)
            ;; ensure our rev helper matches CAS id (text-digest)
            ;; CAS uses file-digest for bytes, which for text is text-digest
            ;; So id-a should equal rev content-a
            _ (t/is (= id-a (rev content-a)))
            offer-a (offer/make-offer {:logical-id [:skill "debugging"] :revision-id id-a :bundle-id "bundle:a"})
            offer-b (offer/make-offer {:logical-id [:skill "debugging"] :revision-id id-b :bundle-id "bundle:b"})
            store (binding/create-store)
            _ (binding/activate! store offer-a)
            catalog (offer/catalog-projection [offer-b]) ; catalog moved to B
            history "history"
            result (mat/materialize {:history history
                                     :bindings (binding/list-active store)
                                     :catalog catalog
                                     :policy nil
                                     :cas (str root)})]
        (t/is (= content-a (:segment/content (first (:effective/segments result)))))
        (t/is (not (str/includes? (:segment/content (first (:effective/segments result))) "SKILL B"))))
      (finally (delete-tree root)))))

;; ---------------------------------------------------------------------------
;; Key test: activate A -> upstream refresh to B -> compact history -> materialize A not B
;; ---------------------------------------------------------------------------

(t/deftest pinning-activate-A-refresh-to-B-compact-materialize-A
  (let [content-a "# SKILL.md A\nVersion A content — debugging original"
        content-b "# SKILL.md B\nVersion B content — debugging updated"
        rev-a (rev content-a)
        rev-b (rev content-b)
        offer-a (offer/make-offer {:logical-id [:skill "debugging"]
                                   :revision-id rev-a
                                   :bundle-id "bundle:rev-a"
                                   :name "debugging"
                                   :description "A"})
        offer-b (offer/make-offer {:logical-id [:skill "debugging"]
                                   :revision-id rev-b
                                   :bundle-id "bundle:rev-b"
                                   :name "debugging"
                                   :description "B"})
        ;; CAS holds both artifacts immutably
        cas {rev-a content-a rev-b content-b}
        ;; Step 1: activate A
        store (binding/create-store)
        binding-a (binding/activate! store offer-a)
        _ (t/is (= rev-a (:revision/id binding-a)) "binding pinned to A")
        ;; Step 2: upstream refresh to B — catalog projection moves
        catalog-before (offer/catalog-projection [offer-a])
        catalog-after (offer/catalog-projection [offer-b])
        _ (t/is (= rev-a (:offer/revision-id (offer/current-offer catalog-before [:skill "debugging"]))))
        _ (t/is (= rev-b (:offer/revision-id (offer/current-offer catalog-after [:skill "debugging"]))) "catalog now at B")
        ;; Step 3: compact history (compression only touches history)
        ;; Simulate compression: history -> compressed history, without touching bindings
        history "long conversation history"
        compressed-history (str "compressed:" history) ; compression subsystem's output
        ;; Ensure compression did not mutate bindings
        _ (t/is (= 1 (count (binding/list-active store))) "binding still present after compact")
        _ (t/is (= rev-a (:revision/id (first (binding/list-active store)))) "binding still pinned to A after compact")
        ;; Step 4: next request must materialize A, not B
        result (mat/materialize {:history compressed-history
                                 :bindings (binding/list-active store)
                                 :catalog catalog-after
                                 :policy nil
                                 :cas cas})]
    (t/is (= 1 (count (:effective/segments result))))
    (let [seg (first (:effective/segments result))]
      (t/is (= rev-a (:segment/revision-id seg)) "segment revision is A")
      (t/is (= content-a (:segment/content seg)) "segment content is A")
      (t/is (str/includes? (:effective/context-string result) content-a) "context contains A")
      (t/is (not (str/includes? (:effective/context-string result) content-b)) "context does NOT contain B — if it does, fail"))
    (when (str/includes? (:effective/context-string (mat/materialize {:history compressed-history
                                                                       :bindings (binding/list-active store)
                                                                       :catalog catalog-after
                                                                       :policy nil
                                                                       :cas cas}))
                         content-b)
      (t/is false "FAIL: materializer returned B instead of pinned A"))))

(t/deftest materialize-respects-host-policy
  (let [content-a "# SKILL A"
        rev-a (rev content-a)
        offer-a (offer/make-offer {:logical-id [:skill "debugging"] :revision-id rev-a :bundle-id "bundle:a"})
        content-b "# SKILL B"
        rev-b (rev content-b)
        offer-b (offer/make-offer {:logical-id [:skill "other"] :revision-id rev-b :bundle-id "bundle:b"})
        cas {rev-a content-a rev-b content-b}
        store (binding/create-store)
        _ (binding/activate! store offer-a)
        _ (binding/activate! store offer-b)
        history "history"
        ;; policy allows only debugging
        pol {:policy/allowed #{[:skill "debugging"]}}
        result (mat/materialize {:history history
                                 :bindings (binding/list-active store)
                                 :catalog (offer/catalog-projection [offer-a offer-b])
                                 :policy pol
                                 :cas cas})]
    (t/is (= 1 (count (:effective/segments result))))
    (t/is (= [:skill "debugging"] (:segment/logical-id (first (:effective/segments result)))))))

(t/deftest materialize-with-real-cas-resolver
  ;; WO-S1 / INV-09: the materializer resolves via REAL CAS, never a
  ;; test-injected resolver fn (cas-fn is banned). A leaf artifact stored
  ;; in a real CAS tree is materialized verbatim through production CAS.
  (let [content-a "# SKILL via real cas"
        rev-a (rev content-a)
        offer-a (offer/make-offer {:logical-id [:skill "debugging"] :revision-id rev-a :bundle-id "bundle:a"})
        store (binding/create-store)
        _ (binding/activate! store offer-a)
        cas-root (temp-cas-root)
        _ (cas/put-bytes! (str cas-root) (txt-bytes content-a) {:media-type "text/markdown"})
        result (mat/materialize {:history "h"
                                 :bindings (binding/list-active store)
                                 :catalog nil
                                 :policy nil
                                 :cas (str cas-root)})]
    (t/is (= content-a (:segment/content (first (:effective/segments result)))))))

(t/deftest compression-does-not-need-skill
  ;; Compression loop should work with no bindings/skill context
  (let [history "history that will be compressed"
        result (mat/materialize {:history history
                                 :bindings []
                                 :catalog nil
                                 :policy nil
                                 :cas nil})]
    (t/is (= history (:effective/context-string result)))
    (t/is (empty? (:effective/segments result)))))

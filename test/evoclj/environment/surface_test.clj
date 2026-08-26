(ns evoclj.environment.surface-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.environment.bundle :as bundle]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.surface :as surf]))

(defn- fresh-registry []
  (reg/create-registry))

(defn- ctx [id rev]
  (surf/make-context-surface {:id id :descriptor {:prompt "hello"} :materializer (fn [x] x) :revision/id rev}))

(defn- tool [id rev]
  (surf/make-tool-surface {:id id :entries {:a {:tool/id :a}} :revision/id rev}))

(defn- dir [id rev access]
  (surf/make-directory-surface {:id id :backend {:type :memory :root "/tmp"} :access-max access :revision/id rev}))

(deftest two-surfaces-same-revision-id
  (testing "two surfaces in same bundle always have same revision/id"
    (let [registry (fresh-registry)
          rev "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          s1 (ctx :ctx/a rev)
          s2 (dir :dir/a rev #{:read :list :stat})
          b (bundle/make-bundle {:bundle-id "bundle:1" :revision-id rev :logical-id :skill/test :surfaces [s1 s2]})
          res (bundle/publish-bundle! registry b)]
      (is (= :published (:status res)))
      (is (= rev (:revision/id b)))
      (is (= rev (:revision/id s1)))
      (is (= rev (:revision/id s2)))
      (let [stored (bundle/get-bundle registry "bundle:1")]
        (is (= 1 (count (set (map :revision/id (:surfaces stored))))))
        (is (= rev (:revision/id stored)))
        (is (= rev (:revision/id (first (:surfaces stored)))))
        (is (= rev (:revision/id (second (:surfaces stored)))))))))

(deftest sibling-co-version-enforced
  (testing "bundle rejects siblings with diverging revision ids"
    (let [rev1 "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          rev2 "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
          s1 (ctx :ctx/a rev1)
          s2 (dir :dir/a rev2 #{:read :list :stat})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (bundle/make-bundle {:bundle-id "b:bad" :revision-id rev1 :logical-id :skill/x :surfaces [s1 s2]}))))))

(deftest collision-fails-whole-publication-not-partially
  (testing "collision fails whole publication not partially"
    (let [registry (fresh-registry)
          rev "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
          dup-id :dup/surface
          s1 (ctx dup-id rev)
          s2 (ctx dup-id rev) ; same surface/id duplicate
          ]
      ;; bundle creation itself should throw collision
      (is (thrown? clojure.lang.ExceptionInfo
                   (bundle/make-bundle {:bundle-id "bundle:dup" :revision-id rev :logical-id :skill/dup :surfaces [s1 s2]})))
      ;; registry must have no partial surfaces
      (is (empty? (bundle/list-bundles registry)))
      (is (empty? (bundle/list-surfaces registry)))
      ;; also test publish path with duplicate ids via raw map bypassing make-bundle validation?
      ;; construct bundle map manually with duplicate ids and try publish
      (let [raw-bundle {:bundle/id "bundle:raw-dup"
                        :revision/id rev
                        :logical/id :skill/raw
                        :surfaces [s1 s2]}]
        (is (thrown? clojure.lang.ExceptionInfo (bundle/publish-bundle! registry raw-bundle)))
        (is (empty? (bundle/list-bundles registry)))
        (is (empty? (bundle/list-surfaces registry)) "no partial set after collision")))))

(deftest any-failure-produces-no-partial-set
  (testing "descriptor invalid -> no partial set"
    (let [registry (fresh-registry)
          rev "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
          good (ctx :ctx/good rev)
          bad {:surface/type :directory :surface/id :dir/bad :backend {:type :memory} :access/max #{:read :list :stat :write :create :delete} :revision/id rev}
          ;; make bad directory missing required? actually this one is valid, create invalid by using future capability
          bad2 {:surface/type :directory :surface/id :dir/bad2 :backend {:type :memory} :access/max #{:read :execute} :revision/id rev}
          bundle-ok (bundle/make-bundle {:bundle-id "bundle:ok" :revision-id rev :logical-id :skill/ok :surfaces [good]})
          _ (bundle/publish-bundle! registry bundle-ok)
          before-count (count (bundle/list-surfaces registry))
          bad-bundle {:bundle/id "bundle:bad-desc"
                      :revision/id rev
                      :logical/id :skill/bad
                      :surfaces [good bad2]}]
      (is (= 1 before-count))
      (is (thrown? clojure.lang.ExceptionInfo (bundle/publish-bundle! registry bad-bundle)))
      (is (= 1 (count (bundle/list-surfaces registry))) "still only first bundle's surfaces")
      (is (= 1 (count (bundle/list-bundles registry))) "no partial second bundle")))
  (testing "invalid descriptor via missing surface/id leaves no partial"
    (let [registry (fresh-registry)
          rev "sha256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
          good (ctx :ctx/good rev)
          bad {:surface/type :context :descriptor {} :materializer identity :revision/id rev} ; missing :surface/id
          bad-bundle {:bundle/id "bundle:missing-id"
                      :revision/id rev
                      :logical/id :skill/missing
                      :surfaces [good bad]}]
      (is (thrown? clojure.lang.ExceptionInfo (bundle/publish-bundle! registry bad-bundle)))
      (is (empty? (bundle/list-bundles registry)))
      (is (empty? (bundle/list-surfaces registry)))))
  (testing "index projection failure leaves no partial"
    (let [registry (fresh-registry)
          rev "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
          good (ctx :ctx/good rev)
          bad-bundle {:bundle/id nil ; missing bundle id triggers index projection failure
                      :revision/id rev
                      :logical/id :skill/bad-index
                      :surfaces [good]}]
      (is (thrown? clojure.lang.ExceptionInfo (bundle/publish-bundle! registry bad-bundle)))
      (is (empty? (bundle/list-bundles registry)))
      (is (empty? (bundle/list-surfaces registry))))))

(deftest skill-context-and-directory-atomically-bound
  (testing "Skill Context and Directory must be bound together atomically; no half-bound"
    (let [registry (fresh-registry)
          rev "sha256:1111111111111111111111111111111111111111111111111111111111111111"
          context-s (ctx :skill/ctx rev)
          dir-s (dir :skill/dir rev #{:read :list :stat})
          bundle (bundle/make-bundle {:bundle-id "bundle:skill" :revision-id rev :logical-id :skill/my-skill :surfaces [context-s dir-s]})
          res (bundle/publish-bundle! registry bundle)]
      (is (= :published (:status res)))
      ;; both present
      (is (some? (bundle/get-surface registry :skill/ctx)))
      (is (some? (bundle/get-surface registry :skill/dir)))
      ;; now try a failing bundle that would attempt to replace skill with one invalid surface: ensure previous skill surfaces remain, no half update
      (let [rev2 "sha256:2222222222222222222222222222222222222222222222222222222222222222"
            ctx2 (ctx :skill/ctx rev2)
            bad-dir {:surface/type :directory :surface/id :skill/dir :backend {:type :memory} :access/max #{:read :execute} :revision/id rev2}
            bad-bundle {:bundle/id "bundle:skill2"
                        :revision/id rev2
                        :logical/id :skill/my-skill
                        :surfaces [ctx2 bad-dir]}]
        (is (thrown? clojure.lang.ExceptionInfo (bundle/publish-bundle! registry bad-bundle)))
        ;; still old surfaces intact, no half-bound where context updated but dir not
        (is (= rev (:revision/id (bundle/get-surface registry :skill/ctx))) "context still old revision")
        (is (= rev (:revision/id (bundle/get-surface registry :skill/dir))) "directory still old revision")))))

(deftest publish-surfaces-convenience-ensures-co-version
  (testing "publish-surfaces! ensures two surfaces share same revision/id derived from payload"
    (let [registry (fresh-registry)
          payload "skill content v1"
          s1 (surf/make-context-surface {:id :ctx/a :descriptor "d" :materializer identity})
          s2 (surf/make-directory-surface {:id :dir/a :backend {:type :memory} :access-max #{:read :list :stat}})]
      (let [res (bundle/publish-surfaces! registry {:logical-id :skill/test :payload payload :surfaces [s1 s2]})]
        (is (= :published (:status res)))
        (let [stored (bundle/get-bundle registry (:bundle/id (:bundle res)))]
          (is (= 1 (count (set (map :revision/id (:surfaces stored))))))
          (is (= (:revision/id stored) (:revision/id (first (:surfaces stored))))))))))

(deftest capability-set-not-ro-rw
  (testing "DirectorySurface uses capability set not RO/RW binary"
    (let [ro (dir :dir/ro "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" #{:read :list :stat})
          rw (dir :dir/rw "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" #{:read :list :stat :write :create :delete})]
      (is (= #{:read :list :stat} (:access/max ro)))
      (is (= #{:read :list :stat :write :create :delete} (:access/max rw)))
      ;; future capability not accepted
      (is (thrown? clojure.lang.ExceptionInfo
                   (surf/make-directory-surface {:id :dir/future :backend {:type :memory} :access-max #{:read :append-only}}))))))

(deftest context-tool-directory-are-peers
  (testing "three peer types are distinct but equal level"
    (let [c (surf/make-context-surface {:id :c :descriptor "d" :materializer identity})
          t (surf/make-tool-surface {:id :t :entries {}})
          d (surf/make-directory-surface {:id :d :backend {} :access-max #{:read :list :stat}})]
      (is (surf/context-surface? c))
      (is (surf/tool-surface? t))
      (is (surf/directory-surface? d))
      (is (surf/surface? c))
      (is (surf/surface? t))
      (is (surf/surface? d))
      (is (not= (:surface/type c) (:surface/type t)))
      (is (not= (:surface/type t) (:surface/type d))))))

(deftest dead-surface-helpers-removed
  (testing "S2: known-access-sets and co-versioned? are dead code and must be gone"
    (let [public-vars (set (keys (ns-publics 'evoclj.environment.surface)))]
      (is (not (contains? public-vars 'known-access-sets)))
      (is (not (contains? public-vars 'co-versioned?))))
    ;; behavior preserved: valid surfaces still construct/validate through the
    ;; production path and co-version enforcement still lives in bundle (tested
    ;; separately by sibling-co-version-enforced).
    (let [rev "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          s (surf/make-context-surface {:id :ctx/x :descriptor {:p "a"} :materializer identity :revision/id rev})]
      (is (surf/context-surface? s))
      (is (= rev (:revision/id s))))))

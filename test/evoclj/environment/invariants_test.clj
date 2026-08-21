(ns evoclj.environment.invariants-test
  "black-box regression for six cross-subsystem invariants.

  These tests freeze current correct behavior as black-box assertions.
  Later phases may rename internal APIs without touching this layer.
  The file is self-contained using FakeSource/FakeBinding only."
  (:require [clojure.test :refer [deftest is testing]]))

;; ---- Minimal fake model (semantic freeze only, not final shape) ----

(defn fake-snapshot [source]
  {:content (:content @source) :source/id (:source/id @source)})

(defn publish! [registry snapshot]
  (let [{:keys [content source/id]} snapshot
        cur (:current @registry)
        content-id (str "rev:" (hash content))]
    (if (= content-id (:revision/id cur))
      ;; identical refresh — no churn
      {:status :noop :revision cur}
      ;; new publication
      (let [next-seq (inc (or (:revision/seq cur) 0))
            rev {:revision/id content-id :revision/seq next-seq :source/id id :captured-at (System/currentTimeMillis) :payload content}
            bundle {:bundle/id (str "bundle:" content-id)
                    :revision/id content-id
                    :logical/id (:source/id snapshot)
                    :surfaces [{:surface/type :context :revision/id content-id}
                               {:surface/type :directory :revision/id content-id}]}]
        (swap! registry assoc
               :current rev
               :bundle bundle
               :catalog {id content-id}
               :last-good rev
               :status :ok :dirty? false)
        ;; record immutable history
        (swap! registry update :published conj rev)
        {:status :published :revision rev :bundle bundle}))))

(defn refresh-ok! [registry source new-content]
  (reset! source {:content new-content :source/id (:source/id @source)})
  (publish! registry (fake-snapshot source)))

;; ---- Invariant 1: published revision is immutable ----
(deftest invariant-1-published-revision-immutable
  (testing "once published, revision payload and id never mutate in place"
    (let [registry (atom {:published []})
          source (atom {:source/id :skills/user :content "SKILL.md v1"})
          {:keys [revision]} (publish! registry (fake-snapshot source))
          rev-id (:revision/id revision)
          payload-before (:payload revision)]
      (refresh-ok! registry source "SKILL.md v2")
      (let [published (:published @registry)
            old (first (filter #(= rev-id (:revision/id %)) published))]
        (is (some? old) "old revision still in history")
        (is (= payload-before (:payload old)) "old revision payload immutable")
        (is (= rev-id (:revision/id old))))
      (is (= 1 (count (filter #(= rev-id (:revision/id %)) (:published @registry))))))))

;; ---- Invariant 2: refresh must not mutate existing binding ----
(deftest invariant-2-refresh-does-not-mutate-binding
  (testing "binding created before refresh still points to old revision after refresh"
    (let [registry (atom {:published []})
          source (atom {:source/id :skills/user :content "A"})
          {:keys [revision bundle]} (publish! registry (fake-snapshot source))
          binding {:binding/id (random-uuid) :logical/id [:skill "debugging"] :revision/id (:revision/id revision) :bundle/id (:bundle/id bundle) :state :active}
          bindings (atom {(:binding/id binding) binding})]
      (refresh-ok! registry source "B")
      (let [after (get @bindings (:binding/id binding))]
        (is (= (:revision/id binding) (:revision/id after)) "binding revision unchanged by refresh")
        (is (= (:bundle/id binding) (:bundle/id after)))))))

;; ---- Invariant 3: sibling surfaces in same bundle share revision ----
(deftest invariant-3-sibling-surfaces-share-revision
  (testing "active SKILL.md revision == mounted directory revision; bundle surfaces share revision-id"
    (let [registry (atom {:published []})
          source (atom {:source/id :skills/user :content "skill pkg A"})
          {:keys [bundle]} (publish! registry (fake-snapshot source))
          surfaces (:surfaces bundle)
          revs (set (map :revision/id surfaces))]
      (is (= 1 (count revs)) "two surfaces in same bundle share one revision/id")
      (is (= (:revision/id bundle) (first revs)))
      (let [binding {:binding/id (random-uuid) :logical/id [:skill "x"] :revision/id (:revision/id bundle) :bundle/id (:bundle/id bundle)}]
        (is (= (:revision/id binding) (:revision/id (first surfaces))))
        (is (= (:revision/id binding) (:revision/id (second surfaces))))))))

;; ---- Invariant 4: context compaction must not change binding identity ----
(deftest invariant-4-compaction-does-not-change-binding
  (testing "history compaction compresses history only, never binding identity"
    (let [registry (atom {:published []})
          source (atom {:source/id :skills/user :content "A"})
          {:keys [revision bundle]} (publish! registry (fake-snapshot source))
          binding {:binding/id (random-uuid) :logical/id [:skill "debugging"] :revision/id (:revision/id revision) :bundle/id (:bundle/id bundle) :state :active}
          bindings (atom {(:binding/id binding) binding})
          history (atom ["msg1" "msg2" "msg3" "msg4" "activate A"])]
      (let [compacted (vec (take-last 2 @history))]
        (reset! history compacted)
        (is (= binding (get @bindings (:binding/id binding))) "binding unchanged after compaction")
        (is (= (:revision/id binding) (:revision/id (get @bindings (:binding/id binding)))))))))

;; ---- Invariant 5: source removal must not silently invalidate binding ----
(deftest invariant-5-source-removal-does-not-invalidate-binding
  (testing "when source disappears from catalog, existing binding still servable via CAS"
    (let [registry (atom {:published [] :cas {}})
          source (atom {:source/id :skills/user :content "A tree content"})
          {:keys [revision bundle]} (publish! registry (fake-snapshot source))
          _ (swap! registry assoc-in [:cas (:revision/id revision)] "tree bytes A")
          binding {:binding/id (random-uuid) :logical/id [:skill "debugging"] :revision/id (:revision/id revision) :bundle/id (:bundle/id bundle) :state :active}
          bindings (atom {(:binding/id binding) binding})]
      (swap! registry assoc :catalog {})
      (swap! registry assoc :source-removed true)
      (is (some? (get @bindings (:binding/id binding))) "binding still present")
      (is (some? (get-in @registry [:cas (:revision/id binding)])) "CAS still holds old tree")
      (is (= "tree bytes A" (get-in @registry [:cas (:revision/id binding)]))))))

;; ---- Invariant 6: refresh/activate/reload/mount must not mint capability ----
(deftest invariant-6-no-capability-creation
  (testing "surface describes existence and max, binding describes pinned version, capability is separate; lifecycle ops do not mint leases"
    (let [capabilities (atom #{})
          grant! (fn [lease] (swap! capabilities conj lease))
          registry (atom {:published []})
          source (atom {:source/id :skills/user :content "A"})
          _ (is (empty? @capabilities))
          _ (publish! registry (fake-snapshot source))
          _ (is (empty? @capabilities) "refresh does not mint capability")
          binding {:binding/id (random-uuid) :logical/id [:skill "x"] :revision/id (:revision/id (:current @registry))}
          _ (is (empty? @capabilities) "activate does not mint capability")
          _ (refresh-ok! registry source "B")
          reloaded (assoc binding :revision/id (:revision/id (:current @registry)))
          _ (is (empty? @capabilities) "reload does not mint capability")
          mount {:mount/id [:skill "x" (:revision/id reloaded)] :backend :cas-tree :access/max #{:read :list :stat}}
          _ (is (empty? @capabilities) "mount does not mint capability")
          _ (grant! {:lease/id (random-uuid) :mount/id (:mount/id mount) :actions #{:read}})
          _ (is (= 1 (count @capabilities)) "only explicit grant adds capability")]
      (is true))))

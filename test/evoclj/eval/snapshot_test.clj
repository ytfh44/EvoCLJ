(ns evoclj.eval.snapshot-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.eval.snapshot :as snap]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.revision :as rev]
            [evoclj.genome.hash :as hash]))

(deftest snapshot-shape
  (testing "make-snapshot assigns uuid and preserves sources"
    (let [sources {:skills/user (rev/payload->id "skill-v1")
                   :mcp/github (rev/payload->id "mcp-v1")}
          s (snap/make-snapshot sources)]
      (is (snap/snapshot? s))
      (is (uuid? (:environment/id s)))
      (is (= sources (:sources s)))
      (is (number? (:captured-at s)))))
  (testing "capture-snapshot freezes current registry revisions"
    (let [registry (reg/create-registry)
          a (fake/make-fake-source :skills/user "skill-v1")
          b (fake/make-fake-source :mcp/github "mcp-v1")
          _ (reg/register-source! registry a)
          _ (reg/register-source! registry b)
          _ (reg/refresh! registry :skills/user)
          _ (reg/refresh! registry :mcp/github)
          s (snap/capture-snapshot registry)]
      (is (snap/snapshot? s))
      (is (= (rev/payload->id "skill-v1") (snap/revision-for s :skills/user)))
      (is (= (rev/payload->id "mcp-v1") (snap/revision-for s :mcp/github))))))

(deftest pinned-eval-survives-live-refresh
  (testing "parent and candidate share captured E even after live refresh"
    (let [registry (reg/create-registry)
          skill (fake/make-fake-source :skills/user "skill-v1")
          mcp (fake/make-fake-source :mcp/github "mcp-v1")
          _ (reg/register-source! registry skill)
          _ (reg/register-source! registry mcp)
          _ (reg/refresh! registry :skills/user)
          _ (reg/refresh! registry :mcp/github)
          captured (snap/capture-snapshot registry)
          parent-sources (snap/pinned-sources captured)
          candidate-sources (snap/pinned-sources captured)
          _ (is (= parent-sources candidate-sources) "same captured E for both sides")
          _ (is (= (rev/payload->id "skill-v1") (:skills/user parent-sources)))
          _ (is (= (rev/payload->id "mcp-v1") (:mcp/github parent-sources)))
          _ (fake/set-payload! skill "skill-v2")
          _ (fake/set-payload! mcp "mcp-v2")
          _ (reg/refresh! registry :skills/user)
          _ (reg/refresh! registry :mcp/github)
          live (snap/live-sources registry)
          pinned-still (snap/pinned-sources captured)]
      (is (= (rev/payload->id "skill-v2") (:skills/user live)) "live moved to v2")
      (is (= (rev/payload->id "mcp-v2") (:mcp/github live)))
      (is (= (rev/payload->id "skill-v1") (:skills/user pinned-still)) "pinned still v1")
      (is (= (rev/payload->id "mcp-v1") (:mcp/github pinned-still)))
      (is (= (snap/revision-for captured :skills/user) (:skills/user pinned-still)))
      (is (= (snap/revision-for captured :mcp/github) (:mcp/github pinned-still)))
      (is (not= (:skills/user live) (:skills/user pinned-still)) "live vs pinned diverge deterministically"))))

(deftest snapshot-determinism
  (testing "same registry state yields same sources map"
    (let [registry (reg/create-registry)
          a (fake/make-fake-source :skills/user "same-content")
          _ (reg/register-source! registry a)
          _ (reg/refresh! registry)
          s1 (snap/capture-snapshot registry)
          s2 (snap/capture-snapshot registry)]
      (is (= (:sources s1) (:sources s2)) "sources deterministic")
      (is (not= (:environment/id s1) (:environment/id s2)) "ids are fresh uuids but sources equal"))))

(deftest environment-not-in-phenotype
  (testing "phenotype hashing excludes environment"
    (let [abi {:kernel 1 :genome 1 :intent 1 :tool 1}
          gid "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          rid "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
          s1 (snap/make-snapshot {:skills/user (rev/payload->id "skill-v1")
                                  :mcp/github (rev/payload->id "mcp-v1")})
          s2 (snap/make-snapshot {:skills/user (rev/payload->id "skill-v2")
                                  :mcp/github (rev/payload->id "mcp-v2")})
          p1 (snap/phenotype-id abi gid rid)
          p2 (snap/phenotype-id abi gid rid)
          expected (hash/text-digest (str (pr-str (into (sorted-map) abi)) gid rid))]
      (is (= p1 p2) "different environments give same phenotype")
      (is (= p1 expected) "phenotype is abi+genome+resolution only")
      (is (not (clojure.string/includes? p1 (str (:environment/id s1)))) "env id not hashed")
      (is (not (clojure.string/includes? p1 (str (:environment/id s2)))))))
  (testing "changing resolution changes phenotype but environment still does not"
    (let [abi {:kernel 1 :genome 1 :intent 1 :tool 1}
          gid "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          rid1 "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
          rid2 "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
          p1 (snap/phenotype-id abi gid rid1)
          p2 (snap/phenotype-id abi gid rid2)]
      (is (not= p1 p2) "resolution does affect phenotype"))))

(deftest revision-for-lookup
  (testing "revision-for returns nil for unknown source"
    (let [s (snap/make-snapshot {:skills/user (rev/payload->id "x")})]
      (is (some? (snap/revision-for s :skills/user)))
      (is (nil? (snap/revision-for s :mcp/github))))))

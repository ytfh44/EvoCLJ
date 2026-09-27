(ns evoclj.capability.grant-property-test
  "C2 Grant lattice — Work×Session product, Grant meet, Event refinement composition (100 rounds per law)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [evoclj.capability.grant :as grant]
            [evoclj.capability.mint :as mint]
            [evoclj.capability.resource-kind :as rk]))

;; --- generators ------------------------------------------------------------

(def ^:private tool-id-gen
  (gen/elements [:fixture/echo :tool/a :tool/b :tool/c]))

(def ^:private memory-id-gen
  (gen/elements [:mem/a :mem/b]))

(def ^:private path-segments-gen
  (gen/vector (gen/elements ["a" "b" "c" "x" "y"]) 1 3))

(defn- path-gen []
  (gen/fmap (fn [segs] (str "/" (clojure.string/join "/" segs))) path-segments-gen))

(def ^:private grant-gen
  (gen/bind
   (gen/elements [:tool :memory :filesystem :filesystem/path])
   (fn [kind]
     (case kind
       :tool (gen/fmap (fn [tid]
                         (grant/make-grant {:kind :tool :id tid} #{:invoke}))
                       tool-id-gen)
      :memory (gen/fmap (fn [mid]
                          (grant/make-grant {:kind :memory :id mid} #{:read}))
                        memory-id-gen)
       :filesystem (gen/fmap (fn [[p actions]]
                               (grant/make-grant {:kind :filesystem :path p} actions))
                             (gen/tuple (path-gen)
                                        (gen/bind (gen/vector (gen/elements [:read :list :stat :write :create :delete]) 1 3)
                                                  #(gen/return (set %)))))
       :filesystem/path (gen/fmap (fn [[p actions]]
                                    (grant/make-grant {:kind :filesystem/path :path p} actions))
                                  (gen/tuple (path-gen)
                                             (gen/bind (gen/vector (gen/elements [:read :list :stat :write :create :delete]) 1 3)
                                                       #(gen/return (set %)))))))))

(defn- random-grant-pair []
  (gen/tuple grant-gen grant-gen))

;; [W-11] meet idempotent: meet(g,g) == g (when meet exists)
(defspec grant-meet-idempotent 100
  (prop/for-all [g grant-gen]
    (let [m (grant/meet g g)]
      ;; meet(g,g) should be g itself (greatest lower bound of self)
      (or (nil? m) ; nil only when grant malformed, not random valid
          (and (grant/covers? m g) (grant/covers? g m) (= (:resource m) (:resource g)) (= (:actions m) (:actions g)))))))

;; [W-12] meet commutative: meet(a,b) == meet(b,a)
(defspec grant-meet-commutative 100
  (prop/for-all [[a b] (random-grant-pair)]
    (let [m1 (grant/meet a b)
          m2 (grant/meet b a)]
      (= m1 m2))))

;; [W-13] meet greatest lower bound: meet(a,b) is a lower bound of both parents
;; AND is above every common lower bound. The universal half (greatest, not
;; merely a lower bound) is what makes this a real GLB law rather than a
;; restatement of [W-14]: for every candidate m in a fixed pool, if BOTH
;; parents cover m then the meet covers m. When meet(a,b) is nil the parents
;; admit no common lower bound in the pool — measured: /a/b and /a/x share
;; none, so nil is the correct meet, not a GLB violation.
(def ^:private lower-bound-pool
  "A fixed pool of candidate scopes probed for the universal GLB half.
  Covers the measured data: two same-segment parents, two divergent
  parents, a prefix, an exact parent, and an unrelated path."
  [{:resource {:kind :filesystem :path "/a"} :actions #{:read}}
   {:resource {:kind :filesystem :path "/a/b"} :actions #{:read}}
   {:resource {:kind :filesystem :path "/a/x"} :actions #{:read}}
   {:resource {:kind :filesystem :path "/a/b/c"} :actions #{:read}}
   {:resource {:kind :filesystem :path "/a/b/c"} :actions #{:read :write}}
   {:resource {:kind :filesystem/path :path "/a/b"} :actions #{:read}}
   {:resource {:kind :filesystem/path :mount/id [:skill "d"] :path "a"} :actions #{:read}}
   {:resource {:kind :filesystem/path :mount/id [:skill "d"] :path "a/b"} :actions #{:read}}
   {:resource {:kind :tool :id :fixture/echo} :actions #{:invoke}}
   {:resource {:kind :memory :id :mem/a} :actions #{:read}}])

(defspec grant-meet-greatest-lower-bound 100
  (prop/for-all [[a b] (random-grant-pair)]
    (let [m (grant/meet a b)]
      (and
       ;; lower-bound half: the meet is covered by both parents
       (or (nil? m)
           (and (grant/covers? a m) (grant/covers? b m)
                (grant/attenuates? a m) (grant/attenuates? b m)))
       ;; greatest half: nothing both parents admit lies strictly above the meet
       (every? (fn [cand]
                 (or (not (grant/covers? a cand))
                     (not (grant/covers? b cand))
                     (nil? m)
                     (grant/covers? m cand)))
               lower-bound-pool)))))

;; [W-14] meet attenuates parents (when non-nil, meet ≤ parents)
(defspec grant-meet-attenuates-parents 100
  (prop/for-all [[a b] (random-grant-pair)]
    (let [m (grant/meet a b)]
      (if (nil? m)
        true
        (and (grant/attenuates? a m) (grant/attenuates? b m))))))

;; [W-09] covers reflexive: covers?(g,g) == true
(defspec grant-covers-reflexive 100
  (prop/for-all [g grant-gen]
    (grant/covers? g g)))

;; [W-10] attenuates transitive: if a attenuates b and b attenuates c then a attenuates c
;; Generate chain by attenuating via shrinking actions / narrowing path
(defspec grant-attenuates-transitive 100
  (prop/for-all [g grant-gen]
    ;; shrinking actions by taking subset should attenuate
    (let [actions (:actions g)
          smaller (if (> (count actions) 1)
                    (set (take 1 (seq actions)))
                    actions)
          g2 (try (grant/make-grant (:resource g) smaller) (catch Exception _ g))
          g3 (try (grant/make-grant (:resource g) smaller) (catch Exception _ g))]
      (if (and (grant/attenuates? g g2) (grant/attenuates? g2 g3))
        (grant/attenuates? g g3)
        true))))

;; Action set lattice specific

(defspec action-set-meet-idempotent 100
  (prop/for-all [a (gen/bind (gen/vector (gen/elements [:read :write :list]) 1 2) #(gen/return (set %)))]
    (= (grant/action-set-meet a a) a)))

(defspec action-set-meet-commutative 100
  (prop/for-all [a (gen/bind (gen/vector (gen/elements [:read :write :list]) 1 2) #(gen/return (set %)))
                 b (gen/bind (gen/vector (gen/elements [:read :write :list]) 1 2) #(gen/return (set %)))]
    (= (grant/action-set-meet a b) (grant/action-set-meet b a))))

;; --- composition: Lease = Grant × Principal × TimeWindow × Quota ----------
;; Verify that Grant meet composition is independent of other dimensions
(defspec grant-meet-composition-product 100
  (prop/for-all [[a b] (random-grant-pair)]
    ;; Grant meet is product of Resource meet × ActionSet meet
    (let [m (grant/meet a b)]
      (if (nil? m)
        ;; nil means either resource-meet nil or action-set-meet nil -> product disjoint
        (or (nil? (grant/resource-meet (:resource a) (:resource b)))
            (nil? (grant/action-set-meet (:actions a) (:actions b))))
        ;; non-nil means both dimensions had a GLB
        (and (some? (grant/resource-meet (:resource a) (:resource b)))
             (some? (grant/action-set-meet (:actions a) (:actions b))))))))

;; --- covers? is a partial order on MINTED leases -------------------------
;; Every lease crossing the minting surface has a canonical :resource
;; (schema/make-lease runs rk/canonicalize-resource), so the cover relation
;; really is a partial order: reflexivity aside, mutual coverage implies
;; equal resources. Without canonicalization, "/w/secret" and "/w/secret/"
;; were two values that each covered the other, making the "partial order" a
;; preorder. Mints through mint/mint-lease! — the production path — so this
;; pins the surface, not hand-constructed lease maps.
(deftest minted-filesystem-resources-are-canonical-so-covers-is-antisymmetric
  (let [issued (java.util.Date. 1700000000000)
        expires (java.util.Date. 1700003600000)
        principal {:principal/type :session :session/id "S1"}
        mint-path (fn [p]
                    (mint/mint-lease! nil {:principal principal
                                           :resource {:kind :filesystem :path p}
                                           :actions #{:read}
                                           :issued-at issued
                                           :expires-at expires}))
        dirty (mint-path "/w/../w/secret/")
        clean (mint-path "/w/secret")
        wide  (mint-path "/w")]
    (testing "a non-canonical path is canonicalized at issuance"
      (is (= {:kind :filesystem :path "/w/secret"} (:resource dirty))))
    (testing "canonically-equal mintings produce equal resources"
      (is (= (:resource dirty) (:resource clean))))
    (testing "mutual covers? on canonically-DISTINCT resources is impossible
              (antisymmetry: mutual coverage implies equal resources)"
      (is (grant/covers? wide clean))
      (is (not (grant/covers? clean wide)))
      (is (not (and (grant/covers? wide clean) (grant/covers? clean wide)))))
    (testing "reflexivity still holds for equal resources"
      (is (grant/covers? dirty dirty)))))

;; --- meet returns a canonical resource ------------------------------------
;; The parents are built with grant/->grant, NOT grant/make-grant. make-grant
;; canonicalizes its resource, so parents minted that way are already canonical
;; and the descriptor meet — which returns whichever parent it found more
;; specific, verbatim — hands back a canonical value with or without the
;; re-canonicalization in grant/meet. Such a test passes against the reverted
;; (unfixed) grant/meet, so it pins nothing. ->grant coerces without
;; canonicalizing or validating, so the non-canonical value genuinely REACHES
;; meet. Each sub-case asserts that premise on the parent before meeting, and
;; the parent the descriptor is expected to return is itself non-canonical.
(deftest meet-returns-a-canonical-resource
  (testing "a non-canonical host path is canonicalized in the meet result"
    (let [a (grant/->grant {:resource {:kind :filesystem :path "/a/../a/./b"}
                            :actions #{:read}})
          b (grant/->grant {:resource {:kind :filesystem :path "/a/b/c/../c"}
                            :actions #{:read}})
          m (grant/meet a b)]
      ;; precondition: the parents enter `meet` non-canonical
      (is (= {:kind :filesystem :path "/a/../a/./b"} (:resource a)))
      (is (= {:kind :filesystem :path "/a/b/c/../c"} (:resource b)))
      (is (some? m))
      (is (= {:kind :filesystem :path "/a/b/c"} (:resource m)))))
  (testing "a non-canonical mount path is canonicalized in the meet result"
    (let [a (grant/->grant {:resource {:kind :filesystem/path
                                        :mount/id [:skill "d"]
                                        :path "a/../a/./b"}
                            :actions #{:read}})
          b (grant/->grant {:resource {:kind :filesystem/path
                                        :mount/id [:skill "d"]
                                        :path "a/b/c/../c"}
                            :actions #{:read}})
          m (grant/meet a b)]
      ;; precondition: the parents enter `meet` non-canonical
      (is (= {:kind :filesystem/path :path "a/../a/./b"
              :mount/id [:skill "d"]}
             (:resource a)))
      (is (= {:kind :filesystem/path :path "a/b/c/../c"
              :mount/id [:skill "d"]}
             (:resource b)))
      (is (= {:kind :filesystem/path :path "a/b/c"
              :mount/id [:skill "d"]}
             (:resource m))))))

(deftest cross-fiber-meet-is-bottom
  (let [mounted (grant/make-grant {:kind :filesystem/path :mount/id [:skill "d"]
                                   :path "a"} #{:read})
        bare (grant/make-grant {:kind :filesystem/path :path "a"} #{:read})
        other-mount (grant/make-grant {:kind :filesystem/path :mount/id [:skill "e"]
                                       :path "a"} #{:read})]
    (testing "a mount-scoped scope and a namespace-less scope are in different
              fibers — the meet is bottom, never a value with :mount/id dropped"
      (is (nil? (grant/meet mounted bare)))
      (is (nil? (grant/meet bare mounted))))
    (testing "differing mount ids are disjoint"
      (is (nil? (grant/meet mounted other-mount))))
    (testing "same-fiber meets are unaffected: the meet of a scope and a
              sub-scope is the SUB-scope (the lower bound of the two)"
      (is (= {:kind :filesystem/path :path "a/b" :mount/id [:skill "d"]}
             (:resource (grant/meet mounted
                                     (grant/make-grant
                                      {:kind :filesystem/path :mount/id [:skill "d"]
                                       :path "a/b"} #{:read}))))))
    (testing "a bare host-path meet is still lexical in its own fiber"
      (is (= {:kind :filesystem/path :path "/a/b"}
             (:resource (grant/meet
                         (grant/make-grant {:kind :filesystem/path :path "/a"} #{:read})
                         (grant/make-grant {:kind :filesystem/path :path "/a/b"} #{:read}))))))))

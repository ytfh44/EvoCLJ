(ns evoclj.eval.paired-test
  "G5 isolated paired Selection runner (Task 8.4).

  run-paired-selection! evaluates the parent (re-evaluated NOW, never
  a stale historical score) and the candidate as a PAIRED comparison
  on the same selection cases, same environment fixture seed, same
  repetitions:

      (run-paired-selection! evaluator
        {:parent-generation \"G42\" :candidate-id \"C17\"
         :case-set [:sel/c1] :repetitions 3})
      ;; => {:parent {...} :candidate {...} :pairs [...]}

  The runner's guarantees (mapped to the task's numbered steps):

  - Step 1: for each case/repetition ONE persisted random seed is
    derived (evoclj.eval.paired/derive-seed) and flows into BOTH sides
    of the pair — fixture providers that accept a seed (fn fixtures)
    observe the SAME fixture version on parent and candidate. The seed
    is deterministic and recorded in every pair (:pair/seed).
  - Step 2: execution order alternates — pair 1 parent-then-candidate,
    pair 2 candidate-then-parent, ... (:order).
  - Step 3: the parent is re-evaluated now, in every pair, through the
    full scheduler with fresh temp stores; a fresh candidate is never
    compared to a stale historical parent score (the runner accepts no
    parent score input at all).
  - Step 4: every side of every pair gets a FRESH Phenotype instance
    (fresh isolated SCI runtime — fresh session namespaces) and a
    fresh pinned session (:side/instance-id, :side/session-id).
  - Step 5: the Mutator sees only post-evaluation aggregate/approved
    diagnostics (evolution-diagnostics); the paired result artifacts
    carry only case IDs + scores + outputs — never case prompts,
    expected outputs, or case bodies (hidden-data-contaminants is
    empty; the runner asserts this before returning).
  - Step 6: case-level results persist by content hash and, when the
    evaluator carries an evaluator-only :artifact/root, as EDN files
    under that root — a path never mounted into candidate workspaces
    (Global Constraint 23)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.eval.paired :as paired]
            [evoclj.genome.hash :as hash]
            [evoclj.provider.protocol :as proto])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)))

;; --- temp-path lifecycle -----------------------------------------------------

(def ^:private temp-paths (atom []))

(defn- temp-path!
  "Create a temp path (directory unless `file?`) and register it for
  cleanup."
  ([prefix] (temp-path! prefix false))
  ([prefix file?]
   (let [p (if file?
             (str (Files/createTempFile prefix "" (make-array FileAttribute 0)))
             (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))]
     (swap! temp-paths conj p)
     p)))

(defn- delete-tree!
  [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(defn- cleanup!
  []
  (doseq [p @temp-paths]
    (delete-tree! (Paths/get p (make-array String 0))))
  (reset! temp-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(defn- write-file!
  [path content]
  (let [p (Paths/get path (make-array String 0))
        parent (.getParent p)]
    (when parent
      (Files/createDirectories parent (make-array FileAttribute 0)))
    (Files/write p (.getBytes ^String content StandardCharsets/UTF_8)
                 (make-array java.nio.file.OpenOption 0))))

;; --- fixture bundles ----------------------------------------------------------

(defn- route-source
  "A route program: {:op :echo :text t} emits a :fixture/echo tool-call
  with (transform t); anything else finishes."
  [transform-expr]
  (str "(ns agent.route)\n"
       "(defn- transform [text] " transform-expr ")\n"
       "(defn run [input]\n"
       "  (let [op (get input :op)]\n"
       "    (case op\n"
       "      :echo {:action {:intent/type :intent/tool-call\n"
       "                      :payload {:tool/id :fixture/echo\n"
       "                                :args {:text (transform (get input :text))}}}}\n"
       "      {:action {:intent/type :intent/finish :payload {:value input}}})))\n"))

(defn- bundle!
  "Build a genome bundle in a fresh temp dir and return its path
  string. The topology is :sci router → :emit; the router runs the
  route program built from `transform-expr` (default: identity)."
  ([transform-expr]
   (let [dir (temp-path! "paired-bundle-")]
     (write-file! (str dir "/manifest.edn")
                  (pr-str {:genome/format 1
                           :agent/id :main
                           :agent/entry :graph/main
                           :abi {:kernel 1 :genome 1 :intent 1 :tool 1}
                           :modules {:topology "topology.edn"
                                     :models "models.edn"
                                     :memory "memory.edn"
                                     :evolution "evolution.edn"}
                           :capabilities/requested #{:model/call}
                           :evolution {:max-risk :behavioral
                                       :mutable #{:parameters :prompts
                                                  :skills :programs}}
                           :metadata {:name "paired-fixture"
                                      :description "paired selection test candidate"}}))
     (write-file! (str dir "/topology.edn")
                  (pr-str {:graph/id :graph/paired
                           :entry :node/router
                           :nodes {:node/router {:node/type :sci
                                                 :program :program/route
                                                 :next :node/emit}
                                   :node/emit {:node/type :emit}}
                           :limits {:max-steps 64}}))
     (write-file! (str dir "/models.edn")
                  "{:models {:planner {:alias :reasoning/high}}}")
     (write-file! (str dir "/memory.edn") "{:memory {}}")
     (write-file! (str dir "/evolution.edn") "{:evolution {}}")
     (write-file! (str dir "/programs/route.clj") (route-source transform-expr))
     dir)))

(defn- provider-catalog
  []
  (edn/read-string (slurp (io/resource "fixtures/resolution/provider-catalog.edn"))))

(defn- route-descriptor
  []
  {:program/id :program/route
   :file "programs/route.clj"
   :entry 'agent.route/run
   :input-schema :schema/route-input
   :output-schema :schema/intent-or-route})

(defn- echo-decision
  [text]
  {:action {:intent/type :intent/tool-call
            :payload {:tool/id :fixture/echo :args {:text text}}}})

(defn- seed-suffix
  "The 8 hex chars of the derived seed that a seed-sensitive fixture
  embeds in its response."
  [seed]
  (subs seed 8 16))

;; --- the seeded fixture -------------------------------------------------------

(defn- seeded-echo-provider
  "A DETERMINISTIC fixture provider. When `seed-sensitive?` the echoed
  text is suffixed with the seed's 8 hex chars — the fixture VERSION is
  a pure function of the seed, so two sides sharing one seed observe
  byte-identical fixture behavior and a side with a different seed
  would mismatch the oracle. Otherwise the suffix is fixed. Either way
  every received seed is recorded into `seed-log`."
  ([seed seed-log] (seeded-echo-provider seed seed-log false))
  ([seed seed-log seed-sensitive?]
   (let [suffix (if seed-sensitive? (seed-suffix seed) "fixed")]
     (reify proto/Provider
       (describe [_]
         {:tool/id :fixture/echo
          :effect :pure
          :input-schema [:map [:text :string]]
          :output-schema [:map [:text :string]]
          :required-action :invoke})
       (normalize-request [_ intent]
         {:tool/id :fixture/echo
          :resource {:kind :tool :id :fixture/echo}
          :args (get-in intent [:payload :args])})
       (execute-request! [_ authorized-request]
         (swap! seed-log conj seed)
         {:text (str (get-in authorized-request [:args :text]) "-" suffix)})))))

;; --- evaluator / request helpers ---------------------------------------------

(defn- selection-case
  "A selection case exercising the seeded echo fixture. `oracle-text`
  is the fixture result the case's oracle expects."
  ([case-id text] (selection-case case-id text {:text (str text "-fixed")}))
  ([case-id text oracle]
   {:case/id case-id
    :task-input {:op :echo :text text}
    :expected-output [(echo-decision text) oracle]
    :tools #{:fixture/echo}}))

(defn- evaluator
  "A minimal valid paired evaluator context. `seed-log` receives every
  seed the fixture is constructed with (Step 1 observability); the
  fixture fn is seed-aware so the runner must hand it the pair's seed.
  Parent and candidate genome bundles are built fresh per call (both
  identity transforms unless overridden)."
  ([cases seed-log] (evaluator cases seed-log {}))
  ([cases seed-log overrides]
   (merge {:provider/catalog (provider-catalog)
           :programs (fn [_] [(route-descriptor)])
           :selection/cases cases
           :selection/fixtures
           {:fixture/echo (fn [seed] (seeded-echo-provider seed seed-log))}
           :genome/roots {"G42" (bundle! "text")
                          "C17" (bundle! "text")}}
          overrides)))

(defn- request
  ([case-set] (request case-set 3))
  ([case-set repetitions]
   {:parent-generation "G42"
    :candidate-id "C17"
    :case-set case-set
    :repetitions repetitions}))

;; ============================================================================
;; Step 1 — ONE derived persisted seed serves BOTH sides of every pair
;; ============================================================================

(deftest step-1-one-derived-seed-serves-both-sides-of-a-pair
  (let [seed-base "paired-seed-base-1"
        seed-log (atom [])
        ;; the fixture VERSION is a pure function of the seed, so the
        ;; oracle must be computed from the very seed the runner derives
        expected-seed (paired/derive-seed seed-base :sel/c1 1)
        oracle {:text (str "hi-" (seed-suffix expected-seed))}
        case (selection-case :sel/c1 "hi" oracle)
        ev (evaluator {:sel/c1 case} seed-log
                      {:seed seed-base
                       :selection/fixtures
                       {:fixture/echo
                        (fn [seed] (seeded-echo-provider seed seed-log true))}})
        result (paired/run-paired-selection!
                ev (request [:sel/c1] 1))
        pair (first (:pairs result))]
    (testing "one persisted seed is derived per case/repetition"
      (is (= expected-seed (:pair/seed pair))
          "the pair records the persisted seed")
      (is (= "sha256:" (subs (:pair/seed pair) 0 7))))
    (testing "the SAME derived seed reaches BOTH sides of the pair"
      (is (= [expected-seed expected-seed] @seed-log)
          "parent and candidate fixtures were constructed with one shared seed"))
    (testing "both sides score against the oracle derived from that one seed"
      (is (= 1.0 (get-in pair [:sides :parent :side/score])))
      (is (= 1.0 (get-in pair [:sides :candidate :side/score])))
      (is (= 1.0 (get-in pair [:case/outcome :score/parent])))
      (is (= 1.0 (get-in pair [:case/outcome :score/candidate])))
      (is (= :tie (:status (:case/outcome pair)))))))

(deftest step-1-seeds-are-deterministic-and-distinct-per-repetition
  (let [seed-base "paired-seed-base-2"
        cases {:sel/c1 (selection-case :sel/c1 "hi")}
        mk (fn [] (evaluator cases (atom []) {:seed seed-base}))
        result-a (paired/run-paired-selection! (mk) (request [:sel/c1] 2))
        result-b (paired/run-paired-selection! (mk) (request [:sel/c1] 2))
        seeds-a (mapv :pair/seed (:pairs result-a))
        seeds-b (mapv :pair/seed (:pairs result-b))]
    (testing "seeds are deterministic across runs with the same seed base"
      (is (= seeds-a seeds-b)))
    (testing "each repetition derives a distinct seed"
      (is (= [(paired/derive-seed seed-base :sel/c1 1)
              (paired/derive-seed seed-base :sel/c1 2)]
             seeds-a))
      (is (apply distinct? seeds-a)))
    (testing "the seed base is reported on the result"
      (is (= seed-base (:seed/base result-a))))))

;; ============================================================================
;; Step 2 — execution order alternates pair by pair
;; ============================================================================

(deftest step-2-execution-order-alternates
  (let [result (paired/run-paired-selection!
                (evaluator {:sel/c1 (selection-case :sel/c1 "hi")} (atom []))
                (request [:sel/c1] 3))]
    (is (= [[:parent :candidate] [:candidate :parent] [:parent :candidate]]
           (mapv :order (:pairs result)))
        "pair 1 parent-then-candidate, pair 2 candidate-then-parent, ...")))

;; ============================================================================
;; Step 3 — the parent is RE-EVALUATED now (never a stale historical score)
;; ============================================================================

(deftest step-3-parent-is-re-evaluated-now
  (let [result (paired/run-paired-selection!
                (evaluator {:sel/c1 (selection-case :sel/c1 "hi")} (atom []))
                (request [:sel/c1] 2))
        pairs (:pairs result)
        parent-sides (mapv #(get-in % [:sides :parent]) pairs)
        candidate-sides (mapv #(get-in % [:sides :candidate]) pairs)]
    (testing "the runner accepts NO parent score input — nothing stale to compare"
      (is (nil? (:parent/score (request [:sel/c1] 2)))
          "the request shape has no :parent/score key")
      (is (false? (contains? (request [:sel/c1] 2) :parent/score))))
    (testing "the parent side is re-evaluated in EVERY pair, freshly"
      (is (= 2 (count parent-sides)))
      (is (every? uuid? (map :side/session-id parent-sides)))
      (is (apply distinct? (map :side/session-id parent-sides)))
      (is (every? #(re-matches #"^sha256:[0-9a-f]{64}$" %) (map :side/output-ref parent-sides))
          "the re-evaluated parent really ran and persisted its outputs"))
    (testing "parent and candidate sessions of one pair are distinct"
      (is (apply distinct?
                 (mapcat (fn [p] [(get-in p [:sides :parent :side/session-id])
                                  (get-in p [:sides :candidate :side/session-id])])
                         pairs))))))

;; ============================================================================
;; Step 4 — FRESH Phenotype instances and FRESH session namespaces per side
;; ============================================================================

(deftest step-4-fresh-phenotype-and-session-per-side
  (let [result (paired/run-paired-selection!
                (evaluator {:sel/c1 (selection-case :sel/c1 "hi")} (atom []))
                (request [:sel/c1] 2))
        sides (mapcat (fn [p] [(get-in p [:sides :parent])
                               (get-in p [:sides :candidate])])
                      (:pairs result))]
    (testing "every side of every pair is a distinct Phenotype INSTANCE"
      (is (= 4 (count sides)))
      (is (apply distinct? (map :side/instance-id sides))))
    (testing "every side runs in a distinct fresh session namespace"
      (is (every? uuid? (map :side/session-id sides)))
      (is (apply distinct? (map :side/session-id sides))))))

;; ============================================================================
;; Step 5 — results carry no hidden case prompts / expected outputs
;; ============================================================================

(deftest step-5-results-embed-no-hidden-case-data
  (let [result (paired/run-paired-selection!
                (evaluator {:sel/c1 (selection-case :sel/c1 "hi")} (atom []))
                (request [:sel/c1] 2))]
    (testing "the returned result embeds no hidden case data"
      (is (empty? (paired/hidden-data-contaminants result))))
    (testing "every pair's case-level outcome embeds no hidden case data"
      (doseq [p (:pairs result)]
        (is (empty? (paired/hidden-data-contaminants p)))
        (is (empty? (paired/hidden-data-contaminants (:case/outcome p))))))
    (testing "the case bodies never leak into the result"
      (let [pr (pr-str result)]
        (is (not (re-find #":task-input" pr)))
        (is (not (re-find #":expected-output" pr)))))
    (testing "the Mutator-facing diagnostic surface is clean and score-only"
      (let [diag (paired/evolution-diagnostics result)]
        (is (empty? (paired/hidden-data-contaminants diag)))
        (is (every? #(= #{:pair/index :case/id :repetition :pair/seed :order
                          :score/parent :score/candidate :delta :status}
                        (set (keys %)))
                    (:pairs diag))
            "per-pair diagnostics carry scores only — no outputs, no prompts")))))

;; ============================================================================
;; Step 6 — case-level results persist under an evaluator-only artifact path
;; ============================================================================

(deftest step-6-case-level-results-persist-under-evaluator-only-root
  (let [artifact-root (temp-path! "paired-artifacts-")
        result (paired/run-paired-selection!
                (evaluator {:sel/c1 (selection-case :sel/c1 "hi")} (atom [])
                           {:artifact/root artifact-root})
                (request [:sel/c1] 2))]
    (testing "every pair reports a content-hash artifact ref"
      (doseq [p (:pairs result)]
        (is (re-matches #"^sha256:[0-9a-f]{64}$" (:result/artifact-ref p)))))
    (testing "case-level results are written under the evaluator-only root"
      (doseq [p (:pairs result)]
        (is (Files/isRegularFile
             (Paths/get (:result/artifact-path p) (make-array String 0))
             (make-array LinkOption 0)))
        (is (str/starts-with? (:result/artifact-path p) artifact-root))))
    (testing "the persisted files hold ONLY case IDs + scores + outputs"
      (doseq [p (:pairs result)]
        (let [on-disk (edn/read-string (slurp (:result/artifact-path p)))]
          (is (empty? (paired/hidden-data-contaminants on-disk)))
          (is (= (:case/id p) (:case/id on-disk)))
          (is (contains? on-disk :parent))
          (is (contains? on-disk :candidate))
          (is (nil? (:task-input on-disk)))
          (is (nil? (:expected-output on-disk)))
          (is (= (:result/artifact-ref p)
                 (hash/text-digest (slurp (:result/artifact-path p))))
              "the persisted file is exactly the ref'd artifact"))))))

;; ============================================================================
;; failure modes — fail closed, never silently
;; ============================================================================

(deftest missing-fixture-fails-closed
  (let [ev (assoc (evaluator {:sel/c1 (selection-case :sel/c1 "hi")} (atom []))
                  :selection/fixtures {})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"fixture"
          (paired/run-paired-selection! ev (request [:sel/c1] 1))))))

(deftest unknown-selection-case-fails-closed
  (let [ev (evaluator {:sel/c1 (selection-case :sel/c1 "hi")} (atom []))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"case"
          (paired/run-paired-selection! ev (request [:sel/ghost] 1))))))

(deftest invalid-request-fails-closed
  (let [ev (evaluator {:sel/c1 (selection-case :sel/c1 "hi")} (atom []))]
    (testing "a non-positive repetition count is rejected"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"repetitions"
            (paired/run-paired-selection! ev (request [:sel/c1] 0)))))
    (testing "an empty case set is rejected"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"case-set"
            (paired/run-paired-selection! ev (request [] 1)))))
    (testing "a non-string candidate id is rejected"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"candidate"
            (paired/run-paired-selection! ev (assoc (request [:sel/c1] 1)
                                                    :candidate-id :C17)))))))

(deftest invalid-evaluator-context-fails-closed
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"catalog"
        (paired/run-paired-selection!
         (dissoc (evaluator {:sel/c1 (selection-case :sel/c1 "hi")} (atom []))
                 :provider/catalog)
         (request [:sel/c1] 1)))))

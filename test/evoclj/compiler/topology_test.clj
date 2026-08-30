(ns evoclj.compiler.topology-test
  "Tests for topology IR validation and compilation (component).

  compile-topology turns a Genome's topology.edn value into validated,
  normalized adjacency IR. It rejects unknown node types, a missing
  entry node, dangling :next edges, node ids that would collide when
  merged, and — per Step 2 — any raw graph cycle that does not pass
  through an explicit :loop node (only :loop nodes may iterate; their
  runtime semantics arrive in component, so only their shape is
  validated here). The compiled value is pure sorted EDN data (Global
  Constraint 22) whose equality and serialization do not depend on the
  key order of the source topology (Step 4)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [evoclj.compiler.topology :as topology]))

(defn- compile-error
  "The ExceptionInfo thrown by compile-topology, or nil."
  [t]
  (try (topology/compile-topology t)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

;; --- supported v0 node types ----------------------------------------------

(deftest supported-v0-node-types
  (is (= #{:llm :sci :tool :route :loop :emit :memory/read :memory/write}
         topology/supported-node-types)))

(deftest syntax-vs-executable-split
  (testing "syntax is the full v0 syntactic set including :route"
    (is (= #{:llm :sci :tool :route :loop :emit :memory/read :memory/write}
           topology/syntax-node-types))
    (is (= topology/syntax-node-types topology/supported-node-types)))
  (testing "executable is the runtime feature set (handler exists)"
    (is (= #{:llm :sci :tool :loop :emit :memory/read :memory/write}
           topology/executable-node-types))
    (is (not (contains? topology/executable-node-types :route))))
  (testing "known-unimplemented is exactly syntax minus executable"
    (is (= #{:route} (set/difference topology/syntax-node-types topology/executable-node-types)))))

(deftest route-topology-rejected-without-handler
  (testing ":route is syntax-only and unrepresentable via compile (Definition > validation)"
    (let [t {:graph/id :graph/main
             :entry :node/a
             :nodes {:node/a {:node/type :route :next :node/b}
                     :node/b {:node/type :emit}}}
          e (compile-error t)]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (= :topology/unsupported-node-type (:error/type (ex-data e))))
      (is (= :unsupported-node-type (:reason (ex-data e))))
      (is (= :node/a (:node-id (ex-data e))))
      (is (= :route (:node/type (ex-data e))))))
  (testing "with an expanded runtime feature set that includes :route, the same topology compiles"
    (let [t {:graph/id :graph/main
             :entry :node/a
             :nodes {:node/a {:node/type :route :next :node/b}
                     :node/b {:node/type :emit}}}
          c (topology/compile-topology t (conj topology/executable-node-types :route))]
      (is (map? c))
      (is (= :route (get-in c [:nodes :node/a :node/type])))
      (is (= [:node/b] (get-in c [:adjacency :node/a]))))))

;; --- seed fixture ----------------------------------------------------------

(deftest seed-topology-fixture-compiles
  (let [t (edn/read-string
           (slurp (io/resource "fixtures/genomes/minimal-valid/topology.edn")))
        c (topology/compile-topology t)]
    (testing "the example chain planner :llm -> router :sci -> finish :emit"
      (is (= :node/planner (:entry c)))
      (is (= :llm (get-in c [:nodes :node/planner :node/type])))
      (is (= :planner (get-in c [:nodes :node/planner :model])))
      (is (= :sci (get-in c [:nodes :node/router :node/type])))
      (is (= :program/route (get-in c [:nodes :node/router :program])))
      (is (= :emit (get-in c [:nodes :node/finish :node/type]))))
    (testing "every compiled node is self-describing"
      (is (= :node/planner (get-in c [:nodes :node/planner :node/id])))
      (is (= :node/router (get-in c [:nodes :node/router :node/id]))))
    (testing "adjacency is the normalized next-edge map"
      (is (= {:node/planner [:node/router]
              :node/router []
              :node/finish []}
             (:adjacency c))))
    (testing "limits pass through"
      (is (= {:max-steps 64} (:limits c))))
    (testing "compiled IR is pure serializable EDN data (Global Constraint 22)"
      (is (= c (edn/read-string (pr-str c)))))))

;; --- unknown node type -----------------------------------------------------

(deftest unknown-node-type-rejected
  (let [e (compile-error {:graph/id :graph/main
                          :entry :node/a
                          :nodes {:node/a {:node/type :warp}}})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :topology/invalid (:error/type (ex-data e))))
    (is (= :unknown-node-type (:reason (ex-data e))))
    (is (= :node/a (:node-id (ex-data e))))
    (is (= :warp (:node/type (ex-data e))))))

(deftest missing-required-node-key-rejected
  (let [e (compile-error {:graph/id :graph/main
                          :entry :node/a
                          :nodes {:node/a {:node/type :llm}}})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :topology/invalid (:error/type (ex-data e))))
    (is (= :missing-required-key (:reason (ex-data e))))
    (is (= :node/a (:node-id (ex-data e))))
    (is (= :model (:key (ex-data e))))))

;; --- missing entry node ----------------------------------------------------

(deftest missing-entry-node-rejected
  (let [e (compile-error {:graph/id :graph/main
                          :entry :node/ghost
                          :nodes {:node/a {:node/type :emit}}})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :topology/invalid (:error/type (ex-data e))))
    (is (= :missing-entry (:reason (ex-data e))))
    (is (= :node/ghost (:entry (ex-data e))))))

;; --- dangling :next --------------------------------------------------------

(deftest dangling-next-rejected
  (let [e (compile-error {:graph/id :graph/main
                          :entry :node/a
                          :nodes {:node/a {:node/type :llm
                                           :model :planner
                                           :next :node/ghost}}})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :topology/invalid (:error/type (ex-data e))))
    (is (= :dangling-next (:reason (ex-data e))))
    (is (= :node/a (:node-id (ex-data e))))
    (is (= :node/ghost (:next (ex-data e))))))

;; --- duplicate IDs after merge ---------------------------------------------

(deftest duplicate-node-ids-after-merge-rejected
  (let [e (compile-error {:graph/id :graph/main
                          :entry :node/a
                          :nodes [[:node/a {:node/type :emit}]
                                  [:node/a {:node/type :emit}]]})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :topology/invalid (:error/type (ex-data e))))
    (is (= :duplicate-node-id (:reason (ex-data e))))
    (is (= [:node/a] (:node-ids (ex-data e))))))

;; --- illegal raw cycles (Step 2) ------------------------------------------

(deftest illegal-raw-cycle-rejected
  (let [e (compile-error {:graph/id :graph/main
                          :entry :node/a
                          :nodes {:node/a {:node/type :sci
                                           :program :program/route
                                           :next :node/b}
                                  :node/b {:node/type :sci
                                           :program :program/route
                                           :next :node/c}
                                  :node/c {:node/type :sci
                                           :program :program/route
                                           :next :node/a}}})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :topology/cycle (:error/type (ex-data e))))
    (is (= [:node/a :node/b :node/c] (:nodes (ex-data e))))))

(deftest self-loop-without-loop-node-rejected
  (let [e (compile-error {:graph/id :graph/main
                          :entry :node/a
                          :nodes {:node/a {:node/type :llm
                                           :model :planner
                                           :next :node/a}}})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :topology/cycle (:error/type (ex-data e))))
    (is (= [:node/a] (:nodes (ex-data e))))))

(deftest explicit-loop-node-cycle-is-allowed
  (testing "a self-cycle through a :loop node compiles"
    (let [c (topology/compile-topology
             {:graph/id :graph/main
              :entry :node/b
              :nodes {:node/b {:node/type :loop
                               :body :node/body
                               :until :program/done?
                               :max-iterations 8
                               :next :node/b}
                      :node/body {:node/type :emit}}})]
      (is (map? c))
      (is (= [:node/b] (get-in c [:adjacency :node/b])))))
  (testing "a :next cycle that passes through a :loop node compiles"
    (let [c (topology/compile-topology
             {:graph/id :graph/main
              :entry :node/a
              :nodes {:node/a {:node/type :sci
                               :program :program/route
                               :next :node/b}
                      :node/b {:node/type :loop
                               :body :node/body
                               :until :program/done?
                               :max-iterations 8
                               :next :node/a}
                      :node/body {:node/type :emit}}})]
      (is (= [:node/b] (get-in c [:adjacency :node/a])))
      (is (= [:node/a] (get-in c [:adjacency :node/b]))))))

;; --- ordering independence (Step 4) ---------------------------------------

(deftest node-ordering-does-not-affect-compiled-topology
  (let [t1 {:graph/id :graph/main
            :entry :node/planner
            :nodes {:node/planner {:node/type :llm :model :planner :next :node/router}
                    :node/router {:node/type :sci :program :program/route}
                    :node/finish {:node/type :emit}}
            :limits {:max-steps 64}}
        t2 {:limits {:max-steps 64}
            :nodes (sorted-map-by (fn [a b] (compare b a))
                                  :node/finish {:node/type :emit}
                                  :node/router {:program :program/route :node/type :sci}
                                  :node/planner {:next :node/router :model :planner :node/type :llm})
            :entry :node/planner
            :graph/id :graph/main}
        c1 (topology/compile-topology t1)
        c2 (topology/compile-topology t2)]
    (testing "equal compiled topology regardless of EDN key order"
      (is (= c1 c2)))
    (testing "deterministic serialization of the compiled IR"
      (is (= (pr-str c1) (pr-str c2))))))

;; --- input shape validation ------------------------------------------------

(deftest invalid-topology-shapes-rejected
  (testing "topology that is not a map"
    (let [e (compile-error [:not :a :map])]
      (is (instance? clojure.lang.ExceptionInfo e))
      (is (= :topology/invalid (:error/type (ex-data e))))
      (is (= :invalid-topology (:reason (ex-data e))))))
  (testing ":graph/id must be a keyword"
    (let [e (compile-error {:graph/id "graph/main"
                            :entry :node/a
                            :nodes {:node/a {:node/type :emit}}})]
      (is (= :invalid-graph-id (:reason (ex-data e))))))
  (testing ":entry must be a keyword"
    (let [e (compile-error {:graph/id :graph/main
                            :entry "node/a"
                            :nodes {:node/a {:node/type :emit}}})]
      (is (= :invalid-entry (:reason (ex-data e))))))
  (testing ":nodes must be a map or a vector of pairs"
    (let [e (compile-error {:graph/id :graph/main
                            :entry :node/a
                            :nodes "nope"})]
      (is (= :invalid-nodes (:reason (ex-data e))))))
  (testing "invalid :limits"
    (let [e (compile-error {:graph/id :graph/main
                            :entry :node/a
                            :nodes {:node/a {:node/type :emit}}
                            :limits {:max-steps 0}})]
      (is (= :invalid-limits (:reason (ex-data e)))))))
 
;; --- compositional edge typing (PLT4) -------------------------------------

(deftest compositional-edge-types-are-checked
  (let [compiled (topology/compile-topology
                  {:graph/id :graph/typed
                   :entry :node/source
                   :nodes {:node/source {:node/type :emit
                                         :output-schema :schema/string
                                         :next :node/sink}
                           :node/sink {:node/type :emit
                                       :input-schema :schema/string}}})]
    (testing "registered schema keywords resolve into compiled node data"
      (is (= :schema/string
             (get-in compiled [:nodes :node/source :output-schema])))
      (is (= :string
             (get-in compiled [:nodes :node/source :schema/output])))
      (is (= :schema/any
             (get-in compiled [:nodes :node/sink :output-schema])))
      (is (= :any
             (get-in compiled [:nodes :node/sink :schema/output]))))
    (testing "typed compiled topology remains pure EDN"
      (is (= compiled (edn/read-string (pr-str compiled)))))))

(deftest mismatched-next-edge-is-rejected-at-compile-time
  (let [e (compile-error
           {:graph/id :graph/typed
            :entry :node/source
            :nodes {:node/source {:node/type :emit
                                  :output-schema :schema/string
                                  :next :node/sink}
                    :node/sink {:node/type :emit
                                :input-schema :schema/int}}})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :topology/type-mismatch (:error/type (ex-data e))))
    (is (= :type-mismatch (:reason (ex-data e))))
    (is (= :next (:edge (ex-data e))))
    (is (= :node/source (:from (ex-data e))))
    (is (= :node/sink (:to (ex-data e))))
    (is (= :schema/string (:output-schema (ex-data e))))
    (is (= :schema/int (:input-schema (ex-data e))))))

(deftest mismatched-body-edge-is-rejected-at-compile-time
  (let [e (compile-error
           {:graph/id :graph/typed-loop
            :entry :node/loop
            :nodes {:node/loop {:node/type :loop
                                :body :node/body
                                :until :program/done?
                                :max-iterations 2
                                :input-schema :schema/int
                                :output-schema :schema/string
                                :next :node/exit}
                    :node/body {:node/type :emit
                                :input-schema :schema/int}
                    :node/exit {:node/type :emit}}})]
    (is (= :topology/type-mismatch (:error/type (ex-data e))))
    (is (= :body (:edge (ex-data e))))
    (is (= :node/loop (:from (ex-data e))))
    (is (= :node/body (:to (ex-data e))))))

(deftest phantom-topology-schema-is-rejected
  (let [e (compile-error
           {:graph/id :graph/typed
            :entry :node/source
            :nodes {:node/source {:node/type :emit
                                  :input-schema :schema/unicorn}}})]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (= :topology/invalid (:error/type (ex-data e))))
    (is (= :unknown-schema (:reason (ex-data e))))
    (is (= :schema/unicorn (:schema (ex-data e))))
    (is (= :input-schema (:field (ex-data e))))))

(deftest structural-subtyping-allows-json-union-output
  (testing "a JSON string output is accepted by the generic JSON schema"
    (let [compiled (topology/compile-topology
                    {:graph/id :graph/typed
                     :entry :node/source
                     :nodes {:node/source {:node/type :emit
                                           :output-schema :schema/string
                                           :next :node/sink}
                             :node/sink {:node/type :emit
                                         :input-schema :schema/json}}})]
      (is (= :schema/string
             (get-in compiled [:nodes :node/source :output-schema])))
      (is (= :schema/json
             (get-in compiled [:nodes :node/sink :input-schema]))))))

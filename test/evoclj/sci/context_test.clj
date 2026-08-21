(ns evoclj.sci.context-test
  "Tests for the closed SCI execution context with an explicit allow
  surface (component).

  make-context returns a Babashka SCI context in which evolvable Genome
  programs run with NO ambient host authority (Global Constraint 7): no
  filesystem, no environment, no Java interop, no process execution, no
  dynamic loading, no arbitrary host vars. The context is configured by
  explicit :namespaces / :classes / :allow policy ONLY — never
  :allow :all — and the only host code reachable from inside it is the
  pure evo.api.intent data constructor namespace (plain maps only; the
  typed Intent ABI arrives in Milestone 4).

  Step 1 asserts what the closed surface DOES allow: arithmetic, maps
  and vectors, pure function definition/calls, and the exposed
  evo.api.intent constructors. Step 2 asserts what it MUST deny:
  System/getenv, java.io.File, Runtime/getRuntime, ProcessBuilder,
  slurp, spit, load-file, host eval, and undeclared require all throw
  inside the context. Step 4 asserts definitions made inside the SCI
  context never mutate host Clojure Vars."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [evoclj.sci.context :as context]
            [sci.core :as sci]))

;; --- shared helpers --------------------------------------------------------

(defn- route-source
  "The seed Genome routing program source (component fixture)."
  []
  (slurp (io/resource "fixtures/genomes/minimal-valid/programs/route.clj")))

(defn- eval-in
  "Evaluate one form string in a fresh closed context."
  [form]
  (sci/eval-string* (context/make-context {}) form))

(defn- denied?
  "True when evaluating `form` in a fresh closed context throws (the
  form is denied or unresolvable), never when it returns a value."
  [form]
  (try
    (eval-in form)
    false
    (catch clojure.lang.ExceptionInfo _ true)))

;; ============================================================================
;; Step 1 — what the closed surface allows
;; ============================================================================

(deftest arithmetic-evaluates-in-context
  (testing "pure arithmetic works inside the context"
    (is (= 6 (eval-in "(+ 1 2 3)")))
    (is (= 42 (eval-in "(* 2 21)")))
    (is (= 1 (eval-in "(mod 7 3)")))
    (is (= 2 (eval-in "(max 1 2)")))
    (is (true? (eval-in "(< 1 2 3)")))))

(deftest maps-and-vectors-are-first-class
  (testing "maps, vectors, and nested collections are plain data"
    (is (= {:a 1 :b 3} (eval-in "{:a 1 :b (+ 1 2)}")))
    (is (= 3 (eval-in "(get-in {:a {:b [1 2 3]}} [:a :b 2])")))
    (is (= [2 3 4] (eval-in "(mapv inc [1 2 3])")))
    (is (= {:x 1 :y 2} (eval-in "(merge {:x 1} {:y 2})")))
    (is (= {:a {:b 3}} (eval-in "(assoc-in {} [:a :b] 3)")))))

(deftest pure-functions-can-be-defined-and-called
  (testing "defn / fn / let / case work for pure decision logic"
    (is (= 42 (eval-in "(defn answer [] (* 6 7)) (answer)")))
    (is (= [2 4 6] (eval-in "(defn double-all [xs] (mapv (fn [x] (* x 2)) xs)) (double-all [1 2 3])")))
    (is (= 15 (eval-in "(reduce + (range 1 6))")))
    (is (= 2 (eval-in "(case :b :a 1 :b 2 3)")))
    (is (= 3 (eval-in "(let [{:keys [a b]} {:a 1 :b 2}] (+ a b))")))
    (is (= 3 (eval-in "(let [[x y] [1 2]] (+ x y))")))))

(deftest intent-constructors-produce-plain-data-maps
  (testing "the exposed evo.api.intent namespace builds plain maps only"
    (let [tool (eval-in "(evo.api.intent/tool-call {:tool/id :fixture/echo :args {:text \"hi\"}})")
          done (eval-in "(evo.api.intent/finish 42)")]
      (is (= {:intent/type :intent/tool-call
              :payload {:tool/id :fixture/echo :args {:text "hi"}}}
             tool))
      (is (= {:intent/type :intent/finish :payload {:value 42}} done))
      (testing "results are pure serializable EDN (Global Constraint 22)"
        (is (= tool (edn/read-string (pr-str tool))))
        (is (= done (edn/read-string (pr-str done))))))))

(deftest api-namespaces-can-be-extended-explicitly
  (testing "a caller can expose additional pure namespaces via :api-namespaces"
    (let [ctx (context/make-context
               {:api-namespaces {'evo.api.fixture {'answer (constantly 42)}}})]
      (is (= 42 (sci/eval-string* ctx "(evo.api.fixture/answer)")))
      (testing "host vars of other namespaces stay unreachable"
        (is (denied? "(clojure.string/upper-case \"x\")"))))))

(deftest run-form-evaluates-the-route-fixture-end-to-end
  (let [ctx (context/make-context {:programs [] :limits {}})
        source (route-source)]
    (testing "echo input routes to a tool-call intent"
      (is (= {:action {:intent/type :intent/tool-call
                       :payload {:tool/id :fixture/echo :args {:text "hi"}}}}
             (context/run-form ctx source 'agent.route/run {:op :echo :text "hi"}))))
    (testing "finish input routes to a finish intent"
      (is (= {:action {:intent/type :intent/finish :payload {:value 7}}}
             (context/run-form ctx source 'agent.route/run {:op :finish :value 7}))))
    (testing "an unknown op falls through to finish carrying the input"
      (is (= {:action {:intent/type :intent/finish :payload {:value {:op :weird}}}}
             (context/run-form ctx source 'agent.route/run {:op :weird}))))))

;; ============================================================================
;; Step 2 — what the closed surface MUST deny
;; ============================================================================

(deftest filesystem-environment-and-process-forms-are-denied
  (doseq [[label form] [["System/getenv" "(System/getenv \"PATH\")"]
                        ["java.io.File constructor" "(java.io.File. \"/tmp/x\")"]
                        ["java.io.File via new" "(new java.io.File \"/tmp/x\")"]
                        ["Runtime/getRuntime" "(Runtime/getRuntime)"]
                        ["ProcessBuilder constructor" "(ProcessBuilder. [\"ls\"])"]
                        ["slurp" "(slurp \"/etc/passwd\")"]
                        ["spit" "(spit \"/tmp/x\" \"y\")"]
                        ["load-file" "(load-file \"evil.clj\")"]
                        ["load-string" "(load-string \"(+ 1 2)\")"]]]
    (is (denied? form) label)))

(deftest host-eval-is-denied
  (doseq [[label form] [["bare eval" "(eval '(+ 1 2))"]
                        ["fully qualified eval" "(clojure.core/eval '(+ 1 2))"]
                        ["read-string" "(read-string \"(+ 1 2)\")"]
                        ["clojure.edn read-string" "(clojure.edn/read-string \"1\")"]]]
    (is (denied? form) label)))

(deftest undeclared-require-is-denied
  (doseq [[label form] [["bare require" "(require 'clojure.string)"]
                        ["aliased require" "(require '[clojure.string :as str])"]
                        ["host namespace require" "(require '[clojure.java.io :as io])"]
                        ["ns clause require" "(ns evil (:require [clojure.java.io :as io]))"]
                        ["use" "(use 'clojure.string)"]]]
    (is (denied? form) label)))

(deftest java-interop-and-dynamic-loading-are-denied
  (doseq [[label form] [["interop special form" "(. System (getenv \"PATH\"))"]
                        ["new special form" "(new System)"]
                        ["class loading" "(Class/forName \"java.lang.Runtime\")"]
                        ["static method access" "(String/valueOf 1)"]
                        ["threads" "(Thread/sleep 100)"]
                        ["arrays" "(make-array String 3)"]]]
    (is (denied? form) label)))

(deftest mutation-and-concurrency-primitives-are-denied
  (doseq [[label form] [["atom" "(atom 0)"]
                        ["swap!" "(swap! (atom 0) inc)"]
                        ["ref" "(ref 0)"]
                        ["delay" "(delay 1)"]
                        ["future" "(future 1)"]
                        ["promise" "(promise)"]
                        ["agent" "(agent 0)"]
                        ["var mutation" "(alter-var-root (var +) (constantly (fn [x y] 0)))"]]]
    (is (denied? form) label)))

(deftest hostile-program-source-is-denied-when-run
  (testing "run-form evaluates hostile source inside the closed context"
    (let [ctx (context/make-context {})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (context/run-form ctx
                                     "(ns evil)\n(defn run [x] (System/getenv \"PATH\"))"
                                     'evil/run {}))))))

;; ============================================================================
;; Step 4 — SCI definitions never touch host Clojure Vars
;; ============================================================================

(def ^:dynamic *host-probe*
  "Host var used to prove SCI defs do not mutate host Clojure Vars."
  :host-value)

(deftest sci-definitions-do-not-create-host-vars
  (let [ctx (context/make-context {})]
    (sci/eval-string* ctx "(def *never-on-host* 41)")
    (is (nil? (resolve 'evoclj.sci.context-test/*never-on-host*)))))

(deftest sci-redefinition-does-not-mutate-host-vars
  (let [ctx (context/make-context {})]
    (sci/eval-string* ctx "(def *host-probe* :sci-value)")
    (is (= :host-value @#'evoclj.sci.context-test/*host-probe*))))

;; ============================================================================
;; context construction contract
;; ============================================================================

(deftest invalid-config-is-rejected-with-typed-error
  (is (thrown? clojure.lang.ExceptionInfo (context/make-context "not-a-map")))
  (is (thrown? clojure.lang.ExceptionInfo
               (context/make-context {:api-namespaces "not-a-map"}))))

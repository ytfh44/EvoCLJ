(ns evoclj.intent.budget-dispatch-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [evoclj.intent.core :as intent]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.registry :as registry]
            [evoclj.store.budget-store :as budget-store]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite]))

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private phenotype-id
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private cap-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def ^:private issued-at (java.util.Date. 0))
(def ^:private expires-at (java.util.Date. 4102444800000))
(def ^:private db-paths (atom []))

(defn- temp-db-path []
  (let [p (str (java.nio.file.Files/createTempFile
                "evoclj-dispatch-budget-" ".db"
                (make-array java.nio.file.attribute.FileAttribute 0)))]
    (swap! db-paths conj p)
    p))

(defn- cleanup! []
  (doseq [p @db-paths]
    (java.nio.file.Files/deleteIfExists
     (java.nio.file.Paths/get p (make-array String 0))))
  (reset! db-paths []))

(use-fixtures :each (fn [f] (f) (cleanup!)))

(deftest dispatch-reserves-and-fails-closed-on-budget-exhaustion
  (let [db (sqlite/spec (temp-db-path))
        _ (migrate/migrate! db)
        _ (budget-store/create-budget! db {:budget/id "dispatch-budget"
                                           :lease/id cap-id
                                           :budget {:calls 1}})
        counter (atom 0)
        reg (registry/create-registry)
        _ (registry/register! reg (fixture/echo-provider {:execution-count counter}))
        lease {:cap/id cap-id
               :principal {:principal/type :session :session/id session-id}
               :resource {:kind :tool :id :fixture/echo}
               :actions #{:invoke}
               :constraints {:max-calls 10}
               :budget {:calls 1}
               :issued-at issued-at
               :expires-at expires-at}
        ctx (dispatch/make-broker-context {:registry reg
                                           :leases [lease]
                                           :usage (atom {})
                                           :now (constantly (java.util.Date. 1700000000000))
                                           :db db})
        make-intent #(intent/tool-call session-id phenotype-id :node/tool 42
                                       {:tool/id :fixture/echo :args {:text "hi"}}
                                       {:wall-ms 1000})
        first-result (dispatch/dispatch! ctx (make-intent))
        second-result (dispatch/dispatch! ctx (make-intent))]
    (is (= :ok (:result/status first-result)))
    (is (= 1 @counter))
    (is (= :error (:result/status second-result)))
    (is (= :capability/budget-exhausted (:error/type second-result)))
    (is (= 1 @counter))
    (is (= {} (:reserved (budget-store/get-budget db "dispatch-budget"))))
    (is (= {:calls 1} (:consumed (budget-store/get-budget db "dispatch-budget"))))))

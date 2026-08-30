(ns evoclj.runtime.orchestrator-test
  "Orchestrator contract tests: 4-round behavior, unknown-tool fail-closed, budget handling."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.jdbc :as jdbc]
            [evoclj.runtime.orchestrator :as sut]
            [evoclj.runtime.assembler :as assembler]
            [evoclj.store.sqlite :as sqlite]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.session :as session]
            [evoclj.store.event :as event]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.intent.dispatch :as dispatch])
  (:import (java.nio.file Files Paths)
           (java.nio.file.attribute FileAttribute)))

(def ^:private hex64 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
(def ^:private genome-id (str "sha256:" hex64))
(def ^:private resolution-id (str "sha256:" (apply str (repeat 64 "c"))))
(def ^:private phenotype-id (str "sha256:" (apply str (repeat 64 "b"))))
(def ^:private generation-id "generation-1")
(def ^:private temp-paths (atom []))
(defn- temp-db-path [] (let [p (str (Files/createTempFile "orch-test-db" ".sqlite" (make-array FileAttribute 0)))] (swap! temp-paths conj p) p))
(defn- temp-cas-dir [] (let [d (str (Files/createTempDirectory "orch-test-cas" (make-array FileAttribute 0)))] (swap! temp-paths conj d) d))
(defn- delete-tree! [path]
  (let [p (Paths/get path (make-array String 0))]
    (when (Files/exists p (make-array java.nio.file.LinkOption 0))
      (if (Files/isDirectory p (make-array java.nio.file.LinkOption 0))
        (doseq [f (reverse (vec (file-seq (clojure.java.io/file path))))] (Files/deleteIfExists (.toPath f)))
        (Files/deleteIfExists p)))))
(defn- cleanup! [] (doseq [p @temp-paths] (try (delete-tree! p) (catch Throwable _ nil))) (reset! temp-paths []))
(use-fixtures :each (fn [f] (f) (cleanup!)))
(defn- fresh-db []
  (let [db (sqlite/spec (temp-db-path))]
    (migrate/migrate! db)
    (artifact/ensure-artifact! db genome-id "application/octet-stream" 0)
    (artifact/ensure-artifact! db resolution-id "application/edn" 0)
    (artifact/ensure-artifact! db phenotype-id "application/edn" 0)
    (artifact/ensure-genome! db genome-id)
    (sqlite/with-db [conn db]
      (jdbc/insert! conn :generations {:id generation-id :genome_id genome-id :resolution_id resolution-id :parent_id nil :state "active" :current 0 :created_at "2025-01-01T00:00:00Z"}))
    db))
(defn- fresh-cas [] (cas/->cas (temp-cas-dir)))
(defn- create-pinned-session [db]
  (let [sid (:session/id (session/create-session! db {:genome/id genome-id :resolution/id resolution-id :phenotype/id phenotype-id :generation/id generation-id}))]
    (event/append-event! db {:session/id sid :generation/id generation-id :phenotype/id phenotype-id :event/type :session/created :cause/event-id nil :payload-ref nil :metadata {}})
    sid))

(deftest four-round-behavior-default-is-4
  (testing "TraditionalOrchestrator defaults to max-tool-rounds 4"
    (is (instance? evoclj.runtime.orchestrator.TraditionalOrchestrator (sut/->TraditionalOrchestrator)))
    (is (satisfies? sut/Orchestrator (sut/->TraditionalOrchestrator)))))

(deftest budget-exhausted-via-max-steps
  (testing "topology max-steps budget halts as :budget-exhausted"
    (let [db (fresh-db) cas-root (fresh-cas)
          chain-nodes {:n0 {:node/type :tool :tool :fixture/echo :next :n1}
                       :n1 {:node/type :tool :tool :fixture/echo :next :n2}
                       :n2 {:node/type :emit}}
          executor {:phenotype {:phenotype/id phenotype-id
                                :compiled {:compiled/genome-id genome-id :compiled/resolution-id resolution-id :compiled/phenotype-id phenotype-id
                                           :topology {:entry :n0 :nodes chain-nodes :limits {:max-steps 1}}}}
                    :stores {:sqlite db :cas cas-root}
                    :dispatch {:leases [] :catalog {}}}
          sid (create-pinned-session db)]
      (with-redefs [dispatch/dispatch! (fn [_ _] {:result/status :ok :value {:text "ok"} :authorization {:decision :allow :lease-id "l1"}})]
        (let [result (scheduler/run-session! executor sid {:input "go"})]
          (is (= :budget-exhausted (:status result)))
          (let [events (event/events-for-session db sid)
                budget-events (filter #(= :session/budget-exhausted (:event/type %)) events)]
            (is (= 1 (count budget-events)))))))))

(deftest unknown-tool-fail-closed
  (testing "model requesting an unknown tool fails the session"
    (let [db (fresh-db) cas-root (fresh-cas)
          model-call-count (atom 0) tool-call-count (atom 0)
          executor {:phenotype {:phenotype/id phenotype-id
                                :compiled {:compiled/genome-id genome-id :compiled/resolution-id resolution-id :compiled/phenotype-id phenotype-id
                                           :topology {:entry :llm :nodes {:llm {:node/type :llm :model/id "fake/model" :tools [{:tool/id :echo-tool :name "echo_tool"}] :next :done} :done {:node/type :emit}} :limits {:max-steps 64}}}}
                    :stores {:sqlite db :cas cas-root}
                    :dispatch {:leases [] :catalog {}}}
          sid (create-pinned-session db)]
      (with-redefs [dispatch/dispatch! (fn [_ intent]
                                         (case (:intent/type intent)
                                           :intent/model-call
                                           (do (swap! model-call-count inc)
                                               {:result/status :ok :value {:model/output {:text "hi"} :tool-calls [{:tool/name "unknown_tool" :tool/call-id "call_1" :tool/arguments {}}]} :authorization {:decision :allow :lease-id "l1"}})
                                           :intent/tool-call
                                           (do (swap! tool-call-count inc)
                                               {:result/status :ok :value {:ok true}})))]
        (let [result (scheduler/run-session! executor sid {:input "test"})]
          (is (= :failed (:status result)))
          (is (= 0 @tool-call-count) "unknown tool never executed"))))))

(deftest max-tool-rounds-passthrough
  (testing "payload :options :max-tool-rounds 2 limits the tool loop to 2 rounds"
    (let [db (fresh-db) cas-root (fresh-cas)
          model-calls (atom 0) tool-calls (atom 0)
          executor {:phenotype {:phenotype/id phenotype-id
                                :compiled {:compiled/genome-id genome-id :compiled/resolution-id resolution-id :compiled/phenotype-id phenotype-id
                                           :topology {:entry :llm :nodes {:llm {:node/type :llm :model/id "fake/model"}} :limits {:max-steps 64}}}}
                    :stores {:sqlite db :cas cas-root}
                    :dispatch {:leases [] :catalog {}}}
          sid (create-pinned-session db)
          intent {:intent/id (str (random-uuid)) :intent/type :intent/model-call :session/id sid :phenotype/id phenotype-id :node/id :llm :budget {:wall-ms 1000}
                  :payload {:base/messages [{:role :user :content "hi"}] :messages [{:role :user :content "hi"}] :tools [{:name "echo_tool" :tool :echo-tool}] :requested-tools [{:tool/id :echo-tool :name "echo_tool"}] :model/id "fake/model" :options {:max-tool-rounds 2}}}
          _ (do (session/transition-session! db sid :created :resolving nil)
                (session/transition-session! db sid :resolving :running nil)
                (event/append-event! db {:session/id sid :generation/id generation-id :phenotype/id phenotype-id :event/type :session/started :cause/event-id (:event/id (first (event/events-for-session db sid))) :payload-ref nil :metadata {}}))
          pin2 (session/get-session db sid)
          cause-id (:event/id (last (event/events-for-session db sid)))
          orch (sut/->TraditionalOrchestrator)]
      (with-redefs [dispatch/dispatch! (fn [_ intent]
                                         (case (:intent/type intent)
                                           :intent/model-call
                                           (do (swap! model-calls inc)
                                               {:result/status :ok :value {:model/output {:text "hi"} :tool-calls [{:tool/name "echo_tool" :tool/call-id (str "call_" @model-calls) :tool/arguments {:text "x"}}]} :authorization {:decision :allow :lease-id "l1"}})
                                           :intent/tool-call
                                           (do (swap! tool-calls inc)
                                               {:result/status :ok :value {:echo "ok"} :authorization {:decision :allow :lease-id "l1"}})))
                    assembler/assemble (fn [& _] (throw (ex-info "skip assembler" {})))]
        (let [step (sut/orchestrate orch executor pin2 cause-id intent [])]
          (is (<= @tool-calls 2))
          (is (<= @model-calls 3))
          (is (map? step))
          (is (contains? step :outputs))
          (is (contains? step :last-event)))))))

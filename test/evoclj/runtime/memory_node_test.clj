(ns evoclj.runtime.memory-node-test
  "Feature R1 e2e: a topology :memory/write -> :memory/read -> :emit
  runs through the real scheduler, dispatching :intent/memory-write
  and :intent/memory-read through the broker to the :memory/kv
  provider, so the read returns exactly what the write stored."
  (:require [clojure.edn :as edn]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [evoclj.compiler.core :as core]
            [evoclj.genome.load :as load]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.memory :as mem]
            [evoclj.provider.registry :as registry]
            [evoclj.runtime.phenotype :as phenotype]
            [evoclj.runtime.scheduler :as scheduler]
            [evoclj.store.artifact :as artifact]
            [evoclj.store.cas :as cas]
            [evoclj.store.event :as event]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.session :as session]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file FileVisitOption Files LinkOption Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))

(def ^:private generation-id "generation-1")

(defn- temp-db-path []
  (str (Files/createTempFile "evoclj-mem-node-" ".db" (make-array FileAttribute 0))))

(defn- temp-cas-dir []
  (str (Files/createTempDirectory "evoclj-mem-node-cas-" (make-array FileAttribute 0))))

(defn- delete-tree! [path]
  (when (Files/exists path (make-array LinkOption 0))
    (with-open [stream (Files/walk path (make-array FileVisitOption 0))]
      (doseq [p (reverse (iterator-seq (.iterator stream)))]
        (Files/deleteIfExists p)))))

(def ^:private temp-paths (atom []))
(defn- track! [p] (swap! temp-paths conj p) p)
(use-fixtures :each (fn [f] (reset! temp-paths []) (f) (doseq [p @temp-paths] (delete-tree! (Paths/get p (make-array String 0))))))

(def ^:private provider-catalog
  {:reasoning/high {:provider :fixture
                    :provider-model "fixture-model-v1"
                    :adapter-version "1"}})

(defn- genome-root
  "A genome whose topology is :memory/write -> :memory/read -> :emit.
  The write node stores the task input payload under :note; the read
  node reads it back; the emit node completes with the read value."
  []
  (let [dir (str (Files/createTempDirectory "evoclj-mem-genome-" (make-array FileAttribute 0)))]
    (track! dir)
    (spit (str dir "/manifest.edn")
          "{:genome/format 1 :agent/id :mem :agent/entry :graph/mem\n
           :abi {:kernel 1 :genome 1 :intent 1 :tool 1}\n
           :modules {:topology \"topology.edn\" :models \"models.edn\"\n
                     :memory \"memory.edn\" :evolution \"evolution.edn\"}\n
           :capabilities/requested #{:memory/write :memory/read}\n
           :evolution {:max-risk :behavioral :mutable #{:parameters}}\n
           :metadata {:name \"memory-fixture\"}}")
    (spit (str dir "/topology.edn")
          "{:graph/id :graph/mem :entry :node/write\n
           :nodes {:node/write {:node/type :memory/write :memory :note :next :node/read}\n
                   :node/read {:node/type :memory/read :memory :note :next :node/emit}\n
                   :node/emit {:node/type :emit}}\n
           :limits {:max-steps 16}}")
    (spit (str dir "/models.edn") "{:models {:planner {:alias :reasoning/high}}}")
    (spit (str dir "/memory.edn") "{:memory/seed {}}")
    (spit (str dir "/evolution.edn") "{:evolution/enabled false}")
    dir))

(deftest memory-nodes-run-end-to-end
  (testing "a :memory/write node stores the task payload and the
            :memory/read node retrieves it, completing the session"
    (let [root (genome-root)
          loaded (load/load-genome (Paths/get root (make-array String 0)))
          compiled (core/compile-genome loaded provider-catalog)
          genome-id (:compiled/genome-id compiled)
          resolution-id (:compiled/resolution-id compiled)
          phenotype-id (:compiled/phenotype-id compiled)
          db-path (temp-db-path)
          _ (track! db-path)
          db (sqlite/spec db-path)
          _ (migrate/migrate! db)
          _ (do
              (doseq [[artifact-id media-type]
                      [[genome-id "application/octet-stream"]
                       [resolution-id "application/edn"]
                       [phenotype-id "application/edn"]]]
                (artifact/ensure-artifact! db artifact-id media-type 0))
              (artifact/ensure-genome! db genome-id))
          _ (sqlite/with-db [conn db]
              (jdbc/insert! conn :generations
                            {:id generation-id
                             :genome_id genome-id
                             :resolution_id resolution-id
                             :parent_id nil
                             :state "active"
                             :current 1
                             :created_at "2025-01-01T00:00:00Z"}))
          cas-root (temp-cas-dir)
          _ (track! cas-root)
          cas-store (cas/->cas cas-root)
          reg (registry/create-registry)
          _ (registry/register! reg (mem/memory-provider {:store db}))
          now (Date.)
          memory-lease {:cap/id (random-uuid)
                        :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id phenotype-id}
                        :resource {:kind :memory :id :note}
                        :actions #{:invoke}
                        :constraints {:max-calls 100}
                        :issued-at now
                        :expires-at (Date. (+ (.getTime now) 60000))}
          ph (phenotype/instantiate
              compiled
              {:stores {:sqlite :poison :cas {:root :poison}}
               :providers {:registry reg}
               :capabilities {:leases [memory-lease] :usage (atom {})}
               :program-sources {}})
          executor {:phenotype ph
                    :stores {:sqlite db :cas cas-store}
                    :dispatch (dispatch/make-broker-context
                               {:registry reg
                                :leases [memory-lease]
                                :usage (atom {})})}
          sid (:session/id
               (session/create-session!
                db
                {:genome/id genome-id
                 :resolution/id resolution-id
                 :phenotype/id phenotype-id
                 :generation/id generation-id}))
          _ (event/append-event! db
                                 {:session/id sid
                                  :generation/id generation-id
                                  :phenotype/id phenotype-id
                                  :event/type :session/created
                                  :cause/event-id nil
                                  :payload-ref nil
                                  :metadata {}})
          result (scheduler/run-session! executor sid {:op :remember :text "hello memory"})
          outputs (when (:output-ref result)
                    (edn/read-string
                     (String. ^bytes (cas/get-bytes cas-store (:output-ref result))
                                    StandardCharsets/UTF_8)))
          events (event/events-for-session db sid)]
      (is (= :completed (:status result)))
      ;; the read node's provider value is the last output before emit
      (is (some #(and (map? %)
                      (= :note (:memory/key %))
                      (true? (:memory/found %))
                      (= "hello memory" (get-in % [:memory/content :text])))
                  outputs))
      (is (>= (count (filter #(= :intent/authorized (:event/type %)) events)) 2))
      (is (= 2 (count (filter #(= :provider/call-completed (:event/type %)) events)))))))

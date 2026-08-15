(ns evoclj.intent.memory-dispatch-test
  "Feature R1: :intent/memory-read and :intent/memory-write dispatch
  through the broker pipeline to the :memory/kv provider — with a
  :memory lease they persist; without a lease they are denied before
  the provider runs (execution counter untouched)."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.intent.dispatch :as dispatch]
            [evoclj.provider.memory :as mem]
            [evoclj.provider.registry :as registry]
            [evoclj.store.migrate :as migrate]
            [evoclj.store.sqlite :as sqlite])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.util Date UUID)))

(defn- temp-db []
  (let [p (str (Files/createTempFile "evoclj-mem-dispatch-" ".db" (make-array FileAttribute 0)))
        db (sqlite/spec p)]
    (migrate/migrate! db)
    db))

(defn- phenotype-id []
  "sha256:0000000000000000000000000000000000000000000000000000000000000000")

(defn- lease [phen-id]
  (let [now (Date.)]
    {:cap/id (UUID/randomUUID)
     :subject {:phenotype/id phen-id}
     :resource {:kind :memory :id :note}
     :actions #{:invoke}
     :constraints {:max-calls 100}
     :issued-at now
     :expires-at (Date. (+ (.getTime now) 60000))}))

(defn- make-intent [type session payload]
  {:intent/id (random-uuid)
   :intent/type type
   :session/id session
   :phenotype/id (phenotype-id)
   :node/id :node/mem
   :cause/event-id 1
   :payload payload
   :budget {:wall-ms 1000 :max-steps 10}
   :metadata {}})

(defn- broker-context [reg provider lease]
  (let [r (registry/create-registry)]
    (when provider (registry/register! r provider))
    (dispatch/make-broker-context
     {:registry r
      :leases (if lease [lease] [])
      :usage (atom {})})))

(deftest memory-dispatch-persists-with-lease
  (testing "a memory-write then a memory-read dispatch through the
            broker succeed with a :memory lease"
    (let [db (temp-db)
          s (UUID/fromString "00000000-0000-0000-0000-0000000000dd")
          c (atom 0)
          ctx (broker-context nil (mem/memory-provider {:store db :execution-count c})
                              (lease (phenotype-id)))
          w (dispatch/dispatch! ctx (make-intent :intent/memory-write s
                                                 {:memory/key :note :memory/content {:v 1}}))
          r (dispatch/dispatch! ctx (make-intent :intent/memory-read s
                                                 {:memory/key :note}))]
      (is (= :ok (:result/status w)))
      (is (= {:memory/key :note :memory/written true} (:value w)))
      (is (= :ok (:result/status r)))
      (is (true? (get-in r [:value :memory/found])))
      (is (= {:v 1} (get-in r [:value :memory/content])))
      (is (= 2 @c) "the provider really ran twice"))))

(deftest memory-dispatch-denied-without-lease
  (testing "without a :memory lease the broker denies BEFORE the
            provider executes (execution counter untouched)"
    (let [db (temp-db)
          s (UUID/fromString "00000000-0000-0000-0000-0000000000dd")
          c (atom 0)
          ctx (broker-context nil (mem/memory-provider {:store db :execution-count c})
                              nil)
          w (dispatch/dispatch! ctx (make-intent :intent/memory-write s
                                                 {:memory/key :note :memory/content 1}))]
      (is (= :error (:result/status w)))
      (is (= :capability/denied (:error/type w)))
      (is (= 0 @c) "a denied request never reaches the provider"))))

(deftest memory-dispatch-unknown-provider
  (testing "without the :memory/kv provider registered the dispatch
            reports :provider/not-found"
    (let [db (temp-db)
          s (UUID/fromString "00000000-0000-0000-0000-0000000000dd")
          ctx (broker-context nil nil nil)
          w (dispatch/dispatch! ctx (make-intent :intent/memory-write s
                                                 {:memory/key :note :memory/content 1}))]
      (is (= :error (:result/status w)))
      (is (= :provider/not-found (:error/type w))))))

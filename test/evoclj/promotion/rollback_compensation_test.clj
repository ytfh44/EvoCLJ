(ns evoclj.promotion.rollback-compensation-test
  "Tests for evoclj.promotion.rollback-compensation/compensation-manifest."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.promotion.rollback-compensation :as rc]))

(def ^:const gen "generation-7")

(defn- ev
  "Minimal event constructor: type, generation, and a few identifying
  fields. Missing fields stay absent so we also exercise fail-soft nil
  filling."
  [type generation-id & {:keys [session-id phenotype-id intent-id timestamp]
                         :or   {session-id   (java.util.UUID/randomUUID)
                                phenotype-id "ART-1"
                                intent-id    "INT-1"
                                timestamp    "2024-01-01T00:00:00Z"}}]
  (cond-> {:event/type   type
           :generation/id generation-id}
    true        (assoc :session/id session-id)
    true        (assoc :phenotype/id phenotype-id)
    true        (assoc :intent/id intent-id)
    timestamp   (assoc :timestamp timestamp)))

(deftest no-matching-events-returns-empty
  "When no event matches the generation (or there are no events at all),
  the manifest reports an empty effects list and a count of 0."
  (testing "empty event vector"
    (let [m (rc/compensation-manifest gen [])]
      (is (= gen (:generation/id m)))
      (is (= [] (:effects m)))
      (is (= 0 (:count m)))))
  (testing "events present but none match this generation's external effects"
    (let [events [(ev :promotion/rollback "generation-7")
                  (ev :session/created "generation-6")
                  (ev :intent/invoked "generation-8")]
          m (rc/compensation-manifest gen events)]
      (is (= [] (:effects m)))
      (is (= 0 (:count m))))))

(deftest matching-events-listed-correctly
  "External-effect events belonging to the target generation are listed
  with their identifying fields, in the order supplied."
  (let [e1 (ev :intent/invoked gen :intent-id "INT-1" :timestamp "t1")
        e2 (ev :memory/write gen :intent-id "INT-2" :timestamp "t2")
        e3 (ev :effect/emitted gen :intent-id "INT-3" :timestamp "t3")
        m (rc/compensation-manifest gen [e1 e2 e3])]
    (is (= gen (:generation/id m)))
    (is (= 3 (:count m)))
    (is (= [{:event/type   :intent/invoked
             :session/id   (:session/id e1)
             :phenotype/id "ART-1"
             :intent/id    "INT-1"
             :timestamp    "t1"}
            {:event/type   :memory/write
             :session/id   (:session/id e2)
             :phenotype/id "ART-1"
             :intent/id    "INT-2"
             :timestamp    "t2"}
            {:event/type   :effect/emitted
             :session/id   (:session/id e3)
             :phenotype/id "ART-1"
             :intent/id    "INT-3"
             :timestamp    "t3"}]
           (:effects m)))))

(deftest non-matching-generation-excluded
  "Events of the right external-effect type but a DIFFERENT generation are
  excluded, while same-generation ones are kept. Also confirms
  fail-soft nil filling when fields are absent."
  (let [keep-1 (ev :intent/invoked gen :intent-id "K1")
        drop-1 (ev :intent/invoked "generation-6" :intent-id "D1")
        drop-2 (ev :effect/emitted "generation-8" :intent-id "D2")
        keep-2 (dissoc (ev :memory/write gen :intent-id "K2") :session/id :timestamp)
        m (rc/compensation-manifest gen [keep-1 drop-1 keep-2 drop-2])
        expected-1 {:event/type   :intent/invoked
                    :session/id   (:session/id keep-1)
                    :phenotype/id "ART-1"
                    :intent/id    "K1"
                    :timestamp    "2024-01-01T00:00:00Z"}
        expected-2 {:event/type   :memory/write
                    :session/id   nil
                    :phenotype/id "ART-1"
                    :intent/id    "K2"
                    :timestamp    nil}]
    (is (= 2 (:count m)))
    (is (= [expected-1 expected-2] (:effects m)))
    (testing "missing :session/id and :timestamp filled with nil (timestamp falls back to :created-at, also absent)"
      (let [last-effect (second (:effects m))]
        (is (nil? (:session/id last-effect)))
        (is (nil? (:timestamp last-effect)))
        (is (= "ART-1" (:phenotype/id last-effect)))
        (is (= "K2" (:intent/id last-effect)))))))

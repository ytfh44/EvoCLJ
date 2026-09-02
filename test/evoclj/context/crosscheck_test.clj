(ns evoclj.context.crosscheck-test
  (:require [clojure.test :as t]
            [evoclj.context.compression.crosscheck :as cc]
            [evoclj.context.compression.envelope :as envelope]
            [evoclj.context.compression.error :as err]))

(defn- base-envelope [task subgoals]
  {:envelope/version 1
   :envelope/created-at "2026-08-17T00:00:00Z"
   :envelope/window {:window/from 0 :window/to 10}
   :envelope/tokens-before 5000
   :envelope/tokens-after 300
   :envelope/compressor {:compressor/model "test-model"
                         :compressor/prompt "compress"}
   :envelope/task task
   :envelope/subgoals subgoals
   :envelope/residue []
   :envelope/evidence []})

(defn- todo [tasks subgoals]
  {:tasks tasks :subgoals subgoals})

(defn- task [id status description]
  {:task/id id :task/status status :task/description description})

(defn- subgoal [id status description parent]
  {:subgoal/id id :subgoal/status status :subgoal/description description :subgoal/parent parent})

(t/deftest crosscheck-valid-when-fields-agree
  (let [t (task "t1" :pending "do the thing")
        sg (subgoal "sg1" :pending "step one" "t1")
        envelope (base-envelope t [sg])
        source (todo [t] [sg])
        result (cc/crosscheck* envelope source)]
    (t/is (true? (:crosscheck/valid? result)))
    (t/is (= t (:envelope/task (:crosscheck/envelope result))))
    (t/is (= [sg] (:envelope/subgoals (:crosscheck/envelope result))))))

(t/deftest crosscheck-auto-corrects-stale-task-status
  (let [t-envelope (task "t1" :pending "do the thing")
        t-todo (task "t1" :in-progress "do the thing")
        envelope (base-envelope t-envelope [])
        source (todo [t-todo] [])
        result (cc/crosscheck* envelope source)]
    (t/is (true? (:crosscheck/valid? result)))
    (t/is (= :in-progress (:task/status (:envelope/task (:crosscheck/envelope result)))))))

(t/deftest crosscheck-auto-corrects-stale-subgoal-status
  (let [sg-envelope (subgoal "sg1" :pending "step one" "t1")
        sg-todo (subgoal "sg1" :completed "step one" "t1")
        envelope (base-envelope (task "t1" :pending "parent") [sg-envelope])
        source (todo [(task "t1" :in-progress "parent")] [sg-todo])
        result (cc/crosscheck* envelope source)]
    (t/is (true? (:crosscheck/valid? result)))
    (t/is (= :completed (:subgoal/status (first (:envelope/subgoals (:crosscheck/envelope result))))))))

(t/deftest crosscheck-mismatches-when-task-unknown-to-todo
  (let [t-envelope (task "t1" :pending "do the thing")
        envelope (base-envelope t-envelope [])
        source (todo [] [])
        result (cc/crosscheck* envelope source)]
    (t/is (false? (:crosscheck/valid? result)))
    (t/is (= 1 (count (:crosscheck/mismatches result))))
    (t/is (= :task (:crosscheck/kind (first (:crosscheck/mismatches result)))))
    (t/is (= "t1" (:crosscheck/id (first (:crosscheck/mismatches result)))))))

(t/deftest crosscheck-mismatches-when-subgoal-unknown-to-todo
  (let [sg-envelope (subgoal "sg1" :pending "step one" "t1")
        envelope (base-envelope (task "t1" :pending "parent") [sg-envelope])
        source (todo [(task "t1" :pending "parent")] [])
        result (cc/crosscheck* envelope source)]
    (t/is (false? (:crosscheck/valid? result)))
    (t/is (= 1 (count (:crosscheck/mismatches result))))
    (t/is (= :subgoal (:crosscheck/kind (first (:crosscheck/mismatches result)))))))

(t/deftest crosscheck-mismatches-on-non-auto-correctable-field
  (let [t-envelope {:task/id "t1" :task/status :pending :task/description "WRONG"}
        t-todo (task "t1" :pending "do the thing")
        envelope (base-envelope t-envelope [])
        source (todo [t-todo] [])
        result (cc/crosscheck* envelope source)]
    (t/is (false? (:crosscheck/valid? result)))
    (t/is (= 1 (count (:crosscheck/mismatches result))))
    (t/is (= :task/description (:crosscheck/field (first (:crosscheck/mismatches result)))))))

(t/deftest crosscheck-throws-on-malformed-envelope
  (let [bad {:envelope/version 1}
        source (todo [] [])]
    (try
      (cc/crosscheck* bad source)
      (t/is false "should have thrown")
      (catch Exception e
        (t/is (= :context/compression-invalid (:error/type (ex-data e))))))))

(t/deftest crosscheck-throws-on-non-map-todo
  (let [t (task "t1" :pending "do the thing")
        envelope (base-envelope t [])]
    (try
      (cc/crosscheck* envelope "not a map")
      (t/is false "should have thrown")
      (catch Exception e
        (t/is (= :context/crosscheck-mismatch (:error/type (ex-data e))))))))

(t/deftest crosscheck-valid?-wrapper
  (let [t (task "t1" :pending "do the thing")
        envelope (base-envelope t [])
        source (todo [t] [])]
    (t/is (true? (cc/crosscheck-valid? envelope source)))))

(t/deftest crosscheck-mismatches-wrapper
  (let [t-envelope (task "t1" :pending "do the thing")
        envelope (base-envelope t-envelope [])
        source (todo [] [])]
    (t/is (= 1 (count (cc/crosscheck-mismatches envelope source))))))

(t/deftest crosscheck-does-not-touch-residue
  (let [t (task "t1" :pending "do the thing")
        residue [{:residue/id 1 :residue/kind :constraint
                  :residue/text "a constraint" :residue/source "user"
                  :residue/at "2026-08-17T00:00:00Z"}]
        envelope (assoc (base-envelope t []) :envelope/residue residue)
        source (todo [t] [])
        result (cc/crosscheck* envelope source)]
    (t/is (= residue (:envelope/residue (:crosscheck/envelope result))))))

(t/run-tests)
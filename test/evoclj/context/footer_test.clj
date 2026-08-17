(ns evoclj.context.footer-test
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [evoclj.context.envelope :as envelope]
            [evoclj.context.footer :as footer]))

;; ---------------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------------

(defn sample-envelope []
  (envelope/make-envelope
    {:task {:task/id "t1" :task/status :in-progress :task/description "do X"}
     :subgoals [{:subgoal/id "sg1" :subgoal/status :completed
                 :subgoal/description "step 1" :subgoal/parent "t1"}
                {:subgoal/id "sg2" :subgoal/status :in-progress
                 :subgoal/description "step 2" :subgoal/parent "t1"}]
     :residue [{:residue/id 1 :residue/kind :constraint
                :residue/text "must use API v2" :residue/source "user"
                :residue/at "2026-08-17T00:00:00Z"}]
     :evidence [{:evidence/id 1 :evidence/kind :observation
                 :evidence/text "saw 500 errors" :evidence/at "2026-08-17T00:00:00Z"}]
     :version 1
     :created-at "2026-08-17T00:00:00Z"
     :window {:window/from 0 :window/to 10}
     :tokens-before 5000
     :tokens-after 300
     :compressor {:compressor/model "test"
                  :compressor/prompt "p"}}))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(t/deftest build-footer-includes-task
  (let [f (footer/build-footer (sample-envelope))]
    (t/is (str/includes? f "t1"))
    (t/is (str/includes? f "do X"))))

(t/deftest build-footer-includes-subgoals
  (let [f (footer/build-footer (sample-envelope))]
    (t/is (str/includes? f "sg1"))
    (t/is (str/includes? f "step 1"))
    (t/is (str/includes? f "sg2"))
    (t/is (str/includes? f "step 2"))))

(t/deftest build-footer-includes-residue
  (let [f (footer/build-footer (sample-envelope))]
    (t/is (str/includes? f "must use API v2"))
    (t/is (str/includes? f "[:constraint]"))))

(t/deftest build-footer-includes-evidence
  (let [f (footer/build-footer (sample-envelope))]
    (t/is (str/includes? f "saw 500 errors"))))

(t/deftest build-footer-includes-archiver-reports
  (let [env (sample-envelope)
        reports [{:archiver/id :test/todo
                  :archiver/description "Todo tracker"
                  :archiver/serialized {:tasks 3 :completed 2}}]
        f (footer/build-footer env {:archiver-reports reports})]
    (t/is (str/includes? f "Todo tracker"))
    (t/is (str/includes? f "COMPRESSION"))))

(t/deftest build-footer-without-archivers-has-no-tool-section
  (let [f (footer/build-footer (sample-envelope))]
    (t/is (not (str/includes? f "TOOL ARCHIVES")))))

(t/deftest build-footer-with-no-residue
  (let [env (envelope/make-envelope
              {:task {:task/id "t1" :task/status :pending :task/description "t"}
               :subgoals []
               :residue []
               :evidence []
               :version 1
               :created-at "2026-08-17T00:00:00Z"
               :window {:window/from 0 :window/to 10}
               :tokens-before 100
               :tokens-after 50
               :compressor {:compressor/model "m" :compressor/prompt "p"}})
        f (footer/build-footer env)]
    (t/is (str/includes? f "(none)"))))

(t/run-tests)

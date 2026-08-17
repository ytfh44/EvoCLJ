(ns evoclj.context.cli-test
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [evoclj.context.cli :as cli]
            [evoclj.context.envelope :as envelope]))

;; ---------------------------------------------------------------------------
;; print-envelope
;; ---------------------------------------------------------------------------

(t/deftest print-envelope-shows-task-info
  (let [env (envelope/make-envelope
             {:task {:task/id "t1" :task/status :completed :task/description "done"}
              :subgoals []
              :residue []
              :evidence []
              :version 1
              :created-at "2026-08-17T00:00:00Z"
              :window {:window/from 0 :window/to 10}
              :tokens-before 5000
              :tokens-after 300
              :compressor {:compressor/model "test-model"
                           :compressor/prompt "compress"}})
        out (with-out-str (cli/print-envelope env))]
    (t/is (str/includes? out "t1"))
    (t/is (str/includes? out "completed"))
    (t/is (str/includes? out "test-model"))))

(t/deftest print-envelope-shows-subgoals
  (let [env (envelope/make-envelope
             {:task {:task/id "t1" :task/status :in-progress :task/description "doing"}
              :subgoals [{:subgoal/id "sg1" :subgoal/status :completed
                          :subgoal/description "step" :subgoal/parent "t1"}]
              :residue []
              :evidence []
              :version 1
              :created-at "2026-08-17T00:00:00Z"
              :window {:window/from 0 :window/to 10}
              :tokens-before 5000
              :tokens-after 300
              :compressor {:compressor/model "test"
                           :compressor/prompt "p"}})
        out (with-out-str (cli/print-envelope env))]
    (t/is (str/includes? out "sg1"))
    (t/is (str/includes? out "step"))))

(t/deftest print-envelope-shows-residue-and-evidence
  (let [env (envelope/make-envelope
             {:task {:task/id "t1" :task/status :pending :task/description "task"}
              :subgoals []
              :residue [{:residue/id 1 :residue/kind :constraint
                         :residue/text "must X" :residue/source "user"
                         :residue/at "2026-08-17T00:00:00Z"}]
              :evidence [{:evidence/id 1 :evidence/kind :observation
                          :evidence/text "saw it" :evidence/at "2026-08-17T00:00:00Z"}]
              :version 1
              :created-at "2026-08-17T00:00:00Z"
              :window {:window/from 0 :window/to 10}
              :tokens-before 5000
              :tokens-after 300
              :compressor {:compressor/model "test"
                           :compressor/prompt "p"}})
        out (with-out-str (cli/print-envelope env))]
    (t/is (str/includes? out "must X"))
    (t/is (str/includes? out "saw it"))))

(t/deftest print-envelope-skips-empty-sections
  (let [env (envelope/make-envelope
             {:task {:task/id "t1" :task/status :pending :task/description "task"}
              :subgoals []
              :residue []
              :evidence []
              :version 1
              :created-at "2026-08-17T00:00:00Z"
              :window {:window/from 0 :window/to 10}
              :tokens-before 5000
              :tokens-after 300
              :compressor {:compressor/model "test"
                           :compressor/prompt "p"}})
        out (with-out-str (cli/print-envelope env))]
    (t/is (not (str/includes? out "--- Subgoals ---")))
    (t/is (not (str/includes? out "--- Residue ---")))
    (t/is (not (str/includes? out "--- Evidence ---")))))

;; ---------------------------------------------------------------------------
;; -main dispatch
;; ---------------------------------------------------------------------------

(t/deftest main-prints-usage-on-unknown-command
  (let [out (with-out-str (cli/-main "unknown"))]
    (t/is (str/includes? out "Usage:"))))

(t/deftest main-prints-usage-on-no-args
  (let [out (with-out-str (cli/-main))]
    (t/is (str/includes? out "Usage:"))))

(t/run-tests)
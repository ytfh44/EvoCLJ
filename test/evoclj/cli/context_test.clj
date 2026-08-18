(ns evoclj.cli.context-test
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.cli.main :as main]))

;; ---------------------------------------------------------------------------
;; The context commands are registered in the main CLI command table.
;; ---------------------------------------------------------------------------

(deftest context-commands-are-registered-in-main-cli
  (let [cmds @#'main/commands]
    (is (contains? cmds ["context" "compress"]))
    (is (contains? cmds ["context" "recompress"]))
    (is (contains? cmds ["context" "loop"]))
    (is (contains? cmds ["context" "inspect"]))))

(deftest unknown-context-subcommand-exits-typed
  (let [{:keys [exit data]} (main/execute ["context" "foo"] {})]
    (is (= 1 exit))
    (is (= :cli/unknown-command (:error/type data)))))

(deftest bare-context-command-exits-typed
  (let [{:keys [exit data]} (main/execute ["context"] {})]
    (is (= 1 exit))
    (is (= :cli/unknown-command (:error/type data)))))

(deftest context-compress-without-args-exits-usage-invalid
  (let [{:keys [exit data]} (main/execute ["context" "compress"] {})]
    (is (= 1 exit))
    (is (= :cli/usage-invalid (:error/type data)))))

(deftest context-recompress-without-args-exits-usage-invalid
  (let [{:keys [exit data]} (main/execute ["context" "recompress"] {})]
    (is (= 1 exit))
    (is (= :cli/usage-invalid (:error/type data)))))

(deftest context-loop-without-args-exits-usage-invalid
  (let [{:keys [exit data]} (main/execute ["context" "loop"] {})]
    (is (= 1 exit))
    (is (= :cli/usage-invalid (:error/type data)))))

(deftest context-inspect-without-args-exits-usage-invalid
  (let [{:keys [exit data]} (main/execute ["context" "inspect"] {})]
    (is (= 1 exit))
    (is (= :cli/usage-invalid (:error/type data)))))

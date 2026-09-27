(ns evoclj.cli.context-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
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

;; ---------------------------------------------------------------------------
;; Failures must keep their CAUSE. The context CLI used to exit through a
;; one-argument *exit* called from INSIDE the try block, so each handler's
;; catch swallowed it and the main CLI saw :cli/usage-invalid with a nil
;; message for every failure — a missing file, an unreadable path and a
;; genuine usage error were indistinguishable.
;; ---------------------------------------------------------------------------

(defn- missing-path []
  (str "no-such-context-" (System/nanoTime) ".edn"))

(deftest a-missing-input-file-is-not-reported-as-a-usage-error
  (doseq [sub ["compress" "recompress" "loop" "inspect"]]
    (let [{:keys [exit data]}
          (main/execute ["context" sub "-i" (missing-path) "-o" (missing-path)] {})]
      (is (= 1 exit) (str sub " exits non-zero"))
      (is (not= :cli/usage-invalid (:error/type data))
          (str sub ": an unreadable input is an I/O failure, not a usage error"))
      (is (= :context/input-unreadable (:error/type data))
          (str sub ": the real cause survives to the caller"))
      (is (string? (:message data))
          (str sub ": the message is carried, not nil"))
      (is (str/includes? (str (:message data)) "cannot read input file")
          (str sub ": the message names the actual failure")))))

(deftest an-empty-input-file-reports-its-own-type
  (let [empty (str (java.io.File/createTempFile "evoclj-empty-" ".edn"))
        _ (spit empty "")
        {:keys [exit data]}
        (main/execute ["context" "inspect" "-i" empty] {})]
    (.delete (java.io.File. empty))
    (is (= 1 exit))
    (is (= :context/input-empty (:error/type data))
        "an empty input is distinct from a missing one and from a usage error")
    (is (string? (:message data)))))

(deftest an-unparseable-envelope-reports-its-own-type
  (let [f (str (java.io.File/createTempFile "evoclj-bad-env-" ".edn"))]
    (spit f "this is not an envelope")
    (let [{:keys [exit data]} (main/execute ["context" "inspect" "-i" f] {})]
      (.delete (java.io.File. f))
      (is (= 1 exit))
      (is (= :context/envelope-invalid (:error/type data)))
      (is (string? (:message data))))))

(deftest missing-required-options-are-still-usage-errors
  ;; unchanged public contract: a missing flag is genuinely a usage error
  (let [{:keys [exit data]} (main/execute ["context" "compress"] {})]
    (is (= 1 exit))
    (is (= :cli/usage-invalid (:error/type data)))))

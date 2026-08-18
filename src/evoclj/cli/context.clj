(ns evoclj.cli.context
  "Main-CLI wrappers for the context compression subsystem.

  Each wrapper delegates to the context CLI and translates its exit
  exceptions into typed :cli/* errors the main CLI can handle uniformly."
  (:require [evoclj.context.cli :as context-cli]
            [evoclj.kernel.error :as err]))

(defn compress!
  [opts]
  (try
    (context-cli/compress-command (:positionals opts))
    nil
    (catch clojure.lang.ExceptionInfo e
      (if-let [exit-code (:exit (ex-data e))]
        (throw (err/error :cli/usage-invalid
                           "context compression command failed"
                           {:exit exit-code
                            :message (:message (ex-data e))}))
        (throw e)))))

(defn recompress!
  [opts]
  (try
    (context-cli/recompress-command (:positionals opts))
    nil
    (catch clojure.lang.ExceptionInfo e
      (if-let [exit-code (:exit (ex-data e))]
        (throw (err/error :cli/usage-invalid
                           "context recompression command failed"
                           {:exit exit-code
                            :message (:message (ex-data e))}))
        (throw e)))))

(defn loop!
  [opts]
  (try
    (context-cli/loop-command (:positionals opts))
    nil
    (catch clojure.lang.ExceptionInfo e
      (if-let [exit-code (:exit (ex-data e))]
        (throw (err/error :cli/usage-invalid
                           "context loop command failed"
                           {:exit exit-code
                            :message (:message (ex-data e))}))
        (throw e)))))

(defn inspect!
  [opts]
  (try
    (context-cli/inspect-command (:positionals opts))
    nil
    (catch clojure.lang.ExceptionInfo e
      (if-let [exit-code (:exit (ex-data e))]
        (throw (err/error :cli/usage-invalid
                           "context inspect command failed"
                           {:exit exit-code
                            :message (:message (ex-data e))}))
        (throw e)))))

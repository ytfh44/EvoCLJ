(ns evoclj.cli.context
  "Main-CLI wrappers for the context compression subsystem.

  Each wrapper delegates to the context CLI and translates its exit
  exception into a typed error the main CLI can handle uniformly.

  The exit exception is re-thrown AS IS — not re-wrapped. Wrapping it was
  the bug: every wrapper hardcoded :error/type :cli/usage-invalid and read
  :message off the ExceptionInfo (which carries only its own literal
  \"context-cli-exit\" text, never the reason), so a missing input file, an
  unreadable path, and a genuine usage error were indistinguishable to the
  caller, and the real cause was nil. The context CLI now puts the cause
  IN the exit's ex-data — :error/type and :message are the subsystem's own
  classification — so the wrapper forwards them verbatim."
  (:require [evoclj.context.compression.cli :as context-cli]
            [evoclj.kernel.error :as err]))

(defn- exit->cli-error
  "Translate a context-CLI exit exception into a typed :cli/* error,
  preserving the exit code, the subsystem's :error/type and its message.
  Anything that is not an exit exception is re-thrown untouched."
  [e what]
  (let [d (ex-data e)]
    (if (contains? d :exit)
      (throw (err/error (or (:error/type d) :context/cli-failed)
                         (or (:message d) (str what " failed"))
                         (assoc d :exit (:exit d))))
      (throw e))))

(defn compress!
  [opts]
  (try
    (context-cli/compress-command (:positionals opts))
    nil
    (catch clojure.lang.ExceptionInfo e
      (exit->cli-error e "context compression command"))))

(defn recompress!
  [opts]
  (try
    (context-cli/recompress-command (:positionals opts))
    nil
    (catch clojure.lang.ExceptionInfo e
      (exit->cli-error e "context recompression command"))))

(defn loop!
  [opts]
  (try
    (context-cli/loop-command (:positionals opts))
    nil
    (catch clojure.lang.ExceptionInfo e
      (exit->cli-error e "context loop command"))))

(defn inspect!
  [opts]
  (try
    (context-cli/inspect-command (:positionals opts))
    nil
    (catch clojure.lang.ExceptionInfo e
      (exit->cli-error e "context inspect command"))))

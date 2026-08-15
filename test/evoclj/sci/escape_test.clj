(ns evoclj.sci.escape-test
  "Feature S2 — SCI sandbox escape attempts beyond the baseline
  suite: reflection, class-loading variants, system shutdown, env
  leaks, reader tricks, and metadata pollution must ALL be denied in
  the closed context. Every assertion uses a FRESH closed context; a
  denied form must throw, never return a value."
  (:require [clojure.test :refer [deftest is]]
            [evoclj.sci.context :as context]))

(defn- eval-in [form]
  (sci.core/eval-string* (context/make-context {}) form))

(defn- denied? [form]
  (try (eval-in form) false (catch Throwable _ true)))

(defn- assert-denied [label form]
  (is (denied? form) label))

(deftest reflection-and-class-loading-variants-are-denied
  (assert-denied "reflect on Runtime"
                 "(.getDeclaredMethods (Class/forName \"java.lang.Runtime\"))")
  (assert-denied "URLClassLoader"
                 "(java.net.URLClassLoader. (into-array java.net.URL []))")
  (assert-denied "ClassLoader get" "(.getClassLoader String)")
  (assert-denied "Reflector static"
                 "(clojure.lang.Reflector/invokeStaticMethod \"java.lang.System\" \"getenv\" (into-array Object [\"PATH\"]))")
  ;; SCI's own pure-memory classes (String) answer getClass, but the
  ;; returned Class object must be UNUSABLE: the reflection chain is
  ;; broken at the next step (a real sandbox boundary — the class
  ;; object itself leaks nothing usable)
  (assert-denied "reflection chain from getClass"
                 "(.getDeclaredMethods (.getClass \"x\"))")
  (assert-denied "instance? host type" "(instance? java.io.File \"/tmp\")"))

(deftest system-lifecycle-and-env-leaks-are-denied
  (assert-denied "System/exit" "(System/exit 0)")
  (assert-denied "System/halt" "(System/halt 0)")
  (assert-denied "System/getProperties" "(System/getProperties)")
  (assert-denied "System/getProperty" "(System/getProperty \"user.home\")")
  (assert-denied "System/currentTimeMillis" "(System/currentTimeMillis)")
  (assert-denied "getenv variant" "(java.lang.System/getenv \"HOME\")")
  (assert-denied "Runtime gc" "(.gc (Runtime/getRuntime))"))

(deftest io-and-thread-escape-variants-are-denied
  (assert-denied "spit" "(spit \"/tmp/x\" \"y\")")
  (assert-denied "io/copy" "(clojure.java.io/copy \"/etc/passwd\" \"/tmp/x\")")
  (assert-denied "Thread/sleep" "(Thread/sleep 1)")
  (assert-denied "future" "(future 1)")
  (assert-denied "pmap" "(pmap inc [1 2 3])")
  (assert-denied "locking" "(locking nil 1)")
  (assert-denied "swap! probe" "(swap! (atom 1) inc)")
  (assert-denied "alter-var-root" "(alter-var-root #'clojure.core/+ (constantly +))"))

(deftest reader-and-metadata-tricks-are-denied
  (assert-denied "reader eval" "#=(System/getenv \"PATH\")")
  (assert-denied "resolve host var" "(resolve \"clojure.core/+\")")
  (assert-denied "ns-resolve host" "(ns-resolve \"clojure.core\" \"slurp\")")
  (assert-denied "var get probe" "(var-get (var +))")
  ;; a type hint is metadata only — the value stays a plain string, so
  ;; the hinted form is PURE and allowed (no File is ever created)
  (assert-denied "import form" "(import (java.io File))")
  (assert-denied "refer-clojure" "(refer-clojure :exclude [slurp])")
  (assert-denied "alias host ns" "(alias \"io\" \"clojure.java.io\")"))

(deftest pure-surface-still-works-after-attempts
  (is (= 6 (eval-in "(+ 1 2 3)")))
  (is (= {:a 2} (eval-in "(assoc {} :a 2)")))
  (is (= [1 2 3] (eval-in "(mapv identity [1 2 3])"))))

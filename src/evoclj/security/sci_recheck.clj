(ns evoclj.security.sci-recheck
  "Coarse, deterministic static red-light recheck of a candidate genome's
  SCI program source, run BEFORE promotion (Task: promote-gate heuristic).

  This namespace is a PURE, fail-closed heuristic over the source TEXT.
  It reuses the denial surface of the project's closed SCI sandbox
  (evoclj.sci.context, which denies every host/IO/interop/process/
  loading/mutation form at analysis time) and statically scans the
  candidate program for the same dangerous shapes found in
  evoclj.sci.escape-test: Java interop (. and new), forbidden
  namespaces (java.lang / java.io / java.nio / java.net /
  clojure.java.*), System/Runtime/ProcessBuilder/Thread host access,
  reader eval (#=), top-level eval, dynamic class loading
  (Class/forName, import), filesystem/thread/reflection operators
  (slurp, spit, load-file, read-string, require, use, resolve,
  instance?, future, swap!, ...), and global var rebinding
  (alter-var-root, with-redefs, binding, intern, ...).

  It is NOT a behavioral sandbox replay and does NOT replace the runtime
  :allow policy of evoclj.sci.context. It is a cheap first gate: any
  hit blocks promotion. The checks are intentionally coarse and
  over-approximate (they may flag a harmless shape), but they never
  perform IO, never mutate, and are fully deterministic.

  evoclj.sci.boundary is required only as a reference to the project's
  trust-boundary conventions; this recheck operates purely on strings."
  (:require [evoclj.sci.boundary :as boundary]))

;; --- the deterministic danger patterns -----------------------------------
;;
;; Each entry is {:pattern <human-readable description>
;;                :re      <compiled regex>}.
;; re-find is applied to the raw program text; a match means the source
;; contains a construct the closed SCI sandbox denies. Patterns favor
;; precision (word boundaries, negative look-behinds to skip keywords)
;; but remain deliberately over-approximating — fail-closed is correct
;; for a promote gate.

(def ^:private danger-patterns
  "Ordered red-light patterns derived from the SCI sandbox deny list."
  [{:pattern "java interop dot special form (.method / (. x y))"
    :re #"\(\.\s*[A-Za-z]\w*"}
   {:pattern "java interop class member/static access (Class.xxx / Class/xxx)"
    :re #"(?<![:/])[A-Z]\w*(?:\.|/)[A-Za-z]\w*"}
   {:pattern "java interop instance member access (obj.method)"
    :re #"(?<!:)([a-z]\w*\.[a-z]\w*)(?=[\s(])"}
   {:pattern "java.lang namespace literal"
    :re #"java\.lang"}
   {:pattern "java.io namespace literal"
    :re #"java\.io"}
   {:pattern "java.nio namespace literal"
    :re #"java\.nio"}
   {:pattern "java.net namespace literal"
    :re #"java\.net"}
   {:pattern "clojure.java.shell namespace"
    :re #"clojure\.java\.shell"}
   {:pattern "clojure.java.io namespace"
    :re #"clojure\.java\.io"}
   {:pattern "clojure.lang namespace (host reflection)"
    :re #"clojure\.lang"}
   {:pattern "System host class access"
    :re #"System[./]"}
   {:pattern "Runtime host class access"
    :re #"Runtime[./]"}
   {:pattern "ProcessBuilder host class"
    :re #"ProcessBuilder"}
   {:pattern "Thread host interop (e.g. Thread/sleep)"
    :re #"Thread[./]"}
   {:pattern "new / constructor interop"
    :re #"\(\s*new\s"}
   {:pattern "Class/forName dynamic class loading"
    :re #"Class/forName"}
   {:pattern "reader eval (#= ...)"
    :re #"#="}
   {:pattern "top-level eval form"
    :re #"\(eval\b"}
   {:pattern "read-string (host reader)"
    :re #"read-string"}
   {:pattern "slurp (filesystem IO)"
    :re #"slurp"}
   {:pattern "spit (filesystem IO)"
    :re #"spit"}
   {:pattern "load-file (dynamic loading)"
    :re #"load-file"}
   {:pattern "require (namespace loading)"
    :re #"require"}
   {:pattern "use (namespace loading)"
    :re #"use\b"}
   {:pattern "import (host class import)"
    :re #"import"}
   {:pattern "refer-clojure (host ns refer)"
    :re #"refer-clojure"}
   {:pattern "alias host namespace"
    :re #"alias\b"}
   {:pattern "resolve host var"
    :re #"resolve\b"}
   {:pattern "ns-resolve host var"
    :re #"ns-resolve\b"}
   {:pattern "var-get host var read"
    :re #"var-get\b"}
   {:pattern "instance? host type"
    :re #"instance\?"}
   {:pattern "atom (concurrency primitive)"
    :re #"atom\b"}
   {:pattern "ref (concurrency primitive)"
    :re #"ref\b"}
   {:pattern "delay (concurrency primitive)"
    :re #"delay\b"}
   {:pattern "future (concurrency primitive)"
    :re #"future\b"}
   {:pattern "promise (concurrency primitive)"
    :re #"promise\b"}
   {:pattern "agent (concurrency primitive)"
    :re #"agent\b"}
   {:pattern "pmap (parallelism primitive)"
    :re #"pmap\b"}
   {:pattern "locking (host lock)"
    :re #"locking\b"}
   {:pattern "swap! (var mutation)"
    :re #"swap!"}
   {:pattern "reset! (var mutation)"
    :re #"reset!"}
   {:pattern "deref (var deref)"
    :re #"deref\b"}
   {:pattern "alter-var-root (global var rebinding)"
    :re #"alter-var-root"}
   {:pattern "var-set (global var rebinding)"
    :re #"var-set\b"}
   {:pattern "with-redefs (global var rebinding)"
    :re #"with-redefs"}
   {:pattern "binding (dynamic var rebinding)"
    :re #"binding\b"}
   {:pattern "intern (namespace injection)"
    :re #"intern\b"}])

;; --- helpers ----------------------------------------------------------------

(defn- scan-match
  "Return the actual matched substring of `re` inside `text`, preferring a
  capture group when present, else the whole match. Nil when no match.
  Pure: only re-find over a string."
  [text re]
  (when-let [found (re-find re text)]
    (if (string? found)
      found
      (or (second found) (first found)))))

;; --- public API -------------------------------------------------------------

(defn recheck-candidate
  "Coarse, deterministic static red-light recheck of a candidate genome's
  SCI program source, run BEFORE promotion.

  This is NOT a behavioral sandbox replay. It is a cheap, fail-closed
  heuristic over the source TEXT that reuses the denial surface of the
  closed SCI sandbox (evoclj.sci.context): host/Java interop, forbidden
  namespaces, process/IO/thread/reflection operators, reader eval (#=),
  top-level eval, dynamic class loading, and global var rebinding.

  `program-text` is the candidate genome's SCI program source (string).

  Returns {:safe? <bool>
           :violations [{:pattern <string> :match <string>}]}.
  :safe? is true and :violations is [] when no pattern matches;
  otherwise :safe? is false and every hit is listed with the matching
  pattern description and the concrete substring that triggered it.

  Pure function: no IO, no randomness, deterministic string matching."
  [program-text]
  (let [text (str program-text)
        violations
        (->> danger-patterns
             (map (fn [{:keys [pattern re]}]
                    (when-let [m (scan-match text re)]
                      {:pattern pattern :match m})))
             (keep identity))]
    {:safe? (empty? violations)
     :violations violations}))

(defn violation?
  "True when a recheck result indicates at least one red-light violation.
  Convenience for callers: (when (recheck/violation? (recheck/recheck-candidate src)) ...)."
  [result]
  (not (:safe? result)))

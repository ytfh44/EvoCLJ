(ns evoclj.store.binding-closure-test
  "WO-B2 closure guard — evoclj.store.binding must reach its
  environment/context/mount collaborators through top-level requires
  and direct function references. Reflective resolution
  (ns-resolve / requiring-resolve) inside this namespace is banned:
  the dependency edges were proven acyclic when B2 landed, so a
  runtime stringly lookup can only ever hide a broken rename from
  static analysis (INV-05).

  Mirrors the mcp.codec-closure precedent (M11): docstrings/comments may
  name the anti-pattern, so only an actual s-expression call form
  counts: (ns-resolve ... or (requiring-resolve ... ."
  (:require [clojure.test :refer [deftest is testing]]))

(def ^:private guarded-sources
  ["src/evoclj/store/binding.clj"])

(deftest no-requiring-resolve-call-sites-in-store-binding
  (testing "no ns-resolve / requiring-resolve CALL SITES remain in store.binding"
    (let [bad (for [p guarded-sources
                    :let [txt (slurp p)]
                    pat [#"\(ns-resolve '" #"\(requiring-resolve '"]
                    :when (re-find pat txt)]
                [p (str pat)])]
      (is (empty? bad)
          (str "reflective resolution call sites still present: " (pr-str bad))))))

(deftest collaborators-are-statically-required
  (testing "the collaborators B2 staticized appear as top-level :require entries"
    (let [txt (slurp "src/evoclj/store/binding.clj")]
      ;; The exact aliased requires whose direct references replaced the
      ;; reflective call sites; if one is removed, the corresponding
      ;; direct calls fail compilation, and this pin names the missing edge.
      (is (re-find #"\[evoclj\.context\.binding :as context-binding\]" txt)
          "evoclj.context.binding required statically")
      (is (re-find #"\[evoclj\.environment\.bundle :as env-bundle\]" txt)
          "evoclj.environment.bundle required statically")
      (is (re-find #"\[evoclj\.mount\.backend :as mount-backend\]" txt)
          "evoclj.mount.backend required statically")
      ;; And the direct references actually occur (not just dead aliases):
      (is (re-find #"env-bundle/get-bundle" txt) "direct registry reader reference")
      (is (re-find #"context-binding/activate!" txt) "direct context activation reference")
      (is (re-find #"mount-backend/cas-tree-backend" txt) "direct mount backend reference"))))

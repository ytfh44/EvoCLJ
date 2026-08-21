(ns evoclj.context.materializer
  "EffectiveContext = Materialize(History, ActiveBindings, CatalogProjection, HostPolicy)

  The materializer reads exact content for each active binding from its
  immutable artifact/tree via CAS — never from the current catalog
  projection. This guarantees pinning: if a binding was activated at
  revision A, and the catalog later moves to B, materialization still
  returns A.

  History is the compressed conversation history (string) produced by
  the compression subsystem (History -> compressed History). The
  materializer does NOT invoke compression; it only combines history
  with segments.

  CAS resolution is polymorphic:
   - if cas is a function (revision-id -> content string), call it
   - if cas is a map artifact-id -> content string, look up
   - if cas is a map with :root (CAS config or path string), use evoclj.store.cas/get-bytes
   - if cas is a string/Path/File (CAS root), use evoclj.store.cas/get-bytes
   - if cas is nil, try to look up :segment/content already on binding (for tests)

  HostPolicy filtering is applied before materialization."
  (:require [clojure.string :as str]
            [evoclj.context.segment :as segment]
            [evoclj.context.policy :as policy]
            [evoclj.kernel.error :as err])
  (:import (java.nio.charset StandardCharsets)))

;; ---------------------------------------------------------------------------
;; CAS fetch helper
;; ---------------------------------------------------------------------------

(defn- fetch-via-cas
  "Fetch content string for revision-id via cas resolver.
  Returns content string or throws if not found."
  [cas revision-id]
  (cond
    (nil? cas)
    (throw (err/error :context/materializer-missing-cas "CAS resolver required to fetch immutable artifact" {:revision-id revision-id}))

    (fn? cas)
    (let [c (cas revision-id)]
      (if (string? c) c
          (throw (err/error :context/materializer-missing "CAS function returned non-string or nil" {:revision-id revision-id :value c}))))

    (and (map? cas) (contains? cas :root))
    ;; CAS config map {:root ... :verify ...}
    (let [cas-ns (try (requiring-resolve 'evoclj.store.cas/get-bytes) (catch Exception _ nil))]
      (if cas-ns
        (let [ba (cas-ns cas revision-id)]
          (String. ^bytes ba StandardCharsets/UTF_8))
        (throw (err/error :context/materializer-missing-cas "evoclj.store.cas/get-bytes not available" {:revision-id revision-id}))))

    (and (map? cas) (contains? cas revision-id))
    ;; plain map artifact-id -> content
    (let [c (get cas revision-id)]
      (if (string? c) c
          (throw (err/error :context/materializer-missing (str "CAS map missing content for " revision-id) {:revision-id revision-id}))))

    (map? cas)
    ;; generic map that may contain artifact-id keys, try lookup
    (if-let [c (get cas revision-id)]
      (if (string? c) c
          (String. ^bytes c StandardCharsets/UTF_8))
      (throw (err/error :context/materializer-missing (str "CAS map missing content for " revision-id) {:revision-id revision-id})))

    (or (string? cas) (instance? java.io.File cas) (instance? java.nio.file.Path cas))
    (let [cas-ns (try (requiring-resolve 'evoclj.store.cas/get-bytes) (catch Exception _ nil))]
      (if cas-ns
        (let [ba (cas-ns cas revision-id)]
          (String. ^bytes ba StandardCharsets/UTF_8))
        (throw (err/error :context/materializer-missing-cas "evoclj.store.cas/get-bytes not available" {:revision-id revision-id}))))

    :else
    (throw (err/error :context/materializer-missing-cas "unsupported CAS resolver type" {:cas-type (type cas) :revision-id revision-id}))))

(defn- resolve-content
  "Resolve content for a binding via CAS. Tries CAS, then falls back to
  binding's embedded content for test convenience if CAS is nil and binding has :binding/content."
  [binding cas]
  (if cas
    (fetch-via-cas cas (:revision/id binding))
    ;; fallback: if binding carries inline content (test helper), use it
    (if-let [c (:binding/content binding)]
      c
      (fetch-via-cas cas (:revision/id binding)))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn materialize
  "Materialize EffectiveContext from history, active bindings, catalog, policy, and CAS.

  Args map:
    :history   — string, compressed history (from compression loop)
    :bindings  — collection of ContextBinding maps (active)
    :catalog   — CatalogProjection (map or fn), currently unused for content but validated
    :policy    — HostPolicy map or nil (allow all)
    :cas       — CAS resolver (function, map, or CAS root/path/config)

  Returns:
    {:effective/history string
     :effective/segments [Segment ...]   ; materialized, policy-filtered, CAS-resolved
     :effective/context-string string     ; segments + history combined
     :effective/bindings [...] }          ; the filtered bindings

  Each segment's content is fetched via CAS from binding's :revision/id,
  never from catalog's current revision."
  [{:keys [history bindings catalog policy cas]}]
  (when-not (or (nil? history) (string? history))
    (throw (err/error :context/materializer-invalid "history must be string or nil" {:history history})))
  (let [history (or history "")
        bindings (or bindings [])
        ;; policy filtering
        filtered (policy/filter-bindings policy bindings)
        ;; materialize each binding via CAS
        segments (mapv (fn [b]
                         (let [content (resolve-content b cas)]
                           (segment/segment-from-binding b content)))
                       filtered)
        ;; effective context string: segments injected before history
        ;; This keeps history compression isolated: compression never saw segments.
        context-str (if (seq segments)
                      (str (str/join "\n\n" (map :segment/content segments))
                           "\n\n--- HISTORY ---\n"
                           history)
                      history)]
    {:effective/history history
     :effective/segments segments
     :effective/context-string context-str
     :effective/bindings (vec filtered)
     :effective/catalog catalog}))

(defn effective-context
  "Alias for materialize, same signature.
  Kept for spec wording: EffectiveContext = Materialize(...)"
  [args]
  (materialize args))

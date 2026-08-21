(ns evoclj.cli.source
  "Generic source lifecycle CLI: source list/inspect/refresh, delegating to EnvironmentRegistry."
  (:require [clojure.string :as str]
            [evoclj.environment.registry :as reg]
            [evoclj.kernel.error :as err]))

;; --- registry resolution -----------------------------------------------------

(defn- registry-for
  "Resolve EnvironmentRegistry atom from opts. Injection priority:
   :registry > :environment/registry > :overrides/:environment/registry > :system/:environment/registry."
  [opts]
  (or (:registry opts)
      (:environment/registry opts)
      (get-in opts [:overrides :environment/registry])
      (get-in opts [:system :environment/registry])
      (try
        (let [sys (try (requiring-resolve 'evoclj.cli.session/build-system) (catch Exception _ nil))]
          (when sys
            (let [system ((resolve 'evoclj.cli.session/build-system) opts)]
              (:environment/registry system))))
        (catch Exception _ nil))
      nil))

(defn- coerce-source-id
  "Coerce CLI string id to keyword when appropriate. Accepts :foo/bar or foo/bar."
  [s]
  (cond
    (keyword? s) s
    (string? s) (let [t (str/trim s)]
                  (if (str/starts-with? t ":")
                    (keyword (subs t 1))
                    (keyword t)))
    :else s))

(defn- lookup-source-entry
  "Find source key in registry that matches sid-str (keyword or string forms)."
  [registry sid-str]
  (let [sources (:sources @registry)
        kw (try (coerce-source-id sid-str) (catch Exception _ nil))]
    (or (when kw (get sources kw))
        (get sources sid-str)
        (when kw (get sources (str kw)))
        ;; try string-keyed lookup fallback
        (some (fn [[k v]] (when (= (str k) (str sid-str)) v)) sources))))

(defn- source-summary
  "Summarize one registered source."
  [sid src registry]
  (let [st (reg/status registry)
        cur (reg/current registry)
        lg (reg/last-good registry)]
    {:source/id sid
     :source/type (str (.getName (class src)))
     :status (:status st)
     :dirty? (:dirty? st)
     :seq (:seq st)
     :current/revision-id (:revision/id cur)
     :last-good/revision-id (:revision/id lg)}))

;; --- commands ----------------------------------------------------------------

(defn list!
  "evoclj source list

  List all registered LiveSources. Delegates to EnvironmentRegistry.
  Returns {:sources [...] :count n}."
  [opts]
  (let [registry (registry-for opts)]
    (if (nil? registry)
      {:sources [] :count 0}
      (let [sources (:sources @registry)]
        {:sources (mapv (fn [[sid src]] (source-summary sid src registry)) sources)
         :count (count sources)}))))

(defn inspect!
  "evoclj source inspect <id>

  Inspect one source's registration, current revision, and status."
  [opts]
  (let [sid-str (first (:positionals opts))]
    (when-not sid-str
      (throw (err/error :cli/usage-invalid
                        "source inspect requires <id>"
                        {:usage "evoclj source inspect <id>"})))
    (let [registry (registry-for opts)]
      (when-not registry
        (throw (err/error :environment/no-source "no source registry available" {:source/id sid-str})))
      (let [src (lookup-source-entry registry sid-str)]
        (when-not src
          (throw (err/error :cli/source-not-found "no source with this id" {:source/id sid-str})))
        (let [sid (or (try (coerce-source-id sid-str) (catch Exception _ sid-str)) sid-str)
              actual-sid (or (some (fn [[k _]] (when (= (str k) (str sid)) k)) (:sources @registry)) sid)
              cur (reg/current registry)
              st (reg/status registry)]
          {:source/id actual-sid
           :source/type (str (.getName (class src)))
           :status (:status st)
           :dirty? (:dirty? st)
           :seq (:seq st)
           :current cur
           :last-good (reg/last-good registry)
           :revision/id (:revision/id cur)
           :payload (:payload cur)})))))

(defn refresh!
  "evoclj source refresh <id>
   evoclj source refresh --all

  Generic lifecycle refresh. Single id refreshes one source; --all refreshes every registered source.
  Delegates to EnvironmentRegistry/refresh!."
  [opts]
  (let [all? (boolean (get-in opts [:options :all]))
        registry (registry-for opts)]
    (when-not registry
      (throw (err/error :environment/no-source "no source registry available" {})))
    (let [sources (:sources @registry)]
      (when (empty? sources)
        (throw (err/error :environment/no-source "no source registered" {})))
      (if all?
        (let [ids (keys sources)
              results (mapv (fn [sid]
                              (try
                                (let [res (reg/refresh! registry sid)]
                                  (assoc res :source/id sid))
                                (catch clojure.lang.ExceptionInfo e
                                  {:source/id sid :status :error :error (ex-data e) :message (.getMessage e)})
                                (catch Exception e
                                  {:source/id sid :status :error :message (.getMessage e)})))
                            ids)]
          {:refreshed :all :count (count ids) :results results})
        (let [sid-str (first (:positionals opts))]
          (when-not sid-str
            (throw (err/error :cli/usage-invalid
                              "source refresh requires <id> or --all"
                              {:usage "evoclj source refresh <id> | evoclj source refresh --all"})))
          (let [kw (try (coerce-source-id sid-str) (catch Exception _ nil))
                actual-sid (or (when (contains? sources kw) kw)
                               (when (contains? sources sid-str) sid-str)
                               (some (fn [[k _]] (when (= (str k) (str sid-str)) k)) sources)
                               kw)
                _ (when-not (contains? sources actual-sid)
                    (throw (err/error :cli/source-not-found "no source with this id" {:source/id sid-str})))]
            (let [res (reg/refresh! registry actual-sid)]
              (assoc res :source/id actual-sid))))))))

;; --- generic aliases (required function names) -------------------------------

(defn source-list
  "Generic source list — alias for list!."
  [opts] (list! opts))

(defn source-inspect
  "Generic source inspect — alias for inspect!."
  [opts] (inspect! opts))

(defn source-refresh
  "Generic source refresh single — delegates to refresh! with one id."
  [opts] (refresh! opts))

(defn source-refresh-all
  "Generic source refresh all — delegates to refresh! with --all."
  [opts] (refresh! (assoc-in opts [:options :all] true)))

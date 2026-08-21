(ns evoclj.runtime.system
  "Integrant wiring for the STABLE HOST components (component Step 4).

  This namespace is the host-component wiring PLAN only. Integrant
  (integrant.core 1.0.1 — init-key / halt-key!) appears here exactly
  once, and only for the stable, host-owned lifecycle components:

    :store/sqlite         the SQLite db spec (evoclj.store.sqlite)
    :store/cas            the content-addressed store config (evoclj.store.cas)
    :provider/registry    the kernel-owned provider registry atom
                          (evoclj.provider.registry)
    :capability/broker    the broker context (evoclj.intent.dispatch)

  Genome graph nodes are NEVER Integrant components: :node/* topology
  nodes, :program/* SCI programs, and :graph/* entries are per-Phenotype
  values constructed inside an isolated SCI runtime by
  evoclj.runtime.phenotype/instantiate, never global host components
  (Global Constraints 22, 23). The Phenotype is deliberately NOT an
  Integrant component either — it is constructed per session by the
  component executor.

  Every method below is THIN and dependency-injected: init-key builds
  one component from the config subtree (or defers to the focused
  module's own constructor), and halt-key! destroys it. v0 components
  own no OS resources — SQLite opens per-operation connections
  (evoclj.store.sqlite/with-db), CAS is a config map, and the registry
  and broker are in-memory atoms — so halt-key! is an honest no-op for
  each of them. Because the methods are thin and inject their
  dependencies through the config map, the component map builds and
  tears down directly WITHOUT the Integrant runtime:

    (ig/init-key :store/sqlite \":memory:\")        ; a java.jdbc spec
    (ig/init-key :provider/registry {})             ; a registry atom
    (ig/init-key :capability/broker {:registry reg :leases []})

  :runtime/executor is the declared extension point of the wiring
  plan: the component scheduler component joins here once the executor
  exists (YAGNI, Global Constraint 24 — a key whose component cannot
  be constructed yet is not registered)."
  (:require [evoclj.intent.dispatch :as dispatch]
            [evoclj.mcp.manager :as mcp-manager]
            [evoclj.provider.registry :as registry]
            [evoclj.store.cas :as cas]
            [evoclj.store.sqlite :as sqlite]
            [integrant.core :as ig]))

;; --- component keys ----------------------------------------------------------

(def store-sqlite-key
  "Integrant key of the :store/sqlite host component: the SQLite db
  spec (string path or java.jdbc spec map)."
  :store/sqlite)

(def store-cas-key
  "Integrant key of the :store/cas host component: the content-
  addressed store config map (see evoclj.store.cas/->cas)."
  :store/cas)

(def provider-registry-key
  "Integrant key of the :provider/registry host component: the
  kernel-owned provider registry atom (see
  evoclj.provider.registry/create-registry)."
  :provider/registry)

(def capability-broker-key
  "Integrant key of the :capability/broker host component: the broker
  context map (see evoclj.intent.dispatch/make-broker-context)."
  :capability/broker)

(def runtime-executor-key
  "Declared extension point of the host wiring plan: the component
  scheduler/executor component key. No init-key is registered until
  the executor exists (YAGNI, Global Constraint 24)."
  :runtime/executor)

;; --- init-key / halt-key! methods -------------------------------------------

(defmethod ig/init-key :store/sqlite
  [_ config]
  "Build the :store/sqlite component: coerce `config` (a SQLite path
  string or a java.jdbc spec map) into a java.jdbc db spec. NO
  connection is opened — evoclj.store.sqlite opens and closes a fresh
  connection per operation (with-db), so the component IS the spec
  value."
  (sqlite/spec config))

(defmethod ig/halt-key! :store/sqlite
  [_ component]
  "Destroy the :store/sqlite component. Per-operation connections mean
  there is no persistent handle to close; the component is a plain spec
  value."
  nil)

(defmethod ig/init-key :store/cas
  [_ config]
  "Build the :store/cas component: a config map from `config`, either a
  bare root (string/File/Path) or {:root ... :verify ...}. NO directory
  is created and nothing is written — evoclj.store.cas/->cas is pure
  value construction."
  (if (map? config)
    (cas/->cas (:root config) {:verify (boolean (:verify config))})
    (cas/->cas config)))

(defmethod ig/halt-key! :store/cas
  [_ component]
  "Destroy the :store/cas component: the CAS handle is a config map
  with no open resource; nothing to close."
  nil)

(defmethod ig/init-key :provider/registry
  [_ _config]
  "Build the :provider/registry component: a fresh, empty provider
  registry atom (evoclj.provider.registry/create-registry). Providers
  are registered into it by the host after init."
  (registry/create-registry))

(defmethod ig/halt-key! :provider/registry
  [_ component]
  "Destroy the :provider/registry component: the registry is an
  in-memory atom owned by the host; nothing to close."
  nil)

(defmethod ig/init-key :capability/broker
  [_ config]
  "Build the :capability/broker component: the broker context map from
  `config` via evoclj.intent.dispatch/make-broker-context. `config`
  carries the injected dependencies — :registry (an Integrant ref
  resolved before init-key runs, or an atom in direct use), :leases,
  :usage, :now, :max-attempts — so the method stays thin and the
  component builds directly without the Integrant runtime."
  (dispatch/make-broker-context config))

(defmethod ig/halt-key! :capability/broker
  [_ component]
  "Destroy the :capability/broker component: the broker context is an
  in-memory map; nothing to close."
  nil)

(defmethod ig/init-key :mcp/manager [_ _] (mcp-manager/create-manager))
(defmethod ig/halt-key! :mcp/manager [_ mgr] (mcp-manager/shutdown! mgr))

(ns evoclj.runtime.subagent-lease
  "The global in-memory lease registry for subagent spawn-derived leases (S4).

  Child leases are minted by evoclj.runtime.subagent-capability/derive-child-leases
  against this registry (durable DB INSERT first, then cache — the P1
  `DB is truth` ordering). Cancellation tombstones the same registry after
  the durable revoke commits (evoclj.runtime.subagent-cancel).

  THIS NAMESPACE EXISTS TO BREAK A CYCLE. The registry used to live in
  evoclj.runtime.subagent, which evoclj.runtime.subagent-cancel deliberately
  does NOT require (subagent-cancel exists precisely to avoid the
  scheduler ↔ subagent cycle). With the registry owned by subagent, cancel
  had to reach it reflectively:

      @(requiring-resolve 'evoclj.runtime.subagent/subagent-lease-registry)

  at two call sites — a stringly lookup that can only ever hide a broken
  rename from static analysis (INV-05). Owning the registry here lets both
  consumers require it directly, so the reflection is gone rather than
  relocated.

  Depends only on evoclj.capability.mint; nothing it requires reaches
  runtime.subagent, so the edge is acyclic."
  (:require [evoclj.capability.mint :as mint]))

(defonce subagent-lease-registry
  (mint/create-lease-registry))

(defn clear-subagent-lease-state!
  "Test helper — clear the global subagent lease registry.
  Safe to call between fixtures."
  []
  (reset! subagent-lease-registry {:evoclj.capability.mint/version 0})
  nil)

(ns evoclj.capability.lease-registry
  "The LeaseRegistry version slot: the ONE definition of the registry's
  monotonic version key, its reader, and its bumper.

  A leaf namespace (no requires) on purpose: mint WRITES the slot, the
  durable stores (evoclj.capability.authority-store,
  evoclj.store.capability-store) PRESERVE it while replacing entries,
  and the runtime/test helpers RESET it — while mint's own dependency
  closure (authority-store -> store.capability-store) must not depend
  on mint itself, so the key cannot live in mint.

  The slot is MEMORY-ONLY: the registry atom is never persisted; only
  the lease rows are (the capabilities table is the durable truth), so
  the key's namespace is an internal detail with no migration impact.")

(def registry-version-key
  "The registry atom's monotonic version slot."
  ::version)

(defn registry-version
  "The registry's current version (0 when uninitialized)."
  [registry]
  (get @registry registry-version-key 0))

(defn bump-registry-version!
  "Increment the registry's version; returns the new version."
  [registry]
  (swap! registry update registry-version-key (fnil inc 0))
  (get @registry registry-version-key))

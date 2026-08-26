(ns evoclj.environment.source
  "LiveSource protocol.

  A Source produces pure candidate snapshots and notifies on invalidation.
  It does NOT decide tool visibility, context activation, filesystem
  authority, or session binding - those concerns live elsewhere. This
  namespace deliberately does not require those modules.")

(defprotocol LiveSource
  (snapshot! [this]
    "Return a pure candidate snapshot map with at least :source/id and :payload.
     MUST be pure: it may only READ current source state and return a value. It
     must NOT publish bundles, mutate the registry, advance counters, or perform
     any other side effect. Capture and publication are separated so the
     Source -> Revision -> Projector -> Bundle chain can run as ONE transaction
     owned by the registry (INV-06).")
  (project [this snapshot]
    "Pure projector: turn a snapshot captured by snapshot! into candidate
     bundle-construction opts for evoclj.environment.bundle/publish-bundle!.
     Returns a map recognized by publish-bundle! (e.g. {:logical-id ...
     :payload ... :surfaces [...]}) or nil to signal 'no bundle for this
     snapshot'. MUST be pure and side-effect free. Throwing from project is how
     a mid-chain failure is expressed (the transaction then fails closed).")
  (subscribe! [this invalidate-fn]
    "Subscribe to invalidation. invalidate-fn is called when the source
     wishes to signal that a refresh should be considered. Returns a handle
     map with :subscription/id (uuid) and :close! (fn with no args).")
  (close! [this]
    "Close the source and release resources. Idempotent."))

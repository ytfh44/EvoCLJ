(ns evoclj.environment.source
  "LiveSource protocol.

  A Source produces pure candidate snapshots and notifies on invalidation.
  It does NOT decide tool visibility, context activation, filesystem
  authority, or session binding - those concerns live elsewhere. This
  namespace deliberately does not require those modules.")

(defprotocol LiveSource
  (snapshot! [this]
    "Return a pure candidate snapshot map with at least :source/id and :payload.
     Must be pure and not perform side effects beyond reading current state.")
  (subscribe! [this invalidate-fn]
    "Subscribe to invalidation. invalidate-fn is called when the source
     wishes to signal that a refresh should be considered. Returns a handle
     map with :subscription/id (uuid) and :close! (fn with no args).")
  (close! [this]
    "Close the source and release resources. Idempotent."))

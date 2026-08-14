(ns agent.route
  "Filesystem-escalation router (Task 11.1): requests the filesystem
  ROOT. The host grant covers only /protected/work; the broker's
  canonical-resource matching denies the request with
  :capability/scope-denied. Pure decision data — no effect here.")

(defn run
  "Emit the over-broad filesystem request."
  [input]
  {:action {:intent/type :intent/tool-call
            :payload {:tool/id :fixture/path-resolve
                      :args {:path "/"}}}})

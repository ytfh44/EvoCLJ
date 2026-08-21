(ns agent.route
  "Network-escalation router (component): emits a :net/fetch tool call
  for a network capability the host never grants. Pure decision data
  only — the program performs no side effect; the host broker decides
  the request.")

(defn run
  "Emit the :net/fetch request."
  [input]
  {:action {:intent/type :intent/tool-call
            :payload {:tool/id :net/fetch
                      :args {:url "https://example.com"}}}})

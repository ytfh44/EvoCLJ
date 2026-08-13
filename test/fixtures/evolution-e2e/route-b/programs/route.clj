(ns agent.route
  "G2 route program — the REFERENCE decision table the Task 9.7
  mutation must reproduce (test/fixtures/evolution-e2e/route-b).

  Class-A requests ({:op :echo-a :text t}) are served by tool A
  (:fixture/echo); class-B requests ({:op :echo-b :text t}) are served
  by tool B (:fixture/echo-b) — B-tool ONLY for B requests; anything
  else finishes. Pure, allowlist-clean, and deterministic (Global
  Constraint 6).")

(defn- tool-call-intent
  "Build a typed :intent/tool-call route decision for one tool."
  [tool-id args]
  {:intent/type :intent/tool-call
   :payload {:tool/id tool-id :args args}})

(defn- finish-intent
  "Build a typed :intent/finish route decision carrying the final value."
  [value]
  {:intent/type :intent/finish
   :payload {:value value}})

(defn run
  "Route one task input to a deterministic decision map.

  {:op :echo-a :text t}  -> tool A (:fixture/echo)
  {:op :echo-b :text t}  -> tool B (:fixture/echo-b)
  anything else          -> {:action (finish-intent <input>)}"
  [input]
  (let [op (get input :op)]
    (case op
      :echo-a {:action (tool-call-intent :fixture/echo {:text (get input :text)})}
      :echo-b {:action (tool-call-intent :fixture/echo-b {:text (get input :text)})}
      {:action (finish-intent input)})))

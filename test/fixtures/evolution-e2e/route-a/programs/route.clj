(ns agent.route
  "G1 route program (Task 9.7 fixture): chooses tool A for EVERY
  request.

  Class-A requests ({:op :echo-a :text t}) are served by tool A
  (:fixture/echo). Class-B requests ({:op :echo-b :text t}) FAIL under
  this policy: the router raises a typed decision error instead of
  emitting a servable intent, so the scheduler records the session as
  :failed and the episode is a failure — the Evolution set shows
  class-B requests fail with A.

  Pure decision logic only: no host interop, no IO, no eval. The host
  SCI sandbox (Milestone 3) is the final enforcement layer; this
  source stays inside the compiler's static policy allowlist by
  construction.")

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
  {:op :echo-b :text t}  -> FAILS: class B is not servable under the
                           A-for-everything policy
  anything else          -> {:action (finish-intent <input>)}

  Pure and deterministic: the same input always yields the same
  decision or the same typed failure."
  [input]
  (let [op (get input :op)]
    (case op
      :echo-a {:action (tool-call-intent :fixture/echo {:text (get input :text)})}
      :echo-b (throw (ex-info "class-B requests fail under the A-for-everything router"
                              {:op :echo-b :tool :fixture/echo}))
      {:action (finish-intent input)})))

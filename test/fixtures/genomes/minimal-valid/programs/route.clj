(ns agent.route
  "Seed Genome routing program (component).

  Pure decision logic only: reads one EDN task input map and returns a
  deterministic route decision map. No host interop, no IO, no eval —
  everything the program can touch arrives as data and leaves as data.
  The host SCI sandbox (Milestone 3) is the final enforcement layer;
  this source stays inside the compiler's static policy allowlist by
  construction: no load-file/eval, no require/use of undeclared host
  namespaces, no Java class literals, no host interop forms, and no
  #= reader-eval.")

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
  "Route one task input map to a deterministic decision map.

  {:op :echo :text t}    -> {:action {:intent/type :intent/tool-call
                                       :payload {:tool/id :fixture/echo
                                                 :args {:text t}}}}
  {:op :finish :value v} -> {:action {:intent/type :intent/finish
                                       :payload {:value v}}}
  anything else          -> {:action {:intent/type :intent/finish
                                       :payload {:value <input>}}}

  Pure and deterministic: the same input always yields the same
  decision, and no side effect is performed here — effects only happen
  when the host broker authorizes the emitted Intent (Milestone 4)."
  [input]
  (let [op (get input :op)]
    (case op
      :echo {:action (tool-call-intent :fixture/echo {:text (get input :text)})}
      :finish {:action (finish-intent (get input :value))}
      {:action (finish-intent input)})))

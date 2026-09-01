(ns agent.route
  "Child-extension router (component): emits the parent's :fixture/echo
  tool call, reusing the parent's capability request. The host lease is
  bound to the PARENT phenotype's id; exact subject matching denies the
  child with :capability/principal-mismatch. Pure decision data.")

(defn run
  "Emit the parent's :fixture/echo request."
  [input]
  {:action {:intent/type :intent/tool-call
            :payload {:tool/id :fixture/echo
                      :args {:text "child-call"}}}})

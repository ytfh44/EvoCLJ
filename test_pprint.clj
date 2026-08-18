(let [m {:envelope/version 1
         :envelope/task {:task/id "t1"}}
      w (java.io.StringWriter.)]
  (clojure.pprint/pprint m w)
  (println (str w)))

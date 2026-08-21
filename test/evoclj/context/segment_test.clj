(ns evoclj.context.segment-test
  (:require [clojure.test :as t]
            [evoclj.context.segment :as seg]
            [evoclj.genome.hash :as hash]))

(t/deftest make-segment-valid
  (let [rev (hash/text-digest "content")]
    (let [s (seg/make-segment {:logical-id [:skill "debugging"]
                               :revision-id rev
                               :bundle-id "bundle:1"
                               :content "# SKILL\nhello"})]
      (t/is (seg/segment? s))
      (t/is (= rev (:segment/revision-id s))))))

(t/deftest segment-from-binding
  (let [rev (hash/text-digest "skill text")
        binding {:logical/id [:skill "debugging"]
                 :revision/id rev
                 :bundle/id "bundle:1"
                 :binding/activated-at 12345}
        s (seg/segment-from-binding binding "skill text")]
    (t/is (= rev (:segment/revision-id s)))
    (t/is (= "skill text" (:segment/content s)))
    (t/is (= 12345 (:segment/activated-at s)))))

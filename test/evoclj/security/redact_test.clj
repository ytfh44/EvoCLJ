(ns evoclj.security.redact-test
  "Task F7 tests for write-path secret redaction
  (evoclj.security.redact).

  Coverage maps to the module contract:

  - :pattern specs replace every regex match in every STRING value
    during the walk — nested in maps, vectors, and other strings — while
    map KEYS are never rewritten and non-matching strings pass through.
  - :key-path specs replace the value at a get-in key path, whether the
    value is a map, a vector, or a plain non-map value, including after
    multiple path components.
  - Idempotence: redacting an already-redacted value yields the same
    value (double redact == single redact).
  - validate-specs! rejects a non-sequential specs collection
    (:reason :not-sequential) and a spec that violates the closed
    RedactSpecSchema (:reason :spec-invalid).
  - redact-event redacts ONLY :metadata and leaves every other event key
    byte-identical."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.security.redact :as redact]))

(def ^:private bearer-spec
  "A :pattern spec for a fictional bearer-token lexical shape."
  {:redact/kind :pattern
   :redact/pattern #"Bearer [A-Za-z0-9._-]+"})

(def ^:private api-key-spec
  "A :key-path spec that removes the value at [:metadata :api-key]."
  {:redact/kind :key-path
   :redact/paths [[:metadata :api-key]]})

(defn- thrown-error
  "The ExceptionInfo thrown by f, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

;; --- :pattern redaction ------------------------------------------------------

(deftest pattern-spec-replaces-matches-in-nested-values
  (let [value {:metadata {:topic "oops"
                          :payload {:token "Bearer abcDEF.xyz"
                                     :note "attach Bearer 123.456 here"}
                          :other "not a match"}}]
    (is (= {:metadata {:topic "oops"
                       :payload {:token "[REDACTED]"
                                  :note "attach [REDACTED] here"}
                       :other "not a match"}}
           (redact/redact value [bearer-spec])))))

(deftest pattern-spec-redacts-inside-vectors-too
  (let [value {:events [{:auth "Bearer abc"} {:auth "Bearer 123"}]}]
    (is (= {:events [{:auth "[REDACTED]"} {:auth "[REDACTED]"}]}
           (redact/redact value [bearer-spec])))))

(deftest pattern-spec-passes-through-untouched-values
  (testing "non-matching strings are unchanged"
    (is (= {:a "plain text" :b 42 :c true :d nil}
           (redact/redact {:a "plain text" :b 42 :c true :d nil} [bearer-spec]))))
  (testing "non-string leaves pass through"
    (is (= {:n 1 :v [2 3] :m {:k :x}}
           (redact/redact {:n 1 :v [2 3] :m {:k :x}} [bearer-spec])))))

(deftest pattern-spec-never-rewrites-keys
  (let [value {:metadata "Bearer abc"}]
    (is (= {:metadata "[REDACTED]"} (redact/redact value [bearer-spec])))
    (testing "the map key itself is not touched when it contains a match"
      (let [input (hash-map "Bearer abc-key" "value")]
        (is (= input (redact/redact input [bearer-spec])))))))

;; --- :key-path redaction -----------------------------------------------------

(deftest key-path-replaces-map-value-at-path
  (let [value {:metadata {:api-key "sk-live-123" :other "keep"}}]
    (is (= {:metadata {:api-key "[REDACTED]" :other "keep"}}
           (redact/redact value [api-key-spec])))))

(deftest key-path-replaces-non-map-value-at-path
  (testing "a plain leaf (string) at the path is replaced too"
    (is (= {:metadata {:api-key "[REDACTED]"}}
           (redact/redact {:metadata {:api-key "raw-secret"}} [api-key-spec]))))
  (testing "a vector leaf at the path is replaced"
    (is (= [{} {:metadata {:api-key "[REDACTED]"}}]
           (redact/redact [{} {:metadata {:api-key ["a" "b"]}}] [api-key-spec])))))

(deftest key-path-supports-nested-paths
  (let [spec {:redact/kind :key-path
              :redact/paths [[:user :credentials :password]]}
        value {:user {:name "a" :credentials {:password "hunter2" :salt "s1"}}}]
    (is (= {:user {:name "a" :credentials {:password "[REDACTED]" :salt "s1"}}}
           (redact/redact value [spec])))))

(deftest key-path-the-absent-key-path-is-untouched
  (let [value {:metadata {:other "value"}}]
    (is (= value (redact/redact value [api-key-spec])))))

;; --- idempotence -------------------------------------------------------------

(deftest redaction-is-idempotent
  (let [value {:metadata {:api-key "sk-abc"
                          :token "Bearer 123.xyz"
                          :note "send Bearer abc via sk-abc"}}
        single (redact/redact value [api-key-spec bearer-spec])]
    (is (= {:metadata {:api-key "[REDACTED]"
                       :token "[REDACTED]"
                       :note "send [REDACTED] via sk-abc"}}
           single))
    (testing "a second redaction yields the same value"
      (is (= single (redact/redact single [api-key-spec bearer-spec]))))))

;; --- specs validation --------------------------------------------------------

(deftest validate-specs-rejects-non-sequential
  (testing "a non-sequential specs value is rejected with :not-sequential"
    (let [e (thrown-error #(redact/validate-specs! {:redact/kind :pattern}))]
      (is (= :security/redact-invalid (:error/type (ex-data e))))
      (is (= :not-sequential (:reason (ex-data e)))))))

(deftest validate-specs-rejects-invalid-spec
  (let [assert-invalid
        (fn [spec]
          (let [e (thrown-error #(redact/validate-specs! [spec]))]
            (is (some? e) (pr-str spec))
            (is (= :security/redact-invalid (:error/type (ex-data e))) (pr-str spec))
            (is (= :spec-invalid (:reason (ex-data e))) (pr-str spec))))]
    (testing "unknown keys are rejected (closed map)"
      (assert-invalid {:redact/kind :pattern :bogus 1}))
    (testing "an unknown kind is rejected"
      (assert-invalid {:redact/kind :explode}))
    (testing "a :pattern spec with a non-regex source is rejected"
      (assert-invalid {:redact/kind :pattern :redact/pattern 42}))
    (testing "a :pattern spec without a regex is schema-valid (:redact/pattern is optional)"
      (is (= [{:redact/kind :pattern}]
             (redact/validate-specs! [{:redact/kind :pattern}]))))
    (testing "a :key-path spec with non-keyword path components is rejected"
      (assert-invalid {:redact/kind :key-path
                       :redact/paths [[:metadata "api-key"]]}))))

(deftest validate-specs-accepts-a-valid-collection
  (is (= [bearer-spec] (redact/validate-specs! [bearer-spec])))
  (is (= [] (redact/validate-specs! []))))

;; --- redact-event ------------------------------------------------------------

(def ^:private meta-key-spec
  "A :key-path spec whose path is relative to the :metadata sub-map that
  redact-event redacts (redact-event applies specs to the metadata value
  itself, so paths name keys inside it, e.g. [:api-key])."
  {:redact/kind :key-path
   :redact/paths [[:api-key]]})

(deftest redact-event-only-touches-metadata
  (let [event {:event/id 1
               :event/type :intent/proposed
               :session/id 42
               :generation/id "g-1"
               :cause/event-id 0
               :payload-ref "sha256:abc"
               :metadata {:api-key "sk-secret" :note "keep"}}
        redacted (redact/redact-event event [meta-key-spec])]
    (testing "the :metadata value is redacted"
      (is (= (assoc (dissoc event :metadata)
                    :metadata {:api-key "[REDACTED]" :note "keep"})
             redacted)))
    (testing "every other event key is byte-identical"
      (is (= (dissoc event :metadata) (dissoc redacted :metadata))))
    (testing "the full metadata is replaced, not merged away"
      (is (= "[REDACTED]" (get-in redacted [:metadata :api-key]))))))

(deftest redact-event-with-absent-metadata-is-unchanged
  (let [event {:event/id 1 :event/type :session/created}]
    (is (= event (redact/redact-event event [meta-key-spec])))))

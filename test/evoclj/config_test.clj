(ns evoclj.config-test
  "Tests for the single validated configuration contract and gated policy
  proposals (foundation F5). Covers: default-config satisfying
  ConfigSchema; load-config from EDN strings and maps with per-section
  defaults + overrides and key-wise :config/profiles merging; the four
  :config/invalid rejection paths (bad EDN, non-map input, unknown
  top-level key, non-map section); resolve-profile for present/absent
  profile keys; config-value direct/nested/absent lookups; and the full
  gated policy lifecycle including the terminal-state invariant and the
  :config/invalid-transition edges."
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [evoclj.config :as cfg]))

(defn- err-of
  "The ExceptionInfo thrown by f, or nil when f does not throw."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

;; ============================================================================
;; default-config / schema
;; ============================================================================

(deftest default-config-validates
  (testing "default-config satisfies ConfigSchema"
    (is (nil? (m/explain cfg/ConfigSchema (cfg/default-config)))))
  (testing "default shape"
    (is (= 1 (:config/version (cfg/default-config))))
    (is (= {} (:config/profiles (cfg/default-config))))
    (is (= {} (:config/model-routing (cfg/default-config))))
    (is (= {:max-cost 0.0 :max-tokens 0} (:config/budget (cfg/default-config))))
    (is (= {} (:config/judge (cfg/default-config))))
    (is (= {} (:config/retention (cfg/default-config))))))

;; ============================================================================
;; load-config
;; ============================================================================

(deftest load-config-from-edn-string
  (let [c (cfg/load-config "{:config/model-routing {:default :fast :fallback :cheap}
                             :config/budget {:hard-cap 100}
                             :config/version 2}")]
    (testing "defaults present after load"
      (is (= {} (:config/profiles c)))
      (is (= {} (:config/judge c)))
      (is (= {} (:config/retention c))))
    (testing "overrides applied per section"
      (is (= 2 (:config/version c)))
      (is (= {:default :fast :fallback :cheap} (:config/model-routing c)))
      (is (= {:max-cost 0.0 :max-tokens 0 :hard-cap 100} (:config/budget c))))
    (testing "the loaded config itself validates"
      (is (nil? (m/explain cfg/ConfigSchema c))))))

(deftest load-config-from-map
  (let [c (cfg/load-config {:config/budget {:soft-cap 50}
                            :config/judge {:model :critic}})]
    (testing "defaults still present"
      (is (= 1 (:config/version c)))
      (is (= {} (:config/model-routing c)))
      (is (= {} (:config/retention c))))
    (testing "overrides applied"
      (is (= {:max-cost 0.0 :max-tokens 0 :soft-cap 50} (:config/budget c)))
      (is (= {:model :critic} (:config/judge c))))))

(deftest load-config-merges-profiles-keywise
  (let [c (cfg/load-config {:config/profiles {:fast {:config/model-routing {:default :fast}}}})]
    (testing "profile is merged key-wise over defaults"
      (is (= {:fast {:config/model-routing {:default :fast}}}
             (:config/profiles c))))))

(deftest load-config-rejects-bad-edn
  (let [e (err-of #(cfg/load-config "{:config/version 1"))]
    (is (some? e))
    (is (= :config/invalid (:error/type (ex-data e))))))

(deftest load-config-rejects-non-map-input
  (testing "a non-map, non-string is rejected"
    (let [e (err-of #(cfg/load-config 42))]
      (is (some? e))
      (is (= :config/invalid (:error/type (ex-data e))))))
  (testing "EDN that parses to a non-map is rejected"
    (let [e (err-of #(cfg/load-config "[1 2 3]"))]
      (is (some? e))
      (is (= :config/invalid (:error/type (ex-data e)))))))

(deftest load-config-rejects-unknown-top-level-key
  (let [e (err-of #(cfg/load-config {:config/version 1 :config/bogus {}}))]
    (is (some? e))
    (is (= :config/invalid (:error/type (ex-data e))))))

(deftest load-config-rejects-non-map-section
  (doseq [section [:config/model-routing :config/budget :config/judge :config/retention]]
    (let [e (err-of #(cfg/load-config {section [:not :a :map]}))]
      (testing (str "non-map " section " is rejected")
        (is (some? e))
        (is (= :config/invalid (:error/type (ex-data e))))))))

;; ============================================================================
;; validate-config!
;; ============================================================================

(deftest validate-config-bang-passes-and-rejects
  (testing "returns the config unchanged when valid"
    (let [c (cfg/default-config)]
      (is (identical? c (cfg/validate-config! c)))))
  (testing "throws :config/invalid on an invalid config"
    (let [e (err-of #(cfg/validate-config! {:config/version "one"}))]
      (is (some? e))
      (is (= :config/invalid (:error/type (ex-data e)))))))

;; ============================================================================
;; resolve-profile
;; ============================================================================

(deftest resolve-profile-merges-section-defaults
  (let [c (cfg/load-config {:config/profiles {:fast {:config/model-routing {:default :fast}
                                                     :config/budget {:hard-cap 200}}}})
        r (cfg/resolve-profile c :fast)]
    (testing "section overrides are applied on top of defaults"
      (is (= {:default :fast} (:config/model-routing r)))
      (is (= {:max-cost 0.0 :max-tokens 0 :hard-cap 200} (:config/budget r))))
    (testing "non-overridden sections fall back to defaults"
      (is (= 1 (:config/version r)))
      (is (= {} (:config/judge r)))
      (is (= {} (:config/retention r))))))

(deftest resolve-profile-missing-key-throws
  (let [c (cfg/load-config {:config/profiles {:fast {}}})
        e (err-of #(cfg/resolve-profile c :slow))]
    (is (some? e))
    (is (= :config/profile-not-found (:error/type (ex-data e))) )
    (is (= :slow (:policy/profile (ex-data e))))))

;; ============================================================================
;; config-value
;; ============================================================================

(deftest config-value-lookups
  (let [c (cfg/load-config {:config/model-routing {:default {:provider :acme :model :probe}}
                            :config/budget {:hard-cap 100}})]
    (testing "direct path"
      (is (= {:max-cost 0.0 :max-tokens 0 :hard-cap 100} (cfg/config-value c [:config/budget]))))
    (testing "nested path"
      (is (= :probe (cfg/config-value c [:config/model-routing :default :model]))))
    (testing "absent path returns nil"
      (is (nil? (cfg/config-value c [:config/nonexistent])))
      (is (nil? (cfg/config-value c [:config/budget :soft-cap]))))
    (testing "an empty path returns the whole config"
      (is (= c (cfg/config-value c []))))))

;; ============================================================================
;; gated policy lifecycle
;; ============================================================================

(deftest propose-policy-creates-proposed-record
  (let [p (cfg/propose-policy :model-routing {:default :fast})
        expl (m/explain cfg/PolicyProposalSchema p)]
    (testing "the record shapes as a valid policy proposal"
      (is (nil? expl)))
    (testing "fresh fields are stamped"
      (is (uuid? (:policy/proposal-id p)))
      (is (instance? java.util.Date (:policy/created-at p))))
    (testing "payload is persisted in the record"
      (is (= :model-routing (:policy/target p)))
      (is (= {:default :fast} (:policy/proposed p)))
      (is (= :proposed (:policy/status p))))))

(deftest policy-transitions-to-approved-and-rejected
  (let [p (cfg/propose-policy :budget {:hard-cap 50})]
    (testing "proposed -> approved"
      (let [a (cfg/transition-policy p :approved)]
        (is (= :approved (:policy/status a)))
        (is (= (:policy/proposal-id p) (:policy/proposal-id a)))
        (is (= (:policy/target a) (:policy/target p)))
        (is (= (:policy/proposed a) (:policy/proposed p)))))
    (testing "proposed -> rejected"
      (let [r (cfg/transition-policy p :rejected)]
        (is (= :rejected (:policy/status r)))))))

(deftest policy-terminal-states-are-immutable
  (let [approved (cfg/transition-policy (cfg/propose-policy :judge {}) :approved)
        rejected (cfg/transition-policy (cfg/propose-policy :judge {}) :rejected)]
    (doseq [[label terminal to]
            [[:approved approved :rejected]
             [:approved approved :proposed]
             [:rejected rejected :approved]
             [:rejected rejected :proposed]]]
      (testing (str "a " label " record cannot transition to " to)
        (let [e (err-of #(cfg/transition-policy terminal to))]
          (is (some? e))
          (is (= :config/invalid-transition (:error/type (ex-data e))))
          (is (= (:policy/status terminal) (:policy/status (ex-data e))))
          (is (= to (:new-status (ex-data e)))))))))

(deftest proposed-to-proposed-rejected
  (let [p (cfg/propose-policy :retention {})
        e (err-of #(cfg/transition-policy p :proposed))]
    (is (some? e))
    (is (= :config/invalid-transition (:error/type (ex-data e))))
    (is (= :proposed (:policy/status (ex-data e))))
    (is (= :proposed (:new-status (ex-data e))))))

;; ============================================================================
;; evolution-loop / canary / budget-extension (component)
;; ============================================================================

(deftest default-config-includes-new-sections
  (let [c (cfg/default-config)]
    (testing "evolution-loop defaults"
      (is (= {:max-generations 20
              :plateau-window 5
              :min-improvement 0.01
              :stop-on-regression? true}
             (:config/evolution-loop c))))
    (testing "canary defaults"
      (is (= {:healthy-window 50} (:config/canary c))))
    (testing "budget extension defaults"
      (is (= {:max-cost 0.0 :max-tokens 0} (:config/budget c))))
    (testing "the default config still validates (new sections included)"
      (is (nil? (m/explain cfg/ConfigSchema c))))))

(deftest load-config-overrides-partial-evolution-loop
  (let [c (cfg/load-config {:config/evolution-loop {:max-generations 3}})]
    (testing "overridden key takes the new value"
      (is (= 3 (:max-generations (:config/evolution-loop c))))
      (is (= 3 (cfg/config-value c [:config/evolution-loop :max-generations]))))
    (testing "non-overridden keys fall back to defaults"
      (is (= 5 (:plateau-window (:config/evolution-loop c))))
      (is (= 0.01 (:min-improvement (:config/evolution-loop c))))
      (is (= true (:stop-on-regression? (:config/evolution-loop c)))))
    (testing "other new section defaults are untouched"
      (is (= {:healthy-window 50} (:config/canary c))))
    (testing "loaded config validates"
      (is (nil? (m/explain cfg/ConfigSchema c))))))

(deftest load-config-overrides-canary-and-budget
  (let [c (cfg/load-config {:config/canary {:healthy-window 10}
                            :config/budget {:max-cost 5.0 :max-tokens 100}})]
    (testing "canary override applies"
      (is (= 10 (:healthy-window (:config/canary c))))
      (is (= 10 (cfg/config-value c [:config/canary :healthy-window]))))
    (testing "budget extension override applies and coexists with prior keys"
      (is (= 5.0 (:max-cost (:config/budget c))))
      (is (= 100 (:max-tokens (:config/budget c)))))
    (testing "loaded config validates"
      (is (nil? (m/explain cfg/ConfigSchema c))))))

(deftest load-config-budget-extension-default-and-override
  (testing "budget extension defaults to 0.0 / 0 when absent"
    (let [c (cfg/load-config {:config/budget {:hard-cap 100}})]
      (is (= 0.0 (:max-cost (:config/budget c))))
      (is (= 0 (:max-tokens (:config/budget c))))
      (is (= 100 (:hard-cap (:config/budget c))))))
  (testing "budget extension is overridable"
    (let [c (cfg/load-config {:config/budget {:max-cost 2.5 :max-tokens 42}})]
      (is (= 2.5 (:max-cost (:config/budget c))))
      (is (= 42 (:max-tokens (:config/budget c))))))
  (testing "canary is overridable and absent keys still fall back"
    (let [c (cfg/load-config {:config/canary {:healthy-window 7}})]
      (is (= 7 (:healthy-window (:config/canary c)))))))
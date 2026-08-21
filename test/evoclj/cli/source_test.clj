(ns evoclj.cli.source-test
  "CLI unification — generic source lifecycle commands delegating to EnvironmentRegistry."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.cli.main :as main]
            [evoclj.environment.fake :as fake]
            [evoclj.environment.registry :as reg]
            [evoclj.environment.static :as static]))

;; Helpers: create a registry with one or more fake sources

(defn- fresh-registry []
  (reg/create-registry))

(defn- with-fake
  ([id payload] (with-fake (fresh-registry) id payload))
  ([registry id payload]
   (let [src (fake/make-fake-source id payload)]
     (reg/register-source! registry src)
     {:registry registry :source src})))

;; source list

(deftest source-list-empty-when-no-registry
  (testing "source list with no registry returns empty"
    (let [{:keys [exit data]} (main/execute ["source" "list"] {})]
      (is (= 0 exit))
      (is (= 0 (:count data)))
      (is (= [] (:sources data))))))

(deftest source-list-delegates-to-registry
  (testing "source list lists registered sources via EnvironmentRegistry"
    (let [{:keys [registry]} (with-fake :test/a "payload-a")
          {:keys [exit data]} (main/execute ["source" "list"] {:registry registry})]
      (is (= 0 exit))
      (is (= 1 (:count data)))
      (is (= 1 (count (:sources data))))
      (is (= :test/a (:source/id (first (:sources data))))))))

(deftest source-list-multiple
  (testing "source list with two sources"
    (let [registry (fresh-registry)
          _ (reg/register-source! registry (fake/make-fake-source :test/a "A"))
          _ (reg/register-source! registry (static/make-static-source :test/static "S"))
          {:keys [exit data]} (main/execute ["source" "list"] {:registry registry})]
      (is (= 0 exit))
      (is (= 2 (:count data)))
      (is (= #{:test/a :test/static} (set (map :source/id (:sources data))))))))

;; source inspect

(deftest source-inspect-requires-id
  (testing "source inspect without id is usage-invalid"
    (let [{:keys [exit data]} (main/execute ["source" "inspect"] {})]
      (is (= 1 exit))
      (is (= :cli/usage-invalid (:error/type data))))))

(deftest source-inspect-existing
  (testing "source inspect <id> returns source details"
    (let [{:keys [registry]} (with-fake :test/a "hello")
          _ (reg/refresh! registry)
          {:keys [exit data]} (main/execute ["source" "inspect" "test/a"] {:registry registry})]
      (is (= 0 exit))
      (is (= :test/a (:source/id data)))
      (is (string? (:source/type data)))
      (is (contains? data :status)))))

(deftest source-inspect-with-colon
  (testing "source inspect accepts :test/a form"
    (let [{:keys [registry]} (with-fake :test/a "hello")
          _ (reg/refresh! registry)
          {:keys [exit data]} (main/execute ["source" "inspect" ":test/a"] {:registry registry})]
      (is (= 0 exit))
      (is (= :test/a (:source/id data))))))

(deftest source-inspect-not-found
  (testing "source inspect unknown id is source-not-found"
    (let [{:keys [registry]} (with-fake :test/a "A")
          {:keys [exit data]} (main/execute ["source" "inspect" "test/missing"] {:registry registry})]
      (is (= 1 exit))
      (is (= :cli/source-not-found (:error/type data))))))

(deftest source-inspect-no-registry
  (testing "source inspect with no registry is no-source"
    (let [{:keys [exit data]} (main/execute ["source" "inspect" "test/a"] {})]
      (is (= 1 exit))
      (is (= :environment/no-source (:error/type data))))))

;; source refresh

(deftest source-refresh-requires-id-or-all
  (testing "source refresh without id nor --all is usage-invalid"
    (let [{:keys [registry]} (with-fake :test/a "A")
          {:keys [exit data]} (main/execute ["source" "refresh"] {:registry registry})]
      (is (= 1 exit))
      (is (= :cli/usage-invalid (:error/type data))))))

(deftest source-refresh-single
  (testing "source refresh <id> delegates to registry/refresh! and increments seq"
    (let [{:keys [registry]} (with-fake :test/a "A")
          r1 (reg/refresh! registry)
          seq1 (:revision/seq (:revision r1))
          _ (fake/set-payload! (get-in @registry [:sources :test/a]) "B")
          {:keys [exit data]} (main/execute ["source" "refresh" "test/a"] {:registry registry})]
      (is (= 0 exit))
      (is (contains? #{:published :noop} (:status data)))
      (when (= :published (:status data))
        (is (= (inc seq1) (:revision/seq (:revision data))))))))

(deftest source-refresh-all
  (testing "source refresh --all refreshes every source"
    (let [registry (fresh-registry)
          _ (reg/register-source! registry (fake/make-fake-source :test/a "A"))
          _ (reg/register-source! registry (static/make-static-source :test/b "B"))
          _ (reg/refresh! registry :test/a)
          _ (reg/refresh! registry :test/b)
          seq-before (:seq @registry)
          ;; mutate one
          _ (fake/set-payload! (get-in @registry [:sources :test/a]) "A2")
          {:keys [exit data]} (main/execute ["source" "refresh" "--all"] {:registry registry})]
      (is (= 0 exit))
      (is (= :all (:refreshed data)))
      (is (= 2 (:count data)))
      (is (= 2 (count (:results data))))
      ;; at least one should have published
      (is (some #(= :published (:status %)) (:results data))))))

(deftest source-refresh-not-found
  (testing "source refresh unknown id is source-not-found"
    (let [{:keys [registry]} (with-fake :test/a "A")
          {:keys [exit data]} (main/execute ["source" "refresh" "test/missing"] {:registry registry})]
      (is (= 1 exit))
      (is (= :cli/source-not-found (:error/type data))))))

(deftest source-refresh-no-source
  (testing "source refresh --all with no registered sources is no-source"
    (let [registry (fresh-registry)
          {:keys [exit data]} (main/execute ["source" "refresh" "--all"] {:registry registry})]
      (is (= 1 exit))
      (is (= :environment/no-source (:error/type data))))))

;; main dispatch wiring: ensure source commands are registered and mcp refresh-providers removed

(deftest source-commands-are-registered
  (testing "source list/inspect/refresh are known commands"
    (let [registry (fresh-registry)
          _ (reg/register-source! registry (fake/make-fake-source :test/a "A"))]
      (is (= 0 (:exit (main/execute ["source" "list"] {:registry registry}))))
      (is (= 0 (:exit (main/execute ["source" "inspect" "test/a"] {:registry registry}))))
      (is (= 0 (:exit (main/execute ["source" "refresh" "test/a"] {:registry registry})))))))

(deftest mcp-refresh-providers-removed
  (testing "mcp refresh-providers is no longer a known command (generic lifecycle)"
    (let [{:keys [exit data]} (main/execute ["mcp" "refresh-providers"] {})]
      (is (= 1 exit))
      (is (= :cli/unknown-command (:error/type data))))))

(deftest mcp-diagnostics-remain
  (testing "mcp status and diagnose remain as diagnostics"
    (let [{:keys [exit data]} (main/execute ["mcp" "status"] {})]
      (is (= 0 exit))
      (is (= :ok (:mcp/status data))))
    (let [{:keys [exit data]} (main/execute ["mcp" "diagnose" "test-conn"] {})]
      (is (= 0 exit))
      (is (contains? data :state)))))

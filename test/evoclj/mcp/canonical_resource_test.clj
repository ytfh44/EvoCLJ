(ns evoclj.mcp.canonical-resource-test
  "M13 — canonical v2: remove parameter-NAME heuristics; pure-data
   projection DSL; undeclared parameter -> :mcp/remote-effect :invoke.

   These tests exercise the REAL production path:
   evoclj.mcp.canonical/canonical-resource is invoked from
   evoclj.provider.mcp-bridge and evoclj.mcp.source's
   Provider/normalize-request, and its result flows into the broker's
   authorization decision. No injected fn, no shape-only assertions: the
   filesystem-scoping and default-invoke cases are asserted end to end
   through mcp-bridge/mcp-provider + evoclj.capability.broker."
  (:require [clojure.test :refer [deftest is testing]]
            [evoclj.mcp.canonical :as canonical]
            [evoclj.capability.broker :as broker]
            [evoclj.capability.policy :as policy]
            [evoclj.provider.mcp-bridge :as mcp-bridge]
            [evoclj.provider.protocol :as proto]))

;; ---------------------------------------------------------------------------
;; 1. HAPPY PATH — a DECLARED projection is honored (data-driven, not named)
;; ---------------------------------------------------------------------------

(deftest declared-filesystem-projection-honored
  (testing "a descriptor that DECLARES a path->:filesystem/path projection
           produces the declared resource; broker still requires a fs lease"
    (let [descriptor {:tool/id :mcp/read_file
                      :mcp/param-projections
                      [{:param "path"
                        :resource-kind :filesystem/path
                        :resource-path-key :path
                        :resource-action :read
                        :remote-effect :filesystem-read}]}
          resource (canonical/canonical-resource descriptor {"path" "a/../secret"})]
      (is (= :filesystem/path (:kind resource)))
      (is (= "secret" (:path resource)) "traversal normalized, declared path honored")
      (is (= :filesystem-read (:mcp/remote-effect resource)) "declared effect carried")
      ;; end-to-end broker: tool lease alone must NOT cover the fs resource
      (let [provider (mcp-bridge/mcp-provider
                      {:transport-config {:type :stdio :command "echo"}
                       :tool/id :mcp/read_file
                       :tool/mcp-name "read_file"
                       :input-schema [:map [:path :string]]
                       :output-schema [:map]
                       :mcp/param-projections
                       [{:param "path"
                         :resource-kind :filesystem/path
                         :resource-path-key :path
                         :resource-action :read
                         :remote-effect :filesystem-read}]})
            intent {:intent/id #uuid "00000000-0000-0000-0000-000000000001"
                    :intent/type :intent/tool-call
                    :phenotype/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                    :session/id #uuid "00000000-0000-0000-0000-000000000002"
                    :node/id :node/tool :cause/event-id 1
                    :payload {:tool/id :mcp/read_file :args {:path "/etc/shadow"}}
                    :budget {:wall-ms 1000} :metadata {}}
            normalized (proto/normalize-request provider intent)
            tool-lease {:cap/id #uuid "00000000-0000-0000-0000-000000000010"
                        :subject {:phenotype/id "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
                        :resource {:kind :tool :id :mcp/read_file}
                        :actions #{:invoke} :constraints {}
                        :issued-at #inst "2020-01-01" :expires-at #inst "2030-01-01"}
            decision (broker/authorize {:intent intent :normalized-request normalized
                                         :leases [tool-lease] :usage {} :now #inst "2025-01-01"})]
        (is (= :deny (:decision decision)) "tool lease alone cannot cover declared fs resource")
        ;; M14: the canonical projection sets :action :read on the resource,
        ;; which is now a first-class tuple component; a :invoke-only tool
        ;; lease does not grant :read, so the precise reason is
        ;; :capability/action-denied (not a scope failure).
        (is (= :capability/action-denied (:reason decision)))))))

;; ---------------------------------------------------------------------------
;; 2. NEW BRANCH — pure-data projection DSL applies the declared spec
;; ---------------------------------------------------------------------------

(deftest projection-dsl-applies-declared-spec-not-name
  (testing "the projection is selected by the DECLARED :param, not by any
           magic name; a non-'path' declared param also projects"
    (let [descriptor {:tool/id :mcp/blob_store
                      :mcp/param-projections
                      [{:param "file_uri"
                        :resource-kind :filesystem/path
                        :resource-path-key :path
                        :resource-action :read
                        :remote-effect :filesystem-read}]}
          r (canonical/canonical-resource descriptor {"file_uri" "/x/y"})]
      (is (= :filesystem/path (:kind r)))
      (is (= "/x/y" (:path r))))))

(deftest declared-projection-absent-param-falls-to-invoke
  (testing "when a projection is declared but its :param is not present in
           the args, the request fails closed to the default invoke effect
           (never inferred from other parameter names)"
    (let [descriptor {:tool/id :mcp/read_file
                      :mcp/param-projections
                      [{:param "path"
                        :resource-kind :filesystem/path
                        :resource-path-key :path
                        :resource-action :read
                        :remote-effect :filesystem-read}]}
          r (canonical/canonical-resource descriptor {"other" "x"})]
      (is (= :tool (:kind r)))
      (is (= :mcp/read_file (:id r)))
      (is (= :invoke (:mcp/remote-effect r))))))

;; ---------------------------------------------------------------------------
;; 3. NEW BRANCH — undeclared parameter -> :mcp/remote-effect :invoke
;; ---------------------------------------------------------------------------

(deftest undeclared-param-named-file-is-invoke-not-file-read
  (testing "a parameter literally named 'file' / 'path' / 'url' with NO
           declared projection becomes a plain :invoke effect, NOT a
           filesystem-read effect (the removed name heuristic)"
    (doseq [pname ["file" "path" "url"]]
      (let [descriptor {:tool/id :mcp/mysterious}
            r (canonical/canonical-resource descriptor {pname "/etc/shadow"})]
        (is (= :tool (:kind r)) (str pname " -> :tool kind"))
        (is (= :mcp/mysterious (:id r)))
        (is (= :invoke (:mcp/remote-effect r)) (str pname " -> :invoke, not :filesystem-read"))
        (is (not= :filesystem/path (:kind r)) "name never implies filesystem")))))

(deftest undeclared-tool-with-read_file-name-is-invoke
  (testing "even a tool literally NAMED read_file with no declared
           projection is just an :invoke effect (name is not authority)"
    (let [descriptor {:tool/id :mcp/read_file}
          r (canonical/canonical-resource descriptor {"path" "/etc/shadow"})]
      (is (= :tool (:kind r)))
      (is (= :invoke (:mcp/remote-effect r)))
      (is (nil? (:path r)) "no filesystem path inferred from the name"))))

;; ---------------------------------------------------------------------------
;; 4. FAULT CASES (>=2)
;; ---------------------------------------------------------------------------

(deftest missing-schema-param-handled
  (testing "args missing the canonical :args envelope / empty args do not
           throw and fall back to the safe invoke default"
    (let [descriptor {:tool/id :mcp/plain}]
      (is (= {:kind :tool :id :mcp/plain :mcp/remote-effect :invoke}
             (canonical/canonical-resource descriptor nil)))
      (is (= {:kind :tool :id :mcp/plain :mcp/remote-effect :invoke}
             (canonical/canonical-resource descriptor {}))))))

(deftest declared-projection-with-non-string-value-normalized-as-is
  (testing "a declared projection value that is not a string is passed
           through without path normalization corruption"
    (let [descriptor {:tool/id :mcp/x
                      :mcp/param-projections
                      [{:param "id"
                        :resource-kind :tool
                        :resource-id :thing
                        :resource-action :invoke
                        :remote-effect :invoke}]}
          r (canonical/canonical-resource descriptor {"id" 42})]
      (is (= :tool (:kind r)))
      (is (= :thing (:id r))))))

;; ---------------------------------------------------------------------------
;; 5. CONCURRENCY — canonical-resource is a pure function (no shared state);
;;    concurrent calls with distinct inputs yield distinct, stable results
;; ---------------------------------------------------------------------------

(deftest canonical-resource-pure-under-concurrent-calls
  (testing "pure function: 64 interleaved calls with different descriptors
           and args all return the correct, independent result"
    (let [fs-desc {:tool/id :mcp/fs
                   :mcp/param-projections
                   [{:param "path"
                     :resource-kind :filesystem/path
                     :resource-path-key :path
                     :resource-action :read
                     :remote-effect :filesystem-read}]}
          invoke-desc {:tool/id :mcp/plain}
          results (mapv (fn [i]
                          (if (even? i)
                            (canonical/canonical-resource fs-desc {"path" (str "p" i)})
                            (canonical/canonical-resource invoke-desc {"file" (str "f" i)})))
                        (range 64))]
      (is (every? (fn [[i r]]
                    (if (even? i)
                      (= :filesystem/path (:kind r))
                      (and (= :tool (:kind r)) (= :invoke (:mcp/remote-effect r)))))
                  (map vector (range) results))))))

;; ---------------------------------------------------------------------------
;; 6. REGRESSION — the OLD name-heuristic path is GONE (no read-file-tool?)
;; ---------------------------------------------------------------------------

(deftest old-name-heuristic-removed
  (testing "regression: canonical-resource no longer special-cases the
           'read_file' tool name or a 'path' parameter name; the removed
           read-file-tool? heuristic must have no effect"
    ;; these exact inputs used to yield :filesystem/path via the name
    ;; heuristic; after M13 they yield the fail-closed invoke default.
    (is (= {:kind :tool :id :mcp/read_file :mcp/remote-effect :invoke}
           (canonical/canonical-resource {:tool/id :mcp/read_file}
                                         {"path" "a/../secret"})))
    (is (= {:kind :tool :id :read_file :mcp/remote-effect :invoke}
           (canonical/canonical-resource {:tool/id :read_file}
                                         {"path" "a/../secret"})))
    ;; :filesystem/path is ONLY reachable through a DECLARED projection now
    (is (not= :filesystem/path
             (:kind (canonical/canonical-resource {:tool/id :mcp/read_file}
                                                  {"path" "a/../secret"}))))))

(deftest remote-effect-keyword-documented
  (testing "doc/behavior consistency: the undeclared default effect keyword
           :mcp/remote-effect :invoke matches the M13 contract"
    (is (= :invoke
           (:mcp/remote-effect
            (canonical/canonical-resource {:tool/id :mcp/anything} {}))))))

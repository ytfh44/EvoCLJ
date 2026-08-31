(ns evoclj.provider.registry-test
  "Tests for provider/tool descriptors and real-resource normalization
  (component).

  Providers adapt real resources behind the kernel-owned broker: the
  protocol is (describe provider) -> descriptor map,
  (normalize-request provider intent) -> canonical resource descriptor
  (the REAL target, produced BEFORE authorization sees anything), and
  (execute-request! provider authorized-request) -> result.

  Step 1 asserts the registry rejects duplicate tool IDs and malformed
  descriptors with typed errors (registration is fail-closed and
  changes nothing). Step 2 asserts the :fixture/echo provider used by
  the seed Genome: its descriptor is exactly the normative example,
  normalize-request turns a user-facing request into a canonical
  resource descriptor in the shape the capability lease matcher
  consumes ({:kind :tool :id ...}, component), and execute-request!
  returns the echoed value. Step 3 asserts the traversal-style
  :fixture/path-resolve fixture: raw \"a/../secret\" input resolves to
  the canonical protected path \"/protected/work/secret\" and IS
  checked as that canonical path — a lease rooted at the protected root
  covers the raw traversal only because it collapses inside the root,
  while a raw path whose joined form PREFIXES the root but
  canonicalizes outside it is never covered. Step 4 asserts the
  secrets rule: constructor config is closed over and never appears in
  descriptors or results.

  The canonical path segment resolution is reused from
  evoclj.capability.lease (component) so the tests here assert the
  provider-side contract on the same canonical forms coverage is
  decided on."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [evoclj.capability.lease :as lease]
            [evoclj.intent.core :as intent]
            [evoclj.kernel.error :as err]
            [evoclj.provider.fixture :as fixture]
            [evoclj.provider.protocol :as proto]
            [evoclj.provider.registry :as registry]
            [malli.core :as m]))

;; --- shared fixtures -------------------------------------------------------

(def ^:private echo-descriptor
  "The normative descriptor example from component — the :fixture/echo
  tool declared by the seed Genome."
  {:tool/id :fixture/echo
   :effect :pure
   :input-schema [:map [:text :string]]
   :output-schema [:map [:text :string]]
   :required-action :invoke
   :retry {:safe? true}})

(def ^:private session-id #uuid "11111111-1111-4111-8111-111111111111")
(def ^:private phenotype-id
  "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def ^:private issued-at (java.util.Date. 1700000000000))
(def ^:private expires-at (java.util.Date. 1700003600000)) ; issued-at + 1h

(def ^:private protected-root "/protected/work")

(defn- echo-intent
  "A validated :intent/tool-call for :fixture/echo carrying args."
  [args]
  (intent/tool-call session-id phenotype-id :node/tool 17
                    {:tool/id :fixture/echo :args args}
                    {:wall-ms 1000}))

(defn- resolve-intent
  "A validated :intent/tool-call for :fixture/path-resolve carrying
  args."
  [args]
  (intent/tool-call session-id phenotype-id :node/tool 17
                    {:tool/id :fixture/path-resolve :args args}
                    {:wall-ms 1000}))

(defn- lease-for
  "A valid CapabilityLease granting :invoke on the given resource
  (tool or filesystem) to phenotype-id."
  [resource]
  {:cap/id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
   :subject {:session/id #uuid "00000000-0000-4000-a000-000000000000" :phenotype/id phenotype-id}
   :resource resource
   :actions #{:invoke}
   :constraints {:max-calls 10}
   :issued-at issued-at
   :expires-at expires-at})

(defn- malformed-provider
  "A Provider whose describe returns the given (possibly malformed)
  descriptor."
  [descriptor]
  (reify proto/Provider
    (describe [_] descriptor)
    (normalize-request [_ _] nil)
    (execute-request! [_ _] nil)))

;; --- shared helpers --------------------------------------------------------

(defn- error-of
  "The ExceptionInfo thrown by the thunk f, or nil when f succeeds."
  [f]
  (try (f)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(defn- is-typed-error
  "Assert the thunk f throws an ExceptionInfo with the given
  :error/type."
  [f expected]
  (let [e (error-of f)]
    (is (some? e) "the call is rejected with an ExceptionInfo")
    (is (= expected (:error/type (ex-data e))))))

;; ============================================================================
;; Step 1 — registration rejects duplicate tool IDs and malformed descriptors
;; ============================================================================

(deftest registration-rejects-duplicate-tool-ids
  (let [r (registry/create-registry)]
    (is (= :fixture/echo (registry/register! r (fixture/echo-provider))))
    (testing "a second registration under the same :tool/id is rejected and changes nothing"
      (is-typed-error #(registry/register! r (fixture/echo-provider))
                      :provider/duplicate-tool-id)
      (is (= echo-descriptor (:descriptor (registry/lookup r :fixture/echo)))))))

(deftest registration-rejects-malformed-descriptors
  (let [cases
        [["not a map" "not-a-descriptor"]
         ["missing :tool/id" (dissoc echo-descriptor :tool/id)]
         [":tool/id not a keyword" (assoc echo-descriptor :tool/id "fixture/echo")]
         ["missing :effect" (dissoc echo-descriptor :effect)]
         ["missing :input-schema" (dissoc echo-descriptor :input-schema)]
         ["missing :output-schema" (dissoc echo-descriptor :output-schema)]
         ["missing :required-action" (dissoc echo-descriptor :required-action)]
         [":input-schema not a Malli schema" (assoc echo-descriptor :input-schema 42)]
         [":output-schema not a Malli schema" (assoc echo-descriptor :output-schema [:map [:text]])]
         [":retry not a map" (assoc echo-descriptor :retry true)]
         [":retry safe? not a boolean" (assoc echo-descriptor :retry {:safe? "yes"})]
         ["unknown top-level key" (assoc echo-descriptor :rogue/key 1)]]
        r (registry/create-registry)]
    (doseq [[label descriptor] cases]
      (testing label
        (is-typed-error #(registry/register! r (malformed-provider descriptor))
                        :provider/descriptor-invalid)))
    (testing "no malformed registration was committed"
      (is (nil? (registry/lookup r :fixture/echo))))))

(deftest registration-rejects-non-provider
  (let [r (registry/create-registry)]
    (testing "an object that does not satisfy the Provider protocol is rejected"
      (is-typed-error #(registry/register! r {:not :a-provider})
                      :provider/not-a-provider))
    (is (nil? (registry/lookup r :fixture/echo)))))

(deftest validate-descriptor-returns-valid-descriptor-unchanged
  (testing "a valid descriptor validates and is returned unchanged (no coercion)"
    (is (identical? echo-descriptor (registry/validate-descriptor echo-descriptor))))
  (testing "descriptors are plain immutable EDN data"
    (is (= echo-descriptor (edn/read-string (pr-str echo-descriptor)))))
  (testing "a malformed descriptor is rejected with a typed error carrying a serializable explanation"
    (let [e (error-of #(registry/validate-descriptor (dissoc echo-descriptor :tool/id)))
          d (err/error-data e)]
      (is (= :provider/descriptor-invalid (:error/type d)))
      (is (seq (:errors (:explanation (:error/data d)))))
      (is (= d (edn/read-string (pr-str d)))))))

(deftest lookup-returns-registered-entry-or-nil
  (let [r (registry/create-registry)
        echo (fixture/echo-provider)
        path (fixture/path-resolve-provider {:root protected-root})]
    (registry/register! r echo)
    (registry/register! r path)
    (testing "lookup returns the descriptor and the exact registered provider instance"
      (let [entry (registry/lookup r :fixture/echo)]
        (is (= echo-descriptor (:descriptor entry)))
        (is (identical? echo (get entry :provider)))))
    (testing "an unregistered tool id is not found"
      (is (nil? (registry/lookup r :fixture/unknown))))))

;; ============================================================================
;; Step 2 — the :fixture/echo provider used by the seed Genome
;; ============================================================================

(deftest echo-provider-describes-normative-tool
  (is (= echo-descriptor (proto/describe (fixture/echo-provider)))))

(deftest echo-provider-normalizes-request-to-canonical-resource
  (let [p (fixture/echo-provider)
        normalized (proto/normalize-request p (echo-intent {:text "hello"}))]
    (testing "normalize-request returns the canonical resource descriptor"
      (is (= {:tool/id :fixture/echo
              :resource {:kind :tool :id :fixture/echo}
              :args {:text "hello"}}
             normalized)))
    (testing "the args validate against the input-schema"
      (is (m/validate (:input-schema echo-descriptor) (:args normalized))))
    (testing "the canonical resource is exactly what a tool capability lease covers"
      (is (lease/resource-covers? (lease-for {:kind :tool :id :fixture/echo})
                                  (:resource normalized)
                                  :invoke)))))

(deftest echo-provider-executes-request
  (let [p (fixture/echo-provider)
        normalized (proto/normalize-request p (echo-intent {:text "hello"}))
        result (proto/execute-request! p normalized)]
    (is (= {:text "hello"} result))
    (testing "the result validates against the output-schema"
      (is (m/validate (:output-schema echo-descriptor) result)))))

(deftest echo-provider-rejects-malformed-input
  (let [p (fixture/echo-provider)]
    (testing "args failing the input-schema are rejected at normalization"
      (is-typed-error #(proto/normalize-request p (echo-intent {:text 42}))
                      :provider/input-invalid)
      (is-typed-error #(proto/normalize-request p (echo-intent {}))
                      :provider/input-invalid))
    (testing "a payload carrying no :args is rejected at normalization"
      (is-typed-error #(proto/normalize-request p {:payload {:tool/id :fixture/echo}})
                      :provider/input-invalid))))

(deftest echo-provider-rejects-unnormalized-execution
  (let [p (fixture/echo-provider)]
    (testing "execute-request! refuses a request that never went through normalize-request"
      (is-typed-error #(proto/execute-request! p {:tool/id :fixture/echo})
                      :provider/request-invalid))))

;; ============================================================================
;; Step 3 — traversal-style fixture: raw input normalizes to the canonical
;;          protected path and is CHECKED as that canonical path
;; ============================================================================

(deftest path-resolve-normalizes-traversal-to-canonical-protected-path
  (let [p (fixture/path-resolve-provider {:root protected-root})
        normalized (proto/normalize-request p (resolve-intent {:path "a/../secret"}))]
    (testing "the raw traversal input resolves to the canonical protected path, not the raw string"
      (is (= "/protected/work/secret" (get-in normalized [:resource :path])))
      (is (= {:kind :filesystem :path "/protected/work/secret"}
             (:resource normalized)))
      (is (not (str/includes? (pr-str (:resource normalized)) "a/../secret"))))
    (testing "the args still carry the user-facing request (for audit) and validate against the input-schema"
      (is (= {:path "a/../secret"} (:args normalized)))
      (is (m/validate (:input-schema (proto/describe p)) (:args normalized))))))

(deftest raw-traversal-input-is-checked-as-the-protected-canonical-path
  (let [p (fixture/path-resolve-provider {:root protected-root})
        root-lease (lease-for {:kind :filesystem :path protected-root})]
    (testing "a lease rooted at the protected root covers the raw traversal input ONLY because it resolves inside the root"
      (let [normalized (proto/normalize-request p (resolve-intent {:path "a/../secret"}))]
        (is (= {:kind :filesystem :path "/protected/work/secret"} (:resource normalized)))
        (is (lease/resource-covers? root-lease (:resource normalized) :invoke))))
    (testing "a raw path whose joined form PREFIXES the root but canonicalizes OUTSIDE it is never covered"
      ;; "/protected/work/../secret" starts with "/protected/work" as a
      ;; raw string, but the canonical target is "/protected/secret".
      (let [normalized (proto/normalize-request p (resolve-intent {:path "../secret"}))]
        (is (= {:kind :filesystem :path "/protected/secret"} (:resource normalized)))
        (is (not (lease/resource-covers? root-lease (:resource normalized) :invoke)))))
    (testing "a traversal escaping to the filesystem root is never covered"
      (let [normalized (proto/normalize-request p (resolve-intent {:path "../../../etc/passwd"}))]
        (is (= {:kind :filesystem :path "/etc/passwd"} (:resource normalized)))
        (is (not (lease/resource-covers? root-lease (:resource normalized) :invoke)))))))

(deftest path-resolve-handles-backslash-windows-and-absolute-input
  (let [p (fixture/path-resolve-provider {:root protected-root})
        root-lease (lease-for {:kind :filesystem :path protected-root})]
    (testing "backslash traversal normalizes to the same canonical protected path"
      (let [normalized (proto/normalize-request p (resolve-intent {:path "a\\..\\secret"}))]
        (is (= {:kind :filesystem :path "/protected/work/secret"} (:resource normalized)))
        (is (lease/resource-covers? root-lease (:resource normalized) :invoke))))
    (testing "an absolute user path stays absolute and is never rebased under the protected root"
      (let [normalized (proto/normalize-request p (resolve-intent {:path "/etc/passwd"}))]
        (is (= {:kind :filesystem :path "/etc/passwd"} (:resource normalized)))
        (is (not (lease/resource-covers? root-lease (:resource normalized) :invoke)))))
    (testing "a Windows drive path canonicalizes and stays outside the protected root"
      (let [normalized (proto/normalize-request p (resolve-intent {:path "C:/a/../secret"}))]
        (is (= {:kind :filesystem :path "/C:/secret"} (:resource normalized)))
        (is (not (lease/resource-covers? root-lease (:resource normalized) :invoke)))))))

(deftest path-resolve-executes-with-canonical-result
  (let [p (fixture/path-resolve-provider {:root protected-root})
        normalized (proto/normalize-request p (resolve-intent {:path "a/../secret"}))
        result (proto/execute-request! p normalized)]
    (is (= {:path "/protected/work/secret"} result))
    (testing "the result validates against the output-schema"
      (is (m/validate (:output-schema (proto/describe p)) result)))))

(deftest path-resolve-rejects-malformed-input
  (let [p (fixture/path-resolve-provider {:root protected-root})]
    (is-typed-error #(proto/normalize-request p (resolve-intent {:path 42}))
                    :provider/input-invalid)
    (is-typed-error #(proto/normalize-request p (resolve-intent {}))
                    :provider/input-invalid)
    (testing "execute-request! refuses a raw, unnormalized path target"
      (is-typed-error #(proto/execute-request! p {:args {:path "a/../secret"}})
                      :provider/request-invalid))))

;; ============================================================================
;; Step 4 — provider secrets/config are constructor-private, never in
;;          descriptors or results
;; ============================================================================

(deftest provider-secrets-never-appear-in-descriptors-or-results
  (let [secret "s3cr3t-token-9876543210"
        echo (fixture/echo-provider {:secret secret})
        path (fixture/path-resolve-provider {:root protected-root :secret secret})]
    (testing "describe (the descriptor) carries no secret"
      (is (not (str/includes? (pr-str (proto/describe echo)) secret)))
      (is (not (str/includes? (pr-str (proto/describe path)) secret))))
    (testing "constructor config never appears in a descriptor"
      (is (not (str/includes? (pr-str (proto/describe path)) protected-root))))
    (testing "normalize-request output carries no secret"
      (is (not (str/includes? (pr-str (proto/normalize-request echo (echo-intent {:text "hello"}))) secret)))
      (is (not (str/includes? (pr-str (proto/normalize-request path (resolve-intent {:path "a/../secret"}))) secret))))
    (testing "execute-request! results carry no secret"
      (is (not (str/includes?
                (pr-str (proto/execute-request! echo {:tool/id :fixture/echo
                                                      :resource {:kind :tool :id :fixture/echo}
                                                      :args {:text "hello"}}))
                secret)))
      (is (not (str/includes?
                (pr-str (proto/execute-request! path {:tool/id :fixture/path-resolve
                                                      :resource {:kind :filesystem
                                                                 :path "/protected/work/secret"}
                                                      :args {:path "a/../secret"}}))
                secret))))))

;; --- Global Constraint 22: provider values are plain validated data ---------

(deftest provider-values-round-trip-through-edn
  (let [p (fixture/echo-provider)
        normalized (proto/normalize-request p (echo-intent {:text "hello"}))
        result (proto/execute-request! p normalized)]
    (is (= normalized (edn/read-string (pr-str normalized))))
    (is (= result (edn/read-string (pr-str result))))
    (is (= echo-descriptor (edn/read-string (pr-str echo-descriptor))))))

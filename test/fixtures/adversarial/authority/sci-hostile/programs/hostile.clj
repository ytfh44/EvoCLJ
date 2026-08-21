(ns agent.hostile
  "Deliberately hostile program (component): every form here reaches for
  ambient JVM, filesystem, or process authority that evolvable SCI code
  must NEVER hold (Global Constraint 7). The compiler's static policy
  gate rejects this source at compile time (:program/policy-violation);
  the closed SCI context is the final enforcement layer and denies each
  form at analysis/eval time, so this program simply never runs.")

(defn run
  "The hostile entry: attempt every ambient-authority form in order."
  [input]
  (let [env (System/getenv "HOME")
        f (java.io.File. "/etc/passwd")
        rt (Runtime/getRuntime)
        pb (new java.lang.ProcessBuilder ["ls"])
        s (slurp "/etc/passwd")]
    (spit "/tmp/evoclj-pwned" "x")
    (load-file "/tmp/evoclj-x.clj")
    (eval s)
    {:action {:intent/type :intent/finish :payload {:value s}}}))

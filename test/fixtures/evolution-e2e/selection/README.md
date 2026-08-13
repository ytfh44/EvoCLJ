# Selection dataset — Task 9.7 hidden fixture

This directory is the PHYSICALLY SEPARATED Selection dataset for the
end-to-end evolutionary promotion test (Global Constraint 11). The
case bodies in the `*.edn` files are loaded ONLY by evaluator code,
after candidate materialization, through
`evoclj.eval.dataset/selection-loader` — the loader is the ONLY
dataset API surface that reveals selection bodies, and it is never
part of the evolution boundary.

CRITICAL (Step 2): the Diagnostician and Mutator adapters receive ONLY
the frozen evidence pack / the closed orchestration context. Neither
adapter, nor the candidate evaluation workspace, can ever reach this
directory: this root never appears in the pack, in the Mutator
context, or in the workspace staging (`dataset/build-candidate-workspace!`
mounts only the Evolution dataset).

## The case contract (documented in the e2e test namespace)

```clojure
{:case/id <keyword>
 :task-input <EDN>            ; fed to the session
 :expected-output <EDN>       ; the oracle — byte-identical by default;
                              ;   the session's accumulated outputs
                              ;   [<route decision> <provider result>]
 :tools #{<tool/id> ...}      ; the tools the case exercises
 :critical? <bool>}           ; optional; a lost critical case fails G5
```

The two cases:

- `:sel/a` — a class-A request (`{:op :echo-a :text "hi"}`) whose
  oracle expects tool A (`:fixture/echo`). Both G1 and G2 pass it.
- `:sel/b` — a class-B request (`{:op :echo-b :text "ho"}`), marked
  `:critical?`, whose oracle expects tool B (`:fixture/echo-b`). G1
  FAILS it (the A-for-everything router raises); G2 passes it. The
  paired comparison (G5) therefore shows G2 beating G1 above the
  profile's `:min-delta`.

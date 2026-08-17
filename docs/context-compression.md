# Context Compression Design Doc

## Goal
Replace a conversation context with a structured, EDN-safe envelope that preserves load-bearing information while reducing tokens.

## Compact Semantics
All compacts insert a special instruction block (the **footer**) at the end of the context. The footer tells the next agent turn what was done, what remains, and what load-bearing constraints must be honored.

The resulting context looks like:

```
<原始上下文>

;; === CONTEXT COMPRESSION ENVELOPE v1 ===
{:envelope/version 1
 :envelope/created-at "2026-08-17T00:00:00Z"
 ...}
;; === END ENVELOPE ===

[CONTEXT COMPRESSION]
You are continuing a compressed session.
Completed steps: ...
Current step: ...
Remaining: ...
Residue (load-bearing): ...
```

The new agent reads the envelope, then follows the footer instructions.

## Envelope Structure
```clojure
{:envelope/version 1
 :envelope/created-at "2026-08-17T00:00:00Z"
 :envelope/tokens-before 5000
 :envelope/tokens-after 300
 :envelope/window {:window/from 0 :window/to 10}
 :envelope/compressor {:compressor/model "gpt-4" :compressor/prompt "compress"}
 :envelope/task {:task/id "t1" :task/status :completed :task/description "..."}
 :envelope/subgoals [...]
 :envelope/residue [...]
 :envelope/evidence [...]}
```

## Principles
- **Residue over concision**: preserve user constraints and unresolved questions.
- **Provenance tracking**: every claim traces to its source session/segment.
- **Idempotent re-compression**: running compression twice produces equivalent output.
- **Fail-closed**: errors are typed and reported; no silent data loss.
- **Decoupled tool registration**: tools register themselves via `CompacterArchive` protocol rather than being hard-coded into the context subsystem.

## Modules
| Module | Responsibility |
|--------|---------------|
| `error` | Typed error keywords (`:error/type`, `:error/message`, etc.) |
| `envelope` | Malli schema, `make-envelope`, `validate-envelope`, `merge-envelopes` |
| `provenance` | Source/claim records + `trace-claim` / `provenance-report` |
| `residue` | Residue append/dedupe by text; supports `:constraint` and `:question` kinds |
| `idempotency` | Re-compression idempotency (core-fields presence check, residue/evidence accumulate) |
| `trigger` | Threshold + marker detection with cooldown, deterministic |
| `compressor` | Save-priority prompt builder + model-call wrapper, returns validated envelope |
| `compacter` | `Compacter` protocol + `DefaultCompacter` record + `run` helper |
| `registry` | `CompacterArchive` protocol + `register!` / `archiver-reports` for tool self-reporting |
| `footer` | Build footer text from envelope + archiver reports |
| `apply` | `apply-envelope` serializes envelope + fresh tail, EDN-safe |
| `eval` | Retention/regression/hallucination eval classes with pass/warn/fail thresholds |
| `crosscheck` | Generic structured-fields validator (not coupled to todo/goal tools) |
| `cli` | `context compress|inspect` commands |

## Invariants
- Envelope is always Malli-validated before use.
- `apply-envelope` never mutates the original context; it produces new EDN-safe content.
- Re-compression never drops residue or evidence items.
- Crosscheck auto-corrects `:task/status` and `:subgoal/status` to match the registered source of truth.
- Tools register via `CompacterArchive` protocol; the context subsystem never hard-codes tool names.
- The footer is generated fresh at compression time and is NOT stored in the envelope.

## User Extension Point
Users implement `Compacter` protocol to plug in their own compaction strategy:

```clojure
(defrecord MyCompacter [model threshold]
  compacter/Compacter
  (compress [this context opts]
    ;; custom logic
    {:envelope <map>
     :footer   <string>}))

(compacter/run context my-compacter {:model "gpt-4"})
```

## Backward Compatibility
- `crosscheck` old signature `[envelope todo]` is a deprecated wrapper forwarding to `crosscheck*`.
- `eval-regression-score` old 4-arg signature is a deprecated wrapper; the `todo` parameter is ignored.
- Both deprecated wrappers will be removed in a future version.

# EvoCLJ MCP 未闭合缺口 — 核验报告与修复规划

Date: 2026-08-20
Scope: 核验 26 项缺口树的真实性（读码取证），归并为 4 个一级根因，给出 6 步可执行重构序列。**只规划，不改码。**
Constraint: `io.modelcontextprotocol.sdk/mcp:2.0.0` 保持 pinned（legacy-era），所有修复在 legacy 语义上闭合，不提前切到 2026-07-28 stateless。

---

## 0. 核验方法

读码锚点：`src/evoclj/mcp/client.clj`、`src/evoclj/mcp/json_schema.clj`、`src/evoclj/mcp/transport.clj`、`src/evoclj/provider/mcp_bridge.clj`、`src/evoclj/kernel/error.clj`、`src/evoclj/provider/protocol.clj`、`src/evoclj/intent/dispatch.clj`、`src/evoclj/provider/registry.clj`、`src/evoclj/capability/{broker,policy}.clj`、`src/evoclj/runtime/system.clj`、`test/evoclj/provider/mcp_bridge_test.clj`，以及 2026-08-19 systemic-fixes design。

判定：`✅ 基本闭合` / `⚠️ 部分闭合` / `❌ 仍开放`。

---

## 1. 逐项核验（26 叶节点）

### A. Contract / Type Boundary

| # | 断言 | 取证 | 判定 |
| --- | ------ | ------ | ------ |
| **A1** JSON Schema 不 sound | `mcp_bridge.clj:unsupported-json-schema-keys` 已把 `oneOf/anyOf/allOf/$ref/$defs/pattern/format/...` 从"丢弃→:any"改为 `wrap-json-schema-validator`→`json_schema/validate` 的 fail-closed 路径，不再静默 :any。但 `json_schema.clj` 仍是子集 validator：只处理 `type/properties/required/additionalProperties/items/enum/const/minimum/maximum/minLength/maxLength/nullable`，其余关键字仍 permissive；无 2020-12 vocabulary 协商、无 `$ref` 外部解析禁止/深度/节点数/budget、无 regex DoS 预算。 | **⚠️ 部分闭合**：fail-open 已堵住，但未达到"完整 2020-12 validator 为 source of truth，Malli 仅为已证明等价的 fast path" |
| **A2** keyword/string 漂移 | `client.clj:edn->json-compatible` 在 `call-tool` 最晚时刻做 keyword→string；`mcp_bridge.clj:normalize-request` 用 `(:input-schema descriptor)` 对 `args` 做 `m/validate`，而该 schema 来自 `json-schema->malli` 的 string-key 形态（`["temperature" :int]`）。当前测试 `json-schema->malli-string-keyed-java-map` 用 string-key map 测 `m/validate`，输入却是 `{"temperature" 0.7}`（string-key），验证通过；但真实 Agent 走 keyword-key（`{:temperature 0.7}`）时路径不一致。未形成 `canonicalize-json-value → string-key JSON-like EDN → 统一验证` 的固定管线。 | **❌ 仍开放**：晚期转换导致 validate 的 key 域不稳定 |
| **A3** outputSchema 混层 | `mcp_bridge.clj:result->edn` 已正确拆出双通道 envelope `{:value {:mcp/model-content [...] :mcp/structured-content <map>} :audit {...}}`（Layer A 已落地）。但 `execute-request!` 内 refresh 仍 `assoc :input-schema / :output-schema` 共用同一 descriptor 字段，未拆成 `:provider/input-schema` vs `:mcp/input-schema`、`:provider/output-schema` vs `:mcp/output-schema`；也未实现"先用 `:mcp/output-schema` 验 `structuredContent`，再组 envelope，再用 `:provider/output-schema` 验整 envelope"。 | **⚠️ 部分闭合**：数据面已拆，schema 面仍混层 |
| **A4** descriptor refresh TOCTOU | `dispatch/dispatch! → dispatch-registered!` 在 `normalize-request` 前 `proto/describe` 取一次 descriptor；`mcp_bridge.clj:execute-request!` 内又在 `call-tool` 前基于 `refresh-ms` 再次 `reset! descriptor-atom` 更新同一 mutable descriptor，无 `D_normalize = D_authorize = D_execute = D_validate` 的 immutable snapshot。存在`D` 在 effect 已开始后被替换。 | **❌ 仍开放** |

### B. Authority / Trust Boundary

| # | 断言 | 取证 | 判定 |
| --- | ------ | ------ | ------ |
| **B1** capability 粒度只有 tool | `mcp_bridge.clj:normalize-request` 固定 `{:kind :tool :id tool-id}`；`capability/policy.clj` 与 `broker.clj` 只在这层判断。`read_file("/etc/shadow")` 与 `read_file("/workspace/a")` 对 broker 不可区分。 | **❌ 仍开放** |
| **B2** transport secret 进入错误数据 | `kernel/error.clj:sanitize` 通过 `secret-keys` 集合逐键 redaction，但 `transport.clj:stdio-transport` 的 `:env` 是任意环境变量 map（`OPENAI_API_KEY` 等不在集合里）；`mcp_bridge.clj:execute-request!` 每次异常都 `err/sanitize transport-cfg` 并 `{:mcp/transport-config ... :cause ...}` 一起写入 error-data，`call-tool`/`call-tool-managed` 也 `sanitize args`。未做到 `Secret material ≠ serializable config`（`:auth/ref` + host 解析）或至少对 `:env`/`:headers` 做 subtree 级 redaction。 | **⚠️ 部分闭合**：点状 redaction 有，面状 redaction 无 |
| **B3** remote tool 消失后 surface 不收缩 | `execute-request!` 的 refresh 逻辑 `when matching → reset!`，`matching` 为 nil 时无动作；无 `:present→:removed`、`:newly-discovered→:discovered-ungranted` 的正式状态机，stale provider 继续可调用。 | **❌ 仍开放** |

### C. Error / Effect Algebra

| # | 断言 | 取证 | 判定 |
| --- | ------ | ------ | ------ |
| **C1** classifier 把 wrapper 误判为 transient | `client.clj:categorize-error` 按 `class-name` 子串 `(io\|transport\|connect\|socket\|...)` 与 `message` 子串 `(tool\|iserror\|result)` 做 regex。`clojure.lang.ExceptionInfo` 自身就含 `"clojure.lang"` 不命中，但 wrapper 的 message 常含 `isError`/`tool` 或 cause class 含 `java.io.IOException` 时，`ExceptionInfo` 的 `ex-data` 类型被跳过，直接按字符串启发式。测试为躲开它特意用 `java.lang.Error`（`"io"` 子串陷阱的反例即证据）。 | **❌ 仍开放**：typed algebra 要求 `stable :error/type → known SDK class → cause chain`，当前是字符串启发式 |
| **C2** isError=true 被错误提升为 provider failure | `client.clj:call-tool` 在 `is-error` 时 `throw :mcp/tool-error`；`mcp_bridge.clj:execute-request!` catch 后 `if (= :mcp/tool-error ...) (throw ex)`，最终 `dispatch/execute-with-retry!` 把它归为 `:provider/execution-failed` 的 failed 分支。规范要求 `isError=true` 是**工具执行错误**，应作为正常 `CallToolResult` 返回给 LLM（`{:mcp/tool-status :error :mcp/model-content [...]}`），绝不 retry、模型可见。 | **❌ 仍开放** |
| **C3** idempotency 只在 EvoCLJ 内部成立 | `dispatch.clj` 对非 pure 写要求 `:metadata {:idempotency/key ...}`，但该 key 从未传入 MCP；`mcp_bridge` 亦无 effect journal（`proposed→authorized→call-started→committed/rejected/ambiguous`）。`request sent → remote committed → connection breaks` 仍被归为 transient 并可能 retry。 | **❌ 仍开放** |

### D. Connection Ownership / Concurrency

取证锚点：`mcp_bridge.clj:connection-pool`、`provider-refresh-fns`（两 JVM-global Atom）、`pool-get/pool-put!/pool-acquire!/pool-release!/pool-remove!/shutdown-pool!`、`client.clj:open!/ensure-open/reopen!`。

| # | 断言 | 取证 | 判定 |
| --- | ------ | ------ | ------ |
| **D1** refcount 数调用不是 owner | `pool-acquire!` 每次 `execute-request!` 调一次 `update :refcount inc`；`pool-release!` 从未在调用路径被使用（仅 `pool-remove!` 显式 close）。`mcp-provider` 构造时不 acquire，调用时才计数。 | **❌ 仍开放** |
| **D2** connection-id 没绑定 transport identity | pool key 只有 `connection-id`（keyword）。`pool-put!` 虽有`新旧 transport-config 不同 → :mcp/pool-conflict` 检查，但 key 仍是单一 id，未纳入 `normalized transport identity + credential identity` 的 `ConnectionKey`。 | **❌ 仍开放** |
| **D3** pool-acquire stale-read lost update | `pool-acquire!` / `pool-put!` / `pool-release!` 均为 `let [entry (get @pool id)]` 在 `swap!` 外读旧快照，再 `swap! assoc (update entry ...)` 写回；`Atom.swap` 的 CAS 只保证写入原子，不保证 read-modify-write 语义。并发 acquire 会互相覆盖。 | **❌ 仍开放** |
| **D4** 首次连接 double-open race | `execute-request!` 中 `or (get (pool-acquire! id) :managed) (let [m (open! cfg)] (pool-put! id m) m)` 无 single-flight；两线程同见 empty，各自 `open!`，后者覆盖前者，前者连接泄漏。 | **❌ 仍开放** |
| **D5** dead-but-open 不会真正愈合 | 健康判断仅 `(:closed? managed)`；`ensure-open` 只在 `closed?` 时 `reopen!`，`reopen!` 仅重试 `open!`。socket/channel 已死但 `:closed? false` 时走 `call-tool` 直接抛异常，未进入 `broken→close old→reconnecting→ready(new generation)`。 | **❌ 仍开放** |
| **D6** pool/refresh registry JVM-global，无 host owner | `connection-pool` 与 `provider-refresh-fns` 为 `def ^:private atom {}`；`shutdown-pool!` 存在但从未被 `runtime.system`/`kernel.system` 的 `halt-key!` 调用；`provider-registry` 是 host-owned，但 MCP 侧资源不在同一生命周期。 | **❌ 仍开放** |

### E. Control / Feedback Plane

| # | 断言 | 取证 | 判定 |
| --- | ------ | ------ | ------ |
| **E1** toolsChanged/progress 没接入 runtime | `client.clj:build-client` 与 `open!` 已支持 `tools-change-consumer`/`progress-consumer`，但 `mcp_bridge.clj:open!` 调用永远 `(mcp-client/open! transport-cfg)` 无参，consumer 悬空。 | **❌ 仍开放** |
| **E2** refresh-provider! 只是 invalidate | `build-refresh-fn` 仅 `(assoc @descriptor-atom :mcp/last-refreshed nil)`；`refresh-provider!`/`refresh-all-mcp-providers!` 亦只清时间戳。有 `list-all-tools` 的能力，但无 `refresh-schema-now!`（同步 list→validate→publish）。 | **❌ 仍开放** |
| **E3** refresh failure 被静默吞掉 | `execute-request!` 内 `try (list-all-tools ...) (catch Throwable _ nil)`，失败后继续 `call-tool` 用 stale descriptor，无 `freshness policy`（`:required | :best-effort | :pinned`）门控。 | **❌ 仍开放** |
| **E4** tool removal 无正式状态转移 | 见 B3：无 `:removed` / `:discovered-ungranted` 的发布与订阅；control 变化与 authority 后果混在同一个 `when matching` 分支。 | **❌ 仍开放** |

### F. Observability / API Truthfulness

| # | 断言 | 取证 | 判定 |
| --- | ------ | ------ | ------ |
| **F1** raw-size-bytes 不是 bytes | `client.clj:call-tool` 中 `raw-size (long (reduce + 0 (map #(.length (str %)) content-block-maps)))`，对 `{:content/type :text :content/text "中文"}` 计的是 `pr-str` 后的 char 数，不是 UTF-8 wire bytes。`"中"` 1 char ≠ 3 bytes 反例成立。 | **❌ 仍开放** |
| **F2** managed counters 没形成持久状态 | `call-tool-managed` 里 `updated (assoc managed :call-count ... :last-latency-ms ...)` 只随 result 返回 `{:mcp/call-count ...}`，从未写回 `managed` record 也未写回 `connection-pool` entry；`pool-put`/`pool-acquire` 的 entry 也从不更新 metrics。 | **❌ 仍开放** |
| **F3** call-tool-streaming 并非真正 streaming | `call-tool-streaming` 是 `reify IReduceInit → (call-tool client tool args) → reduce blocks`，完整 call 返回后才逐块 reduce，无 protocol-level streaming，也未接入 2026 progress/MRTR。 | **❌ 仍开放**（命名承诺 > 实际语义） |

### G. Protocol Generation Boundary

| # | 断言 | 取证 | 判定 |
| --- | ------ | ------ | ------ |
| **G1** client architecture 固化了 initialize/sessionful | `client.clj` 固定 `McpClient/sync → .toolsChangeConsumer/.progressConsumer → .build → .initialize`，未隔离为 `ProtocolAdapter`。当前行为与 pinned 2.0.0 一致，无错，但形状上 MCP 世代与 kernel 语义未解耦。 | **❌ 仍开放**（结构性） |
| **G2** 尚无 2026 cache/subscriptions/freshness 模型 | 无 `tools/list` cacheable + `ttlMs/cacheScope`、无 `subscriptions/listen`、无 generation-aware catalog。 | **❌ 仍开放** |
| **G3** 尚无 MRTR/tasks/per-request metadata 的版本化适配层 | 无 `per-request _meta`、无 Tasks/MRTR 长调用续跑、无 `input/outputSchema` 的 2020-12 全量路径与 budget。 | **❌ 仍开放** |

### 小结：26 项里真正已闭合的

- **无** 完全闭合的项。
- **部分闭合 3 项**：A1（不再 :any，已 fail-closed，但 validator 仍不完整）、A3（双通道 envelope 已落地，schema 层仍混）、B2（点状 redaction 有，面状无）。
- **其余 23 项仍开放**。与 2026-08-19 design 所指的 Layers 1– 已闭合边界一致：data-plane 的 content sandboxing、EDN-safe、audit 显式化已做好，剩下的恰是 control-plane / ownership / algebra / protocol adapter。

> 判定依据：只要某项仍需"靠代码纪律维持"而非"由形状保证"，即判开放。以上开放项均满足该标准。

---

## 2. 四个一级根因归并（与核验对齐）

```
Ⅰ. Boundary conflation        — 一个字段/对象承载 ≥2 层语义
     → A1, A2, A3, B2, C2, F1, F3
Ⅱ. Mutable control state 无版本化 — 动态 descriptor/catalog/connection 随时变，但 effect 未持有 immutable generation
     → A4, B3, E1–E4, C3
Ⅲ. Resource ownership 非一等概念 — global atom 代理 manager/state machine
     → D1–D6, F2
Ⅳ. Protocol semantics 与 kernel semantics 未解耦 — 一代 wire model 渗入 provider/kernel
     → G1–G3, 以及 A1/C2/C3/F3 中与协议世代绑定的部分
```

此归并与根问题陈述一致；核验未发现需要推翻它的反例。

---

## 3. 修复规划 — 六步重构（顺序即依赖）

> 原则：每步都是"形状先行"，先让错误状态不可表示，再搬逻辑。每步独立可测、可回滚；前一步不堵后一步。

### Step 1 — 引入 `CallContract` / `DescriptorGeneration`（根因Ⅱ）

**目标**：`D_normalize = D_authorize = D_execute = D_validate` 由类型保证；refresh 不在 effect 开始后发生。

- 新增 `evoclj.mcp.contract`：

  ```clojure
  {:contract/id            uuid
   :contract/descriptor    <snapshot>     ; 含 generation :contract/gen n, :contract/captured-at
   :contract/normalized    <request>
   :contract/decision      <broker decision>
   :contract/freshness     :required | :best-effort | :pinned
   :contract/stale?        bool            ; best-effort 时注明 generation 已 stale
  }
  ```

- `dispatch.clj` pipeline 在 `normalize-request!` 后立即冻结 `descriptor@generation`，后续 `authorize`/`execute-with-retry!`/`validate-output!` 均消费同一 snapshot；`mcp_bridge` 内联 refresh（`when refresh-ms ... reset!`）移至 pipeline 的 `refresh-if-needed` 阶段（在 normalize 之前），effect 开始后禁止再 `reset! descriptor-atom`。
- `dispatch.clj` 记录 `audit` 时写入 `generation`，`:best-effort + stale` 必须在 audit 中显式注明。
- **闭合**：A4、E3（的一半：refresh 时机）、B3/E4 的前置条件；为 Step 3/4 的 manager generation 打桩。
- **变更面**：`mcp.contract`（新）、`intent.dispatch`、`provider.mcp-bridge`（删内联 refresh）、`provider.registry`（descriptor 携带 `:mcp/generation`）。
- **测试**：并发调用中 `describe` 在 `execute-request!` 内被 refresh 时，`CallContract` 仍校验旧 generation，不漂移；`:required`  freshness 下 refresh 失败即 `:provider/freshness-required` 拒绝 effect。

### Step 2 — 拆远程 schema / 本地 schema / wire 表示三层（根因Ⅰ）

**目标**：`canonical JSON Schema` 为 source of truth，Malli 仅为已证明等价的 fast path；key 域统一为 string-key JSON-like EDN。

- Descriptor 拆分：

  ```clojure
  :provider/input-schema   ; 验整 envelope / Agent-facing
  :provider/output-schema
  :mcp/input-schema        ; 验 structuredContent / remote
  :mcp/output-schema
  :mcp/input-schema-json   ; 保留原始 string-key JSON Schema（当 fallback 时）
  :mcp/output-schema-json
  :mcp/schema-source       :malli | :json-schema-fallback
  ```

- 表示管线固定为：

  ```
  Agent args (keyword) → canonicalize-json-value → string-key JSON-like EDN
    → local Malli validate (:provider/input-schema)
    → remote JSON Schema validate (:mcp/input-schema, 2020-12 validator)
    → MCP serialization
  ```

  新增 `evoclj.mcp.json/value->canonical`（keyword→string, 保留 string-key 透传），删除 `edn->json-compatible` 的晚期转换。
- 引入真正的 2020-12 validator（选 `networknt/json-schema-validator` 或等效，pinned version）：作为安全性 source of truth，支持 `oneOf/anyOf/allOf/$ref/pattern/if-then-else` 等；Malli 编译仅在已证明等价的"常见 MCP 子集"走 fast path，其余走 fallback path（当前 `wrap-json-schema-validator` 保留但由新 validator 驱动）。对 `$ref` 禁止未经策略允许的外部网络解析；对递归 schema 设 depth/node-count/validation-time budget；regex 走 `RE2` 或超时预算。
- **闭合**：A1、A2、A3。
- **变更面**：`mcp.json-schema`（或新增 `mcp.json-validator`）、`mcp.canonical`、`mcp_bridge`（json-schema->malli 分流逻辑）、`client.java-schema->clj`（已正确 output string-key，保留）。
- **测试**：`oneOf`/`$ref` schema 拒绝注册或 fallback 正确拒绝；keyword vs string 输入在不同 generation 下行为一致；`structuredContent` 走 `:mcp/output-schema` 验，整 envelope 走 `:provider/output-schema` 验。

### Step 3 — 引入 host-owned `McpManager`（根因Ⅲ）

**目标**：`connection-pool` + `provider-refresh-fns` + `notification-router` + `metrics` + `health` 统一为 Integrant component 的状态机，不再是 JVM-global Atom。

- 新增 `evoclj.mcp.manager`（Integrant key `:mcp/manager`）持有：

  ```clojure
  {ConnectionKey {:state :connecting | :ready | :broken | :closing | :reconnecting
                 :client <managed>
                 :owners #{provider-id ...}
                 :health {:last-ok <inst> :last-error <sanitized> :failures n}
                 :generation n
                 :metrics {:call-count n :latency-ms n}
                 :transport-identity <normalized>  ; 不含 secret
                 :credential-identity <fingerprint>}} ; :auth/ref 的 hash，非 secret
  ```

  `ConnectionKey = [protocol-version connection-id normalized-transport-identity credential-identity]`（至少含后三者）。
- 并发语义：
  - `pool-{get,acquire,release,put}` 重写为单 `swap!` 内 `update` 的纯函数（无"先 get 再 swap! 旧 entry"），用 `clojure.core/swap!` 的重读-重算保证 lost-update 消除。
  - 首次建立 single-flight：`absent → atomic install promise → :connecting → :ready/:broken`，其他线程共享同一 promise，不各自 `open!`。
  - dead 愈合：`ready --transport failure--> broken --close old--> reconnecting --> ready(new generation)`，`generation` 递增，旧 `CallContract` 自动 stale。
  - refcount 按 owner：`mcp-provider` 构造时 `manager/acquire owner-id`，`dispose!` 时 `release`；调用路径不再改 refcount。
- Host lifecycle：`kernel.system` / `runtime.system` 的 `halt-key!` 调用 `manager/shutdown!` 关闭所有 entry；`provider-registry` 与 `mcp/manager` 同生命周期。
- Secret 面：`transport-config` 进入 manager 前归一化为 `{:auth/ref :secret/... :config <redacted>}`，错误/audit/descriptor 永远看不到 resolved secret；`:env`/`:headers` 做 subtree redaction。
- **闭合**：D1–D6、B2（剩余）、B3（与 Step 4 共同）、F2。
- **变更面**：`mcp.manager`（新）、`mcp.client`（`ensure-open` 接受 manager health）、`mcp_bridge`（删 global atoms，改注入 manager）、`provider.protocol`（新增 `dispose!` 可选方法或独立 `Closeable`）、`runtime.system`/`kernel.system`（新增 `:mcp/manager` component）。
- **测试**：并发 50× `pool-acquire!` 不丢更新；并发 2× 首次 open 仅一次 `open!`；kill 底层 socket 后下一次调用触发 `broken→reconnecting→ready` 且 generation 递增；`halt!` 后无残留线程/连接；`connection-id` 相同但 transport 不同的 provider 互相隔离。

### Step 4 — 重做 error / effect algebra（根因Ⅰ+Ⅱ交叉）

**目标**：`Transport failure ≠ Protocol failure ≠ Tool execution failure ≠ Ambiguous`，typed algebra 替代字符串启发式。

- `client.clj:categorize-error` 重写为：

  ```
  stable :error/type（已是 :mcp/... 的直接保留）
    → known SDK exception class（McpClientException / IOException / JsonParseException 等白名单）
    → recursive cause chain（cause 的 :error/type 上浮）
  ```

  删除全部按 `class-name`/`message` 子串的 regex。
- `call-tool` 对 `isError=true` **不再 throw**，返回正常 result：

  ```clojure
  {:mcp/tool-status :error | :ok
   :mcp/model-content [...]
   :mcp/structured-content ...
   :mcp/is-error true}
  ```

  `mcp_bridge:result->edn` 透传 `:mcp/tool-status`；`dispatch` 与 `call-tool-managed` 将 `tool-status :error` 视为**非 retry、模型可见**的正常结果（`{:result/status :ok :value {:mcp/tool-status :error ...}}`），仅 `Transport/Protocol` 才映射为 `:provider/transient-error`。
- effect journal（轻量，不引入 DB）：

  ```
  :effect/proposed → :effect/authorized → :effect/call-started
    → :effect/committed | :effect/rejected | :effect/ambiguous
  ```

  `call-started` 写入时记录 `idempotency/key` 与 `generation`；`request sent + remote committed + connection breaks` 时判 `ambiguous`，绝不伪装"没执行"。有 remote idempotency 能力的 provider 显式映射，无则保持 `ambiguous` 并通过 `reconcile/status` tool 恢复。
- `F1` 一并修复：`raw-size-bytes` 改为对**序列化后 wire body**（`Jackson` 序列化结果的 `.length`）取 bytes，或改名为 `estimated-rendered-chars` 并修正语义。
- **闭合**：C1、C2、C3、F1。
- **变更面**：`mcp.client`（categorize + isError 语义）、`provider.mcp-bridge`（错误映射 + tool-error 直通）、`intent.dispatch`（transient 判定 + effect journal 状态）、`kernel.error`（新增 `:effect/ambiguous` 相关 type）。
- **测试**：`ExceptionInfo` wrapper 不再被误判为 transient；`isError=true` 返回 ok 且不 retry，模型可见；`remote committed + break` 判 ambiguous 且不自动重放非幂等调用。

### Step 5 — `CanonicalResource` 下沉到对象级（根因Ⅰ，authority 特化）

**目标**：`may invoke tool? ∧ may access canonical resource?` 的两层 lease。

- 新增 `evoclj.mcp.canonical`（或复用 `capability.lease/resource-covers?` 的扩展）：

  ```clojure
  {:kind :filesystem/path :path "/workspace/a.clj" :action :read}
  {:kind :database/table  :server :prod-ro :table "users" :action :select}
  {:kind :tool            :id :mcp/foo} ; 仍保留 tool 层
  ```

- `mcp_bridge:normalize-request` 增加 `canonical-resource` 职责：按 tool 语义把 `args` 解析为 canonical resource（如 `read_file` 的 `path` 归一化 + 越权检查），返回 `{:tool/id ... :resource <canonical> :args <canonical string-key>}`；`capability.policy/decide` 与 `broker/authorize` 消费该 resource 做第二层判断。
- 保留第一层 tool lease，向后兼容：无细粒度 policy 的部署仍仅校验 tool 层；有 policy 的部署在 tool 层之上叠加 object 层。
- **闭合**：B1；为 B3 的 `removed` 语义提供精确的 authority 收缩点。
- **变更面**：`mcp.canonical`（新）、`mcp_bridge`（normalize-request）、`capability.lease`/`policy`（resource 形状扩展）、`provider.registry`（descriptor 可声明 supported resource kinds）。
- **测试**：`read_file` 越权（`/etc/shadow`）被 `:capability/scope-denied` 拒绝，即使同 tool lease 存在；归一化 `a/../secret` 后按 canonical path 判定。

### Step 6 — 版本化 `ProtocolAdapter`（根因Ⅳ）

**目标**：`McpProvider` 位于 adapter 之上；2025 vs 2026 的差异只在 adapter 内。

- 新增 `evoclj.mcp.adapter` protocol：

  ```clojure
  (defprotocol ProtocolAdapter
    (discover [this ctx])          ; list + normalize
    (wire-request [this contract]) ; per-request _meta / headers / session
    (on-notification [this event]) ; toolsChanged/progress/subscriptions
    (cache-policy [this])          ; ttlMs/cacheScope 等
    (continue [this task]))        ; MRTR/Tasks 续跑（2026 侧）
  ```

  实现：
  - `MCP-2025-11 Adapter` — `McpSyncClient + initialize`、sessionful、`Mcp-Session-Id`、`progress` callback。
  - `MCP-2026-07 Adapter` — stateless、per-request `_meta`、cacheable `tools/list`、subscriptions/listen、full 2020-12 schema、MRTR/Tasks。
- `McpProvider` / `Capability` / `audit` / `CanonicalResource` 对 adapter 无感；wire 细节（discovery、header、notification、cache、continuation）收敛到 adapter。
- `G2/G3` 的 cache/subscriptions/MRTR 在此步以 adapter 内部实现落地；legacy 适配器保持现有行为不变。
- `call-tool-streaming` 在此步二选一：改名为 `reduce-content-blocks`（承认非 streaming），或接入 adapter 的 async/reactive channel 使 progress 到达即被消费。
- **闭合**：G1、G2、G3、F3；彻底解除"一代 wire model 渗入 kernel"的耦合。
- **变更面**：`mcp.adapter`（新）、`mcp.client`（拆分为 `client_2025` / `client_2026` 两个实现）、`mcp.manager`（按 adapter 选连接策略）、`mcp_bridge`（消费 adapter 而非直接 client）。
- **测试**：同一 `McpProvider` 在 `2025` 与 `2026` adapter 下行为等价（除 wire 差异）；`tools/list` cache 命中/失效、`list_changed` 订阅、`MRTR` 长调用在 2026 adapter 下可测。

---

## 4. 执行顺序与依赖图

```
Step 1 CallContract ─┬─→ Step 2 Schema layering ─→ Step 4 Error algebra ─→ Step 5 CanonicalResource
                     │                                         ↑
                     └─→ Step 3 McpManager ────────────────────┘
                                                              │
                                                     Step 6 ProtocolAdapter (汇聚)
```

- **并行度**：Step 1 与 Step 3 的设计可并行，但实现上 Step 3 依赖 Step 1 的 generation 概念，建议串行 1→3。
- **为什么 C 晚于 A/B**：`isError` 的正确语义（C2）需要 A3 的双通道 envelope 与 B1 的资源已就位，否则"模型可见错误"的形状会反复改。

---

## 5. 逐项闭合映射（26 → 6）

| 步骤 | 直接闭合 | 间接消除 |
| ------ | ---------- | ---------- |
| 1 CallContract | A4, E2(一半), E3(一半) | 为 D5/E4 的 generation 失效打桩 |
| 2 Schema layering | A1, A2, A3 | C2/F3 的 schema 相关前置 |
| 3 McpManager | D1–D6, F2, B2(剩余), B3(半) | E1/E2/E3 的宿主 |
| 4 Error algebra | C1, C2, C3, F1 | B3/E4 的最终拒绝语义、A3 的 envelope 校验 |
| 5 CanonicalResource | B1, B3(完) | E4 的 authority 收缩精确化 |
| 6 ProtocolAdapter | G1, G2, G3, F3 | A1/C2/C3/F3 的世代演进收敛 |

全部六步完成后，26 项成批消失：D 族被 manager 一次解决，A2–A4 被 CallContract 解决，E 族被 generation-aware catalog 解决，C 族由新 algebra 统一。

---

## 6. 关键类型形状（四个一等对象）

```clojure
;; 1. CallContract — immutable，一次调用一份
{:contract/id          #uuid "..."
 :contract/generation  7
 :contract/descriptor  <snapshot>
 :contract/normalized  {:tool/id ... :resource <canonical> :args <string-key>}
 :contract/decision    {:decision :allow :lease-id ...}
 :contract/freshness   :required | :best-effort | :pinned
 :contract/stale?      false}

;; 2. McpManager — Integrant component，唯一 owner
{:manager/pools {ConnectionKey {:state :ready :client _ :owners #{...}
                               :generation 7 :health _ :metrics _}}
 :manager/registry <provider-refresh-fns>
 :manager/router  <notification-router>}

;; 3. ProtocolAdapter — 版本化适配层
{:adapter/version :mcp/2025-11 | :mcp/2026-07
 :adapter/client  <McpSyncClient | stateless-http-client>
 :adapter/cache   {:ttl-ms ... :scope ...}}

;; 4. CanonicalResource — authority 的第二层
{:kind :filesystem/path :path "/workspace/a.clj" :action :read}
```

---

## 7. 里程碑与验收

| 里程碑 | 验收（摘） |
| -------- | ------------ |
| M1 Contract | A4 并发测试不漂移；`:required` freshness 下 stale 拒绝；audit 含 generation |
| M2 Schema | `oneOf/$ref/pattern` 等不再 :any；keyword/string 统一；`structuredContent` 走 `:mcp/output-schema` |
| M3 Manager | 50 并发 acquire 无 lost update；double-open 仅一次 open!；dead→reconnecting 愈合；`halt!` 无泄漏；`connection-id` 同名异 transport 隔离 |
| M4 Algebra | wrapper 不误判 transient；`isError=true` 为 ok 且不 retry；`ambiguous` 不伪装成功 |
| M5 Canonical | `read_file` 越权被 `scope-denied`，归一化后判定 |
| M6 Adapter | 2025 与 2026 adapter 行为等价（wire 除外）；cache/subscriptions/MRTR 在 2026 侧可测；`call-tool-streaming` 语义诚实 |

每个里程碑前置：`malli` schema、红-绿单测、`mcp-dispatch-test`（sequential-thinking 真服）保持绿色。

---

## 8. 风险与取舍

- **A1 引入真 validator 的依赖与预算**：选 `networknt/json-schema-validator`（纯 Java，无额外网络），pin 版本；为 `pattern`/`$ref` 设硬预算，默认拒绝 external `$ref`。
- **B2 的 secret 分层是 breaking**：`transport-config` 从"含 secret 的 map"变为"含 `:auth/ref` 的 map"，需一次性的 config 迁移与 `sanitize` 的 subtree 兜底。
- **C2 的 isError 语义是 breaking**：调用方原先 `catch :mcp/tool-error`，改为检查 `:mcp/tool-status :error`。需 codemod + 兼容期（保留旧 throw 仅告警）。
- **D 重写需原子性**：所有 pool 操作改为 `swap!` 内的纯 `update`，辅以 `single-flight promise`，避免"先 get 再 assoc 旧 entry"。
- **G 不抢跑**：M6 前不碰 `mcp:2.0.0` 以外的 SDK，保持 legacy 兼容；2026 适配器以新增代码存在，不改现有 `client.clj` 的默认路径。

---

## 9. 下一步（待用户确认后启动）

1. 确认本规划的 Step 顺序与 breaking 变更容忍度（尤其 B2/C2）。
2. 按 Step 1→6 逐层开工：每层一个 coder subagent + reviewer subagent + coordinator commit，测试保持绿色才合入。
3. 每层产出 `specs/<step>-plan.md`（含 Malli 形状与单测清单），避免跨层蔓延。

---

*本文件为规划与核验，不含代码变更。*

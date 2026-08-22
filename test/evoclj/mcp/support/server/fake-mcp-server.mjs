#!/usr/bin/env node
/**
 * fake-mcp-server.mjs — programmable zero-dependency MCP stdio server (WO-T1).
 *
 * Node >= 20, Node standard library only (node:readline). Speaks
 * newline-delimited JSON-RPC 2.0 over stdin/stdout, just enough MCP for
 * evoclj.mcp.client/open! to complete a handshake and drive tools/list /
 * tools/call, with fault-injection knobs for the EvoCLJ test harness.
 *
 * Knobs (environment variable per WO-T1; equivalent CLI flag wins when both
 * are present — CLI flags exist because the production Java SDK transport
 * launches subprocesses and argv is the most robust way to reach it):
 *
 *   FAKE_MODE        ok (default) | slow | malformed | huge | many-pages |
 *                    infinite-cursor | crash-after-init | no-response
 *   FAKE_DELAY_MS    artificial latency applied to every response
 *                    (slow / no-response scenarios), default 0
 *   FAKE_TOOL_COUNT  number of tools generated (huge/many-pages/ok),
 *                    default 50; huge interprets it as KB of description text
 *   FAKE_PAGE_SIZE   page size for many-pages pagination, default 10
 *
 * Behaviour:
 *   initialize                 -> protocolVersion echoes the requested value
 *   notifications/initialized  -> ignored (crash-after-init exits here, code 1)
 *   ping                       -> {}
 *   tools/list   ok            -> single page, FAKE_TOOL_COUNT tools
 *                many-pages    -> pages of FAKE_PAGE_SIZE with nextCursor,
 *                                 last page terminates
 *                infinite-cursor -> every page carries a fresh non-empty
 *                                 nextCursor (never terminates; callers MUST
 *                                 NOT drive unbounded production loops)
 *                huge          -> one tool whose description embeds
 *                                 FAKE_TOOL_COUNT KB of text
 *                malformed     -> raw invalid JSON line (typed protocol error
 *                                 downstream)
 *   tools/call                 -> echo {content:[{type:"text",
 *                                 text:<JSON of arguments>}]}
 *                no-response   -> never answers tools/call, never exits
 *                                 (M15 ambiguous-timeout scenario seed)
 */

import { createInterface } from "node:readline";

const argv = process.argv.slice(2);

function argValue(flag) {
  const i = argv.indexOf(flag);
  return i >= 0 && i + 1 < argv.length ? argv[i + 1] : undefined;
}

function envOr(name, fallback) {
  const v = process.env[name];
  return v === undefined || v === "" ? fallback : v;
}

function numOr(raw, fallback) {
  if (raw === undefined || raw === null || raw === "") return fallback;
  const n = Number(raw);
  return Number.isFinite(n) ? n : fallback;
}

const MODE = String(argValue("--mode") ?? envOr("FAKE_MODE", "ok"));
const DELAY_MS = Math.max(0, numOr(argValue("--delay-ms") ?? envOr("FAKE_DELAY_MS"), 0));
const TOOL_COUNT = Math.max(0, numOr(argValue("--tool-count") ?? envOr("FAKE_TOOL_COUNT"), 50));
const PAGE_SIZE = Math.max(1, numOr(argValue("--page-size") ?? envOr("FAKE_PAGE_SIZE"), 10));

const KNOWN_MODES = new Set([
  "ok", "slow", "malformed", "huge", "many-pages",
  "infinite-cursor", "crash-after-init", "no-response",
]);
if (!KNOWN_MODES.has(MODE)) {
  process.stderr.write(
    `[fake-mcp-server] unknown mode "${MODE}", behaving as "ok"\n`);
}

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function maybeDelay() {
  if (DELAY_MS > 0) await sleep(DELAY_MS);
}

function writeMessage(obj) {
  process.stdout.write(JSON.stringify(obj) + "\n");
}

function respondResult(id, result) {
  writeMessage({ jsonrpc: "2.0", id, result });
}

function respondError(id, code, message) {
  writeMessage({ jsonrpc: "2.0", id, error: { code, message } });
}

function makeTool(name, description) {
  return {
    name,
    description: description ?? `fake tool ${name}`,
    inputSchema: { type: "object", properties: {}, required: [] },
  };
}

function allTools() {
  const tools = [];
  for (let i = 0; i < TOOL_COUNT; i++) {
    tools.push(makeTool(`fake-tool-${i}`));
  }
  return tools;
}

/** huge: exactly one tool whose description carries TOOL_COUNT KB of text. */
function hugeTools() {
  const kb = TOOL_COUNT;
  return [
    makeTool("huge-tool", `huge-description:${kb}KB:${"A".repeat(kb * 1024)}`),
  ];
}

let infinitePageCounter = 0;

/** Cursor format for many-pages: "p<n>" = skip n pages. */
function parsePageOffset(cursor) {
  const m = /^p(\d+)$/.exec(String(cursor ?? ""));
  return m ? Number(m[1]) * PAGE_SIZE : 0;
}

function toolsListResult(cursor) {
  switch (MODE) {
    case "huge":
      return { tools: hugeTools() };

    case "many-pages": {
      const tools = allTools();
      const offset = parsePageOffset(cursor);
      const page = tools.slice(offset, offset + PAGE_SIZE);
      const result = { tools: page };
      if (offset + PAGE_SIZE < tools.length) {
        result.nextCursor =
          `p${(offset + PAGE_SIZE) / PAGE_SIZE}`;
      }
      return result;
    }

    case "infinite-cursor": {
      infinitePageCounter += 1;
      return {
        tools: allTools().slice(0, PAGE_SIZE),
        nextCursor: `inf-${infinitePageCounter}`,
      };
    }

    default: // ok | slow | malformed(handled by caller) | crash-after-init |
              // no-response
      return { tools: allTools() };
  }
}

async function handleMessage(msg) {
  if (!msg || typeof msg !== "object") return;
  const { id, method, params } = msg;

  // Notifications carry no id and are never answered.
  if (id === undefined || id === null) {
    if (method === "notifications/initialized" &&
        MODE === "crash-after-init") {
      // Initialization (handshake) just completed: die visibly, exit code 1.
      // 250ms grace so the SDK's initialize() call has deterministically
      // returned before the process dies (local pipe round-trip is ~1ms;
      // a 30ms grace was racy under load).
      setTimeout(() => process.exit(1), 250);
    }
    return;
  }

  switch (method) {
    case "initialize": {
      await maybeDelay();
      const requested = params && params.protocolVersion
        ? params.protocolVersion
        : "2025-06-18";
      respondResult(id, {
        protocolVersion: requested, // echo the requested value verbatim
        capabilities: { tools: { listChanged: false } },
        serverInfo: { name: "fake-mcp-server", version: "1.0.0" },
        instructions: "programmable fake MCP server (WO-T1 test harness)",
      });
      return;
    }

    case "ping": {
      await maybeDelay();
      respondResult(id, {});
      return;
    }

    case "tools/list": {
      if (MODE === "malformed") {
        await maybeDelay();
        // Deliberately invalid JSON on the wire (unterminated object):
        // downstream parsers must surface a typed protocol error.
        process.stdout.write(`{"jsonrpc":"2.0","id":${JSON.stringify(id)},"resu`);
        process.stdout.write("\n");
        return;
      }
      const result = toolsListResult(params ? params.cursor : undefined);
      respondResult(id, result);
      return;
    }

    case "tools/call": {
      if (MODE === "no-response") {
        // Silence forever, stay alive (M15 ambiguous scenario).
        return;
      }
      await maybeDelay();
      const args = params && params.arguments !== undefined
        ? params.arguments
        : {};
      respondResult(id, {
        content: [{ type: "text", text: JSON.stringify(args) }],
        isError: false,
      });
      return;
    }

    case "prompts/list": {
      respondResult(id, { prompts: [] });
      return;
    }

    case "resources/list": {
      respondResult(id, { resources: [] });
      return;
    }

    default:
      respondError(id, -32601,
        `method not supported by fake server: ${String(method)}`);
  }
}

const rl = createInterface({ input: process.stdin, terminal: false });

rl.on("line", (line) => {
  const trimmed = line.trim();
  if (!trimmed) return;
  let msg;
  try {
    msg = JSON.parse(trimmed);
  } catch {
    // Unparseable inbound frame: answer with a JSON-RPC parse error.
    writeMessage({
      jsonrpc: "2.0", id: null,
      error: { code: -32700, message: "Parse error" },
    });
    return;
  }
  Promise.resolve(handleMessage(msg)).catch((err) => {
    process.stderr.write(
      `[fake-mcp-server] handler error: ${err && err.message}\n`);
    if (msg && msg.id !== undefined && msg.id !== null) {
      respondError(msg.id, -32603, "Internal error");
    }
  });
});

rl.on("close", () => process.exit(0));
process.on("SIGTERM", () => process.exit(0));
process.on("SIGINT", () => process.exit(0));

process.stderr.write(
  `[fake-mcp-server] ready mode=${MODE} delayMs=${DELAY_MS} ` +
  `toolCount=${TOOL_COUNT} pageSize=${PAGE_SIZE}\n`);

# Daemon Architecture

This document explains how `@unrdf/daemon` is structured, why each layer exists, and what trade-offs shaped the design.

---

## What the daemon is

A daemon is a **background service** that:

1. Runs continuously without blocking the main application
2. Schedules work based on triggers (time, events, conditions)
3. Executes operations at appropriate times
4. Reports status through events and metrics
5. Handles failures with retries and graceful degradation

The contrast with the traditional approach:

```
Without daemon                    With daemon
──────────────────────────────    ──────────────────────────────────
Main Application                  Main Application    Daemon
  ├── Process data       ───→         (fast, focused)  ├── Process data
  ├── Send emails                                      ├── Send emails
  ├── Clean cache                                      ├── Clean cache
  └── Generate reports                                 └── Generate reports
```

---

## 5-Layer Architecture

```
┌────────────────────────────────────────┐
│ Application Layer                      │
│ (your code: schedule, execute, listen) │
└────────────────────────────────────────┘
                    ↓
┌────────────────────────────────────────┐
│ MCP Server Layer                       │
│ • JSON-RPC interface (36 tools)        │
│ • withMcpSpan() OTel instrumentation   │
│ • API key authentication               │
│ • Injection detection                  │
└────────────────────────────────────────┘
                    ↓
┌────────────────────────────────────────┐
│ Daemon Core                            │
│ • EventEmitter lifecycle               │
│ • Operation queue (LRU cache)          │
│ • Concurrency control (maxConcurrent)  │
│ • Health and metrics tracking          │
└────────────────────────────────────────┘
                    ↓
┌────────────────────────────────────────┐
│ Trigger Evaluation                     │
│ • Cron expression parsing              │
│ • Interval calculations                │
│ • Event matching (reactive, idle)      │
└────────────────────────────────────────┘
                    ↓
┌────────────────────────────────────────┐
│ Integration Modules (13 modules)       │
│ • Distributed / Raft consensus         │
│ • Federation query execution           │
│ • Hook scheduling and policy           │
│ • KGC-4D event sourcing               │
│ • OTel SDK, tracer, context            │
│ • Merkle receipt generation            │
│ • Streaming / change feeds             │
└────────────────────────────────────────┘
```

---

## The MCP Server Layer

The daemon exposes its functionality over a **Model Context Protocol (MCP) JSON-RPC interface**. This allows external clients — including LLM agents — to call daemon operations as structured tool invocations.

Each of the 36 tools maps to a daemon capability:

- **Context tools** — named-graph management
- **Graph tools** — RDF storage and SPARQL queries
- **Hooks tools** — policy-based rule execution
- **Daemon tools** — lifecycle and scheduling control
- **Template tools** — RDF pattern matching and generation

Every tool is wrapped with `withMcpSpan()`:

```javascript
// From src/mcp/otel-instrumentation.mjs
export function withMcpSpan(toolName, handler) {
  return async args => {
    return withSpan(tracer, `mcp.tool.${toolName}`, async span => {
      span.setAttributes({
        'mcp.tool.name': toolName,
        'mcp.tool.args': JSON.stringify(args),
        'mcp.server.name': 'unrdf-daemon-mcp',
      });
      const result = await handler(args);
      span.setAttributes({
        'mcp.tool.success': true,
        'mcp.tool.result_size': JSON.stringify(result).length,
      });
      return result;
    });
  };
}
```

This gives every tool call a distributed trace span with no additional work from the tool author.

---

## The OTel Instrumentation Layer

OTel tracing is gated by `OTEL_ENABLED`. When false, `withMcpSpan()` is a no-op. When true, the SDK initializes on daemon startup (`otel-sdk.mjs`) and spans are exported to the configured OTLP endpoint.

The tracer is named `unrdf-daemon-mcp`. Spans flow through:

```
Tool call → withMcpSpan() → withSpan() (otel-tracer.mjs)
         → OTEL SDK → BatchSpanProcessor
         → OTLP gRPC exporter → collector
```

W3C trace context propagation (`otel-context.mjs`) allows spans to connect across the daemon to sidecar boundary, enabling end-to-end traces that span process boundaries.

---

## The Security Layer

Security is enforced at two points:

**At the MCP boundary**: API key authentication uses BLAKE3 (256-bit) with constant-time comparison to prevent timing attacks. In `production` mode, missing or invalid keys block all operations. In `development` mode, validation is relaxed to a warning.

**At the operation boundary**: Every operation input is scanned for injection patterns (SQL, SPARQL, command, path traversal) before the handler is called. Error messages are sanitized on the way out to prevent credential leakage.

```
Incoming request
  → API key check (constant-time BLAKE3)
  → Injection scan (SQL, SPARQL, command, path)
  → Operation handler
  → Error sanitization
  → Response
```

---

## Core Concepts

### Operations

An operation is a named, repeatable unit of work:

```javascript
{
  id: 'backup-database',         // unique key
  name: 'Daily Database Backup', // display name
  handler: async () => { ... },  // the work
  metadata: { /* context */ }    // optional labels
}
```

Operations must be **idempotent** — running them twice should produce the same result. They must be **stateless** — no shared mutable state between runs. They must be **async** — return a Promise.

### Triggers

Triggers determine when an operation runs:

| Type       | Use case                                       |
| ---------- | ---------------------------------------------- |
| `interval` | Periodic work (every N ms)                     |
| `cron`     | Time-based schedules (daily at 2 AM)           |
| `idle`     | Background cleanup (when daemon idle > 1 hour) |
| `reactive` | Data-driven (when entity created)              |
| `event`    | Custom events (when `user:signup` emitted)     |

### Events

The daemon communicates exclusively through Node.js EventEmitter events. This design choice enables:

- **Loose coupling**: the daemon does not know what is listening
- **Multi-listener support**: multiple handlers can react to the same event
- **Event chaining**: a `success` handler can trigger the next operation
- **Consistent monitoring**: all executions are observable without polling

The event sequence for every operation is:

```
schedule() → operation:enqueued
execute()  → operation:started → operation:success
                              OR → operation:failure
```

---

## Clustering Model

For single-node deployments, one daemon instance runs all operations. For high-availability deployments, multiple nodes form a cluster using Raft consensus (`consensus.mjs`).

```
Cluster: "production-cluster"
┌──────────────┬──────────────┬──────────────┐
│ Node 1       │ Node 2       │ Node 3       │
│ (LEADER)     │ (Follower)   │ (Follower)   │
│ execute ops  │ standby      │ standby      │
│ replicate    │ replicate    │ elect new    │
└──────────────┴──────────────┴──────────────┘
```

Operation scope determines distribution:

- `local` — runs on the current node only
- `leader` — runs only on the elected leader
- `global` — distributed across all nodes

---

## Key Design Decisions

### Why EventEmitter instead of pure Promises?

`await daemon.execute('op')` tells you about one specific execution. `daemon.on('operation:success', fn)` tells you about every execution, enables chaining, and supports multiple independent observers. Background processing is inherently concurrent and event-driven; EventEmitter maps to this model naturally.

### Why LRU cache for completed operations?

The completed operations cache has a bounded size (1000 entries by default). This provides fast access to recent execution history (sub-millisecond) and automatic eviction without memory growth. For a permanent audit trail, use the KGC-4D event sourcing integration (`kgc-4d-sourcing.mjs`), which generates Merkle-proof receipts for each execution.

### Why Zod runtime validation?

All operation inputs and daemon configuration are validated by Zod schemas at runtime. This catches configuration errors at the boundary, provides IDE autocomplete through inferred types, and acts as a security boundary against malformed inputs from external callers.

---

## When to Use the Daemon

**Good fit:**

- Periodic data processing (ETL jobs, scheduled reports)
- Scheduled maintenance (cache cleanup, index optimization)
- Event-driven workflows (email on user signup)
- Distributed coordination (leader-based global tasks)
- Audit logging with cryptographic receipts

**Consider alternatives:**

- Worker queue (Bull, BullMQ) — if you need more than 1000 operations/second throughput
- System cron — if no clustering or event-driven triggering is needed
- In-process async code — if latency must be under 100ms end-to-end

---

## Performance Characteristics

| Dimension                        | Typical values               |
| -------------------------------- | ---------------------------- |
| Schedule latency                 | less than 1ms                |
| Event emit overhead              | less than 1ms                |
| Memory per daemon instance       | ~50-100 KB                   |
| Memory per scheduled operation   | ~5-10 KB                     |
| Memory per completed entry (LRU) | ~1-2 KB                      |
| Max throughput (default config)  | 5 concurrent x handler speed |
| OTel span overhead               | ~0.5ms per tool call         |

---

## See also

- [Tutorial: Your First Daemon](../tutorials/01-first-daemon.md)
- [How-To: Enable OTel Tracing](../how-to/02-enable-otel-tracing.md)
- [Reference: MCP Tools](../reference/mcp-tools.md)
- [Reference: Daemon API](../reference/daemon-api.md)

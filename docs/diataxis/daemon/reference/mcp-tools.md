# MCP Tools Reference

The daemon exposes a JSON-RPC MCP server with **36 tools**, each wrapped with `withMcpSpan()` from `src/mcp/otel-instrumentation.mjs`. Every tool call creates an OTel span named `mcp.tool.<tool_name>`.

## OTel Span Attributes (all tools)

| Attribute              | Value                          | When set        |
| ---------------------- | ------------------------------ | --------------- |
| `mcp.tool.name`        | Tool name (e.g. `graph_query`) | Always          |
| `mcp.tool.args`        | JSON-stringified arguments     | Always          |
| `mcp.server.name`      | `unrdf-daemon-mcp`             | Always          |
| `mcp.tool.success`     | `true` or `false`              | On completion   |
| `mcp.tool.result_size` | Byte length of JSON result     | On success only |

Span name format: `mcp.tool.<tool_name>` (e.g. `mcp.tool.graph_query`).

---

## Context Tools (4 tools)

Manage named RDF named-graph contexts across the daemon session.

| Tool             | Span name                 | Description                              |
| ---------------- | ------------------------- | ---------------------------------------- |
| `context_add`    | `mcp.tool.context_add`    | Add triples to an existing named context |
| `context_create` | `mcp.tool.context_create` | Create a new named context               |
| `context_list`   | `mcp.tool.context_list`   | List all active named contexts           |
| `context_remove` | `mcp.tool.context_remove` | Remove a named context and its triples   |

---

## Conversion Tools (4 tools)

Transform RDF data between serialization formats.

| Tool          | Span name              | Description                                              |
| ------------- | ---------------------- | -------------------------------------------------------- |
| `convert`     | `mcp.tool.convert`     | Convert RDF between formats (Turtle, N-Triples, JSON-LD) |
| `to_json`     | `mcp.tool.to_json`     | Serialize RDF graph to JSON-LD                           |
| `to_ntriples` | `mcp.tool.to_ntriples` | Serialize RDF graph to N-Triples                         |
| `to_turtle`   | `mcp.tool.to_turtle`   | Serialize RDF graph to Turtle                            |

---

## Daemon Tools (6 tools)

Introspect and control the daemon process and its cluster membership.

| Tool              | Span name                  | Description                             |
| ----------------- | -------------------------- | --------------------------------------- |
| `daemon_cluster`  | `mcp.tool.daemon_cluster`  | Get cluster membership and node status  |
| `daemon_config`   | `mcp.tool.daemon_config`   | Read current daemon configuration       |
| `daemon_list`     | `mcp.tool.daemon_list`     | List all running daemon instances       |
| `daemon_logs`     | `mcp.tool.daemon_logs`     | Retrieve daemon log entries             |
| `daemon_run`      | `mcp.tool.daemon_run`      | Execute a named operation on the daemon |
| `daemon_schedule` | `mcp.tool.daemon_schedule` | Schedule an operation via MCP           |
| `daemon_status`   | `mcp.tool.daemon_status`   | Get current health and metrics          |

_(7 tools in this category — the table above shows all 7)_

---

## Graph Tools (5 tools)

Create, load, dump, query, and inspect RDF graphs stored by the daemon.

| Tool           | Span name               | Description                            |
| -------------- | ----------------------- | -------------------------------------- |
| `graph_create` | `mcp.tool.graph_create` | Create a new RDF graph                 |
| `graph_dump`   | `mcp.tool.graph_dump`   | Dump all triples from a graph          |
| `graph_load`   | `mcp.tool.graph_load`   | Load RDF data into a graph             |
| `graph_query`  | `mcp.tool.graph_query`  | Execute a SPARQL query against a graph |
| `graph_stats`  | `mcp.tool.graph_stats`  | Return triple count and graph metadata |

---

## Hooks Tools (5 tools)

Define, evaluate, and execute Knowledge Hooks — policy-based rules that fire on RDF changes.

| Tool                       | Span name                           | Description                                |
| -------------------------- | ----------------------------------- | ------------------------------------------ |
| `hooks_define`             | `mcp.tool.hooks_define`             | Define a new hook with trigger conditions  |
| `hooks_evaluate_condition` | `mcp.tool.hooks_evaluate_condition` | Test whether a condition evaluates to true |
| `hooks_execute`            | `mcp.tool.hooks_execute`            | Manually trigger a hook by ID              |
| `hooks_list_conditions`    | `mcp.tool.hooks_list_conditions`    | List all registered hook conditions        |
| `hooks_receipts`           | `mcp.tool.hooks_receipts`           | Retrieve execution receipts for hooks      |

---

## MCP Server Tools (4 tools)

Control and inspect the MCP server layer itself.

| Tool          | Span name              | Description                                          |
| ------------- | ---------------------- | ---------------------------------------------------- |
| `mcp_inspect` | `mcp.tool.mcp_inspect` | Inspect MCP server capabilities and registered tools |
| `mcp_start`   | `mcp.tool.mcp_start`   | Start the MCP server                                 |
| `mcp_status`  | `mcp.tool.mcp_status`  | Get MCP server status                                |
| `mcp_stop`    | `mcp.tool.mcp_stop`    | Stop the MCP server                                  |

---

## SPARQL Query Tools (2 tools)

Execute SPARQL queries directly against the daemon's RDF stores.

| Tool         | Span name             | Description                             |
| ------------ | --------------------- | --------------------------------------- |
| `query`      | `mcp.tool.query`      | Execute a SPARQL query string           |
| `query_file` | `mcp.tool.query_file` | Execute a SPARQL query from a file path |

---

## Sync Tools (1 tool)

| Tool   | Span name       | Description                               |
| ------ | --------------- | ----------------------------------------- |
| `sync` | `mcp.tool.sync` | Synchronize RDF data between daemon nodes |

---

## Template Tools (4 tools)

Manage and apply RDF template patterns for knowledge graph construction.

| Tool                | Span name                    | Description                                   |
| ------------------- | ---------------------------- | --------------------------------------------- |
| `template_extract`  | `mcp.tool.template_extract`  | Extract a reusable template from existing RDF |
| `template_generate` | `mcp.tool.template_generate` | Generate RDF from a template                  |
| `template_list`     | `mcp.tool.template_list`     | List available templates                      |
| `template_query`    | `mcp.tool.template_query`    | Query templates by pattern                    |

---

## Tool Count Summary

| Category     | Count  |
| ------------ | ------ |
| Context      | 4      |
| Conversion   | 4      |
| Daemon       | 7      |
| Graph        | 5      |
| Hooks        | 5      |
| MCP Server   | 4      |
| SPARQL Query | 2      |
| Sync         | 1      |
| Template     | 4      |
| **Total**    | **36** |

---

## Source

Tool implementations: `packages/daemon/src/mcp/index.mjs`
OTel wrapper: `packages/daemon/src/mcp/otel-instrumentation.mjs`
Custom OTel conventions: `packages/daemon/custom-conventions.yaml`

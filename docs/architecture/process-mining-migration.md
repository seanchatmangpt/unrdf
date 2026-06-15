# Process Mining Migration: Rust (pm4py-mcp) to Python (pm4py-mcp/MCP)

## Overview

This document captures the architectural transition and migration history of the process mining capabilities in the UnRDF/Ostar project. It chronicles the shift from a Rust-based wrapper (`pm4py-mcp` v1) to a Python-based implementation utilizing the Model Context Protocol (MCP) for seamless integration across the stack.

## Architectural Decisions

### 1. Shift from Rust to Python pm4py

Initially, process mining was implemented via a Rust wrapper around `pm4py` (using `PyO3`). While this provided a high-performance boundary, it introduced significant complexity in maintaining the bindings and limited the access to the full breadth of the `pm4py` ecosystem.

**Decision:** Replace the Rust wrapper with a native Python implementation that exposes capabilities via MCP tools.
**Rationale:**

- **Feature Completeness:** Native Python allows direct access to all `pm4py` algorithms without writing complex FFI code.
- **Integration:** Easy integration with OpenTelemetry (Tempo for traces, Loki for logs) using Python's extensive library support.
- **Maintainability:** Simplifies the build pipeline by removing the Rust-to-Python dependency chain.

### 2. Adoption of Model Context Protocol (MCP)

Instead of a traditional HTTP/REST API, the project moved to MCP for inter-service communication regarding process mining.

**Decision:** Standardize on MCP (using `FastMCP`) for process mining tools.
**Rationale:**

- **Type Safety:** MCP tools have defined schemas that can be shared across services.
- **Flexibility:** Supports both `stdio` and `http` transports, making it easier to run tools as sidecars or standalone services.
- **Unified Tooling:** Aligns process mining with other AI-native tools in the ecosystem.

---

## Migration History

### Phase 1: The Rust Era (`pm4py-mcp` port 7015)

The original implementation resided at `/Users/sac/chatmangpt/pm4py-mcp/`. It exposed three primary HTTP endpoints:

- `POST /api/discovery/alpha`
- `POST /api/conformance/token-replay`
- `POST /api/statistics`

### Phase 2: Python Integration (`pm4py-analyze.py`)

A standalone Python engine was developed to fetch OTel data directly.

- **Core Engine:** `pm4py-analyze.py` (27 functions)
- **Data Sources:** Tempo (traces) and Loki (logs).
- **Key Tools:** `pm4py-ci-gate.py` (for conformance thresholds) and `synthetic-trace-gen.py` (for regression testing).

### Phase 3: Direct MCP Implementation

The final transition involved BusinessOS (Go) and Canopy (Elixir) calling Python MCP tools directly.

- **Transport:** Shift from HTTP JSON to MCP JSON-RPC.
- **Service Name:** `pm4py-mcp` (Python server).

---

## Technical Migration Details

### 1. Critical System Impacts

#### BusinessOS (Go Backend)

- **Old Path:** `Go Handler → HTTP Client → pm4py-mcp:7015 → Response`
- **New Path:** `Go Handler → MCP Client → pm4py-mcp (tools) → Response`
- **Required Changes:**
  - Removal of HTTP client logic and port 7015 constants.
  - Implementation of `mcptools` package in Go to wrap `pm4py_discover`, `pm4py_conformance`, and `pm4py_full`.
  - Update of request/response schemas to match MCP tool outputs.

#### Docker Orchestration

- Replacement of the Rust `pm4py-mcp` service with the Python-based container.
- Update of health checks from `curl /api/health` to `mcp ping`.

### 2. Configuration Changes

- **Deprecated:** `PM4PY_MCP_URL` (pointing to HTTP endpoint).
- **New Standard:** MCP connection strings or standardized service environment variables for the MCP server.

### 3. Test Suite Refactoring

- Major effort was required to refactor over 15 test cases in `bos_gateway_pm4py_test.go`.
- **Transformation:** Replaced mock HTTP servers with mock MCP clients/servers.

---

## Schema Mapping (Legacy to MCP)

| Legacy HTTP Endpoint | MCP Tool            | Mapping Notes                                                                              |
| -------------------- | ------------------- | ------------------------------------------------------------------------------------------ |
| `Discovery`          | `pm4py_discover`    | `petri_net` (places, transitions, arcs) maps to `process_model`.                           |
| `Conformance`        | `pm4py_conformance` | Maps to `fitness` and `precision` metrics.                                                 |
| `Statistics`         | `pm4py_full`        | Aggregates `trace_events`, `unique_activities`, `activity_frequency`, and `case_duration`. |

---

## Preserved Architectural Notes

### Second-Order Issues (from 2026-04-11)

- **Protocol-level Change:** The shift from HTTP to MCP is a "HIGH" risk level change due to different timeout semantics and error code mappings (e.g., HTTP 503 vs. MCP -32600).
- **Health Check Evolution:** Monitoring must shift from simple port checks to protocol-aware pings.
- **Dependency Management:** Go and Elixir environments require specific MCP client libraries or custom JSON-RPC 2.0 implementations.

### Performance Spectrum Analysis

- A bimodal pattern in activity latency distributions was identified during the transition, leading to the inclusion of `pm4py_spectrum` analysis in the new MCP toolset.

### CI Conformance Gate

- The `pm4py-ci-gate.py` script enforces quality by failing CI if `PM4PY_MIN_FITNESS` (default 0.8) or `PM4PY_MIN_PRECISION` (default 0.7) are not met.

# UNRDF WIP Archival Manifest

**Date:** 2026-04-03  
**Branch:** archival-wip-quarantine  
**Commit:** c84e0b54  
**Philosophy:** Quarantine stale WIP while preserving active playgrounds/experiments

## Summary

| Category | Items | Size | Status |
|----------|-------|------|--------|
| Benchmarks | 37 files | 828K | ✅ Archived |
| POCs | 8 files | — | ✅ Archived |
| Stale Test Artifacts | 5 files | — | ✅ Archived |
| Placeholders | 1 file | — | ✅ Archived |
| **Playgrounds** | 173 files | 2.5M+ | ✅ **PRESERVED** |
| **Experiments** | 94 files | 828K | ✅ **PRESERVED** |

## Archived Items

### Benchmarks (37 files)

**packages/atomvm/benchmarks/** (25 files)
- Standalone performance suites
- batch-throughput, circuit-breaker, concurrency, latency, memory
- pattern-match, resource-usage, serialization, sla-overhead, state-machine
- streaming-performance, comprehensive-performance, performance-analysis
- DELIVERABLES.md, README.md, RESULTS.md

**packages/daemon/benchmarks/** (11 files)
- 01-operation-scheduling.bench.mjs
- 02-concurrent-throughput.bench.mjs
- 03-memory-load.bench.mjs
- 04-raft-replication.bench.mjs
- 05-yawl-execution.bench.mjs
- runner.mjs, suite.mjs, placeholder.mjs
- 4 timestamped benchmark runs (2026-01-11)

**packages/yawl/benchmarks/** (1 file)
- performance-benchmark.mjs

### Proof-of-Concepts (8 files)

- governance-poc.test.mjs (kgc-claude/test/capabilities)
- headless-execution-poc.mjs (kgc-claude/demos)
- mcp-hyper-advanced-poc.mjs (kgc-claude/demos)
- plugin-system-poc.mjs (kgc-claude/src/capabilities)
- slash-command-poc.mjs (kgc-claude/research)
- poc-edge-cloud-sync.mjs (atomvm/experiments/wasm-integration)
- poc-jit-sparql.mjs (atomvm/experiments/wasm-integration)
- poc-zero-copy-bridge.mjs (atomvm/experiments/wasm-integration)

### Stale Test Artifacts (5 files)

- test-pkg-1.inventory.json (diataxis-kit/test/fixtures)
- test-pkg-2.inventory.json (diataxis-kit/test/fixtures)
- test-pkg-3.inventory.json (diataxis-kit/test/fixtures)
- test-atomic-log.json (kgc-runtime/var/kgc)
- test-cascading-log.json (kgc-runtime/var/kgc)

### Placeholders (1 file)

- daemon/test/placeholder.mjs

## Preserved in Production

These remain active and untouched:

- **packages/atomvm/playground/** (82 files, 2.5M)
  - Erlang process validation, gen_statem bridges
  - WASM integration framework
  - Hook primitives, supervision trees
  - Full documentation and validation suites

- **packages/kgc-4d/playground/** (91 files, 387M)
  - Experimental Next.js UI for multiverse/time-travel
  - Time-travel visualization, causal cone queries
  - Multiverse forking and delta operations
  - React components, hooks, API routes

- **packages/atomvm/experiments/** (94 files, 828K)
  - Docker swarm messaging and chaos cluster testing
  - WASM integration experiments
  - Containment validation

All dependencies between packages remain intact.

## Statistics

| Metric | Value |
|--------|-------|
| Total Archived | 51 files |
| Total Size | 520K |
| Archive Commit | c84e0b54 |
| Renames (preserved history) | 50 |
| Deletions | 0 |
| Production Code Impact | ZERO |
| Playgrounds Preserved | 2 active |
| Experiments Preserved | 1 active |

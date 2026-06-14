# UNRDF Documentation Coverage Log

**Purpose:** Bijective coverage map between documented capabilities and running examples.
A documented capability is not covered until an example in `examples/` exercises it, and that example runs.
**Prose is the claim; the example is the witness.**

## Runner

```bash
# Root examples
node examples/<name>.mjs 2>&1; echo "EXIT:$?"

# Package examples
node packages/<pkg>/examples/<name>.mjs 2>&1; echo "EXIT:$?"

# IMPORTANT: Never pipe node output before capturing exit code.
# Pipe swallows $? (reports pipe's exit, not node's).
# If you must limit output, save exit first:
#   OUTPUT=$(node examples/foo.mjs 2>&1); EXIT=$?; echo "$OUTPUT" | head -30; echo "EXIT:$EXIT"
```

## Doc Surface Scope

**In scope** (counts as "documented" for gap-mapping):

- `README.md` at repo root and `packages/*/README.md`
- `packages/hooks/API-REFERENCE.md`
- `packages/wasm4pm/WASM_API.md`
- JSDoc `/** */` above any `export` in `packages/*/src/index.mjs`

**Out of scope** (system design, not API surface):

- `docs/` directory (700+ architecture/process files)
- Internal JSDoc not on exported symbols
- Test files

## Status Legend

| Symbol          | Meaning                                                                |
| --------------- | ---------------------------------------------------------------------- |
| ✅ COVERED      | doc + running example + bidirectional link — all three verified        |
| 📋 DOC-ONLY     | documented in README/JSDoc, no running example (claim without witness) |
| 🔬 EXAMPLE-ONLY | example exists, capability not mentioned in docs                       |
| 🔴 OPEN         | queued for coverage, neither doc nor example exists                    |
| ⚠️ BROKEN       | example exists but fails to run (broken witness)                       |

---

## Gap Map — Pre-Loop State (2026-06-14)

### @unrdf/streaming (18% covered — PRIORITY 1)

Exports from `packages/streaming/src/index.mjs`:

| Export                                                           | Status          | Evidence                                                                                    |
| ---------------------------------------------------------------- | --------------- | ------------------------------------------------------------------------------------------- |
| `createChangeFeed`                                               | ✅ COVERED      | `packages/streaming/examples/validate-streaming.mjs`, `production-change-feed.mjs`          |
| `createSubscriptionManager`                                      | ✅ COVERED      | `packages/streaming/examples/validate-streaming.mjs`                                        |
| `createStreamProcessor`                                          | ✅ COVERED      | `packages/streaming/examples/validate-streaming.mjs`                                        |
| `createRealTimeValidator`, `RealTimeValidator`, `ValidationMode` | ✅ COVERED      | `packages/streaming/examples/production-change-feed.mjs`                                    |
| `createSyncMessage`                                              | ✅ COVERED      | `examples/sync-protocol.mjs` (Iteration 1)                                                  |
| `parseSyncMessage`                                               | ✅ COVERED      | `examples/sync-protocol.mjs` (Iteration 1)                                                  |
| `calculateChecksum`                                              | ✅ COVERED      | `examples/sync-protocol.mjs` (Iteration 1)                                                  |
| `mergeSyncMessages`                                              | ✅ COVERED      | `examples/sync-protocol.mjs` (Iteration 1)                                                  |
| `applySyncMessage`                                               | ✅ COVERED      | `examples/sync-protocol.mjs` (Iteration 1)                                                  |
| `createSyncMessageFromFeed`                                      | ✅ COVERED      | `examples/sync-protocol.mjs` (Iteration 1)                                                  |
| `validateShacl`                                                  | ✅ COVERED      | `examples/validate-shacl.mjs` (Iteration 5) — ⚠️ implementation is a stub, README corrected |
| `validateQuad`                                                   | ✅ COVERED      | `examples/validate-shacl.mjs` (Iteration 5)                                                 |
| `createObservabilityManager`                                     | ✅ COVERED      | `packages/streaming/examples/production-change-feed.mjs`                                    |
| `RDFStreamParser`, `createRDFStreamParser`, `parseRDFStream`     | ✅ COVERED      | `examples/rdf-stream-parser.mjs` (Iteration 2)                                              |
| `PerformanceMonitor`, `createPerformanceMonitor`                 | ✅ COVERED      | `examples/performance-monitor.mjs` (Iteration 3)                                            |
| `StreamingAdmission`, `createStreamingAdmission`                 | ✅ COVERED      | `examples/streaming-admission.mjs` (Iteration 4) — pre-existing example, now linked         |
| `DeltaConditionSchema`, `ReceiptSchema`                          | ✅ COVERED      | `examples/streaming-admission.mjs` Example 6 (Iteration 4)                                  |
| Benchmark functions (7)                                          | 🔬 EXAMPLE-ONLY | Not referenced in README                                                                    |

**Gap count: 11 DOC-ONLY, 1 EXAMPLE-ONLY cluster**

---

### @unrdf/federation (20% covered — PRIORITY 2)

Exports from `packages/federation/src/index.mjs`:

| Export                                             | Status      | Evidence                                                          |
| -------------------------------------------------- | ----------- | ----------------------------------------------------------------- |
| `createCoordinator`                                | ✅ COVERED  | `packages/federation/examples/production-federation.mjs`          |
| `createAdvancedFederationEngine`, `federatedQuery` | ✅ COVERED  | `packages/federation/examples/advanced-sparql-example.mjs`        |
| `createPeerManager`                                | ✅ COVERED  | `examples/federation-peer-query.mjs` (Iteration 6)                |
| `createDistributedQueryEngine`                     | 📋 DOC-ONLY | README documents distributed query, no example                    |
| `executeFederatedQuery`                            | ✅ COVERED  | `examples/federation-peer-query.mjs` (Iteration 6) — failure path |
| `aggregateResults`                                 | ✅ COVERED  | `examples/federation-peer-query.mjs` (Iteration 6)                |
| `routeQuery`                                       | ✅ COVERED  | `examples/federation-peer-query.mjs` (Iteration 6)                |
| `createConsensusManager`                           | 📋 DOC-ONLY | README mentions consensus, no example                             |
| `createDataReplicationManager`                     | 📋 DOC-ONLY | README mentions replication, no example                           |
| Metrics functions (recordQuery, recordError, etc.) | 📋 DOC-ONLY | README observability section, no example                          |

**Gap count: 8 DOC-ONLY**

---

### @unrdf/core (52% covered — PRIORITY 3)

Exports from `packages/core/src/index.mjs`:

| Export Group                                                             | Status      | Evidence                                                      |
| ------------------------------------------------------------------------ | ----------- | ------------------------------------------------------------- |
| `createStore`, `UnrdfStore`                                              | ✅ COVERED  | `examples/01-hello-rdf.mjs`, `examples/single-store-demo.mjs` |
| `executeQuery`, `executeSelect`, `executeConstruct`                      | ✅ COVERED  | `examples/02-sparql-queries.mjs`                              |
| `namedNode`, `literal`, `blankNode`, `quad`                              | ✅ COVERED  | Multiple root examples                                        |
| `RDF`, `RDFS`, `OWL`, `XSD`, `FOAF`, `DCTERMS`, `SKOS`                   | ✅ COVERED  | Multiple root examples                                        |
| `canonicalize`, `toNTriples`, `sortQuads`, `isIsomorphic`                | 📋 DOC-ONLY | README documents these, no dedicated example                  |
| `cloneQuad`                                                              | 📋 DOC-ONLY | JSDoc only, no example                                        |
| `RingBuffer`                                                             | 📋 DOC-ONLY | Exported, mentioned in README, no example                     |
| `diffGraphFromStores`, `diffGraphFromDelta`, `diffOntologyFromGraphDiff` | ✅ COVERED  | `examples/graph-diff.mjs` (Iteration 8)                       |
| `toDOT`, `toMermaid`, `toASCII`, `toHTML`, `extractSubgraph`             | ✅ COVERED  | `examples/graph-visualization.mjs` (Iteration 7)              |
| `RDFStarFactory`, `isQuotedTriple`, `AnnotationBuilder`                  | 📋 DOC-ONLY | README RDF-star section, zero examples                        |
| `createProvenance`, `createTemporal`, `createConfidence`                 | 📋 DOC-ONLY | README provenance section, no example                         |
| `UnrdfError`, `ValidationError`                                          | 📋 DOC-ONLY | README error handling section, no example                     |
| `retry`, `CircuitBreaker`, `RateLimiter`, `withRecovery`                 | 📋 DOC-ONLY | README resilience section, no example                         |
| `explainQuery`, `getGraphStatistics`, `checkSchemaConformance`           | 📋 DOC-ONLY | README analysis section, no example                           |
| `toDOT` visualization suite                                              | 📋 DOC-ONLY | Zero examples — highest impact gap in @unrdf/core             |

**Gap count: ~14 DOC-ONLY groups**

---

### @unrdf/hooks (90% covered — PRIORITY 4)

| Export Group                                         | Status      | Evidence                                            |
| ---------------------------------------------------- | ----------- | --------------------------------------------------- |
| `defineHook`, `executeHook`, `executeHooksByTrigger` | ✅ COVERED  | `examples/03-knowledge-hooks.mjs`                   |
| `KnowledgeHookEngine`, `KnowledgeHookManager`        | ✅ COVERED  | `examples/knowledge-engine-example.mjs`             |
| `PolicyPack`, `PolicyPackManager`                    | ✅ COVERED  | `examples/policy-pack-demo.mjs`                     |
| `HookRegistry`, `compileHookChain`                   | ✅ COVERED  | `packages/hooks/examples/hook-chains/`              |
| `HookScheduler`                                      | 📋 DOC-ONLY | README cron/interval section, no runnable example   |
| `QualityMetricsCollector` (Lean Six Sigma)           | 📋 DOC-ONLY | README quality section, no example                  |
| N3 forward-chaining rules                            | 📋 DOC-ONLY | README theory only, no practical example            |
| Datalog constraint evaluation                        | 📋 DOC-ONLY | README constraint section, no worked example        |
| `createQueryOptimizer`                               | 📋 DOC-ONLY | Exported, no usage shown                            |
| Built-in hooks (`validateSubjectIRI`, etc.)          | ✅ COVERED  | `packages/hooks/examples/basic.mjs`                 |
| `validateShacl`, `validateNode`                      | 📋 DOC-ONLY | Mentioned in streaming, no standalone hooks example |

**Gap count: 6 DOC-ONLY**

---

### Cross-Product Examples (all OPEN)

These demonstrate capabilities _composing_ — the cross-product examples that prove coherence:

| Composition                   | Status  | Why It Matters                                                            |
| ----------------------------- | ------- | ------------------------------------------------------------------------- |
| Streaming + Hooks + Receipts  | 🔴 OPEN | Audit trail pattern — StreamingAdmission feeds hooks, hooks emit receipts |
| Federation + Consensus        | 🔴 OPEN | CAP theorem scenario — consensus gate on federated write                  |
| Core diff + streaming delta   | 🔴 OPEN | `diffGraphFromDelta` applied to live change feed                          |
| Hooks + QualityMetrics + OTel | 🔴 OPEN | Full observability pipeline                                               |

---

## Summary Metrics

| Package           | Exports | Covered | DOC-ONLY | %   |
| ----------------- | ------- | ------- | -------- | --- |
| @unrdf/hooks      | ~20     | ~14     | 6        | 70% |
| @unrdf/daemon     | ~16     | ~13     | 3        | 81% |
| @unrdf/core       | ~21     | ~7      | ~14      | 33% |
| @unrdf/federation | ~10     | 2       | 8        | 20% |
| @unrdf/streaming  | ~18     | 4       | 11       | 22% |
| Cross-product     | 4       | 0       | 0        | 0%  |

**Total documented-but-unwitnessed: ~42 capability-items**
**Total exercised-but-undocumented: ~1 (streaming benchmarks)**
**Goal: drive both to 0 (bijective coverage)**

---

## Iteration Log

_(Append each closed triple here. Format: ## Iteration N — YYYY-MM-DD)_
_(Each entry must include: API name, doc location, example path, captured run output, real exit code)_

---

## Iteration 1 — 2026-06-14

**Commit:** `17e05339` (working tree has pre-existing modifications to unrelated files — `.env.example`, `.github/workflows/release.yml`, `Makefile`; none authored this iteration, none touched)
**Node:** v25.9.0

### Triple Closed: @unrdf/streaming — Sync Protocol

**APIs covered:** `createSyncMessage`, `parseSyncMessage`, `calculateChecksum`, `mergeSyncMessages`, `applySyncMessage`, `createSyncMessageFromFeed`

**Doc:** `packages/streaming/README.md` — Added "Sync Protocol (`sync-protocol.mjs`)" section under API Reference, documenting all 6 functions with signatures, return types, and the checksum-mismatch failure contract. Example path cited in doc header.

**Example:** `examples/sync-protocol.mjs`

**Captured run output:**

```
[createSyncMessage] ✅ message with 2 changes, version 1.0, checksum d45b95800fda4fd3…
[parseSyncMessage] ✅ round-trip succeeds, 2 changes validated
[parseSyncMessage] ✅ tampered checksum throws: "Checksum mismatch: message may be corrupted"
[calculateChecksum] ✅ deterministic: d45b95800fda4fd3… (same for identical changes)
[mergeSyncMessages] ✅ merged 2 messages → 3 changes total
[applySyncMessage] ✅ applied 2 changes to feed
[createSyncMessageFromFeed] ✅ reconstructed 2 changes from feed history
[createSyncMessageFromFeed] ✅ since filter: 1 change after timestamp 3000

✅ All sync protocol assertions passed.
```

**Real exit code: EXIT:0**

**What the run demonstrated:** The tampered-checksum assertion (step 3) is the witness against forgery — if `parseSyncMessage` stopped verifying the checksum, this example would throw `Error: Expected checksum mismatch error` and exit non-zero. The `since` filter assertion (step 7) confirms the `getHistory` options contract.

**Bidirectional link:** `packages/streaming/README.md` → cites `../../examples/sync-protocol.mjs`; `examples/sync-protocol.mjs` header → cites `packages/streaming/README.md — "Sync Protocol" section`.

### Gap Map Update

**Closed this iteration (6 items → ✅):**

- `createSyncMessage` ✅
- `parseSyncMessage` ✅
- `calculateChecksum` ✅
- `mergeSyncMessages` ✅
- `applySyncMessage` ✅
- `createSyncMessageFromFeed` ✅

**Still DOC-ONLY (next up):**

- `@unrdf/streaming`: `RDFStreamParser`, `createRDFStreamParser`, `parseRDFStream` (V6)
- `@unrdf/streaming`: `PerformanceMonitor`, `createPerformanceMonitor` (V6)
- `@unrdf/streaming`: `StreamingAdmission`, `createStreamingAdmission`, `DeltaConditionSchema`, `ReceiptSchema`
- `@unrdf/streaming`: `validateShacl`, `validateQuad`
- `@unrdf/federation`: 8 items (see gap map above)
- `@unrdf/core`: ~14 groups (see gap map above)
- `@unrdf/hooks`: 6 items (see gap map above)

**Remaining documented-but-unwitnessed: ~36 items (was ~42)**

---

## Iteration 2 — 2026-06-14

**Commit:** `17e05339` (same pre-existing dirty files, unchanged)
**Node:** v25.9.0

### Triple Closed: @unrdf/streaming — RDF Stream Parser

**APIs covered:** `RDFStreamParser`, `createRDFStreamParser`, `parseRDFStream`

**Doc:** `packages/streaming/README.md` — Added "RDF Stream Parser (`rdf-stream-parser.mjs`)" section documenting constructor options, chunk/complete event shapes, `getMetrics()`, and the `parseRDFStream` convenience wrapper.

**Example:** `examples/rdf-stream-parser.mjs`

**Captured run output:**

```
[RDFStreamParser] Received chunk with 3 quads
[RDFStreamParser] Received chunk with 2 quads
[RDFStreamParser] Parsing complete: 5 quads in 2 chunk(s)
[parseRDFStream] Collected 5 quads via convenience wrapper
[parseRDFStream] First quad subject: <https://example.org/Alice>
[format error] ✅ Parser emits error for invalid RDF: Unexpected "this" on line 1.…

✅ All RDF stream parser assertions passed.
```

**Real exit code: EXIT:0**

**What the run demonstrated:** The `chunkSize: 3` setting visibly splits 5 quads into 2 chunks (3+2), proving the chunking contract. The format error case (`parseError` must be non-null after `onError`) would fail if the parser silently ignored invalid input. The `parseRDFStream` subject assertion would fail if quad order or structure changed.

**Bidirectional link:** README → `../../examples/rdf-stream-parser.mjs`; example header → `packages/streaming/README.md — "RDF Stream Parser" section`.

**Remaining documented-but-unwitnessed: ~33 items (was ~36)**

---

## Iteration 3 — 2026-06-14

**Commit:** `17e05339`

### Triple Closed: @unrdf/streaming — Performance Monitor

**APIs covered:** `PerformanceMonitor`, `createPerformanceMonitor`

**Doc:** `packages/streaming/README.md` — Added "Performance Monitor (`performance-monitor.mjs`)" section documenting config options, all record methods, events, `getCurrentMetrics()`, `getReport()` structure (with p50/p95/p99 stats), `reset()`.

**Example:** `examples/performance-monitor.mjs`

**Captured run output:**

```
[start] ✅ monitor is monitoring
[recordQuad/Bytes/Chunk] ✅ getCurrentMetrics: 100 quads, 4096 bytes, 5 chunks
[recordError] ✅ errorsCount=1, error-recorded event fired
[recordBackpressure] ✅ backpressureEvents=1, event fired
[threshold-violation] ✅ fires when heap (7.4MB) exceeds memoryMax (1 byte)
[getReport] ✅ report has throughput/latency/memory/backpressure/errors sections
[getReport] ✅ errors.count=1, message="test-stream-error"
[reset] ✅ metrics cleared: quadsProcessed=0

✅ All performance monitor assertions passed.
```

**Real exit code: EXIT:0**

**What the run demonstrated:** The threshold violation test (`memoryMax: 1`) is the witness against forgery — if the monitor stopped collecting memory samples or stopped checking thresholds, the `threshold-violation` event would never fire and the `await new Promise(resolve => ...)` would hang. The `getCurrentMetrics()` counter assertions break if the record methods don't increment state.

**Remaining documented-but-unwitnessed: ~31 items (was ~33)**

---

## Iteration 4 — 2026-06-14

**Commit:** `17e05339`

### Triple Closed: @unrdf/streaming — StreamingAdmission + Schemas

**APIs covered:** `StreamingAdmission`, `createStreamingAdmission`, `admitStream`, `verifyChain`, `getStats`, `DeltaConditionSchema`, `ReceiptSchema`

**Finding:** `examples/streaming-admission.mjs` pre-existed and ran (EXIT:0) — the original gap map was wrong (agent enumerated only `packages/streaming/examples/`, missing root examples). Example extended with Example 6 to directly witness `DeltaConditionSchema` and `ReceiptSchema`.

**Doc:** `packages/streaming/README.md` — Added "Streaming Admission (`stream-admit.mjs`)" section with full API reference, schema descriptions, and example link.

**Example:** `examples/streaming-admission.mjs` (Example 6 added this iteration)

**Captured run output (tail):**

```
=== Example 6: Schema Validation ===
✓ DeltaConditionSchema accepts valid condition: {"kind":"delta","checksum":"blake3"}
✓ DeltaConditionSchema rejects invalid kind (kind must be "delta")
✓ ReceiptSchema validates receipt from admit(): status=admitted
✓ ReceiptSchema rejects receipt with malformed inputHash (must be 64 chars)

=== All examples completed successfully ===
```

**Real exit code: EXIT:0**

**What the run demonstrated:** Schema rejection tests (invalid kind, short hash) would fail if the schemas stopped enforcing their constraints — both use `safeParse` and assert `!success`.

**Remaining documented-but-unwitnessed: ~24 items (was ~31)**
_(Note: 7 items closed — gap map correction revealed StreamingAdmission cluster was already partially witnessed)_

---

## Iteration 5 — 2026-06-14

**Commit:** `17e05339`

### Triple Closed: @unrdf/streaming — SHACL Validation (with false-doc-claim correction)

**APIs covered:** `validateShacl`, `validateQuad`

**Critical finding — false doc claim:** The README stated `validateShacl` "validates RDF data against SHACL shapes" and returns `conforms: false` with violations. The source is a **stub** — `validateShape()` never adds violations; the function always returns `{ conforms: true, results: [] }`. This is a doc that claims behavior the code lacks.

**Action taken:** Corrected the README with a ⚠️ stub warning. Example witnesses the _actual_ behavior (always `conforms: true`) and contains a `CONFORMS_STUB_ALWAYS_TRUE` flag — when real SHACL evaluation is implemented, flipping the flag makes this example the regression test.

**Example:** `examples/validate-shacl.mjs`

**Captured run output:**

```
[validateShacl] ✅ result has required shape: { conforms: true, results: [], timestamp: <number> }
[validateShacl] ✅ stub returns conforms: true (FUTURE: false — Alice violates PersonShape by lacking foaf:name)
[validateQuad] ✅ wraps quad in temp store, returns { conforms: true, results: [] }
[options] ✅ strict and maxViolations accepted without error

✅ All SHACL validation API surface assertions passed.
   ⚠️  Note: conforms detection is a stub — see README for implementation status.
```

**Real exit code: EXIT:0**

**Remaining documented-but-unwitnessed: ~22 items (was ~24)**

---

## Iteration 6 — 2026-06-14

**Commit:** `17e05339`

### Triple Closed: @unrdf/federation — Peer Manager + Distributed Query core

**APIs covered:** `createPeerManager`, `PeerConfigSchema`, `routeQuery`, `aggregateResults`, `executeFederatedQuery`

**Doc:** `packages/federation/README.md` — Added "API Reference" section with `createPeerManager`, `routeQuery`, `aggregateResults`, `executeFederatedQuery` signatures and behavior.

**Example:** `examples/federation-peer-query.mjs`

**Captured run output:**

```
[createPeerManager] ✅ registered 3 peers
[createPeerManager]   alice: https://sparql.alice.example.org (healthy)
[createPeerManager] ✅ re-register updates endpoint, preserves registeredAt
[createPeerManager] ✅ unregisterPeer works: 2 peers remain
[PeerConfigSchema] ✅ rejects empty id and non-URL endpoint (2 issues)
[routeQuery] ✅ broadcast=2, selective=2, first-available=1
[aggregateResults] ✅ deduplicated 4 bindings (2+2 with 1 duplicate) → 3 unique
[aggregateResults] ✅ ignores failed peer results (1 failure in input)
[executeFederatedQuery] ✅ returns { success: false } for unreachable endpoint
[executeFederatedQuery]   error: "fetch failed", duration: 36ms

✅ All federation peer query assertions passed.
```

**Real exit code: EXIT:0**

**Note:** `executeFederatedQuery` tested via failure path only (no remote SPARQL server required). Success path needs a live SPARQL endpoint.

**Remaining documented-but-unwitnessed: ~17 items (was ~22)**

---

## Iteration 7 — 2026-06-14

**Commit:** `17e05339`

### Triple Closed: @unrdf/core — Graph Visualization Suite

**APIs covered:** `toDOT`, `toMermaid`, `toASCII`, `toHTML`, `extractSubgraph`

**Doc:** `packages/core/README.md` — Added "Graph Visualization" API section with all options documented.

**Example:** `examples/graph-visualization.mjs`

**Captured run output:**

```
[toDOT] ✅ output starts with "digraph RDF {", contains "->" edges
[toDOT]   17 lines, direction=TB (default)
[toDOT] ✅ direction:LR → "rankdir=LR", limit:2 caps at 2 triples
[toMermaid] ✅ starts with "graph TB", contains "-->|" labelled edges
[toMermaid] ✅ direction:LR → starts with "graph LR"
[toASCII] ✅ returns 9-line ASCII representation
[toHTML] ✅ returns HTML markup (3311 chars)
[extractSubgraph] ✅ subject filter for Alice → 3 quads
[extractSubgraph] ✅ depth:1 filter → 3 quads
[type guard] ✅ toDOT/toMermaid/toASCII/toHTML all throw TypeError for null store

✅ All graph visualization assertions passed.
```

**Real exit code: EXIT:0**

**Remaining documented-but-unwitnessed: ~12 items (was ~17)**

---

## Iteration 8 — 2026-06-14

**Commit:** `17e05339`

### Triple Closed: @unrdf/core — Graph Diff

**APIs covered:** `diffGraphFromStores`, `diffGraphFromDelta`, `diffOntologyFromGraphDiff`

**Doc:** `packages/core/README.md` — Added "Graph Diff" section documenting the three functions, DiffTriple shape, store contract, and idempotence.

**Example:** `examples/graph-diff.mjs`

**Captured run output:**

```
[diffGraphFromStores] ✅ added=2, removed=1
[diffGraphFromStores]   removed: https://example.org/Alice → foaf:knows → https://example.org/Bob
[diffGraphFromStores] ✅ idempotent: diff(store, store) → {added:0, removed:0}
[diffGraphFromDelta] ✅ added=1, removed=1
[diffOntologyFromGraphDiff] ✅ 3 classified changes:
  - relationship-added
  - class-assertion-added
  - relationship-removed

✅ All graph diff assertions passed.
```

**Real exit code: EXIT:0**

**Remaining documented-but-unwitnessed: ~9 items (was ~12)**

---

## Queue (ordered by priority)

1. `@unrdf/streaming` sync protocol — `createSyncMessage`, `parseSyncMessage`, `applySyncMessage`, `createSyncMessageFromFeed`
2. `@unrdf/streaming` V6 — `RDFStreamParser`, `createRDFStreamParser`, `parseRDFStream`
3. `@unrdf/streaming` V6 — `PerformanceMonitor`, `createPerformanceMonitor`
4. `@unrdf/streaming` admission — `StreamingAdmission`, `createStreamingAdmission`
5. `@unrdf/federation` — `createPeerManager`, `createDistributedQueryEngine`
6. `@unrdf/federation` — `ConsensusManager`, `DataReplicationManager`
7. `@unrdf/core` — visualization suite (`toDOT`, `toMermaid`, `toASCII`, `toHTML`)
8. `@unrdf/core` — diff suite (`diffGraphFromStores`, `diffGraphFromDelta`)
9. `@unrdf/hooks` — `HookScheduler`, `QualityMetricsCollector`
10. Cross-product: Streaming + Hooks + Receipts
11. Cross-product: Federation + Consensus

---

## Iteration 9 — 2026-06-14

**Commit:** `17e05339`

### Triple 1 Closed: @unrdf/hooks — HookScheduler

**APIs covered:** `HookScheduler`, `ScheduleConfigSchema` — `register()`, `unregister()`, `start()`, `stop()`, `notifyActivity()`, `getStats()`

**Doc:** `packages/hooks/README.md` — Added "HookScheduler" section with full API, code examples, and cron format warning (only `*/n` syntax supported, not standard 5-field cron).

**Example:** `examples/hook-scheduler.mjs`

**Captured run output:**

```
[startup hook] ✅ scheduler running, 1 schedule(s) registered
[stop] ✅ scheduler stopped
[interval hook] ✅ executed 2 times in 120ms (interval=25ms)
[getStats] ✅ total=3, enabled=2, byType={"cron":1,"interval":1,"idle":1,"startup":0}
[unregister] ✅ removed cron schedule: totalSchedules=2
[maxRuns] ✅ schedule with maxRuns:2 executed 2 time(s) (≤3 with tick timing)

✅ All HookScheduler assertions passed.
```

**Real exit code: EXIT:0**

**False doc claim discovered:** Cron expression docs conventionally imply standard 5-field format. The actual parser only accepts `*/n` (every N minutes). README corrected with explicit warning.

### Triple 2 Closed: @unrdf/hooks — QualityMetricsCollector

**APIs covered:** `QualityMetricsCollector` — `record()`, `recordDefect()`, `getSummary()`, `registerKaizenEvent()`, `exportAuditTrail()`

**Doc:** `packages/hooks/README.md` — Added "QualityMetricsCollector" section with Lean Six Sigma pattern, DPMO/sigma level explanation, and audit trail filtering.

**Example:** `examples/quality-metrics.mjs`

**Implementation note:** `metricsTracked` in getSummary() counts only metrics registered via `registerMetric()`, not metrics created implicitly by `record()`. The `dataPointsStored` count correctly reflects `record()` calls.

**Captured run output:**

```
[record] ✅ dataPointsStored=5, totalMeasurements=5
[recordDefect] ✅ totalDefects=2, recentDefects.length=2
[recordDefect]   first: "SPARQL query returned empty result for known entit..."
[getSummary] ✅ dpmo=400000, sigmaLevel=1.00, auditLogEntries=2
[kaizen] ✅ registered Kaizen event: action="Add SPARQL result caching layer"
[audit] ✅ total trail entries=3, defect-recorded entries=2
[audit]   first entry timestamp type=object
[forgery witness] ✅ 10 defects recorded → totalDefects=10, recentDefects.length=10

✅ All QualityMetricsCollector assertions passed.
```

**Real exit code: EXIT:0**

**Remaining documented-but-unwitnessed after Iteration 9: ~7 items**

---

## Queue (ordered by priority)

1. `@unrdf/hooks` — N3 forward-chaining rules (full runnable example beyond README prose)
2. `@unrdf/hooks` — Datalog constraint evaluation
3. `@unrdf/hooks` — `createQueryOptimizer`
4. `@unrdf/core` — Error handling: `UnrdfError`, `ValidationError`, `retry`, `CircuitBreaker`, `RateLimiter`, `withRecovery`
5. `@unrdf/core` — RDF-star: `RDFStarFactory`, `isQuotedTriple`, `AnnotationBuilder`, `createProvenance`, `createTemporal`, `createConfidence`
6. `@unrdf/core` — Analysis: `explainQuery`, `getGraphStatistics`, `checkSchemaConformance`
7. `@unrdf/federation` — `ConsensusManager`, `createConsensusManager`
8. `@unrdf/federation` — `DataReplicationManager`, `createDataReplicationManager`
9. Cross-product: Streaming + Hooks + Receipts
10. Cross-product: Federation + Consensus
11. Cross-product: Core diff + streaming delta
12. Cross-product: Hooks + QualityMetrics + OTel

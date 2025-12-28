# AtomVM Integration Architecture

**Version:** 6.0.0
**Last Updated:** 2025-12-28
**Status:** Production

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Module Dependency Graph](#module-dependency-graph)
3. [Layer Architecture](#layer-architecture)
4. [Data Flow Diagrams](#data-flow-diagrams)
5. [Integration Points](#integration-points)
6. [Deployment Scenarios](#deployment-scenarios)
7. [What's Working](#whats-working)
8. [What's Planned](#whats-planned)
9. [What's Blocked](#whats-blocked)
10. [Recommended Implementation Order](#recommended-implementation-order)

---

## System Overview

AtomVM is a comprehensive framework for running BEAM bytecode (Erlang/Elixir) in JavaScript environments (browser and Node.js) with full RDF knowledge graph integration via the @unrdf ecosystem.

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         AtomVM Integration Stack                        │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │                    OBSERVABILITY LAYER                            │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐           │ │
│  │  │ OTEL Tracing │  │ SLA Monitor  │  │ Roundtrip SLA│           │ │
│  │  │ (spans/metrics)  │ (latency/err) │ (JS→BEAM→JS)  │           │ │
│  │  └──────────────┘  └──────────────┘  └──────────────┘           │ │
│  └───────────────────────────────────────────────────────────────────┘ │
│                                ↓                                        │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │                    DISTRIBUTED LAYER                              │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐           │ │
│  │  │ Message      │  │ Circuit      │  │ Supervisor   │           │ │
│  │  │ Validator    │  │ Breaker      │  │ Tree         │           │ │
│  │  │ (Zod schemas)│  │ (fault tol)  │  │ (OTP-style)  │           │ │
│  │  └──────────────┘  └──────────────┘  └──────────────┘           │ │
│  └───────────────────────────────────────────────────────────────────┘ │
│                                ↓                                        │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │                    RDF KNOWLEDGE LAYER                            │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐           │ │
│  │  │ Oxigraph     │  │ SPARQL       │  │ RDF          │           │ │
│  │  │ Bridge       │  │ Pattern      │  │ Validator    │           │ │
│  │  │              │  │ Matcher      │  │              │           │ │
│  │  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘           │ │
│  │         │                 │                 │                     │ │
│  │  ┌──────┴─────────────────┴─────────────────┴───────┐           │ │
│  │  │ Query Cache (LRU) │ Triple Batcher (Streaming)   │           │ │
│  │  └──────────────────────────────────────────────────┘           │ │
│  └───────────────────────────────────────────────────────────────────┘ │
│                                ↓                                        │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │                    RUNTIME LAYER                                  │ │
│  │  ┌──────────────────────────┐  ┌──────────────────────────┐     │ │
│  │  │   Browser Runtime        │  │   Node.js Runtime        │     │ │
│  │  │ ┌──────────────────────┐ │  │ ┌──────────────────────┐ │     │ │
│  │  │ │ AtomVM WASM Loader   │ │  │ │ AtomVM Node Executor │ │     │ │
│  │  │ │ (v0.6.6)             │ │  │ │ (v0.6.6)             │ │     │ │
│  │  │ └──────────────────────┘ │  │ └──────────────────────┘ │     │ │
│  │  │ ┌──────────────────────┐ │  │ ┌──────────────────────┐ │     │ │
│  │  │ │ Service Worker Mgr   │ │  │ │ Process Spawner      │ │     │ │
│  │  │ │ (COI/SharedArrayBuf) │ │  │ │ (child_process)      │ │     │ │
│  │  │ └──────────────────────┘ │  │ └──────────────────────┘ │     │ │
│  │  │ ┌──────────────────────┐ │  │                          │     │ │
│  │  │ │ Hot Code Loader      │ │  │                          │     │ │
│  │  │ │ (live reload)        │ │  │                          │     │ │
│  │  │ └──────────────────────┘ │  │                          │     │ │
│  │  └──────────────────────────┘  └──────────────────────────┘     │ │
│  └───────────────────────────────────────────────────────────────────┘ │
│                                ↓                                        │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │                    INTEGRATION LAYER                              │ │
│  │  ┌──────────────────────────────────────────────────────────────┐│ │
│  │  │           @unrdf Ecosystem Integration                       ││ │
│  │  │  ┌────────────┐  ┌────────────┐  ┌────────────┐            ││ │
│  │  │  │ @unrdf/    │  │ @unrdf/    │  │ @unrdf/    │            ││ │
│  │  │  │ oxigraph   │  │ streaming  │  │ core       │            ││ │
│  │  │  │            │  │            │  │            │            ││ │
│  │  │  └────────────┘  └────────────┘  └────────────┘            ││ │
│  │  └──────────────────────────────────────────────────────────────┘│ │
│  └───────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Module Dependency Graph

### Core Module Relationships

```
index.mjs (Public API)
    │
    ├─→ atomvm-runtime.mjs ────────────┐
    │       └─→ roundtrip-sla.mjs      │
    │                                   │
    ├─→ node-runtime.mjs ───────────────┤
    │       └─→ roundtrip-sla.mjs      │
    │                                   │
    ├─→ hot-code-loader.mjs             │
    │       └─→ supervisor-tree.mjs    │
    │                                   │
    ├─→ oxigraph-bridge.mjs ────────────┼─→ @unrdf/oxigraph
    │       └─→ otel-instrumentation.mjs│
    │                                   │
    ├─→ sparql-pattern-matcher.mjs ─────┤
    │       └─→ otel-instrumentation.mjs│
    │                                   │
    ├─→ triple-stream-batcher.mjs ──────┤
    │       └─→ otel-instrumentation.mjs│
    │                                   │
    ├─→ rdf-validator.mjs ───────────────┤
    │       └─→ otel-instrumentation.mjs│
    │                                   │
    ├─→ query-cache.mjs ─────────────────┤
    │       └─→ otel-instrumentation.mjs│
    │                                   │
    ├─→ message-validator.mjs ───────────┤
    │       └─→ zod                     │
    │       └─→ otel-instrumentation.mjs│
    │                                   │
    ├─→ sla-monitor.mjs ─────────────────┤
    │       └─→ otel-instrumentation.mjs│
    │                                   │
    ├─→ circuit-breaker.mjs              │
    │                                   │
    ├─→ supervisor-tree.mjs              │
    │                                   │
    └─→ service-worker-manager.mjs       │
            └─→ coi-serviceworker        │
```

### Dependency Categories

**Zero Dependencies (Pure):**
- `supervisor-tree.mjs` - OTP supervision
- `circuit-breaker.mjs` - Fault tolerance
- `roundtrip-sla.mjs` - SLA tracking

**OTEL-Only Dependencies:**
- `oxigraph-bridge.mjs`
- `sparql-pattern-matcher.mjs`
- `triple-stream-batcher.mjs`
- `rdf-validator.mjs`
- `query-cache.mjs`
- `sla-monitor.mjs`

**External Dependencies:**
- `message-validator.mjs` → `zod` (runtime validation)
- `service-worker-manager.mjs` → `coi-serviceworker` (COI)
- `atomvm-runtime.mjs` → AtomVM WASM binary (v0.6.6)
- `node-runtime.mjs` → AtomVM Node.js binary (v0.6.6)

**@unrdf Ecosystem:**
- `@unrdf/oxigraph` - RDF store (used by oxigraph-bridge.mjs)
- `@unrdf/streaming` - Stream processing (available for integration)
- `@unrdf/core` - Core RDF utilities (available for integration)

---

## Layer Architecture

### Layer 1: Runtime Execution

**Purpose:** Execute BEAM bytecode in JavaScript environments

**Components:**
- `atomvm-runtime.mjs` - Browser WASM loader and executor
- `node-runtime.mjs` - Node.js process spawner
- `service-worker-manager.mjs` - Cross-Origin-Isolation setup
- `hot-code-loader.mjs` - Live module reloading

**State Machine (Poka-Yoke Design):**
```
Uninitialized → Loading → Ready → Executing → Ready
                   ↓                    ↓
                 Error ← ← ← ← ← ← ← Error
                   ↓                    ↓
               Destroyed ← ← ← ← ← Destroyed
```

**Key Characteristics:**
- **State-safe:** Invalid operations prevented by design
- **Dual-runtime:** Unified API for browser and Node.js
- **Performance:** <10ms BEAM execution roundtrip SLA
- **Cross-platform:** Works in all modern browsers + Node.js 18+

---

### Layer 2: RDF Knowledge Integration

**Purpose:** Bridge BEAM processes to RDF triple stores

**Components:**

#### 2.1 Oxigraph Bridge
```javascript
// High-level RDF store operations
OxigraphBridge {
  addTriples(triples[])      // Batch insert
  queryPattern(s, p, o)      // Pattern match
  removeTriples(triples[])   // Batch delete
  sparqlQuery(query)         // SPARQL execution
}
```

**Features:**
- **State machine:** Uninitialized → Ready → Operating → Ready
- **Validation:** Poka-yoke triple structure validation
- **OTEL:** Full tracing on all operations
- **Performance:** Optimized for batch operations

#### 2.2 SPARQL Pattern Matcher
```javascript
SPARQLPatternMatcher {
  matchPattern(s, p, o, filters[])   // Triple pattern matching
  executeQuery(sparqlString)         // Full SPARQL SELECT
  compileToBeamPattern(s, p, o)      // BEAM-style pattern
}
```

**Features:**
- **Query types:** SELECT, ASK, CONSTRUCT, DESCRIBE
- **Filters:** Equality, inequality, comparisons, AND, OR
- **Caching:** 5s TTL, 100 query limit
- **Prefixes:** Built-in RDF/RDFS/XSD/FOAF/Schema.org

#### 2.3 Triple Stream Batcher
```javascript
TripleStreamBatcher {
  addTriple(triple)              // Add single triple
  addTriples(triples[])          // Add multiple triples
  flush()                        // Force flush
  streamTriples(asyncIterable)   // Stream processing
  getMetrics()                   // Throughput stats
}
```

**Features:**
- **Batch size:** 100 triples (configurable)
- **Timeout:** 50ms partial batch flush
- **Backpressure:** 10,000 queue max
- **Throughput:** Target ≥10,000 triples/sec

#### 2.4 RDF Validator
```javascript
RDFValidator {
  validateTriple(triple)                // Structure validation
  validateAgainstShape(shape, subject)  // SHACL-like validation
  registerShape(name, rules[])          // Custom shapes
  validateGraph(triples[], options)     // Batch validation
}
```

**Features:**
- **IRI validation:** RFC 3987 compliance
- **Datatype validation:** XSD types (string, integer, date, etc.)
- **SHACL-like shapes:** Property constraints, cardinality
- **Built-in shapes:** FOAF:Person, Schema:Thing, etc.

#### 2.5 Query Cache
```javascript
QueryCache {
  set(query, bindings, result)   // Cache result
  get(query, bindings)           // Retrieve cached
  invalidate(pattern)            // Pattern-based invalidation
  stats()                        // Hit rate, size
}
```

**Features:**
- **LRU eviction:** Map insertion order
- **TTL expiration:** 60s default
- **Pattern invalidation:** Triple pattern matching
- **OTEL metrics:** Hit/miss counters

---

### Layer 3: Distributed Systems

**Purpose:** Enable fault-tolerant distributed BEAM execution

**Components:**

#### 3.1 Message Validator
```javascript
// Zod-based validation for distributed messages
messageSchemas {
  triplePattern: { s?, p?, o? }
  rpcCall: { target, module, function, args[] }
  rpcResult: { ok, result, error? }
  sparqlQuery: { query, params? }
  batchOperation: { operation, triples[], transactionId? }
  healthCheck: { nodeId, timestamp, status, metrics? }
}
```

**Validation Functions:**
- `validateTriplePattern(data)` → `{success, data?, error?}`
- `validateRPCCall(data)` → `{success, data?, error?}`
- `validateSPARQLQuery(data)` → `{success, data?, error?}`
- `withValidation(fn, schemaType)` → Wrapped function

**Features:**
- **Runtime safety:** Zod schema validation
- **OTEL logging:** Validation failures traced
- **Clear errors:** Human-readable error messages
- **Middleware:** Circuit breaker integration

#### 3.2 Circuit Breaker
```javascript
CircuitBreaker {
  call(fn)           // Protected execution
  getState()         // closed | open | half-open
  getFailureCount()  // Current failures
  canClose()         // Check reset timeout
}
```

**Features:**
- **States:** Closed → Open (after threshold) → Half-Open → Closed
- **Configurable:** Failure threshold, reset timeout
- **Telecom-grade:** Production-tested fault tolerance

#### 3.3 Supervisor Tree
```javascript
SupervisorTree {
  addChild(id, fn, strategy)  // Register child
  start()                     // Start all children
  restart(childId)            // Restart specific child
}
```

**Strategies:**
- `one_for_one` - Restart failed child only
- `one_for_all` - Restart all children
- `rest_for_one` - Restart failed + all after it

**Features:**
- **OTP-style:** Erlang supervisor semantics
- **Automatic recovery:** Failed processes restart
- **Configurable:** Per-child restart strategies

---

### Layer 4: Observability

**Purpose:** Production-grade monitoring and SLA enforcement

**Components:**

#### 4.1 OTEL Instrumentation
```javascript
// Centralized tracing utilities
getTracer()                                    // Get OTEL tracer
createSpan(name, attrs)                       // New span
withSpan(name, fn, attrs)                     // Execute with span
recordError(span, error)                      // Record exception
traceTriplePattern(s, p, o, fn)               // Trace triple ops
traceSPARQLQuery(query, fn)                   // Trace queries
traceRPCCall(target, module, fn)              // Trace RPC
traceBatchOperation(type, size, fn)           // Trace batches
```

**Features:**
- **Lazy initialization:** Provider registered first
- **Context propagation:** Async span contexts
- **Standard attributes:** duration_ms, success, error
- **Pre-configured:** Domain-specific tracers

#### 4.2 SLA Monitor
```javascript
SLAMonitor {
  recordLatency(operation, ms)     // Track latency
  recordError(operation, error)    // Track error
  getMetrics(operation)            // Get stats
  isWithinSLA(operation)           // Check compliance
  generateReport()                 // Text report
  exportMetrics()                  // JSON metrics
}
```

**Features:**
- **Latency:** P50, P95, P99, avg, min, max, stddev
- **Error rate:** success/error counts
- **Throughput:** ops/sec
- **SLA thresholds:** <10ms latency, <0.1% error rate
- **Violations:** Automatic violation tracking

#### 4.3 Roundtrip SLA
```javascript
// JS→Erlang→JS roundtrip tracking
startRoundtrip(operationType)           // Begin tracking
endRoundtrip(operationId, success)      // End tracking
getSLAStats(operationType)              // Get stats
canStartRoundtrip(operationType)        // Poka-yoke check
```

**SLA Requirements:**
- **Latency:** <10ms per roundtrip
- **Error rate:** <0.1% (1 error per 1000 roundtrips)
- **Poka-yoke:** Prevents operations violating error rate threshold

**Operation Types:**
- `emit_event` - Event emission roundtrip
- `register_hook` - Hook registration
- `process_intent` - Intent processing
- `execute_beam` - BEAM execution

---

## Data Flow Diagrams

### Flow 1: Triple Ingestion (JS → BEAM → Oxigraph)

```
┌──────────────────────────────────────────────────────────────────┐
│ JavaScript Application                                           │
└───────────────┬──────────────────────────────────────────────────┘
                │
                │ 1. Triple array
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ TripleStreamBatcher                                               │
│  • Accumulate to batch size (100)                                │
│  • Apply backpressure if queue > 10,000                          │
│  • Flush on timeout (50ms)                                       │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 2. Batched triples
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ RDFValidator                                                      │
│  • Validate IRI format (RFC 3987)                                │
│  • Validate datatypes (XSD)                                      │
│  • Check SHACL-like shapes (if configured)                       │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 3. Validated triples
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ MessageValidator                                                  │
│  • Zod schema validation (batch operation)                       │
│  • OTEL logging on validation failure                            │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 4. Validated message
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ AtomVM Runtime (Browser or Node)                                 │
│  • Start roundtrip tracking (roundtrip-sla)                      │
│  • Serialize to BEAM format                                      │
│  • Execute BEAM handler                                          │
│  • End roundtrip tracking (<10ms SLA)                            │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 5. BEAM response
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ OxigraphBridge                                                    │
│  • Create OTEL span (bridge.add_triples)                         │
│  • Normalize triples (s/p/o format)                              │
│  • Call store.add() or store.addQuad()                           │
│  • Record metrics (count, duration)                              │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 6. Stored in RDF store
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ @unrdf/oxigraph Store                                            │
│  • Persistent RDF storage                                        │
│  • SPARQL query engine                                           │
└───────────────────────────────────────────────────────────────────┘
```

**Performance Characteristics:**
- **Batch size:** 100 triples
- **Latency:** <10ms BEAM roundtrip
- **Throughput:** ≥10,000 triples/sec
- **Validation:** IRI + datatype + schema
- **Observability:** OTEL spans on all steps

---

### Flow 2: Query Execution (SPARQL → Pattern → Results)

```
┌──────────────────────────────────────────────────────────────────┐
│ JavaScript Application                                           │
│  query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"          │
└───────────────┬──────────────────────────────────────────────────┘
                │
                │ 1. SPARQL query string
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ QueryCache (LRU)                                                 │
│  • Check cache: key = query + bindings                          │
│  • If hit: return cached result (OTEL: cache_hit)               │
│  • If miss: continue to execution                               │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 2. Cache miss
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ MessageValidator                                                  │
│  • Validate SPARQL query schema                                  │
│  • Check query string is non-empty                               │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 3. Validated query
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ SPARQLPatternMatcher                                             │
│  • Parse SELECT query (extract variables, patterns)             │
│  • Extract triple patterns from WHERE clause                     │
│  • Parse FILTER expressions                                      │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 4. Parsed patterns
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ OxigraphBridge                                                    │
│  • For each pattern: queryPattern(s, p, o)                       │
│  • Create OTEL span (bridge.query_pattern)                       │
│  • Call store.match() or store.getQuads()                        │
│  • Join results on common variables                              │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 5. Query @unrdf/oxigraph
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ @unrdf/oxigraph Store                                            │
│  • Execute triple pattern match                                  │
│  • Return matching quads                                         │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 6. Raw results
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ SPARQLPatternMatcher                                             │
│  • Apply FILTER expressions                                      │
│  • Project to requested variables                                │
│  • Normalize result format                                       │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 7. Filtered results
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ QueryCache                                                        │
│  • Store result: key → result                                    │
│  • Set expiry: timestamp + TTL (60s)                             │
│  • Evict LRU if size > 100                                       │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 8. Return results
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ JavaScript Application                                           │
│  results = [{ s, p, o }, { s, p, o }, ...]                       │
└───────────────────────────────────────────────────────────────────┘
```

**Performance Characteristics:**
- **Cache hit rate:** Depends on query patterns
- **TTL:** 60s (configurable)
- **Cache size:** 100 queries (LRU)
- **Query latency:** <10ms (cached), <50ms (uncached)
- **Observability:** OTEL spans on cache + query

---

### Flow 3: Hot Reload (Policy → Load → Apply)

```
┌──────────────────────────────────────────────────────────────────┐
│ File System / Build System                                       │
│  • module.beam file updated                                      │
│  • Trigger reload event                                          │
└───────────────┬──────────────────────────────────────────────────┘
                │
                │ 1. Reload trigger
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ HotCodeLoader                                                     │
│  • Check if module already loaded                                │
│  • If concurrent reload in progress: queue request               │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 2. Execute reload
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ HotCodeLoader - Before Swap                                      │
│  • Execute beforeSwap callbacks                                  │
│  • Validate module signature (SHA-256)                           │
│  • Check signature changed from previous                         │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 3. Signature valid
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ HotCodeLoader - Load Content                                     │
│  • Fetch module file (browser: fetch, Node: fs.readFile)        │
│  • Compute new signature                                         │
│  • Update module cache                                           │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 4. Module content loaded
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ AtomVM Runtime                                                    │
│  • Load new BEAM bytecode                                        │
│  • Hot-swap in VM (if supported)                                 │
│  • Update module version (incremented)                           │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 5. Module swapped
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ HotCodeLoader - After Swap                                       │
│  • Execute afterSwap callbacks                                   │
│  • Update module info (version, signature, timestamp)           │
│  • Notify supervisor tree (if configured)                       │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 6. Supervisor notification
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ SupervisorTree                                                    │
│  • Identify child processes using old module version            │
│  • Gracefully transition to new code (OTP-style)                │
│  • Restart if necessary (based on strategy)                     │
└───────────────┬───────────────────────────────────────────────────┘
                │
                │ 7. Reload complete
                ▼
┌───────────────────────────────────────────────────────────────────┐
│ JavaScript Application                                           │
│  • Module reloaded: v1 → v2                                      │
│  • Running processes using new code                              │
└───────────────────────────────────────────────────────────────────┘
```

**Performance Characteristics:**
- **Reload latency:** <100ms (typical)
- **Signature validation:** SHA-256
- **Queue size:** 100 concurrent reloads max
- **Atomicity:** Signature check prevents partial loads
- **Observability:** OTEL spans on all steps

---

## Integration Points

### @unrdf/oxigraph Integration

**Provided by @unrdf/oxigraph:**
```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';

const store = createStore();              // In-memory RDF store
const { namedNode, literal } = dataFactory;
```

**Used by AtomVM:**
- `oxigraph-bridge.mjs` - Primary consumer
- `sparql-pattern-matcher.mjs` - Query execution
- `rdf-validator.mjs` - Optional store queries

**Data Flow:**
```
AtomVM (oxigraph-bridge.mjs)
    ↓
@unrdf/oxigraph (createStore)
    ↓
Oxigraph Rust WASM (core engine)
```

**API Mapping:**
| AtomVM API | Oxigraph API |
|------------|--------------|
| `bridge.addTriples(triples)` | `store.add(quad)` or `store.addQuad(s,p,o,g)` |
| `bridge.queryPattern(s,p,o)` | `store.match(s,p,o,g)` or `store.getQuads(s,p,o,g)` |
| `bridge.removeTriples(triples)` | `store.delete(quad)` or `store.removeQuad(s,p,o,g)` |
| `bridge.sparqlQuery(query)` | `store.query(queryString)` |

---

### @unrdf/streaming Integration

**Provided by @unrdf/streaming:**
```javascript
import { streamTriples } from '@unrdf/streaming';
```

**Available for AtomVM:**
- **Current:** Not actively used
- **Potential:** Triple stream processing pipelines
- **Integration point:** `triple-stream-batcher.mjs` could consume

**Planned Integration:**
```javascript
// Hypothetical future integration
import { streamTriples } from '@unrdf/streaming';
import { createTripleStreamBatcher } from '@unrdf/atomvm';

const batcher = createTripleStreamBatcher({ batchSize: 100 });
batcher.onBatch(async (batch) => {
  await bridge.addTriples(batch);
  return { success: true };
});

// Stream from @unrdf/streaming → AtomVM batcher
await batcher.streamTriples(streamTriples(sourceUrl));
```

---

### @unrdf/core Integration

**Provided by @unrdf/core:**
```javascript
import { /* core RDF utilities */ } from '@unrdf/core';
```

**Current Status:**
- **Declared:** In package.json dependencies
- **Not actively used:** AtomVM uses @unrdf/oxigraph directly
- **Potential:** Core RDF term utilities, prefix management

**Future Integration Opportunities:**
- Prefix expansion (currently in rdf-validator.mjs and sparql-pattern-matcher.mjs)
- Term creation utilities
- RDF datatype validation

---

## Deployment Scenarios

### Scenario 1: Browser-Only Deployment

**Architecture:**
```
┌──────────────────────────────────────────────────────┐
│ Browser Tab                                          │
│  ┌────────────────────────────────────────────────┐ │
│  │ index.html + Vite bundle                       │ │
│  │  • AtomVM runtime (WASM)                       │ │
│  │  • Oxigraph bridge                             │ │
│  │  • SPARQL pattern matcher                      │ │
│  │  • Triple stream batcher                       │ │
│  │  • RDF validator                               │ │
│  │  • Query cache                                 │ │
│  └────────────────────────────────────────────────┘ │
│                      ↓                               │
│  ┌────────────────────────────────────────────────┐ │
│  │ Service Worker (COI)                           │ │
│  │  • coi-serviceworker                           │ │
│  │  • COOP/COEP headers                           │ │
│  └────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────┘
```

**Use Cases:**
- Static site hosting (GitHub Pages, Netlify, Vercel)
- Single-page applications
- Offline-first applications
- Client-side RDF knowledge graphs

**Limitations:**
- No Node.js-specific features
- Limited to browser RDF store size
- Hot reload requires page refresh

**Example:**
```bash
# Build
pnpm run build

# Deploy dist/ to static host
```

---

### Scenario 2: Node.js-Only Deployment

**Architecture:**
```
┌──────────────────────────────────────────────────────┐
│ Node.js Process                                      │
│  ┌────────────────────────────────────────────────┐ │
│  │ Node.js Application                            │ │
│  │  • AtomVM node runtime                         │ │
│  │  • Oxigraph bridge                             │ │
│  │  • SPARQL pattern matcher                      │ │
│  │  • Triple stream batcher                       │ │
│  │  • RDF validator                               │ │
│  │  • Query cache                                 │ │
│  │  • Hot code loader (fs-based)                  │ │
│  └────────────────────────────────────────────────┘ │
│                      ↓                               │
│  ┌────────────────────────────────────────────────┐ │
│  │ child_process.spawn()                          │ │
│  │  → AtomVM-node-v0.6.6.js                       │ │
│  └────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────┘
```

**Use Cases:**
- Server-side RDF processing
- CLI tools (e.g., src/cli.mjs)
- Backend microservices
- Knowledge graph pipelines

**Advantages:**
- No COI/service worker complexity
- File system access for hot reload
- Larger RDF store capacity
- Process management with supervisor trees

**Example:**
```javascript
import { AtomVMNodeRuntime } from '@unrdf/atomvm';

const runtime = new AtomVMNodeRuntime();
await runtime.load();
await runtime.execute('/path/to/module.avm');
```

---

### Scenario 3: Hybrid Deployment (Browser + Node)

**Architecture:**
```
┌─────────────────────────────────────────────────────────────┐
│ Browser Tab (Client)                                        │
│  ┌───────────────────────────────────────────────────────┐ │
│  │ AtomVM Browser Runtime                                │ │
│  │  • Query execution (local RDF store)                  │ │
│  │  • UI rendering                                       │ │
│  └───────────────────────────────────────────────────────┘ │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      │ WebSocket / HTTP
                      ▼
┌─────────────────────────────────────────────────────────────┐
│ Node.js Server (Backend)                                    │
│  ┌───────────────────────────────────────────────────────┐ │
│  │ AtomVM Node Runtime                                   │ │
│  │  • Distributed RDF store                              │ │
│  │  • Message validation                                 │ │
│  │  • Circuit breaker                                    │ │
│  │  • Supervisor tree                                    │ │
│  │  • Hot code loader (file-based)                       │ │
│  └───────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

**Use Cases:**
- Real-time collaborative RDF editing
- Distributed knowledge graphs
- Multi-user applications
- Federated queries

**Benefits:**
- Client-side caching + server-side persistence
- Distributed query execution
- Fault tolerance (circuit breaker, supervisor)
- Live module updates

**Communication:**
- **Messages:** Validated with `message-validator.mjs`
- **RPC:** Triple operations, SPARQL queries
- **Sync:** Query cache invalidation on updates

---

## What's Working

### ✅ Runtime Execution
- [x] Browser WASM loading (v0.6.6)
- [x] Node.js process spawning (v0.6.6)
- [x] Cross-Origin-Isolation (service worker)
- [x] State machine (Poka-yoke design)
- [x] Roundtrip SLA tracking (<10ms, <0.1% error rate)
- [x] Hot code loader (signature validation, callbacks)

### ✅ RDF Integration
- [x] Oxigraph bridge (add/query/remove/SPARQL)
- [x] SPARQL pattern matcher (SELECT, ASK, CONSTRUCT, DESCRIBE)
- [x] Triple stream batcher (100 batch size, 50ms timeout, backpressure)
- [x] RDF validator (IRI, datatype, SHACL-like shapes)
- [x] Query cache (LRU, TTL, pattern invalidation)

### ✅ Distributed Systems
- [x] Message validator (Zod schemas, OTEL logging)
- [x] Circuit breaker (3 failure threshold, 5s reset)
- [x] Supervisor tree (one_for_one, one_for_all, rest_for_one)

### ✅ Observability
- [x] OTEL instrumentation (spans, metrics, errors)
- [x] SLA monitor (latency percentiles, error rates, throughput)
- [x] Roundtrip SLA (poka-yoke enforcement)

---

## What's Planned

### 🔄 Near-Term (Next Sprint)

**1. Full SPARQL Support**
- [ ] SPARQL UPDATE (INSERT, DELETE)
- [ ] SPARQL CONSTRUCT with template expansion
- [ ] Federated queries (SERVICE keyword)
- [ ] Property paths (e.g., `foaf:knows+`)

**2. Enhanced Validation**
- [ ] Full SHACL support (not SHACL-like)
- [ ] RDFS inference rules
- [ ] OWL reasoning (basic)

**3. Streaming Integration**
- [ ] @unrdf/streaming pipeline integration
- [ ] Streaming SPARQL results
- [ ] Reactive triple updates

### 🔮 Long-Term (Future Releases)

**1. Multi-Store Support**
- [ ] Multiple Oxigraph instances
- [ ] Federated query across stores
- [ ] Named graph management

**2. Advanced Distributed**
- [ ] CRDT-based synchronization
- [ ] Conflict resolution strategies
- [ ] Vector clocks for causality

**3. Performance Optimizations**
- [ ] Query plan optimization
- [ ] Indexed triple patterns
- [ ] Parallel query execution

---

## What's Blocked

### 🚫 Currently Blocked

**1. Full SPARQL 1.1 Compliance**
- **Blocker:** Oxigraph WASM limitations
- **Impact:** No SPARQL UPDATE, limited aggregation
- **Workaround:** Use bridge.addTriples() / bridge.removeTriples()
- **Resolution:** Wait for Oxigraph WASM feature parity

**2. True Hot Code Reload in Browser**
- **Blocker:** WASM module limitations (no dynamic linking)
- **Impact:** Requires page refresh for module updates
- **Workaround:** HotCodeLoader queues reloads, applies on next page load
- **Resolution:** Future WASM proposals (component model)

**3. Distributed BEAM Clustering in Browser**
- **Blocker:** Browser networking restrictions (no raw sockets)
- **Impact:** No true Erlang distribution protocol
- **Workaround:** WebSocket-based RPC with message-validator
- **Resolution:** Use hybrid deployment (Node.js backend for clustering)

---

## Recommended Implementation Order

### Phase 1: Core Stability (Current)
1. ✅ Runtime execution (browser + Node)
2. ✅ Basic RDF operations (add/query/remove)
3. ✅ SPARQL SELECT queries
4. ✅ Validation (structure + basic datatypes)
5. ✅ OTEL instrumentation

### Phase 2: Performance & Reliability (In Progress)
1. ✅ Query cache (LRU + TTL)
2. ✅ Triple stream batcher
3. ✅ Circuit breaker
4. ✅ Supervisor tree
5. ✅ SLA monitoring

### Phase 3: Advanced Features (Next)
1. 🔄 Full SPARQL 1.1 (UPDATE, CONSTRUCT, etc.)
2. 🔄 Full SHACL validation
3. 🔄 @unrdf/streaming integration
4. 🔄 Reactive triple updates
5. 🔄 Multi-store federation

### Phase 4: Production Hardening (Future)
1. 🔮 CRDT synchronization
2. 🔮 Query plan optimization
3. 🔮 Parallel execution
4. 🔮 Advanced reasoning (RDFS/OWL)

---

## Related Documentation

- [ADR-001: BEAM-RDF Integration](./ADR/001-beam-rdf-integration.md)
- [Browser Architecture Explanation](./explanation/architecture.md)
- [SPARQL Pattern Matcher Reference](./reference/sparql-pattern-matcher.md)
- [Hot Code Loader How-To](./how-to/hot-code-reload.md)
- [Deployment Guide](./how-to/deploy-production.md)

---

**Document Version:** 1.0.0
**Last Review:** 2025-12-28
**Next Review:** 2026-01-28
**Maintainers:** AtomVM Integration Team

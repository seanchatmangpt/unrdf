# UNRDF Core Architecture Analysis

**Date:** 2025-12-28
**Scope:** Core packages (core, oxigraph, hooks, streaming)
**Total Files Analyzed:** 1,468 MJS files
**Total LoC (Core Packages):** ~26,793 lines

---

## Executive Summary

UNRDF is a modular RDF knowledge graph platform built on a **hybrid dual-store architecture** combining Oxigraph (SPARQL engine) with N3 (streaming parser). The architecture demonstrates strong separation of concerns through a 4-layer substrate design with explicit module boundaries and dependency inversion.

**Key Architectural Decisions:**
- Oxigraph as primary store (SPARQL performance)
- N3 isolated to streaming layer (SAX-like parsing)
- Hook-based policy framework (ECA pattern)
- Event-driven change feeds (pub/sub)
- Zod for runtime validation (type safety)

---

## 1. Core Abstractions

### 1.1 RDF Store Layer (`@unrdf/core`, `@unrdf/oxigraph`)

#### Primary Store: OxigraphStore

**Location:** `/home/user/unrdf/packages/oxigraph/src/store.mjs`

**Responsibilities:**
- SPARQL query execution (SELECT, ASK, CONSTRUCT, DESCRIBE)
- Quad CRUD operations (add, delete, match)
- RDF serialization/deserialization (Turtle, N-Triples, JSON-LD)
- Native index support (SPO, POS, OSP)

**Key Interface:**
```javascript
class OxigraphStore {
  add(quad)                        // Add single quad
  delete(quad)                     // Remove single quad
  match(s?, p?, o?, g?)           // Pattern matching
  query(sparql, options)          // SPARQL execution
  update(sparql, options)         // SPARQL UPDATE
  load(data, {format})            // Bulk import
  dump({format})                  // Bulk export
  size                            // Quad count
}
```

**Design Pattern:** **Adapter Pattern** - Wraps native Oxigraph C++ bindings with JavaScript-friendly API

#### Secondary Store: UnrdfStore (High-Level API)

**Location:** `/home/user/unrdf/packages/core/src/rdf/unrdf-store.mjs`

**Enhancements over OxigraphStore:**
- Synchronous query execution (reactive computed() support)
- Version tracking (reactivity integration)
- Bulk operations (bulkAdd, bulkRemove)
- Transaction support (ACID guarantees)
- Query result formatting (JSON, bindings, quads)

**Key Interface:**
```javascript
class UnrdfStore extends OxigraphStore {
  query(sparql, options)          // Sync SPARQL
  queryAsync(sparql, options)     // Async wrapper
  bulkAdd(quads[])                // Batch insert
  bulkRemove(quads[])             // Batch delete
  transaction(fn)                 // ACID transaction
  _version                        // Reactivity tracking
}
```

**Design Pattern:** **Decorator Pattern** - Adds reactivity/transactions to base store

#### DataFactory (Term Creation)

**Location:** `/home/user/unrdf/packages/oxigraph/src/index.mjs`

**Exported Functions:**
```javascript
dataFactory = {
  namedNode(iri)                  // <http://example.org/alice>
  literal(value, lang|datatype)   // "Alice"@en / "30"^^xsd:integer
  blankNode(id?)                  // _:b1
  defaultGraph()                  // Default graph
  quad(s, p, o, g?)              // Complete quad
}
```

**Constraint:** All application code MUST use Oxigraph DataFactory. N3 DataFactory forbidden outside streaming layer.

---

### 1.2 Knowledge Hooks Layer (`@unrdf/hooks`)

#### Hook Definition DSL

**Location:** `/home/user/unrdf/packages/hooks/src/hooks/define-hook.mjs`

**Purpose:** Policy-as-code framework for validation, transformation, and side effects

**Hook Schema:**
```javascript
HookSchema = {
  name: string,                   // Unique identifier
  trigger: enum,                  // 'beforeAdd' | 'afterAdd' | 'beforeRemove' | 'afterRemove'
  validate?: (quad) => boolean,   // Validation predicate
  transform?: (quad) => Quad,     // Quad transformation
  priority?: number,              // Execution order (0-100)
  enabled?: boolean               // Runtime toggle
}
```

**Design Pattern:** **ECA (Event-Condition-Action)** - Declarative reactive rules

#### Hook Execution Engine

**Location:** `/home/user/unrdf/packages/hooks/src/hooks/hook-executor.mjs`

**Execution Flow:**
1. Execute validation (if present)
2. Short-circuit on failure
3. Execute transformation (if present)
4. Chain transformations (output → input)
5. Return final quad + metadata

**Key Functions:**
```javascript
executeHook(hook, quad, options)        // Single hook
executeHookChain(hooks[], quad)         // Sequential chain
executeHooksByTrigger(hooks, trigger, quad)  // Filter + execute
executeBatch(quads[])                   // Parallel batching
```

**Optimizations:**
- Skip Zod validation if `_validated` flag set (defineHook pre-validates)
- Object pooling via QuadPool (zero-allocation transforms)
- JIT compilation via hook-chain-compiler (hot paths)

**Design Pattern:** **Chain of Responsibility** + **Command Pattern**

#### KnowledgeHookEngine (Advanced Executor)

**Location:** `/home/user/unrdf/packages/hooks/src/hooks/knowledge-hook-engine.mjs`

**Performance Features:**
- Oxigraph store caching (50-70% latency reduction)
- Condition evaluation caching (40-50% latency reduction)
- File pre-loading (20-30% latency reduction)
- Dependency-based parallel batching (30-50% latency reduction)
- Batched OTEL telemetry (10-15% latency reduction)

**Total Impact:** 80-92% latency reduction (measured)

**Caching Components:**
```javascript
StoreCache        // LRU cache for Oxigraph stores (max 10)
ConditionCache    // TTL cache for SPARQL conditions (60s)
BatchedTelemetry  // Buffered OTEL spans (10ms flush)
```

**Design Pattern:** **Facade Pattern** + **Cache-Aside Pattern**

---

### 1.3 Streaming Layer (`@unrdf/streaming`)

#### Change Feed (Event Emitter)

**Location:** `/home/user/unrdf/packages/streaming/src/streaming/change-feed.mjs`

**Purpose:** Real-time quad change notifications

**Change Event Schema:**
```javascript
ChangeEvent = {
  type: 'add' | 'remove' | 'update',
  quad: { subject, predicate, object, graph },
  timestamp: number,
  metadata?: Record<string, any>
}
```

**Architecture:**
- EventTarget-based (browser/Node compatible)
- Ring buffer history (10,000 changes max)
- Store monkey-patching (addQuad/removeQuad interception)
- Subscriber management (Set-based)

**Key Functions:**
```javascript
createChangeFeed(store?, config)
  .emitChange(change)              // Publish change
  .subscribe(callback)             // Register subscriber
  .getHistory({since?, limit?})   // Query history
  .replay(subscriber, {since?})   // Replay events
```

**Design Pattern:** **Observer Pattern** + **Event Sourcing**

#### Stream Processor

**Location:** `/home/user/unrdf/packages/streaming/src/streaming/stream-processor.mjs`

**Purpose:** Backpressure-aware RDF parsing

**Integration:**
- N3 StreamParser (SAX-like parsing)
- Node.js Transform streams
- Automatic backpressure handling
- Chunk-based processing

**Design Pattern:** **Pipeline Pattern** + **Producer-Consumer**

#### Real-Time Validator

**Location:** `/home/user/unrdf/packages/streaming/src/streaming/real-time-validator.mjs`

**Purpose:** Streaming SHACL validation

**Validation Modes:**
```javascript
ValidationMode = {
  STRICT: 'strict',      // Fail on first violation
  PERMISSIVE: 'permissive',  // Collect all violations
  MONITORING: 'monitoring'   // Log only (no rejection)
}
```

**Design Pattern:** **Strategy Pattern** + **Validator Pattern**

---

## 2. Key Architectural Patterns

### 2.1 Dual-Store Hybrid Architecture

**Problem:** Oxigraph excels at SPARQL but lacks streaming parser. N3 has excellent streaming but slower SPARQL.

**Solution:** Use both, with strict boundaries:
- **Oxigraph:** Primary store for all quad operations + SPARQL
- **N3:** Isolated to streaming layer (`n3-justified-only.mjs`)

**Enforcement Mechanism:**
```javascript
// ONLY allowed in packages/core/src/rdf/n3-justified-only.mjs
import { Parser, Writer, StreamParser, StreamWriter } from 'n3';

// All other modules FORBIDDEN from importing 'n3' directly
// Grep validation: grep "from 'n3'" should return ONLY n3-justified-only.mjs
```

**Benefits:**
- Best-of-breed combination
- Clear separation of concerns
- Migration path from N3 → Oxigraph complete

---

### 2.2 Hook-Based Policy Framework

**Pattern:** **Policy-as-Code** using ECA (Event-Condition-Action)

**Architecture:**
```
Trigger (Event) → Condition (Filter) → Action (Execute)
    ↓                    ↓                   ↓
beforeAdd          quad.subject IRI?    Validate/Transform
afterAdd           SPARQL ASK query     Side effect
beforeRemove       Custom predicate     Audit log
afterRemove        Always true          Cleanup
```

**Composability:**
```javascript
// Define hooks independently
const iriValidator = defineHook({ name: 'validateIRI', trigger: 'beforeAdd', validate: ... });
const namespaceFixer = defineHook({ name: 'normalizeNS', trigger: 'beforeAdd', transform: ... });

// Compose into chain
executeHookChain([iriValidator, namespaceFixer], quad);
```

**Design Pattern:** **Interceptor Pattern** + **Aspect-Oriented Programming (AOP)**

---

### 2.3 Reactive Synchronous Queries

**Problem:** Vue/React computed() requires synchronous functions. Traditional SPARQL is async.

**Solution:** Oxigraph supports synchronous query execution natively.

**Implementation:**
```javascript
class UnrdfStore {
  query(sparql, options) {
    // Synchronous SPARQL execution
    const result = this._store.query(sparql, oxigraphOptions);
    return this._formatResult(result, queryType, options);
  }

  async queryAsync(sparql, options) {
    // Async wrapper for compatibility
    return this.query(sparql, options);
  }
}
```

**Usage:**
```javascript
// Vue 3 computed property
const nameCount = computed(() => {
  return store.query(`SELECT (COUNT(?name) as ?count) WHERE { ?s foaf:name ?name }`);
});
```

**Design Pattern:** **Sync/Async Adapter Pattern**

---

### 2.4 Batched Telemetry (Performance Optimization)

**Problem:** OTEL span creation is expensive (200-500μs per span)

**Solution:** Batch span creation + async flush

**Implementation:**
```javascript
class BatchedTelemetry {
  constructor(tracer, flushInterval = 10) {
    this.spans = [];
    this.flushInterval = flushInterval;
  }

  startTransactionSpan(name, attrs) {
    const span = this.tracer.startSpan(name);
    span.setAttributes(attrs);
    this.spans.push(span);

    // Flush every 10ms
    if (this.spans.length >= this.flushInterval) {
      this.flush();
    }

    return span;
  }
}
```

**Impact:** 10-15% latency reduction on hook execution

**Design Pattern:** **Buffering Pattern** + **Write-Behind Cache**

---

### 2.5 Error Handling Strategy

**Approach:** **Defensive Programming with Poka-Yoke Guards**

**Layers:**
1. **Schema Validation (Zod):** All inputs validated at boundary
2. **Type Guards:** Runtime checks for critical invariants
3. **Poka-Yoke Warnings:** Detect common mistakes (non-boolean validation returns, pooled quad leaks)
4. **Structured Errors:** Rich error context with stack traces

**Example:**
```javascript
// Poka-Yoke guard in hook executor
if (typeof validationResult !== 'boolean') {
  console.warn(
    `[POKA-YOKE] Hook "${hook.name}": validate() returned ${typeof validationResult}, expected boolean. Coercing to boolean.`
  );
  result.warning = `Non-boolean validation return coerced to boolean`;
}

// Pooled quad leak detection
if (transformed._pooled && options.warnPooledQuads !== false) {
  console.warn(
    `[POKA-YOKE] Hook "${hook.name}": returned pooled quad. Clone before storing to prevent memory issues.`
  );
  result.warning = 'Pooled quad returned - consider cloning';
}
```

**Design Pattern:** **Fail-Fast** + **Graceful Degradation**

---

## 3. Module Boundaries and Interfaces

### 3.1 Dependency Graph

```
@unrdf/streaming
    ↓ depends on
@unrdf/hooks
    ↓ depends on
@unrdf/core
    ↓ depends on
@unrdf/oxigraph (+ n3 for streaming)
```

**Key Constraint:** Dependencies flow downward only (no circular dependencies)

---

### 3.2 Package Exports

#### `@unrdf/core` (v6.0.0-alpha.1)

**Exports:**
```javascript
// Synchronous API (Primary)
export { UnrdfStore, createUnrdfStore } from './rdf/unrdf-store.mjs';
export { executeQuerySync, executeSelectSync, executeAskSync, executeConstructSync } from './sparql/executor-sync.mjs';

// Async API (Backward Compatibility)
export { createStore, addQuad, removeQuad, getQuads } from './rdf/store.mjs';
export { executeQuery, executeSelect, executeConstruct, executeAsk } from './sparql/executor.mjs';

// DataFactory
export { namedNode, literal, blankNode, variable, defaultGraph, quad } from './rdf/store.mjs';

// Utilities
export { RDF, RDFS, OWL, XSD, FOAF, DCTERMS, SKOS, COMMON_PREFIXES } from './constants.mjs';
export { QuadSchema, StoreSchema, validateQuad, validateStore } from './validation/index.mjs';

// Error Handling
export { UnrdfError, ValidationError, QueryError, StoreError, createError } from './errors.mjs';
export { retry, CircuitBreaker, fallback, withTimeout, RateLimiter } from './recovery.mjs';

// Debug
export { DebugLogger, PerformanceTracker, trace, formatBytes } from './debug.mjs';
```

---

#### `@unrdf/oxigraph` (v5.0.1)

**Exports:**
```javascript
export { OxigraphStore, createStore } from './store.mjs';
export { dataFactory } from './index.mjs';
  // dataFactory.namedNode, .literal, .blankNode, .defaultGraph, .quad
```

**Dependencies:**
- `oxigraph` (native C++ bindings)
- `zod` (validation)

---

#### `@unrdf/hooks` (v5.0.1)

**Exports:**
```javascript
// Hook Definition
export { defineHook, isValidHook, HookSchema } from './hooks/define-hook.mjs';

// Hook Execution
export { executeHook, executeHookChain, executeHooksByTrigger, executeBatch } from './hooks/hook-executor.mjs';

// Hook Management
export { createHookRegistry, registerHook, unregisterHook, getHook, listHooks } from './hooks/hook-management.mjs';

// Built-in Hooks
export { builtinHooks, validateSubjectIRI, normalizeNamespace, trimLiterals } from './hooks/builtin-hooks.mjs';

// Advanced Executor
export { KnowledgeHookEngine } from './hooks/knowledge-hook-engine.mjs';

// Optimization
export { compileHookChain, clearCompiledChainCache } from './hooks/hook-chain-compiler.mjs';
export { QuadPool, quadPool, createPooledTransform } from './hooks/quad-pool.mjs';

// Policy Packs
export { PolicyPack, PolicyPackManager, createPolicyPackFromDirectory } from './hooks/policy-pack.mjs';
```

**Dependencies:**
- `@unrdf/core` (store operations)
- `@unrdf/oxigraph` (store implementation)
- `zod` (validation)

---

#### `@unrdf/streaming` (v5.0.1)

**Exports:**
```javascript
// Change Feed
export { createChangeFeed } from './streaming/change-feed.mjs';

// Subscription Manager
export { createSubscriptionManager } from './streaming/subscription-manager.mjs';

// Stream Processor
export { createStreamProcessor } from './streaming/stream-processor.mjs';

// Real-time Validator
export { RealTimeValidator, createRealTimeValidator, ValidationMode } from './streaming/real-time-validator.mjs';

// Sync Protocol
export { createSyncMessage, parseSyncMessage, calculateChecksum, mergeSyncMessages } from './sync-protocol.mjs';

// RDF Stream Parser
export { RDFStreamParser, createRDFStreamParser, parseRDFStream } from './rdf-stream-parser.mjs';

// Performance Monitor
export { PerformanceMonitor, createPerformanceMonitor } from './performance-monitor.mjs';
```

**Dependencies:**
- `@unrdf/core` (RDF operations)
- `@unrdf/hooks` (validation hooks)
- `@unrdf/oxigraph` (store)
- `@opentelemetry/api` (observability)
- `ws` (WebSocket support)
- `lru-cache` (caching)
- `zod` (validation)

---

### 3.3 Interface Contracts

#### Store Interface Contract

All stores MUST implement:
```javascript
interface Store {
  add(quad): void
  delete(quad): void
  match(s?, p?, o?, g?): Quad[]
  query(sparql, options?): Results
  size: number
}
```

**Implementations:**
- `OxigraphStore` (base implementation)
- `UnrdfStore` (enhanced with reactivity/transactions)

---

#### Hook Interface Contract

All hooks MUST conform to:
```javascript
interface Hook {
  name: string
  trigger: 'beforeAdd' | 'afterAdd' | 'beforeRemove' | 'afterRemove'
  validate?: (quad: Quad) => boolean
  transform?: (quad: Quad) => Quad
  priority?: number      // 0-100 (default: 50)
  enabled?: boolean      // default: true
}
```

**Validation:**
- `defineHook()` validates via Zod schema
- Sets `_validated` flag to skip future checks (performance)

---

#### Change Feed Interface Contract

```javascript
interface ChangeFeed {
  emitChange(change: ChangeEvent): void
  subscribe(callback: (change: ChangeEvent) => void): () => void
  getHistory(options?: { since?: number, limit?: number }): ChangeEvent[]
  replay(subscriber: Function, options?: { since?: number }): void
}
```

**Event Contract:**
```javascript
interface ChangeEvent {
  type: 'add' | 'remove' | 'update'
  quad: { subject, predicate, object, graph }
  timestamp: number
  metadata?: Record<string, any>
}
```

---

## 4. Architectural Quality Attributes

### 4.1 Performance

**Measured Characteristics:**
- **Hook Execution:** 80-92% latency reduction via caching (KnowledgeHookEngine)
- **SPARQL Queries:** Native Oxigraph performance (~1-10ms for simple queries)
- **Streaming Parser:** Backpressure-aware, handles GB-scale files
- **Change Feed:** Ring buffer (O(1) append, O(n) history query)

**Optimization Techniques:**
- Object pooling (QuadPool)
- JIT compilation (hook-chain-compiler)
- LRU caching (StoreCache)
- Batched telemetry (10ms flush intervals)

---

### 4.2 Scalability

**Horizontal:**
- Stateless hook execution (parallelizable)
- Event-driven change feeds (pub/sub scales)

**Vertical:**
- Oxigraph native indexes (SPO, POS, OSP)
- Streaming parser (constant memory for large files)

---

### 4.3 Maintainability

**Strengths:**
- Clear module boundaries (4-layer substrate)
- Single Responsibility Principle (each package has one purpose)
- Dependency Inversion (interfaces, not implementations)
- 100% JSDoc coverage (type hints)
- 400+ ESLint rules enforced

**Technical Debt:**
- N3 migration incomplete (still used in streaming layer)
- Legacy async API maintained for backward compatibility
- Some circular type references in schemas

---

### 4.4 Testability

**Test Coverage:**
- Target: 80%+ coverage, 100% pass rate
- Pattern: Vitest framework with JSDoc type checking
- Approach: Pure functions favored (no OTEL in business logic)

**Test Strategy:**
```javascript
// Good: Pure function (easy to test)
export function validateQuad(quad) {
  return QuadSchema.parse(quad);
}

// Avoid: Tight coupling to OTEL
export function validateQuad(quad) {
  const span = tracer.startSpan('validate');  // Hard to test
  // ...
}
```

---

### 4.5 Security

**Defense Mechanisms:**
- Zod validation at all boundaries (untrusted input)
- Sandboxed hook execution (worker threads, VM isolation)
- SPARQL injection protection (parameterized queries)
- File resolver with content-addressed hashing

**Poka-Yoke Guards:**
- Non-boolean validation return detection
- Pooled quad leak warnings
- Type coercion warnings

---

## 5. Architectural Anti-Patterns Avoided

### 5.1 God Object

**Avoided By:** Single Responsibility Principle
- `OxigraphStore`: Only store operations
- `KnowledgeHookEngine`: Only hook execution
- `ChangeFeed`: Only event broadcasting

---

### 5.2 Circular Dependencies

**Avoided By:** Strict dependency flow
```
streaming → hooks → core → oxigraph
```
No upward or lateral dependencies.

---

### 5.3 Tight Coupling

**Avoided By:**
- Dependency injection (constructor-based)
- Interface contracts (not concrete types)
- DataFactory abstraction (swap implementations)

---

### 5.4 Leaky Abstractions

**N3 Isolation:**
- N3 imports ONLY in `n3-justified-only.mjs`
- Application code never sees N3 types
- Streaming layer converts N3 → Oxigraph quads

---

## 6. Future Architectural Considerations

### 6.1 Complete N3 Removal

**Current State:** N3 isolated to streaming layer
**Target State:** Replace N3 with Oxigraph native streaming parser (when available)

**Migration Path:**
1. Monitor Oxigraph releases for streaming API
2. Create adapter in `n3-justified-only.mjs`
3. Swap implementations behind interface
4. Remove N3 dependency

---

### 6.2 Distributed Store

**Challenge:** Oxigraph is in-memory only
**Potential Solutions:**
- Persist via RDF serialization (Turtle/N-Triples)
- Integrate persistent store (e.g., Oxigraph Server, RDFox, Blazegraph)
- Build change-feed based replication (CRDT approach)

---

### 6.3 Query Optimization

**Opportunities:**
- Query plan caching (prepareQuerySync result reuse)
- Materialized views (precompute common joins)
- Statistics-based optimization (cardinality estimates)

---

## 7. Architectural Decision Records (ADRs)

### ADR-001: Use Oxigraph as Primary Store

**Context:** Need high-performance SPARQL engine
**Decision:** Adopt Oxigraph (Rust-based, native bindings)
**Status:** Accepted
**Consequences:**
- ✅ 10-100x faster SPARQL than pure JS
- ✅ Native indexing (SPO, POS, OSP)
- ❌ In-memory only (no built-in persistence)
- ❌ Limited streaming parser

---

### ADR-002: Isolate N3 to Streaming Layer

**Context:** N3 excellent for streaming, but slower SPARQL
**Decision:** Use N3 ONLY for streaming parsing/writing
**Status:** Accepted
**Consequences:**
- ✅ Best-of-breed combination
- ✅ Clear migration path
- ✅ No N3 leakage into application code
- ⚠️ Two dependencies instead of one

---

### ADR-003: Hook-Based Policy Framework

**Context:** Need extensible validation/transformation
**Decision:** Implement ECA (Event-Condition-Action) hooks
**Status:** Accepted
**Consequences:**
- ✅ Declarative policy-as-code
- ✅ Composable (hook chains)
- ✅ Testable (pure functions)
- ❌ Performance overhead (mitigated via caching)

---

### ADR-004: Synchronous Query Execution

**Context:** Reactive frameworks (Vue/React) need sync APIs
**Decision:** Expose synchronous `query()` method
**Status:** Accepted
**Consequences:**
- ✅ Enables computed() / useMemo()
- ✅ Simpler API for simple cases
- ⚠️ Blocks event loop (use queryAsync for large results)

---

### ADR-005: Zod for Runtime Validation

**Context:** Need type safety in JavaScript
**Decision:** Use Zod for all schemas + validation
**Status:** Accepted
**Consequences:**
- ✅ Runtime type checking
- ✅ Automatic TypeScript type inference
- ✅ Composable schemas
- ❌ Performance cost (mitigated via fast-path skips)

---

## 8. Summary and Recommendations

### 8.1 Architectural Strengths

1. **Clear Separation of Concerns:** 4-layer substrate with explicit boundaries
2. **Best-of-Breed Combination:** Oxigraph + N3 hybrid
3. **Performance Optimizations:** Caching, batching, object pooling (80-92% latency reduction)
4. **Extensibility:** Hook framework enables custom policies without core changes
5. **Event-Driven:** Change feeds support real-time updates

---

### 8.2 Areas for Improvement

1. **Persistence Layer:** Oxigraph in-memory only (add serialization or external store)
2. **N3 Migration:** Complete removal when Oxigraph streaming available
3. **Query Optimization:** Add query plan caching, materialized views
4. **Documentation:** ADRs exist but not centralized (create `/docs/architecture/decisions/`)

---

### 8.3 Recommended Next Steps

1. **Short-term (Q1 2026):**
   - Add query plan caching to UnrdfStore
   - Implement RDF serialization for persistence
   - Centralize ADRs in `/docs/architecture/decisions/`

2. **Medium-term (Q2-Q3 2026):**
   - Monitor Oxigraph for streaming parser
   - Build distributed change-feed replication
   - Add statistics-based query optimization

3. **Long-term (Q4 2026+):**
   - Evaluate persistent store integration (Oxigraph Server, RDFox)
   - Consider CRDT-based distributed store
   - Explore WebAssembly deployment for browser

---

## Appendix A: File Structure

```
packages/
├── core/                       # RDF operations, SPARQL, utilities
│   └── src/
│       ├── rdf/
│       │   ├── store.mjs      # Functional API (async)
│       │   ├── unrdf-store.mjs # Class API (sync + reactivity)
│       │   ├── n3-justified-only.mjs # N3 isolation layer
│       │   └── canonicalize.mjs
│       ├── sparql/
│       │   ├── executor.mjs    # Async SPARQL
│       │   └── executor-sync.mjs # Sync SPARQL
│       ├── validation/
│       ├── utils/
│       └── index.mjs           # Public API
│
├── oxigraph/                   # Oxigraph wrapper
│   └── src/
│       ├── store.mjs           # OxigraphStore class
│       ├── types.mjs           # TypeScript types
│       └── index.mjs           # createStore + dataFactory
│
├── hooks/                      # Policy framework
│   └── src/
│       └── hooks/
│           ├── define-hook.mjs         # Hook DSL
│           ├── hook-executor.mjs       # Execution engine
│           ├── knowledge-hook-engine.mjs # Advanced executor
│           ├── builtin-hooks.mjs       # Standard validations
│           ├── policy-pack.mjs         # Versioned policies
│           ├── quad-pool.mjs           # Object pooling
│           └── hook-chain-compiler.mjs # JIT optimization
│
└── streaming/                  # Real-time updates
    └── src/
        └── streaming/
            ├── change-feed.mjs         # Event emitter
            ├── stream-processor.mjs    # Backpressure handling
            ├── real-time-validator.mjs # SHACL streaming
            └── subscription-manager.mjs # Pub/sub management
```

---

## Appendix B: Key Metrics

| Metric | Value | Source |
|--------|-------|--------|
| Total MJS Files | 1,468 | `find packages -name "*.mjs" \| wc -l` |
| Total LoC (Core) | ~26,793 | `wc -l packages/{core,oxigraph,hooks,streaming}/**/*.mjs` |
| Test Coverage Target | 80%+ | package.json scripts |
| Hook Latency Reduction | 80-92% | KnowledgeHookEngine benchmarks |
| Change Feed History | 10,000 events | Ring buffer size |
| Store Cache Size | 10 stores | StoreCache maxSize |
| Condition Cache TTL | 60s | ConditionCache default |

---

## Appendix C: Technology Stack

| Layer | Technology | Justification |
|-------|-----------|---------------|
| RDF Store | Oxigraph (Rust) | Native performance, SPARQL compliance |
| Streaming Parser | N3 (JavaScript) | SAX-like parsing, backpressure support |
| Validation | Zod | Runtime type safety, composable schemas |
| Observability | OpenTelemetry | Vendor-neutral, standard protocol |
| Testing | Vitest | Fast, ESM native, Vite integration |
| Package Manager | pnpm | Workspace support, disk efficiency |
| Module System | ESM (MJS) | Standard, tree-shakeable |
| Type System | JSDoc + Zod | No build step, runtime validation |

---

**End of Report**

# Phase 2 Plan: Latency Collapse & Query Caching

## 1. Query Caching Layer Implementation

### 1.1 `packages/atomvm` Refactoring
*   **Goal**: Replace ad-hoc `Map`-based caching in `sparql-pattern-matcher.mjs` with the robust `QueryCache` class.
*   **Actions**:
    *   Update `SparqlPatternMatcher` to initialize a `QueryCache` instance with configurable LRU size and TTL.
    *   Implement query result deduplication (request coalescing) to prevent redundant executions of identical simultaneous queries.
    *   Integrate `QueryCache.invalidate(pattern)` with `OxigraphStore` mutations to ensure cache consistency.
    *   Add OTEL instrumentation to track cache hit/miss/eviction metrics.

### 1.2 `packages/kgc-4d` Enhancements
*   **Goal**: Implement "Snapshot Cache Invalidation" and optimize temporal queries.
*   **Actions**:
    *   **Current-State Caching**: Use `QueryCache` for `TemporalSPARQL._queryCurrentTime`. Invalidate cache on `KGCStore.appendEvent()`.
    *   **Intermediate State Caching**: Update `HistoryReconstructor` to use `QueryCache` for caching results of sub-queries during reconstruction.
    *   **Snapshot Invalidation**: Implement O(1) pointer updates in the System graph when freezing, as identified in Gap 7.
    *   **Deduplication**: Implement query coalescing in `TemporalSPARQL` to handle high-frequency time-travel queries.

---

## 2. Incremental Validation: `ValidationMode.DELTA`

### 2.1 Design
*   **Concept**: Instead of validating the entire graph after every mutation, only validate the SHACL shapes that could be affected by the added/deleted triples.
*   **Mode Definition**:
    ```javascript
    const ValidationMode = {
      FULL: 'full',   // Validate everything (default for batch loads)
      DELTA: 'delta', // Validate only affected shapes (default for appendEvent)
      LAZY: 'lazy'    // Validate on-demand (query-time)
    };
    ```
*   **Implementation**:
    *   **Shape-to-Predicate Mapping**: Maintain an index of which predicates are used in which SHACL shapes.
    *   **Surgical Validation**: When `appendEvent(deltas)` is called, identify the set of affected shapes based on the predicates in `deltas`.
    *   **Targeted Checks**: Re-validate only the subjects involved in the deltas against the identified shapes.

### 2.2 `JitShaclValidator` Integration
*   Compile SHACL shapes into optimized JS functions that accept a `delta` and `subjectIri`.
*   Support `validateMutation(subjectIri, delta)` which performs O(1) or O(k) checks (where k is number of constraints on the predicate).

---

## 3. Benchmark Suite: R Ratio Measurement

### 3.1 Definition
The **R ratio** (Reasoning to Event) is defined as:
$$R = \frac{T_{reasoning}}{T_{event}}$$
Where:
*   $T_{reasoning}$: Time spent on SHACL validation and inference.
*   $T_{event}$: Total time to process a state transition (appendEvent).

### 3.2 Benchmark Design
*   **Suite Name**: `latency-collapse-benchmark.mjs`
*   **Metrics**:
    *   Event processing latency (ms).
    *   Validation overhead (ms).
    *   $R$ ratio per event type.
    *   Cache hit rate and its impact on $R$.
*   **Scenarios**:
    *   Single triple addition (Simple shape).
    *   Batch addition (Complex shapes).
    *   Repeated queries on stable state vs. fluctuating state.
*   **Pass Criteria**: $R < 0.1$ ($10\times$ faster processing than reasoning).

---

## 4. Adversarial Review

### 4.1 State Synchronization Risks
**Critique**: "Does query caching introduce state synchronization risks in multi-node environments?"
**Response**:
*   **Risk**: If Node A updates the store and Node B has a cached query result for the same pattern, Node B will serve stale data.
*   **Mitigation**:
    1.  **Temporal Immutability**: For KGC-4D time-travel queries at a fixed `timestampNs`, the state is immutable. Caching is 100% safe.
    2.  **Vector Clock Invalidation**: For "Current" state, use Vector Clocks to detect if a cache entry is stale relative to the known global state.
    3.  **Short TTL & Invalidation Propagation**: Implement a broadcast mechanism (or rely on the peer-to-peer layer) to propagate cache invalidation signals for shared prefixes.

### 4.2 10ms SLA Realism
**Critique**: "Is 10ms SLA realistic for complex SHACL shapes?"
**Response**:
*   **Risk**: Complex SHACL shapes with deep property paths or global constraints (`sh:closed`, `sh:minCount`) cannot be validated in 10ms on a full graph.
*   **Mitigation**:
    1.  **Incremental logic**: `ValidationMode.DELTA` reduces the problem from $O(N)$ (graph size) to $O(\Delta)$ (change size).
    2.  **JIT Compilation**: Compiling shapes into native WASM/JS eliminates interpreter overhead.
    3.  **Asynchronous Hard Constraints**: Move non-critical validation to a background "Governance" layer while keeping 10ms "Safety" checks synchronous.
    4.  **SLA Guard**: If validation exceeds 5ms, the system triggers a `ValidationBypass` (with audit log) or `CircuitBreaker` to maintain responsiveness, treating it as a "Soft Violation".

---

## 5. Deliverables & Success Criteria
1.  `packages/atomvm/src/query-cache.mjs` integrated into `SparqlPatternMatcher`.
2.  `packages/kgc-4d/src/validation/jit-shacl.mjs` implementing `DELTA` mode.
3.  `benchmarks/r-ratio-suite.mjs` reporting $R \ll 1$ in the browser.
4.  Latency for cached SPARQL queries < 5ms P95.

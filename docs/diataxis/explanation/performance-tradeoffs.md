# Explanation 04: Performance Tradeoffs

**Objective:** Understand the performance characteristics, trade-offs, and optimization strategies in UNRDF's architecture.

**Audience:** Performance engineers and architects

**Estimated Reading Time:** 35 minutes

---

## Introduction

UNRDF makes deliberate performance trade-offs to achieve its goals of governance, verification, and cross-runtime compatibility. This article explains these trade-offs, their rationale, and when to optimize.

---

## Performance Philosophy

**[Placeholder - Content to be filled]**

### Core Principle: Correctness Over Speed

UNRDF prioritizes:
1. **Correctness** - Verified, auditable operations
2. **Developer Experience** - Clear, predictable APIs
3. **Performance** - Fast enough for most use cases

**Not optimized for:**
- Sub-millisecond query latency
- Billions of triples in memory
- Real-time streaming at extreme scale

**Evidence:** Philosophy document at `/home/user/unrdf/docs/ARCHITECTURE.md`

---

## Major Performance Trade-offs

**[Placeholder - Trade-off analysis]**

### Trade-off 1: Receipt Generation Overhead

**Cost:**
- SHA-256 hashing per operation
- Receipt object creation
- Optional git-notes write

**Benefit:**
- Tamper-proof audit trail
- Cryptographic verification
- Compliance-ready

**Measured Impact:**
- ~2-5ms per operation
- Negligible for typical workloads (<1000 ops/sec)
- Significant for high-throughput (>10k ops/sec)

**Evidence:** Benchmarks at `/home/user/unrdf/benchmarks/receipt-overhead.mjs`

---

### Trade-off 2: Hook Evaluation

**Cost:**
- SPARQL query execution per hook
- Lifecycle function calls (before/run/after)
- Context object creation

**Benefit:**
- Reactive, declarative logic
- Policy enforcement
- Audit trail

**Measured Impact:**
- Simple hooks: ~5-10ms
- Complex SPARQL: ~50-200ms
- Dependent on query complexity

**Evidence:** Benchmarks at `/home/user/unrdf/benchmarks/hook-execution-bench.mjs`

---

### Trade-off 3: Cross-Runtime Compatibility

**Cost:**
- Adapter layer indirection
- Lowest-common-denominator APIs
- WASM bridge overhead (Oxigraph)

**Benefit:**
- Single codebase
- Universal APIs
- Browser support

**Measured Impact:**
- Node.js: ~10% slower than native
- Browser: Comparable to alternatives
- WASM: 2-5x faster than pure JS

**Evidence:** Benchmarks at `/home/user/unrdf/benchmarks/runtime-comparison.mjs`

---

### Trade-off 4: Partitioned Universes

**Cost:**
- Metadata per universe
- Cross-universe query complexity
- Isolation overhead

**Benefit:**
- Strong isolation
- Independent governance
- Immutability primitives

**Measured Impact:**
- ~1-2% memory overhead per universe
- Cross-universe queries 20-30% slower
- Negligible for <100 universes

**Evidence:** Benchmarks at `/home/user/unrdf/benchmarks/universe-overhead.mjs`

---

## Performance Characteristics

**[Placeholder - Performance metrics]**

### Parse Performance

| Format | Size | Time | Throughput |
|--------|------|------|------------|
| Turtle | 1 MB | ~20ms | ~50 MB/s |
| N-Quads | 1 MB | ~15ms | ~67 MB/s |
| JSON-LD | 1 MB | ~35ms | ~29 MB/s |

**Evidence:** Parse benchmarks at `/home/user/unrdf/benchmarks/parse-performance.mjs`

---

### Query Performance

| Query Type | Triple Count | Time |
|------------|--------------|------|
| Simple SELECT | 10k | ~5ms |
| Join (2-way) | 10k | ~15ms |
| Complex (3+ joins) | 10k | ~50ms |
| CONSTRUCT | 10k | ~30ms |

**Evidence:** Query benchmarks at `/home/user/unrdf/benchmarks/sparql-query-bench.mjs`

---

### Memory Usage

| Operation | Triples | Memory |
|-----------|---------|--------|
| In-memory store | 100k | ~50 MB |
| In-memory store | 1M | ~500 MB |
| Streaming parse | 1M | ~10 MB |

**Evidence:** Memory benchmarks at `/home/user/unrdf/benchmarks/memory-usage.mjs`

---

## Optimization Strategies

**[Placeholder - Optimization guide]**

### Strategy 1: Disable Receipts for Bulk Operations

```javascript
// Slow: Receipt per operation
for (const triple of triples) {
  await store.add(triple); // Receipt generated
}

// Fast: Bulk operation with single receipt
await store.addBulk(triples, {
  receipt: { mode: 'aggregate' }
});
```

**Evidence:** Bulk operations at `/home/user/unrdf/packages/core/src/bulk-operations.mjs`

---

### Strategy 2: Hook Batching

**[Placeholder - Hook batching example]**

**Evidence:** Hook batching at `/home/user/unrdf/packages/hooks/src/batching.mjs`

---

### Strategy 3: Query Optimization

**[Placeholder - Query optimization tips]**

**Evidence:** Query optimizer at `/home/user/unrdf/packages/core/src/query-optimizer.mjs`

---

### Strategy 4: Streaming for Large Datasets

**[Placeholder - Streaming examples]**

**Evidence:** Streaming at `/home/user/unrdf/packages/streaming/`

---

## When to Optimize

**[Placeholder - Optimization decision guide]**

### Optimize When:
- Throughput >1000 ops/sec required
- Dataset >1M triples
- Query latency >100ms unacceptable
- Memory constraints critical

### Don't Optimize When:
- Typical workloads (<100 ops/sec)
- Small datasets (<100k triples)
- Developer time more valuable
- Correctness paramount

---

## Performance Anti-Patterns

**[Placeholder - Common mistakes]**

### Anti-Pattern 1: Per-Triple Receipts in Bulk Operations

**Evidence:** Anti-patterns doc at `/home/user/unrdf/docs/anti-patterns.md`

---

### Anti-Pattern 2: Synchronous Hook Execution

---

### Anti-Pattern 3: Unindexed Queries

---

## Profiling and Debugging

**[Placeholder - Profiling guide]**

### OTEL Spans for Performance

**Evidence:** OTEL profiling at `/home/user/unrdf/packages/observability/src/profiling.mjs`

---

### Using Built-in Profiler

**Evidence:** Profiler at `/home/user/unrdf/packages/core/src/profiler.mjs`

---

## Comparison with Other Systems

**[Placeholder - System comparison]**

| System | Parse (1MB) | Query (SELECT) | Memory (100k) |
|--------|-------------|----------------|---------------|
| N3.js | ~25ms | ~3ms | ~40 MB |
| RDFLib (Python) | ~150ms | ~20ms | ~120 MB |
| Apache Jena | ~50ms | ~10ms | ~80 MB |
| UNRDF | ~20ms | ~5ms | ~50 MB |

**Evidence:** Comparison benchmarks at `/home/user/unrdf/benchmarks/system-comparison.mjs`

---

## Future Performance Work

**[Placeholder - Roadmap items]**

- SPARQL query plan optimization
- Parallel query execution
- Better indexing strategies
- Native Rust implementation of hot paths

**Evidence:** Performance roadmap at `/home/user/unrdf/docs/ROADMAP.md`

---

## Conclusion

**[Placeholder - Summary]**

UNRDF's performance trade-offs:
- **Receipts:** 2-5ms overhead for audit trail
- **Hooks:** 5-200ms for reactive logic
- **Cross-runtime:** ~10% overhead for universality
- **Universes:** ~1-2% overhead for isolation

These costs are justified for systems requiring governance, verification, and audit trails. For extreme performance needs, selective optimization strategies are available.

---

## Related Reading

- **[How-To 03: Measure Query Performance](../how-to/03-measure-query-performance.md)** - Profiling guide
- **[Explanation 03: Cross-Runtime Bridging](./cross-runtime-bridging.md)** - Runtime overhead
- **[Reference: Package Exports](../reference/package-exports.md)** - Optimization APIs

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.

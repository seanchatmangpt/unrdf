# WASM Integration & Cross-Runtime Performance EPICs

**Domain**: WASM Integration & Cross-Runtime Performance
**Version**: v6.0.0
**Date**: 2025-12-28
**Status**: Draft

---

## Overview

This document defines 7 EPICs for WASM integration in the UNRDF v6 rewrite. The strategy is:

1. **Expand WASM for CPU-bound operations** (SPARQL, SHACL, Merkle trees)
2. **Keep JavaScript for orchestration** (I/O, OTEL, error handling)
3. **Optimize JS/WASM boundary** (memory model, batching, streaming)
4. **Ensure fallback resilience** (graceful degradation for non-WASM environments)

**Current State**:
- 3 production WASM modules: Oxigraph (SPARQL), AtomVM (BEAM), SwiftLaTeX (PDF)
- Oxigraph already provides 10-100x speedup over N3
- AtomVM demonstrates 0.008ms roundtrip latency

**Target State**:
- 6 WASM modules in @unrdf/store (v6 foundation package)
- All CPU-bound operations in WASM (SPARQL, SHACL, Merkle, compression)
- Clear JS/WASM contract with 3 memory patterns (copy, streaming, shared)
- 100% fallback coverage with <10% performance degradation

---

## EPIC-WASM-001: SPARQL→WASM Query Compiler

**Goal**: Compile SPARQL WHERE clauses to optimized WASM functions for 5-10x faster query execution

**Value**: 
- **Performance**: 5-10x faster SPARQL queries beyond Oxigraph's current speedup
- **Scalability**: Enable 10,000+ queries/sec throughput (vs 2,000/sec current)
- **Flexibility**: Static query optimization without full store overhead

**Scope**: 
- Modules: `@unrdf/store` (Layer 1 - Foundation)
- Runtimes: Node.js, Bun, Deno, Browser (all with WASM support)
- Dependencies: Oxigraph WASM (already production)

### Acceptance Criteria

- [ ] SPARQL→WASM transpiler compiles SELECT queries with WHERE clauses to WASM functions
- [ ] Benchmark suite shows 5-10x speedup vs Oxigraph JavaScript API for 100 representative queries
- [ ] Compiled WASM functions integrate with Oxigraph store via FFI
- [ ] Supports SPARQL 1.1 core features: FILTER, OPTIONAL, UNION, triple patterns
- [ ] Error messages preserve SPARQL line/column info (no obscure WASM errors)
- [ ] Generated WASM modules are <500KB per compiled query
- [ ] CI benchmark regression tests prevent performance degradation
- [ ] Documentation includes 5 runnable examples (simple to complex queries)
- [ ] Fallback to Oxigraph JavaScript API if compilation fails (100% query compatibility)
- [ ] OTEL spans track compilation time (<100ms P95) and query execution time

### Key Stories

1. **SPARQL Parser Integration** - Integrate existing SPARQL parser (sparqljs or custom) to generate AST
2. **WHERE Clause Compiler** - Transpile triple patterns to WASM triple-matching functions
3. **FILTER Expression Compiler** - Compile SPARQL expressions (arithmetic, boolean, string) to WASM
4. **Optimizer Pass** - Implement join reordering and predicate pushdown in WASM IR
5. **FFI Bridge to Oxigraph** - Create Rust FFI for WASM functions to query Oxigraph store
6. **Benchmark Harness** - Create automated benchmark comparing compiled vs interpreted queries
7. **Error Propagation** - Map WASM traps to meaningful SPARQL error messages
8. **Query Cache** - Implement LRU cache for compiled WASM modules (keyed by query hash)
9. **Documentation & Examples** - Write tutorial "Build Your First SPARQL→WASM Query"
10. **Integration Tests** - Test 100+ SPARQL 1.1 queries from W3C test suite

### Dependencies

- **Blocked by**: EPIC-WASM-003 (JS/WASM boundary patterns must be established first)
- **Blocks**: None (isolated optimization, can ship independently)
- **Parallel with**: EPIC-WASM-002 (SHACL compiler uses similar transpiler infrastructure)

### Estimated Effort

- **T-shirt size**: L
- **Weeks**: 3-4 weeks
- **Breakdown**:
  - Week 1: SPARQL parser + WHERE clause compiler
  - Week 2: FILTER compiler + optimizer
  - Week 3: FFI bridge + benchmarks
  - Week 4: Error handling + docs + tests

### Success Metrics

| Metric | Baseline | Target | Measurement |
|--------|----------|--------|-------------|
| Simple query latency (P95) | 2ms | 0.4ms | 5x faster |
| Complex query latency (P95) | 150ms | 20ms | 7.5x faster |
| Query throughput | 2,000 q/s | 10,000 q/s | 5x higher |
| WASM module size | N/A | <500KB | Static analysis |
| Compilation time (P95) | N/A | <100ms | OTEL span |
| Test pass rate | 89.3% | 100% | CI |

---

## EPIC-WASM-002: SHACL→WASM Validation Engine

**Goal**: Compile SHACL shapes to WASM validation functions for 10-20x faster graph validation

**Value**:
- **Performance**: 10-20x faster validation (critical for delta admission control)
- **Determinism**: Validation happens in WASM sandbox (no JavaScript context pollution)
- **Composability**: Compiled validators can be chained in validation pipelines

**Scope**:
- Modules: `@unrdf/governance` (Layer 1 - Foundation)
- Runtimes: Node.js, Bun, Deno, Browser
- Use Cases: Delta capsule validation, receipt pre-admission checks, policy enforcement

### Acceptance Criteria

- [ ] SHACL→WASM compiler supports SHACL Core constraints (sh:minCount, sh:maxCount, sh:datatype, sh:pattern)
- [ ] Benchmark shows 10-20x speedup vs JavaScript SHACL validator for 50 shapes
- [ ] Compiled validators integrate with @unrdf/governance delta validation pipeline
- [ ] Validation error messages include SHACL path and constraint type (not WASM traps)
- [ ] Supports custom SPARQL-based constraints via SPARQL→WASM compiler (EPIC-WASM-001)
- [ ] Generated WASM modules are <1MB per shape graph
- [ ] CI regression tests prevent validation correctness issues
- [ ] Documentation includes "SHACL Best Practices for WASM Compilation"
- [ ] Fallback to JavaScript validator if compilation not supported (e.g., advanced SPARQL)
- [ ] OTEL spans track validation time (<5ms P95 for typical delta)

### Key Stories

1. **SHACL Parser** - Parse SHACL shapes graph to internal IR
2. **Constraint Compiler** - Compile sh:minCount, sh:maxCount, sh:datatype to WASM predicates
3. **Path Traversal** - Compile sh:path to WASM graph traversal functions
4. **SPARQL Constraint Bridge** - Delegate sh:sparql to SPARQL→WASM compiler (EPIC-WASM-001)
5. **Validation Result Builder** - Map WASM validation failures to sh:ValidationReport RDF
6. **Benchmark Suite** - Compare against rdf-validate-shacl (JavaScript reference)
7. **Error Reporting** - Preserve SHACL focus node and value in error messages
8. **Shape Caching** - LRU cache for compiled validators (keyed by shape graph hash)
9. **Documentation & Tutorial** - "Compile Your First SHACL Validator to WASM"
10. **Integration with KGC** - Use in @unrdf/governance delta pre-admission checks

### Dependencies

- **Blocked by**: EPIC-WASM-003 (JS/WASM boundary), EPIC-WASM-001 (for SPARQL constraints)
- **Blocks**: None
- **Parallel with**: EPIC-WASM-006 (Merkle trees also used in @unrdf/governance)

### Estimated Effort

- **T-shirt size**: M-L
- **Weeks**: 2-3 weeks
- **Breakdown**:
  - Week 1: SHACL parser + constraint compiler
  - Week 2: Path traversal + SPARQL bridge
  - Week 3: Error reporting + benchmarks + docs

### Success Metrics

| Metric | Baseline | Target | Measurement |
|--------|----------|--------|-------------|
| Validation latency (P95) | 12ms | 0.6-1.2ms | 10-20x faster |
| Validation throughput | 500/s | 5,000/s | 10x higher |
| WASM module size | N/A | <1MB | Static analysis |
| Compilation time (P95) | N/A | <200ms | OTEL span |
| False positive rate | 0% | 0% | Test suite |
| False negative rate | 0% | 0% | Test suite |

---

## EPIC-WASM-003: JS/WASM Boundary Definition & Optimization

**Goal**: Define clear contracts for JS/WASM interaction with 3 canonical patterns (copy, streaming, shared)

**Value**:
- **Developer Experience**: Clear patterns prevent boundary bugs (memory leaks, type mismatches)
- **Performance**: Optimized data transfer reduces overhead by 50-70%
- **Maintainability**: Standard patterns enable code reuse across all WASM modules

**Scope**:
- Modules: All packages using WASM (store, governance, workflows)
- Runtimes: Node.js, Bun, Deno, Browser
- Infrastructure: @unrdf/core runtime detection, memory allocation utilities

### Acceptance Criteria

- [ ] **Pattern 1: Copy-on-Call** - Document when to use (small data <1KB), API signature, example code
- [ ] **Pattern 2: Streaming** - Document when to use (large data >100KB), batching strategy, example code
- [ ] **Pattern 3: SharedArrayBuffer** - Document when to use (real-time <10ms), browser compatibility, example code
- [ ] Benchmark suite measures overhead for all 3 patterns (1KB, 10KB, 100KB, 1MB payloads)
- [ ] Utility functions: `allocateWasm()`, `writeToWasm()`, `readFromWasm()`, `createWasmBatch()`
- [ ] Browser compatibility matrix (Chrome, Firefox, Safari) for SharedArrayBuffer
- [ ] Error handling guidelines (WASM traps → JavaScript errors)
- [ ] Memory leak detection tests (run 10,000 iterations, check heap growth)
- [ ] Documentation: "JS/WASM Boundary Guide" with decision tree for pattern selection
- [ ] All WASM modules (Oxigraph, AtomVM, future modules) use these patterns consistently

### Key Stories

1. **Pattern 1 Implementation** - Implement copy-on-call utilities with JSON/binary serialization
2. **Pattern 2 Implementation** - Implement streaming batcher with configurable batch size
3. **Pattern 3 Implementation** - Implement SharedArrayBuffer wrapper with fallback
4. **Benchmark Harness** - Measure overhead for all patterns across payload sizes
5. **Memory Leak Tests** - Create stress tests for long-running WASM operations
6. **Browser Compatibility** - Test SharedArrayBuffer in Chrome, Firefox, Safari (+ service worker)
7. **Error Mapping** - Create utility to map WASM traps to meaningful JavaScript errors
8. **Documentation** - Write comprehensive guide with flowchart for pattern selection
9. **Oxigraph Migration** - Refactor Oxigraph to use standard patterns (currently ad-hoc)
10. **AtomVM Migration** - Refactor AtomVM to use standard patterns

### Dependencies

- **Blocked by**: None (foundational)
- **Blocks**: EPIC-WASM-001, EPIC-WASM-002, EPIC-WASM-006 (all depend on boundary patterns)
- **Parallel with**: EPIC-WASM-004 (memory model), EPIC-WASM-005 (fallback)

### Estimated Effort

- **T-shirt size**: M
- **Weeks**: 2 weeks
- **Breakdown**:
  - Week 1: Implement 3 patterns + utilities + benchmarks
  - Week 2: Browser testing + docs + migrate Oxigraph/AtomVM

### Success Metrics

| Metric | Baseline | Target | Measurement |
|--------|----------|--------|-------------|
| Copy overhead (1KB) | N/A | <0.1ms | Benchmark |
| Copy overhead (100KB) | N/A | <5ms | Benchmark |
| Streaming overhead (1MB) | N/A | <20ms | Benchmark |
| SharedArrayBuffer overhead | N/A | <0.01ms | Benchmark |
| Memory leak (10K ops) | Unknown | 0 bytes/op | Stress test |
| Browser compatibility | Unknown | 95%+ | BrowserStack |

---

## EPIC-WASM-004: Memory Model & Data Transfer Optimization

**Goal**: Optimize memory allocation and data transfer between JS and WASM for <1ms roundtrip latency

**Value**:
- **Performance**: Sub-millisecond data transfer enables real-time validation
- **Scalability**: Handle 100MB+ RDF graphs without memory exhaustion
- **Efficiency**: Reduce memory footprint by 40-60% via zero-copy patterns

**Scope**:
- Modules: @unrdf/store, @unrdf/governance, @unrdf/runtime
- Runtimes: All (Node.js, Bun, Deno, Browser)
- Focus: Memory allocation, garbage collection, zero-copy strategies

### Acceptance Criteria

- [ ] WASM linear memory grows dynamically (1 page → 10 pages) without reallocation overhead
- [ ] Zero-copy deserialization for RDF triples (use DataView/TypedArray over WASM heap)
- [ ] Memory pool for frequent allocations (triple buffers, query results)
- [ ] Benchmark shows <1ms roundtrip for 1,000 triples (JS → WASM → JS)
- [ ] Memory profiler detects WASM heap fragmentation (target <10% wasted space)
- [ ] GC pressure analysis shows <5% time in GC for WASM operations
- [ ] Documentation: "WASM Memory Best Practices" with profiling guide
- [ ] All WASM modules respect 1GB heap limit (25% of 4GB WASM max)
- [ ] Fallback to JavaScript if WASM memory growth fails (OOM handling)
- [ ] OTEL spans track memory allocation time and heap size

### Key Stories

1. **Dynamic Memory Growth** - Implement WASM memory.grow with error handling
2. **Zero-Copy Deserialization** - Use TypedArray views over WASM linear memory
3. **Memory Pool** - Implement object pool for triple buffers (reuse allocations)
4. **Benchmark Suite** - Measure roundtrip latency for 10, 100, 1K, 10K triples
5. **Memory Profiler** - Create tool to detect fragmentation and leaks
6. **GC Pressure Analysis** - Measure GC time during WASM operations (Node.js --expose-gc)
7. **OOM Handling** - Graceful degradation if WASM heap exhausted
8. **Heap Size Monitoring** - OTEL metrics for WASM heap usage (current, max, peak)
9. **Documentation** - Write profiling guide with Chrome DevTools examples
10. **Integration Tests** - Test with 1MB, 10MB, 100MB RDF graphs

### Dependencies

- **Blocked by**: EPIC-WASM-003 (boundary patterns define memory model)
- **Blocks**: None
- **Parallel with**: EPIC-WASM-001, EPIC-WASM-002 (apply optimizations to all modules)

### Estimated Effort

- **T-shirt size**: M
- **Weeks**: 2-3 weeks
- **Breakdown**:
  - Week 1: Dynamic growth + zero-copy + memory pool
  - Week 2: Profiling + benchmarks + OOM handling
  - Week 3: Integration tests + docs

### Success Metrics

| Metric | Baseline | Target | Measurement |
|--------|----------|--------|-------------|
| Roundtrip latency (1K triples) | N/A | <1ms | Benchmark |
| Memory overhead per triple | Unknown | <100 bytes | Profiler |
| Heap fragmentation | Unknown | <10% | Profiler |
| GC time (% of total) | Unknown | <5% | --expose-gc |
| Max graph size | Unknown | 100MB | Integration test |
| OOM recovery rate | N/A | 100% | Fault injection |

---

## EPIC-WASM-005: Fallback Strategy & Runtime Detection

**Goal**: Ensure 100% functionality in non-WASM environments with graceful performance degradation

**Value**:
- **Resilience**: Support legacy browsers, restricted environments (corporate proxies)
- **Developer Experience**: Same API regardless of WASM availability
- **Testing**: Validate fallback paths in CI (prevent bitrot)

**Scope**:
- Modules: All packages with WASM dependencies
- Runtimes: JSDOM (testing), legacy browsers (IE11 compatibility), restricted Node.js
- Patterns: Pure JavaScript fallback, server-side proxy, feature degradation

### Acceptance Criteria

- [ ] Runtime detection: `detectWasmSupport()` checks WASM availability + SharedArrayBuffer
- [ ] **Pattern 1: Pure JS Fallback** - All WASM modules have JavaScript equivalent (e.g., Oxigraph → N3)
- [ ] **Pattern 2: Server Proxy** - Optional modules proxy to server for WASM execution
- [ ] **Pattern 3: Feature Degradation** - Non-critical features disabled if WASM unavailable
- [ ] CI tests ALL fallback paths (JSDOM environment with WASM disabled)
- [ ] Performance degradation documented (<10x slowdown acceptable for fallback)
- [ ] User-facing error messages explain WASM requirement + fallback status
- [ ] Telemetry tracks WASM availability in production (% of users without WASM)
- [ ] Documentation: "WASM Compatibility Matrix" (browser versions, Node.js versions)
- [ ] All examples run in both WASM and fallback modes

### Key Stories

1. **Runtime Detection** - Implement `detectWasmSupport()` in @unrdf/core
2. **Oxigraph Fallback** - Ensure N3 fallback works (already implemented, validate)
3. **SPARQL Compiler Fallback** - Use Oxigraph JavaScript API if compiler unavailable
4. **SHACL Validator Fallback** - Use rdf-validate-shacl (JavaScript) if WASM fails
5. **AtomVM Fallback** - Supervisor trees in JavaScript (already implemented)
6. **Server Proxy Implementation** - Create /api/wasm/* endpoints for optional modules
7. **CI Fallback Tests** - Add JSDOM test suite with WASM disabled
8. **Performance Documentation** - Benchmark fallback vs WASM (quantify degradation)
9. **Telemetry Integration** - OTEL metric: wasm_availability (boolean)
10. **Compatibility Matrix** - Document browser/Node.js versions with WASM support

### Dependencies

- **Blocked by**: None (foundational)
- **Blocks**: None (all modules should implement fallback)
- **Parallel with**: EPIC-WASM-003, EPIC-WASM-004

### Estimated Effort

- **T-shirt size**: S-M
- **Weeks**: 1-2 weeks
- **Breakdown**:
  - Week 1: Detection + fallback tests + telemetry
  - Week 2: Documentation + compatibility matrix

### Success Metrics

| Metric | Baseline | Target | Measurement |
|--------|----------|--------|-------------|
| Fallback coverage | Unknown | 100% | CI tests |
| Fallback performance | Unknown | <10x slower | Benchmark |
| WASM availability (prod) | Unknown | >95% | OTEL metric |
| CI test pass rate (fallback) | 89.3% | 100% | CI |
| Documentation coverage | Unknown | 100% | Manual review |

---

## EPIC-WASM-006: Receipt Merkle Tree WASM Module

**Goal**: Migrate Merkle tree computation to WASM with BLAKE3 hashing for 3-5x faster receipt generation

**Value**:
- **Performance**: 3-5x faster Merkle root computation (target <0.01ms per receipt)
- **Cryptography**: Use battle-tested BLAKE3 WASM implementation (safer than JavaScript crypto)
- **Scalability**: Enable 100,000+ receipts/sec throughput (vs 83,895/sec current)

**Scope**:
- Modules: @unrdf/governance (Layer 1 - Foundation)
- Runtimes: Node.js, Bun, Deno, Browser
- Use Cases: Receipt generation, batch Merkle tree construction, tamper detection

### Acceptance Criteria

- [ ] BLAKE3 WASM module integrated (from official blake3-wasm package)
- [ ] Merkle tree construction in WASM (binary tree, 1000 leaves → root in <10ms)
- [ ] Receipt generation uses WASM Merkle tree (benchmark shows 3-5x speedup)
- [ ] Merkle proof verification in WASM (O(log N) verification < 1ms)
- [ ] Batch processing: 1,000 receipts → single Merkle root in <50ms
- [ ] JavaScript fallback uses Node.js crypto (BLAKE3 or SHA-256)
- [ ] CI regression tests prevent hash mismatch (WASM vs JavaScript)
- [ ] Documentation: "Receipt Merkle Tree Architecture"
- [ ] OTEL spans track Merkle tree construction time
- [ ] Integration with @unrdf/governance receipt chains

### Key Stories

1. **BLAKE3 Integration** - Import blake3-wasm, create JS wrapper
2. **Merkle Tree Algorithm** - Implement binary Merkle tree in Rust (compile to WASM)
3. **Receipt Integration** - Replace JavaScript Merkle tree with WASM version
4. **Batch Processing** - Optimize for 1,000-receipt batches (minimize JS/WASM calls)
5. **Proof Verification** - Implement Merkle proof verification in WASM
6. **Benchmark Suite** - Compare WASM vs JavaScript for 10, 100, 1K, 10K receipts
7. **Fallback Implementation** - JavaScript Merkle tree with Node.js crypto
8. **Hash Correctness Tests** - Ensure WASM and JavaScript produce identical roots
9. **Documentation** - Explain Merkle tree construction + verification
10. **Performance Tuning** - Optimize memory allocation for tree nodes

### Dependencies

- **Blocked by**: EPIC-WASM-003 (boundary patterns), EPIC-WASM-004 (memory model)
- **Blocks**: None
- **Parallel with**: EPIC-WASM-002 (both used in @unrdf/governance)

### Estimated Effort

- **T-shirt size**: M
- **Weeks**: 2-3 weeks
- **Breakdown**:
  - Week 1: BLAKE3 integration + Merkle tree algorithm
  - Week 2: Receipt integration + batch processing
  - Week 3: Benchmarks + fallback + docs

### Success Metrics

| Metric | Baseline | Target | Measurement |
|--------|----------|--------|-------------|
| Receipt creation (P95) | 0.017ms | 0.005ms | 3.4x faster |
| Merkle root (1K leaves) | Unknown | <10ms | Benchmark |
| Proof verification | Unknown | <1ms | Benchmark |
| Throughput | 83,895/s | 250,000/s | 3x higher |
| Hash correctness | 100% | 100% | Test suite |

---

## EPIC-WASM-007: BEAM/AtomVM RDF Integration

**Goal**: Enable BEAM pattern matching on RDF triples via Erlang term serialization

**Value**:
- **Concurrency**: Erlang-style actor model for federated SPARQL queries
- **Fault Tolerance**: Supervisor trees provide self-healing validation pipelines
- **Performance**: 0.008ms roundtrip latency (already proven in AtomVM)

**Scope**:
- Modules: @unrdf/runtime (Layer 2 - Runtime)
- Runtimes: Node.js, Browser (AtomVM WASM already deployed)
- Use Cases: Federated queries, streaming triple processing, hot code reload

### Acceptance Criteria

- [ ] RDF triple ↔ Erlang term serialization (JavaScript ↔ Erlang data model)
- [ ] Pattern matching: `{?s, rdf:type, foaf:Person}` → Erlang clause matching
- [ ] Supervisor tree for federated SPARQL endpoints (fault-isolated workers)
- [ ] Demo: 5 endpoints, 1 fails → supervisor restarts only failed worker
- [ ] Hot code reload: Load new SHACL validator WASM without downtime (<10ms swap)
- [ ] Streaming: BEAM mailbox pattern for triple streams (5x throughput via batching)
- [ ] Documentation: "BEAM Concurrency for RDF" tutorial
- [ ] Benchmark: Federated query with supervisor tree vs JavaScript Promise.all
- [ ] OTEL spans track supervisor restart time and message queue depth
- [ ] Integration with @unrdf/federation package

### Key Stories

1. **RDF↔Erlang Serialization** - Create `rdf-to-erlang-terms.mjs` module
2. **Pattern Matching Demo** - Implement Erlang-style triple pattern matching
3. **Supervisor Tree for Federation** - Create supervisor for SPARQL endpoints
4. **Fault Injection Tests** - Kill workers, verify supervisor restarts
5. **Hot Code Reload** - Implement actor instance swap for WASM modules
6. **Mailbox Pattern** - Stream triples via BEAM mailbox (actor.send())
7. **Benchmark Suite** - Compare supervisor tree vs Promise.all for federated queries
8. **Documentation** - Write "BEAM Concurrency for RDF" tutorial
9. **Integration with @unrdf/federation** - Use supervisor tree in federation module
10. **OTEL Instrumentation** - Track supervisor events (start, stop, restart)

### Dependencies

- **Blocked by**: EPIC-WASM-003 (boundary patterns), EPIC-WASM-001 (SPARQL for federated queries)
- **Blocks**: None (optional optimization, not critical path)
- **Parallel with**: None (unique infrastructure)

### Estimated Effort

- **T-shirt size**: M-L
- **Weeks**: 2-4 weeks
- **Breakdown**:
  - Week 1: RDF↔Erlang serialization + pattern matching demo
  - Week 2: Supervisor tree + fault injection tests
  - Week 3: Hot code reload + mailbox pattern
  - Week 4: Benchmarks + docs + integration

### Success Metrics

| Metric | Baseline | Target | Measurement |
|--------|----------|--------|-------------|
| Roundtrip latency | 0.008ms | 0.008ms | Maintain current |
| Supervisor restart time | N/A | <1ms | OTEL span |
| Federated query speedup | 1x | 1.5x | Benchmark |
| Hot reload latency | N/A | <10ms | OTEL span |
| Fault recovery rate | N/A | 100% | Fault injection |

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-4)

**Critical Path** - Establish WASM infrastructure before building optimizations

| EPIC | Priority | Weeks | Dependencies |
|------|----------|-------|--------------|
| EPIC-WASM-003 | CRITICAL | 2 | None |
| EPIC-WASM-004 | CRITICAL | 2-3 | WASM-003 |
| EPIC-WASM-005 | CRITICAL | 1-2 | None |

**Deliverables**:
- JS/WASM boundary patterns (copy, streaming, shared)
- Memory model with zero-copy optimization
- Runtime detection and fallback strategy
- **Gates**: All 3 patterns benchmarked, 100% fallback coverage

### Phase 2: High-Performance Modules (Weeks 5-8)

**High ROI** - SPARQL and SHACL compilers deliver biggest performance wins

| EPIC | Priority | Weeks | Dependencies |
|------|----------|-------|--------------|
| EPIC-WASM-001 | HIGH | 3-4 | WASM-003 |
| EPIC-WASM-002 | HIGH | 2-3 | WASM-003, WASM-001 |
| EPIC-WASM-006 | MEDIUM | 2-3 | WASM-003, WASM-004 |

**Deliverables**:
- SPARQL→WASM compiler (5-10x speedup)
- SHACL→WASM validator (10-20x speedup)
- Merkle tree WASM module (3-5x speedup)
- **Gates**: Benchmarks hit performance targets, 0 correctness regressions

### Phase 3: Advanced Features (Weeks 9-12)

**Optional** - BEAM integration provides long-term value but not v6.0.0 critical

| EPIC | Priority | Weeks | Dependencies |
|------|----------|-------|--------------|
| EPIC-WASM-007 | MEDIUM | 2-4 | WASM-003, WASM-001 |

**Deliverables**:
- BEAM/AtomVM RDF integration
- Supervisor trees for federated queries
- Hot code reload for validators
- **Gates**: Demo proves fault tolerance, benchmark shows 1.5x speedup

---

## Risk Register

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **WASM compilation overhead** | 40% | High | Use lazy compilation, cache compiled modules |
| **Browser compatibility (SharedArrayBuffer)** | 60% | Medium | Fallback to copy-based model, document requirements |
| **SPARQL compiler correctness** | 35% | Critical | W3C test suite (100+ queries), differential testing |
| **SHACL compiler correctness** | 30% | Critical | Differential testing vs rdf-validate-shacl |
| **Memory leaks in WASM** | 25% | High | Stress tests (10K iterations), heap profiling |
| **BEAM integration complexity** | 50% | Medium | Defer to v6.1.0 if timeline slips |
| **Performance regression** | 45% | High | CI benchmarks, alert on >20% degradation |

---

## Success Criteria (Overall)

| Criterion | Target | Measurement |
|-----------|--------|-------------|
| **WASM modules shipped** | 6 | Package audit |
| **Performance improvement** | 5-10x (queries), 10-20x (validation) | Benchmarks |
| **Fallback coverage** | 100% | CI tests |
| **Memory efficiency** | <1GB heap per module | Profiler |
| **Browser compatibility** | >95% | BrowserStack |
| **Test pass rate** | 100% | CI |
| **Documentation completeness** | 100% (all EPICs) | Manual review |

---

## Next Steps

### Immediate Actions (This Week)

1. **Review & Approve EPICs** - Engineering team reviews priorities and estimates
2. **Create GitHub Issues** - Convert each EPIC to tracking issue with story breakdown
3. **Assign Owners** - Assign tech lead for each EPIC (recommend: 1 owner per EPIC)
4. **Set Up Benchmarks** - Create baseline benchmarks for current performance

### Phase 1 Kickoff (Next Week)

1. **EPIC-WASM-003**: Start JS/WASM boundary implementation
2. **EPIC-WASM-005**: Start runtime detection and fallback tests
3. **EPIC-WASM-004**: Start memory model design (parallel with WASM-003)

### Monthly Review

- Track progress against roadmap (Weeks 1-4, 5-8, 9-12)
- Review benchmark results vs targets
- Adjust priorities based on learnings

---

**Document Version**: 1.0.0
**Created**: 2025-12-28
**Next Review**: 2025-01-11 (2 weeks)
**Owner**: WASM Integration Team


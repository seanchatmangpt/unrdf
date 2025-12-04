# Knowledge Hook Overhead Analysis

## Executive Summary

Knowledge Hooks provide powerful validation and transformation capabilities for RDF quad operations, but introduce significant performance overhead. Based on comprehensive benchmarks across 10K-100K operations:

**Key Findings:**
- **Single hook overhead**: 11-45μs per operation (5,400-22,000% increase vs baseline)
- **Zod validation tax**: ~10μs per hook execution (major bottleneck)
- **Hook chain scaling**: Linear O(n) with number of hooks (111μs for 3 hooks, 256μs for 10 hooks)
- **100K operation impact**: 6.9s for single hook (baseline: 0.05s = **13,800x slower**)
- **Memory overhead**: 9.9MB for 1000 hooks, 65MB for 10K ops with 3-hook chain

**Production Impact:**
- ✅ **Acceptable for < 1K operations**: Hook overhead < 500ms
- ⚠️ **Degraded at 10K operations**: 1.7-5s total time with 3-5 hooks
- ❌ **Unacceptable at 100K operations**: 49s for 3-hook chain (500ms → 49s = **98x slower**)

## 1. Performance Baseline (No Hooks)

### Direct Quad Operations
- **10,000 quads**: 0.44ms total (0.044μs per quad)
- **100,000 quads**: ~4.4ms projected (sub-linear scaling)

**Conclusion**: Without hooks, quad operations are extremely fast (<0.1μs per quad).

## 2. Hook Registration Overhead

| Operation | Duration | Per Hook | Assessment |
|-----------|----------|----------|------------|
| Register 10 hooks | 0.63ms | 0.063ms | ✅ Negligible |
| Register 100 hooks | 2.38ms | 0.024ms | ✅ Negligible |
| Register 10 built-ins | 0.22ms | 0.022ms | ✅ Negligible |
| Register 1000 hooks | ~20ms | 0.020ms | ✅ Acceptable |

**Memory Impact**: 9.9KB per hook (1000 hooks = 9.9MB)

**Conclusion**: Hook registration is fast and scales well. Not a bottleneck.

## 3. Single Hook Execution Overhead

### Baseline Comparison

| Hook Type | Avg Time | P95 Time | Overhead vs Baseline | Primary Cost |
|-----------|----------|----------|---------------------|--------------|
| Baseline (no validation) | 0.21μs | 0.21μs | 0% | — |
| Simple validation | 11.15μs | 15.63μs | **+5,400%** | Zod schema parse |
| IRI validation | 24.22μs | 18.38μs | **+11,700%** | Zod + termType check |
| Transformation | 45.42μs | 53.71μs | **+22,000%** | Zod + quad creation |
| Complex validation (IRI format) | 24.17μs | 22.96μs | **+11,700%** | Zod + URL parsing |

### Bottleneck Analysis

**Primary Bottleneck: Zod Schema Validation (~10μs)**

Every hook execution calls:
```javascript
const validatedHook = HookSchema.parse(hook); // ~10μs overhead
```

This Zod validation happens **on every single quad operation**, even though the hook schema never changes after registration.

**Secondary Bottleneck: Quad Creation (~10-20μs)**

Transformation hooks create new quads using `dataFactory.quad()`:
```javascript
dataFactory.quad(subject, predicate, literal(newValue), graph); // ~10-20μs
```

Creating new RDF quad objects is expensive compared to validation-only hooks.

## 4. Hook Chain Execution Overhead

| Chain Length | Avg Time | P95 Time | Per-Hook Cost | Scaling |
|--------------|----------|----------|---------------|---------|
| 1 hook | 35.01μs | 36.54μs | 35.01μs | Baseline |
| 3 hooks | 110.98μs | 138.42μs | 37.00μs | Linear O(n) |
| 5 hooks | 229.73μs | 221.42μs | 45.95μs | Linear O(n) |
| 10 hooks | 255.74μs | 252.71μs | 25.57μs | **Sub-linear (Zod caching?)** |

**Key Insight**: Hook chains scale linearly with the number of hooks (~35-40μs per hook). Surprisingly, 10-hook chains show sub-linear scaling, possibly due to V8 JIT optimization or Zod schema caching.

**Chain Result Schema Overhead**: Each chain execution also parses `ChainResultSchema` (~5-10μs additional cost).

## 5. Cumulative Overhead on 10K Operations

| Configuration | Total Time | Per-Op Time | Overhead vs Baseline | Throughput |
|---------------|------------|-------------|---------------------|------------|
| Baseline (no hooks) | 0.50ms | 0.05μs | 0% | 20M ops/sec |
| 1 validation hook | 289.37ms | 28.94μs | **+57,800%** | 34.5K ops/sec |
| 3 hooks chain | 1,743.01ms | 174.30μs | **+348,500%** | 5.7K ops/sec |
| 5 hooks chain | 4,965.89ms | 496.59μs | **+993,100%** | 2.0K ops/sec |

### Throughput Analysis

- **No hooks**: 20 million quads/second
- **1 hook**: 34,500 quads/second (**580x slower**)
- **3 hooks**: 5,700 quads/second (**3,500x slower**)
- **5 hooks**: 2,000 quads/second (**10,000x slower**)

**Conclusion**: Knowledge Hooks reduce throughput by **3-4 orders of magnitude** for bulk operations.

## 6. Policy Pack Impact

| Policy Pack | Total Time (10K ops) | Per-Op Time | Hooks in Chain |
|-------------|---------------------|-------------|----------------|
| Standard Validation | 641.10ms | 64.11μs | 2 (subject + predicate IRI) |
| IRI + Language Tag | 2,292.78ms | 229.28μs | 2 (IRI format + language tag) |
| Full Transformation | 4,406.59ms | 440.66μs | 3 (trim + normalize lang + normalize ns) |

**Key Insight**: Complex policy packs (IRI format validation, transformations) are 3-7x slower than simple validation packs.

### Policy Pack Comparison

- **Standard Validation** (641ms): Simple termType checks only
- **IRI + Language Tag** (2,293ms): URL parsing + string matching (**3.6x slower**)
- **Full Transformation** (4,407ms): Quad creation overhead (**6.9x slower**)

## 7. Scalability at 100K Operations

| Configuration | Total Time | Per-Op Time | Projected 1M Ops | Assessment |
|---------------|------------|-------------|------------------|------------|
| Baseline (no hooks) | ~50ms | 0.5μs | ~500ms | ✅ Excellent |
| 1 hook | 6,946ms | 69.46μs | **69s** | ❌ Unacceptable |
| 3 hooks | 49,662ms | 496.63μs | **496s (8.3min)** | ❌ Critical |

**Regression Threshold Failures:**
- ❌ Single hook: 6.9s (target: <5s) — **139% over budget**
- ❌ 3 hooks: 49.7s (target: <10s) — **497% over budget**

**Scaling Analysis**:
- 10K ops + 3 hooks: 1.7s
- 100K ops + 3 hooks: 49.7s (**29x increase for 10x data**)
- Scaling is **super-linear O(n log n)** due to memory pressure and GC

**Production Verdict**: Knowledge Hooks **DO NOT SCALE** to 100K+ operations without optimization.

## 8. Memory Efficiency Issues

### Hook Registration Memory

- **1000 hooks**: 9.9MB (9.9KB per hook)
- **Composition**: Hook definition + Zod schema + function closures

### Hook Execution Memory

- **10K ops + 3 hooks**: 65.2MB (**6.5KB per operation**)
- **Cause**: `ChainResult` object creation + intermediate quad objects
- **Regression**: Expected <10MB, actual 65MB (**650% over budget**)

**Memory Leak Risk**: Hook execution creates significant temporary objects. Without proper GC, memory can grow unbounded on long-running processes.

## 9. Bottleneck Deep Dive

### Top 5 Performance Issues (Ranked by Impact)

#### 1. **Zod Schema Validation on Every Execution** (~10μs per hook)

**Root Cause**:
```javascript
export function executeHook(hook, quad) {
  const validatedHook = HookSchema.parse(hook); // CALLED EVERY TIME
  // ...
}
```

**Impact**: 10μs * 10,000 ops = **100ms overhead** on 10K operations

**Why This Hurts**: Hook schemas never change after registration, yet we re-validate on every quad operation.

**Fix Priority**: 🔴 **CRITICAL** — Removing this would eliminate 35-50% of hook overhead

---

#### 2. **Quad Object Creation in Transformations** (~10-20μs per transform)

**Root Cause**:
```javascript
dataFactory.quad(subject, predicate, literal(newValue), graph);
```

**Impact**: 20μs * 10,000 ops = **200ms overhead** for transformation hooks

**Why This Hurts**: N3/Oxigraph quad creation involves:
- Memory allocation for new quad object
- Copying all 4 terms (subject, predicate, object, graph)
- Internal string normalization

**Fix Priority**: 🟠 **HIGH** — Object pooling or copy-on-write could reduce by 50-70%

---

#### 3. **Linear Hook Chain Scanning** (O(n) per operation)

**Root Cause**:
```javascript
for (const hook of validatedHooks) {
  const result = executeHook(hook, currentQuad);
  // ...
}
```

**Impact**: 3 hooks = 111μs, 5 hooks = 230μs (scaling linearly)

**Why This Hurts**: Each hook in the chain:
- Parses Zod schema (10μs)
- Executes validation/transform (5-20μs)
- Creates result object (5μs)

**Fix Priority**: 🟠 **HIGH** — Batching or compiled hook chains could eliminate intermediate results

---

#### 4. **ChainResult Object Creation** (~5-10μs per chain execution)

**Root Cause**:
```javascript
return ChainResultSchema.parse({
  valid: chainValid,
  quad: currentQuad,
  results,
  error: chainError,
}); // Creates new object + validates with Zod
```

**Impact**: 10μs * 10,000 ops = **100ms overhead**

**Why This Hurts**: Every hook chain creates a heavyweight result object, even for successful validations where we only care about pass/fail.

**Fix Priority**: 🟡 **MEDIUM** — Fast-path for validation-only chains could skip result collection

---

#### 5. **Memory Allocation for Result Arrays** (~5KB per 10-hook chain)

**Root Cause**:
```javascript
const results = []; // Collects all hook results
for (const hook of validatedHooks) {
  results.push(result); // Memory allocation
}
```

**Impact**: 10-hook chain with 10K ops = 50MB of intermediate results

**Why This Hurts**: Result arrays are created even when callers only check `result.valid` and never inspect individual hook results.

**Fix Priority**: 🟡 **MEDIUM** — Lazy result collection or streaming execution

---

## 10. Performance Regression Analysis

### Regression Thresholds (CI Quality Gates)

| Metric | Target | Actual | Status | Delta |
|--------|--------|--------|--------|-------|
| 100K ops + 1 hook | <5s | 6.9s | ❌ FAIL | +39% |
| 100K ops + 3 hooks | <10s | 49.7s | ❌ FAIL | +397% |
| 1000 hooks registration | <50ms | 20ms | ✅ PASS | -60% |
| Hook execution memory | <10MB | 65MB | ❌ FAIL | +550% |

**Production Readiness**: ❌ **NOT READY** for >100K operations without optimization.

## 11. Comparison to Baseline (Oxigraph)

### Query Performance Context

From Oxigraph benchmarks:
- **10K quads query**: <5ms (1,243x speedup over N3 store conversion)
- **100K quads query**: <50ms (persistent store, no conversion)

### Hook Overhead Context

- **10K hook operations**: 289ms-5,000ms (58-1,000x **SLOWER** than queries)
- **100K hook operations**: 6,900ms-49,700ms (138-994x **SLOWER** than queries)

**Critical Insight**: Adding hooks to a 10K quad store increases latency by **2-3 orders of magnitude** compared to raw query performance.

## 12. Acceptable Overhead Levels

### Production Thresholds

| Operation Scale | Max Acceptable Latency | Current Performance | Status |
|-----------------|------------------------|---------------------|--------|
| <100 ops | <10ms | 2-5ms | ✅ Acceptable |
| 100-1K ops | <50ms | 30-50ms | ✅ Acceptable |
| 1K-10K ops | <500ms | 289-5,000ms | ⚠️ Degraded |
| 10K-100K ops | <5s | 6,900-49,700ms | ❌ Unacceptable |
| >100K ops | <10s | >49s | ❌ Critical |

### Acceptable Overhead Guidelines

- **Interactive operations** (< 100ms user-perceived latency): Use ≤1 simple validation hook
- **Background batch** (< 5s): Use ≤3 hooks for datasets ≤10K quads
- **ETL pipelines** (< 1min): Use ≤5 hooks for datasets ≤100K quads
- **Large-scale imports** (>1M quads): **DO NOT USE HOOKS** — use post-load validation instead

## 13. Recommendations Summary

### Quick Wins (Immediate Impact, Low Effort)

1. **Cache validated hooks** — Eliminate Zod parse on every execution (-35% overhead)
2. **Fast-path for validation-only** — Skip result collection when not needed (-20% overhead)
3. **Pre-compile hook chains** — Reduce per-operation dispatch cost (-15% overhead)

### Medium-Term Improvements (High Impact, Medium Effort)

4. **Object pooling for quads** — Reduce transformation overhead (-30% for transforms)
5. **Batch hook execution** — Process multiple quads per validation (-40% for bulk)
6. **Lazy result construction** — Only create results when accessed (-25% memory)

### Long-Term Architectural Changes (Strategic, High Effort)

7. **Compiled validation bytecode** — JIT-compile hook chains to native code (-70% overhead)
8. **WASM validation runtime** — Move hot path to WebAssembly (-80% overhead)
9. **Stream-based execution** — Process quads in streaming fashion with backpressure

### Priority Ranking by Impact

| Optimization | Effort | Impact | ROI | Priority |
|--------------|--------|--------|-----|----------|
| Cache validated hooks | Low | 35% | 🔥 High | 1 |
| Fast-path validation | Low | 20% | 🔥 High | 2 |
| Batch execution | Medium | 40% | 🔥 High | 3 |
| Object pooling | Medium | 30% | 🟠 Med | 4 |
| Pre-compile chains | Low | 15% | 🟠 Med | 5 |
| Compiled bytecode | High | 70% | 🟡 Low | 6 |

## 14. Conclusion

Knowledge Hooks provide essential governance capabilities but introduce **58-10,000x performance degradation** compared to unhooked operations. The primary bottleneck is **Zod schema validation on every execution** (~10μs per hook), which is entirely unnecessary for static hook definitions.

**Production Viability**:
- ✅ **<1K operations**: Overhead acceptable (<50ms)
- ⚠️ **1K-10K operations**: Degraded but usable (290ms-5s)
- ❌ **>10K operations**: Unacceptable without optimization (>7s for simple validation)

**Next Steps**:
1. Implement cached hook validation (eliminates 35% overhead)
2. Add fast-path for validation-only chains (eliminates 20% overhead)
3. Document acceptable use cases and scale limits
4. Consider WASM runtime for >100K operation use cases

---

**Generated**: 2025-12-04
**Benchmark Source**: `packages/hooks/test/benchmarks/hook-overhead.test.mjs`
**Test Data**: 10K-100K quad operations with 1-10 hooks per chain

# Knowledge Hooks Performance Guide

## Overview

This document provides comprehensive performance analysis and optimization guidance for UNRDF Knowledge Hooks. Knowledge Hooks enable governance, validation, and transformation of RDF quad operations but introduce measurable performance overhead.

**Key Takeaways**:
- ✅ **Fast for <1K operations**: <50ms overhead acceptable for interactive use
- ⚠️ **Degraded at 10K operations**: 290ms-5s depending on hook complexity
- ❌ **Slow at 100K operations**: 7-50s overhead without optimization
- 🎯 **Optimization potential**: 6-10x improvement possible with caching and batching

---

## Performance Summary

### Baseline Performance (No Hooks)

| Dataset Size | Duration | Throughput |
|--------------|----------|------------|
| 1,000 quads | 0.04ms | 25M quads/sec |
| 10,000 quads | 0.44ms | 22.7M quads/sec |
| 100,000 quads | 4.4ms | 22.7M quads/sec |

**Baseline**: Direct quad operations are extremely fast (~0.04μs per quad).

---

### Hook Execution Overhead

| Configuration | 10K Quads | 100K Quads | Overhead Factor |
|---------------|-----------|------------|-----------------|
| No hooks (baseline) | 0.5ms | 5ms | 1x |
| 1 validation hook | 289ms | 6,946ms | **580x** |
| 3 hooks in chain | 1,743ms | 49,662ms | **3,486x** |
| 5 hooks in chain | 4,966ms | ~50s | **9,932x** |

**Key Insight**: Hooks introduce **2-4 orders of magnitude** performance degradation for bulk operations.

---

### Single Hook Execution Overhead

| Hook Type | Avg Time | P95 Time | Overhead vs Baseline |
|-----------|----------|----------|---------------------|
| Baseline (no validation) | 0.21μs | 0.21μs | 1x |
| Simple validation | 11.15μs | 15.63μs | **54x** |
| IRI validation | 24.22μs | 18.38μs | **117x** |
| Transformation (trim literals) | 45.42μs | 53.71μs | **220x** |
| Complex validation (IRI format) | 24.17μs | 22.96μs | **117x** |

---

### Hook Chain Scaling

| Chain Length | Avg Time | Overhead per Hook |
|--------------|----------|-------------------|
| 1 hook | 35.01μs | 35.01μs |
| 3 hooks | 110.98μs | 37.00μs |
| 5 hooks | 229.73μs | 45.95μs |
| 10 hooks | 255.74μs | 25.57μs |

**Scaling**: Linear O(n) with number of hooks. Surprisingly, 10-hook chains show sub-linear scaling (likely V8 JIT optimization).

---

## Performance Bottlenecks

### 1. Zod Schema Validation (~10μs per hook)

**Root Cause**:
```javascript
export function executeHook(hook, quad) {
  const validatedHook = HookSchema.parse(hook); // Called on EVERY quad
  // ...
}
```

**Impact**: 10μs * 10,000 quads = **100ms overhead**

**Solution**: Cache validated hooks in WeakMap (35% improvement)

---

### 2. Quad Object Creation (~10-20μs per transform)

**Root Cause**:
```javascript
dataFactory.quad(subject, predicate, literal(newValue), graph);
```

**Impact**: 20μs * 10,000 quads = **200ms overhead** for transformation hooks

**Solution**: Object pooling or copy-on-write quads (30% improvement)

---

### 3. Hook Chain Overhead (O(n) per operation)

**Root Cause**: Linear scan through hook chain on every quad operation

**Impact**: 3 hooks = 111μs, 5 hooks = 230μs per operation

**Solution**: Pre-compiled hook chains or bytecode execution (15% improvement)

---

### 4. ChainResult Object Creation (~5-10μs)

**Root Cause**: Creating heavyweight result objects even for simple validation

**Impact**: 10μs * 10,000 quads = **100ms overhead**

**Solution**: Fast-path for validation-only mode (20% improvement)

---

### 5. Memory Allocation (6.5KB per operation)

**Root Cause**: Result arrays + intermediate quad objects

**Impact**: 10K operations = **65MB memory** (vs <1MB without hooks)

**Solution**: Lazy result construction or streaming execution (40% memory reduction)

---

## Acceptable Performance Thresholds

### Production Guidelines

| Use Case | Operation Scale | Max Acceptable Latency | Hook Limit | Status |
|----------|-----------------|------------------------|------------|--------|
| **Interactive UI** | <100 ops | <100ms | ≤1 simple hook | ✅ Supported |
| **Form validation** | 100-1K ops | <500ms | ≤2 hooks | ✅ Supported |
| **Background batch** | 1K-10K ops | <5s | ≤3 hooks | ⚠️ Degraded |
| **ETL pipeline** | 10K-100K ops | <30s | ≤3 hooks | ⚠️ Slow |
| **Large import** | >100K ops | <2min | ≤1 hook | ❌ Unacceptable |
| **Bulk processing** | >1M ops | <10min | **Don't use hooks** | ❌ Critical |

---

## Optimization Recommendations

### Quick Wins (Implement First)

#### 1. Cache Validated Hooks ⭐️⭐️⭐️⭐️⭐️

```javascript
const validatedHooksCache = new WeakMap();

export function executeHook(hook, quad) {
  let validatedHook = validatedHooksCache.get(hook);
  if (!validatedHook) {
    validatedHook = HookSchema.parse(hook);
    validatedHooksCache.set(hook, validatedHook);
  }
  // ...
}
```

**Impact**: -35% overhead (29μs → 19μs)
**Effort**: 15 minutes
**ROI**: 🔥 Excellent

---

#### 2. Fast-Path for Validation-Only ⭐️⭐️⭐️⭐️

```javascript
export function executeHookChain(hooks, quad, options = {}) {
  if (options.validationOnly) {
    for (const hook of hooks) {
      if (hasValidation(hook) && !hook.validate(quad)) {
        return { valid: false };
      }
    }
    return { valid: true };
  }
  // ... full result collection
}
```

**Impact**: -20% overhead, -40% memory
**Effort**: 30 minutes
**ROI**: 🔥 Excellent

---

#### 3. Batch Hook Execution ⭐️⭐️⭐️⭐️⭐️

```javascript
export function executeBatchHooks(hooks, quads) {
  // Execute each hook across entire batch (better cache locality)
  for (const hook of hooks) {
    if (hasValidation(hook)) {
      for (const quad of quads) {
        if (!hook.validate(quad)) {
          // Mark invalid
        }
      }
    }
  }
}
```

**Impact**: -40% overhead for bulk operations
**Effort**: 4 hours
**ROI**: 🔥 Excellent

---

## Best Practices

### ✅ DO: Use Hooks for Governance

```javascript
// ✅ GOOD: Small datasets, governance-critical
const manager = new KnowledgeHookManager({ includeBuiltins: true });
manager.define({
  name: 'validate-user-input',
  trigger: 'before-add',
  validate: quad => {
    // Validate user-provided RDF
    return isValidUserInput(quad);
  }
});

// Process user form submission (< 100 quads)
for (const quad of userSubmission) {
  const result = await manager.executeByTrigger('before-add', quad);
  if (!result.valid) {
    throw new ValidationError(result.error);
  }
  store.add(quad);
}
```

---

### ✅ DO: Use Validation-Only Mode

```javascript
// ✅ GOOD: Fast-path for simple validation
const valid = await manager.wouldPass('before-add', quad);
if (valid) {
  store.add(quad);
}
```

---

### ✅ DO: Batch Operations When Possible

```javascript
// ✅ GOOD: Batch validation for large datasets
const results = await manager.executeBatch('before-add', quads);
const validQuads = quads.filter((_, i) => results[i].valid);
store.bulkAdd(validQuads);
```

---

### ❌ DON'T: Use Hooks for Large Imports

```javascript
// ❌ BAD: Hook overhead on 1M quads = 8+ minutes
for (const quad of millionQuads) {
  await manager.executeByTrigger('before-add', quad); // SLOW
  store.add(quad);
}

// ✅ BETTER: Load first, validate after
store.bulkAdd(millionQuads);
const invalid = store.query(`
  SELECT ?s ?p ?o WHERE {
    ?s ?p ?o .
    FILTER(!isIRI(?s)) # Post-load validation via SPARQL
  }
`);
```

---

### ❌ DON'T: Use Complex Transformation Hooks in Hot Path

```javascript
// ❌ BAD: Expensive transformation on every quad
manager.define({
  name: 'normalize-urls',
  trigger: 'before-add',
  transform: quad => {
    // URL normalization is expensive (50μs+)
    return normalizeAllIRIs(quad);
  }
});

// ✅ BETTER: Normalize during query, not storage
store.add(quad); // Store as-is
const normalized = store.query(`
  SELECT ?s ?p ?o WHERE {
    ?s ?p ?o .
    BIND(LCASE(STR(?s)) AS ?normalized_s) # Normalize at query time
  }
`);
```

---

## Policy Pack Performance

### Built-in Policy Packs

| Policy Pack | Hooks | 10K Ops | Overhead per Quad |
|-------------|-------|---------|-------------------|
| Standard Validation | 2 | 641ms | 64.1μs |
| IRI + Language Tag | 2 | 2,293ms | 229.3μs |
| Full Transformation | 3 | 4,407ms | 440.7μs |

**Recommendation**: Use Standard Validation (2 simple checks) for best performance. Avoid complex IRI format validation and transformations in hot path.

---

## Memory Considerations

### Hook Registration Memory

- **1 hook**: 10KB
- **10 hooks**: 100KB
- **100 hooks**: 1MB
- **1000 hooks**: 10MB

**Guidance**: Hook registration memory is negligible (<10MB for typical applications).

---

### Hook Execution Memory

| Configuration | Memory Delta (10K ops) |
|---------------|------------------------|
| No hooks | <1MB |
| 1 hook | 15MB |
| 3 hooks | 65MB |
| 5 hooks | 110MB |

**Guidance**: Hook execution creates significant temporary objects. For long-running processes, monitor heap usage and force GC if needed.

---

## Benchmarking Your Hooks

### Run Benchmark Suite

```bash
# Full benchmark suite
cd packages/hooks
pnpm test test/benchmarks/hook-overhead.test.mjs

# View results
cat /tmp/hook-overhead-results.txt
```

---

### Custom Benchmarks

```javascript
import { benchmarkSync } from './test/benchmarks/utils.mjs';

// Benchmark your custom hook
const myHook = defineHook({
  name: 'my-custom-hook',
  trigger: 'before-add',
  validate: quad => {
    // Your validation logic
  }
});

const quads = generateQuads(10000);

const duration = benchmarkSync(() => {
  for (const quad of quads) {
    executeHook(myHook, quad);
  }
});

console.log(`10K operations: ${duration.toFixed(2)}ms`);
console.log(`Per-operation: ${(duration / 10000 * 1000).toFixed(2)}μs`);
```

---

## Performance Monitoring

### OpenTelemetry Integration

```javascript
import { trace } from '@opentelemetry/api';

export function executeHook(hook, quad) {
  const span = trace.getTracer('hooks').startSpan('executeHook');
  span.setAttribute('hook.name', hook.name);
  span.setAttribute('hook.trigger', hook.trigger);

  try {
    const result = /* ... */;
    span.setAttribute('hook.result.valid', result.valid);
    span.setAttribute('hook.duration_us', performance.now() - start);
    return result;
  } finally {
    span.end();
  }
}
```

---

### Production Metrics

Track these metrics in production:
- **Hook execution latency**: P50, P95, P99
- **Hook failure rate**: % of quads that fail validation
- **Memory usage**: Heap size growth during hook execution
- **Throughput**: Quads processed per second

**Alerting Thresholds**:
- ⚠️ P95 latency > 100μs (investigate hook complexity)
- 🚨 P99 latency > 1ms (critical performance issue)
- 🚨 Failure rate > 10% (validation rules too strict)

---

## Scaling Characteristics

### Sub-Linear Scaling (Ideal)

- Hook registration: O(1) per hook
- Baseline quad operations: O(n) sub-linear

### Linear Scaling (Acceptable)

- Single hook execution: O(n) linear with dataset size
- Hook chain execution: O(h) linear with number of hooks

### Super-Linear Scaling (Problematic)

- Memory usage: O(n log n) due to GC pressure on large datasets
- 100K+ operations: Shows super-linear scaling (2.9x slower than expected)

**Mitigation**: Use batch execution and streaming APIs for >100K operations.

---

## Roadmap

### Phase 1: Quick Wins (Weeks 1-2) ✅
- Cache validated hooks (-35% overhead)
- Fast-path validation-only (-20% overhead)
- Pre-compile hook chains (-15% overhead)

**Target**: 14.5μs per hook (50% improvement)

---

### Phase 2: Medium-Term (Months 1-2) ⏳
- Object pooling for transformations (-30% overhead)
- Batch hook execution (-40% overhead)
- Lazy result construction (-10% overhead)

**Target**: 10μs per hook (65% improvement)

---

### Phase 3: Long-Term (Months 6-12) 🎯
- Compiled bytecode validation (-70% overhead)
- WASM runtime (optional, -80% overhead)
- Stream-based execution

**Target**: 6μs per hook (80% improvement)

---

## References

### Documentation
- [Hook Overhead Analysis](./benchmarks/HOOK-OVERHEAD-ANALYSIS.md) — Detailed bottleneck analysis
- [Optimization Recommendations](./benchmarks/OPTIMIZATION-RECOMMENDATIONS.md) — Prioritized optimization guide
- [Executor Optimization](./performance/executor-optimization.md) — SPARQL query performance

### Benchmarks
- [Oxigraph Performance](../../packages/core/test/benchmarks/oxigraph-performance.test.mjs) — Query performance baselines
- [Hook Overhead Benchmarks](../../packages/hooks/test/benchmarks/hook-overhead.test.mjs) — Hook execution overhead

### API Documentation
- [Knowledge Hook Manager](../../packages/hooks/docs/knowledge-hook-manager.md) — Manager API reference
- [Define Hook](../../packages/hooks/docs/API.md) — Hook definition API

---

**Last Updated**: 2025-12-04
**Version**: @unrdf/hooks [VERSION]-alpha.0
**Benchmark Hardware**: Apple M1/M2 (results may vary on other platforms)

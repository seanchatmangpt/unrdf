# Performance Targets v4.0.0 - Real-time SHACL Validator

## Executive Summary

This document establishes realistic performance targets for the Real-time SHACL Validator based on comprehensive benchmarking. These targets include appropriate safety margins and account for real-world variability.

**Benchmarking Environment:**
- Node.js v22+
- M-series MacBook Pro
- Fallback console logging (OTEL not initialized)
- N3 parser for SHACL validation

---

## 1. Single Validation Latency

### Measured Baseline Performance

| Metric | Value | Notes |
|--------|-------|-------|
| Min | 3ms | Best-case scenario |
| Mean | 6.72ms | Average validation time |
| P50 (Median) | 4ms | Typical validation |
| P95 | 7ms | 95% complete within this time |
| P99 | 110ms | Outliers (GC, system load) |
| Max | 122ms | Worst-case observed |

### Recommended Test Targets

**With 20% safety margin above observed P95/P99:**

- **P50 Target:** ≤ 10ms (2.5x above median for stability)
- **P95 Target:** ≤ 10ms (conservative, allows system variance)
- **P99 Target:** ≤ 150ms (accounts for GC pauses and system load)

**Rationale:**
- P50/P95 of 10ms is achievable in >95% of validations
- P99 of 150ms accounts for occasional GC pauses and system load
- Provides stability across different hardware and system conditions

---

## 2. Cache Effectiveness

### Measured Baseline Performance

| Metric | Value | Notes |
|--------|-------|-------|
| Cache Miss Latency | 4ms | First validation of delta |
| Cache Hit Latency | 0ms | Cached validation (near-instant) |
| Speedup Factor | ∞x | Sub-millisecond cache hits |
| Hit Rate (typical) | 50% | With repeated validations |

### Recommended Test Targets

- **Min Cache Hit Rate:** ≥ 40% (realistic for varied workloads)
- **Cache Speedup:** ≥ 5x (conservative estimate for practical workloads)
- **Cache Hit Latency:** < 1ms (sub-millisecond for cached results)

**Rationale:**
- 40% hit rate is achievable with typical data patterns
- 5x speedup is conservative (actual is much higher)
- Accounts for cache maintenance overhead

---

## 3. High-Frequency Validation Throughput

### Measured Baseline Performance

| Metric | Value | Notes |
|--------|-------|-------|
| Total Duration (100 ops) | 848ms | Parallel validation |
| Average Latency | 8.48ms | Per-validation latency |
| Throughput | 117.92 ops/sec | Sustained rate |
| Cache Hit Rate | 0% | Unique deltas in benchmark |

### Recommended Test Targets

- **Min Throughput:** ≥ 80 ops/sec (70% of observed with margin)
- **Max Duration (100 ops):** ≤ 1500ms (50% buffer above observed)
- **Max Avg Latency:** ≤ 15ms (conservative per-validation target)

**Rationale:**
- 80 ops/sec is achievable under load
- 1500ms for 100 ops allows system variance
- 15ms avg latency accounts for contention

---

## 4. Validation Mode Performance

### Measured Baseline Performance

| Mode | Mean | P95 | P99 | Notes |
|------|------|-----|-----|-------|
| DELTA | 4.34ms | 6ms | 6ms | Validates only delta |
| INCREMENTAL | 4.20ms | 6ms | 6ms | Validates affected nodes |
| FULL | 4.06ms | 6ms | 6ms | Full re-validation |

### Recommended Test Targets

**With 30% safety margin for mode-specific variance:**

- **DELTA Mode:** ≤ 10ms (P95)
- **INCREMENTAL Mode:** ≤ 10ms (P95)
- **FULL Mode:** ≤ 10ms (P95)

**Rationale:**
- All modes perform similarly for small graphs
- 10ms target provides consistent expectations
- Full mode may degrade with larger graphs (future monitoring)

---

## 5. Violation Detection Overhead

### Measured Baseline Performance

| Metric | Value | Notes |
|--------|-------|-------|
| Valid Delta Mean | 4.30ms | No violations |
| Invalid Delta Mean | 4.38ms | With violations |
| Overhead | 1.9% | Minimal violation cost |

### Recommended Test Targets

- **Max Overhead:** ≤ 10% (conservative for complex violations)
- **Invalid Delta Latency:** ≤ 15ms (P95)

**Rationale:**
- 1.9% overhead is minimal for simple violations
- 10% target accounts for complex multi-violation scenarios
- Allows for detailed violation reporting

---

## 6. Performance Improvement Roadmap

### Short-term Optimizations (v4.1.0)

1. **Reduce P99 Latency Outliers**
   - Target: P99 < 50ms (down from 110ms)
   - Approach: Pre-warm parser caches, optimize graph traversal

2. **Improve Cache Hit Rate**
   - Target: 60% hit rate (up from 40%)
   - Approach: Semantic delta hashing, delta normalization

3. **Increase Throughput**
   - Target: 150 ops/sec (up from 80 ops/sec)
   - Approach: Batch validation, parallel shape checking

### Medium-term Optimizations (v4.2.0)

1. **Streaming Validation**
   - Target: 500+ ops/sec
   - Approach: Incremental shape validation, delta-aware algorithms

2. **Adaptive Caching**
   - Target: 80% hit rate
   - Approach: LRU with frequency tracking, predictive caching

3. **Distributed Validation**
   - Target: 1000+ ops/sec
   - Approach: Worker thread pool, graph partitioning

---

## 7. Performance Monitoring

### Key Metrics to Track

1. **Latency Percentiles:** P50, P95, P99
2. **Throughput:** Operations per second
3. **Cache Efficiency:** Hit rate, speedup factor
4. **Resource Usage:** Memory, CPU utilization
5. **Violation Detection:** Overhead percentage

### Alerting Thresholds

| Metric | Warning | Critical | Action |
|--------|---------|----------|--------|
| P95 Latency | > 15ms | > 25ms | Investigate hot paths |
| P99 Latency | > 200ms | > 500ms | Check GC, system load |
| Throughput | < 60 ops/sec | < 40 ops/sec | Scale horizontally |
| Cache Hit Rate | < 30% | < 20% | Review cache strategy |
| Error Rate | > 1% | > 5% | Investigate validation logic |

---

## 8. Test Implementation Guidelines

### Test Structure

```javascript
describe('Performance', () => {
  it('should meet P95 latency target', async () => {
    // Run 100 validations, measure P95
    expect(p95).toBeLessThan(10); // ms
  });

  it('should achieve minimum throughput', async () => {
    // Run 100 validations in parallel
    expect(throughput).toBeGreaterThan(80); // ops/sec
  });

  it('should maintain cache efficiency', async () => {
    // Validate same delta twice
    expect(cacheHitRate).toBeGreaterThan(0.4); // 40%
  });
});
```

### Performance Test Best Practices

1. **Warm-up:** Run 10-20 validations before measurement
2. **Sample Size:** Use 50-100 iterations for statistical significance
3. **Isolation:** Run performance tests separately from unit tests
4. **Stability:** Use timeouts and retries for flaky tests
5. **Monitoring:** Log actual performance alongside pass/fail

### Handling Performance Regressions

1. **Minor Regression (< 20%):** Document and monitor
2. **Moderate Regression (20-50%):** Investigate and optimize
3. **Major Regression (> 50%):** Block deployment, fix immediately

---

## 9. Hardware Considerations

### Minimum Requirements

- **CPU:** 2+ cores recommended for parallel validation
- **Memory:** 512MB RAM minimum, 1GB recommended
- **Storage:** Fast SSD for graph persistence

### Scaling Guidelines

| Load | Configuration | Expected Performance |
|------|---------------|---------------------|
| Low (< 10 ops/sec) | Single-threaded | P95 < 10ms |
| Medium (10-100 ops/sec) | Multi-threaded | P95 < 15ms |
| High (> 100 ops/sec) | Distributed | P95 < 20ms |

---

## 10. Conclusion

These performance targets provide realistic, achievable goals for the Real-time SHACL Validator while maintaining stability across different environments and workloads.

**Key Takeaways:**
- ✅ P95 latency of 10ms is achievable for typical validations
- ✅ 80+ ops/sec throughput supports real-world streaming use cases
- ✅ 40%+ cache hit rate provides significant performance benefits
- ✅ < 10% violation detection overhead maintains fast failure paths

**Next Steps:**
1. Update test assertions with these targets
2. Implement performance monitoring
3. Track metrics in production
4. Iterate on optimizations based on real-world data

---

**Generated:** 2025-11-20
**Benchmark Version:** scripts/benchmark-real-time-validator.mjs
**Validator Version:** v4.0.0

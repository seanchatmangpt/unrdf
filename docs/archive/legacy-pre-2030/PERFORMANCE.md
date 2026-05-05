# Performance Guide

Comprehensive guide to performance benchmarking, profiling, and optimization in UNRDF.

## Quick Start

```bash
# Run all benchmarks
npm run bench

# Run specific benchmark
npm run bench:hooks
npm run bench:receipts
npm run bench:sparql

# CPU profiling
npm run profile:cpu hook-execution

# Memory profiling
npm run profile:mem receipt-generation

# Update baseline (after performance improvements)
npm run bench:baseline
```

## Performance Budgets

UNRDF enforces strict performance budgets to prevent regressions. See `performance-budgets.yml` for complete list.

### Critical Path Budgets

| Operation | Budget (P95) | Rationale |
|-----------|--------------|-----------|
| Hook execution | <10μs | Core operation, executed millions of times |
| Receipt generation | <1ms | Cryptographic overhead acceptable |
| SPARQL simple query | <10ms | Interactive UX requirement |
| Test suite | <30s | Developer productivity |
| Build time | <60s | CI/CD efficiency |

## Benchmark Suite

### Available Benchmarks

1. **hook-execution-bench.mjs** - Hook system performance
   - Hook definition latency
   - Single hook execution
   - Hook chain execution
   - Registry operations
   - Batch processing

2. **receipt-generation-bench.mjs** - Cryptographic receipt performance
   - Receipt generation latency
   - Verification performance
   - Throughput under load

3. **sparql-query-bench.mjs** - Query engine performance
   - Simple SELECT queries
   - Filtered queries
   - JOIN operations
   - Aggregate queries

4. **task-activation-bench.mjs** - Task scheduling performance
   - Task activation latency
   - Queue operations
   - Dispatch overhead

5. **workflow-e2e-bench.mjs** - End-to-end workflow performance
   - Simple workflows
   - Complex multi-step workflows
   - Throughput testing

### Running Benchmarks

#### Run All Benchmarks

```bash
npm run bench
```

Output:
```
======================================================================
BENCHMARK SUITE SUMMARY
======================================================================

Total benchmarks: 5
Successful: 5
Failed: 0
Total time: 45.2s

Status: PASSED (all benchmarks within acceptable range)
```

#### Run Specific Benchmark

```bash
# Filter by name
node scripts/bench-all.mjs hook-execution
```

#### Compare with Baseline

```bash
# Detect regressions (default: >10% slower)
npm run bench

# Custom threshold
node scripts/bench-all.mjs --threshold 15
```

#### Update Baseline

After verified performance improvements:

```bash
npm run bench:baseline
```

**WARNING**: Only update baseline after:
1. Code review confirms changes are intentional
2. Performance improvements are reproducible
3. No functionality regressions

### Benchmark Output

Each benchmark outputs:
- Human-readable summary
- Statistical analysis (mean, median, P50, P90, P95, P99)
- Claims validation
- JSON results for automation

Example output:
```
HOOK EXECUTION PERFORMANCE BENCHMARK
======================================================================

Hook Definition (simple)...
  Mean: 12.5 us
  P95:  18.2 us
  P99:  25.1 us

Single Hook Execution...
  Mean: 4.2 us
  P95:  6.8 us
  P99:  9.3 us

CLAIMS VALIDATION
======================================================================

Claim: Hook execution <1ms
  Measured P95: 6.8 us (0.007 ms)
  Target:       1000 us (1.000 ms)
  Status:       PASS ✅
```

## Profiling

### CPU Profiling

Identify hot paths and optimization opportunities:

```bash
npm run profile:cpu hook-execution
```

This generates:
- `.cpuprofile` file for Chrome DevTools
- Flame graph visualization (if speedscope installed)
- Performance analysis report

#### Viewing CPU Profiles

**Option 1: Chrome DevTools**
1. Open Chrome DevTools (F12)
2. Go to "Performance" tab
3. Click "Load Profile"
4. Select `.cpuprofile` file

**Option 2: Speedscope (Recommended)**
```bash
npx speedscope profiles/hook-execution-*.cpuprofile
```

### Memory Profiling

Detect memory leaks and optimization opportunities:

```bash
npm run profile:mem receipt-generation
```

This generates:
- `.heapprofile` file for Chrome DevTools
- Memory growth analysis
- Leak detection report

#### Viewing Memory Profiles

**Chrome DevTools**
1. Open Chrome DevTools (F12)
2. Go to "Memory" tab
3. Click "Load" (bottom left)
4. Select `.heapprofile` file

#### Memory Analysis Report

```
Memory Analysis:
  Peak RSS: 142.5 MB
  Peak Heap Used: 89.3 MB
  Growth Rate: 0.05 MB/s
  Potential Leak: NO ✅
```

**Leak Detection**:
- Growth rate >1MB/s → Potential leak ⚠️
- Growth rate <0.1MB/s → Normal ✅

## Performance Regression Detection

### CI/CD Integration

Performance benchmarks run automatically on:
- **Pull Requests**: Detect regressions before merge
- **Main branch commits**: Update baseline
- **Daily**: Track performance trends

### Regression Thresholds

Default: **10% regression triggers failure**

Example regression report:
```
⚠️  PERFORMANCE REGRESSIONS DETECTED
======================================================================
hook-execution / execution.p95Us:
  Baseline: 6.5 us
  Current:  8.2 us
  Change:   +26.2% ⚠️

Status: FAILED (regressions detected)
```

### Budget Violations

Example budget violation:
```
❌ PERFORMANCE BUDGET VIOLATIONS
======================================================================
receipt-generation / p95Ms:
  Budget: 1.00 ms
  Actual: 1.35 ms
  Excess: +35.0%
```

## Optimization Workflow

### 1. Measure First

```bash
# Run baseline benchmarks
npm run bench
```

**Never optimize without measurement!**

### 2. Profile

```bash
# Identify bottlenecks
npm run profile:cpu <benchmark>
npm run profile:mem <benchmark>
```

### 3. Optimize

Focus on:
- **Hot paths** (>10% of CPU time)
- **High allocation sites** (>10% of memory)
- **P95/P99 tail latency** (impacts user experience)

### 4. Verify

```bash
# Run benchmarks again
npm run bench

# Compare with baseline
# Should show improvement (negative %)
```

### 5. Update Baseline

```bash
# After code review approval
npm run bench:baseline
git add benchmarks/baseline.json
git commit -m "perf: optimize hook execution (15% faster)"
```

## Performance Best Practices

### DO

✅ **Measure before optimizing**
- Run benchmarks to establish baseline
- Profile to identify bottlenecks
- Verify improvements with data

✅ **Focus on critical paths**
- Optimize hot paths (>10% CPU time)
- Reduce P95/P99 latency (user experience)
- Prioritize by impact × frequency

✅ **Use performance budgets**
- Enforce budgets in CI/CD
- Block PRs with regressions
- Track trends over time

✅ **Profile in production mode**
- `NODE_ENV=production`
- Realistic workloads
- Representative data

### DON'T

❌ **Don't optimize prematurely**
- "Premature optimization is the root of all evil"
- Measure first, optimize second
- Focus on actual bottlenecks

❌ **Don't trust intuition**
- Profile, don't guess
- Measure, don't assume
- Data over opinions

❌ **Don't sacrifice correctness**
- Performance < Correctness
- Never skip validation
- Test after optimization

❌ **Don't optimize in isolation**
- Consider system-wide impact
- Monitor downstream effects
- Track memory AND CPU

## Common Performance Patterns

### Pattern 1: Batch Operations

**Problem**: Individual operations too slow

**Solution**: Batch multiple operations

```javascript
// ❌ Slow: Individual operations
for (const item of items) {
  await processItem(item);
}

// ✅ Fast: Batch operations
await processBatch(items);
```

**Benchmark**: `hook-execution-bench.mjs` → Batch Execution

### Pattern 2: Lazy Initialization

**Problem**: Startup time too high

**Solution**: Initialize on first use

```javascript
// ❌ Slow: Eager initialization
const store = new Store();
await store.initialize();

// ✅ Fast: Lazy initialization
const store = new Store();
// Initialize on first use
```

**Benchmark**: `task-activation-bench.mjs` → Activation Latency

### Pattern 3: Caching

**Problem**: Repeated expensive computations

**Solution**: Cache results

```javascript
// ❌ Slow: Recompute every time
function getResult() {
  return expensiveComputation();
}

// ✅ Fast: Cache results
const cache = new Map();
function getResult(key) {
  if (!cache.has(key)) {
    cache.set(key, expensiveComputation(key));
  }
  return cache.get(key);
}
```

**Benchmark**: `sparql-query-bench.mjs` → Query Caching

### Pattern 4: Streaming

**Problem**: Large datasets cause memory issues

**Solution**: Process incrementally

```javascript
// ❌ Slow: Load all into memory
const allData = await loadAll();
return processAll(allData);

// ✅ Fast: Stream processing
return processStream(dataStream);
```

**Benchmark**: `workflow-e2e-bench.mjs` → Streaming Workflows

## Interpreting Results

### Statistical Metrics

- **Mean**: Average latency (sensitive to outliers)
- **Median (P50)**: Typical latency (robust to outliers)
- **P95**: 95% of requests faster than this
- **P99**: 99% of requests faster than this
- **Max**: Worst case latency

**Focus on P95/P99** for user experience.

### Throughput Metrics

- **ops/sec**: Operations per second
- **Sustainable throughput**: Throughput at <95% success rate

### Memory Metrics

- **RSS**: Total memory (OS perspective)
- **Heap Used**: JavaScript heap usage
- **External**: Native memory (buffers, etc.)

## Troubleshooting

### Benchmark Variability

**Symptom**: Results vary significantly between runs

**Solutions**:
1. Increase iterations (reduce statistical noise)
2. Extend warmup phase (reduce JIT impact)
3. Close background applications
4. Run on dedicated hardware (CI)

### Profiling Overhead

**Symptom**: Profiling makes code much slower

**Expected**:
- CPU profiling: 2-5x slower
- Memory profiling: 3-10x slower

**Solution**: Focus on relative differences, not absolute numbers

### Memory Leak Detection

**Symptom**: Memory grows continuously

**Steps**:
1. Run memory profiling: `npm run profile:mem <benchmark>`
2. Check growth rate in report
3. If >1MB/s: Take heap snapshots at different times
4. Compare snapshots in Chrome DevTools
5. Identify objects not being garbage collected

## Advanced Topics

### Custom Benchmarks

Create new benchmark in `benchmarks/`:

```javascript
import { performance } from 'perf_hooks';

const iterations = 10000;
const latencies = [];

for (let i = 0; i < iterations; i++) {
  const start = performance.now();
  await yourOperation();
  latencies.push(performance.now() - start);
}

const stats = calculateStats(latencies);

// Output JSON for automation
console.log('__JSON_RESULTS__');
console.log(JSON.stringify({
  benchmark: 'your-benchmark',
  results: stats,
}, null, 2));
```

### Custom Budgets

Edit `performance-budgets.yml`:

```yaml
your-benchmark:
  - your_metric_p95_us: 100  # <100μs P95
  - throughput_min: 10000    # >10k ops/sec
```

### Continuous Performance Monitoring

Track performance over time:

1. Daily benchmarks run automatically (2am UTC)
2. Results stored in GitHub Actions artifacts
3. Baseline updated on main branch
4. Trends tracked in git history

```bash
# View historical baselines
git log --all --oneline -- benchmarks/baseline.json
```

## Resources

- [Chrome DevTools Performance Profiling](https://developer.chrome.com/docs/devtools/performance/)
- [Node.js Performance Best Practices](https://nodejs.org/en/docs/guides/simple-profiling/)
- [V8 Profiling](https://v8.dev/docs/profile)
- [Speedscope (Flame Graph Viewer)](https://www.speedscope.app/)

## Support

Performance issues? Questions?

1. Check existing benchmarks for similar patterns
2. Run profiling to identify bottlenecks
3. Review this guide for optimization patterns
4. Open issue with benchmark results + profile data

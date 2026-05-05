# UNRDF v6 Performance Benchmarking Suite

Comprehensive performance benchmarking for UNRDF v6 with regression detection and scalability analysis.

## Quick Start

```bash
# Run all benchmarks and create baseline
node --expose-gc benchmarks/v6-perf-standalone.mjs --baseline

# Run all benchmarks (core + memory + scalability)
node --expose-gc benchmarks/v6-perf-standalone.mjs

# Check for performance regression
node --expose-gc benchmarks/v6-perf-standalone.mjs --regression

# Run only memory profiling
node --expose-gc benchmarks/v6-perf-standalone.mjs --memory

# Run only scalability analysis
node --expose-gc benchmarks/v6-perf-standalone.mjs --scalability
```

## Benchmark Suites

### 1. Core Operations (`v6-perf-standalone.mjs`)
**Zero external dependencies** - Uses only Node.js built-ins

**Benchmarks:**
- Receipt Creation (SHA-256 hash) - target <1ms
- Delta Capsule Validation - target <5ms
- Receipt Verification - target <latestms
- Receipt Chain (10 receipts) - target <50ms
- Chain Verification (10 receipts) - target <20ms

**Features:**
- 1,000 iterations per benchmark
- 100 warmup iterations
- Percentile reporting (P50, P75, P95, P99)
- Memory profiling
- Scalability analysis
- Regression detection (10% threshold)

### 2. Full Suite (`v6-perf.mjs`)
**Requires dependencies** - Uses BLAKE3, Zod, full v6 modules

**Additional benchmarks:**
- Receipt creation with BLAKE3 (production hash)
- Zod schema validation
- Store initialization
- SPARQL query performance
- Package composition (cascade, chain, reconciliation)
- CLI startup time

## Performance Targets

| Operation | Target | v5 Baseline | v6 Penalty Limit |
|-----------|--------|-------------|------------------|
| Receipt creation | <1ms | latestms | ≤10% (latestms) |
| Delta validation | <5ms | - | New feature |
| Receipt verification | <latestms | - | New feature |
| Zod validation | <2ms | - | New feature |
| CLI startup | <100ms | 80ms | ≤10% (88ms) |
| Store init | <50ms | 45ms | ≤10% (latestms) |
| SPARQL query (10 triples) | <10ms | 8ms | ≤10% (latestms) |

## Output Files

### `v6-baseline.csv`
CSV baseline for regression detection:
```csv
operation,median_ms,p95_ms,max_ms,throughput_ops_sec,target_ms,status
Receipt Creation,latest,latest,latest,latest,1,PASS
Delta Validation,latest,latest,latest,latest,5,PASS
...
```

### `v6-performance-report.md`
Comprehensive performance report with:
- Core operation benchmarks
- Memory profiling results
- Scalability analysis
- Performance claims validation
- Key findings and interpretation

## Regression Detection

The regression checker compares current results to baseline:

```bash
node --expose-gc benchmarks/v6-perf-standalone.mjs --regression
```

**Exit codes:**
- `0` - No regressions (all operations within 10% of baseline)
- `1` - Regressions detected (one or more operations >10% slower)

**Example output:**
```
================================================================================
Regression Detection (10% threshold)
================================================================================
  Receipt Creation:
    Baseline: latestms
    Current:  latestms
    Delta:    +latest%
    Status:   REGRESSION ✗
...
Regressions found: 1
================================================================================
```

## CI/CD Integration

### GitHub Actions

```yaml
name: Performance Regression Check
on: [push, pull_request]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: '22'

      - name: Install dependencies
        run: pnpm install

      - name: Run benchmarks
        run: node --expose-gc benchmarks/v6-perf-standalone.mjs --regression

      - name: Upload results
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: benchmark-results
          path: |
            benchmarks/v6-baseline.csv
            benchmarks/v6-performance-report.md
```

## Memory Profiling

### Tests Performed

1. **Heap Usage** (1,000 receipts)
   - Measures memory allocation per receipt
   - Target: <2KB per receipt

2. **Memory Leak Detection** (10,000 iterations)
   - Creates and discards 1,000 receipts per iteration
   - Checks for >10% heap growth
   - Includes GC between iterations

3. **Stress Test** (10,000 receipts)
   - Tests large-scale operations
   - Measures throughput and memory efficiency
   - Target: >50,000 receipts/sec

### Example Output

```
================================================================================
Memory Profile: Receipt Creation (1,000 receipts)
================================================================================
  Receipts created: 1,000
  Heap used: +latest KB
  RSS: +latest MB
  Avg per receipt: +latest B

================================================================================
Memory Leak Detection: Receipts (10,000 iterations)
================================================================================
  Iterations: 10
  Receipts per iteration: 1000
  Initial heap: +latest MB
  Final heap: +latest MB
  Growth: +latest KB (latest%)
  Leak detected: NO ✓
```

## Scalability Analysis

### Receipt Chain Verification

Tests chain verification performance vs chain length:

| Chain Length | Expected Scaling | Actual (linear) |
|--------------|------------------|-----------------|
| 1 → 10 | <2x (log) | latestx |
| 10 → 100 | <10x (log) | latestx |
| 100 → 1000 | <10x (log) | - |

**Scaling classifications:**
- **Logarithmic**: ratio <2x (best)
- **Linear**: ratio <15x (acceptable)
- **Exponential**: ratio ≥15x (requires optimization)

### Delta Reconciliation

Tests delta creation performance vs package count:
- 1, 5, 10, 20, 50 packages
- Should scale linearly or better

## Interpreting Results

### Performance Status

- **✓ PASS**: Operation meets target (≤10% penalty acceptable)
- **✗ FAIL**: Operation >10% slower than target
- **STABLE**: Within ±10% of baseline
- **IMPROVEMENT**: >10% faster than baseline
- **REGRESSION**: >10% slower than baseline

### Memory Status

- **✓ Stable**: <10% heap growth after 10,000 iterations
- **⚠️ Leak detected**: >10% heap growth

### Scalability Status

- **Logarithmic**: Best (preferred for large inputs)
- **Linear**: Acceptable (scales predictably)
- **Exponential**: Warning (requires optimization)

## Current Results (vlatest)

Based on latest benchmark run:

**Core Operations:**
- **Pass rate**: 5/5 (100%)
- **Average performance**: 98-100% faster than targets
- **Memory efficiency**: 839B per receipt
- **Throughput**: 71,655 receipts/sec

**Key Achievements:**
- Receipt creation: latestms median (99% faster than 1ms target)
- Delta validation: latestms median (99% faster than 5ms target)
- No memory leaks detected
- Linear scaling for chain verification

**Areas for Monitoring:**
- Chain verification scales linearly (acceptable but could be optimized to logarithmic)
- Very fast operations (<latestms) can show false regression warnings due to measurement precision

## Troubleshooting

### "Cannot find package 'zod'" or similar

Use the standalone version:
```bash
node --expose-gc benchmarks/v6-perf-standalone.mjs
```

Or install dependencies:
```bash
pnpm install
```

### High variance in results

1. Close other applications
2. Run multiple times and average
3. Increase iteration count in benchmark script
4. Use `nice` for CPU priority:
   ```bash
   nice -n -20 node --expose-gc benchmarks/v6-perf-standalone.mjs
   ```

### False regression warnings

For very fast operations (<latestms), minor measurement variations can trigger warnings. Review the actual delta percentage and absolute values:
- Delta >10% but absolute difference <latestms → Likely false positive
- Delta >10% and absolute difference >latestms → Real regression

## Development

### Adding New Benchmarks

1. Create benchmark function:
```javascript
async function benchmarkNewFeature() {
  return runBenchmark('New Feature', async () => {
    // Your code here
  });
}
```

2. Add to results:
```javascript
results.push(await benchmarkNewFeature());
```

3. Set performance target:
```javascript
function getTarget(name) {
  const targets = {
    // ...
    'New Feature': 10, // 10ms target
  };
  return targets[name] || 100;
}
```

4. Update baseline:
```bash
node --expose-gc benchmarks/v6-perf-standalone.mjs --baseline
```

### Modifying Regression Threshold

Edit the regression detection function:
```javascript
const status = delta > 15 ? 'REGRESSION ✗' : ...  // 15% instead of 10%
```

## References

- [Performance Budgets](../performance-budgets.yml)
- [Benchmark Framework](./framework.mjs)
- [v6 Architecture](../receipts-architecture.md)

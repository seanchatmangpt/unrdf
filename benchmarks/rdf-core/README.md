# RDF Core Performance Benchmarks

Comprehensive performance benchmarking suite for @unrdf/core RDF operations.

## Overview

This benchmark suite measures the performance of core RDF operations:

1. **Parsing** - Turtle, N-Triples, JSON-LD parsing
2. **Serialization** - Turtle and N-Triples serialization
3. **SPARQL Queries** - SELECT, ASK, CONSTRUCT query execution
4. **Graph Operations** - Add, remove, find, count operations
5. **Validation** - Quad and store validation with Zod

## Quick Start

```bash
# Quick run (~30 seconds)
node benchmarks/rdf-core/quick-run.mjs

# Full suite (~2-5 minutes)
node benchmarks/rdf-core/runner.mjs

# Save baseline for regression detection
node benchmarks/rdf-core/runner.mjs --save-baseline
```

## Benchmark Structure

```
benchmarks/rdf-core/
├── 01-parsing.bench.mjs           # Parser performance
├── 02-serialization.bench.mjs     # Serializer performance
├── 03-sparql-queries.bench.mjs    # SPARQL execution
├── 04-graph-operations.bench.mjs  # Graph operations
├── 05-validation.bench.mjs        # Validation performance
├── suite.mjs                      # Utilities and reporting
├── runner.mjs                     # Full benchmark runner
├── quick-run.mjs                  # Fast benchmark runner
├── baselines/baseline.json        # Performance baselines
├── PERFORMANCE-REPORT.md          # Latest performance report
└── README.md                      # This file
```

## Benchmark Details

### 01-parsing.bench.mjs

Measures parsing performance across different formats and dataset sizes:

- **Turtle Parsing**: Small (100), Medium (1K), Large (10K) triples
- **N-Triples Parsing**: Small (100), Medium (1K), Large (10K) triples
- **JSON-LD Parsing**: (Future)

**Performance Targets**:
- Small (100 triples): P95 < 50ms
- Medium (1K triples): P95 < 200ms
- Large (10K triples): P95 < 1000ms

### 02-serialization.bench.mjs

Measures serialization performance:

- **Turtle Serialization**: Small, Medium, Large datasets
- **N-Triples Serialization**: Small, Medium, Large datasets

**Performance Targets**:
- Small: P95 < 50ms
- Medium: P95 < 200ms
- Large: P95 < 1000ms

### 03-sparql-queries.bench.mjs

Measures SPARQL query execution performance:

- **SELECT Queries**: Simple, Filter, Join patterns
- **ASK Queries**: Simple and complex patterns
- **CONSTRUCT Queries**: Graph transformation

**Performance Targets**:
- Simple queries: P95 < 10ms
- Filter queries: P95 < 20ms
- Join queries: P95 < 30ms
- ASK queries: P95 < 5ms

### 04-graph-operations.bench.mjs

Measures core graph operations:

- **Add**: Bulk add operations (100, 1K, 10K quads)
- **Remove**: Bulk remove operations
- **Find**: Pattern matching queries
- **Count**: Quad counting

**Performance Targets**:
- Add (100): P95 < 10ms
- Add (1K): P95 < 50ms
- Find: P95 < 5ms
- Count: P95 < 1ms

### 05-validation.bench.mjs

Measures validation performance:

- **Quad Validation**: Single quad Zod validation
- **Store Validation**: Full store validation
- **Term Validation**: Term type checking

**Performance Targets**:
- Quad validation: P95 < 0.1ms
- Store validation (100): P95 < 5ms
- Store validation (1K): P95 < 30ms

## Performance Metrics

All benchmarks measure:

- **Latency**:
  - Mean (average)
  - P50 (median)
  - P95 (95th percentile)
  - P99 (99th percentile)
- **Throughput**: Operations per second
- **Memory**: Memory usage (future)

## Regression Detection

Benchmarks include regression detection:

- **Threshold**: ±20% change from baseline
- **Baseline**: Stored in `baselines/baseline.json`
- **Auto-detect**: Regressions flagged in report

## Running Benchmarks

### Quick Run (Recommended for CI/CD)

```bash
node benchmarks/rdf-core/quick-run.mjs
```

**Features**:
- Reduced iterations for speed
- Core operations only
- ~30 second execution
- Suitable for CI/CD

### Full Suite

```bash
node benchmarks/rdf-core/runner.mjs
```

**Features**:
- All benchmarks
- Multiple dataset sizes
- Comprehensive results
- ~2-5 minute execution

### Options

```bash
# Verbose output
node runner.mjs --verbose

# Save baseline
node runner.mjs --save-baseline

# Run specific suite
node runner.mjs --suite=sparql

# Quick mode (reduced iterations)
node runner.mjs --quick
```

## Interpreting Results

### Console Output

```
✓ parsing.turtle-small: P95=3.497ms (P95 < 50ms)
✗ sparql.select-simple: P95=26.118ms (P95 < 10ms)
```

- ✓ = Passed performance target
- ✗ = Failed performance target (regression or needs optimization)

### JSON Report

Detailed results saved to `rdf-core-benchmarks-YYYY-MM-DDTHH-MM-SS.json`:

```json
{
  "timestamp": "2026-01-11T06:22:59.036Z",
  "summary": {
    "totalBenchmarks": 6,
    "passed": 4,
    "failed": 2
  },
  "benchmarks": {
    "parsing.turtle-small": {
      "latency": {
        "mean": 0.292,
        "p50": 0.090,
        "p95": 3.497,
        "p99": 3.497
      },
      "passed": true
    }
  }
}
```

## Performance Report

See [PERFORMANCE-REPORT.md](./PERFORMANCE-REPORT.md) for detailed analysis, optimization recommendations, and comparison with baselines.

## CI/CD Integration

### GitHub Actions

```yaml
- name: Run RDF Core Benchmarks
  run: |
    node benchmarks/rdf-core/quick-run.mjs

- name: Upload Benchmark Results
  uses: actions/upload-artifact@v3
  with:
    name: benchmark-results
    path: benchmarks/rdf-core/rdf-core-benchmarks-*.json
```

### Performance Regression Gates

Benchmarks fail CI if:
- Any benchmark exceeds target by >100%
- Regressions detected (>20% slower than baseline)

## Known Issues

### Current Performance Issues

1. **SPARQL P95 Latency**: 26ms vs 10ms target
   - Root cause: Query plan generation overhead
   - Fix: Implement query plan caching

2. **Graph Add P95 Latency**: 16.7ms vs 10ms target
   - Root cause: Batch operation optimization needed
   - Fix: Optimize Oxigraph bulk insert

See [PERFORMANCE-REPORT.md](./PERFORMANCE-REPORT.md) for detailed analysis.

## Contributing

### Adding New Benchmarks

1. Create `0X-feature.bench.mjs`
2. Export `runFeatureBenchmarks()` function
3. Add to `runner.mjs` imports and suite list
4. Update this README

### Benchmark Best Practices

1. **Warmup**: Run 5-10 iterations before measurement
2. **Iterations**: Use 20+ iterations for stable results
3. **Isolation**: Each benchmark should be independent
4. **Cleanup**: Clean up resources after benchmarks
5. **Reproducibility**: Use deterministic data generation

### Example Benchmark

```javascript
export async function runMyBenchmarks() {
  console.log('\n▶ Running My Benchmarks...');

  const results = {};
  const latencies = [];

  // Warmup
  for (let i = 0; i < 10; i++) {
    await myOperation();
  }

  // Measure
  for (let i = 0; i < 100; i++) {
    const start = performance.now();
    await myOperation();
    latencies.push(performance.now() - start);
  }

  const stats = analyzeVariance(latencies);

  results['my-operation'] = {
    latency: stats,
    passed: stats.p95 < TARGET_MS,
    target: `P95 < ${TARGET_MS}ms`,
    unit: 'ops/s',
  };

  return {
    results,
    summary: {
      total: 1,
      passed: results['my-operation'].passed ? 1 : 0,
      failed: 0,
    }
  };
}
```

## Troubleshooting

### Benchmarks Taking Too Long

Use quick run mode:
```bash
node benchmarks/rdf-core/quick-run.mjs
```

### Inconsistent Results

1. Ensure no other processes running
2. Disable CPU throttling
3. Increase warmup iterations
4. Use `--expose-gc` for accurate memory profiling

### Memory Issues

```bash
node --max-old-space-size=4096 benchmarks/rdf-core/runner.mjs
```

## References

- [PERFORMANCE-REPORT.md](./PERFORMANCE-REPORT.md) - Latest performance analysis
- [@unrdf/core README](../../packages/core/README.md) - Core package documentation
- [Oxigraph](https://github.com/oxigraph/oxigraph) - Underlying SPARQL engine

---

**Maintainer**: UNRDF Performance Team
**Last Updated**: 2026-01-11
**Benchmark Version**: 1.0.0

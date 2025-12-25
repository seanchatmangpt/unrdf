# UNRDF Comprehensive Benchmark Suite

Quantitative performance analysis for all UNRDF innovations with statistical significance, reproducibility, and regression detection.

## Overview

This benchmark suite provides comprehensive performance testing across all major components of the UNRDF platform:

- **Core Performance**: Workflow, engine, and SPARQL operations
- **Integration**: Federation, streaming, and knowledge engine
- **Advanced Features**: Blockchain receipts and visualization
- **Regression Testing**: Baseline comparison and memory leak detection

## Quick Start

```bash
# Run all benchmarks
pnpm benchmark

# Run specific category
pnpm benchmark:core
pnpm benchmark:integration
pnpm benchmark:advanced
pnpm benchmark:regression

# Generate reports
pnpm benchmark:report

# Baseline management
pnpm benchmark:baseline        # Save current as baseline
pnpm benchmark:compare         # Compare against baseline

# Memory leak detection
pnpm benchmark:memory
```

## Architecture

### Framework (`framework.mjs`)

Core benchmarking utilities providing:

- **Statistical Analysis**
  - Percentile calculations (P50, P75, P95, P99, P99.9)
  - Mean, median, standard deviation
  - Minimum 1000 iterations for significance

- **Performance Measurement**
  - Throughput (operations/second)
  - Latency (milliseconds)
  - Memory usage (RSS, heap, external)
  - CPU time

- **Reproducibility**
  - Warmup runs to eliminate JIT effects
  - GC between benchmarks
  - Fixed random seeds (optional)

- **Reporting**
  - Markdown tables
  - Detailed reports
  - Comparison reports

### Core Benchmarks

#### Workflow Performance (`core/workflow-performance.mjs`)

Measures workflow creation, validation, and serialization:

- Create simple workflow (5 tasks): ~5,000+ ops/sec
- Create medium workflow (20 tasks): ~2,500+ ops/sec
- Create large workflow (100 tasks): ~500+ ops/sec
- Validate workflow: ~5,000+ ops/sec
- Serialize to RDF: ~1,000+ ops/sec

#### Engine Performance (`core/engine-performance.mjs`)

Measures YAWL engine operations:

- Engine initialization: ~1,000+ ops/sec
- Register workflow: ~5,000+ ops/sec
- Create case: ~5,000+ ops/sec
- Task execution: ~3,000+ ops/sec
- Complete workflow: ~1,000+ ops/sec

#### SPARQL Performance (`core/sparql-performance.mjs`)

Measures RDF store operations:

- Store creation: ~1,000+ ops/sec
- Insert single triple: ~10,000+ ops/sec
- Insert batch (100 triples): ~1,000+ ops/sec
- Query (100 triples): ~5,000+ ops/sec
- Query with filter: ~3,000+ ops/sec

### Integration Benchmarks

#### Federation (`integration/federation-benchmark.mjs`)

Measures distributed query performance:

- Create federated engine (2 endpoints): ~1,000+ ops/sec
- Query single endpoint: ~3,000+ ops/sec
- Federated query (2 endpoints): ~2,000+ ops/sec
- Parallel queries: ~1,000+ ops/sec

#### Streaming (`integration/streaming-benchmark.mjs`)

Measures event streaming:

- Create stream: ~10,000+ ops/sec
- Push single event: ~100,000+ ops/sec
- Push and consume: ~50,000+ ops/sec
- Batch operations: ~1,000+ ops/sec
- Backpressure handling: ~5,000+ ops/sec

#### Knowledge Engine (`integration/knowledge-engine-benchmark.mjs`)

Measures knowledge graph operations:

- Insert entity: ~5,000+ ops/sec
- Query by ID (100 entities): ~10,000+ ops/sec
- Query by ID (1000 entities): ~5,000+ ops/sec
- Graph traversal (2 hops): ~2,000+ ops/sec
- Aggregate queries: ~2,000+ ops/sec

### Advanced Benchmarks

#### Blockchain Receipts (`advanced/blockchain-receipt-benchmark.mjs`)

Measures cryptographic operations:

- Create receipt: ~10,000+ ops/sec
- Generate hash: ~50,000+ ops/sec
- Generate signature: ~20,000+ ops/sec
- Verify signature: ~20,000+ ops/sec
- Merkle tree (10 receipts): ~5,000+ ops/sec
- Merkle tree (100 receipts): ~1,000+ ops/sec

#### Visualization (`advanced/visualization-benchmark.mjs`)

Measures graph rendering:

- Create graph (100 nodes): ~5,000+ ops/sec
- Create graph (1000 nodes): ~80+ ops/sec
- Create graph (10000 nodes): ~2+ ops/sec
- Force-directed layout (100 nodes): ~7,500+ ops/sec
- Hierarchical layout (100 nodes): ~7,500+ ops/sec
- Serialize to JSON (100 nodes): ~5,000+ ops/sec

### Regression Testing

#### Baseline Comparison (`regression/baseline-comparison.mjs`)

Compares current performance against saved baseline:

- Loads baseline from `benchmarks/baselines/baseline.json`
- Flags regressions >5% in throughput
- Flags regressions >5% in latency (P95)
- Flags regressions >10% in memory usage
- Generates comparison report

**Usage**:

```bash
# Save baseline (first run or after improvements)
pnpm benchmark:baseline

# Compare against baseline (CI/CD)
pnpm benchmark:compare

# Exit code 1 if regressions detected
```

#### Memory Leak Detection (`regression/memory-leak-detection.mjs`)

Detects memory leaks through controlled testing:

- Monitors memory over time
- Forces GC at intervals
- Measures retained memory
- Calculates growth rate
- Reports potential leaks

**Usage**:

```bash
# Run with GC exposed for accurate results
pnpm benchmark:memory

# Or manually
node --expose-gc benchmarks/regression/memory-leak-detection.mjs
```

## Metrics

### Throughput

Operations per second (ops/sec):

- **High**: >10,000 ops/sec
- **Medium**: 1,000-10,000 ops/sec
- **Low**: <1,000 ops/sec

### Latency

Percentiles (milliseconds):

- **P50 (Median)**: Typical case
- **P95**: 95% of requests
- **P99**: 99% of requests
- **P99.9**: Extreme cases

Target latencies:

- **Fast**: P95 < 1ms
- **Acceptable**: P95 < 10ms
- **Slow**: P95 > 10ms

### Memory

Memory usage (bytes):

- **RSS**: Resident Set Size (total memory)
- **Heap Used**: JavaScript heap usage
- **Heap Total**: Total heap allocated
- **External**: External memory (buffers, etc.)

## Reports

### Summary Report

Quick overview with key metrics:

- Location: `benchmarks/reports/benchmark-summary.md`
- Contains: Tables with throughput, latency, memory
- Format: Markdown tables

### Detailed Report

Comprehensive analysis:

- Location: `benchmarks/reports/benchmark-detailed.md`
- Contains: All percentiles, statistics, memory
- Format: Detailed markdown sections

### Comparison Report

Baseline regression analysis:

- Location: `benchmarks/reports/baseline-comparison.md`
- Contains: Regressions, improvements, unchanged
- Format: Markdown tables with deltas

## CI/CD Integration

### GitHub Actions

```yaml
name: Benchmarks

on:
  push:
    branches: [main]
  pull_request:

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: pnpm/action-setup@v2
      - uses: actions/setup-node@v3
        with:
          node-version: 18
          cache: pnpm

      - run: pnpm install
      - run: pnpm benchmark:compare

      # Upload reports as artifacts
      - uses: actions/upload-artifact@v3
        with:
          name: benchmark-reports
          path: benchmarks/reports/
```

### Performance Budgets

Set thresholds in CI:

```bash
# Fail if throughput drops >5%
# Fail if latency increases >5%
# Fail if memory increases >10%

pnpm benchmark:compare || exit 1
```

## Best Practices

### Writing Benchmarks

1. **Statistical Significance**: Minimum 1000 iterations
2. **Warmup**: At least 100 iterations (10% of total)
3. **GC Management**: Force GC between benchmarks
4. **Isolation**: Each benchmark should be independent
5. **Reproducibility**: Use fixed random seeds when needed

### Interpreting Results

1. **Focus on Percentiles**: P95/P99 more important than mean
2. **Watch for Variability**: High std dev indicates instability
3. **Consider Context**: Throughput vs latency tradeoffs
4. **Memory Trends**: Growing memory = potential leak
5. **Baseline Comparison**: Track regressions over time

### Performance Optimization

1. **Identify Bottlenecks**: Run detailed benchmarks
2. **Profile**: Use Node.js profiler for hotspots
3. **Optimize**: Make targeted improvements
4. **Measure**: Run benchmarks again
5. **Compare**: Verify improvements against baseline

## Troubleshooting

### Benchmarks Too Slow

- Reduce iterations (but maintain >1000 for significance)
- Focus on specific categories
- Use `benchmark:core` instead of `benchmark`

### High Variability

- Ensure no other processes running
- Disable CPU frequency scaling
- Use dedicated benchmark machine
- Increase warmup iterations

### Memory Issues

- Run with `--expose-gc` for accurate measurements
- Check for memory leaks with `benchmark:memory`
- Monitor GC activity
- Reduce iteration count if OOM

### CI/CD Failures

- Check baseline is committed to repo
- Verify Node.js version matches
- Ensure clean state (no cached data)
- Review regression thresholds

## Development

### Adding New Benchmarks

1. Create file in appropriate category directory
2. Import `suite` from `framework.mjs`
3. Define benchmarks with config
4. Add to `run-all.mjs` registry
5. Run and verify results

Example:

```javascript
import { suite } from '../framework.mjs';

export const myBenchmarks = suite('My Feature Performance', {
  'operation name': {
    fn: () => {
      // Code to benchmark
    },
    iterations: 10000,
    warmup: 1000
  }
});
```

### Running Individual Benchmarks

```bash
# Run single benchmark file
node benchmarks/core/workflow-performance.mjs
node benchmarks/advanced/visualization-benchmark.mjs

# Run with custom iterations
node benchmarks/core/workflow-performance.mjs --iterations=5000
```

## License

Same as UNRDF project.

## Contributing

See main UNRDF CONTRIBUTING.md for guidelines.

## Support

- Issues: https://github.com/unrdf/unrdf/issues
- Discussions: https://github.com/unrdf/unrdf/discussions

# Daemon Performance Benchmarking Guide

## Overview

The @unrdf/daemon benchmarking suite provides comprehensive performance measurement and regression detection across all critical operations. This guide explains how to run benchmarks, interpret results, and integrate them into your CI/CD pipeline.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Benchmark Suites](#benchmark-suites)
3. [Running Benchmarks](#running-benchmarks)
4. [Interpreting Results](#interpreting-results)
5. [Regression Detection](#regression-detection)
6. [CI/CD Integration](#cicd-integration)
7. [Baseline Management](#baseline-management)
8. [Performance Targets](#performance-targets)

## Quick Start

### Install Benchmark Dependencies

```bash
# No additional dependencies needed - benchmarks use built-in modules
pnpm install
```

### Run All Benchmarks

```bash
# Run complete benchmark suite
node benchmarks/runner.mjs

# Run specific benchmark
node benchmarks/runner.mjs --benchmark=operation-scheduling

# Run with custom options
node benchmarks/runner.mjs --runs=20 --verbose
```

### View Results

```bash
# View latest report
cat benchmarks/benchmarks-*.json | jq '.summary'

# Generate HTML report (optional)
node benchmarks/reporter.mjs --html
```

## Benchmark Suites

### 1. Operation Scheduling Latency

**File**: `01-operation-scheduling.bench.mjs`

Measures the time required to schedule operations into the daemon queue.

**Metrics**:
- Mean latency per operation
- Min/max latency
- Standard deviation
- Variance (coefficient of variation)
- Queue impact analysis

**Expected Performance**:
- Baseline: 0.15ms mean
- Target: < 0.5ms p99
- Regression threshold: 15% increase

**Example**:
```javascript
import { benchmarkOperationScheduling } from './01-operation-scheduling.bench.mjs';

const result = await benchmarkOperationScheduling({
  runs: 10,
  operationsPerRun: 100
});

console.log(`Mean latency: ${result.value}ms`);
```

### 2. Concurrent Operation Throughput

**File**: `02-concurrent-throughput.bench.mjs`

Measures how many operations can execute concurrently per second.

**Metrics**:
- Operations per second
- Latency percentiles (p50, p95, p99)
- Concurrency impact analysis
- Throughput under varying loads

**Expected Performance**:
- Baseline: 5200 ops/sec
- Target: > 5000 ops/sec
- Regression threshold: 15% decrease

**Example**:
```javascript
import { benchmarkConcurrentThroughput } from './02-concurrent-throughput.bench.mjs';

const result = await benchmarkConcurrentThroughput({
  operationCount: 100,
  runs: 5,
  opDuration: 1
});

console.log(`Throughput: ${result.value} ops/sec`);
console.log(`P95 latency: ${result.p95}ms`);
```

### 3. Memory Usage Under Load

**File**: `03-memory-load.bench.mjs`

Measures heap and RSS memory consumption with varying operation queue sizes.

**Metrics**:
- Bytes per operation
- Peak memory usage
- Memory stability
- Garbage collection impact

**Expected Performance**:
- Baseline: 4200 bytes/operation
- Target: < 5000 bytes/operation
- Regression threshold: 25% increase

**Example**:
```javascript
import { benchmarkMemoryWithLoad } from './03-memory-load.bench.mjs';

const result = await benchmarkMemoryWithLoad({
  maxOperations: 10000,
  steps: 5
});

console.log(`Memory per operation: ${result.value} bytes`);
console.log(`Peak memory: ${result.measurements[4].heapDelta} bytes`);
```

### 4. Raft Replication Latency

**File**: `04-raft-replication.bench.mjs`

Simulates and measures Raft consensus replication across cluster nodes.

**Metrics**:
- Leader replication latency
- Consensus commit latency
- Log replication throughput
- Quorum acknowledgment time

**Expected Performance**:
- Leader replication: 8.5ms mean
- Consensus commit: 12.3ms mean
- Replication throughput: > 1000 ops/sec
- Regression threshold: 15% increase

**Example**:
```javascript
import { benchmarkLeaderReplication } from './04-raft-replication.bench.mjs';

const result = await benchmarkLeaderReplication({
  nodeCount: 3,
  operationCount: 100,
  runs: 5
});

console.log(`Replication latency: ${result.value}ms`);
```

### 5. YAWL Workflow Execution

**File**: `05-yawl-execution.bench.mjs`

Measures workflow execution performance for different YAWL patterns.

**Metrics**:
- Sequential workflow execution time
- Parallel workflow execution time
- Conditional workflow execution time
- Mixed workflow throughput

**Expected Performance**:
- Sequential: 125.5ms per workflow
- Parallel: 85.3ms per workflow
- Conditional: 95.8ms per workflow
- Mixed throughput: > 8 workflows/sec
- Regression threshold: 15% increase

**Example**:
```javascript
import { benchmarkSequentialWorkflow } from './05-yawl-execution.bench.mjs';

const result = await benchmarkSequentialWorkflow({
  stepsPerWorkflow: 5,
  workflowCount: 20,
  runs: 5
});

console.log(`Workflow execution: ${result.value}ms`);
```

## Running Benchmarks

### Create a Benchmark Runner

```javascript
// benchmarks/runner.mjs
import { generateReport, loadBaseline, saveReport, formatReportForConsole } from './suite.mjs';
import { benchmarkOperationScheduling } from './01-operation-scheduling.bench.mjs';
import { benchmarkConcurrentThroughput } from './02-concurrent-throughput.bench.mjs';
import { benchmarkMemoryWithLoad } from './03-memory-load.bench.mjs';
import { benchmarkLeaderReplication } from './04-raft-replication.bench.mjs';
import { benchmarkSequentialWorkflow } from './05-yawl-execution.bench.mjs';

async function runAllBenchmarks() {
  console.log('Starting daemon benchmark suite...\n');

  const results = {};

  // Run each benchmark
  results['operation-scheduling-latency'] =
    await benchmarkOperationScheduling({ runs: 10 });

  results['concurrent-execution-throughput'] =
    await benchmarkConcurrentThroughput({ operationCount: 100, runs: 5 });

  results['memory-usage-with-load'] =
    await benchmarkMemoryWithLoad({ maxOperations: 10000, steps: 5 });

  results['raft-leader-replication'] =
    await benchmarkLeaderReplication({ nodeCount: 3, operationCount: 100, runs: 5 });

  results['yawl-sequential-workflow'] =
    await benchmarkSequentialWorkflow({ stepsPerWorkflow: 5, workflowCount: 20, runs: 5 });

  // Generate report
  const baseline = loadBaseline();
  const report = generateReport(results, baseline);

  // Save and display results
  saveReport(report);
  console.log(formatReportForConsole(report));

  // Exit with error if regressions detected
  if (report.summary.totalRegressions > 0) {
    process.exit(1);
  }
}

runAllBenchmarks().catch(error => {
  console.error('Benchmark failed:', error);
  process.exit(1);
});
```

### Run with Timeout

```bash
# Run with 5-minute timeout
timeout 300s node benchmarks/runner.mjs

# Run specific benchmark with timeout
timeout 60s node benchmarks/runner.mjs --benchmark=operation-scheduling
```

## Interpreting Results

### Report Structure

```json
{
  "timestamp": "2025-01-11T12:30:45.123Z",
  "summary": {
    "totalBenchmarks": 15,
    "totalRegressions": 0,
    "criticalRegressions": 0,
    "warningRegressions": 0,
    "passRate": "100.00"
  },
  "regressions": [],
  "results": {
    "operation-scheduling-latency": {
      "benchmark": { /* current result */ },
      "baseline": { /* baseline data */ },
      "regression": {
        "isRegression": false,
        "ratio": "0.9800",
        "percentChange": "-2.00%",
        "severity": "none"
      }
    }
  }
}
```

### Understanding Metrics

#### Latency Metrics
- **Mean**: Average latency across all samples
- **P50/P95/P99**: Percentile latencies (50th, 95th, 99th)
- **Variance**: Coefficient of variation (lower is more stable)
- **Min/Max**: Minimum and maximum observed values

#### Throughput Metrics
- **Value**: Operations per second (or workflows/sec)
- **Variance**: Consistency of throughput across runs
- **Regression**: Decrease below baseline threshold

#### Memory Metrics
- **Bytes/Op**: Memory consumption per queued operation
- **Peak**: Maximum memory used during benchmark
- **Variance**: Memory stability (GC impact)

### Interpreting Variance

```
Variance < 5%:   Excellent - Very consistent
Variance 5-10%:  Good - Acceptable variance
Variance 10-20%: Acceptable - Some variability
Variance > 20%:  Poor - High variability (investigate)
```

## Regression Detection

### Regression Thresholds

Default thresholds for regression detection:

```javascript
{
  latency: 1.15,        // 15% increase = regression
  memory: 1.25,         // 25% increase = regression
  throughput: 0.85,     // 15% decrease = regression
  variance: 0.05        // 5% max variance
}
```

### Regression Severity

- **Critical**: > 30% deviation from baseline
- **Warning**: 15-30% deviation from baseline
- **None**: < 15% deviation

### Handling Regressions

1. **Verify Results**: Run benchmark again to confirm
2. **Analyze Changes**: Review recent code changes
3. **Update Baseline**: If expected, update baseline (see below)
4. **Investigate Root Cause**: Profile to identify bottleneck

```bash
# Re-run with detailed profiling
node --prof benchmarks/runner.mjs
node --prof-process isolate-*.log > profile.txt
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Performance Benchmarks

on:
  pull_request:
  push:
    branches: [main]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Node
        uses: actions/setup-node@v3
        with:
          node-version: 20
          cache: 'pnpm'

      - name: Install dependencies
        run: pnpm install

      - name: Run benchmarks
        run: |
          timeout 300s node packages/daemon/benchmarks/runner.mjs

      - name: Comment PR with results
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const reports = fs.readdirSync('packages/daemon/benchmarks/')
              .filter(f => f.startsWith('benchmarks-'))
              .sort()
              .reverse();
            const report = JSON.parse(fs.readFileSync(`packages/daemon/benchmarks/${reports[0]}`));
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: `## Performance Benchmarks\n\n${JSON.stringify(report.summary, null, 2)}`
            });

      - name: Fail if regressions
        if: failure()
        run: exit 1
```

### GitLab CI Example

```yaml
benchmark:
  stage: test
  image: node:20
  script:
    - pnpm install
    - timeout 300s node packages/daemon/benchmarks/runner.mjs
  artifacts:
    paths:
      - packages/daemon/benchmarks/benchmarks-*.json
    reports:
      performance: packages/daemon/benchmarks/benchmarks-*.json
  allow_failure: false
```

## Baseline Management

### Load Baseline

```javascript
import { loadBaseline } from './suite.mjs';

const baseline = loadBaseline();
console.log(baseline['operation-scheduling-latency']);
```

### Save New Baseline

```javascript
import { saveBaseline } from './suite.mjs';

const newBaseline = {
  'operation-scheduling-latency': {
    name: 'operation-scheduling-latency',
    type: 'latency',
    unit: 'ms',
    value: 0.15
  }
};

saveBaseline(newBaseline);
```

### Update Baseline After Improvement

```bash
# Run benchmarks and capture results
node benchmarks/runner.mjs --save-baseline

# Or manually
node -e "
import { loadBaseline, saveBaseline } from './suite.mjs';
import { benchmarkOperationScheduling } from './01-operation-scheduling.bench.mjs';

const result = await benchmarkOperationScheduling();
const baseline = loadBaseline();
baseline['operation-scheduling-latency'] = result;
saveBaseline(baseline);
console.log('Baseline updated');
"
```

## Performance Targets

### Operation Scheduling

```
Mean Latency:    < 0.3ms
P99 Latency:     < 0.5ms
Throughput:      > 8000 ops/sec
Queue Impact:    < 15% increase per 10x queue size
```

### Concurrent Execution

```
Mean Latency:    < 2ms
P95 Latency:     < 5ms
P99 Latency:     < 10ms
Throughput:      > 5000 ops/sec
```

### Memory Usage

```
Per-Operation:   < 5000 bytes
Peak (1000 ops): < 6MB
Stability:       < 20% variance
GC Impact:       < 5% overhead
```

### Raft Replication

```
Leader Latency:  < 15ms p95
Commit Latency:  < 20ms p95
Throughput:      > 1000 ops/sec
Quorum Time:     < 10ms p95
```

### YAWL Workflows

```
Sequential:      < 150ms per workflow
Parallel:        < 120ms per workflow
Conditional:     < 130ms per workflow
Mixed Throughput: > 8 workflows/sec
```

## Troubleshooting

### High Variance in Results

1. Check system load: `top`, `vmstat`
2. Disable CPU frequency scaling
3. Close unnecessary applications
4. Run benchmarks in isolation
5. Increase `runs` parameter for more samples

### Memory Benchmarks Show Spikes

1. Enable explicit GC: `node --expose-gc benchmarks/runner.mjs`
2. Run memory benchmarks separately
3. Check for memory leaks with heap profiling
4. Review garbage collection logs

### Timeout Errors

1. Increase timeout for slow systems
2. Reduce benchmark scope: `--benchmark=specific-test`
3. Check for blocking operations
4. Profile with `--prof` flag

## References

- [Benchmarking Best Practices](https://nodejs.org/en/docs/guides/benchmarking/)
- [V8 Profiling Guide](https://nodejs.org/en/docs/guides/simple-profiling/)
- [Performance API](https://nodejs.org/api/perf_hooks.html)

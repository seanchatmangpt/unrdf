# Benchmarking Guide

> **Comprehensive Guide for Running and Interpreting Daemon Performance Benchmarks**

## Overview

The @unrdf/daemon benchmarking suite provides systematic performance measurement, regression detection, and trend analysis. This guide covers all available benchmarks, execution methods, result interpretation, and custom benchmark creation.

## Table of Contents

1. [Available Benchmarks](#available-benchmarks)
2. [Running Benchmarks](#running-benchmarks)
3. [Interpreting Results](#interpreting-results)
4. [Performance Targets](#performance-targets)
5. [Custom Benchmark Creation](#custom-benchmark-creation)
6. [CI/CD Integration](#cicd-integration)
7. [Troubleshooting](#troubleshooting)

---

## Available Benchmarks

### Benchmark Suite Overview

| Suite | File | Focus Area | Key Metrics |
|-------|------|------------|-------------|
| Operation Scheduling | `01-operation-scheduling.bench.mjs` | Scheduling latency | Latency, throughput, queue impact |
| Concurrent Throughput | `02-concurrent-throughput.bench.mjs` | Execution capacity | Ops/sec, percentiles, concurrency scaling |
| Memory Load | `03-memory-load.bench.mjs` | Memory consumption | Bytes/op, peak memory, stability |
| Raft Replication | `04-raft-replication.bench.mjs` | Distributed consensus | Replication latency, commit time |
| YAWL Execution | `05-yawl-execution.bench.mjs` | Workflow patterns | Sequential, parallel, conditional |

### 1. Operation Scheduling Latency

**File**: `benchmarks/01-operation-scheduling.bench.mjs`

Measures the time required to schedule operations into the daemon queue.

**Available Benchmarks**:

```javascript
// Latency per operation
import { benchmarkOperationScheduling } from './01-operation-scheduling.bench.mjs';
const result = await benchmarkOperationScheduling({
  runs: 10,
  operationsPerRun: 100
});
// Returns: { name, type, unit, value, min, max, stdDev, variance }

// Scheduling throughput
import { benchmarkSchedulingThroughput } from './01-operation-scheduling.bench.mjs';
const throughput = await benchmarkSchedulingThroughput({
  concurrentOps: 1000,
  runs: 5
});
// Returns: { name, type, unit, value (ops/sec), variance }

// Queue impact analysis
import { benchmarkQueueImpact } from './01-operation-scheduling.bench.mjs';
const queueImpact = await benchmarkQueueImpact({
  runs: 5
});
// Returns: { name, queueSizeImpact: [{queueSize, mean, variance}] }
```

**Expected Results**:
- Mean latency: <0.3ms
- Throughput: >8000 ops/sec
- Queue impact: <15% increase per 10x queue size

### 2. Concurrent Execution Throughput

**File**: `benchmarks/02-concurrent-throughput.bench.mjs`

Measures operations executed per second under concurrent load.

**Available Benchmarks**:

```javascript
// Basic throughput
import { benchmarkConcurrentThroughput } from './02-concurrent-throughput.bench.mjs';
const throughput = await benchmarkConcurrentThroughput({
  operationCount: 100,
  runs: 5,
  opDuration: 1  // ms per operation
});

// Latency percentiles
import { benchmarkExecutionLatencyPercentiles } from './02-concurrent-throughput.bench.mjs';
const percentiles = await benchmarkExecutionLatencyPercentiles({
  operationCount: 50,
  runs: 5
});
// Returns: { p50, p95, p99, min, max, mean }

// Concurrency scaling
import { benchmarkConcurrencyImpact } from './02-concurrent-throughput.bench.mjs';
const scaling = await benchmarkConcurrencyImpact({
  runs: 3
});
// Returns: { concurrencyImpact: [{concurrency, mean, variance}] }
```

**Expected Results**:
- Throughput: >5000 ops/sec
- P50 latency: <2ms
- P95 latency: <5ms
- P99 latency: <10ms

### 3. Memory Usage Under Load

**File**: `benchmarks/03-memory-load.bench.mjs`

Measures heap and RSS memory consumption with varying queue sizes.

**Available Benchmarks**:

```javascript
// Memory with growing queue
import { benchmarkMemoryWithLoad } from './03-memory-load.bench.mjs';
const memoryLoad = await benchmarkMemoryWithLoad({
  maxOperations: 10000,
  steps: 5
});
// Returns: { measurements: [{operationCount, heapDelta, bytesPerOperation}] }

// Execution memory consumption
import { benchmarkExecutionMemory } from './03-memory-load.bench.mjs';
const execMemory = await benchmarkExecutionMemory({
  operationCount: 1000,
  runs: 3
});

// Memory stability under sustained load
import { benchmarkMemoryStability } from './03-memory-load.bench.mjs';
const stability = await benchmarkMemoryStability({
  duration: 5000,
  opsPerSecond: 100
});
// Returns: { heap: {mean, stdDev, variance}, rss: {...} }
```

**Expected Results**:
- Bytes per operation: <5000
- Peak memory (10K ops): <500MB
- Stability variance: <20%

### 4. Raft Replication

**File**: `benchmarks/04-raft-replication.bench.mjs`

Simulates Raft consensus replication across cluster nodes.

**Available Benchmarks**:

```javascript
// Leader replication latency
import { benchmarkLeaderReplication } from './04-raft-replication.bench.mjs';
const replication = await benchmarkLeaderReplication({
  nodeCount: 3,
  operationCount: 100,
  runs: 5
});

// Consensus commit latency
import { benchmarkConsensusCommit } from './04-raft-replication.bench.mjs';
const commit = await benchmarkConsensusCommit({
  quorumSize: 3,
  operationCount: 100,
  runs: 5
});

// Replication throughput
import { benchmarkReplicationThroughput } from './04-raft-replication.bench.mjs';
const replThroughput = await benchmarkReplicationThroughput({
  nodeCount: 3,
  duration: 5000,
  runs: 3
});
```

**Expected Results**:
- Leader replication: <15ms P95
- Consensus commit: <20ms P95
- Replication throughput: >1000 ops/sec

### 5. YAWL Workflow Execution

**File**: `benchmarks/05-yawl-execution.bench.mjs`

Measures workflow execution for different YAWL patterns.

**Available Benchmarks**:

```javascript
// Sequential workflow
import { benchmarkSequentialWorkflow } from './05-yawl-execution.bench.mjs';
const sequential = await benchmarkSequentialWorkflow({
  stepsPerWorkflow: 5,
  workflowCount: 20,
  runs: 5
});

// Parallel workflow
import { benchmarkParallelWorkflow } from './05-yawl-execution.bench.mjs';
const parallel = await benchmarkParallelWorkflow({
  parallelTasks: 5,
  workflowCount: 20,
  runs: 5
});

// Conditional workflow
import { benchmarkConditionalWorkflow } from './05-yawl-execution.bench.mjs';
const conditional = await benchmarkConditionalWorkflow({
  branchingFactor: 3,
  workflowCount: 20,
  runs: 5
});

// Mixed workflow throughput
import { benchmarkMixedWorkflow } from './05-yawl-execution.bench.mjs';
const mixed = await benchmarkMixedWorkflow({
  workflowCount: 30,
  runs: 3
});
```

**Expected Results**:
- Sequential: <150ms per workflow
- Parallel: <120ms per workflow
- Conditional: <130ms per workflow
- Mixed throughput: >8 workflows/sec

---

## Running Benchmarks

### Command-Line Interface

```bash
# Run complete benchmark suite
node packages/daemon/benchmarks/runner.mjs

# Run with verbose output
node packages/daemon/benchmarks/runner.mjs --verbose

# Run specific benchmark
node packages/daemon/benchmarks/runner.mjs --benchmark=operation-scheduling

# Run specific suite
node packages/daemon/benchmarks/runner.mjs --suite=memory

# Custom run count
node packages/daemon/benchmarks/runner.mjs --runs=20

# Save results as new baseline
node packages/daemon/benchmarks/runner.mjs --save-baseline
```

### CLI Options Reference

| Option | Description | Default |
|--------|-------------|---------|
| `--verbose` | Show detailed output | false |
| `--benchmark=NAME` | Run specific benchmark | all |
| `--suite=NAME` | Run specific suite | all |
| `--runs=N` | Number of benchmark runs | varies |
| `--save-baseline` | Save results as baseline | false |

### Programmatic Execution

```javascript
import {
  generateReport,
  loadBaseline,
  saveReport,
  formatReportForConsole
} from './benchmarks/suite.mjs';

import { benchmarkOperationScheduling } from './benchmarks/01-operation-scheduling.bench.mjs';
import { benchmarkConcurrentThroughput } from './benchmarks/02-concurrent-throughput.bench.mjs';

async function runCustomBenchmarks() {
  const results = {};

  // Run specific benchmarks
  results['scheduling'] = await benchmarkOperationScheduling({ runs: 10 });
  results['throughput'] = await benchmarkConcurrentThroughput({ runs: 5 });

  // Load baseline for comparison
  const baseline = loadBaseline();

  // Generate report
  const report = generateReport(results, baseline);

  // Display results
  console.log(formatReportForConsole(report));

  // Save report
  saveReport(report);

  return report;
}

runCustomBenchmarks();
```

### Timeout Management

```bash
# Run with timeout (recommended: 5 minutes for full suite)
timeout 300s node packages/daemon/benchmarks/runner.mjs

# Run specific benchmark with shorter timeout
timeout 60s node packages/daemon/benchmarks/runner.mjs --benchmark=operation-scheduling
```

---

## Interpreting Results

### Report Structure

```json
{
  "timestamp": "2026-01-18T12:30:45.123Z",
  "summary": {
    "totalBenchmarks": 16,
    "totalRegressions": 0,
    "criticalRegressions": 0,
    "warningRegressions": 0,
    "passRate": "100.00"
  },
  "regressions": [],
  "results": {
    "operation-scheduling-latency": {
      "benchmark": {
        "name": "operation-scheduling-latency",
        "type": "latency",
        "unit": "ms",
        "value": 0.15,
        "min": 0.01,
        "max": 0.45,
        "stdDev": 0.08,
        "variance": 12.5,
        "sampleCount": 1000
      },
      "baseline": { "value": 0.15 },
      "regression": {
        "isRegression": false,
        "ratio": "1.0000",
        "percentChange": "0.00%",
        "severity": "none"
      }
    }
  }
}
```

### Understanding Metrics

#### Latency Metrics

| Metric | Description | Good | Warning | Bad |
|--------|-------------|------|---------|-----|
| Mean | Average latency | <target | target-2x | >2x target |
| P50 | Median (50th percentile) | <target | target-1.5x | >1.5x target |
| P95 | 95th percentile | <2x target | 2x-3x target | >3x target |
| P99 | 99th percentile | <3x target | 3x-5x target | >5x target |
| Variance (CoV) | Stability measure | <10% | 10-20% | >20% |

#### Throughput Metrics

| Metric | Description | Good | Warning | Bad |
|--------|-------------|------|---------|-----|
| Ops/sec | Operations per second | >target | 0.8-1x target | <0.8x target |
| Variance | Consistency | <10% | 10-20% | >20% |

#### Memory Metrics

| Metric | Description | Good | Warning | Bad |
|--------|-------------|------|---------|-----|
| Bytes/op | Memory per operation | <target | target-1.5x | >1.5x target |
| Peak | Maximum memory | <limit | limit-1.3x | >1.3x limit |
| Stability | GC impact variance | <15% | 15-25% | >25% |

### Variance Interpretation

```
Variance < 5%:   Excellent - Very consistent performance
Variance 5-10%:  Good - Normal variance for most systems
Variance 10-20%: Acceptable - Some variability present
Variance > 20%:  Poor - High variability (investigate)
```

### Regression Detection

**Thresholds**:

```javascript
const REGRESSION_THRESHOLDS = {
  latency: 1.15,      // 15% increase = regression
  memory: 1.25,       // 25% increase = regression
  throughput: 0.85,   // 15% decrease = regression
  variance: 0.05      // 5% max acceptable variance
};
```

**Severity Levels**:

| Severity | Deviation | Action |
|----------|-----------|--------|
| None | <15% | No action needed |
| Warning | 15-30% | Investigate cause |
| Critical | >30% | Block release, fix immediately |

---

## Performance Targets

### Operation Scheduling

| Metric | Target | Acceptable | Unacceptable |
|--------|--------|------------|--------------|
| Mean Latency | <0.15ms | <0.3ms | >0.5ms |
| P99 Latency | <0.5ms | <1ms | >2ms |
| Throughput | >10K ops/sec | >8K ops/sec | <5K ops/sec |
| Queue Impact | <10%/10x | <15%/10x | >25%/10x |

### Concurrent Execution

| Metric | Target | Acceptable | Unacceptable |
|--------|--------|------------|--------------|
| Throughput | >5K ops/sec | >3K ops/sec | <1K ops/sec |
| P50 Latency | <2ms | <5ms | >10ms |
| P95 Latency | <5ms | <10ms | >25ms |
| P99 Latency | <10ms | <25ms | >50ms |

### Memory Usage

| Metric | Target | Acceptable | Unacceptable |
|--------|--------|------------|--------------|
| Per Operation | <3KB | <5KB | >10KB |
| Peak (10K ops) | <300MB | <500MB | >800MB |
| Stability | <10% variance | <20% variance | >30% variance |
| GC Pause | <5ms | <10ms | >20ms |

### Raft Replication

| Metric | Target | Acceptable | Unacceptable |
|--------|--------|------------|--------------|
| Leader Latency | <10ms | <15ms | >25ms |
| Commit Latency | <15ms | <20ms | >30ms |
| Throughput | >1.5K ops/sec | >1K ops/sec | <500 ops/sec |

### YAWL Workflows

| Metric | Target | Acceptable | Unacceptable |
|--------|--------|------------|--------------|
| Sequential | <100ms | <150ms | >200ms |
| Parallel | <70ms | <120ms | >180ms |
| Conditional | <80ms | <130ms | >200ms |
| Mixed Throughput | >10 wf/sec | >8 wf/sec | <5 wf/sec |

---

## Custom Benchmark Creation

### Benchmark Template

```javascript
/**
 * @file Custom Benchmark Template
 * @module @unrdf/daemon/benchmarks/custom
 * @description Template for creating custom daemon benchmarks
 */

import { Daemon } from '../src/daemon.mjs';
import { analyzeVariance, storeBenchmarkResult } from './suite.mjs';
import { randomUUID } from 'crypto';

/**
 * Benchmark: [Name]
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=10] - Number of runs
 * @param {number} [options.operationsPerRun=100] - Operations per run
 * @returns {Promise<Object>} Benchmark result
 */
export async function benchmarkCustom(options = {}) {
  const { runs = 10, operationsPerRun = 100 } = options;
  const measurements = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    // Create fresh daemon for each run
    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `benchmark-run-${runIdx}`,
    });
    await daemon.start();

    // === YOUR BENCHMARK LOGIC HERE ===
    const startTime = performance.now();

    // Schedule and execute operations
    for (let i = 0; i < operationsPerRun; i++) {
      daemon.schedule({
        id: `op-${runIdx}-${i}`,
        handler: async () => {
          // Your operation logic
          return { success: true };
        },
      });
    }

    // Execute all operations
    await Promise.all(
      Array.from({ length: operationsPerRun }, (_, i) =>
        daemon.execute(`op-${runIdx}-${i}`).catch(() => {})
      )
    );

    const endTime = performance.now();
    // === END BENCHMARK LOGIC ===

    // Record measurement
    measurements.push(endTime - startTime);

    await daemon.stop();
  }

  // Analyze results
  const variance = analyzeVariance(measurements);

  // Return formatted result
  return storeBenchmarkResult({
    name: 'custom-benchmark',
    type: 'latency',  // 'latency', 'throughput', or 'memory'
    unit: 'ms',       // 'ms', 'ops/sec', 'bytes', etc.
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    sampleCount: measurements.length,
    runs,
    operationsPerRun,
  });
}
```

### Best Practices for Custom Benchmarks

```javascript
// 1. Isolate each run
for (let run = 0; run < runs; run++) {
  const daemon = new Daemon({ daemonId: randomUUID() });
  await daemon.start();
  // ... benchmark logic ...
  await daemon.stop();
}

// 2. Use high-resolution timing
const startTime = performance.now();
// ... operation ...
const duration = performance.now() - startTime;

// 3. Warm up before measuring (optional)
for (let i = 0; i < 100; i++) {
  await warmupOperation();
}

// 4. Force GC between runs (if available)
if (global.gc) {
  global.gc();
}

// 5. Collect multiple samples
const samples = [];
for (let i = 0; i < sampleCount; i++) {
  samples.push(await measureOnce());
}

// 6. Use statistical analysis
const stats = analyzeVariance(samples);

// 7. Document your benchmark
/**
 * @param {Object} options - Options
 * @param {number} options.runs - Run count
 * @returns {Object} Result with value, variance, etc.
 */
```

### Registering Custom Benchmarks

```javascript
// In runner.mjs, add your benchmark
import { benchmarkCustom } from './custom-benchmark.bench.mjs';

const benchmarkNames = [
  // ... existing benchmarks ...
  'custom-benchmark',
];

async function runBenchmarkSuite(name, options) {
  switch (name) {
    // ... existing cases ...
    case 'custom-benchmark':
      return await benchmarkCustom({ runs: options.runs || 10 });
    default:
      throw new Error(`Unknown benchmark: ${name}`);
  }
}
```

---

## CI/CD Integration

### GitHub Actions

```yaml
name: Performance Benchmarks

on:
  pull_request:
  push:
    branches: [main]
  schedule:
    - cron: '0 2 * * *'  # Nightly at 2 AM

jobs:
  benchmark:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: 'pnpm'

      - name: Install pnpm
        uses: pnpm/action-setup@v2
        with:
          version: 8

      - name: Install dependencies
        run: pnpm install

      - name: Run benchmarks
        run: |
          timeout 300s node packages/daemon/benchmarks/runner.mjs --verbose

      - name: Upload benchmark results
        uses: actions/upload-artifact@v4
        with:
          name: benchmark-results
          path: packages/daemon/benchmarks/benchmarks-*.json

      - name: Comment PR with results
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v7
        with:
          script: |
            const fs = require('fs');
            const files = fs.readdirSync('packages/daemon/benchmarks/')
              .filter(f => f.startsWith('benchmarks-'))
              .sort()
              .reverse();

            if (files.length === 0) return;

            const report = JSON.parse(
              fs.readFileSync(`packages/daemon/benchmarks/${files[0]}`)
            );

            const body = `## Performance Benchmark Results

            | Metric | Value |
            |--------|-------|
            | Total Benchmarks | ${report.summary.totalBenchmarks} |
            | Pass Rate | ${report.summary.passRate}% |
            | Regressions | ${report.summary.totalRegressions} |
            | Critical | ${report.summary.criticalRegressions} |
            | Warnings | ${report.summary.warningRegressions} |

            ${report.summary.totalRegressions > 0 ? '**Regressions detected!**' : 'All benchmarks passed.'}
            `;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body
            });

      - name: Fail on regressions
        run: |
          REPORT=$(ls -t packages/daemon/benchmarks/benchmarks-*.json | head -1)
          REGRESSIONS=$(jq '.summary.totalRegressions' $REPORT)
          if [ "$REGRESSIONS" -gt "0" ]; then
            echo "Performance regressions detected!"
            exit 1
          fi
```

### GitLab CI

```yaml
benchmark:
  stage: test
  image: node:20

  before_script:
    - npm install -g pnpm
    - pnpm install

  script:
    - timeout 300s node packages/daemon/benchmarks/runner.mjs --verbose

  artifacts:
    paths:
      - packages/daemon/benchmarks/benchmarks-*.json
    reports:
      performance: packages/daemon/benchmarks/benchmarks-*.json
    expire_in: 30 days

  rules:
    - if: '$CI_PIPELINE_SOURCE == "merge_request_event"'
    - if: '$CI_COMMIT_BRANCH == "main"'
    - if: '$CI_PIPELINE_SOURCE == "schedule"'

  allow_failure: false
```

### Pre-commit Hook

```bash
#!/bin/bash
# .husky/pre-push

echo "Running performance benchmarks..."

# Quick benchmark check
timeout 60s node packages/daemon/benchmarks/runner.mjs \
  --benchmark=operation-scheduling \
  --runs=3

if [ $? -ne 0 ]; then
  echo "Performance regression detected! Push aborted."
  exit 1
fi

echo "Benchmarks passed."
```

---

## Troubleshooting

### High Variance in Results

**Symptoms**: CoV > 20%, inconsistent measurements

**Diagnosis**:
```bash
# Check system load
top
vmstat 1 5

# Check for background processes
ps aux | grep node
```

**Solutions**:
1. Close unnecessary applications
2. Disable CPU frequency scaling: `sudo cpupower frequency-set -g performance`
3. Increase run count: `--runs=20`
4. Run in isolated environment (container, VM)

### Memory Benchmarks Show Spikes

**Symptoms**: Large memory variance, unexpected peaks

**Diagnosis**:
```bash
# Enable GC tracking
node --expose-gc --trace-gc packages/daemon/benchmarks/runner.mjs
```

**Solutions**:
1. Force GC between runs: add `global.gc()` calls
2. Run memory benchmarks separately
3. Increase heap size: `node --max-old-space-size=4096`
4. Check for memory leaks with heap snapshots

### Timeout Errors

**Symptoms**: Benchmarks don't complete, process killed

**Diagnosis**:
```bash
# Check actual duration
time node packages/daemon/benchmarks/runner.mjs --benchmark=NAME
```

**Solutions**:
1. Increase timeout: `timeout 600s`
2. Run specific benchmark: `--benchmark=operation-scheduling`
3. Reduce run count: `--runs=3`
4. Profile with `--prof` flag to find bottlenecks

### Regression False Positives

**Symptoms**: Regressions detected on unchanged code

**Diagnosis**:
```bash
# Compare baselines
diff packages/daemon/benchmarks/baselines/baseline.json \
     packages/daemon/benchmarks/benchmarks-latest.json
```

**Solutions**:
1. Update baseline after legitimate changes: `--save-baseline`
2. Increase variance threshold temporarily
3. Run multiple times to confirm
4. Check for environmental differences

---

## References

- [UPTIME_SIMULATION.md](./UPTIME_SIMULATION.md) - Uptime testing
- [CHAOS_ENGINEERING.md](./CHAOS_ENGINEERING.md) - Chaos experiments
- [performance-tuning.md](./performance-tuning.md) - Performance optimization
- [benchmarking.md](./benchmarking.md) - Original benchmarking guide

---

**Document Version**: 1.0.0
**Last Updated**: 2026-01-18
**Target**: @unrdf/daemon v6.0.0+

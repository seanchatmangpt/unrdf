# Knowledge Hooks Benchmark Implementation Roadmap

**Quick-Start Guide for Developers**

---

## Phase 1: Infrastructure Setup (Week 1)

### Day 1-2: Directory Structure & Dependencies

**Tasks:**
```bash
# Create directory structure
mkdir -p benchmarks/{fixtures,utils,baselines}
mkdir -p .github/workflows
```

**Install dependencies:**
```bash
pnpm add -D @opentelemetry/api \
            @opentelemetry/sdk-trace-base \
            @opentelemetry/sdk-trace-node \
            @opentelemetry/sdk-metrics
```

**Create baseline files:**

1. `benchmarks/fixtures/test-hooks.mjs`
```javascript
/**
 * Reusable test hooks for benchmarks
 */
export function createSimpleHook(id) {
  return {
    meta: {
      name: `test-hook-${id}`,
      description: 'Simple test hook for benchmarking',
    },
    when: {
      kind: 'sparql',
      ref: {
        uri: 'data:text/plain,SELECT * WHERE { ?s ?p ?o }',
        sha256: 'abc123...',
      },
    },
    run: async (event) => {
      return { processed: true, hookId: id };
    },
  };
}

export function createComplexHook(id) {
  return {
    meta: {
      name: `complex-hook-${id}`,
      description: 'Complex hook with all phases',
    },
    when: {
      kind: 'sparql',
      ref: {
        uri: 'data:text/plain,SELECT * WHERE { ?s ?p ?o }',
        sha256: 'abc123...',
      },
    },
    before: async (event) => {
      return { beforeData: Date.now() };
    },
    run: async (event) => {
      // Simulate work
      await new Promise(resolve => setTimeout(resolve, 10));
      return { processed: true, hookId: id };
    },
    after: async (event) => {
      return { afterData: Date.now() };
    },
  };
}

export function createFailingHook(shouldFail) {
  return {
    meta: { name: 'failing-hook' },
    when: {
      kind: 'sparql',
      ref: {
        uri: 'data:text/plain,SELECT * WHERE { ?s ?p ?o }',
        sha256: 'abc123...',
      },
    },
    run: async (event) => {
      if (shouldFail) {
        throw new Error('Intentional failure for testing');
      }
      return { processed: true };
    },
  };
}
```

2. `benchmarks/fixtures/test-events.mjs`
```javascript
/**
 * Reusable test events for benchmarks
 */
import { createStore } from '@unrdf/oxigraph';
import { Parser } from '@unrdf/n3-iso';

export function createSimpleEvent() {
  const store = createStore();
  return {
    name: 'test-event',
    payload: {
      delta: { additions: [], removals: [] },
      storeSize: 0,
      additionsCount: 0,
      removalsCount: 0,
    },
    context: {
      graph: store,
      env: {
        transactionMode: 'pre',
        strictMode: false,
      },
    },
  };
}

export async function createEventWithStore(tripleCount = 1000) {
  const store = createStore();
  const parser = new Parser();

  // Generate test triples
  const turtle = Array.from({ length: tripleCount }, (_, i) =>
    `<http://example.org/subject${i}> <http://example.org/predicate> "Object ${i}" .`
  ).join('\n');

  const quads = parser.parse(turtle);
  for (const quad of quads) {
    store.addQuad(quad);
  }

  return {
    name: 'test-event-with-store',
    payload: {
      delta: { additions: [], removals: [] },
      storeSize: store.size,
      additionsCount: 0,
      removalsCount: 0,
    },
    context: {
      graph: store,
      env: {
        transactionMode: 'pre',
        strictMode: false,
      },
    },
  };
}
```

3. `benchmarks/fixtures/test-stores.mjs`
```javascript
/**
 * Reusable test stores for benchmarks
 */
import { createStore } from '@unrdf/oxigraph';
import { Parser } from '@unrdf/n3-iso';

export function createEmptyStore() {
  return createStore();
}

export async function createSmallStore() {
  return createStoreWithTriples(100);
}

export async function createMediumStore() {
  return createStoreWithTriples(1000);
}

export async function createLargeStore() {
  return createStoreWithTriples(10000);
}

async function createStoreWithTriples(count) {
  const store = createStore();
  const parser = new Parser();

  const turtle = Array.from({ length: count }, (_, i) =>
    `<http://example.org/s${i}> <http://example.org/p> "Object ${i}" .`
  ).join('\n');

  const quads = parser.parse(turtle);
  for (const quad of quads) {
    store.addQuad(quad);
  }

  return store;
}
```

### Day 3-4: Utility Functions

4. `benchmarks/utils/otel-collector.mjs`
```javascript
/**
 * OTEL span collector for benchmarks
 */
import { InMemorySpanExporter } from '@opentelemetry/sdk-trace-base';
import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node';
import { SimpleSpanProcessor } from '@opentelemetry/sdk-trace-base';

export function createOtelCollector() {
  const exporter = new InMemorySpanExporter();
  const provider = new NodeTracerProvider();
  provider.addSpanProcessor(new SimpleSpanProcessor(exporter));
  provider.register();

  return {
    exporter,
    provider,

    getSpans() {
      return exporter.getFinishedSpans();
    },

    reset() {
      exporter.reset();
    },

    shutdown() {
      return provider.shutdown();
    },

    filterSpans(name) {
      return this.getSpans().filter(s => s.name === name);
    },

    getSpanAttributes(spanName) {
      return this.filterSpans(spanName).map(s => s.attributes);
    },
  };
}
```

5. `benchmarks/utils/percentile-calculator.mjs`
```javascript
/**
 * Percentile calculation utilities
 */

export function calculatePercentile(values, percentile) {
  if (!Array.isArray(values) || values.length === 0) {
    return 0;
  }

  const sorted = [...values].sort((a, b) => a - b);
  const index = Math.ceil(sorted.length * percentile) - 1;

  return sorted[Math.max(0, index)];
}

export function calculatePercentiles(values) {
  return {
    p50: calculatePercentile(values, 0.50),
    p95: calculatePercentile(values, 0.95),
    p99: calculatePercentile(values, 0.99),
    max: values.length > 0 ? Math.max(...values) : 0,
    min: values.length > 0 ? Math.min(...values) : 0,
    avg: values.length > 0 ? values.reduce((a, b) => a + b, 0) / values.length : 0,
  };
}

export function calculateThroughput(totalOps, durationMs) {
  if (durationMs === 0) return 0;
  return (totalOps / durationMs) * 1000; // ops per second
}
```

6. `benchmarks/utils/memory-profiler.mjs`
```javascript
/**
 * Memory profiling utilities
 */

export function getMemorySnapshot() {
  const usage = process.memoryUsage();
  return {
    timestamp: Date.now(),
    rss: usage.rss,
    heapUsed: usage.heapUsed,
    heapTotal: usage.heapTotal,
    external: usage.external,
  };
}

export function forceGC() {
  if (global.gc) {
    global.gc();
  } else {
    console.warn('GC not exposed. Run with --expose-gc flag');
  }
}

export function calculateMemoryDelta(before, after) {
  return {
    rss: after.rss - before.rss,
    heapUsed: after.heapUsed - before.heapUsed,
    heapTotal: after.heapTotal - before.heapTotal,
    external: after.external - before.external,
  };
}

export function detectMemoryLeak(baseline, cleanup, tolerance = 0.05) {
  const heapGrowth = cleanup.heapUsed - baseline.heapUsed;
  const heapGrowthPercent = heapGrowth / baseline.heapUsed;

  const leaked = heapGrowthPercent > tolerance;

  return {
    leaked,
    heapGrowth,
    heapGrowthPercent,
    tolerance,
    message: leaked
      ? `Memory leak detected: ${(heapGrowthPercent * 100).toFixed(2)}% growth`
      : 'No memory leak detected',
  };
}

export function formatBytes(bytes) {
  if (bytes < 1024) return `${bytes}B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(2)}KB`;
  return `${(bytes / 1024 / 1024).toFixed(2)}MB`;
}
```

7. `benchmarks/utils/metrics-aggregator.mjs`
```javascript
/**
 * Metrics aggregation from OTEL spans
 */
import { calculatePercentiles, calculateThroughput } from './percentile-calculator.mjs';

export function aggregateSpanMetrics(spans, spanName) {
  const filteredSpans = spans.filter(s => s.name === spanName);

  if (filteredSpans.length === 0) {
    return {
      count: 0,
      durations: {},
      errors: 0,
    };
  }

  const durations = filteredSpans.map(s =>
    s.attributes['benchmark.duration_ms'] || 0
  );

  const errors = filteredSpans.filter(s =>
    s.status?.code === 2 // ERROR
  ).length;

  return {
    count: filteredSpans.length,
    durations: calculatePercentiles(durations),
    errorRate: errors / filteredSpans.length,
    errors,
  };
}

export function aggregateMemoryMetrics(spans, spanName) {
  const filteredSpans = spans.filter(s => s.name === spanName);

  if (filteredSpans.length === 0) {
    return {
      count: 0,
      totalDelta: 0,
      averageDelta: 0,
    };
  }

  const deltas = filteredSpans.map(s =>
    s.attributes['benchmark.memory_delta_bytes'] || 0
  );

  const totalDelta = deltas.reduce((a, b) => a + b, 0);
  const averageDelta = totalDelta / deltas.length;

  return {
    count: filteredSpans.length,
    totalDelta,
    averageDelta,
    deltas: calculatePercentiles(deltas),
  };
}

export function calculateBenchmarkScore(metrics, criteria) {
  let passed = 0;
  let total = 0;

  for (const [key, target] of Object.entries(criteria)) {
    total++;

    const actual = getNestedValue(metrics, key);
    if (actual !== undefined && actual <= target) {
      passed++;
    }
  }

  return {
    passed,
    total,
    score: total > 0 ? (passed / total) * 100 : 0,
  };
}

function getNestedValue(obj, path) {
  return path.split('.').reduce((acc, part) => acc?.[part], obj);
}
```

### Day 5: Benchmark Runner

8. `benchmarks/runner.mjs`
```javascript
/**
 * Main benchmark runner
 */
import { createOtelCollector } from './utils/otel-collector.mjs';
import { readFileSync, writeFileSync, readdirSync } from 'node:fs';
import { join } from 'node:path';

export async function runBenchmarks(options = {}) {
  const {
    mode = 'local', // 'local' | 'ci'
    output = 'console', // 'console' | 'json'
    suite = null, // Run specific suite or all
    compare = null, // Baseline file to compare against
    saveBaseline = null, // File to save baseline to
  } = options;

  console.log('🚀 Knowledge Hooks Benchmark Suite\n');
  console.log(`   Mode: ${mode}`);
  console.log(`   Output: ${output}`);
  if (suite) console.log(`   Suite: ${suite}`);
  console.log('');

  // Discover benchmark files
  const benchmarkFiles = readdirSync(join(process.cwd(), 'benchmarks'))
    .filter(f => f.endsWith('.bench.mjs'))
    .filter(f => !suite || f.includes(suite))
    .sort();

  if (benchmarkFiles.length === 0) {
    console.error('❌ No benchmark files found');
    process.exit(1);
  }

  console.log(`   Running ${benchmarkFiles.length} benchmarks...\n`);

  const results = {
    version: '1.0.0',
    timestamp: new Date().toISOString(),
    environment: {
      node: process.version,
      platform: process.platform,
      cpus: require('os').cpus().length,
      memory: `${Math.round(require('os').totalmem() / 1024 / 1024 / 1024)}GB`,
    },
    benchmarks: [],
  };

  // Run benchmarks sequentially
  for (let i = 0; i < benchmarkFiles.length; i++) {
    const file = benchmarkFiles[i];
    const suiteName = file.replace('.bench.mjs', '');

    console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━`);
    console.log(`📊 Benchmark ${i + 1}/${benchmarkFiles.length}: ${suiteName}`);
    console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`);

    try {
      const benchmark = await import(`./${file}`);
      const result = await benchmark.run();

      results.benchmarks.push(result);

      // Print result
      if (output === 'console') {
        printBenchmarkResult(result);
      }

      // Cool-down period between benchmarks
      if (i < benchmarkFiles.length - 1) {
        console.log('\n   (1s cool-down...)\n');
        await new Promise(resolve => setTimeout(resolve, 1000));
      }
    } catch (error) {
      console.error(`❌ Benchmark ${suiteName} failed:`, error.message);
      results.benchmarks.push({
        benchmark: suiteName,
        error: error.message,
        passed: false,
      });
    }
  }

  // Calculate summary
  const summary = calculateSummary(results);
  results.summary = summary;

  // Print summary
  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('🎯 Overall Results');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
  console.log(`Total Duration: ${(summary.totalDuration / 1000).toFixed(1)}s`);
  console.log(`Benchmarks: ${summary.passed}/${summary.total} passed ${summary.passed === summary.total ? '✅' : '❌'}`);
  console.log(`Overall Score: ${summary.score.toFixed(1)}%\n`);

  // Compare against baseline if requested
  if (compare) {
    const comparison = compareWithBaseline(results, compare);
    printComparison(comparison);
  }

  // Save baseline if requested
  if (saveBaseline) {
    writeFileSync(saveBaseline, JSON.stringify(results, null, 2));
    console.log(`\n✅ Baseline saved to ${saveBaseline}`);
  }

  // Output JSON if requested
  if (output === 'json') {
    console.log(JSON.stringify(results, null, 2));
  }

  // Exit with appropriate code
  const exitCode = summary.passed === summary.total ? 0 : 1;
  process.exit(exitCode);
}

function printBenchmarkResult(result) {
  console.log(`Iterations: ${result.iterations || 'N/A'}`);
  console.log(`Duration: ${result.duration_ms || 'N/A'}ms\n`);

  if (result.metrics) {
    console.log('Metrics:');
    for (const [key, value] of Object.entries(result.metrics)) {
      const passed = result.validation?.details?.find(d => d.criterion.includes(key))?.passed;
      const status = passed === undefined ? '' : passed ? '✅' : '❌';
      console.log(`  ├─ ${key}: ${formatValue(value)} ${status}`);
    }
  }

  console.log(`\nStatus: ${result.validation?.passed ? '✅ PASSED' : '❌ FAILED'}`);
  if (result.validation) {
    console.log(`  (${result.validation.passed}/${result.validation.total_criteria} criteria)`);
  }
}

function formatValue(value) {
  if (typeof value === 'number') {
    if (value > 1000000) return `${(value / 1000000).toFixed(2)}MB`;
    if (value > 1000) return `${(value / 1000).toFixed(2)}KB`;
    return value.toFixed(2);
  }
  return String(value);
}

function calculateSummary(results) {
  const total = results.benchmarks.length;
  const passed = results.benchmarks.filter(b => b.validation?.passed).length;
  const totalDuration = results.benchmarks.reduce((sum, b) => sum + (b.duration_ms || 0), 0);
  const score = total > 0 ? (passed / total) * 100 : 0;

  return {
    total,
    passed,
    failed: total - passed,
    totalDuration,
    score,
  };
}

function compareWithBaseline(current, baselineFile) {
  const baseline = JSON.parse(readFileSync(baselineFile, 'utf-8'));

  // Compare critical metrics
  const comparisons = [];

  for (let i = 0; i < current.benchmarks.length; i++) {
    const currentBench = current.benchmarks[i];
    const baselineBench = baseline.benchmarks[i];

    if (!baselineBench) continue;

    comparisons.push({
      benchmark: currentBench.benchmark,
      regressions: detectRegressions(currentBench.metrics, baselineBench.metrics),
    });
  }

  return comparisons;
}

function detectRegressions(current, baseline) {
  const regressions = [];

  // Latency regression: +20%
  if (current.p95 && baseline.p95) {
    const change = (current.p95 - baseline.p95) / baseline.p95;
    if (change > 0.20) {
      regressions.push({
        metric: 'p95',
        change: `+${(change * 100).toFixed(1)}%`,
        severity: 'high',
      });
    }
  }

  // Throughput regression: -15%
  if (current.throughput && baseline.throughput) {
    const change = (current.throughput - baseline.throughput) / baseline.throughput;
    if (change < -0.15) {
      regressions.push({
        metric: 'throughput',
        change: `${(change * 100).toFixed(1)}%`,
        severity: 'high',
      });
    }
  }

  // Memory regression: +30%
  if (current.memory_per_hook && baseline.memory_per_hook) {
    const change = (current.memory_per_hook - baseline.memory_per_hook) / baseline.memory_per_hook;
    if (change > 0.30) {
      regressions.push({
        metric: 'memory_per_hook',
        change: `+${(change * 100).toFixed(1)}%`,
        severity: 'medium',
      });
    }
  }

  return regressions;
}

function printComparison(comparisons) {
  console.log('\n📊 Regression Analysis:\n');

  let hasRegressions = false;

  for (const comp of comparisons) {
    if (comp.regressions.length > 0) {
      hasRegressions = true;
      console.log(`❌ ${comp.benchmark}:`);
      for (const reg of comp.regressions) {
        console.log(`   - ${reg.metric}: ${reg.change} (${reg.severity} severity)`);
      }
    }
  }

  if (!hasRegressions) {
    console.log('✅ No regressions detected');
  }
}

// Auto-run if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const args = process.argv.slice(2);
  const options = {};

  for (let i = 0; i < args.length; i++) {
    if (args[i] === '--mode' && args[i + 1]) options.mode = args[i + 1];
    if (args[i] === '--output' && args[i + 1]) options.output = args[i + 1];
    if (args[i] === '--suite' && args[i + 1]) options.suite = args[i + 1];
    if (args[i] === '--compare' && args[i + 1]) options.compare = args[i + 1];
    if (args[i] === '--save-baseline' && args[i + 1]) options.saveBaseline = args[i + 1];
  }

  runBenchmarks(options);
}
```

---

## Phase 2: Core Benchmarks (Week 2)

### Day 1: Hook Registration Benchmark

9. `benchmarks/01-hook-registration.bench.mjs`
```javascript
/**
 * @file Hook Registration Performance Benchmark
 * @benchmark hook-registration
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import { KnowledgeHookManager } from '../src/knowledge-engine/knowledge-hook-manager.mjs';
import { createSimpleHook } from './fixtures/test-hooks.mjs';
import { createOtelCollector } from './utils/otel-collector.mjs';
import { aggregateSpanMetrics, aggregateMemoryMetrics } from './utils/metrics-aggregator.mjs';
import { forceGC, getMemorySnapshot, formatBytes } from './utils/memory-profiler.mjs';

const tracer = trace.getTracer('unrdf-benchmark');

export async function run() {
  const collector = createOtelCollector();
  const ITERATIONS = 100;
  const WARMUP = 10;

  return tracer.startActiveSpan('benchmark.hook-registration.start', async (span) => {
    try {
      // Setup
      const manager = new KnowledgeHookManager({ enableKnowledgeHooks: true });

      // Warmup
      await warmup(manager, WARMUP);

      // Baseline memory
      forceGC();
      const baselineMemory = getMemorySnapshot();

      // Benchmark
      const startTime = Date.now();

      for (let i = 0; i < ITERATIONS; i++) {
        const iterSpan = tracer.startSpan('benchmark.hook-registration.iteration', {
          attributes: { 'benchmark.iteration': i },
        });

        const iterStart = Date.now();
        const memBefore = getMemorySnapshot();

        const hook = createSimpleHook(i);
        manager.addKnowledgeHook(hook);

        const memAfter = getMemorySnapshot();
        const iterDuration = Date.now() - iterStart;

        iterSpan.setAttributes({
          'benchmark.suite': 'hook-registration',
          'benchmark.duration_ms': iterDuration,
          'benchmark.memory_delta_bytes': memAfter.heapUsed - memBefore.heapUsed,
          'benchmark.success': true,
        });

        iterSpan.setStatus({ code: SpanStatusCode.OK });
        iterSpan.end();
      }

      const totalDuration = Date.now() - startTime;

      // Collect metrics
      const spans = collector.getSpans();
      const metrics = aggregateSpanMetrics(spans, 'benchmark.hook-registration.iteration');
      const memMetrics = aggregateMemoryMetrics(spans, 'benchmark.hook-registration.iteration');

      // Calculate throughput
      const throughput = (ITERATIONS / totalDuration) * 1000;

      // Validation
      const validation = {
        total_criteria: 5,
        passed: 0,
        failed: 0,
        details: [],
      };

      const criteria = [
        { name: 'p95 < 10ms', actual: metrics.durations.p95, target: 10 },
        { name: 'p99 < 10ms', actual: metrics.durations.p99, target: 10 },
        { name: 'memory < 100KB', actual: memMetrics.averageDelta / 1024, target: 100 },
        { name: 'throughput > 100/s', actual: throughput, target: 100 },
        { name: 'error rate < 0.1%', actual: metrics.errorRate * 100, target: 0.1 },
      ];

      for (const criterion of criteria) {
        const passed = criterion.actual <= criterion.target;
        validation.details.push({
          criterion: criterion.name,
          actual: criterion.actual,
          target: criterion.target,
          passed,
        });
        if (passed) validation.passed++;
        else validation.failed++;
      }

      span.setAttributes({
        'benchmark.iterations': ITERATIONS,
        'benchmark.duration_ms': totalDuration,
        'benchmark.passed': validation.failed === 0,
      });

      span.setStatus({ code: SpanStatusCode.OK });

      return {
        benchmark: 'hook-registration',
        iterations: ITERATIONS,
        duration_ms: totalDuration,
        metrics: {
          'registration.p50': metrics.durations.p50,
          'registration.p95': metrics.durations.p95,
          'registration.p99': metrics.durations.p99,
          'registration.memory_delta_kb': memMetrics.averageDelta / 1024,
          'registration.throughput': throughput,
        },
        validation,
      };
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw error;
    } finally {
      span.end();
      await collector.shutdown();
    }
  });
}

async function warmup(manager, iterations) {
  for (let i = 0; i < iterations; i++) {
    const hook = createSimpleHook(`warmup-${i}`);
    manager.addKnowledgeHook(hook);
  }
  manager.clearKnowledgeHooks();
}
```

### Day 2-3: Additional Benchmarks

Create similar files for:
- `02-hook-execution-latency.bench.mjs`
- `03-concurrent-execution.bench.mjs`
- `04-memory-footprint.bench.mjs`
- `05-condition-evaluation.bench.mjs`
- `06-circuit-breaker.bench.mjs`

(Follow the same pattern as `01-hook-registration.bench.mjs`)

---

## Phase 3: CI/CD Integration (Week 3)

### GitHub Actions Workflow

10. `.github/workflows/benchmarks.yml`
```yaml
name: Benchmarks

on:
  pull_request:
  push:
    branches: [main]
  schedule:
    - cron: '0 0 * * *' # Daily at midnight

jobs:
  benchmark:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout
        uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '20'

      - name: Install pnpm
        uses: pnpm/action-setup@v2
        with:
          version: 8

      - name: Install dependencies
        run: pnpm install

      - name: Run benchmarks
        run: node --expose-gc benchmarks/runner.mjs --mode=ci --output=json > benchmark-results.json

      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: benchmark-results.json

      - name: Download baseline
        if: github.event_name == 'pull_request'
        run: |
          gh api \
            -H "Accept: application/vnd.github+json" \
            /repos/${{ github.repository }}/contents/benchmarks/baselines/baseline-latest.json \
            --jq '.content' | base64 -d > baseline.json || echo '{}' > baseline.json
        env:
          GH_TOKEN: ${{ github.token }}

      - name: Compare against baseline
        if: github.event_name == 'pull_request'
        run: node benchmarks/runner.mjs --compare=baseline.json

      - name: Comment PR
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const results = JSON.parse(fs.readFileSync('benchmark-results.json'));

            const comment = `
            ## 📊 Benchmark Results

            **Overall Score:** ${results.summary.score.toFixed(1)}%
            **Benchmarks:** ${results.summary.passed}/${results.summary.total} passed
            **Duration:** ${(results.summary.totalDuration / 1000).toFixed(1)}s

            ### Critical Metrics

            ${results.benchmarks.map(b => `
            #### ${b.benchmark}
            - Status: ${b.validation?.passed ? '✅ PASSED' : '❌ FAILED'}
            - Score: ${b.validation?.passed}/${b.validation?.total_criteria}
            `).join('\n')}

            <details>
            <summary>View Full Results</summary>

            \`\`\`json
            ${JSON.stringify(results, null, 2)}
            \`\`\`

            </details>
            `;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: comment
            });

      - name: Save baseline
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        run: |
          mkdir -p benchmarks/baselines
          cp benchmark-results.json benchmarks/baselines/baseline-$(date +%Y-%m-%d).json
          cp benchmark-results.json benchmarks/baselines/baseline-latest.json
          git config user.name "GitHub Actions"
          git config user.email "actions@github.com"
          git add benchmarks/baselines/
          git commit -m "chore: update benchmark baseline"
          git push
```

---

## Quick Start Commands

```bash
# Run all benchmarks locally
node --expose-gc benchmarks/runner.mjs

# Run specific benchmark
node --expose-gc benchmarks/runner.mjs --suite=hook-registration

# Run in CI mode with JSON output
node --expose-gc benchmarks/runner.mjs --mode=ci --output=json

# Compare against baseline
node --expose-gc benchmarks/runner.mjs --compare=benchmarks/baselines/baseline-latest.json

# Save new baseline
node --expose-gc benchmarks/runner.mjs --save-baseline=benchmarks/baselines/baseline-$(date +%Y-%m-%d).json
```

---

## Checklist

### Week 1: Infrastructure
- [ ] Create directory structure
- [ ] Install dependencies
- [ ] Create test fixtures (hooks, events, stores)
- [ ] Implement OTEL collector
- [ ] Implement percentile calculator
- [ ] Implement memory profiler
- [ ] Implement metrics aggregator
- [ ] Implement benchmark runner

### Week 2: Benchmarks
- [ ] Implement hook registration benchmark
- [ ] Implement hook execution benchmark
- [ ] Implement concurrent execution benchmark
- [ ] Implement memory footprint benchmark
- [ ] Implement condition evaluation benchmark
- [ ] Implement circuit breaker benchmark

### Week 3: CI/CD
- [ ] Create GitHub Actions workflow
- [ ] Test workflow on PR
- [ ] Set up baseline storage
- [ ] Implement regression detection
- [ ] Test PR comment generation

### Week 4: Documentation
- [ ] Document benchmark architecture
- [ ] Create developer guide
- [ ] Write troubleshooting guide
- [ ] Record demo video (optional)

---

**Ready to start? Begin with Phase 1, Day 1!**

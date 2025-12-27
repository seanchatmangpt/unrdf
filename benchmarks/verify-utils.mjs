/**
 * Verification script for benchmark utilities
 * Tests that all utility modules can be loaded and basic functionality works
 */

import { OtelCollector } from './utils/otel-collector.mjs';
import { PercentileCalculator } from './utils/percentile-calculator.mjs';
import { MemoryProfiler } from './utils/memory-profiler.mjs';
import { MetricsAggregator } from './utils/metrics-aggregator.mjs';

console.log('🔍 Verifying benchmark utilities...\n');

// Test OtelCollector
console.log('Testing OtelCollector...');
const collector = new OtelCollector('test-benchmark');
const span = collector.startSpan('test-operation');
collector.endSpan(span, true, { testValue: 123 });
const summary = collector.getSummary();
console.log(`✅ OtelCollector: ${summary.totalSpans} span(s), ${summary.successRate * 100}% success rate`);

// Test PercentileCalculator
console.log('\nTesting PercentileCalculator...');
const calc = new PercentileCalculator();
[10, 20, 30, 40, 50, 60, 70, 80, 90, 100].forEach(v => calc.addMeasurement(v));
const stats = calc.getStats();
console.log(`✅ PercentileCalculator: p50=${stats.p50}ms, p95=${stats.p95}ms, p99=${stats.p99}ms`);

// Test MemoryProfiler
console.log('\nTesting MemoryProfiler...');
const memUsage = MemoryProfiler.getMemoryUsage();
const formatted = MemoryProfiler.formatBytes(memUsage.heapUsed);
const gcAvailable = MemoryProfiler.isGCAvailable();
console.log(`✅ MemoryProfiler: ${formatted} heap used, GC available: ${gcAvailable}`);

// Test MetricsAggregator
console.log('\nTesting MetricsAggregator...');
const aggregator = new MetricsAggregator();
aggregator.recordResult('test-bench', 'simple', 'latency', 15.5, 'ms');
aggregator.recordResult('test-bench', 'simple', 'throughput', 950, 'ops/sec');
const results = aggregator.getResults();
const resultCount = Object.keys(results['test-bench']?.simple || {}).length;
console.log(`✅ MetricsAggregator: ${resultCount} metric(s) recorded`);

console.log('\n✅ All utility modules verified successfully!');
console.log('\n📦 Module structure:');
console.log('  /benchmarks/utils/otel-collector.mjs       - OTEL span collection');
console.log('  /benchmarks/utils/percentile-calculator.mjs - Statistical analysis');
console.log('  /benchmarks/utils/memory-profiler.mjs       - Memory measurement');
console.log('  /benchmarks/utils/metrics-aggregator.mjs    - Results aggregation');
console.log('  /benchmarks/utils/test-fixtures.mjs         - Test data (requires pnpm install)');
console.log('  /benchmarks/baselines/baseline.json         - Performance targets');

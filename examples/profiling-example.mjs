/**
 * @fileoverview Performance Profiling Examples
 */

import { createProfiler, quickProfile } from '../src/profiling/profiler.mjs';
import { _measureLatency } from '../src/profiling/latency-profiler.mjs';
import { measureMemory } from '../src/profiling/memory-profiler.mjs';
import { measureCpu, CpuProfiler } from '../src/profiling/cpu-profiler.mjs';
import { Reporter } from '../src/profiling/reporter.mjs';

/**
 * Example 1: Basic profiling with the main profiler
 */
async function example1_BasicProfiling() {
  console.log('\n=== Example 1: Basic Profiling ===\n');

  const profiler = createProfiler({
    enableLatency: true,
    enableMemory: true,
    enableCpu: false,
  });

  // Profile a simple operation
  const { result, profile } = await profiler.profile('simple-operation', async () => {
    // Simulate some work
    const data = [];
    for (let i = 0; i < 10000; i++) {
      data.push({ id: i, value: Math.random() });
    }

    // Simulate async delay
    await new Promise(resolve => setTimeout(resolve, 50));

    return data.length;
  });

  console.log('Operation result:', result);
  console.log('\nProfile results:');
  console.log('  Latency:', profile.latency.duration.toFixed(2), 'ms');
  console.log('  p95:', profile.latency.p95.toFixed(2), 'ms');
  console.log('  Memory delta:', formatBytes(profile.memory.heapUsedDelta));
}

/**
 * Example 2: Multiple operations with statistics
 */
async function example2_MultipleOperations() {
  console.log('\n=== Example 2: Multiple Operations with Stats ===\n');

  const profiler = createProfiler();

  // Profile the same operation multiple times
  for (let i = 0; i < 10; i++) {
    await profiler.profile('repeated-operation', async () => {
      const delay = Math.random() * 100;
      await new Promise(resolve => setTimeout(resolve, delay));
      return delay;
    });
  }

  // Get aggregate statistics
  const stats = profiler.getStats('repeated-operation');
  console.log('Aggregate statistics:');
  console.log('  Count:', stats.count);
  console.log('  Mean latency:', stats.latency.mean.toFixed(2), 'ms');
  console.log('  p50:', stats.latency.p50.toFixed(2), 'ms');
  console.log('  p95:', stats.latency.p95.toFixed(2), 'ms');
  console.log('  p99:', stats.latency.p99.toFixed(2), 'ms');
}

/**
 * Example 3: Performance budget enforcement
 */
async function example3_PerformanceBudget() {
  console.log('\n=== Example 3: Performance Budget Enforcement ===\n');

  const profiler = createProfiler();

  const { profile } = await profiler.profile('database-query', async () => {
    // Simulate database query
    await new Promise(resolve => setTimeout(resolve, 80));
    return { rows: 100 };
  });

  // Check against performance budget
  const budget = {
    p50: 50, // 50ms target
    p95: 100, // 100ms max
    p99: 150, // 150ms absolute max
  };

  const latencyProfiler = profiler.latencyProfiler;
  const budgetCheck = latencyProfiler.checkBudget(profile.latency, budget);

  console.log('Performance budget check:');
  console.log('  Passed:', budgetCheck.passed);

  if (!budgetCheck.passed) {
    console.log('  Violations:');
    budgetCheck.violations.forEach(v => {
      console.log(
        `    ${v.metric}: ${v.actual.toFixed(2)}ms (budget: ${v.budget}ms, exceeded by ${v.exceeded.toFixed(2)}ms)`
      );
    });
  }
}

/**
 * Example 4: Memory profiling with leak detection
 */
async function example4_MemoryProfiling() {
  console.log('\n=== Example 4: Memory Profiling ===\n');

  const { _result, metrics } = await measureMemory('memory-intensive', async () => {
    const largeArray = new Array(1000000).fill(0).map((_, i) => ({
      id: i,
      data: new Array(10).fill(Math.random()),
    }));

    await new Promise(resolve => setTimeout(resolve, 100));

    return largeArray.length;
  });

  console.log('Memory metrics:');
  console.log('  Heap delta:', formatBytes(metrics.heapUsedDelta));
  console.log('  Peak heap:', formatBytes(metrics.heapUsedPeak));
  console.log('  Trend:', metrics.trend.direction);
  console.log('  Growth rate:', formatBytes(metrics.trend.growthRate), '/s');
  console.log('  Leak detected:', metrics.leakDetected);
}

/**
 * Example 5: CPU profiling (Node.js only)
 */
async function example5_CpuProfiling() {
  console.log('\n=== Example 5: CPU Profiling ===\n');

  if (!CpuProfiler.isAvailable()) {
    console.log('⚠️  CPU profiling not available in this environment');
    return;
  }

  const { _result, metrics } = await measureCpu('cpu-intensive', async () => {
    // CPU-intensive operation
    let sum = 0;
    for (let i = 0; i < 10000000; i++) {
      sum += Math.sqrt(i);
    }
    return sum;
  });

  if (metrics) {
    console.log('CPU metrics:');
    console.log('  Total time:', (metrics.totalTime / 1000).toFixed(2), 'ms');
    console.log('  Sample count:', metrics.sampleCount);
    console.log('  Hot functions:');
    metrics.hotFunctions.slice(0, 5).forEach((fn, i) => {
      console.log(`    ${i + 1}. ${fn.name.slice(0, 50)}`);
      console.log(
        `       ${fn.selfTimePercent.toFixed(2)}% | ${(fn.selfTime / 1000).toFixed(2)} ms`
      );
    });
  }
}

/**
 * Example 6: Quick profiling helper
 */
async function example6_QuickProfile() {
  console.log('\n=== Example 6: Quick Profile Helper ===\n');

  const { _result, profile } = await quickProfile('quick-operation', async () => {
    await new Promise(resolve => setTimeout(resolve, 30));
    return 'done';
  });

  console.log('Quick profile:');
  console.log('  Duration:', profile.latency.duration.toFixed(2), 'ms');
  console.log('  Memory delta:', formatBytes(profile.memory.heapUsedDelta));
}

/**
 * Example 7: Reporting - Terminal, JSON, HTML
 */
async function example7_Reporting() {
  console.log('\n=== Example 7: Profile Reporting ===\n');

  const profiler = createProfiler({
    enableLatency: true,
    enableMemory: true,
    enableCpu: false,
  });

  const { profile } = await profiler.profile('report-demo', async () => {
    const data = new Array(100000).fill(0).map((_, i) => i * i);
    await new Promise(resolve => setTimeout(resolve, 50));
    return data.length;
  });

  // Terminal output
  console.log(Reporter.toTerminal(profile));

  // Save as JSON
  try {
    Reporter.save(profile, 'profile-report.json', {
      format: 'json',
      dir: '.',
    });
    console.log('✓ Saved JSON report: profile-report.json');
  } catch (err) {
    console.log('⚠️  Could not save JSON report:', err.message);
  }

  // Save as HTML
  try {
    Reporter.save(profile, 'profile-report.html', {
      format: 'html',
      dir: '.',
      title: 'Performance Profile Demo',
    });
    console.log('✓ Saved HTML report: profile-report.html');
  } catch (err) {
    console.log('⚠️  Could not save HTML report:', err.message);
  }
}

/**
 * Example 8: Performance regression detection
 */
async function example8_RegressionDetection() {
  console.log('\n=== Example 8: Regression Detection ===\n');

  const profiler = createProfiler();

  // Baseline profile
  const { profile: baseline } = await profiler.profile('api-endpoint', async () => {
    await new Promise(resolve => setTimeout(resolve, 45));
    return { data: 'baseline' };
  });

  // Current profile (simulating regression)
  const { profile: current } = await profiler.profile('api-endpoint', async () => {
    await new Promise(resolve => setTimeout(resolve, 65)); // Slower!
    return { data: 'current' };
  });

  // Compare profiles
  const comparison = Reporter.compare(baseline, current);

  console.log('Regression check:');
  console.log('  Regression detected:', comparison.regression);

  if (comparison.regressions.length > 0) {
    console.log('  Regressions:');
    comparison.regressions.forEach(r => {
      console.log(`    ${r.metric}: ${r.change.toFixed(2)}% slower`);
      console.log(`      Baseline: ${r.baseline.toFixed(2)}ms`);
      console.log(`      Current: ${r.current.toFixed(2)}ms`);
    });
  }

  if (comparison.improvements.length > 0) {
    console.log('  Improvements:');
    comparison.improvements.forEach(i => {
      console.log(`    ${i.metric}: ${Math.abs(i.change).toFixed(2)}% faster`);
    });
  }
}

/**
 * Example 9: UNRDF operation profiling
 */
async function example9_UnrdfOperations() {
  console.log('\n=== Example 9: UNRDF Operation Profiling ===\n');

  const profiler = createProfiler({
    enableLatency: true,
    enableMemory: true,
  });

  // Simulate UNRDF operations with performance targets
  const operations = [
    { name: 'parse', target: 50, fn: () => simulateParse() },
    { name: 'query', target: 100, fn: () => simulateQuery() },
    { name: 'hook-execution', target: 80, fn: () => simulateHook() },
    { name: 'transaction-commit', target: 120, fn: () => simulateCommit() },
  ];

  console.log('Performance Targets:');
  console.log('  Parse operations:    < 50ms');
  console.log('  Query operations:    < 100ms');
  console.log('  Hook execution:      < 80ms');
  console.log('  Transaction commit:  < 120ms\n');

  for (const op of operations) {
    const { profile } = await profiler.profile(op.name, op.fn);

    const passed = profile.latency.p95 < op.target;
    const status = passed ? '✓' : '✗';
    const color = passed ? '' : '⚠️ ';

    console.log(
      `${color}${status} ${op.name.padEnd(20)} p95: ${profile.latency.p95.toFixed(2)}ms (target: ${op.target}ms)`
    );
  }
}

// Helper functions
function formatBytes(bytes) {
  if (Math.abs(bytes) < 1024) return bytes + ' B';
  const units = ['KB', 'MB', 'GB'];
  let u = -1;
  do {
    bytes /= 1024;
    ++u;
  } while (Math.abs(bytes) >= 1024 && u < units.length - 1);
  return bytes.toFixed(2) + ' ' + units[u];
}

// Simulate UNRDF operations
async function simulateParse() {
  await new Promise(resolve => setTimeout(resolve, Math.random() * 40 + 10));
  return { triples: 100 };
}

async function simulateQuery() {
  await new Promise(resolve => setTimeout(resolve, Math.random() * 80 + 20));
  return { results: 50 };
}

async function simulateHook() {
  await new Promise(resolve => setTimeout(resolve, Math.random() * 60 + 20));
  return { executed: true };
}

async function simulateCommit() {
  await new Promise(resolve => setTimeout(resolve, Math.random() * 100 + 40));
  return { committed: true };
}

// Run all examples
async function runAllExamples() {
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║  UNRDF v3.1.0 - Performance Profiling Examples           ║');
  console.log('╚═══════════════════════════════════════════════════════════╝');

  try {
    await example1_BasicProfiling();
    await example2_MultipleOperations();
    await example3_PerformanceBudget();
    await example4_MemoryProfiling();
    await example5_CpuProfiling();
    await example6_QuickProfile();
    await example7_Reporting();
    await example8_RegressionDetection();
    await example9_UnrdfOperations();

    console.log('\n✓ All examples completed successfully!\n');
  } catch (error) {
    console.error('\n✗ Error running examples:', error);
    process.exit(1);
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllExamples();
}

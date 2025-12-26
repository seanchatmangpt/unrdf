#!/usr/bin/env node
/**
 * CPU PROFILING DEMONSTRATION
 *
 * Shows how to identify CPU hotspots using:
 * 1. Performance timing for functions
 * 2. Sampling-based profiling
 * 3. Hotspot identification
 *
 * Run with: node --prof cpu-profile-demo.mjs
 * Then: node --prof-process isolate-*.log > cpu-profile.txt
 *
 * @module profiling/cpu-profile-demo
 */

import { performance } from 'node:perf_hooks';

// ============================================================================
// SIMULATED WORKLOADS
// ============================================================================

/**
 * CPU-intensive task: String manipulation
 */
function heavyStringOperation(iterations = 1000) {
  let result = '';
  for (let i = 0; i < iterations; i++) {
    result += `iteration-${i}-${Math.random()}-`;
    result = result.toUpperCase().toLowerCase();
  }
  return result.length;
}

/**
 * CPU-intensive task: Array operations
 */
function heavyArrayOperation(size = 10000) {
  const arr = Array.from({ length: size }, (_, i) => i);
  return arr
    .map(x => x * 2)
    .filter(x => x % 2 === 0)
    .reduce((sum, x) => sum + x, 0);
}

/**
 * CPU-intensive task: Object manipulation
 */
function heavyObjectOperation(count = 1000) {
  const objects = [];
  for (let i = 0; i < count; i++) {
    objects.push({
      id: `obj-${i}`,
      data: { value: Math.random(), timestamp: Date.now() },
      metadata: { iteration: i, processed: false },
    });
  }

  // Process objects
  return objects
    .map(obj => ({ ...obj, metadata: { ...obj.metadata, processed: true } }))
    .filter(obj => obj.data.value > 0.5)
    .length;
}

// ============================================================================
// PERFORMANCE MEASUREMENT
// ============================================================================

/**
 * Measure function execution time
 */
function measureFunction(fn, name, iterations = 100) {
  const times = [];

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    fn();
    const end = performance.now();
    times.push(end - start);
  }

  const totalTime = times.reduce((sum, t) => sum + t, 0);
  const avgTime = totalTime / iterations;
  const minTime = Math.min(...times);
  const maxTime = Math.max(...times);

  // Calculate percentiles
  const sortedTimes = [...times].sort((a, b) => a - b);
  const p50 = sortedTimes[Math.floor(iterations * 0.5)];
  const p95 = sortedTimes[Math.floor(iterations * 0.95)];
  const p99 = sortedTimes[Math.floor(iterations * 0.99)];

  return {
    name,
    iterations,
    totalTime: totalTime.toFixed(2),
    avgTime: avgTime.toFixed(4),
    minTime: minTime.toFixed(4),
    maxTime: maxTime.toFixed(4),
    p50: p50.toFixed(4),
    p95: p95.toFixed(4),
    p99: p99.toFixed(4),
  };
}

// ============================================================================
// HOTSPOT IDENTIFICATION
// ============================================================================

async function identifyHotspots() {
  console.log('\n=== CPU HOTSPOT IDENTIFICATION ===\n');

  const workloads = [
    {
      name: 'String Operations',
      fn: () => heavyStringOperation(1000),
    },
    {
      name: 'Array Operations',
      fn: () => heavyArrayOperation(10000),
    },
    {
      name: 'Object Operations',
      fn: () => heavyObjectOperation(1000),
    },
  ];

  const results = [];

  for (const workload of workloads) {
    console.log(`Profiling: ${workload.name}...`);
    const result = measureFunction(workload.fn, workload.name, 100);
    results.push(result);
  }

  return results;
}

// ============================================================================
// MAIN EXECUTION
// ============================================================================

async function main() {
  console.log('╔════════════════════════════════════════════════════════════════╗');
  console.log('║  CPU PROFILING DEMONSTRATION                                   ║');
  console.log('║  Performance Timing & Hotspot Identification                  ║');
  console.log('╚════════════════════════════════════════════════════════════════╝');

  const results = await identifyHotspots();

  console.log('\n╔════════════════════════════════════════════════════════════════╗');
  console.log('║  CPU PROFILING RESULTS                                         ║');
  console.log('╚════════════════════════════════════════════════════════════════╝\n');

  console.log('📊 HOTSPOT SUMMARY (100 iterations each)\n');

  // Sort by total time descending
  results.sort((a, b) => parseFloat(b.totalTime) - parseFloat(a.totalTime));

  const maxNameLength = Math.max(...results.map(r => r.name.length));

  console.log('┌─' + '─'.repeat(maxNameLength) + '─┬──────────────┬──────────────┬──────────────┐');
  console.log('│ ' + 'Function'.padEnd(maxNameLength) + ' │ Total (ms)   │ Avg (ms)     │ P95 (ms)     │');
  console.log('├─' + '─'.repeat(maxNameLength) + '─┼──────────────┼──────────────┼──────────────┤');

  for (const result of results) {
    console.log(
      `│ ${result.name.padEnd(maxNameLength)} │ ${result.totalTime.padStart(12)} │ ${result.avgTime.padStart(12)} │ ${result.p95.padStart(12)} │`
    );
  }

  console.log('└─' + '─'.repeat(maxNameLength) + '─┴──────────────┴──────────────┴──────────────┘');

  console.log('\n🔥 TOP 3 HOTSPOTS:');
  results.slice(0, 3).forEach((result, i) => {
    const percentage = ((parseFloat(result.totalTime) /
      results.reduce((sum, r) => sum + parseFloat(r.totalTime), 0)) * 100).toFixed(1);
    console.log(`   ${i + 1}. ${result.name}: ${result.totalTime}ms (${percentage}% of total time)`);
  });

  console.log('\n📈 DETAILED METRICS:\n');

  for (const result of results) {
    console.log(`${result.name}:`);
    console.log(`  Iterations: ${result.iterations}`);
    console.log(`  Total Time: ${result.totalTime} ms`);
    console.log(`  Average: ${result.avgTime} ms`);
    console.log(`  Min: ${result.minTime} ms`);
    console.log(`  Max: ${result.maxTime} ms`);
    console.log(`  P50 (median): ${result.p50} ms`);
    console.log(`  P95: ${result.p95} ms`);
    console.log(`  P99: ${result.p99} ms`);
    console.log('');
  }

  console.log('✅ CPU profiling completed');
  console.log('\nNOTE: For detailed flame graphs, run:');
  console.log('  1. node --prof profiling/cpu-profile-demo.mjs');
  console.log('  2. node --prof-process isolate-*.log > cpu-profile.txt');
  console.log('  3. Review cpu-profile.txt for detailed breakdown');
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { main, measureFunction, identifyHotspots };

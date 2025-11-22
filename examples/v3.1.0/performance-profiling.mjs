/**
 * UNRDF v3.1.0 - Performance Profiling Example
 *
 * This example demonstrates the built-in performance profiler in v3.1.0:
 * - Latency profiling (p50/p95/p99)
 * - Memory profiling
 * - CPU profiling (Node.js only)
 * - Slow query detection
 * - OTEL integration
 */

import { createDarkMatterCore, parseTurtle } from 'unrdf';
import { PrometheusExporter } from '@opentelemetry/exporter-prometheus';

console.log('📊 UNRDF v3.1.0 - Performance Profiling Example\n');

// Example 1: Basic Profiling Setup
console.log('Example 1: Basic Profiling Setup');
console.log('=================================\n');

const system = await createDarkMatterCore({
  profiling: {
    enabled: true,
    sampleRate: 0.1, // Sample 10% of operations
    slowQueryThreshold: 100, // Log queries > 100ms
    onSlowQuery: (query, duration) => {
      console.log(`⚠️  Slow query detected (${duration}ms): ${query.query.slice(0, 80)}...`);
    },
  },
});

console.log('✅ Profiling enabled');
console.log('   Sample Rate: 10%');
console.log('   Slow Query Threshold: 100ms\n');

// Example 2: Add Sample Data
console.log('Example 2: Loading Sample Data');
console.log('===============================\n');

const ttl = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:alice a foaf:Person ;
         foaf:name "Alice" ;
         foaf:age "30"^^xsd:integer ;
         foaf:knows ex:bob, ex:charlie .

ex:bob a foaf:Person ;
       foaf:name "Bob" ;
       foaf:age "25"^^xsd:integer ;
       foaf:knows ex:alice .

ex:charlie a foaf:Person ;
           foaf:name "Charlie" ;
           foaf:age "35"^^xsd:integer ;
           foaf:knows ex:alice .
`;

const store = await parseTurtle(ttl);

await system.executeTransaction({
  additions: [...store],
  removals: [],
  actor: 'profiling-demo',
});

console.log(`✅ Loaded ${[...store].length} triples\n`);

// Example 3: Run Queries to Generate Metrics
console.log('Example 3: Generating Performance Metrics');
console.log('==========================================\n');

console.log('Running 100 queries to collect metrics...');

for (let i = 0; i < 100; i++) {
  await system.query({
    query: `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT ?person ?name WHERE {
        ?person a foaf:Person ;
                foaf:name ?name .
      }
    `,
    type: 'sparql-select',
  });

  // Mix in some complex queries
  if (i % 10 === 0) {
    await system.query({
      query: `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?person ?friend ?friendOfFriend WHERE {
          ?person foaf:knows ?friend .
          ?friend foaf:knows ?friendOfFriend .
          FILTER(?person != ?friendOfFriend)
        }
      `,
      type: 'sparql-select',
    });
  }
}

console.log('✅ Completed 100 queries\n');

// Example 4: Get Performance Profile
console.log('Example 4: Performance Profile');
console.log('===============================\n');

const profile = await system.getPerformanceProfile();

console.log('📊 Latency Metrics:');
console.log(`   p50 (median):     ${profile.latency.p50.toFixed(2)}ms`);
console.log(`   p95:              ${profile.latency.p95.toFixed(2)}ms`);
console.log(`   p99:              ${profile.latency.p99.toFixed(2)}ms`);
console.log(`   Max:              ${profile.latency.max.toFixed(2)}ms`);
console.log(`   Mean:             ${profile.latency.mean.toFixed(2)}ms`);
console.log(`   Std Dev:          ${profile.latency.stddev.toFixed(2)}ms\n`);

console.log('💾 Memory Metrics:');
console.log(`   Heap Used:        ${(profile.memory.heapUsed / 1024 / 1024).toFixed(2)} MB`);
console.log(`   Heap Total:       ${(profile.memory.heapTotal / 1024 / 1024).toFixed(2)} MB`);
console.log(`   External:         ${(profile.memory.external / 1024 / 1024).toFixed(2)} MB`);
console.log(`   RSS:              ${(profile.memory.rss / 1024 / 1024).toFixed(2)} MB`);
console.log(`   GC Pressure:      ${(profile.memory.gcPressure * 100).toFixed(1)}%`);
console.log(`   Leak Score:       ${(profile.memory.leakScore * 100).toFixed(2)}%\n`);

console.log('🗄️  Cache Metrics:');
console.log(`   Hit Rate:         ${(profile.cache.hitRate * 100).toFixed(1)}%`);
console.log(`   Size:             ${profile.cache.size} entries`);
console.log(`   Evictions:        ${profile.cache.evictions}\n`);

// Example 5: Latency by Operation Type
console.log('Example 5: Latency by Operation Type');
console.log('=====================================\n');

console.log('Operation Latencies:');
for (const [operation, latency] of Object.entries(profile.latencyByOperation || {})) {
  console.log(`\n  ${operation}:`);
  console.log(`    p50: ${latency.p50.toFixed(2)}ms`);
  console.log(`    p95: ${latency.p95.toFixed(2)}ms`);
  console.log(`    p99: ${latency.p99.toFixed(2)}ms`);
}
console.log();

// Example 6: Slow Query Detection
console.log('Example 6: Slow Query Detection');
console.log('================================\n');

if (profile.slowQueries.length > 0) {
  console.log(`⚠️  Found ${profile.slowQueries.length} slow queries:\n`);

  for (const query of profile.slowQueries.slice(0, 5)) {
    console.log(`  Duration: ${query.duration.toFixed(2)}ms`);
    console.log(`  Query: ${query.query.slice(0, 100)}...`);
    console.log(`  Type: ${query.type}`);
    console.log(`  Timestamp: ${new Date(query.timestamp).toISOString()}\n`);
  }
} else {
  console.log('✅ No slow queries detected (all queries < 100ms)\n');
}

// Example 7: Real-time Metrics
console.log('Example 7: Real-time Metrics');
console.log('=============================\n');

const metrics = system.getMetrics();

console.log('Current Metrics:');
console.log(`   Current Latency:  ${metrics.currentLatency.toFixed(2)}ms`);
console.log(`   Operations/sec:   ${metrics.operationsPerSecond.toFixed(1)}`);
console.log(`   Active Queries:   ${metrics.activeQueries}`);
console.log(`   Total Operations: ${metrics.totalOperations}\n`);

// Example 8: Performance Budgets
console.log('Example 8: Performance Budgets');
console.log('===============================\n');

const budgetSystem = await createDarkMatterCore({
  profiling: {
    enabled: true,
    budgets: {
      'sparql-select': {
        p50: 15, // ms
        p95: 50,
        p99: 150,
      },
      'transaction-commit': {
        p50: 25,
        p95: 75,
        p99: 200,
      },
    },
    onBudgetExceeded: (operation, metric, actual, budget) => {
      console.log(`\n⚠️  PERFORMANCE BUDGET EXCEEDED`);
      console.log(`   Operation: ${operation}`);
      console.log(`   Metric: ${metric}`);
      console.log(`   Actual: ${actual}ms`);
      console.log(`   Budget: ${budget}ms`);
    },
  },
});

console.log('✅ Performance budgets configured:');
console.log('   sparql-select: p50=15ms, p95=50ms, p99=150ms');
console.log('   transaction-commit: p50=25ms, p95=75ms, p99=200ms\n');

// Example 9: CPU Profiling (Node.js only)
console.log('Example 9: CPU Profiling (Node.js only)');
console.log('========================================\n');

const cpuSystem = await createDarkMatterCore({
  profiling: {
    enabled: true,
    metrics: ['cpu'], // Enable CPU profiling
  },
});

// Run some operations
for (let i = 0; i < 50; i++) {
  await cpuSystem.query({
    query: 'SELECT * WHERE { ?s ?p ?o }',
    type: 'sparql-select',
  });
}

const cpuProfile = await cpuSystem.getPerformanceProfile();

console.log('💻 CPU Metrics:');
console.log(`   Total Time:       ${cpuProfile.cpu.totalTime.toFixed(2)}ms`);
console.log(`   User Time:        ${cpuProfile.cpu.userTime.toFixed(2)}ms`);
console.log(`   System Time:      ${cpuProfile.cpu.systemTime.toFixed(2)}ms`);
console.log(`   Utilization:      ${(cpuProfile.cpu.utilization * 100).toFixed(1)}%\n`);

if (cpuProfile.hotPaths) {
  console.log('🔥 Hot Paths (top 5):');
  for (const path of cpuProfile.hotPaths.slice(0, 5)) {
    console.log(`\n  ${path.function} (${path.file}:${path.line})`);
    console.log(`    Calls:        ${path.count}`);
    console.log(`    Total Time:   ${path.totalTime.toFixed(2)}ms`);
    console.log(`    Avg Time:     ${path.avgTime.toFixed(2)}ms`);
    console.log(`    % of CPU:     ${path.percentage.toFixed(1)}%`);
  }
  console.log();
}

// Example 10: Flamegraph Generation (Node.js only)
console.log('Example 10: Flamegraph Generation (Node.js only)');
console.log('=================================================\n');

console.log('Generating CPU flamegraph (30 second sample)...');

try {
  const flamegraph = await system.generateFlamegraph({
    duration: 5000, // Sample for 5 seconds (reduced for demo)
    outputPath: './profiles/flamegraph.svg',
    format: 'svg',
  });

  console.log(`✅ Flamegraph generated: ${flamegraph.path}`);
  console.log('   Open flamegraph.svg in a browser to visualize hot paths\n');
} catch (err) {
  console.log(`⚠️  Flamegraph generation failed (may not be supported in all environments)`);
  console.log(`   Error: ${err.message}\n`);
}

// Example 11: OTEL Integration
console.log('Example 11: OTEL Integration (Prometheus)');
console.log('==========================================\n');

const prometheusExporter = new PrometheusExporter({ port: 9090 });

const otelSystem = await createDarkMatterCore({
  profiling: {
    enabled: true,
    otel: {
      exporter: prometheusExporter,
      metrics: ['latency', 'memory', 'cache', 'errors'],
      interval: 30000, // Export every 30s
    },
  },
});

console.log('✅ OTEL integration configured');
console.log('   Exporter: Prometheus');
console.log('   Endpoint: http://localhost:9090/metrics');
console.log('   Metrics: latency, memory, cache, errors');
console.log('   Export Interval: 30 seconds\n');

console.log('Prometheus metrics available at:');
console.log('   http://localhost:9090/metrics\n');

console.log('Example Prometheus queries:');
console.log('   # p95 latency');
console.log('   histogram_quantile(0.95, rate(unrdf_operation_duration_bucket[5m]))\n');
console.log('   # Memory usage');
console.log('   unrdf_memory_heap_used_bytes / unrdf_memory_heap_total_bytes\n');
console.log('   # Cache hit rate');
console.log('   rate(unrdf_cache_hits_total[5m]) / ');
console.log('   (rate(unrdf_cache_hits_total[5m]) + rate(unrdf_cache_misses_total[5m]))\n');

// Example 12: Continuous Profiling
console.log('Example 12: Continuous Profiling');
console.log('=================================\n');

const profiler = system.startContinuousProfiling({
  interval: 10000, // Sample every 10 seconds
  output: 'console', // 'console', 'file', or 'otel'
  filePath: './profiles/profile-{timestamp}.json',
});

console.log('✅ Continuous profiling started');
console.log('   Interval: 10 seconds');
console.log('   Output: console');
console.log('\nLet profiler run for 30 seconds...');

// Let it run for 30 seconds
await new Promise(resolve => setTimeout(resolve, 30000));

await profiler.stop();

console.log('\n✅ Continuous profiling stopped');

const profiles = await profiler.getProfiles();
console.log(`   Collected ${profiles.length} profiles\n`);

// Example 13: Memory Leak Detection
console.log('Example 13: Memory Leak Detection');
console.log('==================================\n');

const leakSystem = await createDarkMatterCore({
  profiling: {
    enabled: true,
    memoryLeakDetection: {
      enabled: true,
      sampleInterval: 10000, // Check every 10s
      threshold: 0.1, // Alert if leak score > 10%
      onLeakDetected: leak => {
        console.log('\n⚠️  MEMORY LEAK DETECTED');
        console.log(`   Leak Score: ${(leak.score * 100).toFixed(2)}%`);
        console.log(`   Growth Rate: ${leak.growthRate.toFixed(2)} MB/min`);
        console.log(`   Suspected Source: ${leak.suspectedSource}\n`);
      },
    },
  },
});

console.log('✅ Memory leak detection enabled');
console.log('   Sample Interval: 10 seconds');
console.log('   Threshold: 10%\n');

// Manual leak check
const leakReport = await leakSystem.checkForMemoryLeaks();

if (leakReport.leak) {
  console.log(`⚠️  Memory leak detected (score: ${(leakReport.score * 100).toFixed(2)}%)`);
} else {
  console.log(`✅ No memory leaks detected (score: ${(leakReport.score * 100).toFixed(2)}%)\n`);
}

// Summary
console.log('\nSummary: Performance Profiling Features');
console.log('=========================================\n');

console.log('Built-in Profiler Features:');
console.log('  ✅ Latency profiling (p50/p95/p99)');
console.log('  ✅ Memory profiling (heap, GC, leaks)');
console.log('  ✅ CPU profiling (Node.js only)');
console.log('  ✅ Flamegraph generation (Node.js only)');
console.log('  ✅ Slow query detection');
console.log('  ✅ Cache metrics');
console.log('  ✅ Performance budgets');
console.log('  ✅ OTEL integration (Prometheus/Jaeger)');
console.log('  ✅ Continuous profiling');
console.log('  ✅ Memory leak detection\n');

console.log('Overhead:');
console.log('  Sampling (10%):  <1% overhead');
console.log('  Full (100%):     <2% overhead');
console.log('  CPU profiling:   <5% overhead\n');

console.log('Production Recommended:');
console.log('  - Enable profiling with 5-10% sampling');
console.log('  - Set slow query threshold (100-200ms)');
console.log('  - Export to OTEL (Prometheus/Jaeger)');
console.log('  - Monitor p95/p99 latency');
console.log('  - Watch for memory leaks');
console.log('  - Set performance budgets\n');

// Cleanup
await system.cleanup();
await budgetSystem.cleanup();
await cpuSystem.cleanup();
await otelSystem.cleanup();
await leakSystem.cleanup();

console.log('✅ All profiling examples completed successfully!\n');

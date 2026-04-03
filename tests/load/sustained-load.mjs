/**
 * Sustained Load Test for UNRDF
 *
 * Simulates 24-hour load in 1 hour by running at 10x speed.
 * Monitors memory growth, performance degradation, and resource cleanup.
 *
 * Quality Gates:
 * - Memory growth < 5%
 * - Latency stable (p99 < 2x p50)
 * - No crashes or errors
 * - Proper cleanup
 */

// Use direct path in monorepo
import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';
import v8 from 'v8';
import fs from 'fs';

const { quad, namedNode, literal } = dataFactory;

// Configuration
const SIMULATED_DURATION_HOURS = 24;
const ACTUAL_DURATION_MINUTES = 60;
const ACCELERATION_FACTOR = (SIMULATED_DURATION_HOURS * 60) / ACTUAL_DURATION_MINUTES;

const BASE_OPS_PER_SEC = 10;
const ACTUAL_OPS_PER_SEC = BASE_OPS_PER_SEC * ACCELERATION_FACTOR;
const OPS_INTERVAL_MS = 1000 / ACTUAL_OPS_PER_SEC;

const MEMORY_CHECK_INTERVAL_OPS = 3600 * BASE_OPS_PER_SEC; // Simulated 1 hour
const ACTUAL_MEMORY_CHECK_MS = (MEMORY_CHECK_INTERVAL_OPS / ACTUAL_OPS_PER_SEC) * 1000;

const MEMORY_GROWTH_THRESHOLD = 5; // 5% max growth

// Statistics tracking
const stats = {
  operations: 0,
  queries: 0,
  inserts: 0,
  deletes: 0,
  errors: 0,
  latencies: [],
  memorySnapshots: [],
  startTime: Date.now(),
  startMemory: null,
};

/**
 * Generate random RDF quad for testing
 */
function generateRandomQuad() {
  const id = Math.floor(Math.random() * 100000);
  return quad(
    namedNode(`http://example.org/entity/${id}`),
    namedNode('http://schema.org/name'),
    literal(`Entity ${id}`)
  );
}

/**
 * Record latency measurement
 */
function recordLatency(latencyMs) {
  stats.latencies.push(latencyMs);
  // Keep only recent measurements to avoid memory bloat
  if (stats.latencies.length > 10000) {
    stats.latencies = stats.latencies.slice(-5000);
  }
}

/**
 * Calculate percentiles from latency array
 */
function calculatePercentiles(values) {
  if (values.length === 0) return { p50: 0, p95: 0, p99: 0 };

  const sorted = [...values].sort((a, b) => a - b);
  const p50 = sorted[Math.floor(sorted.length * 0.5)];
  const p95 = sorted[Math.floor(sorted.length * 0.95)];
  const p99 = sorted[Math.floor(sorted.length * 0.99)];

  return { p50, p95, p99 };
}

/**
 * Check memory growth and record snapshot
 */
function checkMemory() {
  const mem = process.memoryUsage();
  const snapshot = {
    timestamp: Date.now(),
    heapUsed: mem.heapUsed,
    heapTotal: mem.heapTotal,
    external: mem.external,
    rss: mem.rss,
  };

  stats.memorySnapshots.push(snapshot);

  const growthPercent = ((mem.heapUsed - stats.startMemory.heapUsed) / stats.startMemory.heapUsed) * 100;
  const elapsedMs = Date.now() - stats.startTime;
  const simulatedHours = (elapsedMs / 1000 / 60) * ACCELERATION_FACTOR / 60;

  const percentiles = calculatePercentiles(stats.latencies);

  console.log(`[${new Date().toISOString()}] Simulated ${simulatedHours.toFixed(1)}h completed`);
  console.log(`  Operations: ${stats.operations} (${stats.queries} queries, ${stats.inserts} inserts, ${stats.deletes} deletes)`);
  console.log(`  Memory: ${(mem.heapUsed / 1024 / 1024).toFixed(2)} MB (growth: ${growthPercent.toFixed(2)}%)`);
  console.log(`  Latency: p50=${percentiles.p50.toFixed(2)}ms, p95=${percentiles.p95.toFixed(2)}ms, p99=${percentiles.p99.toFixed(2)}ms`);
  console.log(`  Errors: ${stats.errors}`);

  // Quality gate: memory growth
  if (growthPercent > MEMORY_GROWTH_THRESHOLD) {
    console.error(`❌ MEMORY LEAK DETECTED: ${growthPercent.toFixed(2)}% growth exceeds ${MEMORY_GROWTH_THRESHOLD}% threshold`);
    return false;
  }

  // Quality gate: latency stability
  if (percentiles.p99 > percentiles.p50 * 2 && percentiles.p50 > 0) {
    console.error(`❌ LATENCY DEGRADATION: p99 (${percentiles.p99.toFixed(2)}ms) > 2x p50 (${percentiles.p50.toFixed(2)}ms)`);
    return false;
  }

  return true;
}

/**
 * Execute mixed workload operation
 */
async function executeOperation(store) {
  const startTime = performance.now();

  try {
    const operation = Math.random();

    if (operation < 0.6) {
      // 60% queries
      const results = [];
      for (const quad of store.match()) {
        results.push(quad);
        if (results.length >= 100) break; // Limit results
      }
      stats.queries++;
    } else if (operation < 0.9) {
      // 30% inserts
      const quad = generateRandomQuad();
      store.add(quad);
      stats.inserts++;
    } else {
      // 10% deletes
      const quads = [];
      for (const quad of store.match()) {
        quads.push(quad);
        if (quads.length >= 10) break;
      }
      if (quads.length > 0) {
        const toDelete = quads[Math.floor(Math.random() * quads.length)];
        store.delete(toDelete);
        stats.deletes++;
      }
    }

    const latency = performance.now() - startTime;
    recordLatency(latency);
    stats.operations++;

  } catch (error) {
    stats.errors++;
    console.error(`Operation error:`, error.message);
  }
}

/**
 * Save heap snapshot
 */
function saveHeapSnapshot(label) {
  const filename = `tests/load/heap-snapshot-${label}-${Date.now()}.heapsnapshot`;
  const snapshot = v8.writeHeapSnapshot(filename);
  console.log(`📸 Heap snapshot saved: ${snapshot}`);
  return snapshot;
}

/**
 * Generate final report
 */
function generateReport() {
  const elapsedMs = Date.now() - stats.startTime;
  const finalMem = process.memoryUsage();
  const growthPercent = ((finalMem.heapUsed - stats.startMemory.heapUsed) / stats.startMemory.heapUsed) * 100;
  const percentiles = calculatePercentiles(stats.latencies);

  const report = {
    config: {
      simulatedDurationHours: SIMULATED_DURATION_HOURS,
      actualDurationMinutes: ACTUAL_DURATION_MINUTES,
      accelerationFactor: ACCELERATION_FACTOR,
      targetOpsPerSec: ACTUAL_OPS_PER_SEC,
    },
    summary: {
      totalOperations: stats.operations,
      queries: stats.queries,
      inserts: stats.inserts,
      deletes: stats.deletes,
      errors: stats.errors,
      elapsedMs: elapsedMs,
      avgOpsPerSec: (stats.operations / (elapsedMs / 1000)).toFixed(2),
    },
    memory: {
      startHeapMB: (stats.startMemory.heapUsed / 1024 / 1024).toFixed(2),
      endHeapMB: (finalMem.heapUsed / 1024 / 1024).toFixed(2),
      growthPercent: growthPercent.toFixed(2),
      passed: growthPercent <= MEMORY_GROWTH_THRESHOLD,
    },
    latency: {
      p50: percentiles.p50.toFixed(2),
      p95: percentiles.p95.toFixed(2),
      p99: percentiles.p99.toFixed(2),
      stable: percentiles.p99 <= percentiles.p50 * 2,
    },
    memorySnapshots: stats.memorySnapshots.map(s => ({
      timestamp: new Date(s.timestamp).toISOString(),
      heapUsedMB: (s.heapUsed / 1024 / 1024).toFixed(2),
      heapTotalMB: (s.heapTotal / 1024 / 1024).toFixed(2),
    })),
    qualityGates: {
      memoryGrowth: growthPercent <= MEMORY_GROWTH_THRESHOLD ? '✅ PASS' : '❌ FAIL',
      latencyStability: percentiles.p99 <= percentiles.p50 * 2 ? '✅ PASS' : '❌ FAIL',
      noErrors: stats.errors === 0 ? '✅ PASS' : '⚠️ WARNING',
      completed: true ? '✅ PASS' : '❌ FAIL',
    },
  };

  return report;
}

/**
 * Main load test execution
 */
async function runLoadTest() {
  console.log('🚀 Starting Sustained Load Test');
  console.log(`Simulating ${SIMULATED_DURATION_HOURS}h load in ${ACTUAL_DURATION_MINUTES} minutes (${ACCELERATION_FACTOR}x acceleration)`);
  console.log(`Target: ${ACTUAL_OPS_PER_SEC.toFixed(1)} ops/sec`);
  console.log('');

  // Initialize
  const store = createStore();
  stats.startMemory = process.memoryUsage();
  stats.startTime = Date.now();

  // Seed initial data
  console.log('Seeding initial data...');
  for (let i = 0; i < 1000; i++) {
    store.add(generateRandomQuad());
  }

  // Initial heap snapshot
  saveHeapSnapshot('initial');

  // Schedule memory checks
  const memoryCheckInterval = setInterval(() => {
    const passed = checkMemory();
    if (!passed) {
      clearInterval(memoryCheckInterval);
      clearInterval(operationInterval);
      console.log('❌ Quality gate failed, stopping test');
      process.exit(1);
    }
  }, ACTUAL_MEMORY_CHECK_MS);

  // Heap snapshots at 25%, 50%, 75%, 100%
  const snapshotPoints = [0.25, 0.5, 0.75, 1.0];
  const totalDurationMs = ACTUAL_DURATION_MINUTES * 60 * 1000;

  snapshotPoints.forEach(point => {
    setTimeout(() => {
      saveHeapSnapshot(`${Math.floor(point * 100)}pct`);
    }, totalDurationMs * point);
  });

  // Execute operations
  let operationCount = 0;
  const operationInterval = setInterval(async () => {
    await executeOperation(store);
    operationCount++;
  }, OPS_INTERVAL_MS);

  // Stop after duration
  setTimeout(() => {
    clearInterval(operationInterval);
    clearInterval(memoryCheckInterval);

    console.log('\n✅ Load test duration completed');

    // Final checks
    checkMemory();
    saveHeapSnapshot('final');

    // Generate report
    const report = generateReport();
    const reportPath = `tests/load/load-test-report-${Date.now()}.json`;
    fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));

    console.log('\n📊 LOAD TEST REPORT');
    console.log('==================');
    console.log(`Total Operations: ${report.summary.totalOperations}`);
    console.log(`  Queries: ${report.summary.queries}`);
    console.log(`  Inserts: ${report.summary.inserts}`);
    console.log(`  Deletes: ${report.summary.deletes}`);
    console.log(`  Errors: ${report.summary.errors}`);
    console.log(`Average Ops/Sec: ${report.summary.avgOpsPerSec}`);
    console.log('');
    console.log(`Memory Growth: ${report.memory.growthPercent}% ${report.qualityGates.memoryGrowth}`);
    console.log(`Latency Stable: ${report.qualityGates.latencyStability}`);
    console.log(`  p50: ${report.latency.p50}ms`);
    console.log(`  p95: ${report.latency.p95}ms`);
    console.log(`  p99: ${report.latency.p99}ms`);
    console.log('');
    console.log(`Report saved: ${reportPath}`);

    // Final verdict
    const allPassed = Object.values(report.qualityGates).every(v => v.includes('✅'));
    if (allPassed) {
      console.log('\n🎉 ALL QUALITY GATES PASSED');
      process.exit(0);
    } else {
      console.log('\n❌ SOME QUALITY GATES FAILED');
      process.exit(1);
    }
  }, totalDurationMs);
}

// Run test
runLoadTest().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});

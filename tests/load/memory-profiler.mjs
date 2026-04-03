/**
 * Memory Profiler for UNRDF
 *
 * Advanced memory analysis with:
 * - Heap snapshot comparison
 * - Memory leak detection
 * - Object retention analysis
 * - GC impact measurement
 */

// Use direct path in monorepo
import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';
import v8 from 'v8';
import fs from 'fs';

const { quad, namedNode, literal } = dataFactory;

/**
 * Configuration
 */
const PROFILE_DURATION_MS = 10 * 60 * 1000; // 10 minutes
const SNAPSHOT_INTERVAL_MS = 2 * 60 * 1000; // 2 minutes
const OPS_PER_SEC = 100;

/**
 * Generate test quad
 */
function generateQuad(id) {
  return quad(
    namedNode(`http://example.org/entity/${id}`),
    namedNode('http://schema.org/name'),
    literal(`Entity ${id}`)
  );
}

/**
 * Take heap snapshot with metadata
 */
function takeSnapshot(label, metadata = {}) {
  const mem = process.memoryUsage();
  const filename = `tests/load/heap-${label}-${Date.now()}.heapsnapshot`;

  console.log(`\n📸 Taking heap snapshot: ${label}`);
  v8.writeHeapSnapshot(filename);

  const snapshot = {
    label,
    filename,
    timestamp: Date.now(),
    memory: {
      heapUsed: mem.heapUsed,
      heapTotal: mem.heapTotal,
      external: mem.external,
      rss: mem.rss,
    },
    metadata,
  };

  console.log(`   Heap: ${(mem.heapUsed / 1024 / 1024).toFixed(2)} MB`);
  console.log(`   Total: ${(mem.heapTotal / 1024 / 1024).toFixed(2)} MB`);
  console.log(`   External: ${(mem.external / 1024 / 1024).toFixed(2)} MB`);
  console.log(`   RSS: ${(mem.rss / 1024 / 1024).toFixed(2)} MB`);

  return snapshot;
}

/**
 * Measure GC impact
 */
function measureGCImpact() {
  if (!global.gc) {
    console.log('⚠️ Run with --expose-gc for GC measurements');
    return null;
  }

  const before = process.memoryUsage().heapUsed;
  const startTime = performance.now();

  global.gc();

  const gcTime = performance.now() - startTime;
  const after = process.memoryUsage().heapUsed;
  const freed = before - after;

  return {
    gcTimeMs: gcTime,
    freedBytes: freed,
    freedMB: freed / 1024 / 1024,
  };
}

/**
 * Analyze memory growth between snapshots
 */
function analyzeGrowth(snapshots) {
  if (snapshots.length < 2) return null;

  const first = snapshots[0];
  const last = snapshots[snapshots.length - 1];

  const durationMs = last.timestamp - first.timestamp;
  const heapGrowth = last.memory.heapUsed - first.memory.heapUsed;
  const growthRate = (heapGrowth / durationMs) * 1000; // bytes/sec

  return {
    durationSec: durationMs / 1000,
    heapGrowthMB: heapGrowth / 1024 / 1024,
    growthRateKBPerSec: (growthRate / 1024).toFixed(2),
    growthPercent: ((heapGrowth / first.memory.heapUsed) * 100).toFixed(2),
  };
}

/**
 * Detect potential memory leaks
 */
function detectLeaks(snapshots, gcResults) {
  const issues = [];

  // Check monotonic growth
  let consecutiveGrowth = 0;
  for (let i = 1; i < snapshots.length; i++) {
    const growth = snapshots[i].memory.heapUsed - snapshots[i - 1].memory.heapUsed;
    if (growth > 0) {
      consecutiveGrowth++;
    } else {
      consecutiveGrowth = 0;
    }
  }

  if (consecutiveGrowth >= snapshots.length - 1) {
    issues.push({
      severity: 'HIGH',
      type: 'MONOTONIC_GROWTH',
      description: 'Heap size growing continuously without decrease',
    });
  }

  // Check GC effectiveness
  if (gcResults && gcResults.length > 0) {
    const avgFreed = gcResults.reduce((sum, gc) => sum + gc.freedMB, 0) / gcResults.length;
    if (avgFreed < 1) {
      issues.push({
        severity: 'MEDIUM',
        type: 'INEFFECTIVE_GC',
        description: `GC freeing minimal memory (avg ${avgFreed.toFixed(2)} MB)`,
      });
    }
  }

  // Check growth rate
  const analysis = analyzeGrowth(snapshots);
  if (analysis && parseFloat(analysis.growthRateKBPerSec) > 100) {
    issues.push({
      severity: 'HIGH',
      type: 'HIGH_GROWTH_RATE',
      description: `High memory growth rate: ${analysis.growthRateKBPerSec} KB/sec`,
    });
  }

  return issues;
}

/**
 * Run memory profiling session
 */
async function runProfiling() {
  console.log('🔍 UNRDF Memory Profiling Session');
  console.log(`Duration: ${PROFILE_DURATION_MS / 1000 / 60} minutes`);
  console.log(`Snapshots: Every ${SNAPSHOT_INTERVAL_MS / 1000 / 60} minutes`);
  console.log(`Workload: ${OPS_PER_SEC} ops/sec\n`);

  const snapshots = [];
  const gcResults = [];
  const stats = {
    operations: 0,
    queries: 0,
    inserts: 0,
    deletes: 0,
  };

  // Initialize store
  const store = createStore();

  // Seed initial data
  console.log('Seeding initial data...');
  for (let i = 0; i < 1000; i++) {
    store.add(generateQuad(i));
  }

  // Initial snapshot
  snapshots.push(takeSnapshot('initial', { operations: 0 }));

  // Schedule periodic snapshots
  const snapshotTimer = setInterval(() => {
    // Force GC before snapshot
    const gcResult = measureGCImpact();
    if (gcResult) {
      gcResults.push(gcResult);
      console.log(`   GC freed ${gcResult.freedMB.toFixed(2)} MB in ${gcResult.gcTimeMs.toFixed(2)}ms`);
    }

    snapshots.push(takeSnapshot(`interval-${snapshots.length}`, {
      operations: stats.operations,
      queries: stats.queries,
      inserts: stats.inserts,
      deletes: stats.deletes,
    }));
  }, SNAPSHOT_INTERVAL_MS);

  // Execute workload
  const operationInterval = setInterval(() => {
    const op = Math.random();

    if (op < 0.6) {
      // Query
      let count = 0;
      for (const _ of store.match()) {
        count++;
        if (count >= 10) break;
      }
      stats.queries++;
    } else if (op < 0.9) {
      // Insert
      store.add(generateQuad(stats.operations + 1000));
      stats.inserts++;
    } else {
      // Delete
      const quads = [];
      for (const q of store.match()) {
        quads.push(q);
        if (quads.length >= 10) break;
      }
      if (quads.length > 0) {
        store.delete(quads[Math.floor(Math.random() * quads.length)]);
        stats.deletes++;
      }
    }

    stats.operations++;
  }, 1000 / OPS_PER_SEC);

  // Stop after duration
  setTimeout(() => {
    clearInterval(operationInterval);
    clearInterval(snapshotTimer);

    console.log('\n✅ Profiling duration complete\n');

    // Final snapshot
    const gcResult = measureGCImpact();
    if (gcResult) gcResults.push(gcResult);

    snapshots.push(takeSnapshot('final', {
      operations: stats.operations,
      queries: stats.queries,
      inserts: stats.inserts,
      deletes: stats.deletes,
    }));

    // Analyze results
    console.log('\n📊 Memory Analysis');
    console.log('==================\n');

    const growth = analyzeGrowth(snapshots);
    if (growth) {
      console.log('Memory Growth:');
      console.log(`  Duration: ${growth.durationSec.toFixed(0)} seconds`);
      console.log(`  Heap Growth: ${growth.heapGrowthMB.toFixed(2)} MB (${growth.growthPercent}%)`);
      console.log(`  Growth Rate: ${growth.growthRateKBPerSec} KB/sec`);
    }

    if (gcResults.length > 0) {
      const totalFreed = gcResults.reduce((sum, gc) => sum + gc.freedMB, 0);
      const avgGCTime = gcResults.reduce((sum, gc) => sum + gc.gcTimeMs, 0) / gcResults.length;
      console.log('\nGC Statistics:');
      console.log(`  Collections: ${gcResults.length}`);
      console.log(`  Total Freed: ${totalFreed.toFixed(2)} MB`);
      console.log(`  Avg GC Time: ${avgGCTime.toFixed(2)}ms`);
    }

    const leaks = detectLeaks(snapshots, gcResults);
    console.log('\nLeak Detection:');
    if (leaks.length === 0) {
      console.log('  ✅ No memory leaks detected');
    } else {
      leaks.forEach(leak => {
        console.log(`  ⚠️ [${leak.severity}] ${leak.type}: ${leak.description}`);
      });
    }

    // Generate report
    const report = {
      timestamp: new Date().toISOString(),
      config: {
        durationMs: PROFILE_DURATION_MS,
        snapshotIntervalMs: SNAPSHOT_INTERVAL_MS,
        opsPerSec: OPS_PER_SEC,
      },
      stats,
      snapshots: snapshots.map(s => ({
        label: s.label,
        filename: s.filename,
        timestamp: new Date(s.timestamp).toISOString(),
        heapUsedMB: (s.memory.heapUsed / 1024 / 1024).toFixed(2),
        metadata: s.metadata,
      })),
      analysis: {
        growth,
        gcStats: gcResults.length > 0 ? {
          count: gcResults.length,
          totalFreedMB: gcResults.reduce((sum, gc) => sum + gc.freedMB, 0).toFixed(2),
          avgGCTimeMs: (gcResults.reduce((sum, gc) => sum + gc.gcTimeMs, 0) / gcResults.length).toFixed(2),
        } : null,
        leaks,
      },
    };

    const reportPath = `tests/load/memory-profile-${Date.now()}.json`;
    fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));

    console.log(`\n📄 Profile report saved: ${reportPath}`);
    console.log('\n✅ Memory profiling complete');

    process.exit(leaks.length === 0 ? 0 : 1);
  }, PROFILE_DURATION_MS);
}

// Run profiler
runProfiling().catch(error => {
  console.error('Profiling failed:', error);
  process.exit(1);
});

/**
 * Slow Stable Test - 5 ops/sec for 2 minutes
 * Minimal rate to isolate memory leak source
 */

import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';
import fs from 'fs';

const { quad, namedNode, literal } = dataFactory;

const DURATION_MS = 2 * 60 * 1000; // 2 minutes
const OPS_PER_SEC = 5;
const REPORT_INTERVAL_MS = 15 * 1000; // Every 15 seconds

const stats = {
  operations: 0,
  startMemory: null,
  startTime: Date.now(),
};

function generateQuad(id) {
  return quad(
    namedNode(`http://example.org/entity/${id}`),
    namedNode('http://schema.org/name'),
    literal(`Entity ${id}`)
  );
}

function reportMemory() {
  const mem = process.memoryUsage();
  const growthPercent = ((mem.heapUsed - stats.startMemory.heapUsed) / stats.startMemory.heapUsed) * 100;
  const elapsedSec = (Date.now() - stats.startTime) / 1000;

  console.log(`[${elapsedSec.toFixed(0)}s] Ops: ${stats.operations}, Heap: ${(mem.heapUsed / 1024 / 1024).toFixed(1)}MB (+${growthPercent.toFixed(1)}%)`);

  return growthPercent;
}

async function run() {
  console.log('🐌 Slow Stable Test (5 ops/sec for 2 min)\n');

  const store = createStore();

  // Seed
  console.log('Seeding 1000 quads...');
  for (let i = 0; i < 1000; i++) {
    store.add(generateQuad(i));
  }

  // Stabilize
  if (global.gc) global.gc();
  await new Promise(r => setTimeout(r, 1000));

  stats.startMemory = process.memoryUsage();
  stats.startTime = Date.now();
  console.log(`Baseline: ${(stats.startMemory.heapUsed / 1024 / 1024).toFixed(1)}MB\n`);

  // Report interval
  const reportInterval = setInterval(reportMemory, REPORT_INTERVAL_MS);

  // Execute operations (only queries, no inserts/deletes)
  const opInterval = setInterval(() => {
    let count = 0;
    for (const _ of store.match()) {
      count++;
      if (count >= 10) break;
    }
    stats.operations++;
  }, 1000 / OPS_PER_SEC);

  // Stop after duration
  setTimeout(() => {
    clearInterval(opInterval);
    clearInterval(reportInterval);

    const finalGrowth = reportMemory();

    const report = {
      timestamp: new Date().toISOString(),
      duration: DURATION_MS / 1000,
      opsPerSec: OPS_PER_SEC,
      totalOps: stats.operations,
      memoryGrowthPercent: finalGrowth.toFixed(2),
      passed: finalGrowth < 20, // Very lenient for slow test
    };

    const reportPath = `tests/load/slow-stable-${Date.now()}.json`;
    fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));

    console.log(`\n${report.passed ? '✅' : '❌'} Growth: ${report.memoryGrowthPercent}%`);
    console.log(`📄 ${reportPath}`);

    process.exit(report.passed ? 0 : 1);
  }, DURATION_MS);
}

run().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});

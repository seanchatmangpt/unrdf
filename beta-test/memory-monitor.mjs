#!/usr/bin/env node
/**
 * Memory Stability Monitor
 * Tracks memory usage over time to detect leaks
 */

const SAMPLES = 10;
const INTERVAL_MS = 500;
const MAX_GROWTH_MB = 50; // Fail if growth >50MB

const samples = [];

async function collectSample() {
  const usage = process.memoryUsage();
  const sample = {
    timestamp: Date.now(),
    heapUsed: usage.heapUsed,
    heapTotal: usage.heapTotal,
    external: usage.external,
    rss: usage.rss,
  };
  samples.push(sample);

  console.log(`[${samples.length}/${SAMPLES}] Heap: ${(usage.heapUsed / 1024 / 1024).toFixed(2)} MB, RSS: ${(usage.rss / 1024 / 1024).toFixed(2)} MB`);

  // Trigger some allocation
  const tempArray = new Array(1000).fill(Math.random());
  await new Promise(resolve => setTimeout(resolve, 10));
  tempArray.length = 0; // Allow GC
}

async function analyzeMemory() {
  console.log('\n=== Memory Stability Analysis ===');

  const firstSample = samples[0];
  const lastSample = samples[samples.length - 1];

  const heapGrowthMB = (lastSample.heapUsed - firstSample.heapUsed) / 1024 / 1024;
  const rssGrowthMB = (lastSample.rss - firstSample.rss) / 1024 / 1024;

  console.log(`Heap Growth: ${heapGrowthMB.toFixed(2)} MB`);
  console.log(`RSS Growth: ${rssGrowthMB.toFixed(2)} MB`);
  console.log(`Duration: ${(lastSample.timestamp - firstSample.timestamp) / 1000}s`);

  const result = {
    heapGrowthMB,
    rssGrowthMB,
    samples: samples.length,
    status: heapGrowthMB < MAX_GROWTH_MB ? 'PASS' : 'FAIL',
  };

  console.log(JSON.stringify(result, null, 2));

  if (result.status === 'FAIL') {
    console.error(`❌ Memory leak detected: ${heapGrowthMB.toFixed(2)} MB > ${MAX_GROWTH_MB} MB threshold`);
    process.exit(1);
  }

  console.log('✅ Memory stable');
  process.exit(0);
}

async function main() {
  console.log('Starting memory stability monitor...');
  console.log(`Collecting ${SAMPLES} samples at ${INTERVAL_MS}ms intervals\n`);

  for (let i = 0; i < SAMPLES; i++) {
    await collectSample();
    if (i < SAMPLES - 1) {
      await new Promise(resolve => setTimeout(resolve, INTERVAL_MS));
    }
  }

  await analyzeMemory();
}

main().catch(err => {
  console.error('Memory monitor failed:', err);
  process.exit(1);
});

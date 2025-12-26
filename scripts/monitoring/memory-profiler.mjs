#!/usr/bin/env node
/**
 * Memory Profiler
 * Profiles memory usage during test execution
 */

import { execSync } from 'child_process';
import { writeFileSync } from 'fs';

const SAMPLE_INTERVAL = 100; // ms

/**
 * Get current memory usage
 * @returns {Object} Memory stats
 */
function getMemoryUsage() {
  const usage = process.memoryUsage();
  return {
    rss: Math.round(usage.rss / 1024 / 1024), // MB
    heapTotal: Math.round(usage.heapTotal / 1024 / 1024),
    heapUsed: Math.round(usage.heapUsed / 1024 / 1024),
    external: Math.round(usage.external / 1024 / 1024),
    timestamp: Date.now()
  };
}

/**
 * Profile memory during operation
 * @param {Function} operation - Operation to profile
 * @returns {Object} Profile results
 */
async function profileMemory(operation) {
  const samples = [];
  let sampling = true;

  // Start sampling
  const samplingInterval = setInterval(() => {
    if (sampling) {
      samples.push(getMemoryUsage());
    }
  }, SAMPLE_INTERVAL);

  // Run operation
  const startTime = Date.now();
  await operation();
  const duration = Date.now() - startTime;

  // Stop sampling
  sampling = false;
  clearInterval(samplingInterval);

  // Force GC if available
  if (global.gc) {
    global.gc();
    samples.push({ ...getMemoryUsage(), gc: true });
  }

  return {
    duration,
    samples,
    summary: {
      peak_usage: Math.max(...samples.map(s => s.heapUsed)),
      avg_usage: Math.round(samples.reduce((sum, s) => sum + s.heapUsed, 0) / samples.length),
      sample_count: samples.length
    }
  };
}

/**
 * Run memory-intensive test
 */
async function runMemoryTest() {
  console.log('Running memory profiling...');

  // Profile a typical workload
  const result = await profileMemory(async () => {
    // Simulate typical usage
    const data = [];
    for (let i = 0; i < 10000; i++) {
      data.push({
        subject: `http://example.org/subject${i}`,
        predicate: 'http://example.org/predicate',
        object: `http://example.org/object${i}`
      });
    }

    // Force some GC cycles
    if (global.gc) {
      global.gc();
    }

    return data;
  });

  return result;
}

/**
 * Main execution
 */
async function main() {
  const profile = await runMemoryTest();

  // Write profile to file
  writeFileSync('memory-profile.json', JSON.stringify(profile, null, 2));

  // Write log
  const log = `Memory Profiling Results
========================
Duration: ${profile.duration}ms
Samples: ${profile.summary.sample_count}
Peak Usage: ${profile.summary.peak_usage}MB
Avg Usage: ${profile.summary.avg_usage}MB
`;

  console.log(log);
  writeFileSync('memory-profile.log', log);

  console.log('\n✅ Memory profiling completed');
  console.log('Results saved to: memory-profile.json');
}

main().catch(error => {
  console.error('Memory profiling error:', error);
  process.exit(1);
});

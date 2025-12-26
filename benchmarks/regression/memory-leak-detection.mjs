/**
 * @file Memory Leak Detection Benchmarks
 * @module benchmarks/regression/memory-leak-detection
 *
 * @description
 * Detect memory leaks through controlled testing:
 * - Monitor memory over time
 * - Detect growing heap
 * - Force GC and measure retained memory
 * - Report potential leaks
 */

import { suite, getMemoryUsage, formatBytes } from '../framework.mjs';

// =============================================================================
// Memory Leak Detection
// =============================================================================

/**
 * Run memory leak detection test
 * @param {Function} fn - Function to test
 * @param {object} options - Test options
 * @returns {Promise<object>} Leak detection results
 */
export async function detectMemoryLeak(fn, options = {}) {
  const {
    iterations = 1000,
    samplingInterval = 100,
    gcInterval = 200,
    leakThreshold = 1024 * 1024 // 1 MB growth = potential leak
  } = options;

  const memorySnapshots = [];
  let leakDetected = false;
  let leakDetails = null;

  // Force initial GC if available
  if (global.gc) {
    global.gc();
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  const initialMemory = getMemoryUsage();
  memorySnapshots.push({ iteration: 0, ...initialMemory });

  for (let i = 1; i <= iterations; i++) {
    // Run function
    await fn();

    // Sample memory at intervals
    if (i % samplingInterval === 0) {
      // Force GC before sampling
      if (global.gc && i % gcInterval === 0) {
        global.gc();
        await new Promise(resolve => setTimeout(resolve, 10));
      }

      const memory = getMemoryUsage();
      memorySnapshots.push({ iteration: i, ...memory });

      // Check for leak (compare to initial)
      const heapGrowth = memory.heapUsed - initialMemory.heapUsed;
      const rssGrowth = memory.rss - initialMemory.rss;

      if (heapGrowth > leakThreshold || rssGrowth > leakThreshold * 2) {
        leakDetected = true;
        leakDetails = {
          iteration: i,
          heapGrowth,
          rssGrowth,
          currentHeap: memory.heapUsed,
          currentRss: memory.rss
        };
      }
    }
  }

  // Final GC and measurement
  if (global.gc) {
    global.gc();
    await new Promise(resolve => setTimeout(resolve, 100));
  }

  const finalMemory = getMemoryUsage();
  memorySnapshots.push({ iteration: iterations, ...finalMemory });

  // Calculate leak rate
  const heapGrowth = finalMemory.heapUsed - initialMemory.heapUsed;
  const rssGrowth = finalMemory.rss - initialMemory.rss;
  const heapGrowthRate = heapGrowth / iterations; // bytes per iteration
  const rssGrowthRate = rssGrowth / iterations;

  return {
    leakDetected,
    leakDetails,
    initialMemory,
    finalMemory,
    heapGrowth,
    rssGrowth,
    heapGrowthRate,
    rssGrowthRate,
    snapshots: memorySnapshots,
    summary: {
      iterations,
      heapGrowth: formatBytes(heapGrowth),
      rssGrowth: formatBytes(rssGrowth),
      heapGrowthRate: `${formatBytes(heapGrowthRate)}/iter`,
      rssGrowthRate: `${formatBytes(rssGrowthRate)}/iter`
    }
  };
}

/**
 * Format leak detection report
 * @param {object} result - Leak detection result
 * @returns {string} Markdown report
 */
export function formatLeakReport(result) {
  const lines = [];

  lines.push('# Memory Leak Detection Report\n');

  // Summary
  lines.push('## Summary\n');
  lines.push(`- **Iterations**: ${result.summary.iterations}`);
  lines.push(`- **Heap Growth**: ${result.summary.heapGrowth}`);
  lines.push(`- **RSS Growth**: ${result.summary.rssGrowth}`);
  lines.push(`- **Heap Growth Rate**: ${result.summary.heapGrowthRate}`);
  lines.push(`- **RSS Growth Rate**: ${result.summary.rssGrowthRate}\n`);

  // Leak status
  if (result.leakDetected) {
    lines.push('## ⚠️ Potential Memory Leak Detected\n');
    lines.push(`- **Detected at iteration**: ${result.leakDetails.iteration}`);
    lines.push(`- **Heap growth**: ${formatBytes(result.leakDetails.heapGrowth)}`);
    lines.push(`- **RSS growth**: ${formatBytes(result.leakDetails.rssGrowth)}\n`);
  } else {
    lines.push('## ✓ No Memory Leak Detected\n');
  }

  // Memory snapshots table
  lines.push('## Memory Snapshots\n');
  lines.push('| Iteration | Heap Used | RSS | Heap Total |');
  lines.push('|-----------|-----------|-----|------------|');

  for (const snapshot of result.snapshots) {
    lines.push(`| ${snapshot.iteration} | ${formatBytes(snapshot.heapUsed)} | ${formatBytes(snapshot.rss)} | ${formatBytes(snapshot.heapTotal)} |`);
  }
  lines.push('');

  return lines.join('\n');
}

// =============================================================================
// Test Cases
// =============================================================================

/**
 * Test case: No leak (properly cleaned)
 */
async function testNoLeak() {
  const arr = [];
  for (let i = 0; i < 100; i++) {
    arr.push(i);
  }
  arr.length = 0; // Clean up
}

/**
 * Test case: Potential leak (growing array)
 */
const leakyArray = [];
async function testWithLeak() {
  for (let i = 0; i < 100; i++) {
    leakyArray.push({ id: i, data: Buffer.alloc(1024) }); // Intentional leak
  }
}

/**
 * Test case: Proper cleanup with map
 */
const cache = new Map();
async function testMapWithCleanup() {
  const key = Math.random().toString();
  cache.set(key, { data: Buffer.alloc(1024) });

  // Cleanup old entries
  if (cache.size > 100) {
    const firstKey = cache.keys().next().value;
    cache.delete(firstKey);
  }
}

// =============================================================================
// Benchmark Suite
// =============================================================================

export const memoryLeakBenchmarks = suite('Memory Leak Detection', {
  'detect no leak scenario': {
    fn: async () => {
      return await detectMemoryLeak(testNoLeak, {
        iterations: 1000,
        samplingInterval: 100,
        gcInterval: 200
      });
    },
    iterations: 1,
    warmup: 0,
    gc: false
  },

  'detect leak scenario': {
    fn: async () => {
      return await detectMemoryLeak(testWithLeak, {
        iterations: 100, // Fewer iterations for intentional leak
        samplingInterval: 10,
        gcInterval: 20
      });
    },
    iterations: 1,
    warmup: 0,
    gc: false
  },

  'detect cleanup scenario': {
    fn: async () => {
      return await detectMemoryLeak(testMapWithCleanup, {
        iterations: 500,
        samplingInterval: 50,
        gcInterval: 100
      });
    },
    iterations: 1,
    warmup: 0,
    gc: false
  }
});

// =============================================================================
// Runner
// =============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  console.log('Running memory leak detection tests...\n');
  console.log('Note: Run with --expose-gc flag for accurate results\n');

  if (!global.gc) {
    console.warn('⚠️  GC not exposed. Run with: node --expose-gc memory-leak-detection.mjs\n');
  }

  // Test 1: No leak
  console.log('Test 1: No leak scenario');
  const result1 = await detectMemoryLeak(testNoLeak, {
    iterations: 1000,
    samplingInterval: 100
  });
  console.log(formatLeakReport(result1));

  // Test 2: With leak (intentional)
  console.log('\nTest 2: Leak scenario (intentional)');
  const result2 = await detectMemoryLeak(testWithLeak, {
    iterations: 100,
    samplingInterval: 10
  });
  console.log(formatLeakReport(result2));

  // Test 3: Cleanup
  console.log('\nTest 3: Cleanup scenario');
  const result3 = await detectMemoryLeak(testMapWithCleanup, {
    iterations: 500,
    samplingInterval: 50
  });
  console.log(formatLeakReport(result3));

  process.exit(0);
}

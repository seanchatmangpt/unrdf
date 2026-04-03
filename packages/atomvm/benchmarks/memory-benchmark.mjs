/**
 * Memory Profiling Benchmark Suite
 *
 * Measures memory usage patterns including startup, growth, leaks, and concurrent operations.
 *
 * @module benchmarks/memory-benchmark
 */

import { performance } from 'perf_hooks';

/**
 * Get current memory usage in MB
 * @returns {Object} Memory usage statistics
 */
function getMemoryUsage() {
  const usage = process.memoryUsage();
  return {
    rss: (usage.rss / 1024 / 1024).toFixed(2), // Resident Set Size
    heapTotal: (usage.heapTotal / 1024 / 1024).toFixed(2),
    heapUsed: (usage.heapUsed / 1024 / 1024).toFixed(2),
    external: (usage.external / 1024 / 1024).toFixed(2),
    arrayBuffers: (usage.arrayBuffers / 1024 / 1024).toFixed(2),
  };
}

/**
 * Force garbage collection (requires --expose-gc flag)
 */
function forceGC() {
  if (global.gc) {
    global.gc();
  }
}

/**
 * Measure startup memory usage
 * @returns {Promise<Object>} Startup memory statistics
 */
export async function measureStartupMemory() {
  console.log('\n=== Startup Memory Benchmark ===');

  forceGC();
  await new Promise(resolve => setTimeout(resolve, 100));

  const initialMemory = getMemoryUsage();
  console.log('Initial Memory:');
  console.log(`  RSS: ${initialMemory.rss} MB`);
  console.log(`  Heap Used: ${initialMemory.heapUsed} MB`);
  console.log(`  Heap Total: ${initialMemory.heapTotal} MB`);
  console.log(`  External: ${initialMemory.external} MB`);

  // Simulate module loading
  const modules = [];
  for (let i = 0; i < 10; i++) {
    modules.push({
      id: i,
      data: new Array(1000).fill(i),
    });
  }

  const postLoadMemory = getMemoryUsage();
  const memoryDelta = {
    rss: (parseFloat(postLoadMemory.rss) - parseFloat(initialMemory.rss)).toFixed(2),
    heapUsed: (parseFloat(postLoadMemory.heapUsed) - parseFloat(initialMemory.heapUsed)).toFixed(2),
  };

  console.log('\nPost-Load Memory:');
  console.log(`  RSS: ${postLoadMemory.rss} MB (+${memoryDelta.rss} MB)`);
  console.log(`  Heap Used: ${postLoadMemory.heapUsed} MB (+${memoryDelta.heapUsed} MB)`);

  return {
    initial: initialMemory,
    postLoad: postLoadMemory,
    delta: memoryDelta,
  };
}

/**
 * Measure memory growth over time
 * @param {number} operations - Number of operations to perform
 * @param {number} samplingInterval - Interval between samples (ms)
 * @returns {Promise<Object>} Memory growth statistics
 */
export async function measureMemoryGrowth(operations = 1000, samplingInterval = 100) {
  console.log(`\n=== Memory Growth Benchmark (${operations} operations) ===`);

  forceGC();
  await new Promise(resolve => setTimeout(resolve, 100));

  const samples = [];
  const startMemory = getMemoryUsage();
  const startTime = performance.now();

  console.log('Initial Memory:');
  console.log(`  RSS: ${startMemory.rss} MB`);
  console.log(`  Heap Used: ${startMemory.heapUsed} MB`);

  // Simulate operations that allocate memory
  const data = [];
  let lastSample = Date.now();

  for (let i = 0; i < operations; i++) {
    // Simulate operation
    data.push({
      id: i,
      timestamp: Date.now(),
      payload: new Array(100).fill(Math.random()),
    });

    // Sample memory at intervals
    if (Date.now() - lastSample >= samplingInterval) {
      const currentMemory = getMemoryUsage();
      samples.push({
        operation: i,
        time: (performance.now() - startTime).toFixed(2),
        rss: parseFloat(currentMemory.rss),
        heapUsed: parseFloat(currentMemory.heapUsed),
      });
      lastSample = Date.now();
    }

    // Progress
    if ((i + 1) % Math.floor(operations / 10) === 0) {
      console.log(`  Progress: ${((i + 1) / operations * 100).toFixed(0)}%`);
    }
  }

  const endTime = performance.now();
  const endMemory = getMemoryUsage();

  // Calculate growth rate
  const totalGrowth = parseFloat(endMemory.heapUsed) - parseFloat(startMemory.heapUsed);
  const timeElapsed = (endTime - startTime) / 1000; // seconds
  const growthRate = (totalGrowth / timeElapsed).toFixed(2); // MB/s
  const memoryPerOperation = (totalGrowth / operations * 1024).toFixed(2); // KB/op

  console.log('\nFinal Memory:');
  console.log(`  RSS: ${endMemory.rss} MB`);
  console.log(`  Heap Used: ${endMemory.heapUsed} MB`);
  console.log(`\nGrowth Statistics:`);
  console.log(`  Total Growth: ${totalGrowth.toFixed(2)} MB`);
  console.log(`  Time Elapsed: ${timeElapsed.toFixed(2)}s`);
  console.log(`  Growth Rate: ${growthRate} MB/s`);
  console.log(`  Memory Per Operation: ${memoryPerOperation} KB`);

  return {
    start: startMemory,
    end: endMemory,
    totalGrowth: totalGrowth.toFixed(2),
    growthRate,
    memoryPerOperation,
    samples,
  };
}

/**
 * Detect memory leaks
 * @param {number} iterations - Number of iterations
 * @returns {Promise<Object>} Leak detection results
 */
export async function detectMemoryLeaks(iterations = 100) {
  console.log(`\n=== Memory Leak Detection (${iterations} iterations) ===`);

  const measurements = [];

  for (let i = 0; i < iterations; i++) {
    forceGC();
    await new Promise(resolve => setTimeout(resolve, 10));

    const beforeMemory = getMemoryUsage();

    // Simulate operation that might leak
    const temp = [];
    for (let j = 0; j < 1000; j++) {
      temp.push({ data: new Array(100).fill(j) });
    }

    // Clear temp (should be GC'd)
    temp.length = 0;

    forceGC();
    await new Promise(resolve => setTimeout(resolve, 10));

    const afterMemory = getMemoryUsage();

    measurements.push({
      iteration: i,
      heapUsedBefore: parseFloat(beforeMemory.heapUsed),
      heapUsedAfter: parseFloat(afterMemory.heapUsed),
      delta: (parseFloat(afterMemory.heapUsed) - parseFloat(beforeMemory.heapUsed)).toFixed(3),
    });

    if ((i + 1) % Math.floor(iterations / 10) === 0) {
      console.log(`  Progress: ${((i + 1) / iterations * 100).toFixed(0)}%`);
    }
  }

  // Analyze for leaks (increasing trend in baseline memory)
  const firstTenAvg = measurements.slice(0, 10).reduce((sum, m) => sum + m.heapUsedAfter, 0) / 10;
  const lastTenAvg = measurements.slice(-10).reduce((sum, m) => sum + m.heapUsedAfter, 0) / 10;
  const baselineGrowth = lastTenAvg - firstTenAvg;
  const leakDetected = baselineGrowth > 1; // >1MB growth suggests leak

  console.log(`\nLeak Detection Results:`);
  console.log(`  First 10 iterations avg: ${firstTenAvg.toFixed(2)} MB`);
  console.log(`  Last 10 iterations avg: ${lastTenAvg.toFixed(2)} MB`);
  console.log(`  Baseline Growth: ${baselineGrowth.toFixed(2)} MB`);
  console.log(`  Leak Detected: ${leakDetected ? '⚠ YES' : '✓ NO'}`);

  return {
    measurements,
    firstTenAvg: firstTenAvg.toFixed(2),
    lastTenAvg: lastTenAvg.toFixed(2),
    baselineGrowth: baselineGrowth.toFixed(2),
    leakDetected,
  };
}

/**
 * Measure memory usage with concurrent operations
 * @param {number} concurrency - Number of concurrent operations
 * @returns {Promise<Object>} Concurrent memory statistics
 */
export async function measureConcurrentMemory(concurrency = 100) {
  console.log(`\n=== Concurrent Memory Benchmark (${concurrency} concurrent ops) ===`);

  forceGC();
  await new Promise(resolve => setTimeout(resolve, 100));

  const startMemory = getMemoryUsage();
  console.log('Initial Memory:');
  console.log(`  RSS: ${startMemory.rss} MB`);
  console.log(`  Heap Used: ${startMemory.heapUsed} MB`);

  // Create concurrent operations
  const operations = [];
  for (let i = 0; i < concurrency; i++) {
    operations.push(
      (async () => {
        const data = new Array(1000).fill(Math.random());
        await new Promise(resolve => setTimeout(resolve, Math.random() * 100));
        return data.length;
      })()
    );
  }

  const peakMemory = getMemoryUsage();
  console.log('\nPeak Memory (during concurrent ops):');
  console.log(`  RSS: ${peakMemory.rss} MB`);
  console.log(`  Heap Used: ${peakMemory.heapUsed} MB`);

  // Wait for completion
  await Promise.all(operations);

  forceGC();
  await new Promise(resolve => setTimeout(resolve, 100));

  const endMemory = getMemoryUsage();
  console.log('\nFinal Memory (after GC):');
  console.log(`  RSS: ${endMemory.rss} MB`);
  console.log(`  Heap Used: ${endMemory.heapUsed} MB`);

  const peakDelta = (parseFloat(peakMemory.heapUsed) - parseFloat(startMemory.heapUsed)).toFixed(2);
  const memoryPerOp = (parseFloat(peakDelta) / concurrency * 1024).toFixed(2);

  console.log(`\nConcurrent Memory Statistics:`);
  console.log(`  Peak Memory Delta: ${peakDelta} MB`);
  console.log(`  Memory Per Operation: ${memoryPerOp} KB`);

  return {
    start: startMemory,
    peak: peakMemory,
    end: endMemory,
    peakDelta,
    memoryPerOp,
  };
}

/**
 * Run comprehensive memory benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runComprehensiveMemoryBenchmarks() {
  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  COMPREHENSIVE MEMORY BENCHMARK SUITE         ║');
  console.log('╚════════════════════════════════════════════════╝');

  const results = {
    startup: await measureStartupMemory(),
    growth: await measureMemoryGrowth(1000, 100),
    leaks: await detectMemoryLeaks(100),
    concurrent_100: await measureConcurrentMemory(100),
    concurrent_1000: await measureConcurrentMemory(1000),
  };

  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  MEMORY BENCHMARK SUMMARY                     ║');
  console.log('╚════════════════════════════════════════════════╝');
  console.log('\nStartup:');
  console.log(`  Initial Heap: ${results.startup.initial.heapUsed} MB`);
  console.log(`  Post-Load Heap: ${results.startup.postLoad.heapUsed} MB`);
  console.log(`  Delta: +${results.startup.delta.heapUsed} MB`);
  console.log('\nGrowth (1000 ops):');
  console.log(`  Growth Rate: ${results.growth.growthRate} MB/s`);
  console.log(`  Memory Per Op: ${results.growth.memoryPerOperation} KB`);
  console.log('\nLeak Detection:');
  console.log(`  Baseline Growth: ${results.leaks.baselineGrowth} MB`);
  console.log(`  Leak Detected: ${results.leaks.leakDetected ? '⚠ YES' : '✓ NO'}`);
  console.log('\nConcurrent (100 ops):');
  console.log(`  Peak Delta: ${results.concurrent_100.peakDelta} MB`);
  console.log(`  Memory Per Op: ${results.concurrent_100.memoryPerOp} KB`);
  console.log('\nConcurrent (1000 ops):');
  console.log(`  Peak Delta: ${results.concurrent_1000.peakDelta} MB`);
  console.log(`  Memory Per Op: ${results.concurrent_1000.memoryPerOp} KB`);

  return results;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runComprehensiveMemoryBenchmarks().catch(console.error);
}

/**
 * Resource Usage Benchmark Suite
 *
 * Monitors CPU, memory, and system resource usage during operations.
 *
 * @module benchmarks/resource-usage-benchmark
 */

import { performance } from 'perf_hooks';
import { cpuUsage } from 'process';

/**
 * Get current memory usage
 * @returns {Object} Memory statistics
 */
function getMemoryStats() {
  const usage = process.memoryUsage();
  return {
    rss: (usage.rss / 1024 / 1024).toFixed(2),
    heapTotal: (usage.heapTotal / 1024 / 1024).toFixed(2),
    heapUsed: (usage.heapUsed / 1024 / 1024).toFixed(2),
    external: (usage.external / 1024 / 1024).toFixed(2),
  };
}

/**
 * Get CPU usage delta
 * @param {Object} startUsage - Start CPU usage from process.cpuUsage()
 * @returns {Object} CPU usage statistics
 */
function getCPUStats(startUsage) {
  const endUsage = cpuUsage(startUsage);
  return {
    user: (endUsage.user / 1000).toFixed(2), // Convert to milliseconds
    system: (endUsage.system / 1000).toFixed(2),
    total: ((endUsage.user + endUsage.system) / 1000).toFixed(2),
  };
}

/**
 * Measure CPU usage during operations
 * @param {number} operations - Number of operations
 * @returns {Promise<Object>} Benchmark results
 */
export async function measureCPUUsage(operations = 1000) {
  console.log(`\n=== CPU Usage Benchmark (${operations} operations) ===`);

  const startCPU = cpuUsage();
  const startTime = performance.now();

  // Perform CPU-intensive operations
  for (let i = 0; i < operations; i++) {
    // Simulate computation
    let sum = 0;
    for (let j = 0; j < 10000; j++) {
      sum += Math.sqrt(j) * Math.random();
    }

    if ((i + 1) % Math.floor(operations / 10) === 0) {
      console.log(`  Progress: ${((i + 1) / operations * 100).toFixed(0)}%`);
    }
  }

  const totalTime = performance.now() - startTime;
  const cpuStats = getCPUStats(startCPU);

  const cpuUtilization = (parseFloat(cpuStats.total) / totalTime * 100).toFixed(2);

  console.log(`\nResults:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`  CPU User: ${cpuStats.user}ms`);
  console.log(`  CPU System: ${cpuStats.system}ms`);
  console.log(`  CPU Total: ${cpuStats.total}ms`);
  console.log(`  CPU Utilization: ${cpuUtilization}%`);

  return {
    operations,
    totalTime: totalTime.toFixed(2),
    cpu: cpuStats,
    utilization: cpuUtilization,
  };
}

/**
 * Measure memory usage during operations
 * @param {number} operations - Number of operations
 * @returns {Promise<Object>} Benchmark results
 */
export async function measureMemoryUsage(operations = 1000) {
  console.log(`\n=== Memory Usage Benchmark (${operations} operations) ===`);

  if (global.gc) {
    global.gc();
  }
  await new Promise(resolve => setTimeout(resolve, 100));

  const startMemory = getMemoryStats();
  const samples = [];

  console.log('Initial Memory:');
  console.log(`  Heap Used: ${startMemory.heapUsed} MB`);
  console.log(`  RSS: ${startMemory.rss} MB`);

  // Perform memory-allocating operations
  const data = [];
  for (let i = 0; i < operations; i++) {
    data.push({
      id: i,
      payload: new Array(1000).fill(Math.random()),
    });

    if ((i + 1) % Math.floor(operations / 10) === 0) {
      const currentMemory = getMemoryStats();
      samples.push({
        operation: i + 1,
        heapUsed: parseFloat(currentMemory.heapUsed),
        rss: parseFloat(currentMemory.rss),
      });
      console.log(`  Progress: ${((i + 1) / operations * 100).toFixed(0)}% - Heap: ${currentMemory.heapUsed} MB`);
    }
  }

  const endMemory = getMemoryStats();
  const memoryGrowth = (parseFloat(endMemory.heapUsed) - parseFloat(startMemory.heapUsed)).toFixed(2);
  const memoryPerOp = (parseFloat(memoryGrowth) / operations * 1024).toFixed(2);

  console.log(`\nFinal Memory:`);
  console.log(`  Heap Used: ${endMemory.heapUsed} MB`);
  console.log(`  RSS: ${endMemory.rss} MB`);
  console.log(`  Memory Growth: ${memoryGrowth} MB`);
  console.log(`  Memory Per Op: ${memoryPerOp} KB`);

  return {
    operations,
    startMemory,
    endMemory,
    growth: memoryGrowth,
    memoryPerOp,
    samples,
  };
}

/**
 * Identify resource hotspots
 * @param {number} iterations - Number of test iterations
 * @returns {Promise<Object>} Benchmark results
 */
export async function identifyHotspots(iterations = 100) {
  console.log(`\n=== Resource Hotspot Identification (${iterations} iterations) ===`);

  const operations = {
    'CPU-intensive': async () => {
      let sum = 0;
      for (let i = 0; i < 100000; i++) {
        sum += Math.sqrt(i) * Math.random();
      }
      return sum;
    },
    'Memory-intensive': async () => {
      const arr = new Array(10000).fill(0).map(() => Math.random());
      return arr.reduce((a, b) => a + b, 0);
    },
    'IO-intensive': async () => {
      await new Promise(resolve => setTimeout(resolve, 10));
    },
  };

  const hotspots = {};

  for (const [name, operation] of Object.entries(operations)) {
    const startCPU = cpuUsage();
    const startMemory = getMemoryStats();
    const startTime = performance.now();

    for (let i = 0; i < iterations; i++) {
      await operation();
    }

    const totalTime = performance.now() - startTime;
    const cpuStats = getCPUStats(startCPU);
    const endMemory = getMemoryStats();

    hotspots[name] = {
      time: totalTime.toFixed(2),
      cpu: cpuStats.total,
      memory: (parseFloat(endMemory.heapUsed) - parseFloat(startMemory.heapUsed)).toFixed(2),
    };

    console.log(`\n${name}:`);
    console.log(`  Time: ${totalTime.toFixed(2)}ms`);
    console.log(`  CPU: ${cpuStats.total}ms`);
    console.log(`  Memory Delta: ${hotspots[name].memory} MB`);
  }

  return hotspots;
}

/**
 * Find optimization opportunities
 * @returns {Promise<Object>} Optimization recommendations
 */
export async function findOptimizations() {
  console.log(`\n=== Optimization Opportunities ===`);

  const optimizations = [];

  // Test 1: Check memory allocation patterns
  const startMem = getMemoryStats();
  const allocations = [];

  for (let i = 0; i < 100; i++) {
    allocations.push(new Array(1000).fill(i));
  }

  const midMem = getMemoryStats();
  const allocGrowth = parseFloat(midMem.heapUsed) - parseFloat(startMem.heapUsed);

  if (allocGrowth > 10) {
    optimizations.push({
      type: 'Memory',
      severity: 'Medium',
      description: `High memory allocation rate: ${allocGrowth.toFixed(2)} MB for 100 allocations`,
      recommendation: 'Consider object pooling or preallocating buffers',
    });
  }

  // Test 2: Check CPU efficiency
  const startCPU = cpuUsage();
  const cpuStartTime = performance.now();

  for (let i = 0; i < 10000; i++) {
    Math.sqrt(i);
  }

  const cpuTime = performance.now() - cpuStartTime;
  const cpuStats = getCPUStats(startCPU);
  const cpuEfficiency = (parseFloat(cpuStats.total) / cpuTime * 100).toFixed(2);

  if (parseFloat(cpuEfficiency) < 50) {
    optimizations.push({
      type: 'CPU',
      severity: 'Low',
      description: `Low CPU efficiency: ${cpuEfficiency}%`,
      recommendation: 'Consider optimizing computation or using worker threads',
    });
  }

  console.log('\nOptimization Recommendations:');
  if (optimizations.length === 0) {
    console.log('  ✓ No major optimization opportunities identified');
  } else {
    optimizations.forEach((opt, i) => {
      console.log(`\n  ${i + 1}. [${opt.severity}] ${opt.type}:`);
      console.log(`     ${opt.description}`);
      console.log(`     → ${opt.recommendation}`);
    });
  }

  return optimizations;
}

/**
 * Run comprehensive resource usage benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runComprehensiveResourceBenchmarks() {
  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  COMPREHENSIVE RESOURCE USAGE BENCHMARK SUITE ║');
  console.log('╚════════════════════════════════════════════════╝');

  const results = {
    cpu: await measureCPUUsage(1000),
    memory: await measureMemoryUsage(1000),
    hotspots: await identifyHotspots(100),
    optimizations: await findOptimizations(),
  };

  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  RESOURCE USAGE BENCHMARK SUMMARY             ║');
  console.log('╚════════════════════════════════════════════════╝');
  console.log('\nCPU Usage (1000 operations):');
  console.log(`  CPU Total: ${results.cpu.cpu.total}ms`);
  console.log(`  Utilization: ${results.cpu.utilization}%`);
  console.log('\nMemory Usage (1000 operations):');
  console.log(`  Growth: ${results.memory.growth} MB`);
  console.log(`  Per Operation: ${results.memory.memoryPerOp} KB`);
  console.log('\nHotspots:');
  Object.entries(results.hotspots).forEach(([name, stats]) => {
    console.log(`  ${name}:`);
    console.log(`    Time: ${stats.time}ms, CPU: ${stats.cpu}ms, Memory: ${stats.memory}MB`);
  });
  console.log('\nOptimizations:');
  console.log(`  Found: ${results.optimizations.length} opportunities`);

  return results;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runComprehensiveResourceBenchmarks().catch(console.error);
}

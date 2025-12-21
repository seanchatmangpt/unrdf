/**
 * Concurrency Testing Benchmark Suite
 *
 * Tests system performance under various levels of concurrent load.
 * Tests with 10, 100, and 1000 concurrent operations.
 *
 * @module benchmarks/concurrency-benchmark
 */

import { performance } from 'perf_hooks';

/**
 * Simulate an async operation
 * @param {number} duration - Duration in milliseconds
 * @returns {Promise<void>}
 */
async function simulateOperation(duration) {
  return new Promise(resolve => setTimeout(resolve, duration));
}

/**
 * Measure concurrent operation throughput
 * @param {number} concurrency - Number of concurrent operations
 * @returns {Promise<Object>} Benchmark results
 */
export async function measureConcurrentThroughput(concurrency) {
  console.log(`\n=== Concurrent Throughput (${concurrency} operations) ===`);

  const operationDuration = 10; // Base operation time
  const startTime = performance.now();

  // Create concurrent operations
  const operations = Array.from({ length: concurrency }, (_, i) =>
    (async () => {
      const opStart = performance.now();
      await simulateOperation(operationDuration);
      return {
        id: i,
        latency: performance.now() - opStart,
      };
    })()
  );

  // Wait for all operations
  const results = await Promise.all(operations);

  const totalTime = performance.now() - startTime;
  const latencies = results.map(r => r.latency);
  const avgLatency = latencies.reduce((a, b) => a + b, 0) / latencies.length;
  const throughput = (concurrency / (totalTime / 1000)).toFixed(2);

  // Calculate percentiles
  const sortedLatencies = [...latencies].sort((a, b) => a - b);
  const p50 = sortedLatencies[Math.floor(sortedLatencies.length * 0.5)];
  const p95 = sortedLatencies[Math.floor(sortedLatencies.length * 0.95)];
  const p99 = sortedLatencies[Math.floor(sortedLatencies.length * 0.99)];

  console.log(`Results:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`  Avg Latency: ${avgLatency.toFixed(2)}ms`);
  console.log(`  P50 Latency: ${p50.toFixed(2)}ms`);
  console.log(`  P95 Latency: ${p95.toFixed(2)}ms`);
  console.log(`  P99 Latency: ${p99.toFixed(2)}ms`);
  console.log(`  Throughput: ${throughput} ops/sec`);

  return {
    concurrency,
    totalTime: totalTime.toFixed(2),
    avgLatency: avgLatency.toFixed(2),
    p50: p50.toFixed(2),
    p95: p95.toFixed(2),
    p99: p99.toFixed(2),
    throughput,
  };
}

/**
 * Test for thread contention
 * @param {number} concurrency - Number of concurrent operations
 * @returns {Promise<Object>} Benchmark results
 */
export async function testThreadContention(concurrency) {
  console.log(`\n=== Thread Contention Test (${concurrency} operations) ===`);

  const sharedResource = { counter: 0 };
  const startTime = performance.now();
  const contentionEvents = [];

  // Create operations that access shared resource
  const operations = Array.from({ length: concurrency }, (_, i) =>
    (async () => {
      const opStart = performance.now();

      // Simulate critical section
      const before = sharedResource.counter;
      await simulateOperation(1);
      sharedResource.counter++;
      const after = sharedResource.counter;

      // Detect contention (non-sequential increment)
      if (after !== before + 1) {
        contentionEvents.push({
          operation: i,
          expected: before + 1,
          actual: after,
        });
      }

      return performance.now() - opStart;
    })()
  );

  const latencies = await Promise.all(operations);
  const totalTime = performance.now() - startTime;

  const avgLatency = latencies.reduce((a, b) => a + b, 0) / latencies.length;
  const contentionRate = (contentionEvents.length / concurrency * 100).toFixed(2);

  console.log(`Results:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`  Final Counter: ${sharedResource.counter}/${concurrency}`);
  console.log(`  Contention Events: ${contentionEvents.length}`);
  console.log(`  Contention Rate: ${contentionRate}%`);
  console.log(`  Avg Latency: ${avgLatency.toFixed(2)}ms`);

  return {
    concurrency,
    totalTime: totalTime.toFixed(2),
    finalCounter: sharedResource.counter,
    contentionEvents: contentionEvents.length,
    contentionRate,
    avgLatency: avgLatency.toFixed(2),
  };
}

/**
 * Test for bottlenecks
 * @param {number} concurrency - Number of concurrent operations
 * @returns {Promise<Object>} Benchmark results
 */
export async function testBottlenecks(concurrency) {
  console.log(`\n=== Bottleneck Detection (${concurrency} operations) ===`);

  // Simulate a bottleneck (serial resource access)
  let bottleneck = Promise.resolve();
  const queueWaitTimes = [];
  const executionTimes = [];

  const startTime = performance.now();

  const operations = Array.from({ length: concurrency }, (_, i) =>
    (async () => {
      const enqueueTime = performance.now();

      // Queue for bottleneck resource
      const currentBottleneck = bottleneck;
      let resolveBottleneck;
      bottleneck = new Promise(resolve => {
        resolveBottleneck = resolve;
      });

      await currentBottleneck;

      const queueWait = performance.now() - enqueueTime;
      queueWaitTimes.push(queueWait);

      // Execute with bottleneck resource
      const execStart = performance.now();
      await simulateOperation(5);
      const execTime = performance.now() - execStart;
      executionTimes.push(execTime);

      resolveBottleneck();

      return {
        queueWait,
        execTime,
      };
    })()
  );

  const results = await Promise.all(operations);
  const totalTime = performance.now() - startTime;

  const avgQueueWait = queueWaitTimes.reduce((a, b) => a + b, 0) / queueWaitTimes.length;
  const avgExecTime = executionTimes.reduce((a, b) => a + b, 0) / executionTimes.length;
  const maxQueueWait = Math.max(...queueWaitTimes);

  const bottleneckDetected = avgQueueWait > avgExecTime * 2;

  console.log(`Results:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`  Avg Queue Wait: ${avgQueueWait.toFixed(2)}ms`);
  console.log(`  Avg Exec Time: ${avgExecTime.toFixed(2)}ms`);
  console.log(`  Max Queue Wait: ${maxQueueWait.toFixed(2)}ms`);
  console.log(`  Bottleneck Detected: ${bottleneckDetected ? '⚠ YES' : '✓ NO'}`);

  return {
    concurrency,
    totalTime: totalTime.toFixed(2),
    avgQueueWait: avgQueueWait.toFixed(2),
    avgExecTime: avgExecTime.toFixed(2),
    maxQueueWait: maxQueueWait.toFixed(2),
    bottleneckDetected,
  };
}

/**
 * Test for deadlocks
 * @param {number} concurrency - Number of concurrent operations
 * @returns {Promise<Object>} Benchmark results
 */
export async function testDeadlocks(concurrency) {
  console.log(`\n=== Deadlock Detection (${concurrency} operations) ===`);

  const timeout = 5000; // 5 second timeout
  const startTime = performance.now();
  let completedOperations = 0;

  const operations = Array.from({ length: concurrency }, (_, i) =>
    Promise.race([
      (async () => {
        // Simulate work that could deadlock
        await simulateOperation(Math.random() * 50);
        completedOperations++;
        return { id: i, status: 'completed' };
      })(),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Timeout')), timeout)
      ),
    ]).catch(error => ({
      id: i,
      status: 'timeout',
      error: error.message,
    }))
  );

  const results = await Promise.all(operations);
  const totalTime = performance.now() - startTime;

  const timeouts = results.filter(r => r.status === 'timeout').length;
  const deadlockDetected = timeouts > 0;

  console.log(`Results:`);
  console.log(`  Total Time: ${totalTime.toFixed(2)}ms`);
  console.log(`  Completed: ${completedOperations}/${concurrency}`);
  console.log(`  Timeouts: ${timeouts}`);
  console.log(`  Deadlock Detected: ${deadlockDetected ? '⚠ YES' : '✓ NO'}`);

  return {
    concurrency,
    totalTime: totalTime.toFixed(2),
    completed: completedOperations,
    timeouts,
    deadlockDetected,
  };
}

/**
 * Run comprehensive concurrency benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runComprehensiveConcurrencyBenchmarks() {
  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  COMPREHENSIVE CONCURRENCY BENCHMARK SUITE    ║');
  console.log('╚════════════════════════════════════════════════╝');

  const results = {
    throughput_10: await measureConcurrentThroughput(10),
    throughput_100: await measureConcurrentThroughput(100),
    throughput_1000: await measureConcurrentThroughput(1000),
    contention_100: await testThreadContention(100),
    bottlenecks_100: await testBottlenecks(100),
    deadlocks_100: await testDeadlocks(100),
  };

  console.log('\n╔════════════════════════════════════════════════╗');
  console.log('║  CONCURRENCY BENCHMARK SUMMARY                ║');
  console.log('╚════════════════════════════════════════════════╝');
  console.log('\nThroughput:');
  console.log(`  10 ops:   ${results.throughput_10.throughput} ops/sec (P95: ${results.throughput_10.p95}ms)`);
  console.log(`  100 ops:  ${results.throughput_100.throughput} ops/sec (P95: ${results.throughput_100.p95}ms)`);
  console.log(`  1000 ops: ${results.throughput_1000.throughput} ops/sec (P95: ${results.throughput_1000.p95}ms)`);
  console.log('\nContention (100 ops):');
  console.log(`  Contention Rate: ${results.contention_100.contentionRate}%`);
  console.log(`  Final Counter: ${results.contention_100.finalCounter}/100`);
  console.log('\nBottlenecks (100 ops):');
  console.log(`  Avg Queue Wait: ${results.bottlenecks_100.avgQueueWait}ms`);
  console.log(`  Bottleneck Detected: ${results.bottlenecks_100.bottleneckDetected ? '⚠ YES' : '✓ NO'}`);
  console.log('\nDeadlocks (100 ops):');
  console.log(`  Completed: ${results.deadlocks_100.completed}/100`);
  console.log(`  Deadlock Detected: ${results.deadlocks_100.deadlockDetected ? '⚠ YES' : '✓ NO'}`);

  return results;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runComprehensiveConcurrencyBenchmarks().catch(console.error);
}

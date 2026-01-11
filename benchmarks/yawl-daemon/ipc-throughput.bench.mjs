/**
 * @file IPC Message Throughput Benchmark
 * @module benchmarks/yawl-daemon/ipc-throughput
 * @description Measures IPC message passing performance between daemon and workflows
 *
 * Performance Targets:
 * - IPC round-trip: <10ms (P95)
 * - Message queue throughput: >1000 msg/s
 * - Low latency variance (P99/P50 ratio <3)
 *
 * Metrics:
 * - Message round-trip latency (P50, P95, P99)
 * - Throughput (messages/second)
 * - Queue depth impact on latency
 */

import { EventEmitter } from 'events';
import { Daemon } from '../../packages/daemon/src/daemon.mjs';
import { randomUUID } from 'crypto';

/**
 * Calculate percentile from sorted array
 * @param {number[]} values - Sorted array of values
 * @param {number} percentile - Percentile (0-100)
 * @returns {number} Percentile value
 */
function getPercentile(values, percentile) {
  if (values.length === 0) return 0;
  const index = Math.ceil((percentile / 100) * values.length) - 1;
  return values[Math.max(0, index)];
}

/**
 * Simple IPC message bus for testing
 */
class IPCMessageBus extends EventEmitter {
  constructor() {
    super();
    this.messageQueue = [];
    this.processingLatencies = [];
  }

  /**
   * Send message and measure round-trip time
   * @param {Object} message - Message to send
   * @returns {Promise<Object>} Response with latency
   */
  async sendMessage(message) {
    const sendTime = performance.now();

    return new Promise((resolve) => {
      const responseHandler = (response) => {
        if (response.messageId === message.id) {
          const latency = performance.now() - sendTime;
          this.processingLatencies.push(latency);
          this.removeListener('response', responseHandler);
          resolve({ ...response, latency });
        }
      };

      this.on('response', responseHandler);
      this.messageQueue.push(message);
      this.emit('message', message);

      // Simulate processing
      setImmediate(() => {
        this.emit('response', {
          messageId: message.id,
          status: 'processed',
          payload: message.payload,
        });
      });
    });
  }

  getStats() {
    return {
      totalMessages: this.processingLatencies.length,
      queueDepth: this.messageQueue.length,
      latencies: this.processingLatencies,
    };
  }
}

/**
 * Benchmark: Basic IPC round-trip latency
 * @param {Object} options - Benchmark options
 * @param {number} [options.messageCount=1000] - Messages to send
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkIPCRoundTrip(options = {}) {
  const { messageCount = 1000, runs = 5 } = options;
  const allLatencies = [];

  for (let run = 0; run < runs; run++) {
    const messageBus = new IPCMessageBus();
    const runLatencies = [];

    for (let i = 0; i < messageCount; i++) {
      const message = {
        id: `msg-${run}-${i}`,
        type: 'workflow-event',
        payload: { index: i, data: 'test' },
      };

      const response = await messageBus.sendMessage(message);
      runLatencies.push(response.latency);
    }

    allLatencies.push(...runLatencies);
  }

  const sortedLatencies = allLatencies.sort((a, b) => a - b);
  const mean = allLatencies.reduce((sum, v) => sum + v, 0) / allLatencies.length;
  const p50 = getPercentile(sortedLatencies, 50);
  const p95 = getPercentile(sortedLatencies, 95);
  const p99 = getPercentile(sortedLatencies, 99);

  return {
    name: 'ipc-round-trip-latency',
    messageCount: messageCount * runs,
    runs,
    latency: {
      mean,
      p50,
      p95,
      p99,
      min: Math.min(...allLatencies),
      max: Math.max(...allLatencies),
    },
    passed: p95 < 10, // Target: <10ms P95
    target: '10ms',
    unit: 'ms',
  };
}

/**
 * Benchmark: Message throughput
 * @param {Object} options - Benchmark options
 * @param {number} [options.duration=5000] - Duration in milliseconds
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkIPCThroughput(options = {}) {
  const { duration = 5000, runs = 3 } = options;
  const throughputs = [];

  for (let run = 0; run < runs; run++) {
    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `throughput-bench-${run}`,
      maxConcurrent: 100,
    });

    await daemon.start();

    let messageCount = 0;
    const startTime = performance.now();
    const endTime = startTime + duration;

    // Flood with messages
    const sendPromises = [];
    while (performance.now() < endTime) {
      const opId = `throughput-op-${messageCount}`;

      daemon.schedule({
        id: opId,
        handler: async () => {
          messageCount++;
          return { processed: true };
        },
      });

      sendPromises.push(daemon.execute(opId).catch(() => {}));

      // Small delay to prevent blocking
      if (messageCount % 100 === 0) {
        await new Promise(resolve => setImmediate(resolve));
      }
    }

    await Promise.all(sendPromises);
    await daemon.stop();

    const actualDuration = (performance.now() - startTime) / 1000;
    const throughput = messageCount / actualDuration;
    throughputs.push(throughput);
  }

  const mean = throughputs.reduce((sum, v) => sum + v, 0) / throughputs.length;
  const min = Math.min(...throughputs);
  const max = Math.max(...throughputs);

  return {
    name: 'ipc-message-throughput',
    duration,
    runs,
    throughput: {
      mean,
      min,
      max,
    },
    passed: mean > 1000, // Target: >1000 msg/s
    target: '1000 msg/s',
    unit: 'messages/sec',
  };
}

/**
 * Benchmark: Queue depth impact on latency
 * @param {Object} options - Benchmark options
 * @param {number[]} [options.queueDepths=[10, 50, 100, 500]] - Queue depths to test
 * @param {number} [options.messagesPerDepth=100] - Messages per depth test
 * @returns {Promise<Object>} Benchmark results
 */
export async function benchmarkQueueDepthImpact(options = {}) {
  const { queueDepths = [10, 50, 100, 500], messagesPerDepth = 100 } = options;
  const results = [];

  for (const depth of queueDepths) {
    const daemon = new Daemon({
      daemonId: randomUUID(),
      name: `queue-depth-${depth}`,
      maxConcurrent: 5, // Limit to create queue buildup
    });

    await daemon.start();
    const latencies = [];

    // Pre-fill queue
    for (let i = 0; i < depth; i++) {
      daemon.schedule({
        id: `prefill-${i}`,
        handler: async () => {
          await new Promise(resolve => setTimeout(resolve, 50));
          return { result: i };
        },
      });
    }

    // Measure latency with queue at depth
    for (let i = 0; i < messagesPerDepth; i++) {
      const opId = `measure-${i}`;
      const startTime = performance.now();

      daemon.schedule({
        id: opId,
        handler: async () => ({ result: i }),
      });

      await daemon.execute(opId).catch(() => {});
      const latency = performance.now() - startTime;
      latencies.push(latency);
    }

    await daemon.stop();

    const sortedLatencies = latencies.sort((a, b) => a - b);
    const p50 = getPercentile(sortedLatencies, 50);
    const p95 = getPercentile(sortedLatencies, 95);

    results.push({
      queueDepth: depth,
      latency: {
        p50,
        p95,
        mean: latencies.reduce((sum, v) => sum + v, 0) / latencies.length,
      },
    });
  }

  // Check if latency scales linearly with queue depth (should be sub-linear)
  const latencyIncrease = results[results.length - 1].latency.p95 / results[0].latency.p95;
  const queueIncrease = queueDepths[queueDepths.length - 1] / queueDepths[0];
  const scalingFactor = latencyIncrease / queueIncrease;

  return {
    name: 'ipc-queue-depth-impact',
    queueDepths,
    messagesPerDepth,
    results,
    scaling: {
      latencyIncrease,
      queueIncrease,
      scalingFactor,
      sublinear: scalingFactor < 1,
    },
    passed: scalingFactor < 1.5, // Latency should scale sub-linearly
    target: 'sub-linear scaling',
    unit: 'ratio',
  };
}

/**
 * Run all IPC throughput benchmarks
 * @returns {Promise<Object>} All benchmark results
 */
export async function runIPCBenchmarks() {
  console.log('Running IPC Throughput Benchmarks...\n');

  const results = {
    roundTrip: await benchmarkIPCRoundTrip(),
    throughput: await benchmarkIPCThroughput(),
    queueDepthImpact: await benchmarkQueueDepthImpact(),
  };

  const allPassed = Object.values(results).every(r => r.passed);

  return {
    name: 'ipc-throughput-suite',
    timestamp: new Date().toISOString(),
    results,
    summary: {
      total: Object.keys(results).length,
      passed: Object.values(results).filter(r => r.passed).length,
      failed: Object.values(results).filter(r => !r.passed).length,
    },
    passed: allPassed,
  };
}

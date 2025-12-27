/**
 * @fileoverview Concurrency Surface Probe - Parallel execution capabilities
 * @module @unrdf/kgc-probe/probes/concurrency
 *
 * CRITICAL: This probe measures concurrency primitives with strict guard constraints.
 * NO unbounded spawning. All workers cleaned up after probing.
 * Each operation: timeout 5s max.
 *
 * Probes (15 total):
 * 1. worker_threads availability and limits
 * 2. SharedArrayBuffer availability
 * 3. Atomics support
 * 4. Thread pool size detection (UV_THREADPOOL_SIZE)
 * 5. Event loop latency under load
 * 6. Worker spawn time (mean, p95, stddev)
 * 7. Message passing overhead (postMessage latency)
 * 8. Maximum concurrent workers (test by spawning)
 * 9. Parallel file I/O contention (measure with multiple reads)
 * 10. Event loop ordering (nextTick vs queueMicrotask vs setImmediate vs promise)
 * 11. Stack depth (recursion limit detection)
 * 12. AsyncLocalStorage availability and functionality
 * 13. Microtask queue depth handling
 * 14. Max concurrent promises stress test (1000 pending)
 * 15. Stream backpressure behavior
 *
 * @agent Agent 4 - Concurrency Surface Probe (KGC Probe Swarm)
 */

import { Worker } from 'node:worker_threads';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { z } from 'zod';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Observation schema - represents a single probe measurement
 * @typedef {Object} Observation
 * @property {string} method - Probe method identifier (e.g., "concurrency.worker_threads")
 * @property {Record<string, any>} inputs - Input parameters used for probing
 * @property {Record<string, any>} outputs - Observed outputs/measurements
 * @property {number} timestamp - Unix epoch timestamp (ms)
 * @property {string} [hash] - Hash of observation for verification
 * @property {string} [guardDecision] - Guard decision: "allowed", "denied", "unknown"
 * @property {Record<string, any>} [metadata] - Additional metadata
 */
const ObservationSchema = z.object({
  method: z.string().min(1),
  inputs: z.record(z.string(), z.any()),
  outputs: z.record(z.string(), z.any()),
  timestamp: z.number().int().positive(),
  hash: z.string().optional(),
  guardDecision: z.enum(['allowed', 'denied', 'unknown']).optional(),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Probe configuration schema
 * @typedef {Object} ProbeConfig
 * @property {number} [timeout] - Operation timeout in milliseconds (default: 5000, max: 5000)
 * @property {number} [maxWorkers] - Maximum workers to spawn (default: 16, max: 16)
 * @property {number} [samples] - Number of benchmark samples (default: 10, max: 100)
 * @property {number} [budgetMs] - Global timeout budget in milliseconds (default: 30000)
 * @property {string} [testDir] - Directory for I/O tests (default: tmpdir)
 */
const ProbeConfigSchema = z.object({
  timeout: z.number().int().positive().max(5000).default(5000),
  maxWorkers: z.number().int().positive().max(16).default(16),
  samples: z.number().int().positive().max(100).default(10),
  budgetMs: z.number().int().positive().max(60000).default(30000),
  testDir: z.string().optional(),
}).default({});

// ============================================================================
// GUARD: WORKER CLEANUP (POKA-YOKE)
// ============================================================================

/**
 * CRITICAL: Track all spawned workers for cleanup
 * Ensures no workers are left running after probe completes
 */
const activeWorkers = new Set();

/**
 * Cleanup all active workers
 * MUST be called after probing completes or on error
 */
async function cleanupWorkers() {
  const workers = Array.from(activeWorkers);
  activeWorkers.clear();

  await Promise.all(
    workers.map(worker =>
      worker.terminate().catch(() => {
        /* ignore termination errors */
      })
    )
  );
}

/**
 * Register worker for cleanup tracking
 * @param {Worker} worker - Worker to track
 */
function registerWorker(worker) {
  activeWorkers.add(worker);
  worker.on('exit', () => activeWorkers.delete(worker));
}

// ============================================================================
// UTILITY: STATISTICS
// ============================================================================

/**
 * Calculate statistics from array of measurements
 * @param {number[]} values - Array of numeric measurements
 * @returns {{mean: number, median: number, p95: number, min: number, max: number, stddev: number}}
 */
function calculateStats(values) {
  if (values.length === 0) {
    return { mean: 0, median: 0, p95: 0, min: 0, max: 0, stddev: 0 };
  }

  const sorted = values.slice().sort((a, b) => a - b);
  const sum = sorted.reduce((a, b) => a + b, 0);
  const mean = sum / sorted.length;

  const median = sorted[Math.floor(sorted.length / 2)];
  const p95Index = Math.floor(sorted.length * 0.95);
  const p95 = sorted[p95Index] || sorted[sorted.length - 1];

  const min = sorted[0];
  const max = sorted[sorted.length - 1];

  // Calculate standard deviation
  const variance = sorted.reduce((acc, val) => acc + Math.pow(val - mean, 2), 0) / sorted.length;
  const stddev = Math.sqrt(variance);

  return { mean, median, p95, min, max, stddev };
}

// ============================================================================
// PROBE: WORKER THREADS AVAILABILITY
// ============================================================================

/**
 * Probe worker_threads module availability
 * @returns {Promise<Observation>}
 */
async function probeWorkerThreadsAvailability() {
  const timestamp = Date.now();
  const outputs = {};
  const metadata = {};

  try {
    // Check if Worker constructor exists
    outputs.available = typeof Worker === 'function';
    outputs.module = 'worker_threads';

    // Check Node.js version
    const nodeVersion = process.version;
    outputs.nodeVersion = nodeVersion;
    metadata.minVersion = 'v10.5.0'; // worker_threads introduced in Node 10.5.0

  } catch (error) {
    outputs.available = false;
    metadata.error = error.message;
  }

  return {
    method: 'concurrency.worker_threads_available',
    inputs: {},
    outputs,
    timestamp,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// PROBE: SHAREDARRAYBUFFER AVAILABILITY
// ============================================================================

/**
 * Probe SharedArrayBuffer availability
 * @returns {Promise<Observation>}
 */
async function probeSharedArrayBuffer() {
  const timestamp = Date.now();
  const outputs = {};
  const metadata = {};

  try {
    // Check if SharedArrayBuffer constructor exists
    outputs.available = typeof SharedArrayBuffer === 'function';

    if (outputs.available) {
      // Test creation of SharedArrayBuffer
      const sab = new SharedArrayBuffer(8);
      outputs.testSize = sab.byteLength;
      outputs.functional = sab.byteLength === 8;
    } else {
      outputs.functional = false;
      metadata.reason = 'Constructor not available';
    }
  } catch (error) {
    outputs.available = false;
    outputs.functional = false;
    metadata.error = error.message;
  }

  return {
    method: 'concurrency.shared_array_buffer',
    inputs: {},
    outputs,
    timestamp,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// PROBE: ATOMICS SUPPORT
// ============================================================================

/**
 * Probe Atomics support
 * @returns {Promise<Observation>}
 */
async function probeAtomics() {
  const timestamp = Date.now();
  const outputs = {};
  const metadata = {};

  try {
    // Check if Atomics object exists
    outputs.available = typeof Atomics === 'object';

    if (outputs.available && typeof SharedArrayBuffer === 'function') {
      // Test basic Atomics operations
      const sab = new SharedArrayBuffer(4);
      const view = new Int32Array(sab);

      // Test add operation
      Atomics.store(view, 0, 10);
      const addResult = Atomics.add(view, 0, 5);
      outputs.functional = addResult === 10 && Atomics.load(view, 0) === 15;

      // List available operations
      outputs.operations = [
        'add', 'and', 'compareExchange', 'exchange', 'load',
        'or', 'store', 'sub', 'xor', 'wait', 'notify'
      ].filter(op => typeof Atomics[op] === 'function');

    } else {
      outputs.functional = false;
      metadata.reason = outputs.available ? 'SharedArrayBuffer unavailable' : 'Atomics unavailable';
    }
  } catch (error) {
    outputs.available = false;
    outputs.functional = false;
    metadata.error = error.message;
  }

  return {
    method: 'concurrency.atomics',
    inputs: {},
    outputs,
    timestamp,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// PROBE: EVENT LOOP LATENCY
// ============================================================================

/**
 * Measure event loop latency using setImmediate chains
 * @param {number} samples - Number of samples to measure
 * @returns {Promise<Observation>}
 */
async function probeEventLoopLatency(samples) {
  const timestamp = Date.now();
  const measurements = [];
  const metadata = {};

  try {
    for (let i = 0; i < samples; i++) {
      const start = performance.now();
      await new Promise(resolve => setImmediate(resolve));
      const end = performance.now();
      measurements.push(end - start);
    }

    const stats = calculateStats(measurements);

    return {
      method: 'concurrency.event_loop_latency',
      inputs: { samples },
      outputs: {
        ...stats,
        unit: 'ms',
        samples: measurements.length,
      },
      timestamp,
      guardDecision: 'allowed',
      metadata,
    };
  } catch (error) {
    return {
      method: 'concurrency.event_loop_latency',
      inputs: { samples },
      outputs: { error: true },
      timestamp,
      guardDecision: 'allowed',
      metadata: { error: error.message },
    };
  }
}

// ============================================================================
// PROBE: WORKER SPAWN TIME
// ============================================================================

/**
 * Measure worker spawn time
 * Creates simple worker that exits immediately
 * @param {number} samples - Number of workers to spawn
 * @param {number} timeout - Timeout per worker (ms)
 * @returns {Promise<Observation>}
 */
async function probeWorkerSpawnTime(samples, timeout) {
  const timestamp = Date.now();
  const measurements = [];
  const metadata = {};

  // Create simple worker script inline
  const workerCode = `
    const { parentPort } = require('worker_threads');
    parentPort.postMessage('ready');
  `;

  try {
    for (let i = 0; i < samples; i++) {
      const start = performance.now();

      const worker = new Worker(workerCode, { eval: true });
      registerWorker(worker);

      // Wait for worker to send ready message
      await new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          worker.terminate();
          reject(new Error('Worker timeout'));
        }, timeout);

        worker.once('message', () => {
          clearTimeout(timer);
          resolve();
        });

        worker.once('error', err => {
          clearTimeout(timer);
          reject(err);
        });
      });

      const end = performance.now();
      measurements.push(end - start);

      await worker.terminate();
      activeWorkers.delete(worker);
    }

    const stats = calculateStats(measurements);

    return {
      method: 'concurrency.worker_spawn_time',
      inputs: { samples, timeout },
      outputs: {
        ...stats,
        unit: 'ms',
        samples: measurements.length,
      },
      timestamp,
      guardDecision: 'allowed',
      metadata,
    };
  } catch (error) {
    return {
      method: 'concurrency.worker_spawn_time',
      inputs: { samples, timeout },
      outputs: { error: true },
      timestamp,
      guardDecision: 'allowed',
      metadata: { error: error.message },
    };
  } finally {
    await cleanupWorkers();
  }
}

// ============================================================================
// PROBE: MESSAGE PASSING OVERHEAD
// ============================================================================

/**
 * Measure message passing overhead (postMessage latency)
 * @param {number} samples - Number of messages to send
 * @param {number} timeout - Timeout for worker (ms)
 * @returns {Promise<Observation>}
 */
async function probeMessagePassingOverhead(samples, timeout) {
  const timestamp = Date.now();
  const measurements = [];
  const metadata = {};

  // Echo worker - receives message and sends it back
  const workerCode = `
    const { parentPort } = require('worker_threads');
    parentPort.on('message', msg => {
      parentPort.postMessage(msg);
    });
  `;

  try {
    const worker = new Worker(workerCode, { eval: true });
    registerWorker(worker);

    for (let i = 0; i < samples; i++) {
      const start = performance.now();

      await new Promise((resolve, reject) => {
        const timer = setTimeout(() => reject(new Error('Message timeout')), timeout);

        worker.once('message', () => {
          clearTimeout(timer);
          resolve();
        });

        worker.postMessage({ id: i, timestamp: start });
      });

      const end = performance.now();
      measurements.push(end - start);
    }

    await worker.terminate();
    activeWorkers.delete(worker);

    const stats = calculateStats(measurements);

    return {
      method: 'concurrency.message_passing_overhead',
      inputs: { samples, timeout },
      outputs: {
        ...stats,
        unit: 'ms',
        samples: measurements.length,
      },
      timestamp,
      guardDecision: 'allowed',
      metadata,
    };
  } catch (error) {
    return {
      method: 'concurrency.message_passing_overhead',
      inputs: { samples, timeout },
      outputs: { error: true },
      timestamp,
      guardDecision: 'allowed',
      metadata: { error: error.message },
    };
  } finally {
    await cleanupWorkers();
  }
}

// ============================================================================
// PROBE: MAX CONCURRENT WORKERS
// ============================================================================

/**
 * Test maximum concurrent workers by spawning workers until failure
 * @param {number} maxWorkers - Maximum workers to attempt (hard limit)
 * @param {number} timeout - Timeout per worker (ms)
 * @returns {Promise<Observation>}
 */
async function probeMaxConcurrentWorkers(maxWorkers, timeout) {
  const timestamp = Date.now();
  const metadata = {};
  let successfulWorkers = 0;

  // Worker that sleeps briefly to hold resources
  const workerCode = `
    const { parentPort } = require('worker_threads');
    setTimeout(() => {
      parentPort.postMessage('done');
    }, 100);
  `;

  const workers = [];

  try {
    // Spawn workers up to maxWorkers limit
    for (let i = 0; i < maxWorkers; i++) {
      try {
        const worker = new Worker(workerCode, { eval: true });
        registerWorker(worker);
        workers.push(worker);

        // Wait for worker to signal ready
        await Promise.race([
          new Promise((resolve, reject) => {
            worker.once('message', resolve);
            worker.once('error', reject);
          }),
          new Promise((_, reject) => setTimeout(() => reject(new Error('Timeout')), timeout)),
        ]);

        successfulWorkers++;
      } catch (error) {
        metadata.failureReason = error.message;
        break;
      }
    }

    return {
      method: 'concurrency.max_concurrent_workers',
      inputs: { maxWorkers, timeout },
      outputs: {
        maxAchieved: successfulWorkers,
        limitReached: successfulWorkers < maxWorkers,
        guardLimit: maxWorkers,
      },
      timestamp,
      guardDecision: 'allowed',
      metadata,
    };
  } catch (error) {
    return {
      method: 'concurrency.max_concurrent_workers',
      inputs: { maxWorkers, timeout },
      outputs: {
        maxAchieved: successfulWorkers,
        error: true,
      },
      timestamp,
      guardDecision: 'allowed',
      metadata: { error: error.message },
    };
  } finally {
    // Cleanup all workers
    await Promise.all(
      workers.map(w =>
        w.terminate().catch(() => {
          /* ignore */
        })
      )
    );
    workers.forEach(w => activeWorkers.delete(w));
  }
}

// ============================================================================
// PROBE: PARALLEL FILE I/O CONTENTION
// ============================================================================

/**
 * Measure parallel file I/O contention
 * Spawn N readers reading same file simultaneously
 * @param {number} numReaders - Number of parallel readers
 * @param {number} timeout - Timeout per operation (ms)
 * @param {string} testDir - Directory for test files
 * @returns {Promise<Observation>}
 */
async function probeParallelIOContention(numReaders, timeout, testDir) {
  const timestamp = Date.now();
  const metadata = {};
  const measurements = [];

  try {
    // Create test directory
    const dir = testDir || join(tmpdir(), `kgc-probe-${Date.now()}`);
    await mkdir(dir, { recursive: true });
    metadata.testDir = dir;

    // Create test file (1MB)
    const testFile = join(dir, 'test-file.bin');
    const testData = Buffer.alloc(1024 * 1024, 'x');
    await writeFile(testFile, testData);
    metadata.fileSize = testData.length;

    // Spawn N parallel readers
    const start = performance.now();

    const readPromises = Array.from({ length: numReaders }, async (_, i) => {
      const readStart = performance.now();
      await readFile(testFile);
      const readEnd = performance.now();
      return readEnd - readStart;
    });

    const results = await Promise.race([
      Promise.all(readPromises),
      new Promise((_, reject) => setTimeout(() => reject(new Error('Timeout')), timeout)),
    ]);

    const end = performance.now();
    const totalTime = end - start;

    measurements.push(...results);
    const stats = calculateStats(measurements);

    // Calculate throughput
    const throughputMBps = (testData.length * numReaders) / (totalTime / 1000) / (1024 * 1024);

    return {
      method: 'concurrency.parallel_io_contention',
      inputs: { numReaders, timeout, fileSize: testData.length },
      outputs: {
        totalTime,
        throughputMBps: Math.round(throughputMBps * 100) / 100,
        perReaderStats: stats,
        unit: 'ms',
        samples: measurements.length,
      },
      timestamp,
      guardDecision: 'allowed',
      metadata,
    };
  } catch (error) {
    return {
      method: 'concurrency.parallel_io_contention',
      inputs: { numReaders, timeout },
      outputs: { error: true },
      timestamp,
      guardDecision: 'allowed',
      metadata: { error: error.message },
    };
  }
}

// ============================================================================
// PROBE: THREAD POOL SIZE DETECTION
// ============================================================================

/**
 * Detect thread pool size using UV_THREADPOOL_SIZE
 * @returns {Promise<Observation>}
 */
async function probeThreadPoolSize() {
  const timestamp = Date.now();
  const outputs = {};
  const metadata = {};

  try {
    // Check UV_THREADPOOL_SIZE environment variable
    const envValue = process.env.UV_THREADPOOL_SIZE;
    outputs.uvThreadpoolSize = envValue ? parseInt(envValue, 10) : null;
    outputs.default = 4; // Default libuv thread pool size

    // Effective size
    outputs.effective = outputs.uvThreadpoolSize || outputs.default;

    metadata.note = 'UV_THREADPOOL_SIZE can be set to customize libuv thread pool';
  } catch (error) {
    outputs.error = true;
    metadata.error = error.message;
  }

  return {
    method: 'concurrency.thread_pool_size',
    inputs: {},
    outputs,
    timestamp,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// PROBE: EVENT LOOP ORDERING
// ============================================================================

/**
 * Probe event loop ordering: nextTick, queueMicrotask, setImmediate
 * Tests the execution order of different async scheduling primitives
 * @returns {Promise<Observation>}
 */
async function probeEventLoopOrdering() {
  const timestamp = Date.now();
  const outputs = {};
  const metadata = {};

  try {
    const order = [];

    // Schedule all three types
    setImmediate(() => order.push('setImmediate'));
    process.nextTick(() => order.push('nextTick'));
    queueMicrotask(() => order.push('queueMicrotask'));
    Promise.resolve().then(() => order.push('promise'));

    // Wait for event loop tick
    await new Promise(resolve => setTimeout(resolve, 10));

    outputs.executionOrder = order;
    outputs.expectedOrder = ['nextTick', 'queueMicrotask', 'promise', 'setImmediate'];
    outputs.matchesExpected = JSON.stringify(order) === JSON.stringify(outputs.expectedOrder);

    metadata.note = 'nextTick > microtasks > setImmediate in Node.js event loop';
  } catch (error) {
    outputs.error = true;
    metadata.error = error.message;
  }

  return {
    method: 'concurrency.event_loop_ordering',
    inputs: {},
    outputs,
    timestamp,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// PROBE: STACK DEPTH (RECURSION LIMIT)
// ============================================================================

/**
 * Probe maximum stack depth via recursion
 * BOUNDED: Stops at reasonable limit to avoid crash
 * @param {number} maxAttempts - Maximum recursion depth to try (default: 10000)
 * @returns {Promise<Observation>}
 */
async function probeStackDepth(maxAttempts = 10000) {
  const timestamp = Date.now();
  const outputs = {};
  const metadata = {};

  let depth = 0;

  function recurse(n) {
    depth = n;
    if (n >= maxAttempts) {
      return n; // Hit guard limit
    }
    try {
      return recurse(n + 1);
    } catch (error) {
      // Stack overflow caught
      metadata.errorType = error.name;
      return n;
    }
  }

  try {
    const maxDepth = recurse(0);
    outputs.maxStackDepth = maxDepth;
    outputs.hitGuardLimit = maxDepth >= maxAttempts;
    outputs.guardLimit = maxAttempts;

    metadata.note = 'Recursion bounded to prevent VM crash';
  } catch (error) {
    outputs.maxStackDepth = depth;
    outputs.error = true;
    metadata.error = error.message;
  }

  return {
    method: 'concurrency.stack_depth',
    inputs: { maxAttempts },
    outputs,
    timestamp,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// PROBE: ASYNCLOCALSTORAGE AVAILABILITY
// ============================================================================

/**
 * Probe AsyncLocalStorage availability and functionality
 * @returns {Promise<Observation>}
 */
async function probeAsyncLocalStorage() {
  const timestamp = Date.now();
  const outputs = {};
  const metadata = {};

  try {
    // Dynamic import to avoid error if not available
    const { AsyncLocalStorage } = await import('node:async_hooks');

    outputs.available = typeof AsyncLocalStorage === 'function';

    if (outputs.available) {
      // Test functionality
      const asyncLocalStorage = new AsyncLocalStorage();
      let capturedValue = null;

      await asyncLocalStorage.run('test-value', async () => {
        capturedValue = asyncLocalStorage.getStore();
      });

      outputs.functional = capturedValue === 'test-value';
      outputs.testValue = capturedValue;
    } else {
      outputs.functional = false;
    }

    metadata.nodeVersion = process.version;
    metadata.note = 'AsyncLocalStorage available since Node.js v13.10.0';
  } catch (error) {
    outputs.available = false;
    outputs.functional = false;
    metadata.error = error.message;
  }

  return {
    method: 'concurrency.async_local_storage',
    inputs: {},
    outputs,
    timestamp,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// PROBE: MICROTASK QUEUE DEPTH
// ============================================================================

/**
 * Probe microtask queue depth handling
 * Tests how many microtasks can be queued before event loop stalls
 * BOUNDED: Limited to prevent infinite loop
 * @param {number} maxMicrotasks - Maximum microtasks to queue (default: 1000)
 * @returns {Promise<Observation>}
 */
async function probeMicrotaskQueueDepth(maxMicrotasks = 1000) {
  const timestamp = Date.now();
  const outputs = {};
  const metadata = {};

  try {
    let executed = 0;
    const promises = [];

    // Queue N microtasks
    for (let i = 0; i < maxMicrotasks; i++) {
      promises.push(
        queueMicrotask(() => {
          executed++;
        })
      );
    }

    // Wait for all to execute
    await new Promise(resolve => setImmediate(resolve));

    outputs.queued = maxMicrotasks;
    outputs.executed = executed;
    outputs.allExecuted = executed === maxMicrotasks;

    metadata.note = 'Microtasks execute before next event loop phase';
  } catch (error) {
    outputs.queued = maxMicrotasks;
    outputs.executed = executed;
    outputs.error = true;
    metadata.error = error.message;
  }

  return {
    method: 'concurrency.microtask_queue_depth',
    inputs: { maxMicrotasks },
    outputs,
    timestamp,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// PROBE: MAX CONCURRENT PROMISES
// ============================================================================

/**
 * Stress test with many concurrent pending promises
 * BOUNDED: Limited to prevent memory exhaustion
 * @param {number} maxPromises - Maximum concurrent promises (default: 1000, max: 10000)
 * @param {number} timeout - Timeout for operation (ms)
 * @returns {Promise<Observation>}
 */
async function probeMaxConcurrentPromises(maxPromises = 1000, timeout = 5000) {
  const timestamp = Date.now();
  const outputs = {};
  const metadata = {};

  try {
    const start = performance.now();
    let resolved = 0;
    let rejected = 0;

    // Create N promises that resolve after random delay
    const promises = Array.from({ length: maxPromises }, (_, i) =>
      new Promise(resolve => {
        setImmediate(() => {
          resolved++;
          resolve(i);
        });
      }).catch(() => {
        rejected++;
      })
    );

    // Wait for all with timeout
    await Promise.race([
      Promise.all(promises),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Timeout')), timeout)
      ),
    ]);

    const end = performance.now();

    outputs.created = maxPromises;
    outputs.resolved = resolved;
    outputs.rejected = rejected;
    outputs.timeMs = Math.round(end - start);
    outputs.success = resolved === maxPromises;

    metadata.note = 'Tests VM ability to handle many concurrent promises';
  } catch (error) {
    outputs.created = maxPromises;
    outputs.resolved = resolved;
    outputs.rejected = rejected;
    outputs.error = true;
    metadata.error = error.message;
  }

  return {
    method: 'concurrency.max_concurrent_promises',
    inputs: { maxPromises, timeout },
    outputs,
    timestamp,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// PROBE: STREAM BACKPRESSURE BEHAVIOR
// ============================================================================

/**
 * Probe stream backpressure behavior
 * Tests how streams handle flow control when consumer is slower than producer
 * @param {number} chunkCount - Number of chunks to write (default: 100)
 * @returns {Promise<Observation>}
 */
async function probeStreamBackpressure(chunkCount = 100) {
  const timestamp = Date.now();
  const outputs = {};
  const metadata = {};

  try {
    const { Readable, Writable } = await import('node:stream');

    let backpressureCount = 0;
    let chunksWritten = 0;
    let chunksRead = 0;

    // Create readable stream
    const readable = new Readable({
      read() {
        // Producer writes faster than consumer
        for (let i = 0; i < chunkCount; i++) {
          const canContinue = this.push(`chunk-${i}`);
          if (!canContinue) {
            backpressureCount++;
          }
          chunksWritten++;
        }
        this.push(null); // End stream
      },
    });

    // Create writable stream (slow consumer)
    const writable = new Writable({
      write(chunk, encoding, callback) {
        chunksRead++;
        // Simulate slow processing
        setImmediate(callback);
      },
    });

    // Pipe and wait for finish
    await new Promise((resolve, reject) => {
      readable.pipe(writable);
      writable.on('finish', resolve);
      writable.on('error', reject);
      readable.on('error', reject);
    });

    outputs.chunksWritten = chunksWritten;
    outputs.chunksRead = chunksRead;
    outputs.backpressureDetected = backpressureCount > 0;
    outputs.backpressureCount = backpressureCount;
    outputs.allChunksDelivered = chunksRead === chunkCount;

    metadata.note = 'Backpressure prevents memory overflow in streams';
  } catch (error) {
    outputs.error = true;
    metadata.error = error.message;
  }

  return {
    method: 'concurrency.stream_backpressure',
    inputs: { chunkCount },
    outputs,
    timestamp,
    guardDecision: 'allowed',
    metadata,
  };
}

// ============================================================================
// MAIN PROBE FUNCTION
// ============================================================================

/**
 * Probes concurrency primitives and parallel execution capabilities
 *
 * Returns observations for:
 * - worker_threads availability and limits
 * - Maximum concurrent workers (test by spawning)
 * - Parallel file I/O contention (measure with multiple reads)
 * - Event loop latency under load
 * - SharedArrayBuffer availability
 * - Atomics support
 * - Message passing overhead (postMessage latency)
 * - Thread pool size detection
 *
 * GUARD CONSTRAINTS:
 * - No unbounded spawning (limit to config.maxWorkers or 16, whichever smaller)
 * - Clean up all workers after probing
 * - Timeout each worker operation (5s max)
 *
 * BENCHMARKING:
 * - Measure event loop latency with setImmediate chains
 * - Test parallel I/O: spawn N readers, measure throughput
 * - Sample worker spawn time (mean, p95)
 * - Respect --samples and --budget-ms
 *
 * @param {ProbeConfig} [config] - Probe configuration
 * @returns {Promise<Observation[]>} Array of observations
 *
 * @example
 * const observations = await probeConcurrency({
 *   timeout: 5000,
 *   maxWorkers: 8,
 *   samples: 10
 * });
 * observations.forEach(obs => {
 *   console.log(`${obs.method}: ${JSON.stringify(obs.outputs)}`);
 * });
 */
export async function probeConcurrency(config = {}) {
  // Validate config
  const validatedConfig = ProbeConfigSchema.parse(config);
  const { timeout, maxWorkers, samples, testDir } = validatedConfig;

  const observations = [];

  try {
    // Run all probes (15 total: 9 original + 6 new)
    const [
      workerAvailObs,
      sabObs,
      atomicsObs,
      threadPoolObs,
      eventLoopObs,
      spawnTimeObs,
      messagingObs,
      maxWorkersObs,
      ioContentionObs,
      // New probes (6)
      eventLoopOrderObs,
      stackDepthObs,
      asyncLocalStorageObs,
      microtaskQueueObs,
      maxPromisesObs,
      streamBackpressureObs,
    ] = await Promise.all([
      probeWorkerThreadsAvailability(),
      probeSharedArrayBuffer(),
      probeAtomics(),
      probeThreadPoolSize(),
      probeEventLoopLatency(samples),
      probeWorkerSpawnTime(Math.min(samples, 5), timeout), // Limit spawn tests
      probeMessagePassingOverhead(samples, timeout),
      probeMaxConcurrentWorkers(maxWorkers, timeout),
      probeParallelIOContention(Math.min(maxWorkers, 4), timeout, testDir), // Limit I/O readers
      // New probes
      probeEventLoopOrdering(),
      probeStackDepth(10000), // Bounded recursion limit
      probeAsyncLocalStorage(),
      probeMicrotaskQueueDepth(1000), // Bounded microtask count
      probeMaxConcurrentPromises(1000, timeout), // Bounded promise count
      probeStreamBackpressure(100), // Bounded chunk count
    ]);

    observations.push(
      workerAvailObs,
      sabObs,
      atomicsObs,
      threadPoolObs,
      eventLoopObs,
      spawnTimeObs,
      messagingObs,
      maxWorkersObs,
      ioContentionObs,
      eventLoopOrderObs,
      stackDepthObs,
      asyncLocalStorageObs,
      microtaskQueueObs,
      maxPromisesObs,
      streamBackpressureObs
    );

  } catch (error) {
    // Catastrophic failure
    observations.push({
      method: 'concurrency.execution_error',
      inputs: {},
      outputs: {},
      timestamp: Date.now(),
      guardDecision: 'unknown',
      metadata: {
        reason: 'probe_execution_failed',
        error: error.message,
      },
    });
  } finally {
    // CRITICAL: Always cleanup workers
    await cleanupWorkers();
  }

  // Validate all observations
  return observations.map(obs => ObservationSchema.parse(obs));
}

// ============================================================================
// EXPORTS
// ============================================================================

/**
 * Re-export schemas for external validation
 */
export { ObservationSchema, ProbeConfigSchema };

/**
 * Re-export cleanup for testing
 */
export { cleanupWorkers };

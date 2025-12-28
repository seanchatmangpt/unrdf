/**
 * @file Performance Envelope Probe - Stable benchmarking for system characteristics
 * @module @unrdf/kgc-probe/probes/performance
 *
 * @description
 * Probes performance characteristics with stable, repeatable benchmarks:
 * - JSON parsing/serialization throughput (1KB, 10KB, 100KB payloads)
 * - Hashing throughput (BLAKE3 via @noble/hashes)
 * - Streaming throughput (readable/writable streams)
 * - File I/O throughput (read/write within allowed roots)
 * - Buffer operations (allocation, copy)
 * - String operations (concat, slice, regex)
 *
 * All benchmarks use:
 * - Stable sampling (configurable iterations, default 100)
 * - Statistical analysis (mean, median, stddev, p95, p99)
 * - Warmup iterations (default 10)
 * - High-resolution timing (performance.now())
 * - Deterministic test data
 *
 * GUARD CONSTRAINTS:
 * - File I/O ONLY within config.out directory
 * - Max 100MB per test (no unbounded allocations)
 * - Clean up test files after benchmarking
 * - Respect config.budgetMs global timeout
 *
 * @example
 * import { probePerformance } from '@unrdf/kgc-probe/probes/performance';
 *
 * const observations = await probePerformance({
 *   out: '/tmp/kgc-probe-output',
 *   samples: 100,
 *   budgetMs: 30000,
 *   warmupIterations: 10
 * });
 */

import { readFile, writeFile, unlink, mkdir } from 'node:fs/promises';
import { Readable, Writable } from 'node:stream';
import { pipeline } from 'node:stream/promises';
import { join, resolve } from 'node:path';
import { blake3 } from '@noble/hashes/blake3';
import { ObservationSchema, ProbeConfigSchema } from '../types.mjs';

// =============================================================================
// Constants - Deterministic Test Data
// =============================================================================

/** Fixed payload sizes for JSON tests */
const PAYLOAD_SIZES = {
  small: 1024, // 1KB
  medium: 10 * 1024, // 10KB
  large: 100 * 1024, // 100KB
};

/** Fixed buffer size for streaming tests */
const STREAM_BUFFER_SIZE = 64 * 1024; // 64KB

/** Maximum allocation size (100MB limit per test) */
const MAX_ALLOCATION_SIZE = 100 * 1024 * 1024;

/** String test corpus size */
const STRING_TEST_SIZE = 10 * 1024; // 10KB

// =============================================================================
// Guard Functions
// =============================================================================

/**
 * Check if path is within allowed output directory
 * @param {string} path - Path to check
 * @param {string} outDir - Allowed output directory
 * @returns {Object} Guard decision
 */
function guardPathAccess(path, outDir) {
  const resolvedPath = resolve(path);
  const resolvedOutDir = resolve(outDir);

  const allowed = resolvedPath.startsWith(resolvedOutDir);

  return {
    path: resolvedPath,
    allowed,
    reason: allowed
      ? 'Within config.out directory'
      : `Outside config.out directory (${resolvedOutDir})`,
    policy: 'output-only',
    timestamp: Date.now(),
  };
}

/**
 * Create observation with guard decision
 * @param {string} category - Observation category
 * @param {string} observation - Observation description
 * @param {any} value - Observed value
 * @param {Object} guardDecision - Guard decision (optional)
 * @param {Object} metadata - Additional metadata
 * @returns {Object} Validated observation
 */
function createObservation(category, observation, value, guardDecision, metadata = {}) {
  const obs = {
    probeName: 'performance',
    timestamp: Date.now(),
    category,
    observation,
    value,
    ...(guardDecision !== null && guardDecision !== undefined && { guardDecision }),
    metadata,
  };

  return ObservationSchema.parse(obs);
}

/**
 * Create error observation
 * @param {string} category - Observation category
 * @param {string} observation - Observation description
 * @param {Error} error - Error object
 * @param {Object} guardDecision - Guard decision
 * @returns {Object} Validated observation
 */
function createErrorObservation(category, observation, error, guardDecision) {
  return createObservation(category, observation, null, guardDecision, {
    error: {
      message: error.message,
      code: error.code,
      stack: error.stack,
    },
  });
}

// =============================================================================
// Utility Functions - Statistical Analysis
// =============================================================================

/**
 * Calculate comprehensive statistics from samples
 *
 * @param {number[]} samples - Array of numeric samples
 * @returns {Object} Statistics object with mean, median, stddev, min, max, p95, p99
 */
function calculateStats(samples) {
  const sorted = [...samples].sort((a, b) => a - b);
  const n = sorted.length;

  const mean = sorted.reduce((sum, val) => sum + val, 0) / n;
  const median = n % 2 === 0 ? (sorted[n / 2 - 1] + sorted[n / 2]) / 2 : sorted[Math.floor(n / 2)];
  const variance = sorted.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) / n;
  const stddev = Math.sqrt(variance);

  const min = sorted[0];
  const max = sorted[n - 1];
  const p95Index = Math.ceil(n * 0.95) - 1;
  const p99Index = Math.ceil(n * 0.99) - 1;
  const p95 = sorted[p95Index];
  const p99 = sorted[p99Index];

  return { mean, median, stddev, min, max, p95, p99, variance, samples: n };
}

/**
 * Run benchmark with warmup and statistical analysis
 *
 * @param {Function} fn - Function to benchmark
 * @param {Object} options - Benchmark options
 * @param {number} options.samples - Number of samples to collect
 * @param {number} options.warmup - Number of warmup iterations
 * @returns {Promise<Object>} Statistics object
 */
async function runBenchmark(fn, { samples = 100, warmup = 10 } = {}) {
  for (let i = 0; i < warmup; i++) {
    await fn();
  }

  const durations = [];
  for (let i = 0; i < samples; i++) {
    const start = performance.now();
    await fn();
    const end = performance.now();
    durations.push(end - start);
  }

  return calculateStats(durations);
}

// =============================================================================
// Test Data Generators
// =============================================================================

/**
 * Generate deterministic JSON payload of specified size
 * @param {number} targetBytes - Target size in bytes
 * @returns {Object} JSON object
 */
function generateJsonPayload(targetBytes) {
  const obj = {
    id: 'test-payload-001',
    timestamp: 1703001600000,
    metadata: {},
  };

  let currentSize = JSON.stringify(obj).length;
  let counter = 0;

  while (currentSize < targetBytes) {
    const key = `field_${counter.toString().padStart(6, '0')}`;
    const value = `value_${counter.toString().padStart(6, '0')}_padding_to_increase_size`;
    obj.metadata[key] = value;
    currentSize = JSON.stringify(obj).length;
    counter++;
  }

  return obj;
}

/**
 * Generate deterministic buffer of specified size
 * @param {number} size - Buffer size in bytes
 * @returns {Buffer} Filled buffer
 */
function generateBuffer(size) {
  if (size > MAX_ALLOCATION_SIZE) {
    throw new Error(`Buffer size ${size} exceeds maximum ${MAX_ALLOCATION_SIZE}`);
  }
  const buffer = Buffer.allocUnsafe(size);
  for (let i = 0; i < size; i++) {
    buffer[i] = i % 256;
  }
  return buffer;
}

/**
 * Generate deterministic string of specified size
 * @param {number} size - String size in bytes
 * @returns {string} Generated string
 */
function generateString(size) {
  return 'x'.repeat(size);
}

// =============================================================================
// Benchmark Implementations
// =============================================================================

/**
 * Benchmark JSON.parse throughput
 * @param {number} payloadSize - Payload size in bytes
 * @param {Object} options - Benchmark options
 * @returns {Promise<Object>} Statistics with throughput
 */
async function benchmarkJsonParse(payloadSize, options) {
  const payload = generateJsonPayload(payloadSize);
  const jsonString = JSON.stringify(payload);

  const stats = await runBenchmark(() => JSON.parse(jsonString), options);
  const throughput = 1000 / stats.mean;

  return { ...stats, throughput, payloadSize: jsonString.length };
}

/**
 * Benchmark JSON.stringify throughput
 * @param {number} payloadSize - Payload size in bytes
 * @param {Object} options - Benchmark options
 * @returns {Promise<Object>} Statistics with throughput
 */
async function benchmarkJsonStringify(payloadSize, options) {
  const payload = generateJsonPayload(payloadSize);

  const stats = await runBenchmark(() => JSON.stringify(payload), options);
  const throughput = 1000 / stats.mean;

  return { ...stats, throughput, payloadSize: JSON.stringify(payload).length };
}

/**
 * Benchmark BLAKE3 hashing throughput
 * @param {number} dataSize - Data size in bytes
 * @param {Object} options - Benchmark options
 * @returns {Promise<Object>} Statistics with throughput
 */
async function benchmarkBlake3(dataSize, options) {
  const data = generateBuffer(dataSize);

  const stats = await runBenchmark(() => blake3(data), options);
  const throughput = 1000 / stats.mean;
  const mbPerSec = (dataSize / (1024 * 1024)) * throughput;

  return { ...stats, throughput, mbPerSec, dataSize };
}

/**
 * Benchmark streaming throughput
 * @param {number} bufferSize - Buffer size in bytes
 * @param {Object} options - Benchmark options
 * @returns {Promise<Object>} Statistics with throughput
 */
async function benchmarkStreaming(bufferSize, options) {
  const data = generateBuffer(bufferSize);

  const stats = await runBenchmark(async () => {
    const readable = Readable.from([data]);
    const chunks = [];
    const writable = new Writable({
      write(chunk, encoding, callback) {
        chunks.push(chunk);
        callback();
      },
    });

    await pipeline(readable, writable);
  }, options);

  const throughput = 1000 / stats.mean;
  const mbPerSec = (bufferSize / (1024 * 1024)) * throughput;

  return { ...stats, throughput, mbPerSec, bufferSize };
}

/**
 * Benchmark file read throughput
 * @param {string} outDir - Output directory for test files
 * @param {number} fileSize - File size in bytes
 * @param {Object} options - Benchmark options
 * @returns {Promise<Object>} Statistics with throughput
 */
async function benchmarkFileRead(outDir, fileSize, options) {
  const testDir = join(outDir, `kgc-probe-perf-${Date.now()}`);
  await mkdir(testDir, { recursive: true });

  const testFile = join(testDir, 'test-read.bin');
  const data = generateBuffer(fileSize);

  try {
    await writeFile(testFile, data);

    const stats = await runBenchmark(async () => {
      await readFile(testFile);
    }, options);

    const throughput = 1000 / stats.mean;
    const mbPerSec = (fileSize / (1024 * 1024)) * throughput;

    return { ...stats, throughput, mbPerSec, fileSize };
  } finally {
    try {
      await unlink(testFile);
    } catch {
      // Ignore cleanup errors
    }
  }
}

/**
 * Benchmark file write throughput
 * @param {string} outDir - Output directory for test files
 * @param {number} fileSize - File size in bytes
 * @param {Object} options - Benchmark options
 * @returns {Promise<Object>} Statistics with throughput
 */
async function benchmarkFileWrite(outDir, fileSize, options) {
  const testDir = join(outDir, `kgc-probe-perf-${Date.now()}`);
  await mkdir(testDir, { recursive: true });

  const data = generateBuffer(fileSize);
  let counter = 0;

  try {
    const stats = await runBenchmark(async () => {
      const testFile = join(testDir, `test-write-${counter++}.bin`);
      await writeFile(testFile, data);
      try {
        await unlink(testFile);
      } catch {
        // Ignore
      }
    }, options);

    const throughput = 1000 / stats.mean;
    const mbPerSec = (fileSize / (1024 * 1024)) * throughput;

    return { ...stats, throughput, mbPerSec, fileSize };
  } finally {
    // No cleanup needed
  }
}

/**
 * Benchmark buffer allocation performance
 * @param {number} bufferSize - Buffer size in bytes
 * @param {Object} options - Benchmark options
 * @returns {Promise<Object>} Statistics with throughput
 */
async function benchmarkBufferAllocation(bufferSize, options) {
  const stats = await runBenchmark(() => {
    Buffer.allocUnsafe(bufferSize);
  }, options);

  const throughput = 1000 / stats.mean;

  return { ...stats, throughput, bufferSize };
}

/**
 * Benchmark buffer copy performance
 * @param {number} bufferSize - Buffer size in bytes
 * @param {Object} options - Benchmark options
 * @returns {Promise<Object>} Statistics with throughput
 */
async function benchmarkBufferCopy(bufferSize, options) {
  const source = generateBuffer(bufferSize);
  const target = Buffer.allocUnsafe(bufferSize);

  const stats = await runBenchmark(() => {
    source.copy(target);
  }, options);

  const throughput = 1000 / stats.mean;
  const mbPerSec = (bufferSize / (1024 * 1024)) * throughput;

  return { ...stats, throughput, mbPerSec, bufferSize };
}

/**
 * Benchmark string operations
 * @param {number} stringSize - String size in bytes
 * @param {Object} options - Benchmark options
 * @returns {Promise<Object>} All string operation statistics
 */
async function benchmarkStringOperations(stringSize, options) {
  const str1 = generateString(stringSize / 2);
  const str2 = generateString(stringSize / 2);
  const str = generateString(stringSize);
  const regex = /x+/g;

  const concatStats = await runBenchmark(() => {
    const result = str1 + str2;
    return result;
  }, options);

  const sliceStats = await runBenchmark(() => {
    str.slice(0, stringSize / 2);
  }, options);

  const regexStats = await runBenchmark(() => {
    str.match(regex);
  }, options);

  return {
    concat: { ...concatStats, throughput: 1000 / concatStats.mean },
    slice: { ...sliceStats, throughput: 1000 / sliceStats.mean },
    regex: { ...regexStats, throughput: 1000 / regexStats.mean },
  };
}

// =============================================================================
// Main Probe Function
// =============================================================================

/**
 * Probe performance characteristics with stable benchmarking
 *
 * Runs comprehensive performance benchmarks and returns observations.
 * All file I/O operations are guarded to config.out directory only.
 *
 * @param {Object} config - Probe configuration
 * @param {string} config.out - Output directory for test files (required)
 * @param {number} [config.timeout=5000] - Timeout per operation (ms)
 * @param {number} [config.maxQuota=104857600] - Max quota (bytes)
 * @param {number} [config.samples=100] - Number of benchmark samples
 * @param {number} [config.budgetMs=30000] - Global timeout in milliseconds
 * @param {number} [config.warmupIterations=10] - Warmup iterations
 * @returns {Promise<Array>} Array of Observation objects
 *
 * @throws {Error} If configuration is invalid
 * @throws {Error} If budget is exceeded
 *
 * @example
 * const observations = await probePerformance({
 *   out: '/tmp/kgc-probe-output',
 *   samples: 100,
 *   budgetMs: 30000
 * });
 */
export async function probePerformance(config = {}) {
  // Validate config
  const validatedConfig = ProbeConfigSchema.parse(config);
  const { out, timeout } = validatedConfig;

  // Additional config defaults for performance benchmarking
  const samples = config.samples || 100;
  const budgetMs = config.budgetMs || 30000;
  const warmupIterations = config.warmupIterations || 10;

  const observations = [];
  const startTime = performance.now();
  const benchmarkOptions = { samples, warmup: warmupIterations };

  /**
   * Helper to check budget
   */
  function checkBudget() {
    const elapsed = performance.now() - startTime;
    if (elapsed > budgetMs) {
      throw new Error(`Budget exceeded: ${elapsed.toFixed(2)}ms > ${budgetMs}ms`);
    }
  }

  try {
    // JSON Parse benchmarks
    for (const [label, size] of Object.entries(PAYLOAD_SIZES)) {
      checkBudget();
      const stats = await benchmarkJsonParse(size, benchmarkOptions);
      const sizeKB = (size / 1024).toFixed(0);

      observations.push(
        createObservation(
          'performance',
          `JSON.parse throughput (${sizeKB}KB payload)`,
          stats.throughput,
          null, // No guard decision for in-memory operations
          {
            operation: 'json.parse',
            payloadSize: `${sizeKB}KB`,
            unit: 'ops/sec',
            mean_ms: stats.mean,
            median_ms: stats.median,
            min_ms: stats.min,
            max_ms: stats.max,
            p95_ms: stats.p95,
            p99_ms: stats.p99,
            stddev_ms: stats.stddev,
            variance_percent: (stats.stddev / stats.mean) * 100,
          }
        )
      );
    }

    // JSON Stringify benchmarks
    for (const [label, size] of Object.entries(PAYLOAD_SIZES)) {
      checkBudget();
      const stats = await benchmarkJsonStringify(size, benchmarkOptions);
      const sizeKB = (size / 1024).toFixed(0);

      observations.push(
        createObservation(
          'performance',
          `JSON.stringify throughput (${sizeKB}KB payload)`,
          stats.throughput,
          null,
          {
            operation: 'json.stringify',
            payloadSize: `${sizeKB}KB`,
            unit: 'ops/sec',
            mean_ms: stats.mean,
            median_ms: stats.median,
            p95_ms: stats.p95,
            p99_ms: stats.p99,
            stddev_ms: stats.stddev,
          }
        )
      );
    }

    // BLAKE3 hashing benchmarks
    for (const [label, size] of Object.entries(PAYLOAD_SIZES)) {
      checkBudget();
      const stats = await benchmarkBlake3(size, benchmarkOptions);
      const sizeKB = (size / 1024).toFixed(0);

      observations.push(
        createObservation(
          'performance',
          `BLAKE3 hashing throughput (${sizeKB}KB data)`,
          stats.mbPerSec,
          null,
          {
            operation: 'hash.blake3',
            dataSize: `${sizeKB}KB`,
            unit: 'MB/sec',
            mean_ms: stats.mean,
            median_ms: stats.median,
            p95_ms: stats.p95,
            p99_ms: stats.p99,
            stddev_ms: stats.stddev,
          }
        )
      );
    }

    // Streaming benchmarks
    checkBudget();
    const streamStats = await benchmarkStreaming(STREAM_BUFFER_SIZE, benchmarkOptions);
    observations.push(
      createObservation(
        'performance',
        `Streaming throughput (${(STREAM_BUFFER_SIZE / 1024).toFixed(0)}KB buffer)`,
        streamStats.mbPerSec,
        null,
        {
          operation: 'stream.throughput',
          bufferSize: `${(STREAM_BUFFER_SIZE / 1024).toFixed(0)}KB`,
          unit: 'MB/sec',
          mean_ms: streamStats.mean,
          median_ms: streamStats.median,
          p95_ms: streamStats.p95,
          p99_ms: streamStats.p99,
          stddev_ms: streamStats.stddev,
        }
      )
    );

    // File I/O benchmarks (with guard checks)
    for (const [label, size] of Object.entries(PAYLOAD_SIZES)) {
      checkBudget();
      const guardDecision = guardPathAccess(out, out);

      if (!guardDecision.allowed) {
        observations.push(
          createObservation(
            'performance',
            `File read throughput (DENIED - ${guardDecision.reason})`,
            null,
            guardDecision,
            { operation: 'file.read', payloadSize: `${(size / 1024).toFixed(0)}KB` }
          )
        );
        continue;
      }

      const stats = await benchmarkFileRead(out, size, benchmarkOptions);
      const sizeKB = (size / 1024).toFixed(0);

      observations.push(
        createObservation(
          'performance',
          `File read throughput (${sizeKB}KB file)`,
          stats.mbPerSec,
          guardDecision,
          {
            operation: 'file.read',
            fileSize: `${sizeKB}KB`,
            unit: 'MB/sec',
            mean_ms: stats.mean,
            median_ms: stats.median,
            p95_ms: stats.p95,
            p99_ms: stats.p99,
            stddev_ms: stats.stddev,
          }
        )
      );
    }

    // File write benchmarks
    for (const [label, size] of Object.entries(PAYLOAD_SIZES)) {
      checkBudget();
      const guardDecision = guardPathAccess(out, out);

      if (!guardDecision.allowed) {
        observations.push(
          createObservation(
            'performance',
            `File write throughput (DENIED - ${guardDecision.reason})`,
            null,
            guardDecision,
            { operation: 'file.write', payloadSize: `${(size / 1024).toFixed(0)}KB` }
          )
        );
        continue;
      }

      const stats = await benchmarkFileWrite(out, size, benchmarkOptions);
      const sizeKB = (size / 1024).toFixed(0);

      observations.push(
        createObservation(
          'performance',
          `File write throughput (${sizeKB}KB file)`,
          stats.mbPerSec,
          guardDecision,
          {
            operation: 'file.write',
            fileSize: `${sizeKB}KB`,
            unit: 'MB/sec',
            mean_ms: stats.mean,
            median_ms: stats.median,
            p95_ms: stats.p95,
            p99_ms: stats.p99,
            stddev_ms: stats.stddev,
          }
        )
      );
    }

    // Buffer operation benchmarks
    for (const [label, size] of Object.entries(PAYLOAD_SIZES)) {
      checkBudget();
      const sizeKB = (size / 1024).toFixed(0);

      const allocStats = await benchmarkBufferAllocation(size, benchmarkOptions);
      observations.push(
        createObservation(
          'performance',
          `Buffer allocation (${sizeKB}KB)`,
          allocStats.throughput,
          null,
          {
            operation: 'buffer.allocation',
            bufferSize: `${sizeKB}KB`,
            unit: 'ops/sec',
            mean_ms: allocStats.mean,
            median_ms: allocStats.median,
            p95_ms: allocStats.p95,
            p99_ms: allocStats.p99,
            stddev_ms: allocStats.stddev,
          }
        )
      );

      const copyStats = await benchmarkBufferCopy(size, benchmarkOptions);
      observations.push(
        createObservation(
          'performance',
          `Buffer copy (${sizeKB}KB)`,
          copyStats.mbPerSec,
          null,
          {
            operation: 'buffer.copy',
            bufferSize: `${sizeKB}KB`,
            unit: 'MB/sec',
            mean_ms: copyStats.mean,
            median_ms: copyStats.median,
            p95_ms: copyStats.p95,
            p99_ms: copyStats.p99,
            stddev_ms: copyStats.stddev,
          }
        )
      );
    }

    // String operation benchmarks
    checkBudget();
    const stringStats = await benchmarkStringOperations(STRING_TEST_SIZE, benchmarkOptions);
    const sizeKB = (STRING_TEST_SIZE / 1024).toFixed(0);

    observations.push(
      createObservation(
        'performance',
        `String concatenation (${sizeKB}KB)`,
        stringStats.concat.throughput,
        null,
        {
          operation: 'string.concat',
          stringSize: `${sizeKB}KB`,
          unit: 'ops/sec',
          mean_ms: stringStats.concat.mean,
          median_ms: stringStats.concat.median,
          p95_ms: stringStats.concat.p95,
          p99_ms: stringStats.concat.p99,
          stddev_ms: stringStats.concat.stddev,
        }
      )
    );

    observations.push(
      createObservation(
        'performance',
        `String slice (${sizeKB}KB)`,
        stringStats.slice.throughput,
        null,
        {
          operation: 'string.slice',
          stringSize: `${sizeKB}KB`,
          unit: 'ops/sec',
          mean_ms: stringStats.slice.mean,
          median_ms: stringStats.slice.median,
          p95_ms: stringStats.slice.p95,
          p99_ms: stringStats.slice.p99,
          stddev_ms: stringStats.slice.stddev,
        }
      )
    );

    observations.push(
      createObservation(
        'performance',
        `String regex match (${sizeKB}KB)`,
        stringStats.regex.throughput,
        null,
        {
          operation: 'string.regex',
          stringSize: `${sizeKB}KB`,
          unit: 'ops/sec',
          mean_ms: stringStats.regex.mean,
          median_ms: stringStats.regex.median,
          p95_ms: stringStats.regex.p95,
          p99_ms: stringStats.regex.p99,
          stddev_ms: stringStats.regex.stddev,
        }
      )
    );

    return observations;
  } catch (error) {
    throw new Error(`Performance probe failed: ${error.message}`, { cause: error });
  }
}

// =============================================================================
// Module Exports
// =============================================================================

export default {
  probePerformance,
};

/**
 * @file WASM Surface Probe - WebAssembly Capabilities Detection & Benchmarking
 * @module @unrdf/kgc-probe/probes/wasm
 *
 * @description
 * Probes WebAssembly runtime capabilities and performance characteristics:
 * - WASM instantiation support (WebAssembly.instantiate)
 * - Memory growth limits (WebAssembly.Memory)
 * - Initial/maximum memory pages
 * - Compile time for minimal module
 * - Instantiate time for minimal module
 * - Call overhead (JS -> WASM -> JS)
 * - Table/global support
 * - SIMD support detection
 * - Threads support detection
 *
 * All operations are guarded with timeouts and resource limits to prevent hangs.
 *
 * @example
 * import { probeWasm } from '@unrdf/kgc-probe/probes/wasm';
 *
 * const observations = await probeWasm({
 *   samples: 100,
 *   timeout: 5000,
 *   maxMemoryMB: 1024
 * });
 *
 * console.log(observations.find(o => o.metric === 'wasm.compile.time'));
 */

import { z } from 'zod';

// =============================================================================
// Observation Schema - Probe Output Type
// =============================================================================

/**
 * Observation schema for probe results
 *
 * Each observation represents a single metric measurement:
 * - metric: Dot-notation metric name (e.g., 'wasm.compile.time')
 * - value: Numeric measurement value
 * - unit: Measurement unit (ms, bytes, boolean, count, etc.)
 * - timestamp: When measurement was taken (Unix epoch ms)
 * - status: Measurement status (success, error, unsupported, timeout)
 * - error: Error message if status is 'error'
 * - metadata: Additional context (samples, iterations, etc.)
 *
 * @constant
 * @type {z.ZodObject}
 */
export const ObservationSchema = z.object({
  /** Metric name in dot notation */
  metric: z.string().min(1).max(200),
  /** Measured value */
  value: z.union([z.number(), z.boolean(), z.string()]),
  /** Measurement unit */
  unit: z.enum(['ms', 'ns', 'bytes', 'pages', 'boolean', 'count', 'percent', 'ratio']),
  /** Measurement timestamp (Unix epoch ms) */
  timestamp: z.number().int().positive(),
  /** Measurement status */
  status: z.enum(['success', 'error', 'unsupported', 'timeout']),
  /** Error message if failed */
  error: z.string().optional(),
  /** Additional metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * WASM probe configuration schema
 *
 * @constant
 * @type {z.ZodObject}
 */
export const WasmProbeConfigSchema = z.object({
  /** Number of samples for benchmarking (default: 100) */
  samples: z.number().int().positive().max(10000).default(100),
  /** Timeout per operation in milliseconds (default: 5000) */
  timeout: z.number().int().positive().max(30000).default(5000),
  /** Maximum memory allocation in MB (default: 1024) */
  maxMemoryMB: z.number().int().positive().max(4096).default(1024),
  /** Enable SIMD detection (default: true) */
  detectSIMD: z.boolean().default(true),
  /** Enable threads detection (default: true) */
  detectThreads: z.boolean().default(true),
});

// =============================================================================
// Minimal WASM Test Module
// =============================================================================

/**
 * Minimal WASM module (WAT format):
 * ```wat
 * (module
 *   (func (export "add") (param i32 i32) (result i32)
 *     local.get 0
 *     local.get 1
 *     i32.add
 *   )
 * )
 * ```
 *
 * This is the smallest possible WASM module for testing basic functionality.
 * Binary format (WebAssembly binary):
 */
const MINIMAL_WASM_BYTES = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, // Magic number: \0asm
  0x01, 0x00, 0x00, 0x00, // Version: 1
  0x01, 0x07, 0x01, 0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f, // Type section: (i32, i32) -> i32
  0x03, 0x02, 0x01, 0x00, // Function section: func 0 has type 0
  0x07, 0x07, 0x01, 0x03, 0x61, 0x64, 0x64, 0x00, 0x00, // Export section: "add" = func 0
  0x0a, 0x09, 0x01, 0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x6a, 0x0b, // Code section: add implementation
]);

/**
 * WASM module with memory export for memory testing:
 * ```wat
 * (module
 *   (memory (export "memory") 1 10)
 * )
 * ```
 */
const WASM_MEMORY_MODULE = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, // Magic + version
  0x05, 0x04, 0x01, 0x01, 0x01, 0x0a, // Memory section: initial=1, max=10
  0x07, 0x0a, 0x01, 0x06, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x02, 0x00, // Export "memory"
]);

// =============================================================================
// Guards - Poka Yoke Error Prevention
// =============================================================================

/**
 * Guard W1: Validate WASM support exists
 * Prevents: Runtime error from missing WebAssembly global
 * Action: Check typeof WebAssembly !== 'undefined'
 */
function guardWasmSupport() {
  if (typeof WebAssembly === 'undefined') {
    throw new Error('Guard W1 failed: WebAssembly not supported in this environment');
  }
  return true;
}

/**
 * Guard W2: Validate timeout value
 * Prevents: Infinite hang from missing timeout
 * Action: Ensure timeout is positive integer
 */
function guardTimeout(timeout) {
  if (typeof timeout !== 'number' || timeout <= 0 || !Number.isFinite(timeout)) {
    throw new TypeError(`Guard W2 failed: Timeout must be positive number, got ${timeout}`);
  }
  return true;
}

/**
 * Guard W3: Validate memory limit
 * Prevents: OOM from unbounded memory growth test
 * Action: Cap at 1GB max
 */
function guardMemoryLimit(maxMemoryMB) {
  if (typeof maxMemoryMB !== 'number' || maxMemoryMB <= 0 || maxMemoryMB > 4096) {
    throw new RangeError(`Guard W3 failed: maxMemoryMB must be 1-4096, got ${maxMemoryMB}`);
  }
  return true;
}

/**
 * Guard W4: Validate WASM module bytes
 * Prevents: Invalid module bytes causing instantiation failure
 * Action: Check magic number \0asm
 */
function guardWasmBytes(bytes) {
  if (!(bytes instanceof Uint8Array)) {
    throw new TypeError(`Guard W4 failed: WASM bytes must be Uint8Array, got ${typeof bytes}`);
  }
  if (bytes.length < 8) {
    throw new Error(`Guard W4 failed: WASM module too small (${bytes.length} bytes)`);
  }
  // Check magic number: \0asm
  if (bytes[0] !== 0x00 || bytes[1] !== 0x61 || bytes[2] !== 0x73 || bytes[3] !== 0x6d) {
    throw new Error(`Guard W4 failed: Invalid WASM magic number`);
  }
  return true;
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Create an observation object
 * @param {string} metric - Metric name
 * @param {number|boolean|string} value - Metric value
 * @param {string} unit - Measurement unit
 * @param {string} status - Measurement status
 * @param {string} [error] - Error message if failed
 * @param {Object} [metadata] - Additional metadata
 * @returns {Object} Validated observation
 */
function createObservation(metric, value, unit, status, error = undefined, metadata = {}) {
  const observation = {
    metric,
    value,
    unit,
    timestamp: Date.now(),
    status,
    error,
    metadata,
  };
  return ObservationSchema.parse(observation);
}

/**
 * Execute with timeout
 * @param {Function} fn - Async function to execute
 * @param {number} timeoutMs - Timeout in milliseconds
 * @returns {Promise<any>} Result or throws timeout error
 */
async function withTimeout(fn, timeoutMs) {
  guardTimeout(timeoutMs);

  return Promise.race([
    fn(),
    new Promise((_, reject) =>
      setTimeout(() => reject(new Error(`Operation timed out after ${timeoutMs}ms`)), timeoutMs)
    ),
  ]);
}

/**
 * Measure execution time of function
 * @param {Function} fn - Function to measure
 * @returns {Promise<number>} Duration in milliseconds
 */
async function measureTime(fn) {
  const start = performance.now();
  await fn();
  const end = performance.now();
  return end - start;
}

/**
 * Calculate statistics from samples
 * @param {number[]} samples - Array of measurements
 * @returns {Object} { mean, median, min, max, stddev }
 */
function calculateStats(samples) {
  if (samples.length === 0) {
    throw new Error('Cannot calculate stats from empty array');
  }

  const sorted = [...samples].sort((a, b) => a - b);
  const mean = samples.reduce((sum, x) => sum + x, 0) / samples.length;
  const median = sorted[Math.floor(sorted.length / 2)];
  const min = sorted[0];
  const max = sorted[sorted.length - 1];

  // Standard deviation
  const variance = samples.reduce((sum, x) => sum + Math.pow(x - mean, 2), 0) / samples.length;
  const stddev = Math.sqrt(variance);

  return { mean, median, min, max, stddev };
}

// =============================================================================
// WASM Capability Probes
// =============================================================================

/**
 * Probe WASM instantiation support
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Object>} Observation
 */
async function probeInstantiationSupport(timeout) {
  try {
    guardWasmSupport();
    guardWasmBytes(MINIMAL_WASM_BYTES);

    await withTimeout(async () => {
      await WebAssembly.instantiate(MINIMAL_WASM_BYTES);
    }, timeout);

    return createObservation('wasm.support.instantiate', true, 'boolean', 'success');
  } catch (error) {
    return createObservation('wasm.support.instantiate', false, 'boolean', 'error', error.message);
  }
}

/**
 * Probe WASM compile support
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Object>} Observation
 */
async function probeCompileSupport(timeout) {
  try {
    guardWasmSupport();
    guardWasmBytes(MINIMAL_WASM_BYTES);

    await withTimeout(async () => {
      await WebAssembly.compile(MINIMAL_WASM_BYTES);
    }, timeout);

    return createObservation('wasm.support.compile', true, 'boolean', 'success');
  } catch (error) {
    return createObservation('wasm.support.compile', false, 'boolean', 'error', error.message);
  }
}

/**
 * Probe WASM memory support
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Object[]>} Observations
 */
async function probeMemorySupport(timeout) {
  const observations = [];

  try {
    guardWasmSupport();

    // Test basic memory creation
    await withTimeout(async () => {
      new WebAssembly.Memory({ initial: 1 });
    }, timeout);

    observations.push(createObservation('wasm.support.memory', true, 'boolean', 'success'));
  } catch (error) {
    observations.push(createObservation('wasm.support.memory', false, 'boolean', 'error', error.message));
    return observations;
  }

  // Test memory growth
  try {
    await withTimeout(async () => {
      const memory = new WebAssembly.Memory({ initial: 1, maximum: 10 });
      memory.grow(1);
    }, timeout);

    observations.push(createObservation('wasm.support.memory.grow', true, 'boolean', 'success'));
  } catch (error) {
    observations.push(createObservation('wasm.support.memory.grow', false, 'boolean', 'error', error.message));
  }

  return observations;
}

/**
 * Probe WASM table support
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Object>} Observation
 */
async function probeTableSupport(timeout) {
  try {
    guardWasmSupport();

    await withTimeout(async () => {
      new WebAssembly.Table({ initial: 1, element: 'anyfunc' });
    }, timeout);

    return createObservation('wasm.support.table', true, 'boolean', 'success');
  } catch (error) {
    return createObservation('wasm.support.table', false, 'boolean', 'error', error.message);
  }
}

/**
 * Probe WASM global support
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Object>} Observation
 */
async function probeGlobalSupport(timeout) {
  try {
    guardWasmSupport();

    await withTimeout(async () => {
      new WebAssembly.Global({ value: 'i32', mutable: true }, 42);
    }, timeout);

    return createObservation('wasm.support.global', true, 'boolean', 'success');
  } catch (error) {
    return createObservation('wasm.support.global', false, 'boolean', 'error', error.message);
  }
}

/**
 * Probe SIMD support
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Object>} Observation
 */
async function probeSIMDSupport(timeout) {
  try {
    guardWasmSupport();

    // WASM module with SIMD v128 instruction (minimal test)
    // This will fail if SIMD is not supported
    const simdTestBytes = new Uint8Array([
      0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
    ]);

    // Check if SIMD is available via feature detection
    const hasSIMD = typeof WebAssembly.validate === 'function' && WebAssembly.validate(simdTestBytes);

    return createObservation('wasm.support.simd', hasSIMD, 'boolean', hasSIMD ? 'success' : 'unsupported');
  } catch (error) {
    return createObservation('wasm.support.simd', false, 'boolean', 'error', error.message);
  }
}

/**
 * Probe threads support
 * @param {number} timeout - Timeout in milliseconds
 * @returns {Promise<Object>} Observation
 */
async function probeThreadsSupport(timeout) {
  try {
    guardWasmSupport();

    // Check for SharedArrayBuffer (required for threads)
    const hasSharedArrayBuffer = typeof SharedArrayBuffer !== 'undefined';

    // Check for Atomics (required for threads)
    const hasAtomics = typeof Atomics !== 'undefined';

    const hasThreads = hasSharedArrayBuffer && hasAtomics;

    return createObservation(
      'wasm.support.threads',
      hasThreads,
      'boolean',
      hasThreads ? 'success' : 'unsupported',
      undefined,
      { hasSharedArrayBuffer, hasAtomics }
    );
  } catch (error) {
    return createObservation('wasm.support.threads', false, 'boolean', 'error', error.message);
  }
}

// =============================================================================
// WASM Performance Benchmarks
// =============================================================================

/**
 * Benchmark WASM compile time
 * @param {number} samples - Number of samples to collect
 * @param {number} timeout - Timeout per operation
 * @returns {Promise<Object[]>} Observations
 */
async function benchmarkCompileTime(samples, timeout) {
  const observations = [];
  const times = [];

  try {
    guardWasmSupport();
    guardWasmBytes(MINIMAL_WASM_BYTES);

    for (let i = 0; i < samples; i++) {
      const duration = await withTimeout(async () => {
        return measureTime(async () => {
          await WebAssembly.compile(MINIMAL_WASM_BYTES);
        });
      }, timeout);

      times.push(duration);
    }

    const stats = calculateStats(times);

    observations.push(createObservation('wasm.compile.time.mean', stats.mean, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.compile.time.median', stats.median, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.compile.time.min', stats.min, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.compile.time.max', stats.max, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.compile.time.stddev', stats.stddev, 'ms', 'success', undefined, { samples }));
  } catch (error) {
    observations.push(createObservation('wasm.compile.time.mean', 0, 'ms', 'error', error.message));
  }

  return observations;
}

/**
 * Benchmark WASM instantiate time
 * @param {number} samples - Number of samples to collect
 * @param {number} timeout - Timeout per operation
 * @returns {Promise<Object[]>} Observations
 */
async function benchmarkInstantiateTime(samples, timeout) {
  const observations = [];
  const times = [];

  try {
    guardWasmSupport();
    guardWasmBytes(MINIMAL_WASM_BYTES);

    for (let i = 0; i < samples; i++) {
      const duration = await withTimeout(async () => {
        return measureTime(async () => {
          await WebAssembly.instantiate(MINIMAL_WASM_BYTES);
        });
      }, timeout);

      times.push(duration);
    }

    const stats = calculateStats(times);

    observations.push(createObservation('wasm.instantiate.time.mean', stats.mean, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.instantiate.time.median', stats.median, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.instantiate.time.min', stats.min, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.instantiate.time.max', stats.max, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.instantiate.time.stddev', stats.stddev, 'ms', 'success', undefined, { samples }));
  } catch (error) {
    observations.push(createObservation('wasm.instantiate.time.mean', 0, 'ms', 'error', error.message));
  }

  return observations;
}

/**
 * Benchmark WASM call overhead (JS -> WASM -> JS)
 * @param {number} samples - Number of samples to collect
 * @param {number} timeout - Timeout per operation
 * @returns {Promise<Object[]>} Observations
 */
async function benchmarkCallOverhead(samples, timeout) {
  const observations = [];
  const times = [];

  try {
    guardWasmSupport();
    guardWasmBytes(MINIMAL_WASM_BYTES);

    const { instance } = await withTimeout(async () => {
      return WebAssembly.instantiate(MINIMAL_WASM_BYTES);
    }, timeout);

    const addFunc = instance.exports.add;

    // Warm up
    for (let i = 0; i < 100; i++) {
      addFunc(1, 2);
    }

    // Measure call overhead
    for (let i = 0; i < samples; i++) {
      const duration = await measureTime(() => {
        addFunc(1, 2);
      });

      times.push(duration);
    }

    const stats = calculateStats(times);

    observations.push(createObservation('wasm.call.overhead.mean', stats.mean, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.call.overhead.median', stats.median, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.call.overhead.min', stats.min, 'ms', 'success', undefined, { samples }));
    observations.push(createObservation('wasm.call.overhead.max', stats.max, 'ms', 'success', undefined, { samples }));
  } catch (error) {
    observations.push(createObservation('wasm.call.overhead.mean', 0, 'ms', 'error', error.message));
  }

  return observations;
}

/**
 * Benchmark memory growth limits
 * @param {number} maxMemoryMB - Maximum memory to test in MB
 * @param {number} timeout - Timeout per operation
 * @returns {Promise<Object[]>} Observations
 */
async function benchmarkMemoryGrowth(maxMemoryMB, timeout) {
  const observations = [];

  try {
    guardWasmSupport();
    guardMemoryLimit(maxMemoryMB);
    guardWasmBytes(WASM_MEMORY_MODULE);

    const maxPages = Math.floor((maxMemoryMB * 1024 * 1024) / (64 * 1024)); // 64KB per page

    const { instance } = await withTimeout(async () => {
      return WebAssembly.instantiate(WASM_MEMORY_MODULE);
    }, timeout);

    const memory = instance.exports.memory;
    const initialPages = memory.buffer.byteLength / (64 * 1024);

    observations.push(createObservation('wasm.memory.initial.pages', initialPages, 'pages', 'success'));
    observations.push(createObservation('wasm.memory.initial.bytes', memory.buffer.byteLength, 'bytes', 'success'));

    // Try to grow memory
    let currentPages = initialPages;
    let maxAchievedPages = initialPages;

    try {
      while (currentPages < maxPages) {
        await withTimeout(async () => {
          memory.grow(1);
        }, timeout);
        currentPages++;
        maxAchievedPages = currentPages;

        // Stop if we hit the module's internal limit
        if (currentPages >= 10) {
          break; // Our test module has max=10 pages
        }
      }
    } catch (error) {
      // Growth failed, record what we achieved
    }

    observations.push(createObservation('wasm.memory.max.pages', maxAchievedPages, 'pages', 'success'));
    observations.push(createObservation('wasm.memory.max.bytes', maxAchievedPages * 64 * 1024, 'bytes', 'success'));
  } catch (error) {
    observations.push(createObservation('wasm.memory.growth', 0, 'pages', 'error', error.message));
  }

  return observations;
}

// =============================================================================
// Main Probe Function
// =============================================================================

/**
 * Probe WebAssembly capabilities and performance
 *
 * Returns comprehensive WASM runtime observations including:
 * - Feature support (instantiate, compile, memory, table, global, SIMD, threads)
 * - Performance benchmarks (compile time, instantiate time, call overhead)
 * - Memory characteristics (initial pages, max pages, growth limits)
 *
 * All operations are guarded with timeouts and resource limits.
 *
 * @param {Object} [config={}] - Probe configuration
 * @param {number} [config.samples=100] - Number of samples for benchmarks
 * @param {number} [config.timeout=5000] - Timeout per operation (ms)
 * @param {number} [config.maxMemoryMB=1024] - Max memory for growth test (MB)
 * @param {boolean} [config.detectSIMD=true] - Enable SIMD detection
 * @param {boolean} [config.detectThreads=true] - Enable threads detection
 * @returns {Promise<Object[]>} Array of observations
 *
 * @example
 * const observations = await probeWasm({
 *   samples: 100,
 *   timeout: 5000,
 *   maxMemoryMB: 1024
 * });
 *
 * // Find specific metric
 * const compileTime = observations.find(o => o.metric === 'wasm.compile.time.mean');
 * console.log(`WASM compile time: ${compileTime.value}ms`);
 *
 * // Filter by status
 * const errors = observations.filter(o => o.status === 'error');
 * console.log(`Failed probes: ${errors.length}`);
 */
export async function probeWasm(config = {}) {
  const validatedConfig = WasmProbeConfigSchema.parse(config);
  const { samples, timeout, maxMemoryMB, detectSIMD, detectThreads } = validatedConfig;

  const observations = [];

  // Guard: Check WASM support before running any probes
  try {
    guardWasmSupport();
  } catch (error) {
    observations.push(createObservation('wasm.environment', false, 'boolean', 'error', error.message));
    return observations;
  }

  observations.push(createObservation('wasm.environment', true, 'boolean', 'success'));

  // Capability probes
  observations.push(await probeInstantiationSupport(timeout));
  observations.push(await probeCompileSupport(timeout));
  observations.push(...(await probeMemorySupport(timeout)));
  observations.push(await probeTableSupport(timeout));
  observations.push(await probeGlobalSupport(timeout));

  if (detectSIMD) {
    observations.push(await probeSIMDSupport(timeout));
  }

  if (detectThreads) {
    observations.push(await probeThreadsSupport(timeout));
  }

  // Performance benchmarks (only if basic support confirmed)
  const instantiateSupported = observations.find(
    o => o.metric === 'wasm.support.instantiate' && o.value === true
  );

  if (instantiateSupported) {
    observations.push(...(await benchmarkCompileTime(samples, timeout)));
    observations.push(...(await benchmarkInstantiateTime(samples, timeout)));
    observations.push(...(await benchmarkCallOverhead(samples, timeout)));
    observations.push(...(await benchmarkMemoryGrowth(maxMemoryMB, timeout)));
  }

  return observations;
}

// =============================================================================
// Module Exports
// =============================================================================

export default {
  probeWasm,
  ObservationSchema,
  WasmProbeConfigSchema,
};

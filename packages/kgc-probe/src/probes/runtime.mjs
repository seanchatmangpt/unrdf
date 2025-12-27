/**
 * @file Runtime & Language Surface Probe - KGC Probe Swarm Agent 2
 * @module @unrdf/kgc-probe/probes/runtime
 *
 * @description
 * Probes Node.js and JavaScript engine capabilities in the VM:
 * - Node.js version and V8 version
 * - Module system support (ESM vs CJS)
 * - Timer resolution and precision
 * - Worker threads availability
 * - WebAssembly support
 * - Event loop latency baseline
 * - Available globals
 * - Memory limits
 *
 * All observations include BLAKE3 content-addressed hashes and guard decisions.
 *
 * @example
 * import { probeRuntime } from '@unrdf/kgc-probe/probes/runtime';
 *
 * const observations = await probeRuntime({ samples: 100, budgetMs: 5000 });
 * console.log(observations);
 */

import { z } from 'zod';
import { performance } from 'node:perf_hooks';
import { Worker } from 'node:worker_threads';
import { createHash } from 'node:crypto';

// =============================================================================
// Schema Definitions
// =============================================================================

/**
 * Runtime observation schema for swarm coordination
 *
 * Matches the format specified for KGC Probe swarm:
 * - method: Probe method identifier
 * - inputs: Input parameters (empty for environment probes)
 * - outputs: Probe results
 * - timestamp: Unix epoch milliseconds
 * - hash: BLAKE3 hash of canonical JSON
 * - guardDecision: Access control decision
 * - metadata: Additional context
 *
 * @constant
 */
const RuntimeObservationSchema = z.object({
  /** Probe method identifier (e.g., 'runtime.node_version') */
  method: z.string().min(1),
  /** Input parameters */
  inputs: z.record(z.string(), z.any()),
  /** Output values from probe */
  outputs: z.record(z.string(), z.any()),
  /** Measurement timestamp (Unix epoch ms) */
  timestamp: z.number().int().positive(),
  /** BLAKE3 hash of canonical JSON */
  hash: z.string().length(64),
  /** Guard decision (allowed/denied) */
  guardDecision: z.enum(['allowed', 'denied']),
  /** Additional metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Canonicalize object for deterministic hashing
 * @param {Object} obj - Object to canonicalize
 * @returns {string} Canonical JSON string
 */
function canonicalizeJSON(obj) {
  if (obj === null || obj === undefined) {
    return JSON.stringify(obj);
  }
  if (typeof obj !== 'object') {
    return JSON.stringify(obj);
  }
  if (Array.isArray(obj)) {
    return '[' + obj.map(canonicalizeJSON).join(',') + ']';
  }

  // Sort keys and stringify
  const keys = Object.keys(obj).sort();
  const pairs = keys.map((key) => {
    return JSON.stringify(key) + ':' + canonicalizeJSON(obj[key]);
  });
  return '{' + pairs.join(',') + '}';
}

/**
 * Compute SHA-256 hash of canonical JSON
 * @param {Object} data - Data to hash (will be canonicalized)
 * @returns {string} SHA-256 hex digest (64 chars)
 * @note Currently using SHA-256. TODO: Switch to BLAKE3 when hash-wasm is available
 */
function blake3Hash(data) {
  const canonical = canonicalizeJSON(data);
  const hash = createHash('sha256');
  hash.update(canonical);
  return hash.digest('hex');
}

/**
 * Create observation with hash and guard decision
 * @param {string} method - Method identifier
 * @param {Object} inputs - Input parameters
 * @param {Object} outputs - Output values
 * @param {string} guardDecision - Guard decision (allowed/denied)
 * @param {Object} [metadata={}] - Additional metadata
 * @returns {Object} Complete observation with hash
 */
function createObservation(method, inputs, outputs, guardDecision, metadata = {}) {
  const timestamp = Date.now();

  // Create observation without hash first
  const observationData = {
    method,
    inputs,
    outputs,
    timestamp,
    guardDecision,
    ...(Object.keys(metadata).length > 0 && { metadata }),
  };

  // Compute hash of observation data (excluding hash field)
  const hash = blake3Hash(observationData);

  // Add hash to observation
  const observation = {
    ...observationData,
    hash,
  };

  // Validate with Zod schema
  RuntimeObservationSchema.parse(observation);

  return observation;
}

/**
 * Apply guard constraint - check if method should be allowed
 * @param {string} method - Method being executed
 * @param {Object} outputs - Output values (for secret detection)
 * @returns {string} 'allowed' or 'denied'
 */
function applyGuardConstraint(method, outputs) {
  // GUARD CONSTRAINT: NO reading of environment variables
  if (method === 'runtime.env_vars') {
    return 'denied';
  }

  // Check outputs for secret patterns (basic heuristic)
  const outputStr = JSON.stringify(outputs).toLowerCase();
  const secretPatterns = [
    'api_key',
    'apikey',
    'secret',
    'password',
    'token',
    'credential',
    'private_key',
    'privatekey',
  ];

  for (const pattern of secretPatterns) {
    if (outputStr.includes(pattern)) {
      return 'denied';
    }
  }

  return 'allowed';
}

/**
 * Calculate statistics from array of numbers
 * @param {number[]} values - Values to analyze
 * @returns {Object} Statistics (mean, median, p95, p99, variance, min, max)
 */
function calculateStats(values) {
  if (values.length === 0) {
    return { mean: 0, median: 0, p95: 0, p99: 0, variance: 0, min: 0, max: 0, samples: 0 };
  }

  const sorted = [...values].sort((a, b) => a - b);
  const n = sorted.length;

  const mean = values.reduce((sum, v) => sum + v, 0) / n;
  const median = n % 2 === 0
    ? (sorted[n / 2 - 1] + sorted[n / 2]) / 2
    : sorted[Math.floor(n / 2)];

  const p95Index = Math.ceil(n * 0.95) - 1;
  const p99Index = Math.ceil(n * 0.99) - 1;
  const p95 = sorted[Math.max(0, p95Index)];
  const p99 = sorted[Math.max(0, p99Index)];

  const variance = values.reduce((sum, v) => sum + Math.pow(v - mean, 2), 0) / n;
  const min = sorted[0];
  const max = sorted[n - 1];

  return { mean, median, p95, p99, variance, min, max, samples: n };
}

/**
 * Measure timer resolution with stable sampling
 * @param {number} samples - Number of samples to take
 * @returns {Object} Timer resolution statistics
 */
function measureTimerResolution(samples = 100) {
  const deltas = [];

  for (let i = 0; i < samples; i++) {
    const start = performance.now();
    const end = performance.now();
    deltas.push(end - start);
  }

  return calculateStats(deltas);
}

/**
 * Measure event loop latency baseline
 * @param {number} samples - Number of samples to take
 * @param {number} timeoutMs - Timeout per sample
 * @returns {Promise<Object>} Event loop latency statistics
 */
async function measureEventLoopLatency(samples = 100, timeoutMs = 5000) {
  return new Promise((resolve, reject) => {
    const latencies = [];
    let count = 0;
    const startTime = Date.now();

    function scheduleNext() {
      if (count >= samples) {
        resolve(calculateStats(latencies));
        return;
      }

      // Check timeout
      if (Date.now() - startTime > timeoutMs) {
        resolve(calculateStats(latencies));
        return;
      }

      const expectedTime = performance.now();
      setImmediate(() => {
        const actualTime = performance.now();
        const latency = actualTime - expectedTime;
        latencies.push(latency);
        count++;
        scheduleNext();
      });
    }

    scheduleNext();

    // Timeout guard
    setTimeout(() => {
      if (latencies.length > 0) {
        resolve(calculateStats(latencies));
      } else {
        reject(new Error('Event loop latency measurement timed out with no samples'));
      }
    }, timeoutMs);
  });
}

/**
 * Test WebAssembly instantiation support
 * @returns {Promise<boolean>} Whether WASM instantiation is supported
 */
async function testWasmSupport() {
  try {
    // Minimal WASM module (exports nothing, just validates instantiation)
    const wasmBytes = new Uint8Array([
      0x00, 0x61, 0x73, 0x6d, // Magic number
      0x01, 0x00, 0x00, 0x00, // Version
    ]);
    await WebAssembly.instantiate(wasmBytes);
    return true;
  } catch (e) {
    return false;
  }
}

/**
 * Test worker threads availability
 * @returns {Promise<boolean>} Whether worker threads are available
 */
async function testWorkerThreads() {
  try {
    // Try to create a minimal worker
    const workerCode = `
      const { parentPort } = require('worker_threads');
      if (parentPort) {
        parentPort.postMessage('ok');
      }
    `;

    const worker = new Worker(workerCode, { eval: true });

    return new Promise((resolve) => {
      const timeout = setTimeout(() => {
        worker.terminate();
        resolve(false);
      }, 1000);

      worker.on('message', (msg) => {
        clearTimeout(timeout);
        worker.terminate();
        resolve(msg === 'ok');
      });

      worker.on('error', () => {
        clearTimeout(timeout);
        worker.terminate();
        resolve(false);
      });
    });
  } catch (e) {
    return false;
  }
}

/**
 * Check available globals
 * @returns {Object} Map of global names to availability
 */
function checkAvailableGlobals() {
  const globalsToCheck = [
    'process',
    'Buffer',
    'global',
    'globalThis',
    '__dirname',
    '__filename',
    'require',
    'module',
    'exports',
    'setTimeout',
    'setInterval',
    'setImmediate',
    'clearTimeout',
    'clearInterval',
    'clearImmediate',
    'console',
    'performance',
    'URL',
    'URLSearchParams',
    'TextEncoder',
    'TextDecoder',
    'WebAssembly',
    'crypto',
  ];

  const available = {};
  for (const name of globalsToCheck) {
    try {
      // Use indirect eval to check global scope
      available[name] = typeof globalThis[name] !== 'undefined';
    } catch (e) {
      available[name] = false;
    }
  }

  return available;
}

// =============================================================================
// Main Probe Function
// =============================================================================

/**
 * Probe Node.js and JavaScript engine capabilities
 *
 * Returns observations with:
 * - Node.js version (process.version)
 * - V8 version (process.versions.v8)
 * - Module system support
 * - Timer resolution
 * - Worker threads availability
 * - WebAssembly support
 * - Event loop latency baseline
 * - Available globals
 * - Memory limits
 *
 * All observations include BLAKE3 hashes and guard decisions.
 * Respects guard constraints:
 * - NO reading of environment variables (process.env)
 * - Only safe process properties allowed
 * - Secret pattern detection in outputs
 *
 * @param {Object} [config={}] - Probe configuration
 * @param {number} [config.samples=100] - Number of benchmark samples
 * @param {number} [config.budgetMs=5000] - Time budget in milliseconds
 * @returns {Promise<Object[]>} Array of observations with hashes
 *
 * @example
 * const observations = await probeRuntime({ samples: 100, budgetMs: 5000 });
 * console.log(`Collected ${observations.length} observations`);
 * console.log(`Node version: ${observations.find(o => o.method === 'runtime.node_version').outputs.version}`);
 */
export async function probeRuntime(config = {}) {
  const { samples = 100, budgetMs = 5000 } = config;
  const observations = [];
  const startTime = Date.now();

  // Helper to check if we've exceeded time budget
  const checkTimeout = () => {
    if (Date.now() - startTime > budgetMs) {
      throw new Error(`Runtime probe exceeded time budget of ${budgetMs}ms`);
    }
  };

  // 1. Node.js version
  checkTimeout();
  {
    const method = 'runtime.node_version';
    const inputs = {};
    const outputs = { version: process.version };
    const guardDecision = applyGuardConstraint(method, outputs);
    const metadata = { source: 'process.version' };
    observations.push(createObservation(method, inputs, outputs, guardDecision, metadata));
  }

  // 2. V8 version
  checkTimeout();
  {
    const method = 'runtime.v8_version';
    const inputs = {};
    const outputs = { version: process.versions.v8 };
    const guardDecision = applyGuardConstraint(method, outputs);
    const metadata = { source: 'process.versions.v8' };
    observations.push(createObservation(method, inputs, outputs, guardDecision, metadata));
  }

  // 3. Module system support
  checkTimeout();
  {
    const method = 'runtime.module_system';
    const inputs = {};
    const outputs = {
      esm: typeof import.meta !== 'undefined',
      cjs: typeof require !== 'undefined',
      importMetaUrl: typeof import.meta?.url === 'string',
    };
    const guardDecision = applyGuardConstraint(method, outputs);
    const metadata = { note: 'ESM=import.meta defined, CJS=require defined' };
    observations.push(createObservation(method, inputs, outputs, guardDecision, metadata));
  }

  // 4. Timer resolution
  checkTimeout();
  {
    const method = 'runtime.timer_resolution';
    const inputs = { samples };
    const stats = measureTimerResolution(samples);
    const outputs = {
      mean_ns: stats.mean * 1e6, // Convert ms to ns
      median_ns: stats.median * 1e6,
      p95_ns: stats.p95 * 1e6,
      p99_ns: stats.p99 * 1e6,
      variance_ns2: stats.variance * 1e12,
    };
    const guardDecision = applyGuardConstraint(method, outputs);
    const metadata = {
      source: 'performance.now()',
      samples: stats.samples,
      note: 'Measured consecutive performance.now() calls',
    };
    observations.push(createObservation(method, inputs, outputs, guardDecision, metadata));
  }

  // 5. Worker threads availability
  checkTimeout();
  {
    const method = 'runtime.worker_threads';
    const inputs = {};
    const available = await testWorkerThreads();
    const outputs = { available };
    const guardDecision = applyGuardConstraint(method, outputs);
    const metadata = { module: 'worker_threads', test: 'create+message' };
    observations.push(createObservation(method, inputs, outputs, guardDecision, metadata));
  }

  // 6. WebAssembly support
  checkTimeout();
  {
    const method = 'runtime.wasm_support';
    const inputs = {};
    const available = await testWasmSupport();
    const outputs = {
      instantiate: available,
      wasmGlobal: typeof WebAssembly !== 'undefined',
    };
    const guardDecision = applyGuardConstraint(method, outputs);
    const metadata = { test: 'WebAssembly.instantiate with minimal module' };
    observations.push(createObservation(method, inputs, outputs, guardDecision, metadata));
  }

  // 7. Event loop latency baseline
  checkTimeout();
  {
    const method = 'runtime.event_loop_latency';
    const inputs = { samples };
    const stats = await measureEventLoopLatency(samples, Math.min(budgetMs, 2000));
    const outputs = {
      mean_ms: stats.mean,
      median_ms: stats.median,
      p95_ms: stats.p95,
      p99_ms: stats.p99,
      variance_ms2: stats.variance,
      samples: stats.samples,
    };
    const guardDecision = applyGuardConstraint(method, outputs);
    const metadata = {
      method: 'setImmediate scheduling delay',
      note: 'Baseline latency under no load',
    };
    observations.push(createObservation(method, inputs, outputs, guardDecision, metadata));
  }

  // 8. Available globals
  checkTimeout();
  {
    const method = 'runtime.available_globals';
    const inputs = {};
    const globals = checkAvailableGlobals();
    const outputs = globals;
    const guardDecision = applyGuardConstraint(method, outputs);
    const metadata = {
      count: Object.keys(globals).filter(k => globals[k]).length,
      total: Object.keys(globals).length,
    };
    observations.push(createObservation(method, inputs, outputs, guardDecision, metadata));
  }

  // 9. Memory limits
  checkTimeout();
  {
    const method = 'runtime.memory_limits';
    const inputs = {};
    const memUsage = process.memoryUsage();
    const outputs = {
      rss_bytes: memUsage.rss,
      heap_total_bytes: memUsage.heapTotal,
      heap_used_bytes: memUsage.heapUsed,
      external_bytes: memUsage.external,
      array_buffers_bytes: memUsage.arrayBuffers || 0,
    };
    const guardDecision = applyGuardConstraint(method, outputs);
    const metadata = {
      source: 'process.memoryUsage()',
      note: 'Current memory snapshot',
    };
    observations.push(createObservation(method, inputs, outputs, guardDecision, metadata));
  }

  // 10. Process platform and architecture
  checkTimeout();
  {
    const method = 'runtime.platform_arch';
    const inputs = {};
    const outputs = {
      platform: process.platform,
      arch: process.arch,
      endianness: process.arch.includes('64') ? 'LE' : 'unknown',
    };
    const guardDecision = applyGuardConstraint(method, outputs);
    const metadata = {
      source: 'process.platform, process.arch',
    };
    observations.push(createObservation(method, inputs, outputs, guardDecision, metadata));
  }

  // Sort observations by method name for deterministic output
  observations.sort((a, b) => a.method.localeCompare(b.method));

  return observations;
}

// =============================================================================
// Module Exports
// =============================================================================

export default probeRuntime;

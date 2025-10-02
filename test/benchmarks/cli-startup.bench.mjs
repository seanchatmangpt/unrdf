/**
 * @fileoverview CLI Startup Performance Benchmark
 *
 * @description
 * Measures CLI cold start time to validate < 100ms target.
 * This is CRITICAL for user experience - first impression matters.
 *
 * Performance Target:
 * - Cold start: < 100ms (v3 target)
 * - Warm start: < 50ms
 * - Import time: < 30ms
 *
 * Current v1.0 CLI: 487ms cold start ❌ (needs 5x improvement)
 */

import { bench, describe } from 'vitest';
import { exec, spawn } from 'node:child_process';
import { promisify } from 'node:util';
import { performance } from 'node:perf_hooks';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const execAsync = promisify(exec);
const __dirname = dirname(fileURLToPath(import.meta.url));
const CLI_PATH = join(__dirname, '../../src/cli.mjs');

/**
 * Measure process spawn time (cold start)
 * @returns {Promise<number>} Duration in milliseconds
 */
async function measureColdStart() {
  return new Promise((resolve, reject) => {
    const start = performance.now();
    const child = spawn('node', [CLI_PATH, '--version'], {
      stdio: 'pipe',
      env: { ...process.env, NODE_ENV: 'production' }
    });

    let output = '';
    child.stdout.on('data', (data) => {
      output += data.toString();
    });

    child.on('close', (code) => {
      const duration = performance.now() - start;
      if (code === 0 || output.includes('1.0.0')) {
        resolve(duration);
      } else {
        reject(new Error(`CLI exited with code ${code}`));
      }
    });

    child.on('error', reject);
  });
}

/**
 * Measure module import time (warm start)
 * @returns {Promise<number>} Duration in milliseconds
 */
async function measureImportTime() {
  const start = performance.now();

  // Dynamic import to avoid caching
  const timestamp = Date.now();
  await import(`${CLI_PATH}?t=${timestamp}`);

  return performance.now() - start;
}

describe('CLI Startup Performance', () => {
  // Benchmark cold start (process spawn)
  bench('CLI cold start (--version)', async () => {
    await measureColdStart();
  }, {
    iterations: 10,
    time: 5000, // 5 seconds
    warmupIterations: 2,
    warmupTime: 1000 // 1 second
  });

  // Benchmark warm start (module import)
  bench('CLI module import', async () => {
    await measureImportTime();
  }, {
    iterations: 20,
    time: 5000,
    warmupIterations: 5,
    warmupTime: 1000
  });

  // Benchmark help command (common first interaction)
  bench('CLI help command', async () => {
    const start = performance.now();
    await execAsync(`node ${CLI_PATH} --help`);
    return performance.now() - start;
  }, {
    iterations: 10,
    time: 5000,
    warmupIterations: 2
  });
});

describe('CLI Performance Analysis', () => {
  bench('Measure startup components', async () => {
    const measurements = {
      processSpawn: 0,
      moduleLoad: 0,
      contextInit: 0,
      total: 0
    };

    // Total time
    const totalStart = performance.now();

    // Process spawn time
    const spawnStart = performance.now();
    const result = await execAsync(`node ${CLI_PATH} --version`);
    measurements.processSpawn = performance.now() - spawnStart;

    // Module load time (import)
    const importStart = performance.now();
    await import(`${CLI_PATH}?t=${Date.now()}`);
    measurements.moduleLoad = performance.now() - importStart;

    measurements.total = performance.now() - totalStart;

    // Log breakdown for analysis
    console.log('\nCLI Startup Breakdown:');
    console.log(`  Process spawn: ${measurements.processSpawn.toFixed(2)}ms`);
    console.log(`  Module load:   ${measurements.moduleLoad.toFixed(2)}ms`);
    console.log(`  Total:         ${measurements.total.toFixed(2)}ms`);
    console.log(`  Target:        100ms`);
    console.log(`  Status:        ${measurements.total < 100 ? '✅ PASS' : '❌ FAIL'}`);
  }, {
    iterations: 5,
    time: 10000
  });
});

describe('CLI Performance Targets Validation', () => {
  bench('Validate < 100ms cold start target', async () => {
    const durations = [];

    // Run 10 cold starts to get statistical distribution
    for (let i = 0; i < 10; i++) {
      const duration = await measureColdStart();
      durations.push(duration);
    }

    // Calculate percentiles
    const sorted = durations.sort((a, b) => a - b);
    const p50 = sorted[Math.floor(sorted.length * 0.50)];
    const p95 = sorted[Math.floor(sorted.length * 0.95)];
    const p99 = sorted[Math.floor(sorted.length * 0.99)];
    const mean = durations.reduce((sum, d) => sum + d, 0) / durations.length;

    console.log('\nCLI Cold Start Performance:');
    console.log(`  Mean: ${mean.toFixed(2)}ms`);
    console.log(`  P50:  ${p50.toFixed(2)}ms`);
    console.log(`  P95:  ${p95.toFixed(2)}ms`);
    console.log(`  P99:  ${p99.toFixed(2)}ms`);
    console.log(`  Target: < 100ms`);
    console.log(`  Status: ${p99 < 100 ? '✅ PASS' : '❌ FAIL (needs optimization)'}`);

    // Return mean for benchmark reporting
    return mean;
  }, {
    iterations: 3,
    time: 30000 // 30 seconds
  });
});

/**
 * Optimization recommendations based on benchmark results:
 *
 * 1. LAZY LOADING (estimated 200ms+ savings):
 *    - Defer Comunica import until query command
 *    - Defer SHACL validator import until validate command
 *    - Use dynamic imports for non-core functionality
 *
 * 2. CACHING (estimated 50ms+ savings):
 *    - Cache parsed config files
 *    - Reuse QueryEngine instances
 *    - Pre-compile common SPARQL queries
 *
 * 3. BUNDLING (estimated 100ms+ savings):
 *    - Bundle CLI with esbuild/rollup
 *    - Tree-shake unused dependencies
 *    - Minimize dependency tree depth
 *
 * 4. SIDECAR MODE (estimated 400ms+ savings):
 *    - Offload heavy operations to sidecar
 *    - CLI becomes thin client
 *    - Near-instant startup for most commands
 */

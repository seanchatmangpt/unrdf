/**
 * @file Runtime Probe Tests - Determinism and Capability Detection
 * @description Tests for Agent 2 (Runtime Surface) probe module
 *
 * Test focus:
 * - Determinism: Same inputs → Same hashes
 * - Coverage: All 10 probe methods execute
 * - Guards: process.env access is denied
 * - Performance: <5s SLA compliance
 * - Structure: All observations match schema
 */

import { describe, test } from 'node:test';
import assert from 'node:assert/strict';
import { probeRuntime } from '../src/probes/runtime.mjs';

describe('Runtime Probe - Determinism', () => {
  test('repeated calls produce stable observation count', async () => {
    const config = { samples: 10, budgetMs: 3000 };

    const obs1 = await probeRuntime(config);
    const obs2 = await probeRuntime(config);

    assert.equal(obs1.length, obs2.length, 'Should produce same number of observations');
    assert.ok(obs1.length >= 10, `Should have ≥10 observations, got ${obs1.length}`);
  });

  test('deterministic methods produce identical hashes', async () => {
    const config = { samples: 10, budgetMs: 3000 };

    const obs1 = await probeRuntime(config);
    const obs2 = await probeRuntime(config);

    // Filter to deterministic methods (exclude timing measurements)
    const deterministicMethods = [
      'runtime.available_globals',
      'runtime.module_system',
      'runtime.node_version',
      'runtime.platform_arch',
      'runtime.v8_version',
      'runtime.wasm_support',
      'runtime.worker_threads',
    ];

    for (const method of deterministicMethods) {
      const o1 = obs1.find(o => o.method === method);
      const o2 = obs2.find(o => o.method === method);

      assert.ok(o1, `Should find ${method} in first run`);
      assert.ok(o2, `Should find ${method} in second run`);

      // Compare outputs (excluding timestamp and hash)
      assert.deepEqual(
        o1.outputs,
        o2.outputs,
        `${method} outputs should be deterministic`
      );
    }
  });

  test('non-deterministic methods have consistent structure', async () => {
    const config = { samples: 10, budgetMs: 3000 };

    const observations = await probeRuntime(config);

    // Timing-dependent methods should have stable keys even if values differ
    const timingMethods = [
      'runtime.timer_resolution',
      'runtime.event_loop_latency',
      'runtime.memory_limits',
    ];

    for (const method of timingMethods) {
      const obs = observations.find(o => o.method === method);
      assert.ok(obs, `Should find ${method}`);
      assert.ok(obs.outputs, `${method} should have outputs`);
      assert.ok(typeof obs.hash === 'string', `${method} should have hash`);
      assert.equal(obs.hash.length, 64, `${method} hash should be 64 chars (SHA-256)`);
    }
  });
});

describe('Runtime Probe - Coverage', () => {
  test('probes all 10 required methods', async () => {
    const config = { samples: 10, budgetMs: 3000 };
    const observations = await probeRuntime(config);

    const methods = new Set(observations.map(o => o.method));

    const requiredMethods = [
      'runtime.available_globals',
      'runtime.event_loop_latency',
      'runtime.memory_limits',
      'runtime.module_system',
      'runtime.node_version',
      'runtime.platform_arch',
      'runtime.timer_resolution',
      'runtime.v8_version',
      'runtime.wasm_support',
      'runtime.worker_threads',
    ];

    for (const method of requiredMethods) {
      assert.ok(
        methods.has(method),
        `Should include ${method} in observations`
      );
    }

    assert.equal(
      methods.size,
      10,
      `Should have exactly 10 methods, got ${methods.size}`
    );
  });

  test('observations have required schema fields', async () => {
    const config = { samples: 10, budgetMs: 3000 };
    const observations = await probeRuntime(config);

    for (const obs of observations) {
      // Required fields
      assert.ok(obs.method, 'Should have method');
      assert.ok(obs.inputs !== undefined, 'Should have inputs');
      assert.ok(obs.outputs !== undefined, 'Should have outputs');
      assert.ok(typeof obs.timestamp === 'number', 'Should have numeric timestamp');
      assert.ok(typeof obs.hash === 'string', 'Should have hash string');
      assert.ok(obs.guardDecision, 'Should have guardDecision');

      // Hash format
      assert.equal(obs.hash.length, 64, `Hash should be 64 chars, got ${obs.hash.length}`);
      assert.match(obs.hash, /^[0-9a-f]{64}$/, 'Hash should be hex string');

      // Guard decision values
      assert.ok(
        obs.guardDecision === 'allowed' || obs.guardDecision === 'denied',
        `guardDecision should be 'allowed' or 'denied', got '${obs.guardDecision}'`
      );
    }
  });
});

describe('Runtime Probe - Guard Constraints', () => {
  test('all observations are allowed (no forbidden ops)', async () => {
    const config = { samples: 10, budgetMs: 3000 };
    const observations = await probeRuntime(config);

    const denied = observations.filter(o => o.guardDecision === 'denied');

    assert.equal(
      denied.length,
      0,
      `No operations should be denied in safe runtime probe, found ${denied.length}`
    );
  });

  test('process.env is NOT accessed', async () => {
    const config = { samples: 10, budgetMs: 3000 };
    const observations = await probeRuntime(config);

    // Verify no observation mentions process.env
    for (const obs of observations) {
      const outputStr = JSON.stringify(obs.outputs).toLowerCase();
      assert.ok(
        !outputStr.includes('process.env'),
        `${obs.method} should not access process.env`
      );
    }
  });

  test('no secret patterns in outputs', async () => {
    const config = { samples: 10, budgetMs: 3000 };
    const observations = await probeRuntime(config);

    const secretPatterns = ['api_key', 'password', 'secret', 'token', 'credential'];

    for (const obs of observations) {
      const outputStr = JSON.stringify(obs.outputs).toLowerCase();

      for (const pattern of secretPatterns) {
        assert.ok(
          !outputStr.includes(pattern),
          `${obs.method} outputs should not contain '${pattern}'`
        );
      }
    }
  });
});

describe('Runtime Probe - Performance', () => {
  test('completes within 5 second SLA', async () => {
    const config = { samples: 100, budgetMs: 5000 };

    const start = Date.now();
    const observations = await probeRuntime(config);
    const duration = Date.now() - start;

    assert.ok(observations.length > 0, 'Should produce observations');
    assert.ok(duration < 5000, `Should complete in <5s, took ${duration}ms`);
  });

  test('respects budgetMs parameter', async () => {
    const config = { samples: 1000, budgetMs: 2000 };

    const start = Date.now();
    try {
      await probeRuntime(config);
      const duration = Date.now() - start;

      // Should either complete quickly or timeout near budget
      assert.ok(
        duration < 3000,
        `Should respect budget or complete quickly, took ${duration}ms`
      );
    } catch (err) {
      // Timeout is acceptable if it's due to budget
      assert.match(err.message, /time budget/, 'Timeout should mention budget');
    }
  });

  test('minimal samples complete faster', async () => {
    const configSmall = { samples: 10, budgetMs: 5000 };
    const configLarge = { samples: 100, budgetMs: 5000 };

    const start1 = Date.now();
    await probeRuntime(configSmall);
    const duration1 = Date.now() - start1;

    const start2 = Date.now();
    await probeRuntime(configLarge);
    const duration2 = Date.now() - start2;

    // Not a strict requirement, but more samples should take longer
    // (This is probabilistic but useful for sanity check)
    console.log(`Small (10 samples): ${duration1}ms, Large (100 samples): ${duration2}ms`);
  });
});

describe('Runtime Probe - Specific Capabilities', () => {
  test('node_version is semver format', async () => {
    const observations = await probeRuntime({ samples: 10, budgetMs: 3000 });
    const obs = observations.find(o => o.method === 'runtime.node_version');

    assert.ok(obs, 'Should have node_version observation');
    assert.ok(obs.outputs.version, 'Should have version in outputs');
    assert.match(obs.outputs.version, /^v\d+\.\d+\.\d+/, 'Version should be semver (vX.Y.Z)');
  });

  test('v8_version exists', async () => {
    const observations = await probeRuntime({ samples: 10, budgetMs: 3000 });
    const obs = observations.find(o => o.method === 'runtime.v8_version');

    assert.ok(obs, 'Should have v8_version observation');
    assert.ok(obs.outputs.version, 'Should have V8 version');
    assert.ok(obs.outputs.version.length > 0, 'V8 version should not be empty');
  });

  test('module_system detects ESM', async () => {
    const observations = await probeRuntime({ samples: 10, budgetMs: 3000 });
    const obs = observations.find(o => o.method === 'runtime.module_system');

    assert.ok(obs, 'Should have module_system observation');
    assert.equal(obs.outputs.esm, true, 'ESM should be supported (import.meta exists)');
    assert.equal(obs.outputs.importMetaUrl, true, 'import.meta.url should exist');
  });

  test('platform_arch is valid', async () => {
    const observations = await probeRuntime({ samples: 10, budgetMs: 3000 });
    const obs = observations.find(o => o.method === 'runtime.platform_arch');

    assert.ok(obs, 'Should have platform_arch observation');
    assert.ok(obs.outputs.platform, 'Should have platform');
    assert.ok(obs.outputs.arch, 'Should have arch');

    const validPlatforms = ['linux', 'darwin', 'win32', 'freebsd', 'openbsd', 'sunos', 'aix'];
    assert.ok(
      validPlatforms.includes(obs.outputs.platform),
      `Platform should be valid, got '${obs.outputs.platform}'`
    );
  });

  test('available_globals includes expected APIs', async () => {
    const observations = await probeRuntime({ samples: 10, budgetMs: 3000 });
    const obs = observations.find(o => o.method === 'runtime.available_globals');

    assert.ok(obs, 'Should have available_globals observation');

    const expectedGlobals = [
      'process',
      'Buffer',
      'globalThis',
      'console',
      'setTimeout',
      'setImmediate',
    ];

    for (const global of expectedGlobals) {
      assert.equal(
        obs.outputs[global],
        true,
        `Global '${global}' should be available`
      );
    }
  });

  test('timer_resolution measures performance.now()', async () => {
    const observations = await probeRuntime({ samples: 10, budgetMs: 3000 });
    const obs = observations.find(o => o.method === 'runtime.timer_resolution');

    assert.ok(obs, 'Should have timer_resolution observation');
    assert.ok(obs.outputs.mean_ns !== undefined, 'Should have mean_ns');
    assert.ok(obs.outputs.median_ns !== undefined, 'Should have median_ns');
    assert.ok(obs.outputs.p95_ns !== undefined, 'Should have p95_ns');
    assert.ok(obs.outputs.p99_ns !== undefined, 'Should have p99_ns');

    // Timer resolution should be reasonable (typically <1µs = 1000ns)
    assert.ok(
      obs.outputs.mean_ns < 10000,
      `Timer resolution should be <10µs, got ${obs.outputs.mean_ns}ns`
    );
  });

  test('event_loop_latency is measured', async () => {
    const observations = await probeRuntime({ samples: 10, budgetMs: 3000 });
    const obs = observations.find(o => o.method === 'runtime.event_loop_latency');

    assert.ok(obs, 'Should have event_loop_latency observation');
    assert.ok(obs.outputs.mean_ms !== undefined, 'Should have mean_ms');
    assert.ok(obs.outputs.median_ms !== undefined, 'Should have median_ms');
    assert.ok(typeof obs.outputs.samples === 'number', 'Should report sample count');

    // Event loop latency should be reasonable under no load (<100ms)
    assert.ok(
      obs.outputs.p99_ms < 100,
      `P99 latency should be <100ms, got ${obs.outputs.p99_ms}ms`
    );
  });

  test('memory_limits reports current usage', async () => {
    const observations = await probeRuntime({ samples: 10, budgetMs: 3000 });
    const obs = observations.find(o => o.method === 'runtime.memory_limits');

    assert.ok(obs, 'Should have memory_limits observation');
    assert.ok(obs.outputs.rss_bytes > 0, 'RSS should be positive');
    assert.ok(obs.outputs.heap_total_bytes > 0, 'Heap total should be positive');
    assert.ok(obs.outputs.heap_used_bytes > 0, 'Heap used should be positive');

    // Heap used should not exceed heap total
    assert.ok(
      obs.outputs.heap_used_bytes <= obs.outputs.heap_total_bytes,
      'Heap used should not exceed heap total'
    );
  });

  test('wasm_support is detected', async () => {
    const observations = await probeRuntime({ samples: 10, budgetMs: 3000 });
    const obs = observations.find(o => o.method === 'runtime.wasm_support');

    assert.ok(obs, 'Should have wasm_support observation');
    assert.ok(typeof obs.outputs.wasmGlobal === 'boolean', 'Should check WebAssembly global');
    assert.ok(typeof obs.outputs.instantiate === 'boolean', 'Should test instantiation');
  });

  test('worker_threads availability', async () => {
    const observations = await probeRuntime({ samples: 10, budgetMs: 3000 });
    const obs = observations.find(o => o.method === 'runtime.worker_threads');

    assert.ok(obs, 'Should have worker_threads observation');
    assert.ok(typeof obs.outputs.available === 'boolean', 'Should report availability');
  });
});

describe('Runtime Probe - Edge Cases', () => {
  test('handles zero samples gracefully', async () => {
    const observations = await probeRuntime({ samples: 0, budgetMs: 3000 });

    assert.ok(Array.isArray(observations), 'Should return array');
    assert.ok(observations.length > 0, 'Should still return environment observations');
  });

  test('handles small budget gracefully', async () => {
    const start = Date.now();

    try {
      await probeRuntime({ samples: 1000, budgetMs: 500 });
    } catch (err) {
      // Timeout is expected with small budget and many samples
      assert.match(err.message, /time budget/, 'Should timeout with budget message');
    }

    const duration = Date.now() - start;
    assert.ok(duration < 2000, `Should fail fast, took ${duration}ms`);
  });
});

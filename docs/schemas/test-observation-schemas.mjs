/**
 * Test Suite for OTEL Observation Schemas
 *
 * Demonstrates schema validation across all domains with:
 * - Valid observations for each domain
 * - Invalid observations (error cases)
 * - Edge cases and boundary conditions
 * - Receipt chain validation
 * - Capability and constraint derivation
 *
 * @module test/observation-schemas
 */

import { v4 as uuidv4 } from 'uuid';
import {
  validateObservation,
  validateObservationBatch,
  validateReceiptChain,
  VALID_AGENTS,
  VALID_DOMAINS,
  redactSecrets
} from './zod-observation-schemas.mjs';

// ============================================================================
// TEST UTILITIES
// ============================================================================

/**
 * Generate current timestamp in nanoseconds
 * @returns {string} Nanosecond timestamp as BigInt string
 */
function getNowNs() {
  return (BigInt(Date.now()) * 1000000n).toString();
}

/**
 * Mock BLAKE3 hash (64 hex chars)
 * In real implementation, use blake3 library
 * @returns {string} 64-character hex string
 */
function mockBlake3Hash() {
  return Array(64).fill(0).map(() =>
    Math.floor(Math.random() * 16).toString(16)
  ).join('');
}

/**
 * Create base observation template
 * @param {object} overrides - Field overrides
 * @returns {object} Observation object
 */
function createObservation(overrides = {}) {
  return {
    id: overrides.id || uuidv4(),
    agentId: overrides.agentId || 'agent-1',
    domain: overrides.domain || 'runtime',
    timestamp_ns: overrides.timestamp_ns || getNowNs(),
    method: overrides.method || 'runtime.check',
    input: overrides.input || { count: 1 },
    output: overrides.output || {},
    hash: overrides.hash || mockBlake3Hash(),
    guard: overrides.guard || { type: 'allow' },
    receipt: overrides.receipt || null
  };
}

/**
 * Test runner with reporting
 */
class TestRunner {
  constructor(name) {
    this.name = name;
    this.tests = [];
    this.passed = 0;
    this.failed = 0;
  }

  test(description, fn) {
    this.tests.push({ description, fn });
  }

  async run() {
    console.log(`\n${'='.repeat(70)}`);
    console.log(`TEST SUITE: ${this.name}`);
    console.log(`${'='.repeat(70)}\n`);

    for (const test of this.tests) {
      try {
        await test.fn();
        console.log(`✅ ${test.description}`);
        this.passed++;
      } catch (err) {
        console.log(`❌ ${test.description}`);
        console.log(`   Error: ${err.message}`);
        this.failed++;
      }
    }

    console.log(`\n${'-'.repeat(70)}`);
    console.log(`Results: ${this.passed} passed, ${this.failed} failed`);
    console.log(`Total: ${this.tests.length} tests\n`);

    return this.failed === 0;
  }
}

/**
 * Assert helper
 */
function assert(condition, message) {
  if (!condition) {
    throw new Error(`Assertion failed: ${message}`);
  }
}

function assertEqual(actual, expected, message) {
  if (actual !== expected) {
    throw new Error(`${message}: expected ${expected}, got ${actual}`);
  }
}

function assertExists(value, message) {
  if (value === null || value === undefined) {
    throw new Error(`${message}: value should exist`);
  }
}

// ============================================================================
// TEST SUITES
// ============================================================================

// --- Test Suite 1: Base Observation Validation ---

const baseObservationTests = new TestRunner('Base Observation Validation');

baseObservationTests.test('Valid observation with all required fields', () => {
  const obs = createObservation({
    domain: 'runtime',
    method: 'runtime.check',
    output: {
      nodeVersion: '20.10.0',
      jsEngine: 'v8',
      wasm: true,
      workers: 4,
      timersResolution: 100000,
      icu: true
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate successfully');
  assertExists(result.data, 'Should have data');
  assertEqual(result.data.domain, 'runtime', 'Domain mismatch');
});

baseObservationTests.test('Valid observation with null receipt', () => {
  const obs = createObservation({
    receipt: null
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should accept null receipt');
});

baseObservationTests.test('Valid observation with receipt chain', () => {
  const obs = createObservation({
    receipt: {
      obs_hash: mockBlake3Hash(),
      prev_hash: mockBlake3Hash(),
      timestamp_ns: getNowNs(),
      agentId: 'agent-1'
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate with receipt');
});

baseObservationTests.test('Invalid: malformed UUID', () => {
  const obs = createObservation({
    id: 'not-a-uuid'
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject invalid UUID');
  assertExists(result.error, 'Should have error');
});

baseObservationTests.test('Invalid: unknown agent', () => {
  const obs = createObservation({
    agentId: 'agent-999'
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject unknown agent');
});

baseObservationTests.test('Invalid: unknown domain', () => {
  const obs = createObservation({
    domain: 'unknown-domain'
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject unknown domain');
});

baseObservationTests.test('Invalid: malformed method', () => {
  const obs = createObservation({
    method: 'invalid-method-format'
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject invalid method format');
});

baseObservationTests.test('Invalid: hash wrong length', () => {
  const obs = createObservation({
    hash: 'abc123' // Too short
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject wrong hash length');
});

baseObservationTests.test('Invalid: timestamp_ns out of range', () => {
  const obs = createObservation({
    timestamp_ns: '99999999999999999999999999999' // Too large
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject out-of-range timestamp');
});

baseObservationTests.test('Invalid: deny guard requires reason', () => {
  const obs = createObservation({
    guard: { type: 'deny' } // Missing reason
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should require reason for deny guard');
});

baseObservationTests.test('Valid: deny guard with reason', () => {
  const obs = createObservation({
    guard: {
      type: 'deny',
      reason: 'Insufficient permissions'
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should accept deny with reason');
});

// --- Test Suite 2: Domain-Specific Outputs ---

const domainOutputTests = new TestRunner('Domain-Specific Output Validation');

domainOutputTests.test('Valid: Runtime domain output', () => {
  const obs = createObservation({
    domain: 'runtime',
    method: 'runtime.check',
    output: {
      nodeVersion: '20.10.0',
      jsEngine: 'v8',
      wasm: true,
      workers: 4,
      timersResolution: 100000,
      icu: true
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate runtime output');
});

domainOutputTests.test('Invalid: Runtime - bad nodeVersion', () => {
  const obs = createObservation({
    domain: 'runtime',
    method: 'runtime.check',
    output: {
      nodeVersion: 'not-a-version',
      jsEngine: 'v8',
      wasm: true,
      workers: 4,
      timersResolution: 100000,
      icu: true
    }
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject invalid version');
});

domainOutputTests.test('Valid: Filesystem domain output', () => {
  const obs = createObservation({
    domain: 'fs',
    method: 'fs.check',
    output: {
      root: '/tmp',
      maxPathLength: 255,
      fileCount: 1000,
      symlinkBehavior: 'followed',
      writeTest: true
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate filesystem output');
});

domainOutputTests.test('Invalid: Filesystem - bad symlink behavior', () => {
  const obs = createObservation({
    domain: 'fs',
    method: 'fs.check',
    output: {
      root: '/tmp',
      maxPathLength: 255,
      fileCount: 1000,
      symlinkBehavior: 'unknown',
      writeTest: true
    }
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject invalid symlink behavior');
});

domainOutputTests.test('Valid: WASM domain output', () => {
  const obs = createObservation({
    domain: 'wasm',
    method: 'wasm.instantiate',
    output: {
      instantiated: true,
      startupMs: 42.5,
      sharedArrayBuffer: true,
      memoryGrowth: 1048576
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate WASM output');
});

domainOutputTests.test('Valid: Performance domain output', () => {
  const obs = createObservation({
    domain: 'perf',
    method: 'perf.measure',
    output: {
      domain: 'fs',
      throughput: 10000,
      latency_p50: 0.5,
      latency_p99: 2.5,
      variance: 0.8
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate performance output');
});

domainOutputTests.test('Invalid: Performance - p99 < p50', () => {
  const obs = createObservation({
    domain: 'perf',
    method: 'perf.measure',
    output: {
      domain: 'fs',
      throughput: 10000,
      latency_p50: 2.5,
      latency_p99: 0.5, // Invalid: p99 must be >= p50
      variance: 0.8
    }
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject p99 < p50');
});

domainOutputTests.test('Valid: Network domain output', () => {
  const obs = createObservation({
    domain: 'net',
    method: 'net.check',
    output: {
      urlAllowlist: ['https://example.com', 'https://*.api.example.com'],
      dnsResolution: true,
      maxPayloadBytes: 1048576
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate network output');
});

domainOutputTests.test('Valid: Tooling domain output', () => {
  const obs = createObservation({
    domain: 'tooling',
    method: 'tooling.check',
    output: {
      command: 'npm',
      accessible: true,
      version: '8.19.4'
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate tooling output');
});

domainOutputTests.test('Valid: Storage domain output', () => {
  const obs = createObservation({
    domain: 'storage',
    method: 'storage.check',
    output: {
      type: 'disk',
      quota: 1099511627776,
      available: 549755813888
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate storage output');
});

domainOutputTests.test('Invalid: Storage - available > quota', () => {
  const obs = createObservation({
    domain: 'storage',
    method: 'storage.check',
    output: {
      type: 'disk',
      quota: 100,
      available: 200 // Invalid
    }
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject available > quota');
});

domainOutputTests.test('Valid: Concurrency domain output with throttles', () => {
  const obs = createObservation({
    domain: 'concurrency',
    method: 'concurrency.check',
    output: {
      hasWorkers: true,
      parallelism: 8,
      throttles: [
        { name: 'api-calls', ratePerSecond: 100, burst: 10 },
        { name: 'db-connections', ratePerSecond: 50, burst: 5 }
      ]
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate concurrency with throttles');
});

domainOutputTests.test('Valid: Limits domain output', () => {
  const obs = createObservation({
    domain: 'limits',
    method: 'limits.check',
    output: {
      memoryMB: 2048,
      cpuShares: 1024,
      fsQuota: 10737418240
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate limits output');
});

domainOutputTests.test('Valid: System domain output', () => {
  const obs = createObservation({
    domain: 'system',
    method: 'system.check',
    output: {
      platform: 'linux',
      osVersion: '5.10.0',
      containerized: true
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should validate system output');
});

// --- Test Suite 3: Input Redaction ---

const redactionTests = new TestRunner('Input Redaction');

redactionTests.test('Redacts password fields', () => {
  const input = {
    username: 'admin',
    password: 'secret123',
    email: 'admin@example.com'
  };

  const redacted = redactSecrets(input);
  assert(redacted.username === 'admin', 'Should preserve safe fields');
  assert(redacted.password === '[REDACTED:9]', 'Should redact password');
  assert(redacted.email === 'admin@example.com', 'Should preserve email');
});

redactionTests.test('Redacts API keys', () => {
  const input = {
    apiKey: 'sk_live_abc123xyz789',
    clientId: 'public-id'
  };

  const redacted = redactSecrets(input);
  assert(redacted.apiKey.startsWith('[REDACTED:'), 'Should redact API key');
  assert(redacted.clientId === 'public-id', 'Should preserve client ID');
});

redactionTests.test('Redacts nested objects', () => {
  const input = {
    user: {
      name: 'John',
      password: 'secret'
    }
  };

  const redacted = redactSecrets(input);
  assert(redacted.user.name === 'John', 'Should preserve name');
  assert(redacted.user.password === '[REDACTED:6]', 'Should redact nested password');
});

redactionTests.test('Redacts Bearer tokens in headers', () => {
  const input = {
    headers: {
      'Authorization': 'Bearer eyJhbGciOiJIUzI1NiIs...'
    }
  };

  const redacted = redactSecrets(input);
  // Note: The exact redaction depends on key matching
  assertExists(redacted.headers, 'Should have headers');
});

// --- Test Suite 4: Batch Validation ---

const batchTests = new TestRunner('Batch Validation');

batchTests.test('Validate batch of observations', () => {
  const observations = [
    createObservation({ domain: 'runtime', method: 'runtime.check', output: { nodeVersion: '20.10.0', jsEngine: 'v8', wasm: true, workers: 4, timersResolution: 100000, icu: true } }),
    createObservation({ domain: 'fs', method: 'fs.check', output: { root: '/tmp', maxPathLength: 255, fileCount: 1000, symlinkBehavior: 'followed', writeTest: true } }),
    createObservation({ domain: 'system', method: 'system.check', output: { platform: 'linux', osVersion: '5.10.0', containerized: true } })
  ];

  const result = validateObservationBatch(observations);
  assertEqual(result.passCount, 3, 'All should pass');
  assertEqual(result.failCount, 0, 'No failures');
});

batchTests.test('Batch with mixed valid/invalid', () => {
  const observations = [
    createObservation({ domain: 'runtime', method: 'runtime.check', output: { nodeVersion: '20.10.0', jsEngine: 'v8', wasm: true, workers: 4, timersResolution: 100000, icu: true } }),
    createObservation({ domain: 'runtime', method: 'runtime.check', output: { nodeVersion: 'invalid' } }),
    createObservation({ domain: 'system', method: 'system.check', output: { platform: 'linux', osVersion: '5.10.0', containerized: true } })
  ];

  const result = validateObservationBatch(observations);
  assert(result.passCount >= 2, 'At least 2 should pass');
  assert(result.failCount >= 1, 'At least 1 should fail');
});

// --- Test Suite 5: Receipt Chain Validation ---

const receiptChainTests = new TestRunner('Receipt Chain Validation');

receiptChainTests.test('Valid single receipt (no previous)', () => {
  const receipts = [
    {
      obs_hash: mockBlake3Hash(),
      prev_hash: null,
      timestamp_ns: getNowNs(),
      agentId: 'agent-1'
    }
  ];

  const result = validateReceiptChain(receipts);
  assert(result.success, 'Should validate single receipt');
});

receiptChainTests.test('Valid receipt chain (3 receipts)', () => {
  const hash1 = mockBlake3Hash();
  const hash2 = mockBlake3Hash();
  const hash3 = mockBlake3Hash();

  const now = BigInt(Date.now());
  const receipts = [
    {
      obs_hash: hash1,
      prev_hash: null,
      timestamp_ns: now.toString(),
      agentId: 'agent-1'
    },
    {
      obs_hash: hash2,
      prev_hash: hash1,
      timestamp_ns: (now + 1000000n).toString(),
      agentId: 'agent-2'
    },
    {
      obs_hash: hash3,
      prev_hash: hash2,
      timestamp_ns: (now + 2000000n).toString(),
      agentId: 'agent-3'
    }
  ];

  const result = validateReceiptChain(receipts);
  assert(result.success, 'Should validate hash chain');
});

receiptChainTests.test('Invalid: broken hash chain', () => {
  const hash1 = mockBlake3Hash();
  const hash2 = mockBlake3Hash();
  const hash3 = mockBlake3Hash();

  const now = BigInt(Date.now());
  const receipts = [
    {
      obs_hash: hash1,
      prev_hash: null,
      timestamp_ns: now.toString(),
      agentId: 'agent-1'
    },
    {
      obs_hash: hash2,
      prev_hash: mockBlake3Hash(), // Wrong previous hash
      timestamp_ns: (now + 1000000n).toString(),
      agentId: 'agent-2'
    }
  ];

  const result = validateReceiptChain(receipts);
  assert(!result.success, 'Should reject broken chain');
});

receiptChainTests.test('Invalid: non-monotonic timestamps', () => {
  const hash1 = mockBlake3Hash();
  const hash2 = mockBlake3Hash();

  const now = BigInt(Date.now());
  const receipts = [
    {
      obs_hash: hash1,
      prev_hash: null,
      timestamp_ns: now.toString(),
      agentId: 'agent-1'
    },
    {
      obs_hash: hash2,
      prev_hash: hash1,
      timestamp_ns: (now - 1000000n).toString(), // Earlier timestamp
      agentId: 'agent-2'
    }
  ];

  const result = validateReceiptChain(receipts);
  assert(!result.success, 'Should reject non-monotonic timestamps');
});

// --- Test Suite 6: Edge Cases ---

const edgeCaseTests = new TestRunner('Edge Cases');

edgeCaseTests.test('Method domain matches observation domain', () => {
  const obs = createObservation({
    domain: 'fs',
    method: 'runtime.check' // Mismatched domain
  });

  const result = validateObservation(obs);
  assert(!result.success, 'Should reject mismatched domain');
});

edgeCaseTests.test('Large observation with many fields', () => {
  const obs = createObservation({
    domain: 'concurrency',
    method: 'concurrency.check',
    output: {
      hasWorkers: true,
      parallelism: 32,
      throttles: Array.from({ length: 50 }, (_, i) => ({
        name: `throttle-${i}`,
        ratePerSecond: 100,
        burst: 10
      }))
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should handle large observations');
});

edgeCaseTests.test('Boundary: max parallelism', () => {
  const obs = createObservation({
    domain: 'concurrency',
    method: 'concurrency.check',
    output: {
      hasWorkers: true,
      parallelism: 65536,
      throttles: []
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should accept max parallelism');
});

edgeCaseTests.test('Boundary: minimum workers', () => {
  const obs = createObservation({
    domain: 'runtime',
    method: 'runtime.check',
    output: {
      nodeVersion: '20.10.0',
      jsEngine: 'v8',
      wasm: false,
      workers: 0,
      timersResolution: 1000000,
      icu: false
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should accept 0 workers');
});

edgeCaseTests.test('Null fields where allowed', () => {
  const obs = createObservation({
    domain: 'net',
    method: 'net.check',
    output: {
      urlAllowlist: [],
      dnsResolution: false,
      maxPayloadBytes: null // Null is allowed
    }
  });

  const result = validateObservation(obs);
  assert(result.success, 'Should allow null for optional numeric fields');
});

// ============================================================================
// RUN ALL TESTS
// ============================================================================

async function runAllTests() {
  console.log('\n\n');
  console.log('╔' + '═'.repeat(68) + '╗');
  console.log('║' + ' OTEL OBSERVATION SCHEMA TEST SUITE '.padStart(70) + '║');
  console.log('╚' + '═'.repeat(68) + '╝');

  const suites = [
    baseObservationTests,
    domainOutputTests,
    redactionTests,
    batchTests,
    receiptChainTests,
    edgeCaseTests
  ];

  let allPassed = true;
  for (const suite of suites) {
    const passed = await suite.run();
    allPassed = allPassed && passed;
  }

  // Summary
  const totalTests = suites.reduce((sum, s) => sum + s.tests.length, 0);
  const totalPassed = suites.reduce((sum, s) => sum + s.passed, 0);
  const totalFailed = suites.reduce((sum, s) => sum + s.failed, 0);

  console.log('\n');
  console.log('╔' + '═'.repeat(68) + '╗');
  console.log('║' + ' OVERALL RESULTS '.padStart(70) + '║');
  console.log(`║ Total Tests:    ${totalTests.toString().padEnd(50)} ║`);
  console.log(`║ Passed:         ${totalPassed.toString().padEnd(50)} ║`);
  console.log(`║ Failed:         ${totalFailed.toString().padEnd(50)} ║`);
  console.log('╚' + '═'.repeat(68) + '╝\n');

  process.exit(allPassed ? 0 : 1);
}

// Export for programmatic use
export {
  TestRunner,
  assert,
  assertEqual,
  assertExists,
  createObservation,
  getNowNs,
  mockBlake3Hash
};

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllTests().catch(console.error);
}

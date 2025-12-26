/**
 * @file Comprehensive tests for shadow mode system
 * @description Tests for shadow operations, mismatch detection, and routing
 */

import { strict as assert } from 'assert';
import { shadowWrite, shadowRead, partialServe } from './shadow.mjs';
import { mismatchReport, hashMismatchReport } from './mismatch-report.mjs';
import { defineRoute, routingDecision, pathRoute, catchAllRoute } from './routing.mjs';

// Test counter
let testsPassed = 0;
let testsFailed = 0;

/**
 * Test helper
 */
function test(name, fn) {
  return async () => {
    try {
      await fn();
      console.log(`✅ ${name}`);
      testsPassed++;
    } catch (err) {
      console.error(`❌ ${name}`);
      console.error(`   ${err.message}`);
      testsFailed++;
    }
  };
}

/**
 * Test: Shadow write with matching results
 */
const testShadowWriteMatch = test('Shadow write: matching results', async () => {
  const handler = async (req) => ({ id: req.id, data: 'test' });

  const result = await shadowWrite(handler, handler, { id: 123 });

  assert.equal(result.match, true, 'Results should match');
  assert.deepEqual(result.legacyResult, result.facadeResult, 'Results should be identical');
  assert.equal(result.mismatchHash, undefined, 'No mismatch hash for matching results');
});

/**
 * Test: Shadow write with mismatch
 */
const testShadowWriteMismatch = test('Shadow write: mismatch detection', async () => {
  const legacyHandler = async (req) => ({ id: req.id, value: 'legacy' });
  const facadeHandler = async (req) => ({ id: req.id, value: 'facade' });

  const result = await shadowWrite(legacyHandler, facadeHandler, { id: 123 });

  assert.equal(result.match, false, 'Results should not match');
  assert.notDeepEqual(result.legacyResult, result.facadeResult, 'Results should differ');
  assert.ok(result.mismatchHash, 'Mismatch hash should be present');
  assert.equal(typeof result.mismatchHash, 'string', 'Mismatch hash should be string');
  assert.ok(result.mismatchReport, 'Mismatch report should be present');
});

/**
 * Test: Shadow write timeout handling
 */
const testShadowWriteTimeout = test('Shadow write: timeout handling', async () => {
  const fastHandler = async (req) => ({ id: req.id, data: 'fast' });
  const slowHandler = async (req) => {
    await new Promise(resolve => setTimeout(resolve, 10000)); // 10s delay
    return { id: req.id, data: 'slow' };
  };

  // Facade timeout should not affect legacy result
  const result = await shadowWrite(fastHandler, slowHandler, { id: 123 }, { timeout: 100 });

  assert.equal(result.legacyResult.data, 'fast', 'Legacy should complete');
  assert.equal(result.facadeResult, null, 'Facade should timeout');
});

/**
 * Test: Shadow read with matching data
 */
const testShadowReadMatch = test('Shadow read: matching data', async () => {
  const store = async (query) => [{ id: 1, name: 'Alice' }];

  const result = await shadowRead(store, store, { filter: 'test' });

  assert.equal(result.match, true, 'Data should match');
  assert.deepEqual(result.legacyData, result.facadeData, 'Data should be identical');
});

/**
 * Test: Shadow read with mismatch
 */
const testShadowReadMismatch = test('Shadow read: mismatch detection', async () => {
  const legacyStore = async (query) => [{ id: 1, name: 'Alice' }];
  const facadeStore = async (query) => [{ id: 1, name: 'alice' }]; // Different case

  const result = await shadowRead(legacyStore, facadeStore, { filter: 'test' });

  assert.equal(result.match, false, 'Data should not match');
  assert.ok(result.mismatchHash, 'Mismatch hash should be present');
  assert.ok(result.mismatchReport, 'Mismatch report should be present');
});

/**
 * Test: Mismatch report determinism
 */
const testMismatchDeterminism = test('Mismatch report: deterministic hashing (100 iterations)', async () => {
  const legacyValue = { id: 123, name: 'Alice' };
  const facadeValue = { id: 123, name: 'alice' };

  const hashes = new Set();

  for (let i = 0; i < 100; i++) {
    const report = mismatchReport(legacyValue, facadeValue);
    hashes.add(report.mismatchHash);
  }

  assert.equal(hashes.size, 1, 'All hashes should be identical (deterministic)');
});

/**
 * Test: Mismatch report severity classification
 */
const testMismatchSeverity = test('Mismatch report: severity classification', async () => {
  // Critical: Missing field
  const report1 = mismatchReport({ id: 123, name: 'Alice' }, { id: 123 });
  assert.equal(report1.severity, 'critical', 'Missing field should be critical');

  // Critical: Type mismatch
  const report2 = mismatchReport({ id: 123 }, { id: '123' });
  assert.equal(report2.severity, 'critical', 'Type mismatch should be critical');

  // Info: Case difference
  const report3 = mismatchReport({ email: 'alice@test.com' }, { email: 'ALICE@TEST.COM' });
  assert.equal(report3.severity, 'info', 'Case difference should be info');
});

/**
 * Test: Routing decision with multiple routes
 */
const testRoutingDecision = test('Routing decision: route selection', async () => {
  const routes = [
    pathRoute('/api/v2', 'facade', { priority: 10 }),
    pathRoute('/api', 'legacy', { priority: 5 }),
    catchAllRoute('legacy')
  ];

  const target1 = routingDecision(routes, { path: '/api/v2/users' });
  assert.equal(target1, 'facade', '/api/v2 should route to facade');

  const target2 = routingDecision(routes, { path: '/api/v1/users' });
  assert.equal(target2, 'legacy', '/api should route to legacy');

  const target3 = routingDecision(routes, { path: '/other' });
  assert.equal(target3, 'legacy', 'Unknown paths should route to legacy (catch-all)');
});

/**
 * Test: Routing with weight (canary)
 */
const testRoutingWeight = test('Routing decision: weight-based canary', async () => {
  const routes = [
    pathRoute('/api/canary', 'facade', { weight: 50 }), // 50% to facade
    catchAllRoute('legacy')
  ];

  let facadeCount = 0;
  const iterations = 1000;

  for (let i = 0; i < iterations; i++) {
    const target = routingDecision(routes, { id: `req-${i}`, path: '/api/canary' });
    if (target === 'facade') facadeCount++;
  }

  const percentage = facadeCount / iterations * 100;

  // Allow ±5% margin for randomness
  assert.ok(percentage >= 45 && percentage <= 55,
    `Expected ~50% to facade, got ${percentage.toFixed(1)}%`);
});

/**
 * Test: Partial serve
 */
const testPartialServe = test('Partial serve: route-based serving', async () => {
  const legacyHandler = async (req) => ({ source: 'legacy', id: req.id });
  const facadeHandler = async (req) => ({ source: 'facade', id: req.id });

  const routes = [
    pathRoute('/api/new', 'facade', { weight: 100 }),
    catchAllRoute('legacy')
  ];

  const result1 = await partialServe(
    routes,
    { id: 1, path: '/api/new/endpoint' },
    { legacy: legacyHandler, facade: facadeHandler }
  );
  assert.equal(result1.source, 'facade', 'New endpoints should use facade');

  const result2 = await partialServe(
    routes,
    { id: 2, path: '/api/old/endpoint' },
    { legacy: legacyHandler, facade: facadeHandler }
  );
  assert.equal(result2.source, 'legacy', 'Old endpoints should use legacy');
});

/**
 * Test: Partial serve with shadow mode
 */
const testPartialServeShadow = test('Partial serve: shadow mode execution', async () => {
  const legacyHandler = async (req) => ({ source: 'legacy', id: req.id });
  const facadeHandler = async (req) => ({ source: 'facade', id: req.id });

  const routes = [
    pathRoute('/api/test', 'facade', { weight: 100 }),
    catchAllRoute('legacy')
  ];

  const result = await partialServe(
    routes,
    { id: 1, path: '/api/test' },
    { legacy: legacyHandler, facade: facadeHandler },
    { shadowMode: true }
  );

  // Should return facade result but execute both
  assert.ok(result, 'Result should be present');
  assert.equal(result.source, 'facade', 'Should return facade result');
});

/**
 * Test: Define route validation
 */
const testDefineRoute = test('Define route: validation', async () => {
  // Valid route
  const route1 = defineRoute((req) => true, 'legacy', { weight: 50 });
  assert.equal(route1.target, 'legacy');
  assert.equal(route1.weight, 50);

  // Invalid predicate
  try {
    defineRoute('not a function', 'legacy');
    assert.fail('Should throw for invalid predicate');
  } catch (err) {
    assert.ok(err.message.includes('function'), 'Should validate predicate');
  }

  // Invalid target
  try {
    defineRoute((req) => true, 'invalid');
    assert.fail('Should throw for invalid target');
  } catch (err) {
    assert.ok(err.message.includes('legacy') || err.message.includes('facade'),
      'Should validate target');
  }

  // Invalid weight
  try {
    defineRoute((req) => true, 'legacy', { weight: 150 });
    assert.fail('Should throw for invalid weight');
  } catch (err) {
    assert.ok(err.message.includes('weight'), 'Should validate weight');
  }
});

/**
 * Test: Complex routing scenarios
 */
const testComplexRouting = test('Routing: complex scenarios', async () => {
  const routes = [
    // Health check always goes to facade
    defineRoute((req) => req.path === '/health', 'facade', { priority: 100, weight: 100 }),
    // CRM endpoints to facade
    pathRoute('/api/crm', 'facade', { priority: 50, weight: 100 }),
    // Legacy endpoints stay on legacy
    pathRoute('/api/legacy', 'legacy', { priority: 50, weight: 100 }),
    // Everything else to legacy
    catchAllRoute('legacy')
  ];

  const tests = [
    { path: '/health', expected: 'facade' },
    { path: '/api/crm/users', expected: 'facade' },
    { path: '/api/crm/contacts', expected: 'facade' },
    { path: '/api/legacy/old', expected: 'legacy' },
    { path: '/api/unknown', expected: 'legacy' },
    { path: '/other', expected: 'legacy' }
  ];

  for (const { path, expected } of tests) {
    const target = routingDecision(routes, { path });
    assert.equal(target, expected, `${path} should route to ${expected}`);
  }
});

/**
 * Test: Mismatch path detection
 */
const testMismatchPath = test('Mismatch report: path detection', async () => {
  const legacy = { user: { profile: { email: 'alice@test.com' } } };
  const facade = { user: { profile: { email: 'ALICE@TEST.COM' } } };

  const report = mismatchReport(legacy, facade);

  assert.deepEqual(report.path, ['user', 'profile', 'email'],
    'Should identify exact path to difference');
});

/**
 * Test: Deep equality edge cases
 */
const testDeepEquality = test('Deep equality: edge cases', async () => {
  // Dates
  const handler1 = async () => ({ date: new Date('2024-01-01') });
  const handler2 = async () => ({ date: new Date('2024-01-01') });
  const result1 = await shadowWrite(handler1, handler2, {});
  assert.equal(result1.match, true, 'Equal dates should match');

  // Nested arrays
  const handler3 = async () => ({ items: [{ id: 1 }, { id: 2 }] });
  const handler4 = async () => ({ items: [{ id: 1 }, { id: 2 }] });
  const result2 = await shadowWrite(handler3, handler4, {});
  assert.equal(result2.match, true, 'Equal nested arrays should match');

  // Null vs undefined
  const handler5 = async () => ({ value: null });
  const handler6 = async () => ({ value: undefined });
  const result3 = await shadowWrite(handler5, handler6, {});
  assert.equal(result3.match, false, 'null and undefined should not match');
});

/**
 * Run all tests
 */
async function runTests() {
  console.log('Running shadow mode system tests...\n');

  const tests = [
    testShadowWriteMatch,
    testShadowWriteMismatch,
    testShadowWriteTimeout,
    testShadowReadMatch,
    testShadowReadMismatch,
    testMismatchDeterminism,
    testMismatchSeverity,
    testRoutingDecision,
    testRoutingWeight,
    testPartialServe,
    testPartialServeShadow,
    testDefineRoute,
    testComplexRouting,
    testMismatchPath,
    testDeepEquality
  ];

  for (const testFn of tests) {
    await testFn();
  }

  console.log(`\n${'='.repeat(50)}`);
  console.log(`Tests passed: ${testsPassed}`);
  console.log(`Tests failed: ${testsFailed}`);
  console.log(`Total: ${testsPassed + testsFailed}`);
  console.log(`${'='.repeat(50)}`);

  if (testsFailed > 0) {
    process.exit(1);
  }
}

// Run tests if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runTests();
}

export { runTests };

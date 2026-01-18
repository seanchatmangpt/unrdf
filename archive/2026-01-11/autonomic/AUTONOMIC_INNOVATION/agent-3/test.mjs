/**
 * @file test.mjs
 * @description Comprehensive test suite for Agent 3 Lens system
 * @module agent-3/test
 */

import { strict as assert } from 'node:assert';
import { createStore } from '../../packages/oxigraph/src/index.mjs';
import { stableIRI, stableSkolem } from './stable-ids.mjs';
import { defineLens, compileLens, executeLensToGraph, executeLensFromGraph } from './lens.mjs';
import { customerLens, customerLensProgram } from './demo-customer-lens.mjs';

/**
 * Test 1: Stable IRI determinism
 * Verify that stableIRI generates identical IRIs for identical inputs across 1000 iterations
 */
function testStableIRIDeterminism() {
  console.log('\n[TEST 1] Stable IRI Determinism (1000 iterations)');

  const domain = 'kgc-facade';
  const entity = 'customer';
  const attr = 'customer-123';

  // Generate IRI 1000 times
  const iris = new Set();
  const startTime = Date.now();

  for (let i = 0; i < 1000; i++) {
    const iri = stableIRI(domain, entity, attr);
    iris.add(iri);
  }

  const duration = Date.now() - startTime;

  // Should produce exactly 1 unique IRI
  assert.equal(iris.size, 1, 'Expected exactly 1 unique IRI from 1000 calls');

  const avgTime = duration / 1000;
  console.log(`  ✅ Generated 1 unique IRI from 1000 calls`);
  console.log(`  ✅ Average time: ${avgTime.toFixed(3)}ms per call`);
  console.log(`  ✅ Sample IRI: ${Array.from(iris)[0]}`);

  // Verify performance target (< 1ms per call)
  assert.ok(avgTime < 1, `Performance target failed: ${avgTime.toFixed(3)}ms > 1ms`);

  return { success: true, avgTime, iris: Array.from(iris) };
}

/**
 * Test 2: Skolem determinism
 * Verify that stableSkolem generates identical blank nodes for identical inputs across 100 iterations
 */
function testSkolemDeterminism() {
  console.log('\n[TEST 2] Skolem Determinism (100 iterations)');

  const template = 'customer-{id}-{attr}';
  const values = { id: '123', attr: 'address' };

  // Generate Skolem 100 times
  const skolems = new Set();

  for (let i = 0; i < 100; i++) {
    const skolem = stableSkolem(template, values);
    skolems.add(skolem);
  }

  // Should produce exactly 1 unique Skolem
  assert.equal(skolems.size, 1, 'Expected exactly 1 unique Skolem from 100 calls');

  console.log(`  ✅ Generated 1 unique Skolem from 100 calls`);
  console.log(`  ✅ Sample Skolem: ${Array.from(skolems)[0]}`);

  // Test with different value orders (should still be deterministic)
  const values2 = { attr: 'address', id: '123' }; // Different property order
  const skolem2 = stableSkolem(template, values2);

  assert.equal(skolem2, Array.from(skolems)[0], 'Skolem should be independent of property order');
  console.log(`  ✅ Skolem independent of property order`);

  return { success: true, skolems: Array.from(skolems) };
}

/**
 * Test 3: Lens round-trip (DTO → RDF → DTO)
 * Verify that transformation is lossless and byte-identical
 */
function testLensRoundTrip() {
  console.log('\n[TEST 3] Lens Round-Trip (DTO → RDF → DTO)');

  const originalDTO = {
    id: 'customer-456',
    name: 'Bob Smith',
    email: 'bob@example.com',
    registeredAt: '2025-12-26T10:00:00Z'
  };

  // Phase 1: DTO → RDF
  const { quads, subjects } = executeLensToGraph(originalDTO, customerLensProgram);
  console.log(`  → Generated ${quads.length} quads`);
  assert.equal(quads.length, 4, 'Expected 4 quads for 4 DTO fields');
  assert.equal(subjects.length, 1, 'Expected 1 subject IRI');

  // Phase 2: Store in Oxigraph
  const store = createStore();
  for (const quad of quads) {
    store.add(quad);
  }
  console.log(`  → Stored ${quads.length} quads in RDF store`);

  // Phase 3: RDF → DTO
  const reconstructedDTO = executeLensFromGraph(subjects, store, customerLensProgram);
  console.log(`  → Reconstructed DTO:`, reconstructedDTO);

  // Phase 4: Verify deep equality
  assert.deepEqual(reconstructedDTO, originalDTO, 'Reconstructed DTO must match original');
  console.log(`  ✅ Round-trip successful: DTO → RDF → DTO`);
  console.log(`  ✅ Byte-identical reconstruction verified`);

  return { success: true, originalDTO, reconstructedDTO, quads: quads.length };
}

/**
 * Test 4: Lens program serialization (JSON portability)
 * Verify that compiled lens programs can be serialized and deserialized without loss
 */
function testLensProgramSerialization() {
  console.log('\n[TEST 4] Lens Program Serialization (JSON portability)');

  // Serialize lens program
  const serialized = JSON.stringify(customerLensProgram);
  console.log(`  → Serialized program: ${serialized.length} bytes`);

  // Deserialize
  const deserialized = JSON.parse(serialized);
  console.log(`  → Deserialized program successfully`);

  // Verify structure
  assert.ok(deserialized.name, 'Program must have name');
  assert.ok(deserialized.version, 'Program must have version');
  assert.ok(Array.isArray(deserialized.toGraph), 'Program must have toGraph rules');
  assert.ok(Array.isArray(deserialized.fromGraph), 'Program must have fromGraph rules');
  assert.ok(deserialized.stableIds, 'Program must have stableIds');

  console.log(`  ✅ Program structure verified`);

  // Test execution with deserialized program
  const testDTO = { id: 'test-789', name: 'Charlie', email: 'charlie@example.com', registeredAt: '2025-12-26T12:00:00Z' };
  const { quads } = executeLensToGraph(testDTO, deserialized);

  assert.equal(quads.length, 4, 'Deserialized program should produce same number of quads');
  console.log(`  ✅ Deserialized program executes correctly`);
  console.log(`  ✅ JSON portability verified`);

  return { success: true, serialized, deserialized };
}

/**
 * Test 5: Multiple DTOs with same ID space
 * Verify that different entities get different IRIs but share stable IRI space
 */
function testMultipleDTOsSameIDSpace() {
  console.log('\n[TEST 5] Multiple DTOs with Stable IRI Space');

  const customers = [
    { id: 'customer-001', name: 'Alice', email: 'alice@example.com', registeredAt: '2025-01-01T00:00:00Z' },
    { id: 'customer-002', name: 'Bob', email: 'bob@example.com', registeredAt: '2025-01-02T00:00:00Z' },
    { id: 'customer-001', name: 'Alice Updated', email: 'alice-new@example.com', registeredAt: '2025-01-01T00:00:00Z' }
  ];

  const results = customers.map(customer => {
    const { subjects } = executeLensToGraph(customer, customerLensProgram);
    return { id: customer.id, iri: subjects[0] };
  });

  console.log(`  → Generated IRIs for ${customers.length} customers`);

  // customer-001 should have same IRI both times
  const customer001IRIs = results.filter(r => r.id === 'customer-001').map(r => r.iri);
  assert.equal(customer001IRIs[0], customer001IRIs[1], 'Same ID must produce same IRI');
  console.log(`  ✅ customer-001 stable: ${customer001IRIs[0]}`);

  // customer-002 should have different IRI
  const customer002IRI = results.find(r => r.id === 'customer-002').iri;
  assert.notEqual(customer002IRI, customer001IRIs[0], 'Different IDs must produce different IRIs');
  console.log(`  ✅ customer-002 unique: ${customer002IRI}`);

  console.log(`  ✅ IRI space stability verified`);

  return { success: true, results };
}

/**
 * Test 6: Skolem determinism with template variations
 * Verify that different templates or values produce different Skolems
 */
function testSkolemVariations() {
  console.log('\n[TEST 6] Skolem Variations (collision resistance)');

  const testCases = [
    { template: 'customer-{id}', values: { id: '123' } },
    { template: 'customer-{id}', values: { id: '456' } },
    { template: 'order-{id}', values: { id: '123' } },
    { template: 'customer-{id}-{attr}', values: { id: '123', attr: 'address' } }
  ];

  const skolems = testCases.map(tc => stableSkolem(tc.template, tc.values));
  const uniqueSkolems = new Set(skolems);

  console.log(`  → Generated ${skolems.length} Skolems`);
  assert.equal(uniqueSkolems.size, testCases.length, 'Each variation should produce unique Skolem');

  console.log(`  ✅ All ${uniqueSkolems.size} Skolems unique (no collisions)`);

  for (let i = 0; i < testCases.length; i++) {
    console.log(`  → Case ${i + 1}: ${skolems[i]}`);
  }

  return { success: true, skolems };
}

/**
 * Run all tests
 */
function runAllTests() {
  console.log('=== Agent 3 Lens System Test Suite ===');
  console.log('Running 6 comprehensive tests...\n');

  const results = [];
  let passed = 0;
  let failed = 0;

  const tests = [
    { name: 'Test 1: Stable IRI Determinism', fn: testStableIRIDeterminism },
    { name: 'Test 2: Skolem Determinism', fn: testSkolemDeterminism },
    { name: 'Test 3: Lens Round-Trip', fn: testLensRoundTrip },
    { name: 'Test 4: Lens Program Serialization', fn: testLensProgramSerialization },
    { name: 'Test 5: Multiple DTOs Same ID Space', fn: testMultipleDTOsSameIDSpace },
    { name: 'Test 6: Skolem Variations', fn: testSkolemVariations }
  ];

  for (const test of tests) {
    try {
      const result = test.fn();
      results.push({ name: test.name, status: 'PASSED', result });
      passed++;
    } catch (error) {
      results.push({ name: test.name, status: 'FAILED', error: error.message });
      failed++;
      console.error(`\n  ❌ ${test.name} FAILED:`, error.message);
    }
  }

  console.log('\n=== Test Summary ===');
  console.log(`Total: ${tests.length} tests`);
  console.log(`Passed: ${passed} ✅`);
  console.log(`Failed: ${failed} ❌`);
  console.log(`Success Rate: ${((passed / tests.length) * 100).toFixed(1)}%`);

  if (failed === 0) {
    console.log('\n🎉 All tests passed! Lens system ready for integration.');
  } else {
    console.error('\n⚠️  Some tests failed. Review errors above.');
    process.exit(1);
  }

  return { passed, failed, results };
}

// Run tests if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runAllTests();
}

export { runAllTests };

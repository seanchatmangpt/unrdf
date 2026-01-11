/**
 * @file test-stable-ids.mjs
 * @description Standalone test for stable ID generation (no oxigraph dependency)
 * @module agent-3/test-stable-ids
 */

import { strict as assert } from 'node:assert';
import { stableIRI, stableSkolem, stableHash } from './stable-ids.mjs';

console.log('=== Agent 3 Stable ID Test Suite ===');
console.log('Testing stable identifier generation without external dependencies\n');

let passed = 0;
let failed = 0;

// Test 1: Stable IRI Determinism
try {
  console.log('[TEST 1] Stable IRI Determinism (1000 iterations)');

  const domain = 'kgc-facade';
  const entity = 'customer';
  const attr = 'customer-123';

  const iris = new Set();
  const startTime = Date.now();

  for (let i = 0; i < 1000; i++) {
    const iri = stableIRI(domain, entity, attr);
    iris.add(iri);
  }

  const duration = Date.now() - startTime;
  const avgTime = duration / 1000;

  assert.equal(iris.size, 1, 'Expected exactly 1 unique IRI from 1000 calls');
  assert.ok(avgTime < 1, `Performance target failed: ${avgTime.toFixed(3)}ms > 1ms`);

  console.log(`  ✅ Generated 1 unique IRI from 1000 calls`);
  console.log(`  ✅ Average time: ${avgTime.toFixed(3)}ms per call`);
  console.log(`  ✅ Sample IRI: ${Array.from(iris)[0]}`);

  passed++;
} catch (error) {
  console.error(`  ❌ FAILED:`, error.message);
  failed++;
}

// Test 2: Skolem Determinism
try {
  console.log('\n[TEST 2] Skolem Determinism (100 iterations)');

  const template = 'customer-{id}-{attr}';
  const values = { id: '123', attr: 'address' };

  const skolems = new Set();

  for (let i = 0; i < 100; i++) {
    const skolem = stableSkolem(template, values);
    skolems.add(skolem);
  }

  assert.equal(skolems.size, 1, 'Expected exactly 1 unique Skolem from 100 calls');
  console.log(`  ✅ Generated 1 unique Skolem from 100 calls`);
  console.log(`  ✅ Sample Skolem: ${Array.from(skolems)[0]}`);

  // Test property order independence
  const values2 = { attr: 'address', id: '123' };
  const skolem2 = stableSkolem(template, values2);

  assert.equal(skolem2, Array.from(skolems)[0], 'Skolem should be independent of property order');
  console.log(`  ✅ Skolem independent of property order`);

  passed++;
} catch (error) {
  console.error(`  ❌ FAILED:`, error.message);
  failed++;
}

// Test 3: IRI Uniqueness
try {
  console.log('\n[TEST 3] IRI Uniqueness (collision resistance)');

  const testCases = [
    { domain: 'kgc-facade', entity: 'customer', attr: 'customer-001' },
    { domain: 'kgc-facade', entity: 'customer', attr: 'customer-002' },
    { domain: 'kgc-facade', entity: 'order', attr: 'customer-001' },
    { domain: 'other-domain', entity: 'customer', attr: 'customer-001' }
  ];

  const iris = testCases.map(tc => stableIRI(tc.domain, tc.entity, tc.attr));
  const uniqueIris = new Set(iris);

  assert.equal(uniqueIris.size, testCases.length, 'Each variation should produce unique IRI');
  console.log(`  ✅ All ${uniqueIris.size} IRIs unique (no collisions)`);

  for (let i = 0; i < testCases.length; i++) {
    console.log(`  → Case ${i + 1}: ${iris[i]}`);
  }

  passed++;
} catch (error) {
  console.error(`  ❌ FAILED:`, error.message);
  failed++;
}

// Test 4: Skolem Variations
try {
  console.log('\n[TEST 4] Skolem Variations (collision resistance)');

  const testCases = [
    { template: 'customer-{id}', values: { id: '123' } },
    { template: 'customer-{id}', values: { id: '456' } },
    { template: 'order-{id}', values: { id: '123' } },
    { template: 'customer-{id}-{attr}', values: { id: '123', attr: 'address' } }
  ];

  const skolems = testCases.map(tc => stableSkolem(tc.template, tc.values));
  const uniqueSkolems = new Set(skolems);

  assert.equal(uniqueSkolems.size, testCases.length, 'Each variation should produce unique Skolem');
  console.log(`  ✅ All ${uniqueSkolems.size} Skolems unique (no collisions)`);

  for (let i = 0; i < testCases.length; i++) {
    console.log(`  → Case ${i + 1}: ${skolems[i]}`);
  }

  passed++;
} catch (error) {
  console.error(`  ❌ FAILED:`, error.message);
  failed++;
}

// Test 5: Input Validation
try {
  console.log('\n[TEST 5] Input Validation');

  // Test stableIRI validation
  let errorCount = 0;

  try {
    stableIRI('', 'entity', 'attr');
  } catch (e) {
    errorCount++;
  }

  try {
    stableIRI('domain', '', 'attr');
  } catch (e) {
    errorCount++;
  }

  try {
    stableIRI('domain', 'entity', '');
  } catch (e) {
    errorCount++;
  }

  assert.equal(errorCount, 3, 'Expected 3 validation errors for empty inputs');
  console.log(`  ✅ Validated 3 error cases for stableIRI`);

  // Test stableSkolem validation
  errorCount = 0;

  try {
    stableSkolem('', { id: '123' });
  } catch (e) {
    errorCount++;
  }

  try {
    stableSkolem('template', null);
  } catch (e) {
    errorCount++;
  }

  assert.equal(errorCount, 2, 'Expected 2 validation errors for invalid inputs');
  console.log(`  ✅ Validated 2 error cases for stableSkolem`);

  passed++;
} catch (error) {
  console.error(`  ❌ FAILED:`, error.message);
  failed++;
}

// Test 6: Hash Consistency
try {
  console.log('\n[TEST 6] Hash Consistency');

  const input = 'test-input-string';
  const hash1 = stableHash(input);
  const hash2 = stableHash(input);

  assert.equal(hash1, hash2, 'Hash should be consistent for same input');
  assert.equal(hash1.length, 16, 'Hash should be 16 characters by default');
  console.log(`  ✅ Hash consistent: ${hash1}`);

  // Test custom length
  const hash3 = stableHash(input, 32);
  assert.equal(hash3.length, 32, 'Hash should support custom length');
  console.log(`  ✅ Custom length supported: ${hash3.substring(0, 16)}...`);

  passed++;
} catch (error) {
  console.error(`  ❌ FAILED:`, error.message);
  failed++;
}

// Summary
console.log('\n=== Test Summary ===');
console.log(`Total: 6 tests`);
console.log(`Passed: ${passed} ✅`);
console.log(`Failed: ${failed} ❌`);
console.log(`Success Rate: ${((passed / 6) * 100).toFixed(1)}%`);

if (failed === 0) {
  console.log('\n🎉 All stable-ids tests passed! Core identifier system ready.');
} else {
  console.error('\n⚠️  Some tests failed. Review errors above.');
  process.exit(1);
}

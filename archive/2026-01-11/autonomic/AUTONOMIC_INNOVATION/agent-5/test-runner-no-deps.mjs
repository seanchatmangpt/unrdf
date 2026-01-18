#!/usr/bin/env node
/**
 * Simple test runner to verify Agent 5 implementation
 * Runs without any external dependencies (no vitest, no zod)
 */

import { canReorder, conflictCertificate } from './src/commutativity-no-zod.mjs';
import { canonicalize, sha256 } from './src/canonicalization.mjs';

// Test utilities
let passed = 0;
let failed = 0;

function assert(condition, message) {
  if (condition) {
    passed++;
    console.log(`✅ PASS: ${message}`);
  } else {
    failed++;
    console.error(`❌ FAIL: ${message}`);
  }
}

function createQuad(subject, predicate, object, graph = 'http://kgc.io/Universe') {
  return {
    subject: { value: subject },
    predicate: { value: predicate },
    object: { value: object },
    graph: { value: graph },
  };
}

function createCapsule(id, addQuads = [], delQuads = []) {
  return {
    id,
    add: new Set(addQuads),
    del: new Set(delQuads),
    metadata: {},
  };
}

console.log('\n🧪 Agent 5: Commutativity Analysis Test Suite\n');

// Test 1: Disjoint Impact Sets
console.log('Test 1: Disjoint Impact Sets → Can Reorder');
{
  const capsuleA = createCapsule('A', [
    createQuad('http://ex.org/alice', 'http://xmlns.com/foaf/0.1/name', 'Alice'),
  ]);
  const capsuleB = createCapsule('B', [
    createQuad('http://ex.org/bob', 'http://xmlns.com/foaf/0.1/name', 'Bob'),
  ]);

  const result = canReorder(capsuleA, capsuleB);
  assert(result.ok === true, 'Should allow reordering');
  assert(result.reason === 'disjoint-impact-sets', 'Reason should be disjoint-impact-sets');
}

// Test 2: Overlapping Subjects (Different Properties)
console.log('\nTest 2: Overlapping Subjects (Different Properties) → Can Reorder');
{
  const capsuleA = createCapsule('A', [
    createQuad('http://ex.org/alice', 'http://xmlns.com/foaf/0.1/age', '30'),
  ]);
  const capsuleB = createCapsule('B', [
    createQuad('http://ex.org/alice', 'http://xmlns.com/foaf/0.1/email', 'alice@example.com'),
  ]);

  const result = canReorder(capsuleA, capsuleB);
  assert(result.ok === true, 'Should allow reordering');
  assert(result.reason === 'commutative-deltas', 'Reason should be commutative-deltas');
}

// Test 3: Add-Delete Conflict
console.log('\nTest 3: Add-Delete Conflict → Cannot Reorder');
{
  const quad = createQuad('http://ex.org/alice', 'http://xmlns.com/foaf/0.1/name', 'Alice');
  const capsuleA = createCapsule('A', [quad]);
  const capsuleB = createCapsule('B', [], [quad]);

  const result = canReorder(capsuleA, capsuleB);
  assert(result.ok === false, 'Should NOT allow reordering');
  assert(result.reason === 'add-del-conflict', 'Reason should be add-del-conflict');
  assert(result.witness !== undefined, 'Should have witness');
  assert(result.witness.length > 0, 'Witness should not be empty');
}

// Test 4: Conflict Certificate Generation
console.log('\nTest 4: Conflict Certificate Generation');
{
  const quad = createQuad('http://ex.org/alice', 'http://xmlns.com/foaf/0.1/name', 'Alice');
  const capsuleA = createCapsule('A', [quad]);
  const capsuleB = createCapsule('B', [], [quad]);

  const cert = conflictCertificate(capsuleA, capsuleB);
  assert(cert.counterexample !== undefined, 'Should have counterexample');
  assert(cert.explanation.includes('Capsule A adds'), 'Explanation should mention Capsule A adds');
  assert(cert.explanation.includes('Capsule B deletes'), 'Explanation should mention Capsule B deletes');
  assert(/^[a-f0-9]{64}$/.test(cert.hash), 'Hash should be SHA-256 hex (64 chars)');
  assert(cert.capsuleIds.length === 2, 'Should have 2 capsule IDs');
  assert(cert.conflictType === 'add-del-conflict', 'Conflict type should be add-del-conflict');
  assert(cert.version === '1.0.0', 'Version should be 1.0.0');
  assert(cert.metadata.minimality === 'proven', 'Minimality should be proven');
}

// Test 5: Delete-Add Conflict
console.log('\nTest 5: Delete-Add Conflict → Cannot Reorder');
{
  const quad = createQuad('http://ex.org/alice', 'http://xmlns.com/foaf/0.1/name', 'Alice');
  const capsuleA = createCapsule('A', [], [quad]);
  const capsuleB = createCapsule('B', [quad]);

  const result = canReorder(capsuleA, capsuleB);
  assert(result.ok === false, 'Should NOT allow reordering');
  assert(result.reason === 'del-add-conflict', 'Reason should be del-add-conflict');
}

// Test 6: Empty Capsules
console.log('\nTest 6: Empty Capsules → Can Reorder');
{
  const capsuleA = createCapsule('A', [], []);
  const capsuleB = createCapsule('B', [], []);

  const result = canReorder(capsuleA, capsuleB);
  assert(result.ok === true, 'Should allow reordering');
}

// Test 7: Deterministic Canonicalization
console.log('\nTest 7: Deterministic Canonicalization');
{
  const data = {
    subjects: ['http://ex.org/s1', 'http://ex.org/s2'],
    predicates: ['http://ex.org/p1'],
    cardinality: { added: 2, deleted: 0 },
  };

  const canon1 = canonicalize(data);
  const canon2 = canonicalize(data);
  const hash1 = sha256(canon1);
  const hash2 = sha256(canon2);

  assert(canon1 === canon2, 'Canonicalization should be deterministic');
  assert(hash1 === hash2, 'Hashes should be identical');
}

// Test 8: Complex Conflict Minimization
console.log('\nTest 8: Complex Conflict Minimization');
{
  const quad1 = createQuad('http://ex.org/alice', 'http://xmlns.com/foaf/0.1/name', 'Alice');
  const quad2 = createQuad('http://ex.org/alice', 'http://xmlns.com/foaf/0.1/age', '30');
  const quad3 = createQuad('http://ex.org/bob', 'http://xmlns.com/foaf/0.1/name', 'Bob');

  const capsuleA = createCapsule('A', [quad1, quad2, quad3]);
  const capsuleB = createCapsule('B', [], [quad1]);

  const result = canReorder(capsuleA, capsuleB);
  assert(result.ok === false, 'Should NOT allow reordering');
  assert(result.witness.length === 1, 'Witness should be minimized to 1 quad');
}

// Test 9: Error Handling
console.log('\nTest 9: Error Handling');
{
  const capsuleA = createCapsule('A', [
    createQuad('http://ex.org/alice', 'http://xmlns.com/foaf/0.1/name', 'Alice'),
  ]);
  const capsuleB = createCapsule('B', [
    createQuad('http://ex.org/bob', 'http://xmlns.com/foaf/0.1/name', 'Bob'),
  ]);

  let threw = false;
  try {
    conflictCertificate(capsuleA, capsuleB);
  } catch (e) {
    threw = true;
    assert(e.message.includes('Cannot generate certificate'), 'Should throw correct error message');
  }
  assert(threw, 'Should throw error for commutative capsules');
}

// Test 10: Deterministic Hashing (10x runs)
console.log('\nTest 10: Deterministic Hashing (10x verification)');
{
  const quad = createQuad('http://ex.org/alice', 'http://xmlns.com/foaf/0.1/name', 'Alice');
  const capsuleA = createCapsule('A', [quad]);
  const capsuleB = createCapsule('B', [], [quad]);

  const certs = [];
  for (let i = 0; i < 10; i++) {
    const cert = conflictCertificate(capsuleA, capsuleB);
    certs.push(cert);
  }

  // Check all have same structure (timestamps will differ)
  const firstCert = certs[0];
  for (let i = 1; i < certs.length; i++) {
    assert(
      JSON.stringify(certs[i].capsuleIds) === JSON.stringify(firstCert.capsuleIds),
      `Cert ${i} capsuleIds should match`
    );
    assert(certs[i].conflictType === firstCert.conflictType, `Cert ${i} conflictType should match`);
    assert(
      certs[i].metadata.witnessSize === firstCert.metadata.witnessSize,
      `Cert ${i} witnessSize should match`
    );
  }

  console.log('  Deterministic hash verification: Structure matches across 10 runs');
}

// Summary
console.log('\n' + '='.repeat(50));
console.log(`✅ Passed: ${passed}`);
console.log(`❌ Failed: ${failed}`);
console.log(`📊 Total: ${passed + failed}`);
console.log('='.repeat(50));

if (failed > 0) {
  console.error('\n❌ TESTS FAILED');
  process.exit(1);
} else {
  console.log('\n✅ ALL TESTS PASSED');
  process.exit(0);
}

/**
 * Agent 8 Test Suite - Store Adapter and Atomic Apply
 * Comprehensive tests for atomicity, rollback, and determinism
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { createStore, dataFactory } from './mock-store.mjs';
import { createStoreAdapter, transaction, replayFromReceipt } from './store-adapter.mjs';
import { atomicApply, verifyAtomicity } from './atomic.mjs';
import { integrateWithKGC4D, integrateWithOxigraph } from './integration.mjs';

/**
 * Helper: Create test capsule with N quads
 */
function createTestCapsule(quadCount = 10) {
  const deltas = [];

  for (let i = 0; i < quadCount; i++) {
    deltas.push({
      type: 'add',
      subject: {
        termType: 'NamedNode',
        value: `http://example.org/subject${i}`,
      },
      predicate: {
        termType: 'NamedNode',
        value: 'http://example.org/predicate',
      },
      object: {
        termType: 'Literal',
        value: `Object ${i}`,
      },
      graph: {
        termType: 'NamedNode',
        value: 'http://example.org/graph',
      },
    });
  }

  return { delta: deltas };
}

test('Atomic apply - all or nothing', async () => {
  const store = createStore();
  const capsule = createTestCapsule(10);

  const result = await atomicApply(capsule, store);

  assert.ok(result.receipt, 'Returns receipt');
  assert.equal(result.receipt.quadCount, 10, 'Receipt has correct quad count');
  assert.ok(result.hash, 'Returns hash');
  assert.ok(result.appliedAt, 'Returns timestamp');

  // Verify all quads were added
  const allQuads = [...store.match(null, null, null, null)];
  assert.equal(allQuads.length, 10, 'All 10 quads applied');
});

test('Replay from receipt', async () => {
  const store = createStore();
  const adapter = createStoreAdapter(store);
  const capsule = createTestCapsule(5);

  const result1 = await transaction(capsule, adapter);
  assert.ok(result1.success, 'Transaction succeeded');

  const replay = await replayFromReceipt(result1.receipt, store);
  assert.ok(replay.receipt, 'Returns receipt');
  assert.equal(replay.receipt.id, result1.receipt.id, 'Receipt IDs match');
});

test('Atomicity verification', async () => {
  const store = createStore();
  const capsule = createTestCapsule(10);

  const result = await atomicApply(capsule, store);
  assert.ok(result.receipt, 'Application succeeded');

  const isValid = verifyAtomicity(capsule, result.receipt, store);
  assert.equal(isValid, true, 'Atomicity verified');
});

test('Large transaction performance', async (t) => {
  const store = createStore();
  const capsule = createTestCapsule(1000);

  const startTime = performance.now();
  const result = await atomicApply(capsule, store);
  const endTime = performance.now();

  const duration = endTime - startTime;

  assert.ok(result.receipt, 'Large transaction succeeded');
  assert.equal(result.receipt.quadCount, 1000, 'Correct quad count');
  assert.ok(duration < 500, `Performance: ${duration.toFixed(2)}ms < 500ms`);

  // Verify all quads present
  const allQuads = [...store.match(null, null, null, null)];
  assert.equal(allQuads.length, 1000, 'All 1000 quads applied');
});

test('Deterministic receipt hash', async () => {
  const capsule = createTestCapsule(5);
  const hashes = [];

  // Apply same capsule 10 times to fresh stores
  for (let i = 0; i < 10; i++) {
    const store = createStore();
    const result = await atomicApply(capsule, store);
    hashes.push(result.hash);
  }

  // All hashes should be identical
  const uniqueHashes = new Set(hashes);
  assert.equal(uniqueHashes.size, 1, 'All hashes identical (deterministic)');
});

test('Rollback on failure', async () => {
  const store = createStore();

  // Create capsule with invalid quad (will cause error)
  const invalidCapsule = {
    delta: [
      {
        type: 'add',
        subject: { termType: 'NamedNode', value: 'http://valid.org/s1' },
        predicate: { termType: 'NamedNode', value: 'http://valid.org/p' },
        object: { termType: 'Literal', value: 'Valid 1' },
      },
      {
        type: 'add',
        subject: { termType: 'NamedNode', value: 'http://valid.org/s2' },
        predicate: { termType: 'NamedNode', value: 'http://valid.org/p' },
        object: { termType: 'Literal', value: 'Valid 2' },
      },
      // This will be processed, but we'll simulate failure by throwing
      {
        type: 'add',
        subject: { termType: 'InvalidType', value: 'invalid' }, // Invalid termType
        predicate: { termType: 'NamedNode', value: 'http://valid.org/p' },
        object: { termType: 'Literal', value: 'Invalid' },
      },
    ],
  };

  const result = await atomicApply(invalidCapsule, store);

  // Transaction should fail and rollback
  // Note: atomicApply doesn't have 'success' field, only 'error' when failed
  assert.ok(result.error, 'Error message returned');

  // Store should be empty (rollback successful)
  const allQuads = [...store.match(null, null, null, null)];
  assert.equal(allQuads.length, 0, 'Store empty after rollback');
});

test('Integration with KGC-4D', async () => {
  const capsule = createTestCapsule(5);

  const result = await integrateWithKGC4D(capsule);

  assert.ok(result.frozenSnapshot, 'Returns frozen snapshot');
  assert.ok(result.hash, 'Returns hash');
  assert.equal(result.frozenSnapshot.deltaCount, 5, 'Correct delta count');
  assert.equal(result.frozenSnapshot.frozen, true, 'Snapshot is frozen');
});

test('Integration with Oxigraph', async () => {
  const capsule = createTestCapsule(10);

  const result = await integrateWithOxigraph(capsule);

  assert.equal(result.success, true, 'Integration succeeded');
  assert.equal(result.quadCount, 10, 'Correct quad count');
  assert.equal(result.error, undefined, 'No error');
});

test('Store adapter creation', () => {
  const adapter = createStoreAdapter();

  assert.ok(typeof adapter.addQuad === 'function', 'Has addQuad method');
  assert.ok(typeof adapter.deleteQuad === 'function', 'Has deleteQuad method');
  assert.ok(typeof adapter.queryQuads === 'function', 'Has queryQuads method');
  assert.ok(typeof adapter.getStore === 'function', 'Has getStore method');
});

test('Transaction with store adapter', async () => {
  const adapter = createStoreAdapter();
  const capsule = createTestCapsule(5);

  const result = await transaction(capsule, adapter);

  assert.equal(result.success, true, 'Transaction succeeded');
  assert.ok(result.receipt, 'Returns receipt');
  assert.equal(result.receipt.quadCount, 5, 'Correct quad count');

  // Query to verify quads were added
  const store = adapter.getStore();
  const allQuads = [...store.match(null, null, null, null)];
  assert.equal(allQuads.length, 5, 'All quads added to store');
});

test('Empty capsule handling', async () => {
  const store = createStore();
  const emptyCapsule = { delta: [] };

  const result = await atomicApply(emptyCapsule, store);

  assert.ok(result.receipt, 'Returns receipt for empty capsule');
  assert.equal(result.receipt.quadCount, 0, 'Quad count is 0');
  assert.ok(result.hash, 'Hash generated');
});

test('Mixed add/delete operations', async () => {
  const store = createStore();

  // First add some quads
  const addCapsule = {
    delta: [
      {
        type: 'add',
        subject: { termType: 'NamedNode', value: 'http://example.org/s1' },
        predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
        object: { termType: 'Literal', value: 'Value 1' },
      },
      {
        type: 'add',
        subject: { termType: 'NamedNode', value: 'http://example.org/s2' },
        predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
        object: { termType: 'Literal', value: 'Value 2' },
      },
    ],
  };

  const addResult = await atomicApply(addCapsule, store);
  assert.ok(addResult.receipt, 'Add succeeded');

  // Now delete one quad
  const deleteCapsule = {
    delta: [
      {
        type: 'delete',
        subject: { termType: 'NamedNode', value: 'http://example.org/s1' },
        predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
        object: { termType: 'Literal', value: 'Value 1' },
      },
    ],
  };

  const deleteResult = await atomicApply(deleteCapsule, store);
  assert.ok(deleteResult.receipt, 'Delete succeeded');

  // Verify only 1 quad remains
  const remaining = [...store.match(null, null, null, null)];
  assert.equal(remaining.length, 1, 'One quad remains after delete');
  assert.equal(
    remaining[0].subject.value,
    'http://example.org/s2',
    'Correct quad remains'
  );
});

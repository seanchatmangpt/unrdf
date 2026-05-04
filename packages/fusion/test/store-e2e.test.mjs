/**
 * @file Store E2E Test - Unified store adapter verification
 * @module @unrdf/fusion/test/store-e2e
 *
 * Comprehensive test covering:
 * 1. Create store
 * 2. Add 10 quads
 * 3. Freeze snapshot
 * 4. Reconstruct
 * 5. Verify hashes match
 * 6. Assert determinism (run twice, compare)
 */

import assert from 'node:assert';
import { test } from 'node:test';
import {
  createStoreAdapter,
  transactional,
  freeze,
  reconstruct,
  verifySnapshot,
} from '../src/store-adapter.mjs';
import { dataFactory } from '../src/mock-store.mjs';

const { namedNode, literal, defaultGraph, quad } = dataFactory;

/**
 * Test 1: Create store and add 10 quads
 */
test('Store Adapter - Create and Add Quads', async () => {
  const adapter = await createStoreAdapter();

  // Add 10 quads
  for (let i = 0; i < 10; i++) {
    const q = quad(
      namedNode(`http://example.org/subject${i}`),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal(`Name ${i}`),
      defaultGraph()
    );
    adapter.addQuad(q);
  }

  // Verify count
  const quads = adapter.query();
  assert.strictEqual(quads.length, 10, 'Store should have 10 quads');

  console.log('✅ Test 1 passed: Created store and added 10 quads');
});

/**
 * Test 2: Freeze snapshot and verify hash
 */
test('Store Adapter - Freeze Snapshot', async () => {
  const adapter = await createStoreAdapter();

  // Add 10 quads
  for (let i = 0; i < 10; i++) {
    const q = quad(
      namedNode(`http://example.org/subject${i}`),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal(`Name ${i}`),
      defaultGraph()
    );
    adapter.addQuad(q);
  }

  // Freeze
  const frozen = await freeze(adapter);

  // Verify structure
  assert.ok(frozen.hash, 'Frozen snapshot should have hash');
  assert.ok(frozen.timestamp, 'Frozen snapshot should have timestamp');
  assert.ok(frozen.snapshot, 'Frozen snapshot should have snapshot data');
  assert.strictEqual(typeof frozen.hash, 'string', 'Hash should be string');
  assert.strictEqual(frozen.hash.length, 64, 'SHA-256 hash should be 64 hex chars');

  console.log('✅ Test 2 passed: Froze snapshot with hash:', frozen.hash.slice(0, 16) + '...');
});

/**
 * Test 3: Reconstruct from snapshot
 */
test('Store Adapter - Reconstruct from Snapshot', async () => {
  const adapter = await createStoreAdapter();

  // Add 10 quads
  for (let i = 0; i < 10; i++) {
    const q = quad(
      namedNode(`http://example.org/subject${i}`),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal(`Name ${i}`),
      defaultGraph()
    );
    adapter.addQuad(q);
  }

  // Freeze
  const frozen = await freeze(adapter);

  // Reconstruct
  const reconstructed = await reconstruct(frozen);

  // Verify reconstructed store
  const reconstructedQuads = reconstructed.query();
  assert.strictEqual(reconstructedQuads.length, 10, 'Reconstructed store should have 10 quads');

  console.log('✅ Test 3 passed: Reconstructed store from snapshot');
});

/**
 * Test 4: Verify hashes match after reconstruction
 */
test('Store Adapter - Verify Hashes Match', async () => {
  const adapter = await createStoreAdapter();

  // Add 10 quads
  for (let i = 0; i < 10; i++) {
    const q = quad(
      namedNode(`http://example.org/subject${i}`),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal(`Name ${i}`),
      defaultGraph()
    );
    adapter.addQuad(q);
  }

  // Freeze original
  const originalFrozen = await freeze(adapter);

  // Reconstruct
  const reconstructed = await reconstruct(originalFrozen);

  // Freeze reconstructed
  const reconstructedFrozen = await freeze(reconstructed);

  // Verify hashes match
  assert.strictEqual(
    originalFrozen.hash,
    reconstructedFrozen.hash,
    'Original and reconstructed hashes should match'
  );

  // Use verifySnapshot
  const verification = await verifySnapshot(reconstructed, originalFrozen);
  assert.strictEqual(verification.valid, true, 'Snapshot should be valid');

  console.log('✅ Test 4 passed: Hashes match after reconstruction');
  console.log('   Original hash:', originalFrozen.hash.slice(0, 16) + '...');
  console.log('   Reconstructed hash:', reconstructedFrozen.hash.slice(0, 16) + '...');
});

/**
 * Test 5: Assert determinism (run freeze twice, compare hashes)
 */
test('Store Adapter - Determinism', async () => {
  const adapter = await createStoreAdapter();

  // Add 10 quads
  for (let i = 0; i < 10; i++) {
    const q = quad(
      namedNode(`http://example.org/subject${i}`),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal(`Name ${i}`),
      defaultGraph()
    );
    adapter.addQuad(q);
  }

  // Freeze twice
  const frozen1 = await freeze(adapter);
  const frozen2 = await freeze(adapter);

  // Hashes should match (deterministic)
  assert.strictEqual(
    frozen1.hash,
    frozen2.hash,
    'Multiple freezes of same store should produce same hash (deterministic)'
  );

  console.log('✅ Test 5 passed: Freeze is deterministic');
  console.log('   First freeze:', frozen1.hash.slice(0, 16) + '...');
  console.log('   Second freeze:', frozen2.hash.slice(0, 16) + '...');
});

/**
 * Test 6: Transaction with rollback
 */
test('Store Adapter - Transaction Rollback', async () => {
  const adapter = await createStoreAdapter();

  // Add initial quad
  const q1 = quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice'),
    defaultGraph()
  );
  adapter.addQuad(q1);

  // Verify initial state
  assert.strictEqual(adapter.query().length, 1, 'Should have 1 quad initially');

  // Transaction that fails
  try {
    await transactional(adapter, async (txAdapter) => {
      const q2 = quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Bob'),
        defaultGraph()
      );
      txAdapter.addQuad(q2);

      // Throw error to trigger rollback
      throw new Error('Transaction failed');
    });
  } catch (error) {
    // Expected error
    assert.ok(error.message.includes('Transaction failed'), 'Should throw transaction error');
  }

  // Verify rollback - should still have only 1 quad
  assert.strictEqual(adapter.query().length, 1, 'Should have 1 quad after rollback');

  console.log('✅ Test 6 passed: Transaction rollback works correctly');
});

/**
 * Test 7: Empty store snapshot
 */
test('Store Adapter - Empty Store Snapshot', async () => {
  const adapter = await createStoreAdapter();

  // Freeze empty store
  const frozen = await freeze(adapter);

  assert.ok(frozen.hash, 'Empty store should have hash');
  assert.strictEqual(frozen.snapshot, '', 'Empty store snapshot should be empty string');

  // Reconstruct empty store
  const reconstructed = await reconstruct(frozen);
  assert.strictEqual(reconstructed.query().length, 0, 'Reconstructed empty store should have 0 quads');

  console.log('✅ Test 7 passed: Empty store snapshot works');
});

/**
 * Test 8: Full E2E workflow
 */
test('Store Adapter - Full E2E Workflow', async () => {
  // 1. Create store
  const adapter = await createStoreAdapter();
  console.log('   1. Created store');

  // 2. Add 10 quads
  for (let i = 0; i < 10; i++) {
    const q = quad(
      namedNode(`http://example.org/subject${i}`),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal(`Name ${i}`),
      defaultGraph()
    );
    adapter.addQuad(q);
  }
  console.log('   2. Added 10 quads');

  // 3. Freeze snapshot
  const frozen = await freeze(adapter);
  console.log('   3. Froze snapshot:', frozen.hash.slice(0, 16) + '...');

  // 4. Reconstruct
  const reconstructed = await reconstruct(frozen);
  console.log('   4. Reconstructed store');

  // 5. Verify hashes match
  const verification = await verifySnapshot(reconstructed, frozen);
  assert.strictEqual(verification.valid, true, 'Snapshot verification should pass');
  console.log('   5. Verified hashes match');

  // 6. Assert determinism (run twice)
  const frozen2 = await freeze(adapter);
  assert.strictEqual(frozen.hash, frozen2.hash, 'Deterministic freezing should produce same hash');
  console.log('   6. Verified determinism');

  console.log('✅ Test 8 passed: Full E2E workflow completed successfully');
});

// Summary
console.log('\n=== Store Adapter E2E Test Suite ===');
console.log('Testing unified store adapter pattern with:');
console.log('- createStoreAdapter() - unified interface');
console.log('- transactional() - all-or-nothing semantics');
console.log('- freeze() - immutable snapshots');
console.log('- reconstruct() - restore from snapshot');
console.log('- verifySnapshot() - hash verification');
console.log('=====================================\n');

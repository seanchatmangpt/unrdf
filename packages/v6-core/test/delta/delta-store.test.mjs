/**
 * Delta Store Tests - v6-core
 *
 * Tests covering:
 * - DeltaStore CRUD operations
 * - Status tracking (proposed, applied, rejected)
 * - State management integration
 * - File reading capabilities
 *
 * @module @unrdf/v6-core/test/delta/store
 */

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { writeFile, unlink } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import {
  DeltaStore,
  createDeltaStore,
  readDeltaFromFile,
  DeltaStatus,
  getDefaultStore,
  resetDefaultStore
} from '../../src/delta/store.mjs';

// ============================================================================
// Test Suite 1: Basic CRUD Operations (5 tests)
// ============================================================================

test('DeltaStore - store and retrieve delta', async () => {
  const store = new DeltaStore();

  const delta = {
    id: 'delta-1',
    from: 'state-abc',
    to: 'state-def',
    operations: [
      { type: 'add', subject: 's1', predicate: 'p1', object: 'o1' }
    ],
    metadata: {
      proposedAt: new Date().toISOString(),
      status: 'proposed'
    }
  };

  await store.store('delta-1', delta);
  const retrieved = await store.get('delta-1');

  assert.deepStrictEqual(retrieved, delta);
});

test('DeltaStore - has() checks existence', async () => {
  const store = new DeltaStore();

  const delta = {
    id: 'delta-2',
    from: 'state-1',
    to: 'state-2',
    operations: [{ type: 'add', subject: 's', predicate: 'p', object: 'o' }],
    metadata: { proposedAt: new Date().toISOString(), status: 'proposed' }
  };

  await store.store('delta-2', delta);

  assert.strictEqual(await store.has('delta-2'), true);
  assert.strictEqual(await store.has('nonexistent'), false);
});

test('DeltaStore - list all deltas', async () => {
  const store = new DeltaStore();

  const delta1 = {
    id: 'delta-3',
    from: 'state-1',
    to: 'state-2',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'proposed' }
  };

  const delta2 = {
    id: 'delta-4',
    from: 'state-2',
    to: 'state-3',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'applied' }
  };

  await store.store('delta-3', delta1);
  await store.store('delta-4', delta2);

  const allDeltas = await store.list();
  assert.strictEqual(allDeltas.length, 2);
});

test('DeltaStore - filter by status', async () => {
  const store = new DeltaStore();

  await store.store('delta-5', {
    id: 'delta-5',
    from: 'state-1',
    to: 'state-2',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'proposed' }
  });

  await store.store('delta-6', {
    id: 'delta-6',
    from: 'state-2',
    to: 'state-3',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'applied' }
  });

  const proposedDeltas = await store.list({ status: 'proposed' });
  assert.strictEqual(proposedDeltas.length, 1);
  assert.strictEqual(proposedDeltas[0].id, 'delta-5');

  const appliedDeltas = await store.list({ status: 'applied' });
  assert.strictEqual(appliedDeltas.length, 1);
  assert.strictEqual(appliedDeltas[0].id, 'delta-6');
});

test('DeltaStore - delete delta', async () => {
  const store = new DeltaStore();

  await store.store('delta-7', {
    id: 'delta-7',
    from: 'state-1',
    to: 'state-2',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'proposed' }
  });

  assert.strictEqual(await store.has('delta-7'), true);

  const deleted = await store.delete('delta-7');
  assert.strictEqual(deleted, true);
  assert.strictEqual(await store.has('delta-7'), false);

  // Try deleting again
  const deletedAgain = await store.delete('delta-7');
  assert.strictEqual(deletedAgain, false);
});

// ============================================================================
// Test Suite 2: Status Management (3 tests)
// ============================================================================

test('DeltaStore - mark delta as applied', async () => {
  const store = new DeltaStore();

  await store.store('delta-8', {
    id: 'delta-8',
    from: 'state-1',
    to: 'state-2',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'proposed' }
  });

  const updated = await store.markApplied('delta-8', {
    stateHashAfter: 'state-2-actual'
  });

  assert.strictEqual(updated.metadata.status, 'applied');
  assert.ok(updated.metadata.appliedAt);
  assert.strictEqual(updated.metadata.stateHashAfter, 'state-2-actual');
});

test('DeltaStore - mark delta as rejected', async () => {
  const store = new DeltaStore();

  await store.store('delta-9', {
    id: 'delta-9',
    from: 'state-1',
    to: 'state-2',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'proposed' }
  });

  const updated = await store.markRejected('delta-9', 'Failed admissibility check');

  assert.strictEqual(updated.metadata.status, 'rejected');
  assert.ok(updated.metadata.rejectedAt);
  assert.strictEqual(updated.metadata.rejectionReason, 'Failed admissibility check');
});

test('DeltaStore - status update throws if not found', async () => {
  const store = new DeltaStore();

  await assert.rejects(
    async () => store.markApplied('nonexistent'),
    /Delta not found/
  );

  await assert.rejects(
    async () => store.markRejected('nonexistent', 'reason'),
    /Delta not found/
  );
});

// ============================================================================
// Test Suite 3: Statistics and Queries (2 tests)
// ============================================================================

test('DeltaStore - get statistics', async () => {
  const store = new DeltaStore();

  await store.store('delta-10', {
    id: 'delta-10',
    from: 'state-1',
    to: 'state-2',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'proposed' }
  });

  await store.store('delta-11', {
    id: 'delta-11',
    from: 'state-2',
    to: 'state-3',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'applied' }
  });

  await store.store('delta-12', {
    id: 'delta-12',
    from: 'state-3',
    to: 'state-4',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'rejected' }
  });

  const stats = await store.getStats();

  assert.strictEqual(stats.total, 3);
  assert.strictEqual(stats.byStatus.proposed, 1);
  assert.strictEqual(stats.byStatus.applied, 1);
  assert.strictEqual(stats.byStatus.rejected, 1);
});

test('DeltaStore - clear all deltas', async () => {
  const store = new DeltaStore();

  await store.store('delta-13', {
    id: 'delta-13',
    from: 'state-1',
    to: 'state-2',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'proposed' }
  });

  assert.strictEqual((await store.list()).length, 1);

  await store.clear();

  assert.strictEqual((await store.list()).length, 0);
});

// ============================================================================
// Test Suite 4: File Reading (3 tests)
// ============================================================================

test('readDeltaFromFile - reads valid JSON file', async () => {
  const tmpFile = join(tmpdir(), `delta-test-${Date.now()}.json`);

  const deltaData = {
    from: 'state-abc',
    to: 'state-def',
    operations: [
      { type: 'add', subject: 's', predicate: 'p', object: 'o' }
    ]
  };

  await writeFile(tmpFile, JSON.stringify(deltaData, null, 2), 'utf-8');

  try {
    const delta = await readDeltaFromFile(tmpFile);
    assert.deepStrictEqual(delta, deltaData);
  } finally {
    await unlink(tmpFile);
  }
});

test('readDeltaFromFile - throws on missing file', async () => {
  await assert.rejects(
    async () => readDeltaFromFile('/nonexistent/path.json'),
    /Failed to read delta from file/
  );
});

test('readDeltaFromFile - throws on invalid JSON', async () => {
  const tmpFile = join(tmpdir(), `delta-test-invalid-${Date.now()}.json`);

  await writeFile(tmpFile, 'invalid json{{{', 'utf-8');

  try {
    await assert.rejects(
      async () => readDeltaFromFile(tmpFile),
      /Failed to read delta from file/
    );
  } finally {
    await unlink(tmpFile);
  }
});

// ============================================================================
// Test Suite 5: Factory and Singleton (2 tests)
// ============================================================================

test('createDeltaStore - factory function', async () => {
  const store = createDeltaStore();

  assert.ok(store instanceof DeltaStore);

  await store.store('delta-14', {
    id: 'delta-14',
    from: 'state-1',
    to: 'state-2',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'proposed' }
  });

  assert.strictEqual(await store.has('delta-14'), true);
});

test('getDefaultStore - singleton instance', async () => {
  resetDefaultStore(); // Clear any previous state

  const store1 = getDefaultStore();
  const store2 = getDefaultStore();

  assert.strictEqual(store1, store2);

  await store1.store('delta-15', {
    id: 'delta-15',
    from: 'state-1',
    to: 'state-2',
    operations: [],
    metadata: { proposedAt: new Date().toISOString(), status: 'proposed' }
  });

  assert.strictEqual(await store2.has('delta-15'), true);

  resetDefaultStore();
});

// ============================================================================
// Test Suite 6: State Store Integration (3 tests)
// ============================================================================

test('DeltaStore - getCurrentStateHash with no state store', async () => {
  const store = new DeltaStore();
  const hash = await store.getCurrentStateHash();
  assert.strictEqual(hash, null);
});

test('DeltaStore - getCurrentStateHash with state store', async () => {
  const mockStateStore = {
    getStateHash: async () => 'state-hash-123'
  };

  const store = new DeltaStore({ stateStore: mockStateStore });
  const hash = await store.getCurrentStateHash();
  assert.strictEqual(hash, 'state-hash-123');
});

test('DeltaStore - applyOperations returns result', async () => {
  const store = new DeltaStore();

  const operations = [
    { type: 'add', subject: 's1', predicate: 'p1', object: 'o1' }
  ];

  const result = await store.applyOperations(operations);

  assert.strictEqual(result.applied, true);
  assert.strictEqual(result.operationCount, 1);
  assert.strictEqual(result.stateHash, null);
});

console.log('\n✅ All delta store tests passed');

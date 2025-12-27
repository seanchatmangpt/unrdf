/**
 * @fileoverview Tests for Two-Phase Commit Transaction Manager
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { TransactionManager } from '../src/transaction.mjs';

describe('TransactionManager', () => {
  let txManager;

  beforeEach(() => {
    txManager = new TransactionManager();
  });

  it('should execute two-phase commit successfully', async () => {
    // Begin transaction
    const tx = txManager.begin([
      {
        id: 'op1',
        type: 'add_capsule',
        data: { id: 'capsule1', content: 'test data' },
      },
      {
        id: 'op2',
        type: 'add_capsule',
        data: { id: 'capsule2', content: 'more test data' },
      },
    ]);

    expect(tx.status).toBe('pending');
    expect(tx.operations).toHaveLength(2);

    // Phase 1: Prepare
    const prepareResult = await txManager.prepare(tx.id);

    expect(prepareResult.success).toBe(true);
    expect(prepareResult.errors).toHaveLength(0);
    expect(prepareResult.undoOps).toHaveLength(2);

    const updatedTx = txManager.getTransaction(tx.id);
    expect(updatedTx.status).toBe('prepared');

    // Phase 2: Commit
    const commitResult = await txManager.commit(tx.id);

    expect(commitResult.success).toBe(true);
    expect(commitResult.receipts).toHaveLength(2);
    expect(commitResult.errors).toHaveLength(0);

    const finalTx = txManager.getTransaction(tx.id);
    expect(finalTx.status).toBe('committed');

    // Verify state updated
    const state = txManager.getState();
    expect(state.capsules).toBeDefined();
    expect(state.capsules['capsule1']).toBeDefined();
    expect(state.capsules['capsule2']).toBeDefined();
  });

  it('should rollback transaction on commit failure', async () => {
    // Add a capsule first
    const initialTx = txManager.begin([
      {
        id: 'initial',
        type: 'add_capsule',
        data: { id: 'existing', content: 'existing data' },
      },
    ]);

    await txManager.prepare(initialTx.id);
    await txManager.commit(initialTx.id);

    // Create transaction that will fail during commit
    const tx = txManager.begin([
      {
        id: 'op1',
        type: 'add_capsule',
        data: { id: 'new_capsule', content: 'new data' },
      },
      {
        id: 'op2',
        type: 'invalid_type', // This will cause failure
        data: {},
      },
    ]);

    const prepareResult = await txManager.prepare(tx.id);
    expect(prepareResult.success).toBe(false);

    const finalTx = txManager.getTransaction(tx.id);
    expect(finalTx.status).toBe('aborted');

    // Verify state not changed
    const state = txManager.getState();
    expect(state.capsules['new_capsule']).toBeUndefined();
    expect(state.capsules['existing']).toBeDefined(); // Original still there
  });

  it('should support cascading transactions with parent hash', async () => {
    // First transaction
    const tx1 = txManager.begin([
      {
        id: 'op1',
        type: 'add_capsule',
        data: { id: 'capsule1', content: 'first' },
      },
    ]);

    await txManager.prepare(tx1.id);
    const commit1 = await txManager.commit(tx1.id);

    expect(commit1.success).toBe(true);
    expect(commit1.receipts).toHaveLength(1);

    const receipt1 = commit1.receipts[0];
    const parentHash = receipt1.hash;

    // Second transaction with parent hash
    const tx2 = txManager.begin(
      [
        {
          id: 'op2',
          type: 'add_capsule',
          data: { id: 'capsule2', content: 'second' },
        },
      ],
      parentHash
    );

    expect(tx2.parentHash).toBe(parentHash);

    await txManager.prepare(tx2.id);
    await txManager.commit(tx2.id);

    // Verify both transactions committed
    const finalTx1 = txManager.getTransaction(tx1.id);
    const finalTx2 = txManager.getTransaction(tx2.id);

    expect(finalTx1.status).toBe('committed');
    expect(finalTx2.status).toBe('committed');
    expect(finalTx2.parentHash).toBe(receipt1.hash);
  });

  it('should handle rollback with undo operations', async () => {
    // Add initial capsule
    const tx1 = txManager.begin([
      {
        id: 'op1',
        type: 'add_capsule',
        data: { id: 'capsule1', content: 'original' },
      },
    ]);

    await txManager.prepare(tx1.id);
    await txManager.commit(tx1.id);

    // Verify capsule added
    let state = txManager.getState();
    expect(state.capsules).toBeDefined();
    expect(state.capsules['capsule1']).toBeDefined();

    // Create transaction to remove it
    const tx2 = txManager.begin([
      {
        id: 'op2',
        type: 'remove_capsule',
        data: { capsule_id: 'capsule1' },
      },
    ]);

    await txManager.prepare(tx2.id);
    await txManager.commit(tx2.id);

    // Verify capsule removed
    state = txManager.getState();
    expect(state.capsules['capsule1']).toBeUndefined();

    // Rollback the removal
    const rollbackResult = await txManager.rollback(tx2.id);

    expect(rollbackResult.success).toBe(true);
    expect(rollbackResult.undone).toBe(1);

    // Verify capsule restored
    state = txManager.getState();
    expect(state.capsules['capsule1']).toBeDefined();
    expect(state.capsules['capsule1'].content).toBe('original');
  });
});

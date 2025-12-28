/**
 * @fileoverview Tests for Rollback Event Log System
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { RollbackLog } from '../src/rollback.mjs';
import { promises as fs } from 'fs';
import path from 'path';

const TEST_LOG_PATH = './var/kgc/test-undo-log.json';

describe('RollbackLog', () => {
  let rollbackLog;

  beforeEach(async () => {
    rollbackLog = new RollbackLog(TEST_LOG_PATH);
    // Clear any existing log
    try {
      await fs.unlink(TEST_LOG_PATH);
    } catch (error) {
      // File doesn't exist - that's fine
    }
  });

  afterEach(async () => {
    // Cleanup
    try {
      await fs.unlink(TEST_LOG_PATH);
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  it('should append and retrieve undo log entries', async () => {
    const txId = 'tx_test_001';
    const operations = [
      { id: 'op1', type: 'add_capsule', data: { id: 'c1' } },
      { id: 'op2', type: 'add_capsule', data: { id: 'c2' } },
    ];

    // Append entry
    const entry = await rollbackLog.append(txId, operations, 'hash123');

    expect(entry.transaction_id).toBe(txId);
    expect(entry.operations).toHaveLength(2);
    expect(entry.hash).toBe('hash123');
    expect(entry.timestamp).toBeDefined();

    // Retrieve entry
    const retrieved = await rollbackLog.getByTransactionId(txId);

    expect(retrieved).toBeDefined();
    expect(retrieved.transaction_id).toBe(txId);
    expect(retrieved.operations).toHaveLength(2);

    // Verify persistence
    const newLog = new RollbackLog(TEST_LOG_PATH);
    const persistedEntry = await newLog.getByTransactionId(txId);

    expect(persistedEntry).toBeDefined();
    expect(persistedEntry.transaction_id).toBe(txId);
  });

  it('should replay undo operations in correct order', async () => {
    const txId = 'tx_replay_001';
    const operations = [
      { id: 'op1', type: 'remove_capsule', data: { capsule_id: 'c1' } },
      { id: 'op2', type: 'remove_capsule', data: { capsule_id: 'c2' } },
      { id: 'op3', type: 'remove_capsule', data: { capsule_id: 'c3' } },
    ];

    await rollbackLog.append(txId, operations);

    // Track applied operations
    const applied = [];
    const applyOp = async (op) => {
      applied.push(op.id);
    };

    // Replay
    const result = await rollbackLog.replay(txId, applyOp);

    expect(result.success).toBe(true);
    expect(result.operations_applied).toBe(3);
    expect(result.errors).toHaveLength(0);

    // Verify operations applied in reverse order
    expect(applied).toEqual(['op3', 'op2', 'op1']);
  });

  it('should manage multiple log entries and provide statistics', async () => {
    // Add multiple entries
    await rollbackLog.append('tx_001', [{ id: 'op1', type: 'add_capsule', data: {} }]);

    // Wait a bit to ensure different timestamps
    await new Promise(resolve => setTimeout(resolve, 10));

    await rollbackLog.append('tx_002', [{ id: 'op2', type: 'add_capsule', data: {} }]);

    await new Promise(resolve => setTimeout(resolve, 10));

    await rollbackLog.append('tx_003', [
      { id: 'op3', type: 'add_capsule', data: {} },
      { id: 'op4', type: 'add_capsule', data: {} },
    ]);

    // Get all entries
    const allEntries = await rollbackLog.getAll();
    expect(allEntries).toHaveLength(3);

    // Get statistics
    const stats = await rollbackLog.getStats();
    expect(stats.total).toBe(3);
    expect(stats.oldest).toBeDefined();
    expect(stats.newest).toBeDefined();
    expect(new Date(stats.newest).getTime()).toBeGreaterThanOrEqual(
      new Date(stats.oldest).getTime()
    );

    // Get by time range
    const start = new Date(stats.oldest);
    const end = new Date(stats.newest);

    const rangeEntries = await rollbackLog.getByTimeRange(start, end);
    expect(rangeEntries).toHaveLength(3);

    // Clear log
    await rollbackLog.clear();
    const clearedStats = await rollbackLog.getStats();
    expect(clearedStats.total).toBe(0);
  });
});

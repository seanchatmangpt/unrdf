/**
 * Transaction Manager Tests - Ultra-fast
 * Commit smoke test only
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { TransactionManager } from '../src/transaction.mjs';

describe('TransactionManager', () => {
  let txManager;

  beforeEach(() => {
    txManager = new TransactionManager();
  });

  it('should begin transaction', () => {
    const tx = txManager.begin([{ id: 'op1', type: 'add_capsule', data: { id: 'c1', content: 'test' } }]);
    expect(tx).toBeDefined();
    expect(tx.id).toBeDefined();
    expect(tx.status).toBe('pending');
  });
});

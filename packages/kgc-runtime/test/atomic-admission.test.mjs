/**
 * @fileoverview Tests for Atomic Capsule Admission
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { AtomicAdmissionGate, cascadingRollback } from '../src/atomic-admission.mjs';

const createCapsule = (id, o_hash, file_edits) => ({
  id,
  o_hash,
  file_edits,
  metadata: { created_at: new Date().toISOString() },
});

const createFileEdit = (file_path, line_start, line_end, content, operation = 'replace') => ({
  file_path,
  line_start,
  line_end,
  content,
  operation,
});

const defaultTotalOrder = {
  rules: [],
  default_rule: {
    strategy: 'earlier_wins',
  },
};

describe('AtomicAdmissionGate', () => {
  let gate;

  beforeEach(() => {
    gate = new AtomicAdmissionGate({ logPath: './var/kgc/test-atomic-log.json' });
  });

  it('should admit all capsules atomically when no conflicts', async () => {
    const capsules = [
      createCapsule('c1', 'hash1', [
        createFileEdit('file1.js', 1, 5, 'content1'),
      ]),
      createCapsule('c2', 'hash2', [
        createFileEdit('file2.js', 1, 5, 'content2'),
      ]),
      createCapsule('c3', 'hash3', [
        createFileEdit('file3.js', 1, 5, 'content3'),
      ]),
    ];

    const result = await gate.admitCapsules(capsules, defaultTotalOrder);

    expect(result.success).toBe(true);
    expect(result.admitted).toHaveLength(3);
    expect(result.admitted).toContain('c1');
    expect(result.admitted).toContain('c2');
    expect(result.admitted).toContain('c3');
    expect(result.denied).toHaveLength(0);
    expect(result.transaction_id).toBeDefined();
    expect(result.receipts).toBeDefined();
    expect(result.receipts).toHaveLength(3);

    // Verify capsules are admitted
    expect(gate.isAdmitted('c1')).toBe(true);
    expect(gate.isAdmitted('c2')).toBe(true);
    expect(gate.isAdmitted('c3')).toBe(true);
  });

  it('should reject all capsules atomically when conflicts detected', async () => {
    const capsules = [
      createCapsule('c1', 'hash_a', [
        createFileEdit('file.js', 1, 10, 'content1'),
      ]),
      createCapsule('c2', 'hash_b', [
        createFileEdit('file.js', 5, 15, 'content2'), // Conflict with c1
      ]),
    ];

    const result = await gate.admitCapsules(capsules, defaultTotalOrder);

    expect(result.success).toBe(false);
    expect(result.admitted).toHaveLength(0);
    expect(result.denied).toHaveLength(1); // One capsule denied due to conflict
    expect(result.conflict_receipts).toHaveLength(1);
    expect(result.errors).toBeDefined();
    expect(result.errors[0]).toContain('Conflict detected');

    // Verify no capsules admitted
    expect(gate.isAdmitted('c1')).toBe(false);
    expect(gate.isAdmitted('c2')).toBe(false);
  });

  it('should rollback transaction and remove admitted capsules', async () => {
    const capsules = [
      createCapsule('c1', 'hash1', [
        createFileEdit('file1.js', 1, 5, 'content1'),
      ]),
      createCapsule('c2', 'hash2', [
        createFileEdit('file2.js', 1, 5, 'content2'),
      ]),
    ];

    // Admit capsules
    const admitResult = await gate.admitCapsules(capsules, defaultTotalOrder);

    expect(admitResult.success).toBe(true);
    expect(gate.isAdmitted('c1')).toBe(true);
    expect(gate.isAdmitted('c2')).toBe(true);

    // Rollback transaction
    const rollbackResult = await gate.rollbackTransaction(admitResult.transaction_id);

    expect(rollbackResult.success).toBe(true);
    expect(rollbackResult.undone).toBeGreaterThan(0);

    // Verify capsules removed from admitted set
    expect(gate.isAdmitted('c1')).toBe(false);
    expect(gate.isAdmitted('c2')).toBe(false);
  });
});

describe('Cascading Rollback', () => {
  it('should rollback transaction and all dependent transactions', async () => {
    const gate = new AtomicAdmissionGate({ logPath: './var/kgc/test-cascading-log.json' });

    // Transaction 1
    const capsules1 = [
      createCapsule('c1', 'hash1', [
        createFileEdit('file1.js', 1, 5, 'content1'),
      ]),
    ];

    const result1 = await gate.admitCapsules(capsules1, defaultTotalOrder);
    expect(result1.success).toBe(true);

    // Transaction 2 (dependent on tx1)
    const capsules2 = [
      createCapsule('c2', 'hash2', [
        createFileEdit('file2.js', 1, 5, 'content2'),
      ]),
    ];

    const result2 = await gate.admitCapsules(capsules2, defaultTotalOrder);
    expect(result2.success).toBe(true);

    // Cascading rollback
    const rollbackResult = await cascadingRollback(gate, result1.transaction_id);

    expect(rollbackResult.success).toBe(true);
    expect(rollbackResult.rolled_back).toContain(result1.transaction_id);

    // Verify all capsules removed
    expect(gate.isAdmitted('c1')).toBe(false);
    expect(gate.isAdmitted('c2')).toBe(false);
  });
});

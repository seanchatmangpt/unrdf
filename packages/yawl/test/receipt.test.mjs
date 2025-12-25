/**
 * YAWL Receipt Module Tests
 * Tests BLAKE3 cryptographic receipt generation and verification
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  generateReceipt,
  verifyReceipt,
  verifyChainLink,
  ProofChain,
  RECEIPT_EVENT_TYPES,
  ReceiptSchema,
  YawlReceipt,
  buildReceipt,
} from '../src/receipt.mjs';

// =============================================================================
// generateReceipt Tests
// =============================================================================

describe('generateReceipt', () => {
  it('should generate a valid receipt with all required fields', async () => {
    const event = {
      eventType: 'TASK_ENABLED',
      caseId: 'case-001',
      taskId: 'Approval',
      workItemId: 'wi-001',
      payload: {
        decision: 'ENABLE',
        justification: {
          hookValidated: 'pre-enable-hook',
          sparqlQuery: 'ASK { ?case :hasManager ?m }',
          reasoning: 'Preconditions satisfied',
        },
        actor: 'system',
      },
    };

    const receipt = await generateReceipt(event);

    // Verify structure
    expect(receipt.id).toBeDefined();
    expect(receipt.eventType).toBe('TASK_ENABLED');
    expect(receipt.caseId).toBe('case-001');
    expect(receipt.taskId).toBe('Approval');
    expect(receipt.workItemId).toBe('wi-001');

    // Verify timestamps
    expect(typeof receipt.t_ns).toBe('bigint');
    expect(receipt.t_ns).toBeGreaterThan(0n);
    expect(receipt.timestamp_iso).toMatch(/^\d{4}-\d{2}-\d{2}T/);

    // Verify cryptographic hashes (64-char BLAKE3)
    expect(receipt.payloadHash).toHaveLength(64);
    expect(receipt.receiptHash).toHaveLength(64);
    expect(receipt.previousReceiptHash).toBeNull();

    // Verify payload preserved
    expect(receipt.payload.decision).toBe('ENABLE');
    expect(receipt.payload.justification.hookValidated).toBe('pre-enable-hook');
    expect(receipt.payload.actor).toBe('system');
  });

  it('should chain receipts correctly', async () => {
    const event1 = {
      eventType: 'CASE_CREATED',
      caseId: 'case-002',
      taskId: 'Start',
      payload: { decision: 'CREATE', actor: 'user1' },
    };

    const event2 = {
      eventType: 'TASK_ENABLED',
      caseId: 'case-002',
      taskId: 'Approval',
      payload: { decision: 'ENABLE', actor: 'system' },
    };

    const r1 = await generateReceipt(event1, null);
    const r2 = await generateReceipt(event2, r1);

    // First receipt has no previous
    expect(r1.previousReceiptHash).toBeNull();

    // Second receipt chains to first
    expect(r2.previousReceiptHash).toBe(r1.receiptHash);

    // Receipt hashes are unique
    expect(r1.receiptHash).not.toBe(r2.receiptHash);
  });

  it('should include KGC-4D integration fields', async () => {
    const event = {
      eventType: 'TASK_COMPLETED',
      caseId: 'case-003',
      taskId: 'ProcessData',
      payload: { decision: 'COMPLETE' },
      kgcEventId: 'evt-12345',
      gitRef: 'abc123def456',
      vectorClock: {
        nodeId: 'node-1',
        counters: { 'node-1': '5' },
      },
    };

    const receipt = await generateReceipt(event);

    expect(receipt.kgcEventId).toBe('evt-12345');
    expect(receipt.gitRef).toBe('abc123def456');
    expect(receipt.vectorClock.nodeId).toBe('node-1');
    expect(receipt.vectorClock.counters['node-1']).toBe('5');
  });

  it('should reject invalid event types', async () => {
    const event = {
      eventType: 'INVALID_TYPE',
      caseId: 'case-004',
      taskId: 'Test',
      payload: { decision: 'TEST' },
    };

    await expect(generateReceipt(event)).rejects.toThrow('Invalid event type');
  });

  it('should produce deterministic hashes for same input', async () => {
    // Note: Due to timestamps, we can't get exact same hashes
    // But the hash algorithm should be deterministic given same inputs
    const event = {
      eventType: 'TASK_STARTED',
      caseId: 'case-005',
      taskId: 'Task1',
      payload: { decision: 'START' },
    };

    const r1 = await generateReceipt(event);
    const r2 = await generateReceipt(event);

    // Different timestamps mean different hashes
    expect(r1.receiptHash).not.toBe(r2.receiptHash);

    // But both should be valid 64-char hashes
    expect(r1.receiptHash).toHaveLength(64);
    expect(r2.receiptHash).toHaveLength(64);
  });
});

// =============================================================================
// verifyReceipt Tests
// =============================================================================

describe('verifyReceipt', () => {
  it('should verify a valid receipt', async () => {
    const event = {
      eventType: 'TASK_ENABLED',
      caseId: 'case-v1',
      taskId: 'Task1',
      payload: { decision: 'ENABLE' },
    };

    const receipt = await generateReceipt(event);
    const result = await verifyReceipt(receipt);

    expect(result.valid).toBe(true);
    expect(result.checks.payloadHashValid).toBe(true);
    expect(result.checks.chainHashValid).toBe(true);
    expect(result.checks.timestampValid).toBe(true);
  });

  it('should detect tampered payload', async () => {
    const event = {
      eventType: 'TASK_ENABLED',
      caseId: 'case-v2',
      taskId: 'Task1',
      payload: { decision: 'ENABLE' },
    };

    const receipt = await generateReceipt(event);

    // Tamper with payload
    receipt.payload.decision = 'TAMPERED';

    const result = await verifyReceipt(receipt);

    expect(result.valid).toBe(false);
    expect(result.error).toContain('payload hash mismatch');
    expect(result.checks.payloadHashValid).toBe(false);
  });

  it('should detect tampered receiptHash', async () => {
    const event = {
      eventType: 'TASK_COMPLETED',
      caseId: 'case-v3',
      taskId: 'Task1',
      payload: { decision: 'COMPLETE' },
    };

    const receipt = await generateReceipt(event);

    // Tamper with receipt hash
    receipt.receiptHash = 'a'.repeat(64);

    const result = await verifyReceipt(receipt);

    expect(result.valid).toBe(false);
    expect(result.error).toContain('chain hash mismatch');
  });
});

// =============================================================================
// verifyChainLink Tests
// =============================================================================

describe('verifyChainLink', () => {
  it('should verify valid chain link', async () => {
    const event1 = {
      eventType: 'CASE_CREATED',
      caseId: 'case-c1',
      taskId: 'Start',
      payload: { decision: 'CREATE' },
    };

    const event2 = {
      eventType: 'TASK_ENABLED',
      caseId: 'case-c1',
      taskId: 'Task1',
      payload: { decision: 'ENABLE' },
    };

    const r1 = await generateReceipt(event1, null);
    const r2 = await generateReceipt(event2, r1);

    const result = await verifyChainLink(r2, r1);

    expect(result.valid).toBe(true);
  });

  it('should detect broken chain link', async () => {
    const event1 = {
      eventType: 'CASE_CREATED',
      caseId: 'case-c2',
      taskId: 'Start',
      payload: { decision: 'CREATE' },
    };

    const event2 = {
      eventType: 'TASK_ENABLED',
      caseId: 'case-c2',
      taskId: 'Task1',
      payload: { decision: 'ENABLE' },
    };

    const r1 = await generateReceipt(event1, null);
    const r2 = await generateReceipt(event2, null); // Not chained!

    const result = await verifyChainLink(r2, r1);

    expect(result.valid).toBe(false);
    expect(result.error).toContain('Chain broken');
  });
});

// =============================================================================
// ProofChain Tests
// =============================================================================

describe('ProofChain', () => {
  let chain;

  beforeEach(() => {
    chain = new ProofChain('test-node');
  });

  it('should create empty chain', () => {
    expect(chain.length).toBe(0);
    expect(chain.getLatest()).toBeNull();
  });

  it('should append genesis receipt', async () => {
    const event = {
      eventType: 'CASE_CREATED',
      caseId: 'case-p1',
      taskId: 'Start',
      payload: { decision: 'CREATE' },
    };

    const receipt = await generateReceipt(event, null);
    await chain.append(receipt);

    expect(chain.length).toBe(1);
    expect(chain.getLatest()).toBe(receipt);
  });

  it('should append chained receipts', async () => {
    const events = [
      { eventType: 'CASE_CREATED', caseId: 'case-p2', taskId: 'Start', payload: { decision: 'CREATE' } },
      { eventType: 'TASK_ENABLED', caseId: 'case-p2', taskId: 'Task1', payload: { decision: 'ENABLE' } },
      { eventType: 'TASK_STARTED', caseId: 'case-p2', taskId: 'Task1', payload: { decision: 'START' } },
      { eventType: 'TASK_COMPLETED', caseId: 'case-p2', taskId: 'Task1', payload: { decision: 'COMPLETE' } },
    ];

    for (const event of events) {
      const receipt = await generateReceipt(event, chain.getLatest());
      await chain.append(receipt);
    }

    expect(chain.length).toBe(4);
  });

  it('should verify entire chain', async () => {
    const events = [
      { eventType: 'CASE_CREATED', caseId: 'case-p3', taskId: 'Start', payload: { decision: 'CREATE' } },
      { eventType: 'TASK_ENABLED', caseId: 'case-p3', taskId: 'Task1', payload: { decision: 'ENABLE' } },
      { eventType: 'TASK_COMPLETED', caseId: 'case-p3', taskId: 'Task1', payload: { decision: 'COMPLETE' } },
    ];

    for (const event of events) {
      const receipt = await generateReceipt(event, chain.getLatest());
      await chain.append(receipt);
    }

    const result = await chain.verify();
    expect(result.valid).toBe(true);
  });

  it('should compute Merkle root', async () => {
    const events = [
      { eventType: 'CASE_CREATED', caseId: 'case-p4', taskId: 'Start', payload: { decision: 'CREATE' } },
      { eventType: 'TASK_ENABLED', caseId: 'case-p4', taskId: 'Task1', payload: { decision: 'ENABLE' } },
    ];

    for (const event of events) {
      const receipt = await generateReceipt(event, chain.getLatest());
      await chain.append(receipt);
    }

    const root = await chain.getMerkleRoot();

    expect(root).toHaveLength(64);
    expect(root).toMatch(/^[0-9a-f]{64}$/);
  });

  it('should return zero hash for empty chain', async () => {
    const root = await chain.getMerkleRoot();
    expect(root).toBe('0'.repeat(64));
  });

  it('should get receipts by case', async () => {
    const events = [
      { eventType: 'CASE_CREATED', caseId: 'case-A', taskId: 'Start', payload: { decision: 'CREATE' } },
      { eventType: 'CASE_CREATED', caseId: 'case-B', taskId: 'Start', payload: { decision: 'CREATE' } },
      { eventType: 'TASK_ENABLED', caseId: 'case-A', taskId: 'Task1', payload: { decision: 'ENABLE' } },
    ];

    for (const event of events) {
      const receipt = await generateReceipt(event, chain.getLatest());
      await chain.append(receipt);
    }

    const caseAReceipts = chain.getReceiptsForCase('case-A');
    expect(caseAReceipts).toHaveLength(2);

    const caseBReceipts = chain.getReceiptsForCase('case-B');
    expect(caseBReceipts).toHaveLength(1);
  });

  it('should reject non-genesis first receipt', async () => {
    // Create a receipt with non-null previousReceiptHash
    const event = {
      eventType: 'CASE_CREATED',
      caseId: 'case-p5',
      taskId: 'Start',
      payload: { decision: 'CREATE' },
    };

    // Create a fake "previous" receipt
    const fakeReceipt = await generateReceipt(event, null);
    const chainedReceipt = await generateReceipt(event, fakeReceipt);

    await expect(chain.append(chainedReceipt)).rejects.toThrow('Genesis receipt must have null previousReceiptHash');
  });

  it('should serialize and deserialize', async () => {
    const events = [
      { eventType: 'CASE_CREATED', caseId: 'case-p6', taskId: 'Start', payload: { decision: 'CREATE' } },
      { eventType: 'TASK_ENABLED', caseId: 'case-p6', taskId: 'Task1', payload: { decision: 'ENABLE' } },
    ];

    for (const event of events) {
      const receipt = await generateReceipt(event, chain.getLatest());
      await chain.append(receipt);
    }

    const json = chain.toJSON();
    const restored = ProofChain.fromJSON(json);

    expect(restored.length).toBe(2);
    expect(restored.nodeId).toBe('test-node');

    // Verify restored chain is valid
    const result = await restored.verify();
    expect(result.valid).toBe(true);
  });

  it('should export audit trail', async () => {
    const events = [
      { eventType: 'CASE_CREATED', caseId: 'case-p7', taskId: 'Start', payload: { decision: 'CREATE' } },
      { eventType: 'TASK_ENABLED', caseId: 'case-p7', taskId: 'Task1', payload: { decision: 'ENABLE' } },
    ];

    for (const event of events) {
      const receipt = await generateReceipt(event, chain.getLatest());
      await chain.append(receipt);
    }

    const audit = await chain.exportAuditTrail();

    expect(audit.nodeId).toBe('test-node');
    expect(audit.receiptCount).toBe(2);
    expect(audit.chainValid).toBe(true);
    expect(audit.merkleRoot).toHaveLength(64);
    expect(audit.receipts).toHaveLength(2);
    expect(audit.exportedAt).toMatch(/^\d{4}-\d{2}-\d{2}T/);
  });

  it('should generate and verify Merkle proofs', async () => {
    const events = [
      { eventType: 'CASE_CREATED', caseId: 'case-p8', taskId: 'Start', payload: { decision: 'CREATE' } },
      { eventType: 'TASK_ENABLED', caseId: 'case-p8', taskId: 'Task1', payload: { decision: 'ENABLE' } },
      { eventType: 'TASK_STARTED', caseId: 'case-p8', taskId: 'Task1', payload: { decision: 'START' } },
      { eventType: 'TASK_COMPLETED', caseId: 'case-p8', taskId: 'Task1', payload: { decision: 'COMPLETE' } },
    ];

    for (const event of events) {
      const receipt = await generateReceipt(event, chain.getLatest());
      await chain.append(receipt);
    }

    const merkleRoot = await chain.getMerkleRoot();

    // Verify proof for each receipt
    for (let i = 0; i < chain.length; i++) {
      const receipt = chain.getReceipt(i);
      const proof = await chain.getMerkleProof(i);
      const isValid = await chain.verifyMerkleProof(receipt.receiptHash, proof, merkleRoot);
      expect(isValid).toBe(true);
    }
  });
});

// =============================================================================
// Legacy API Tests (Backward Compatibility)
// =============================================================================

describe('YawlReceipt (Legacy)', () => {
  it('should create and hash a legacy receipt', async () => {
    const receipt = new YawlReceipt({
      id: 'receipt-001',
      caseId: 'case-legacy-1',
      taskId: 'Task1',
      action: 'enable',
      timestamp: 1704067200000000000n,
      beforeHash: 'a'.repeat(64),
      afterHash: 'b'.repeat(64),
    });

    expect(receipt.valid).toBe(true);

    const hash = await receipt.computeHash();
    expect(hash).toHaveLength(64);
  });

  it('should verify legacy receipt integrity', async () => {
    const receipt = new YawlReceipt({
      id: 'receipt-002',
      caseId: 'case-legacy-2',
      taskId: 'Task1',
      action: 'complete',
      timestamp: 1704067200000000000n,
      beforeHash: 'a'.repeat(64),
      afterHash: 'b'.repeat(64),
    });

    await receipt.computeHash();
    const isValid = await receipt.verify();
    expect(isValid).toBe(true);
  });

  it('should serialize and deserialize legacy receipt', async () => {
    const receipt = new YawlReceipt({
      id: 'receipt-003',
      caseId: 'case-legacy-3',
      taskId: 'Task1',
      action: 'start',
      timestamp: 1704067200000000000n,
      beforeHash: 'a'.repeat(64),
      afterHash: 'b'.repeat(64),
      actor: 'user1',
    });

    await receipt.computeHash();

    const json = receipt.toJSON();
    const restored = YawlReceipt.fromJSON(json);

    expect(restored.id).toBe(receipt.id);
    expect(restored.caseId).toBe(receipt.caseId);
    expect(restored.timestamp).toBe(receipt.timestamp);
    expect(restored._hash).toBe(receipt._hash);
  });
});

describe('buildReceipt (Legacy)', () => {
  it('should build a legacy receipt with state hashes', async () => {
    const receipt = await buildReceipt({
      caseId: 'case-build-1',
      taskId: 'Task1',
      action: 'enable',
      beforeState: { workItems: [] },
      afterState: { workItems: [{ id: 'wi-1', state: 'enabled' }] },
    });

    expect(receipt).toBeInstanceOf(YawlReceipt);
    expect(receipt.caseId).toBe('case-build-1');
    expect(receipt.beforeHash).toHaveLength(64);
    expect(receipt.afterHash).toHaveLength(64);
  });

  it('should chain legacy receipts', async () => {
    const r1 = await buildReceipt({
      caseId: 'case-build-2',
      taskId: 'Task1',
      action: 'enable',
      beforeState: {},
      afterState: { enabled: true },
    });

    const r2 = await buildReceipt({
      caseId: 'case-build-2',
      taskId: 'Task1',
      action: 'complete',
      beforeState: { enabled: true },
      afterState: { completed: true },
      previousReceipt: r1,
    });

    expect(r2.previousReceiptHash).toBe(await r1.getHash());

    const chainResult = await r2.verifyChain(r1);
    expect(chainResult.valid).toBe(true);
  });
});

// =============================================================================
// RECEIPT_EVENT_TYPES Tests
// =============================================================================

describe('RECEIPT_EVENT_TYPES', () => {
  it('should have all required event types', () => {
    expect(RECEIPT_EVENT_TYPES.CASE_CREATED).toBe('CASE_CREATED');
    expect(RECEIPT_EVENT_TYPES.TASK_ENABLED).toBe('TASK_ENABLED');
    expect(RECEIPT_EVENT_TYPES.TASK_STARTED).toBe('TASK_STARTED');
    expect(RECEIPT_EVENT_TYPES.TASK_COMPLETED).toBe('TASK_COMPLETED');
    expect(RECEIPT_EVENT_TYPES.TASK_CANCELLED).toBe('TASK_CANCELLED');
    expect(RECEIPT_EVENT_TYPES.TASK_FAILED).toBe('TASK_FAILED');
    expect(RECEIPT_EVENT_TYPES.TASK_TIMEOUT).toBe('TASK_TIMEOUT');
    expect(RECEIPT_EVENT_TYPES.WORK_ITEM_CREATED).toBe('WORK_ITEM_CREATED');
    expect(RECEIPT_EVENT_TYPES.CONTROL_FLOW_EVALUATED).toBe('CONTROL_FLOW_EVALUATED');
    expect(RECEIPT_EVENT_TYPES.RESOURCE_ALLOCATED).toBe('RESOURCE_ALLOCATED');
    expect(RECEIPT_EVENT_TYPES.RESOURCE_RELEASED).toBe('RESOURCE_RELEASED');
  });

  it('should be frozen (immutable)', () => {
    expect(Object.isFrozen(RECEIPT_EVENT_TYPES)).toBe(true);
  });
});

// =============================================================================
// ReceiptSchema Tests
// =============================================================================

describe('ReceiptSchema', () => {
  it('should validate correct receipt structure', async () => {
    const event = {
      eventType: 'TASK_ENABLED',
      caseId: 'case-schema-1',
      taskId: 'Task1',
      payload: { decision: 'ENABLE' },
    };

    const receipt = await generateReceipt(event);

    const result = ReceiptSchema.safeParse(receipt);
    expect(result.success).toBe(true);
  });

  it('should reject invalid hash length', () => {
    const invalidReceipt = {
      id: crypto.randomUUID(),
      eventType: 'TASK_ENABLED',
      t_ns: 1000000000n,
      timestamp_iso: '2024-01-01T00:00:00.000Z',
      caseId: 'case-1',
      taskId: 'task-1',
      previousReceiptHash: null,
      payloadHash: 'tooshort',
      receiptHash: 'a'.repeat(64),
      payload: { decision: 'TEST' },
    };

    const result = ReceiptSchema.safeParse(invalidReceipt);
    expect(result.success).toBe(false);
  });
});

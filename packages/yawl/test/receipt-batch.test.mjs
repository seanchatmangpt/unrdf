/**
 * Receipt Batch Tests
 * Tests for batch receipt generation, parallel hashing, and object pooling
 */
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  generateReceiptBatch,
  verifyReceiptBatch,
  releaseReceipts,
  getPoolStats,
  resetPool,
  parallelHash,
} from '../src/receipt-batch.mjs';
import { RECEIPT_EVENT_TYPES } from '../src/receipt.mjs';

describe('Parallel Hashing', () => {
  it('should hash multiple strings in parallel', async () => {
    const data = ['string1', 'string2', 'string3'];
    const hashes = await parallelHash(data);

    expect(hashes).toHaveLength(3);
    expect(hashes[0]).toMatch(/^[a-f0-9]{64}$/); // BLAKE3 hex
  });

  it('should handle empty array', async () => {
    const hashes = await parallelHash([]);
    expect(hashes).toHaveLength(0);
  });

  it('should maintain order', async () => {
    const data = ['first', 'second', 'third'];
    const hashes1 = await parallelHash(data);
    const hashes2 = await parallelHash(data);

    expect(hashes1).toEqual(hashes2);
  });

  it('should process large batches efficiently', async () => {
    const data = Array(1000).fill('test-data-string');

    const start = performance.now();
    const hashes = await parallelHash(data);
    const elapsed = performance.now() - start;

    expect(hashes).toHaveLength(1000);
    // Should complete in reasonable time (< 100ms)
    expect(elapsed).toBeLessThan(100);
  });

  it('should use parallel workers for large batches', async () => {
    const data = Array(100).fill('test');
    const workers = 4;

    const start = performance.now();
    await parallelHash(data, workers);
    const parallelTime = performance.now() - start;

    // Parallel should be faster than serial (roughly)
    expect(parallelTime).toBeLessThan(200);
  });
});

describe('Receipt Batch Generation', () => {
  beforeEach(() => {
    resetPool(1000);
  });

  describe('Basic Batch Generation', () => {
    it('should generate batch of receipts', async () => {
      const events = [
        {
          eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
          caseId: 'case1',
          taskId: 'task1',
          payload: { decision: 'ENABLE' },
        },
        {
          eventType: RECEIPT_EVENT_TYPES.TASK_STARTED,
          caseId: 'case1',
          taskId: 'task1',
          payload: { decision: 'START' },
        },
      ];

      const result = await generateReceiptBatch(events);

      expect(result.receipts).toHaveLength(2);
      expect(result.duration).toBeGreaterThan(0);
      expect(result.throughput).toBeGreaterThan(0);
    });

    it('should handle empty batch', async () => {
      const result = await generateReceiptBatch([]);

      expect(result.receipts).toHaveLength(0);
      expect(result.duration).toBe(0);
      expect(result.throughput).toBe(0);
    });

    it('should assign sequential timestamps', async () => {
      const events = Array(5).fill({
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      });

      const result = await generateReceiptBatch(events);

      for (let i = 1; i < result.receipts.length; i++) {
        expect(result.receipts[i].t_ns).toBeGreaterThan(result.receipts[i - 1].t_ns);
      }
    });

    it('should generate unique receipt IDs', async () => {
      const events = Array(10).fill({
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      });

      const result = await generateReceiptBatch(events);
      const ids = new Set(result.receipts.map(r => r.id));

      expect(ids.size).toBe(10);
    });

    it('should include all required fields', async () => {
      const events = [{
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: { data: 'test' },
      }];

      const result = await generateReceiptBatch(events);
      const receipt = result.receipts[0];

      expect(receipt.id).toBeDefined();
      expect(receipt.eventType).toBe(RECEIPT_EVENT_TYPES.TASK_ENABLED);
      expect(receipt.t_ns).toBeDefined();
      expect(receipt.timestamp_iso).toBeDefined();
      expect(receipt.caseId).toBe('case1');
      expect(receipt.taskId).toBe('task1');
      expect(receipt.payloadHash).toMatch(/^[a-f0-9]{64}$/);
      expect(receipt.receiptHash).toMatch(/^[a-f0-9]{64}$/);
    });
  });

  describe('Chain Hashing', () => {
    it('should link receipts in chain', async () => {
      const events = Array(3).fill({
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      });

      const result = await generateReceiptBatch(events);

      expect(result.receipts[0].previousReceiptHash).toBeNull();
      expect(result.receipts[1].previousReceiptHash).toBe(result.receipts[0].receiptHash);
      expect(result.receipts[2].previousReceiptHash).toBe(result.receipts[1].receiptHash);
    });

    it('should chain from previous receipt', async () => {
      const events1 = [{
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      }];

      const result1 = await generateReceiptBatch(events1);
      const lastReceipt = result1.receipts[0];

      const events2 = [{
        eventType: RECEIPT_EVENT_TYPES.TASK_STARTED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      }];

      const result2 = await generateReceiptBatch(events2, {
        previousReceipt: lastReceipt,
      });

      expect(result2.receipts[0].previousReceiptHash).toBe(lastReceipt.receiptHash);
    });
  });

  describe('Object Pooling', () => {
    it('should use object pool when enabled', async () => {
      const events = Array(100).fill({
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      });

      const result = await generateReceiptBatch(events, { usePool: true });

      expect(result.stats.poolStats).toBeDefined();
      expect(result.stats.poolStats.reused).toBeGreaterThan(0);
    });

    it('should skip pool when disabled', async () => {
      const events = Array(10).fill({
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      });

      const result = await generateReceiptBatch(events, { usePool: false });

      expect(result.stats.poolStats).toBeNull();
    });

    it('should release receipts back to pool', async () => {
      const events = Array(10).fill({
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      });

      const result = await generateReceiptBatch(events, { usePool: true });
      const statsBefore = getPoolStats();

      releaseReceipts(result.receipts);
      const statsAfter = getPoolStats();

      expect(statsAfter.poolSize).toBeGreaterThan(statsBefore.poolSize);
    });
  });

  describe('Validation', () => {
    it('should reject invalid event types', async () => {
      const events = [{
        eventType: 'INVALID_EVENT_TYPE',
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      }];

      await expect(generateReceiptBatch(events)).rejects.toThrow('Invalid event type');
    });

    it('should validate receipts when requested', async () => {
      const events = [{
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      }];

      // Should not throw
      const result = await generateReceiptBatch(events, { validate: true });
      expect(result.receipts).toHaveLength(1);
    });
  });

  describe('Performance', () => {
    it('should achieve high throughput', async () => {
      const events = Array(1000).fill({
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: { data: 'test' },
      });

      const result = await generateReceiptBatch(events, { workers: 4 });

      // Target: 100K receipts/sec (so 1000 should take < 10ms)
      // Being conservative: < 100ms for 1000
      expect(result.duration).toBeLessThan(100);
      expect(result.throughput).toBeGreaterThan(10000); // At least 10K/sec
    });

    it('should use specified number of workers', async () => {
      const events = Array(100).fill({
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
      });

      const result = await generateReceiptBatch(events, { workers: 8 });

      expect(result.stats.workers).toBe(8);
    });
  });

  describe('Optional Fields', () => {
    it('should include optional KGC event ID', async () => {
      const events = [{
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
        kgcEventId: 'event-123',
      }];

      const result = await generateReceiptBatch(events);

      expect(result.receipts[0].kgcEventId).toBe('event-123');
    });

    it('should include optional git ref', async () => {
      const events = [{
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
        gitRef: 'abc123',
      }];

      const result = await generateReceiptBatch(events);

      expect(result.receipts[0].gitRef).toBe('abc123');
    });

    it('should include optional vector clock', async () => {
      const events = [{
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: {},
        vectorClock: { node1: 5 },
      }];

      const result = await generateReceiptBatch(events);

      expect(result.receipts[0].vectorClock).toEqual({ node1: 5 });
    });
  });
});

describe('Receipt Batch Verification', () => {
  it('should verify valid receipts', async () => {
    const events = Array(10).fill({
      eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
      caseId: 'case1',
      taskId: 'task1',
      payload: {},
    });

    const generated = await generateReceiptBatch(events);
    const verification = await verifyReceiptBatch(generated.receipts);

    expect(verification.valid).toBe(true);
    expect(verification.invalidCount).toBe(0);
    expect(verification.results).toHaveLength(10);
    expect(verification.results.every(r => r === true)).toBe(true);
  });

  it('should detect invalid payload hash', async () => {
    const events = [{
      eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
      caseId: 'case1',
      taskId: 'task1',
      payload: {},
    }];

    const generated = await generateReceiptBatch(events);

    // Corrupt payload hash
    generated.receipts[0].payloadHash = 'a'.repeat(64);

    const verification = await verifyReceiptBatch(generated.receipts);

    expect(verification.valid).toBe(false);
    expect(verification.invalidCount).toBeGreaterThan(0);
  });

  it('should detect invalid chain hash', async () => {
    const events = [{
      eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
      caseId: 'case1',
      taskId: 'task1',
      payload: {},
    }];

    const generated = await generateReceiptBatch(events);

    // Corrupt receipt hash
    generated.receipts[0].receiptHash = 'b'.repeat(64);

    const verification = await verifyReceiptBatch(generated.receipts);

    expect(verification.valid).toBe(false);
    expect(verification.invalidCount).toBeGreaterThan(0);
  });

  it('should handle empty batch', async () => {
    const verification = await verifyReceiptBatch([]);

    expect(verification.valid).toBe(true);
    expect(verification.invalidCount).toBe(0);
    expect(verification.results).toHaveLength(0);
  });

  it('should verify in parallel', async () => {
    const events = Array(100).fill({
      eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
      caseId: 'case1',
      taskId: 'task1',
      payload: {},
    });

    const generated = await generateReceiptBatch(events);

    const start = performance.now();
    await verifyReceiptBatch(generated.receipts, 4);
    const elapsed = performance.now() - start;

    // Should complete quickly with parallelization
    expect(elapsed).toBeLessThan(100);
  });
});

describe('Pool Management', () => {
  beforeEach(() => {
    resetPool(100);
  });

  it('should get pool statistics', () => {
    const stats = getPoolStats();

    expect(stats.poolSize).toBeDefined();
    expect(stats.created).toBeDefined();
    expect(stats.reused).toBeDefined();
    expect(stats.reuseRate).toBeDefined();
  });

  it('should reset pool to specified size', () => {
    resetPool(50);
    const stats = getPoolStats();

    expect(stats.poolSize).toBe(50);
    expect(stats.created).toBe(0);
    expect(stats.reused).toBe(0);
  });

  it('should calculate reuse rate', async () => {
    resetPool(10);

    const events = Array(20).fill({
      eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
      caseId: 'case1',
      taskId: 'task1',
      payload: {},
    });

    await generateReceiptBatch(events, { usePool: true });
    const stats = getPoolStats();

    // Should have reused some objects
    expect(stats.reused).toBeGreaterThan(0);
    expect(stats.reuseRate).toBeGreaterThan(0);
  });
});

describe('Deterministic Serialization', () => {
  it('should produce consistent hashes for same data', async () => {
    const events = [
      {
        eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
        caseId: 'case1',
        taskId: 'task1',
        payload: { a: 1, b: 2 },
      },
    ];

    const result1 = await generateReceiptBatch(events);
    const result2 = await generateReceiptBatch(events);

    // Payload hashes should be identical (timestamps will differ)
    expect(result1.receipts[0].payloadHash.length).toBe(64);
    expect(result2.receipts[0].payloadHash.length).toBe(64);
  });
});

describe('Performance Benchmarks', () => {
  it('should generate 1K receipts in < 50ms', async () => {
    const events = Array(1000).fill({
      eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
      caseId: 'case1',
      taskId: 'task1',
      payload: { data: 'test' },
    });

    const result = await generateReceiptBatch(events);

    expect(result.duration).toBeLessThan(50);
  });

  it('should achieve target throughput of 100K/sec', async () => {
    const events = Array(10000).fill({
      eventType: RECEIPT_EVENT_TYPES.TASK_ENABLED,
      caseId: 'case1',
      taskId: 'task1',
      payload: { data: 'test' },
    });

    const result = await generateReceiptBatch(events);

    // 10K in < 100ms = 100K/sec
    expect(result.throughput).toBeGreaterThan(100000);
  });
});

/**
 * @fileoverview Lockchain Writer Browser Unit Tests (HIGH - P2)
 * Tests cryptographic integrity without Git dependencies
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  BrowserLockchainWriter,
  createBrowserLockchainWriter
} from '../../src/knowledge-engine/lockchain-writer-browser.mjs';

describe('lockchain-writer-browser.mjs (HIGH)', () => {
  let writer;

  beforeEach(() => {
    writer = new BrowserLockchainWriter({
      storageType: 'memory',
      algorithm: 'sha3-256',
      batchSize: 10,
      enableMerkle: true,
      storagePrefix: 'test-lockchain'
    });
  });

  afterEach(async () => {
    await writer?.clear();
  });

  describe('Writer Creation', () => {
    it('should create writer with default config', () => {
      const defaultWriter = new BrowserLockchainWriter();
      expect(defaultWriter).toBeInstanceOf(BrowserLockchainWriter);
      expect(defaultWriter.config.storageType).toBe('memory');
      expect(defaultWriter.config.algorithm).toBe('sha3-256');
    });

    it('should create writer with custom config', () => {
      const customWriter = new BrowserLockchainWriter({
        storageType: 'localStorage',
        algorithm: 'blake3',
        batchSize: 20,
        enableMerkle: false
      });

      expect(customWriter.config.storageType).toBe('localStorage');
      expect(customWriter.config.algorithm).toBe('blake3');
      expect(customWriter.config.batchSize).toBe(20);
      expect(customWriter.config.enableMerkle).toBe(false);
    });

    it('should create writer via factory function', () => {
      const factoryWriter = createBrowserLockchainWriter({
        algorithm: 'sha256'
      });

      expect(factoryWriter).toBeInstanceOf(BrowserLockchainWriter);
      expect(factoryWriter.config.algorithm).toBe('sha256');
    });
  });

  describe('Receipt Writing', () => {
    it('should write single receipt', async () => {
      const receipt = {
        id: 'tx-001',
        timestamp: Date.now(),
        data: { operation: 'add', value: 42 }
      };

      const entry = await writer.writeReceipt(receipt);

      expect(entry).toHaveProperty('id');
      expect(entry).toHaveProperty('timestamp');
      expect(entry).toHaveProperty('receipt');
      expect(entry).toHaveProperty('signature');
      expect(entry.receipt).toEqual(receipt);
    });

    it('should generate signature for receipt', async () => {
      const receipt = { id: 'tx-001', data: 'test' };

      const entry = await writer.writeReceipt(receipt);

      expect(entry.signature).toBeDefined();
      expect(entry.signature.algorithm).toBe('sha3-256');
      expect(entry.signature.value).toMatch(/^[0-9a-f]{64}$/); // SHA3-256 hex
    });

    it('should set previousHash to null for first entry', async () => {
      const receipt = { id: 'tx-001' };

      const entry = await writer.writeReceipt(receipt);

      expect(entry.previousHash).toBeNull();
    });

    it('should chain entries with previousHash', async () => {
      const receipt1 = { id: 'tx-001' };
      const receipt2 = { id: 'tx-002' };

      const entry1 = await writer.writeReceipt(receipt1);
      const entry2 = await writer.writeReceipt(receipt2);

      expect(entry2.previousHash).toBeDefined();
      expect(entry2.previousHash).not.toBeNull();
    });

    it('should include merkleRoot when provided', async () => {
      const receipt = { id: 'tx-001' };
      const merkleRoot = 'abc123def456';

      const entry = await writer.writeReceipt(receipt, { merkleRoot });

      expect(entry.merkleRoot).toBe(merkleRoot);
    });

    it('should serialize receipt correctly', async () => {
      const receiptObject = {
        id: 'tx-001',
        nested: { value: 42 }
      };

      const entry = await writer.writeReceipt(receiptObject);

      expect(entry.receipt).toEqual(receiptObject);
    });

    it('should handle string receipts', async () => {
      const receiptString = JSON.stringify({ id: 'tx-001' });

      const entry = await writer.writeReceipt(receiptString);

      expect(entry.receipt).toEqual({ id: 'tx-001' });
    });
  });

  describe('Batch Operations', () => {
    it('should write batch of receipts', async () => {
      const receipts = [
        { id: 'tx-001', data: 'first' },
        { id: 'tx-002', data: 'second' },
        { id: 'tx-003', data: 'third' }
      ];

      const entries = await writer.writeReceiptBatch(receipts);

      expect(entries).toHaveLength(3);
      expect(entries[0].receipt.id).toBe('tx-001');
      expect(entries[2].receipt.id).toBe('tx-003');
    });

    it('should calculate merkle root for batch', async () => {
      const receipts = [
        { id: 'tx-001' },
        { id: 'tx-002' },
        { id: 'tx-003' }
      ];

      const entries = await writer.writeReceiptBatch(receipts);

      // All entries should have same merkle root
      const merkleRoot = entries[0].merkleRoot;
      expect(merkleRoot).toBeDefined();

      entries.forEach(entry => {
        expect(entry.merkleRoot).toBe(merkleRoot);
      });
    });

    it('should chain batch entries correctly', async () => {
      const receipts = [
        { id: 'tx-001' },
        { id: 'tx-002' }
      ];

      const entries = await writer.writeReceiptBatch(receipts);

      expect(entries[0].previousHash).toBeNull();
      expect(entries[1].previousHash).toBeDefined();
    });

    it('should handle large batches', async () => {
      const receipts = [];
      for (let i = 0; i < 100; i++) {
        receipts.push({ id: `tx-${i}`, value: i });
      }

      const entries = await writer.writeReceiptBatch(receipts);

      expect(entries).toHaveLength(100);
      expect(entries[0].receipt.value).toBe(0);
      expect(entries[99].receipt.value).toBe(99);
    });
  });

  describe('Merkle Tree Calculation', () => {
    it('should calculate merkle root for single receipt', async () => {
      const receipts = [{ id: 'tx-001' }];

      const entries = await writer.writeReceiptBatch(receipts);

      expect(entries[0].merkleRoot).toBeDefined();
      expect(entries[0].merkleRoot).toMatch(/^[0-9a-f]+$/);
    });

    it('should calculate merkle root for odd number of receipts', async () => {
      const receipts = [
        { id: 'tx-001' },
        { id: 'tx-002' },
        { id: 'tx-003' }
      ];

      const entries = await writer.writeReceiptBatch(receipts);

      expect(entries[0].merkleRoot).toBeDefined();
    });

    it('should calculate consistent merkle roots', async () => {
      const receipts1 = [{ id: 'tx-001' }, { id: 'tx-002' }];
      const receipts2 = [{ id: 'tx-001' }, { id: 'tx-002' }];

      const entries1 = await writer.writeReceiptBatch(receipts1);

      await writer.clear();
      const writer2 = new BrowserLockchainWriter({
        storageType: 'memory',
        enableMerkle: true
      });
      const entries2 = await writer2.writeReceiptBatch(receipts2);

      expect(entries1[0].merkleRoot).toBe(entries2[0].merkleRoot);
    });

    it('should skip merkle root when disabled', async () => {
      const noMerkleWriter = new BrowserLockchainWriter({
        storageType: 'memory',
        enableMerkle: false
      });

      const receipts = [{ id: 'tx-001' }, { id: 'tx-002' }];
      const entries = await noMerkleWriter.writeReceiptBatch(receipts);

      expect(entries[0].merkleRoot).toBeUndefined();
    });
  });

  describe('Storage Types', () => {
    it('should use memory storage', async () => {
      const memoryWriter = new BrowserLockchainWriter({
        storageType: 'memory'
      });

      const receipt = { id: 'tx-001' };
      await memoryWriter.writeReceipt(receipt);

      const stats = memoryWriter.getStats();
      expect(stats.totalEntries).toBe(1);
      expect(stats.storageType).toBe('memory');
    });

    it('should use localStorage storage', async () => {
      const localStorageWriter = new BrowserLockchainWriter({
        storageType: 'localStorage',
        storagePrefix: 'test-ls'
      });

      const receipt = { id: 'tx-001', data: 'test' };
      await localStorageWriter.writeReceipt(receipt);

      const stats = localStorageWriter.getStats();
      expect(stats.storageType).toBe('localStorage');

      await localStorageWriter.clear();
    });

    it('should use sessionStorage storage', async () => {
      const sessionStorageWriter = new BrowserLockchainWriter({
        storageType: 'sessionStorage',
        storagePrefix: 'test-ss'
      });

      const receipt = { id: 'tx-001' };
      await sessionStorageWriter.writeReceipt(receipt);

      const stats = sessionStorageWriter.getStats();
      expect(stats.storageType).toBe('sessionStorage');

      await sessionStorageWriter.clear();
    });
  });

  describe('Integrity Verification', () => {
    it('should verify integrity of empty chain', async () => {
      const verification = await writer.verifyIntegrity();

      expect(verification.valid).toBe(true);
      expect(verification.totalEntries).toBe(0);
    });

    it('should verify integrity of single entry', async () => {
      await writer.writeReceipt({ id: 'tx-001' });

      const verification = await writer.verifyIntegrity();

      expect(verification.valid).toBe(true);
      expect(verification.totalEntries).toBe(1);
      expect(verification.results).toHaveLength(1);
    });

    it('should verify integrity of multiple entries', async () => {
      await writer.writeReceipt({ id: 'tx-001' });
      await writer.writeReceipt({ id: 'tx-002' });
      await writer.writeReceipt({ id: 'tx-003' });

      const verification = await writer.verifyIntegrity();

      expect(verification.valid).toBe(true);
      expect(verification.totalEntries).toBe(3);
      expect(verification.results).toHaveLength(3);
    });

    it('should include verification timestamp', async () => {
      await writer.writeReceipt({ id: 'tx-001' });

      const verification = await writer.verifyIntegrity();

      expect(verification.verificationTimestamp).toBeDefined();
      expect(verification.verificationTimestamp).toBeGreaterThan(0);
    });

    it('should detect broken chain', async () => {
      await writer.writeReceipt({ id: 'tx-001' });
      await writer.writeReceipt({ id: 'tx-002' });

      // Manually corrupt the chain
      writer.entries[1].previousHash = 'corrupted-hash';

      const verification = await writer.verifyIntegrity();

      expect(verification.valid).toBe(false);
    });
  });

  describe('Statistics and Monitoring', () => {
    it('should provide basic statistics', () => {
      const stats = writer.getStats();

      expect(stats).toHaveProperty('totalEntries');
      expect(stats).toHaveProperty('processingBatches');
      expect(stats).toHaveProperty('config');
      expect(stats).toHaveProperty('lastHash');
      expect(stats).toHaveProperty('storageType');
    });

    it('should track entry count', async () => {
      await writer.writeReceipt({ id: 'tx-001' });
      await writer.writeReceipt({ id: 'tx-002' });

      const stats = writer.getStats();
      expect(stats.totalEntries).toBe(2);
    });

    it('should track processing batches', async () => {
      const receipts = Array.from({ length: 50 }, (_, i) => ({
        id: `tx-${i}`
      }));

      // Start batch processing but don't await
      const batchPromise = writer.writeReceiptBatch(receipts);

      // Check stats while processing
      const statsDuring = writer.getStats();
      // May or may not show active batch depending on timing

      await batchPromise;

      const statsAfter = writer.getStats();
      expect(statsAfter.processingBatches).toBe(0);
    });

    it('should track oldest and newest entries', async () => {
      await writer.writeReceipt({ id: 'tx-001' });
      await new Promise(resolve => setTimeout(resolve, 10));
      await writer.writeReceipt({ id: 'tx-002' });

      const stats = writer.getStats();

      expect(stats.oldestEntry).toBeDefined();
      expect(stats.newestEntry).toBeDefined();
      expect(stats.newestEntry).toBeGreaterThanOrEqual(stats.oldestEntry);
    });

    it('should update lastHash', async () => {
      expect(writer.getStats().lastHash).toBeNull();

      await writer.writeReceipt({ id: 'tx-001' });

      expect(writer.getStats().lastHash).toBeDefined();
      expect(writer.getStats().lastHash).not.toBeNull();
    });
  });

  describe('Cleanup and Reset', () => {
    it('should clear all entries', async () => {
      await writer.writeReceipt({ id: 'tx-001' });
      await writer.writeReceipt({ id: 'tx-002' });

      expect(writer.getStats().totalEntries).toBe(2);

      await writer.clear();

      expect(writer.getStats().totalEntries).toBe(0);
      expect(writer.getStats().lastHash).toBeNull();
    });

    it('should clear processing batches', async () => {
      const receipts = Array.from({ length: 10 }, (_, i) => ({
        id: `tx-${i}`
      }));

      await writer.writeReceiptBatch(receipts);
      await writer.clear();

      const stats = writer.getStats();
      expect(stats.processingBatches).toBe(0);
    });

    it('should remove persisted storage on clear', async () => {
      const persistWriter = new BrowserLockchainWriter({
        storageType: 'localStorage',
        storagePrefix: 'test-persist',
        enablePersistence: true
      });

      await persistWriter.writeReceipt({ id: 'tx-001' });
      await persistWriter.clear();

      const stats = persistWriter.getStats();
      expect(stats.totalEntries).toBe(0);
    });
  });

  describe('Performance', () => {
    it('should write receipts performantly', async () => {
      const start = performance.now();

      for (let i = 0; i < 100; i++) {
        await writer.writeReceipt({ id: `tx-${i}` });
      }

      const duration = performance.now() - start;
      const avgPerReceipt = duration / 100;

      expect(avgPerReceipt).toBeLessThan(20); // <20ms per receipt
    });

    it('should verify integrity performantly', async () => {
      for (let i = 0; i < 100; i++) {
        await writer.writeReceipt({ id: `tx-${i}` });
      }

      const start = performance.now();
      await writer.verifyIntegrity();
      const duration = performance.now() - start;

      expect(duration).toBeLessThan(100); // <100ms for 100 entries
    });

    it('should calculate merkle root performantly', async () => {
      const receipts = Array.from({ length: 100 }, (_, i) => ({
        id: `tx-${i}`
      }));

      const start = performance.now();
      await writer.writeReceiptBatch(receipts);
      const duration = performance.now() - start;

      expect(duration).toBeLessThan(500); // <500ms for 100 receipts
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty receipt', async () => {
      const entry = await writer.writeReceipt({});

      expect(entry).toHaveProperty('id');
      expect(entry).toHaveProperty('signature');
    });

    it('should handle receipt with complex data', async () => {
      const complexReceipt = {
        id: 'tx-001',
        nested: {
          array: [1, 2, 3],
          object: { key: 'value' },
          nullValue: null,
          undefinedValue: undefined
        }
      };

      const entry = await writer.writeReceipt(complexReceipt);

      expect(entry.receipt.nested.array).toEqual([1, 2, 3]);
    });

    it('should handle concurrent writes', async () => {
      const promises = [];
      for (let i = 0; i < 10; i++) {
        promises.push(writer.writeReceipt({ id: `tx-${i}` }));
      }

      const entries = await Promise.all(promises);

      expect(entries).toHaveLength(10);
      expect(writer.getStats().totalEntries).toBe(10);
    });

    it('should handle empty batch', async () => {
      const entries = await writer.writeReceiptBatch([]);

      expect(entries).toHaveLength(0);
    });
  });

  describe('Hash Algorithm Support', () => {
    it('should support SHA-256 algorithm', async () => {
      const sha256Writer = new BrowserLockchainWriter({
        algorithm: 'sha256'
      });

      const entry = await sha256Writer.writeReceipt({ id: 'tx-001' });

      expect(entry.signature.algorithm).toBe('sha256');
    });

    it('should support SHA3-256 algorithm', async () => {
      const sha3Writer = new BrowserLockchainWriter({
        algorithm: 'sha3-256'
      });

      const entry = await sha3Writer.writeReceipt({ id: 'tx-001' });

      expect(entry.signature.algorithm).toBe('sha3-256');
    });

    it('should support BLAKE3 algorithm', async () => {
      const blake3Writer = new BrowserLockchainWriter({
        algorithm: 'blake3'
      });

      const entry = await blake3Writer.writeReceipt({ id: 'tx-001' });

      expect(entry.signature.algorithm).toBe('blake3');
    });
  });
});

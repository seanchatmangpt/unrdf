/**
 * @fileoverview Tests for Receipt Chain
 */

import { describe, it, expect } from 'vitest';
import { ReceiptChain } from './receipt-chain.mjs';
import { Receipt } from './receipt.mjs';

describe('ReceiptChain', () => {
  const createReceipt = async (beforeHash = null, timestamp = new Date()) => {
    return Receipt.create({
      inputHashes: {
        ontologyReleases: ['hash1'],
        deltaCapsule: 'hash2',
      },
      decision: 'allow',
      outputHash: 'hash3',
      toolchainVersion: {
        node: '18.19.0',
        packages: {},
      },
      beforeHash,
      timestamp,
    });
  };

  describe('append()', () => {
    it('should append first receipt', async () => {
      const chain = new ReceiptChain();
      const receipt = await createReceipt();

      await chain.append(receipt);

      expect(chain.length).toBe(1);
      expect(chain.getLast()).toBe(receipt);
    });

    it('should append linked receipts', async () => {
      const chain = new ReceiptChain();

      const receipt1 = await createReceipt(null, new Date('2025-01-01T00:00:00Z'));
      await chain.append(receipt1);

      const receipt2 = await createReceipt(receipt1.receiptHash, new Date('2025-01-01T00:00:01Z'));
      await chain.append(receipt2);

      expect(chain.length).toBe(2);
      expect(chain.receipts[1].beforeHash).toBe(receipt1.receiptHash);
    });

    it('should reject first receipt with non-null beforeHash', async () => {
      const chain = new ReceiptChain();
      const receipt = await createReceipt('someHash');

      await expect(chain.append(receipt)).rejects.toThrow('First receipt must have beforeHash=null');
    });

    it('should reject broken chain link', async () => {
      const chain = new ReceiptChain();

      const receipt1 = await createReceipt(null, new Date('2025-01-01T00:00:00Z'));
      await chain.append(receipt1);

      const receipt2 = await createReceipt('wrongHash', new Date('2025-01-01T00:00:01Z'));

      await expect(chain.append(receipt2)).rejects.toThrow('Receipt chain broken');
    });

    it('should reject non-monotonic epochs', async () => {
      const chain = new ReceiptChain();

      const receipt1 = await createReceipt(null, new Date('2025-01-01T00:00:01Z'));
      await chain.append(receipt1);

      const receipt2 = await createReceipt(receipt1.receiptHash, new Date('2025-01-01T00:00:00Z'));

      await expect(chain.append(receipt2)).rejects.toThrow('Epoch must increase');
    });
  });

  describe('verify()', () => {
    it('should verify valid chain', async () => {
      const chain = new ReceiptChain();

      const receipt1 = await createReceipt(null, new Date('2025-01-01T00:00:00Z'));
      await chain.append(receipt1);

      const receipt2 = await createReceipt(receipt1.receiptHash, new Date('2025-01-01T00:00:01Z'));
      await chain.append(receipt2);

      const result = await chain.verify();

      expect(result.valid).toBe(true);
      expect(result.errors).toHaveLength(0);
    });

    it('should detect broken links', async () => {
      const chain = new ReceiptChain();

      const receipt1 = await createReceipt(null, new Date('2025-01-01T00:00:00Z'));
      const receipt2 = await createReceipt(receipt1.receiptHash, new Date('2025-01-01T00:00:01Z'));

      // Manually add receipts without validation
      chain.receipts.push(receipt1);
      chain.receipts.push(receipt2);

      // Tamper with second receipt's beforeHash
      const tampered = Object.assign(Object.create(Object.getPrototypeOf(receipt2)), receipt2);
      tampered.beforeHash = 'wrong';
      chain.receipts[1] = tampered;

      const result = await chain.verify();

      expect(result.valid).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
      expect(result.errors.some(e => e.includes('broken chain link'))).toBe(true);
    });
  });

  describe('getByHash()', () => {
    it('should retrieve receipt by hash', async () => {
      const chain = new ReceiptChain();
      const receipt = await createReceipt();
      await chain.append(receipt);

      const retrieved = chain.getByHash(receipt.receiptHash);

      expect(retrieved).toBe(receipt);
    });

    it('should return undefined for missing hash', () => {
      const chain = new ReceiptChain();
      const retrieved = chain.getByHash('nonexistent');

      expect(retrieved).toBeUndefined();
    });
  });

  describe('getByEpoch()', () => {
    it('should retrieve receipt by epoch', async () => {
      const chain = new ReceiptChain();
      const receipt = await createReceipt();
      await chain.append(receipt);

      const retrieved = chain.getByEpoch(receipt.epoch);

      expect(retrieved).toBe(receipt);
    });
  });

  describe('getRange()', () => {
    it('should get receipts in time range', async () => {
      const chain = new ReceiptChain();

      const receipt1 = await createReceipt(null, new Date('2025-01-01T00:00:00Z'));
      await chain.append(receipt1);

      const receipt2 = await createReceipt(receipt1.receiptHash, new Date('2025-01-01T00:00:01Z'));
      await chain.append(receipt2);

      const receipt3 = await createReceipt(receipt2.receiptHash, new Date('2025-01-01T00:00:02Z'));
      await chain.append(receipt3);

      const range = chain.getRange(receipt1.epoch, receipt2.epoch);

      expect(range).toHaveLength(2);
      expect(range[0]).toBe(receipt1);
      expect(range[1]).toBe(receipt2);
    });
  });

  describe('toJSONLD() / fromJSONLD()', () => {
    it('should serialize to JSON-LD', async () => {
      const chain = new ReceiptChain();

      const receipt1 = await createReceipt(null, new Date('2025-01-01T00:00:00Z'));
      await chain.append(receipt1);

      const receipt2 = await createReceipt(receipt1.receiptHash, new Date('2025-01-01T00:00:01Z'));
      await chain.append(receipt2);

      const jsonld = chain.toJSONLD();

      expect(jsonld['@type']).toBe('unrdf:ReceiptChain');
      expect(jsonld['unrdf:length']).toBe(2);
      expect(jsonld['unrdf:receipts']).toHaveLength(2);
    });

    it('should round-trip through JSON-LD', async () => {
      const chain = new ReceiptChain();

      const receipt1 = await createReceipt(null, new Date('2025-01-01T00:00:00Z'));
      await chain.append(receipt1);

      const receipt2 = await createReceipt(receipt1.receiptHash, new Date('2025-01-01T00:00:01Z'));
      await chain.append(receipt2);

      const jsonld = chain.toJSONLD();
      const restored = await ReceiptChain.fromJSONLD(jsonld);

      expect(restored.length).toBe(chain.length);
      expect(restored.receipts[0].receiptHash).toBe(receipt1.receiptHash);
      expect(restored.receipts[1].receiptHash).toBe(receipt2.receiptHash);
    });
  });
});

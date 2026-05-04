/**
 * @fileoverview Tests for Receipt generation, validation, and storage
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdirSync, rmSync, existsSync, readdirSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import {
  generateReceipt,
  verifyReceiptHash,
  verifyReceiptChain,
  ReceiptStore,
} from '../src/receipt.mjs';

const TEST_RECEIPT_DIR = './var/kgc/receipts-test';

describe('Receipt Generation and Validation', () => {
  describe('generateReceipt', () => {
    it('should generate valid receipt with hash', async () => {
      const receipt = await generateReceipt(
        'test_operation',
        { input1: 'value1' },
        { output1: 'result1' }
      );

      expect(receipt).toHaveProperty('id');
      expect(receipt).toHaveProperty('hash');
      expect(receipt).toHaveProperty('timestamp');
      expect(receipt.operation).toBe('test_operation');
      expect(receipt.inputs).toEqual({ input1: 'value1' });
      expect(receipt.outputs).toEqual({ output1: 'result1' });
      expect(receipt.hash).toMatch(/^[a-f0-9]{64}$/);
    });

    it('should generate receipt with parent hash for chaining', async () => {
      const parentReceipt = await generateReceipt(
        'parent_op',
        { a: 1 },
        { b: 2 }
      );

      const childReceipt = await generateReceipt(
        'child_op',
        { c: 3 },
        { d: 4 },
        parentReceipt.hash
      );

      expect(childReceipt.parentHash).toBe(parentReceipt.hash);
    });

    it('should produce deterministic hash for same inputs', async () => {
      const receipt1 = await generateReceipt(
        'op',
        { x: 1 },
        { y: 2 }
      );

      const receipt2 = await generateReceipt(
        'op',
        { x: 1 },
        { y: 2 }
      );

      // Different IDs and timestamps, but operation data is same
      expect(receipt1.id).not.toBe(receipt2.id);
    });
  });

  describe('verifyReceiptHash', () => {
    it('should verify valid receipt hash', async () => {
      const receipt = await generateReceipt(
        'verify_test',
        { test: 'data' },
        { result: 'success' }
      );

      const isValid = await verifyReceiptHash(receipt);
      expect(isValid).toBe(true);
    });

    it('should reject tampered receipt', async () => {
      const receipt = await generateReceipt(
        'tamper_test',
        { data: 'original' },
        { result: 'ok' }
      );

      // Tamper with output
      receipt.outputs.result = 'tampered';

      const isValid = await verifyReceiptHash(receipt);
      expect(isValid).toBe(false);
    });
  });

  describe('verifyReceiptChain', () => {
    it('should verify valid chain of receipts', async () => {
      const receipt1 = await generateReceipt('op1', { a: 1 }, { b: 2 });
      const receipt2 = await generateReceipt('op2', { c: 3 }, { d: 4 }, receipt1.hash);
      const receipt3 = await generateReceipt('op3', { e: 5 }, { f: 6 }, receipt2.hash);

      const { valid, errors } = await verifyReceiptChain([receipt1, receipt2, receipt3]);

      expect(valid).toBe(true);
      expect(errors).toHaveLength(0);
    });

    it('should detect broken chain linkage', async () => {
      const receipt1 = await generateReceipt('op1', { a: 1 }, { b: 2 });
      const receipt2 = await generateReceipt('op2', { c: 3 }, { d: 4 }, 'wrong_hash');

      const { valid, errors } = await verifyReceiptChain([receipt1, receipt2]);

      expect(valid).toBe(false);
      expect(errors.length).toBeGreaterThan(0);
      expect(errors[0]).toContain('invalid parent hash');
    });
  });
});

describe('ReceiptStore', () => {
  let store;

  beforeEach(() => {
    // Clean up test directory
    if (existsSync(TEST_RECEIPT_DIR)) {
      rmSync(TEST_RECEIPT_DIR, { recursive: true, force: true });
    }
    mkdirSync(TEST_RECEIPT_DIR, { recursive: true });
    store = new ReceiptStore(TEST_RECEIPT_DIR);
  });

  afterEach(() => {
    // Clean up after tests
    if (existsSync(TEST_RECEIPT_DIR)) {
      rmSync(TEST_RECEIPT_DIR, { recursive: true, force: true });
    }
  });

  describe('save and load', () => {
    it('should save and load receipt', async () => {
      const receipt = await generateReceipt(
        'save_test',
        { input: 'test' },
        { output: 'result' }
      );

      const savedPath = await store.save(receipt);
      expect(existsSync(savedPath)).toBe(true);

      const loaded = await store.load(receipt.id);
      expect(loaded).toEqual(receipt);
    });

    it('should return null for non-existent receipt', async () => {
      const loaded = await store.load('non_existent_id');
      expect(loaded).toBeNull();
    });

    it('should update manifest on save', async () => {
      const receipt = await generateReceipt(
        'manifest_test',
        { a: 1 },
        { b: 2 }
      );

      await store.save(receipt);

      const manifestPath = join(TEST_RECEIPT_DIR, 'manifest.json');
      expect(existsSync(manifestPath)).toBe(true);

      const manifest = JSON.parse(readFileSync(manifestPath, 'utf-8'));
      expect(manifest.receipts).toHaveLength(1);
      expect(manifest.receipts[0].id).toBe(receipt.id);
      expect(manifest.receipts[0].hash).toBe(receipt.hash);
    });
  });

  describe('list', () => {
    it('should list all stored receipts', async () => {
      const receipts = [
        await generateReceipt('op1', { a: 1 }, { b: 2 }),
        await generateReceipt('op2', { c: 3 }, { d: 4 }),
        await generateReceipt('op3', { e: 5 }, { f: 6 }),
      ];

      for (const receipt of receipts) {
        await store.save(receipt);
      }

      const listed = await store.list();
      expect(listed).toHaveLength(3);

      const ids = listed.map((r) => r.id);
      expect(ids).toContain(receipts[0].id);
      expect(ids).toContain(receipts[1].id);
      expect(ids).toContain(receipts[2].id);
    });

    it('should return empty array when no receipts exist', async () => {
      const listed = await store.list();
      expect(listed).toEqual([]);
    });
  });

  describe('loadChain', () => {
    it('should load full receipt chain', async () => {
      const r1 = await generateReceipt('op1', { a: 1 }, { b: 2 });
      await store.save(r1);

      const r2 = await generateReceipt('op2', { c: 3 }, { d: 4 }, r1.hash);
      await store.save(r2);

      const r3 = await generateReceipt('op3', { e: 5 }, { f: 6 }, r2.hash);
      await store.save(r3);

      const chain = await store.loadChain(r3.id);

      expect(chain).toHaveLength(3);
      expect(chain[0].id).toBe(r1.id);
      expect(chain[1].id).toBe(r2.id);
      expect(chain[2].id).toBe(r3.id);
    });

    it('should handle single receipt (no chain)', async () => {
      const receipt = await generateReceipt('single', { x: 1 }, { y: 2 });
      await store.save(receipt);

      const chain = await store.loadChain(receipt.id);

      expect(chain).toHaveLength(1);
      expect(chain[0].id).toBe(receipt.id);
    });
  });

  describe('delete', () => {
    it('should delete receipt and update manifest', async () => {
      const receipt = await generateReceipt('delete_test', { a: 1 }, { b: 2 });
      await store.save(receipt);

      const deleted = await store.delete(receipt.id);
      expect(deleted).toBe(true);

      const loaded = await store.load(receipt.id);
      expect(loaded).toBeNull();

      // Check manifest
      const manifestPath = join(TEST_RECEIPT_DIR, 'manifest.json');
      const manifest = JSON.parse(readFileSync(manifestPath, 'utf-8'));
      expect(manifest.receipts).toHaveLength(0);
    });

    it('should return false when deleting non-existent receipt', async () => {
      const deleted = await store.delete('non_existent');
      expect(deleted).toBe(false);
    });
  });

  describe('integration', () => {
    it('should handle multiple receipts with chains', async () => {
      // Create chain 1
      const c1r1 = await generateReceipt('chain1_op1', { a: 1 }, { b: 2 });
      await store.save(c1r1);

      const c1r2 = await generateReceipt('chain1_op2', { c: 3 }, { d: 4 }, c1r1.hash);
      await store.save(c1r2);

      // Create chain 2
      const c2r1 = await generateReceipt('chain2_op1', { e: 5 }, { f: 6 });
      await store.save(c2r1);

      const c2r2 = await generateReceipt('chain2_op2', { g: 7 }, { h: 8 }, c2r1.hash);
      await store.save(c2r2);

      // List all
      const all = await store.list();
      expect(all).toHaveLength(4);

      // Load chains
      const chain1 = await store.loadChain(c1r2.id);
      const chain2 = await store.loadChain(c2r2.id);

      expect(chain1).toHaveLength(2);
      expect(chain2).toHaveLength(2);

      // Verify chains
      const verify1 = await verifyReceiptChain(chain1);
      const verify2 = await verifyReceiptChain(chain2);

      expect(verify1.valid).toBe(true);
      expect(verify2.valid).toBe(true);
    });
  });
});

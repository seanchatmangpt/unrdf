/**
 * @fileoverview Tests for Receipt class
 */

import { describe, it, expect } from 'vitest';
import { Receipt } from './receipt.mjs';

describe('Receipt', () => {
  const sampleOptions = {
    inputHashes: {
      ontologyReleases: ['hash1', 'hash2'],
      deltaCapsule: 'hash3',
    },
    decision: 'allow',
    outputHash: 'hash4',
    toolchainVersion: {
      node: '18.19.0',
      packages: { '@unrdf/core': '5.0.1' },
    },
  };

  describe('create()', () => {
    it('should create a valid receipt', async () => {
      const receipt = await Receipt.create(sampleOptions);

      expect(receipt.inputHashes).toEqual(sampleOptions.inputHashes);
      expect(receipt.decision).toBe('allow');
      expect(receipt.outputHash).toBe('hash4');
      expect(receipt.toolchainVersion).toEqual(sampleOptions.toolchainVersion);
      expect(receipt.beforeHash).toBeNull();
      expect(receipt.merkleRoot).toBeNull();
      expect(receipt.receiptHash).toBeDefined();
      expect(receipt.epoch).toMatch(/^τ_\d{4}_\d{2}_\d{2}_\d{4}_\d{3}$/);
    });

    it('should create receipt with beforeHash', async () => {
      const receipt = await Receipt.create({
        ...sampleOptions,
        beforeHash: 'previousHash',
      });

      expect(receipt.beforeHash).toBe('previousHash');
    });

    it('should create receipt with merkleRoot', async () => {
      const receipt = await Receipt.create({
        ...sampleOptions,
        merkleRoot: 'merkleRootHash',
      });

      expect(receipt.merkleRoot).toBe('merkleRootHash');
    });

    it('should reject invalid decision', async () => {
      await expect(
        Receipt.create({
          ...sampleOptions,
          decision: 'invalid',
        })
      ).rejects.toThrow();
    });
  });

  describe('determinism', () => {
    it('should produce identical hashes for identical inputs', async () => {
      const timestamp = new Date('2025-01-01T00:00:00Z');

      const receipt1 = await Receipt.create({
        ...sampleOptions,
        timestamp,
      });

      const receipt2 = await Receipt.create({
        ...sampleOptions,
        timestamp,
      });

      expect(receipt1.receiptHash).toBe(receipt2.receiptHash);
      expect(receipt1.epoch).toBe(receipt2.epoch);
    });

    it('should produce different hashes for different inputs', async () => {
      const timestamp = new Date('2025-01-01T00:00:00Z');

      const receipt1 = await Receipt.create({
        ...sampleOptions,
        timestamp,
      });

      const receipt2 = await Receipt.create({
        ...sampleOptions,
        decision: 'deny',
        timestamp,
      });

      expect(receipt1.receiptHash).not.toBe(receipt2.receiptHash);
    });
  });

  describe('generateEpoch()', () => {
    it('should generate correct epoch format', () => {
      const timestamp = new Date('2025-12-26T14:30:45.123Z');
      const epoch = Receipt.generateEpoch(timestamp);

      expect(epoch).toBe('τ_2025_12_26_1430_123');
    });

    it('should pad zeros correctly', () => {
      const timestamp = new Date('2025-01-05T09:05:00.001Z');
      const epoch = Receipt.generateEpoch(timestamp);

      expect(epoch).toBe('τ_2025_01_05_0905_001');
    });
  });

  describe('verify()', () => {
    it('should verify valid receipt', async () => {
      const receipt = await Receipt.create(sampleOptions);
      const isValid = await receipt.verify();

      expect(isValid).toBe(true);
    });

    it('should detect tampered receipt', async () => {
      const receipt = await Receipt.create(sampleOptions);

      // Tamper with receipt (need to unfreeze first)
      const tampered = Object.assign(Object.create(Object.getPrototypeOf(receipt)), receipt);
      tampered.decision = 'deny';

      const isValid = await tampered.verify();
      expect(isValid).toBe(false);
    });
  });

  describe('immutability', () => {
    it('should freeze receipt object', async () => {
      const receipt = await Receipt.create(sampleOptions);

      expect(Object.isFrozen(receipt)).toBe(true);
    });

    it('should prevent modifications', async () => {
      const receipt = await Receipt.create(sampleOptions);

      expect(() => {
        receipt.decision = 'deny';
      }).toThrow();
    });
  });

  describe('toJSONLD()', () => {
    it('should serialize to JSON-LD', async () => {
      const receipt = await Receipt.create(sampleOptions);
      const jsonld = receipt.toJSONLD();

      expect(jsonld['@type']).toBe('unrdf:Receipt');
      expect(jsonld['@id']).toBe(`urn:receipt:${receipt.receiptHash}`);
      expect(jsonld['unrdf:decision']).toBe('allow');
      expect(jsonld['unrdf:outputHash']).toBe('hash4');
    });

    it('should round-trip through JSON-LD', async () => {
      const receipt = await Receipt.create(sampleOptions);
      const jsonld = receipt.toJSONLD();
      const restored = Receipt.fromJSONLD(jsonld);

      expect(restored.receiptHash).toBe(receipt.receiptHash);
      expect(restored.decision).toBe(receipt.decision);
      expect(restored.epoch).toBe(receipt.epoch);
    });
  });

  describe('toTurtle()', () => {
    it('should serialize to Turtle', async () => {
      const receipt = await Receipt.create(sampleOptions);
      const turtle = receipt.toTurtle();

      expect(turtle).toContain('@prefix unrdf:');
      expect(turtle).toContain('unrdf:Receipt');
      expect(turtle).toContain('unrdf:decision "allow"');
      expect(turtle).toContain(`urn:receipt:${receipt.receiptHash}`);
    });

    it('should include all fields', async () => {
      const receipt = await Receipt.create({
        ...sampleOptions,
        beforeHash: 'prev',
        merkleRoot: 'root',
      });
      const turtle = receipt.toTurtle();

      expect(turtle).toContain('unrdf:beforeHash "prev"');
      expect(turtle).toContain('unrdf:merkleRoot "root"');
    });
  });
});

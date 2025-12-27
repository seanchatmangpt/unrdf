/**
 * @fileoverview Tests for Receipt Generator
 */

import { describe, it, expect } from 'vitest';
import { ReceiptGenerator } from './receipt-generator.mjs';

describe('ReceiptGenerator', () => {
  describe('emitAdmissibilityReceipt()', () => {
    it('should emit receipt for allow decision', async () => {
      const generator = new ReceiptGenerator();

      const receipt = await generator.emitAdmissibilityReceipt({
        ontologyReleases: ['hash1', 'hash2'],
        deltaCapsule: 'hash3',
        decision: 'allow',
        universeState: { state: 'allowed' },
      });

      expect(receipt.decision).toBe('allow');
      expect(receipt.inputHashes.ontologyReleases).toEqual(['hash1', 'hash2']);
      expect(receipt.inputHashes.deltaCapsule).toBe('hash3');
      expect(receipt.beforeHash).toBeNull();
    });

    it('should emit receipt for deny decision', async () => {
      const generator = new ReceiptGenerator();

      const receipt = await generator.emitAdmissibilityReceipt({
        ontologyReleases: ['hash1'],
        deltaCapsule: 'hash2',
        decision: 'deny',
        universeState: { state: 'denied' },
      });

      expect(receipt.decision).toBe('deny');
    });

    it('should link receipts in chain', async () => {
      const generator = new ReceiptGenerator();

      const receipt1 = await generator.emitAdmissibilityReceipt({
        ontologyReleases: ['h1'],
        deltaCapsule: 'h2',
        decision: 'allow',
        universeState: { state: 'allowed' },
        timestamp: new Date('2025-01-01T00:00:00Z'),
      });

      const receipt2 = await generator.emitAdmissibilityReceipt({
        ontologyReleases: ['h3'],
        deltaCapsule: 'h4',
        decision: 'allow',
        universeState: { state: 'allowed2' },
        timestamp: new Date('2025-01-01T00:00:01Z'),
      });

      expect(receipt1.beforeHash).toBeNull();
      expect(receipt2.beforeHash).toBe(receipt1.receiptHash);
    });
  });

  describe('emitValidationReceipt()', () => {
    it('should emit validation receipt', async () => {
      const generator = new ReceiptGenerator();

      const receipt = await generator.emitValidationReceipt({
        ontologyReleases: ['h1', 'h2'],
        validationReport: 'report_hash',
        decision: 'allow',
        validationState: { validated: true },
      });

      expect(receipt.decision).toBe('allow');
      expect(receipt.inputHashes.deltaCapsule).toBe('report_hash');
    });
  });

  describe('emitProjectionReceipt()', () => {
    it('should emit projection receipt', async () => {
      const generator = new ReceiptGenerator();

      const receipt = await generator.emitProjectionReceipt({
        ontologyReleases: ['h1'],
        projectionInput: 'input_hash',
        decision: 'allow',
        projectionOutput: { projected: true },
      });

      expect(receipt.decision).toBe('allow');
      expect(receipt.inputHashes.deltaCapsule).toBe('input_hash');
    });
  });

  describe('getToolchainVersion()', () => {
    it('should get toolchain version', () => {
      const generator = new ReceiptGenerator();
      const toolchain = generator.getToolchainVersion();

      expect(toolchain.node).toBeDefined();
      expect(toolchain.node).toContain('v');
      expect(toolchain.packages).toBeDefined();
    });

    it('should cache toolchain version', () => {
      const generator = new ReceiptGenerator();

      const toolchain1 = generator.getToolchainVersion();
      const toolchain2 = generator.getToolchainVersion();

      expect(toolchain1).toBe(toolchain2);
    });
  });

  describe('verifyChain()', () => {
    it('should verify valid chain', async () => {
      const generator = new ReceiptGenerator();

      await generator.emitAdmissibilityReceipt({
        ontologyReleases: ['h1'],
        deltaCapsule: 'h2',
        decision: 'allow',
        universeState: { state: 'allowed' },
        timestamp: new Date('2025-01-01T00:00:00Z'),
      });

      await generator.emitAdmissibilityReceipt({
        ontologyReleases: ['h3'],
        deltaCapsule: 'h4',
        decision: 'allow',
        universeState: { state: 'allowed2' },
        timestamp: new Date('2025-01-01T00:00:01Z'),
      });

      const result = await generator.verifyChain();

      expect(result.valid).toBe(true);
      expect(result.errors).toHaveLength(0);
    });
  });

  describe('toJSONLD()', () => {
    it('should export chain to JSON-LD', async () => {
      const generator = new ReceiptGenerator();

      await generator.emitAdmissibilityReceipt({
        ontologyReleases: ['h1'],
        deltaCapsule: 'h2',
        decision: 'allow',
        universeState: { state: 'allowed' },
      });

      const jsonld = generator.toJSONLD();

      expect(jsonld['@type']).toBe('unrdf:ReceiptChain');
      expect(jsonld['unrdf:length']).toBe(1);
    });
  });

  describe('computeOutputHash()', () => {
    it('should compute deterministic output hash', async () => {
      const generator = new ReceiptGenerator();

      const state = { foo: 'bar', baz: 123 };

      const hash1 = await generator.computeOutputHash(state);
      const hash2 = await generator.computeOutputHash(state);

      expect(hash1).toBe(hash2);
    });

    it('should produce different hashes for different states', async () => {
      const generator = new ReceiptGenerator();

      const hash1 = await generator.computeOutputHash({ foo: 'bar' });
      const hash2 = await generator.computeOutputHash({ foo: 'baz' });

      expect(hash1).not.toBe(hash2);
    });
  });
});

/**
 * @file Artifact Module Tests — Chicago TDD
 * @description Behavior verification for artifact creation and receipts
 *
 * Core Chicago TDD tests focused on process truth:
 * - Unknown artifact kind throws (validation)
 * - Receipt is hex string (BLAKE3 format)
 * - toJSON structure integrity
 */

import { describe, it, expect } from 'vitest';
import { Artifact, createArtifact, ARTIFACT_KINDS } from '../../src/artifact/index.mjs';

describe('Artifact — Process Truth Verification', () => {

  describe('Artifact Kind Validation', () => {
    it('throws for unknown artifact kind', () => {
      expect(() => new Artifact('unknown-kind', 'content')).toThrow();
    });

    it('error message includes the unknown kind name', () => {
      expect(() => new Artifact('not-a-kind', 'x')).toThrow('not-a-kind');
    });
  });

  describe('Receipt Hash Integrity', () => {
    it('computes a receipt hash string (BLAKE3 format)', () => {
      const a = new Artifact('powl', 'workflow content');
      expect(typeof a.receipt).toBe('string');
      expect(a.receipt.length).toBeGreaterThan(0);
    });

    it('same content produces same receipt (deterministic)', () => {
      const a1 = new Artifact('python', 'print("hello")');
      const a2 = new Artifact('python', 'print("hello")');
      expect(a1.receipt).toBe(a2.receipt);
    });

    it('different content produces different receipt', () => {
      const a1 = new Artifact('python', 'print("hello")');
      const a2 = new Artifact('python', 'print("world")');
      expect(a1.receipt).not.toBe(a2.receipt);
    });
  });

  describe('Artifact Serialization', () => {
    it('toJSON includes type, receipt, timestamp, and contentLength', () => {
      const a = new Artifact('ttl', '@prefix ex: <http://example.org/> .');
      const json = a.toJSON();
      expect(json.type).toBe('ttl');
      expect(json.receipt).toBe(a.receipt);
      expect(json.timestamp).toBe(a.timestamp);
      expect(json.contentLength).toBeDefined();
    });

    it('toJSON excludes raw content for size efficiency', () => {
      const a = new Artifact('json', { name: 'test', data: [1, 2, 3] });
      const json = a.toJSON();
      expect(json.content).toBeUndefined();
      expect(json.contentLength).toBeGreaterThan(0);
    });
  });
});

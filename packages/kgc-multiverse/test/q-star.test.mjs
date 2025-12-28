/**
 * KGC Multiverse - Q* Validation Tests
 * Tests Q* invariants: identity stability, RDF semantics, provenance chain
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  QStarErrorCode,
  QStarIDSchema,
  QStarRDFSchema,
  QStarPROVSchema,
  QStarSnapshotSchema,
  extractIRIs,
  computeCanonicalHash,
  createQStarSnapshot,
  QStarValidator,
  createQStarValidator,
  validateQStarSnapshot,
} from '../src/q-star.mjs';

// Test fixtures
const createTestQuads = (count = 10) => {
  const quads = [];
  for (let i = 0; i < count; i++) {
    quads.push({
      subject: { value: `http://example.com/subject/${i}`, termType: 'NamedNode' },
      predicate: { value: `http://example.com/predicate/${i % 3}`, termType: 'NamedNode' },
      object: { value: `value-${i}`, termType: 'Literal' },
    });
  }
  return quads;
};

const createTestReceipts = (count = 5) => {
  const receipts = [];
  let previousHash = null;
  const baseTimestamp = BigInt(Date.now()) * 1_000_000n;

  for (let i = 0; i < count; i++) {
    const currentHash = 'a'.repeat(64 - i.toString().length) + i.toString().padStart(i.toString().length, '0');
    receipts.push({
      Q_ID: `Q*_${i.toString(16).padStart(16, '0')}`,
      Q_PROV: {
        timestamp: baseTimestamp + BigInt(i * 1000000),
        previousHash: previousHash,
        contentHash: currentHash.slice(0, 64),
        sequenceNumber: i,
      },
    });
    previousHash = currentHash.slice(0, 64);
  }
  return receipts;
};

describe('Q* Validation System', () => {
  describe('IRI Extraction', () => {
    it('extracts IRIs from quads', () => {
      const quads = createTestQuads(5);
      const iris = extractIRIs(quads);

      expect(iris.size).toBeGreaterThan(0);
      expect(iris.has('http://example.com/subject/0')).toBe(true);
      expect(iris.has('http://example.com/predicate/0')).toBe(true);
    });

    it('handles empty quads array', () => {
      const iris = extractIRIs([]);
      expect(iris.size).toBe(0);
    });

    it('deduplicates IRIs', () => {
      const quads = [
        { subject: { value: 'http://ex.com/s1', termType: 'NamedNode' }, predicate: { value: 'http://ex.com/p1' }, object: { value: 'v1', termType: 'Literal' } },
        { subject: { value: 'http://ex.com/s1', termType: 'NamedNode' }, predicate: { value: 'http://ex.com/p1' }, object: { value: 'v2', termType: 'Literal' } },
      ];
      const iris = extractIRIs(quads);

      // Should only have 2 unique IRIs (s1 and p1), not 4
      expect(iris.size).toBe(2);
    });
  });

  describe('Canonical Hash', () => {
    it('computes hash for quads', async () => {
      const quads = createTestQuads(3);
      const hash = await computeCanonicalHash(quads);

      expect(hash).toMatch(/^[a-f0-9]{64}$/);
    });

    it('produces consistent hash for same quads', async () => {
      const quads = createTestQuads(3);
      const hash1 = await computeCanonicalHash(quads);
      const hash2 = await computeCanonicalHash(quads);

      expect(hash1).toBe(hash2);
    });

    it('produces different hash for different quads', async () => {
      const quads1 = createTestQuads(3);
      const quads2 = createTestQuads(5);

      const hash1 = await computeCanonicalHash(quads1);
      const hash2 = await computeCanonicalHash(quads2);

      expect(hash1).not.toBe(hash2);
    });
  });

  describe('Q* Snapshot Creation', () => {
    it('creates valid snapshot', async () => {
      const quads = createTestQuads(10);

      const snapshot = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads,
        receipts: [],
      });

      // Validate against schema
      expect(() => QStarSnapshotSchema.parse(snapshot)).not.toThrow();

      // Check structure
      expect(snapshot.Q_ID.Q_ID).toBe('Q*_0123456789abcdef');
      expect(snapshot.Q_RDF.quadCount).toBe(10);
      expect(snapshot.Q_PROV.chainLength).toBe(1);
    });

    it('includes IRI corpus in snapshot', async () => {
      const quads = createTestQuads(5);

      const snapshot = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads,
        receipts: [],
      });

      expect(snapshot.Q_ID.iriCorpus).toBeDefined();
      expect(snapshot.Q_ID.iriCount).toBeGreaterThan(0);
      expect(snapshot.Q_ID.iriCorpus.length).toBe(snapshot.Q_ID.iriCount);
    });
  });

  describe('Identity Stability (Q1, Q2)', () => {
    let validator;

    beforeEach(() => {
      validator = createQStarValidator({ strict: true });
    });

    it('passes when all IRIs preserved', async () => {
      const quads = createTestQuads(10);

      const snapshot_i = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads,
        receipts: [],
      });

      // Add more quads (preserves original IRIs)
      const moreQuads = [
        ...quads,
        { subject: { value: 'http://ex.com/new', termType: 'NamedNode' }, predicate: { value: 'http://ex.com/p' }, object: { value: 'v', termType: 'Literal' } },
      ];

      const snapshot_j = await createQStarSnapshot({
        universeID: 'Q*_fedcba9876543210',
        universeRDF: 'http://kgc.io/multiverse/fedcba9876543210',
        quads: moreQuads,
        receipts: [],
      });

      const result = validator.checkIdentityStability(snapshot_i, snapshot_j);

      expect(result.valid).toBe(true);
      expect(result.details.irisPreserved).toBe(snapshot_i.Q_ID.iriCount);
    });

    it('fails when IRIs are missing (Q1)', async () => {
      const quads = createTestQuads(10);

      const snapshot_i = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads,
        receipts: [],
      });

      // Remove some quads (losing IRIs)
      const fewerQuads = quads.slice(0, 3);

      const snapshot_j = await createQStarSnapshot({
        universeID: 'Q*_fedcba9876543210',
        universeRDF: 'http://kgc.io/multiverse/fedcba9876543210',
        quads: fewerQuads,
        receipts: [],
      });

      const result = validator.checkIdentityStability(snapshot_i, snapshot_j);

      expect(result.valid).toBe(false);
      expect(result.errorCode).toBe(QStarErrorCode.Q1_IRI_MISSING);
      expect(result.details.totalMissing).toBeGreaterThan(0);
    });
  });

  describe('RDF Semantics (Q3, Q4)', () => {
    let validator;

    beforeEach(() => {
      validator = createQStarValidator();
    });

    it('passes when semantics preserved', async () => {
      const quads = createTestQuads(10);

      const snapshot_i = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads,
        receipts: [],
      });

      // Same quads, different universe
      const snapshot_j = await createQStarSnapshot({
        universeID: 'Q*_fedcba9876543210',
        universeRDF: 'http://kgc.io/multiverse/fedcba9876543210',
        quads,
        receipts: [],
      });

      const result = validator.checkRDFSemantics(snapshot_i, snapshot_j);

      expect(result.valid).toBe(true);
    });

    it('detects major semantic changes', async () => {
      const quads_i = createTestQuads(100);

      const snapshot_i = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads: quads_i,
        receipts: [],
      });

      // Only 10 quads (10% of original)
      const quads_j = createTestQuads(10);

      const snapshot_j = await createQStarSnapshot({
        universeID: 'Q*_fedcba9876543210',
        universeRDF: 'http://kgc.io/multiverse/fedcba9876543210',
        quads: quads_j,
        receipts: [],
      });

      const result = validator.checkRDFSemantics(snapshot_i, snapshot_j);

      // Still valid but with major change warning
      expect(result.valid).toBe(true);
      expect(result.details.majorChange).toBe(true);
    });

    it('detects hash divergence when strict', async () => {
      const quads_i = createTestQuads(10);
      const quads_j = createTestQuads(10);
      quads_j[0].object.value = 'modified-value'; // Change one value

      const snapshot_i = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads: quads_i,
        receipts: [],
      });

      const snapshot_j = await createQStarSnapshot({
        universeID: 'Q*_fedcba9876543210',
        universeRDF: 'http://kgc.io/multiverse/fedcba9876543210',
        quads: quads_j,
        receipts: [],
      });

      // With allowQuadChanges=false, should detect divergence
      const result = validator.checkRDFSemantics(snapshot_i, snapshot_j, { allowQuadChanges: false });

      expect(result.valid).toBe(false);
      expect(result.errorCode).toBe(QStarErrorCode.Q4_SPARQL_DIVERGENCE);
    });
  });

  describe('Provenance Chain (Q5-Q8)', () => {
    let validator;

    beforeEach(() => {
      validator = createQStarValidator();
    });

    it('validates intact hash chain (Q5)', () => {
      const receipts = createTestReceipts(5);
      const result = validator.checkProvenanceChain(receipts);

      expect(result.valid).toBe(true);
      expect(result.details.chainLength).toBe(5);
    });

    it('detects broken hash chain (Q5)', () => {
      const receipts = createTestReceipts(5);

      // Break the chain by modifying a previousHash
      receipts[2].Q_PROV.previousHash = 'broken'.padEnd(64, '0');

      const result = validator.checkProvenanceChain(receipts);

      expect(result.valid).toBe(false);
      expect(result.errorCode).toBe(QStarErrorCode.Q5_HASH_MISMATCH);
    });

    it('detects timestamp ordering violations (Q6)', () => {
      const receipts = createTestReceipts(5);

      // Make timestamps out of order
      const temp = receipts[2].Q_PROV.timestamp;
      receipts[2].Q_PROV.timestamp = receipts[1].Q_PROV.timestamp - 1000000n;

      const result = validator.checkProvenanceChain(receipts);

      expect(result.valid).toBe(false);
      expect(result.errorCode).toBe(QStarErrorCode.Q6_TIMESTAMP_ORDER);
    });

    it('handles empty receipt chain (genesis)', () => {
      const result = validator.checkProvenanceChain([]);

      expect(result.valid).toBe(true);
      expect(result.details.chainLength).toBe(0);
    });
  });

  describe('Comprehensive Q* Validation', () => {
    let validator;

    beforeEach(() => {
      validator = createQStarValidator({ strict: true });
    });

    it('validates complete Q* invariants', async () => {
      const quads = createTestQuads(10);
      const receipts = createTestReceipts(3);

      const snapshot_i = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads,
        receipts,
      });

      const snapshot_j = await createQStarSnapshot({
        universeID: 'Q*_fedcba9876543210',
        universeRDF: 'http://kgc.io/multiverse/fedcba9876543210',
        quads, // Same quads
        receipts,
      });

      const result = validator.validateQStar({
        snapshot_i,
        snapshot_j,
        receipts,
      });

      expect(result.allPassed).toBe(true);
      expect(result.results.identity.valid).toBe(true);
      expect(result.results.rdfSemantics.valid).toBe(true);
      expect(result.results.provenance.valid).toBe(true);
    });

    it('tracks validation history', async () => {
      const quads = createTestQuads(5);

      const snapshot = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads,
        receipts: [],
      });

      // Run multiple validations
      validator.checkIdentityStability(snapshot, snapshot);
      validator.checkRDFSemantics(snapshot, snapshot);
      validator.checkProvenanceChain([]);

      const history = validator.getValidationHistory();

      expect(history.length).toBe(3);
    });

    it('clears validation history', () => {
      const testValidator = createQStarValidator();

      // Add some history
      testValidator.checkProvenanceChain([]);

      expect(testValidator.getValidationHistory().length).toBe(1);

      testValidator.clearHistory();

      expect(testValidator.getValidationHistory().length).toBe(0);
    });
  });

  describe('Schema Validation', () => {
    it('validates Q*_ID schema', () => {
      const valid = {
        Q_ID: 'Q*_0123456789abcdef',
        Q_RDF: 'http://kgc.io/multiverse/test',
      };

      expect(() => QStarIDSchema.parse(valid)).not.toThrow();
    });

    it('rejects invalid Q*_ID format', () => {
      const invalid = {
        Q_ID: 'invalid-id',
        Q_RDF: 'http://kgc.io/multiverse/test',
      };

      expect(() => QStarIDSchema.parse(invalid)).toThrow();
    });

    it('validates Q*_PROV schema', () => {
      const valid = {
        timestamp: BigInt(Date.now()) * 1_000_000n,
        currentHash: 'a'.repeat(64),
        sequenceNumber: 0,
        chainLength: 1,
      };

      expect(() => QStarPROVSchema.parse(valid)).not.toThrow();
    });
  });

  describe('Edge Cases', () => {
    it('handles 100k quads', async () => {
      const quads = createTestQuads(100000);
      const iris = extractIRIs(quads);

      expect(iris.size).toBeGreaterThan(0);

      const hash = await computeCanonicalHash(quads);
      expect(hash).toMatch(/^[a-f0-9]{64}$/);
    }, 10000);

    it('handles empty store', async () => {
      const snapshot = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads: [],
        receipts: [],
      });

      expect(snapshot.Q_RDF.quadCount).toBe(0);
      expect(snapshot.Q_ID.iriCount).toBe(0);
    });

    it('validateQStarSnapshot helper works', async () => {
      const quads = createTestQuads(5);

      const snapshot = await createQStarSnapshot({
        universeID: 'Q*_0123456789abcdef',
        universeRDF: 'http://kgc.io/multiverse/0123456789abcdef',
        quads,
        receipts: [],
      });

      const result = validateQStarSnapshot(snapshot);

      expect(result.valid).toBe(true);
    });
  });
});

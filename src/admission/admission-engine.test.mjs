/**
 * @fileoverview Comprehensive test suite for Admission Engine
 *
 * Tests cover:
 * - Valid Δ (additive only) → ALLOW
 * - Invalid Δ (edits protected NS) → DENY
 * - Invalid Δ (redefines substrate term) → DENY
 * - Invalid Δ (collision) → DENY
 * - All invariants run and failures logged
 *
 * @module admission/admission-engine.test
 */

import { describe, it, expect } from 'vitest';
import { DeltaCapsule } from './delta-capsule.mjs';
import { AdmissionEngine } from './admission-engine.mjs';
import {
  guardEditIndustrialSubstrate,
  guardRedefineProtectedTerm,
  guardWeakenCorporateCanon,
  isProtectedNamespace,
  isCanonicalTerm
} from './forbidden-operations.mjs';
import {
  Q_typing,
  Q_noncollision,
  Q_monotone,
  Q_determinism,
  Q_provenance,
  Q_bounds
} from './invariants.mjs';

/**
 * Helper: Create a valid quad
 */
function createQuad(s, p, o, g = null) {
  return {
    subject: {
      termType: s.startsWith('_:') ? 'BlankNode' : 'NamedNode',
      value: s
    },
    predicate: {
      termType: 'NamedNode',
      value: p
    },
    object: {
      termType: o.startsWith('http') ? 'NamedNode' : 'Literal',
      value: o
    },
    graph: g ? {
      termType: 'NamedNode',
      value: g
    } : undefined
  };
}

/**
 * Helper: Create a valid delta capsule
 */
function createValidDelta(overrides = {}) {
  return new DeltaCapsule({
    partition: {
      namespace: 'http://example.org/overlay/',
      name: 'test-overlay',
      protected: false
    },
    changes: [{
      operation: 'add',
      quads: [
        createQuad(
          'http://example.org/overlay/subject1',
          'http://example.org/overlay/predicate1',
          'http://example.org/overlay/object1'
        )
      ]
    }],
    invariants: [
      { name: 'Q_typing', enabled: true },
      { name: 'Q_noncollision', enabled: true },
      { name: 'Q_monotone', enabled: true },
      { name: 'Q_determinism', enabled: true },
      { name: 'Q_provenance', enabled: true },
      { name: 'Q_bounds', enabled: true }
    ],
    provenance: {
      agent: 'test-agent',
      timestamp: new Date().toISOString(),
      source: 'test',
      justification: 'test data'
    },
    ...overrides
  });
}

describe('DeltaCapsule', () => {
  it('should create a valid delta capsule', () => {
    const delta = createValidDelta();

    expect(delta.id).toBeDefined();
    expect(delta.partition.name).toBe('test-overlay');
    expect(delta.getQuadCount()).toBe(1);
    expect(delta.isAdditiveOnly()).toBe(true);
  });

  it('should compute deterministic hash', () => {
    const delta1 = createValidDelta();
    const delta2 = createValidDelta();

    // Same content should produce same hash
    expect(delta1.getHash()).toBe(delta2.getHash());
  });

  it('should reject invalid capsule structure', () => {
    expect(() => {
      new DeltaCapsule({
        // Missing required fields
        partition: { namespace: 'http://example.org/', name: 'test' }
      });
    }).toThrow();
  });

  it('should extract namespaces correctly', () => {
    const delta = createValidDelta();
    const namespaces = delta.getNamespaces();

    expect(namespaces.has('http://example.org/overlay/')).toBe(true);
  });
});

describe('Forbidden Operations', () => {
  describe('guardEditIndustrialSubstrate', () => {
    it('should ALLOW additions to overlay namespace', () => {
      const delta = createValidDelta();
      const result = guardEditIndustrialSubstrate(delta);

      expect(result.allowed).toBe(true);
      expect(result.guardName).toBe('EditIndustrialSubstrate');
    });

    it('should DENY deletions from protected namespace', () => {
      const delta = new DeltaCapsule({
        partition: { namespace: 'http://example.org/', name: 'test', protected: false },
        changes: [{
          operation: 'delete',
          quads: [
            createQuad(
              'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
              'http://www.w3.org/2000/01/rdf-schema#label',
              'Type'
            )
          ]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: {
          agent: 'test',
          timestamp: new Date().toISOString()
        }
      });

      const result = guardEditIndustrialSubstrate(delta);

      expect(result.allowed).toBe(false);
      expect(result.severity).toBe('block');
      expect(result.violatingQuads).toBeDefined();
      expect(result.violatingQuads.length).toBeGreaterThan(0);
    });
  });

  describe('guardRedefineProtectedTerm', () => {
    it('should DENY redefinition of canonical term', () => {
      const delta = new DeltaCapsule({
        partition: { namespace: 'http://example.org/', name: 'test', protected: false },
        changes: [{
          operation: 'add',
          quads: [
            createQuad(
              'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
              'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
              'http://www.w3.org/2000/01/rdf-schema#Class'
            )
          ]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: {
          agent: 'test',
          timestamp: new Date().toISOString()
        }
      });

      const result = guardRedefineProtectedTerm(delta);

      expect(result.allowed).toBe(false);
      expect(result.guardName).toBe('RedefineProtectedTerm');
    });
  });

  describe('guardWeakenCorporateCanon', () => {
    it('should DENY deletion of canonical type assertion', () => {
      const delta = new DeltaCapsule({
        partition: { namespace: 'http://example.org/', name: 'test', protected: false },
        changes: [{
          operation: 'delete',
          quads: [
            createQuad(
              'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
              'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
              'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
            )
          ]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: {
          agent: 'test',
          timestamp: new Date().toISOString()
        }
      });

      const result = guardWeakenCorporateCanon(delta);

      expect(result.allowed).toBe(false);
      expect(result.guardName).toBe('WeakenCorporateCanon');
    });
  });

  describe('helper functions', () => {
    it('should identify protected namespaces', () => {
      expect(isProtectedNamespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#type')).toBe(true);
      expect(isProtectedNamespace('http://example.org/custom')).toBe(false);
    });

    it('should identify canonical terms', () => {
      expect(isCanonicalTerm('http://www.w3.org/1999/02/22-rdf-syntax-ns#type')).toBe(true);
      expect(isCanonicalTerm('http://example.org/custom')).toBe(false);
    });
  });
});

describe('Invariants', () => {
  describe('Q_typing', () => {
    it('should PASS for valid RDF quads', () => {
      const delta = createValidDelta();
      const result = Q_typing(delta);

      expect(result.passed).toBe(true);
      expect(result.invariantName).toBe('Q_typing');
    });

    it('should FAIL for literal subject', () => {
      const delta = new DeltaCapsule({
        partition: { namespace: 'http://example.org/', name: 'test', protected: false },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'Literal', value: 'invalid' },
            predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
            object: { termType: 'Literal', value: 'test' }
          }]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: { agent: 'test', timestamp: new Date().toISOString() }
      });

      const result = Q_typing(delta);

      expect(result.passed).toBe(false);
      expect(result.violations).toBeDefined();
      expect(result.violations.length).toBeGreaterThan(0);
    });

    it('should FAIL for invalid IRI', () => {
      const delta = new DeltaCapsule({
        partition: { namespace: 'http://example.org/', name: 'test', protected: false },
        changes: [{
          operation: 'add',
          quads: [{
            subject: { termType: 'NamedNode', value: 'not-a-valid-iri' },
            predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
            object: { termType: 'Literal', value: 'test' }
          }]
        }],
        invariants: [{ name: 'Q_typing', enabled: true }],
        provenance: { agent: 'test', timestamp: new Date().toISOString() }
      });

      const result = Q_typing(delta);

      expect(result.passed).toBe(false);
    });
  });

  describe('Q_noncollision', () => {
    it('should PASS for overlay namespace', () => {
      const delta = createValidDelta();
      const result = Q_noncollision(delta);

      expect(result.passed).toBe(true);
    });

    it('should FAIL for protected namespace collision', () => {
      const delta = new DeltaCapsule({
        partition: { namespace: 'http://example.org/', name: 'test', protected: false },
        changes: [{
          operation: 'add',
          quads: [
            createQuad(
              'http://www.w3.org/1999/02/22-rdf-syntax-ns#customType',
              'http://example.org/p',
              'test'
            )
          ]
        }],
        invariants: [{ name: 'Q_noncollision', enabled: true }],
        provenance: { agent: 'test', timestamp: new Date().toISOString() }
      });

      const result = Q_noncollision(delta);

      expect(result.passed).toBe(false);
      expect(result.violations).toBeDefined();
    });
  });

  describe('Q_monotone', () => {
    it('should PASS for additive overlay', () => {
      const delta = createValidDelta();
      const result = Q_monotone(delta);

      expect(result.passed).toBe(true);
      expect(result.metadata.isAdditiveOnly).toBe(true);
    });

    it('should FAIL for modification of canonical term', () => {
      const delta = new DeltaCapsule({
        partition: { namespace: 'http://example.org/', name: 'test', protected: false },
        changes: [{
          operation: 'add',
          quads: [
            createQuad(
              'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
              'http://www.w3.org/2000/01/rdf-schema#domain',
              'http://example.org/Something'
            )
          ]
        }],
        invariants: [{ name: 'Q_monotone', enabled: true }],
        provenance: { agent: 'test', timestamp: new Date().toISOString() }
      });

      const result = Q_monotone(delta);

      expect(result.passed).toBe(false);
    });
  });

  describe('Q_determinism', () => {
    it('should PASS for valid hash', () => {
      const delta = createValidDelta();
      const result = Q_determinism(delta);

      expect(result.passed).toBe(true);
      expect(result.metadata.hash).toBeDefined();
      expect(result.metadata.hash.length).toBe(64);
    });

    it('should detect hash mismatch', () => {
      const delta = createValidDelta();
      const originalHash = delta.getHash();

      const result = Q_determinism(delta, {
        expectedHash: 'wrong-hash-1234567890abcdef1234567890abcdef1234567890abcdef1234567890'
      });

      expect(result.passed).toBe(false);
    });
  });

  describe('Q_provenance', () => {
    it('should PASS for valid provenance', () => {
      const delta = createValidDelta();
      const result = Q_provenance(delta);

      expect(result.passed).toBe(true);
      expect(result.metadata.agent).toBe('test-agent');
    });

    it('should FAIL for missing agent', () => {
      const delta = new DeltaCapsule({
        partition: { namespace: 'http://example.org/', name: 'test', protected: false },
        changes: [{
          operation: 'add',
          quads: [createQuad('http://example.org/s', 'http://example.org/p', 'o')]
        }],
        invariants: [{ name: 'Q_provenance', enabled: true }],
        provenance: {
          agent: '',
          timestamp: new Date().toISOString()
        }
      });

      const result = Q_provenance(delta);

      expect(result.passed).toBe(false);
      expect(result.violations.some(v => v.error.includes('Agent'))).toBe(true);
    });
  });

  describe('Q_bounds', () => {
    it('should PASS for small capsule', () => {
      const delta = createValidDelta();
      const result = Q_bounds(delta);

      expect(result.passed).toBe(true);
      expect(result.metadata.quadCount).toBe(1);
    });

    it('should FAIL for excessive quad count', () => {
      const quads = [];
      for (let i = 0; i < 100; i++) {
        quads.push(createQuad(
          `http://example.org/s${i}`,
          'http://example.org/p',
          `object${i}`
        ));
      }

      const delta = new DeltaCapsule({
        partition: { namespace: 'http://example.org/', name: 'test', protected: false },
        changes: [{ operation: 'add', quads }],
        invariants: [{ name: 'Q_bounds', enabled: true }],
        provenance: { agent: 'test', timestamp: new Date().toISOString() }
      });

      const result = Q_bounds(delta, { bounds: { maxQuads: 50 } });

      expect(result.passed).toBe(false);
      expect(result.violations.some(v => v.error.includes('Quad count'))).toBe(true);
    });
  });
});

describe('AdmissionEngine', () => {
  it('should ALLOW valid additive capsule', async () => {
    const engine = new AdmissionEngine({ strictMode: true });
    const delta = createValidDelta();

    const decision = await engine.admitCapsule(delta);

    expect(decision.allowed).toBe(true);
    expect(decision.decision).toBe('ALLOW');
    expect(decision.checks.forbiddenOperations.passed).toBe(true);
    expect(decision.checks.invariants.passed).toBe(true);
  });

  it('should DENY capsule that edits protected namespace', async () => {
    const engine = new AdmissionEngine({ strictMode: true });

    const delta = new DeltaCapsule({
      partition: { namespace: 'http://example.org/', name: 'test', protected: false },
      changes: [{
        operation: 'delete',
        quads: [
          createQuad(
            'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            'http://www.w3.org/2000/01/rdf-schema#label',
            'Type'
          )
        ]
      }],
      invariants: [{ name: 'Q_typing', enabled: true }],
      provenance: { agent: 'test', timestamp: new Date().toISOString() }
    });

    const decision = await engine.admitCapsule(delta);

    expect(decision.allowed).toBe(false);
    expect(decision.decision).toBe('DENY');
    expect(decision.checks.forbiddenOperations.passed).toBe(false);
    expect(decision.checks.forbiddenOperations.blockedBy).toContain('EditIndustrialSubstrate');
  });

  it('should DENY capsule that redefines substrate term', async () => {
    const engine = new AdmissionEngine({ strictMode: true });

    const delta = new DeltaCapsule({
      partition: { namespace: 'http://example.org/', name: 'test', protected: false },
      changes: [{
        operation: 'add',
        quads: [
          createQuad(
            'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            'http://example.org/MyClass'
          )
        ]
      }],
      invariants: [{ name: 'Q_typing', enabled: true }],
      provenance: { agent: 'test', timestamp: new Date().toISOString() }
    });

    const decision = await engine.admitCapsule(delta);

    expect(decision.allowed).toBe(false);
    expect(decision.checks.forbiddenOperations.passed).toBe(false);
  });

  it('should DENY capsule with namespace collision', async () => {
    const engine = new AdmissionEngine({ strictMode: true });

    const delta = new DeltaCapsule({
      partition: { namespace: 'http://example.org/', name: 'test', protected: false },
      changes: [{
        operation: 'add',
        quads: [
          createQuad(
            'http://www.w3.org/2002/07/owl#myCustomClass',
            'http://example.org/p',
            'test'
          )
        ]
      }],
      invariants: [{ name: 'Q_noncollision', enabled: true }],
      provenance: { agent: 'test', timestamp: new Date().toISOString() }
    });

    const decision = await engine.admitCapsule(delta);

    expect(decision.allowed).toBe(false);
    expect(decision.checks.invariants.passed).toBe(false);
  });

  it('should track statistics', async () => {
    const engine = new AdmissionEngine();

    const validDelta = createValidDelta();
    await engine.admitCapsule(validDelta);

    const stats = engine.getStats();

    expect(stats.totalProcessed).toBe(1);
    expect(stats.allowed).toBe(1);
    expect(stats.denied).toBe(0);
  });

  it('should maintain audit log', async () => {
    const engine = new AdmissionEngine({ auditLog: true });

    const delta = createValidDelta();
    await engine.admitCapsule(delta);

    const log = engine.getAuditLog();

    expect(log.length).toBe(1);
    expect(log[0].decision).toBe('ALLOW');
    expect(log[0].agent).toBe('test-agent');
  });

  it('should support dry-run validation', async () => {
    const engine = new AdmissionEngine();

    const delta = createValidDelta();
    const result = await engine.validateCapsule(delta);

    expect(result.isDryRun).toBe(true);
    expect(result.valid).toBe(true);

    // Stats should not be updated
    const stats = engine.getStats();
    expect(stats.totalProcessed).toBe(0);
  });

  it('should handle batch processing', async () => {
    const engine = new AdmissionEngine();

    const deltas = [
      createValidDelta(),
      createValidDelta(),
      createValidDelta()
    ];

    const results = await engine.admitBatch(deltas);

    expect(results.length).toBe(3);
    expect(results.every(r => r.allowed)).toBe(true);
  });
});

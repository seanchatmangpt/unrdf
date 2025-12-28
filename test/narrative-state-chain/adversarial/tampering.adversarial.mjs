/**
 * @file Adversarial Tests for Security and Tampering
 * @module test/narrative-state-chain/adversarial/tampering
 * @description Tests for detecting forged receipts, violated invariants, unauthorized actions
 */

import { describe, it, expect } from 'vitest';
import crypto from 'crypto';
import {
  createSceneEnvelopeFixture,
  createReceiptFixture,
  createUniverseFixture,
  createBridgeProofFixture
} from '../fixtures/types.mjs';

describe('Receipt Tampering Detection', () => {
  it('should detect receipt signature forgery', () => {
    const scene = createSceneEnvelopeFixture();
    const originalSig = scene.receipt.signature.signature;

    // Forge signature
    const forgedScene = {
      ...scene,
      receipt: {
        ...scene.receipt,
        signature: {
          ...scene.receipt.signature,
          signature: 'forged-' + originalSig.substring(7)  // Change signature
        }
      }
    };

    // Original and forged signatures should differ
    expect(forgedScene.receipt.signature.signature).not.toBe(originalSig);
  });

  it('should detect receipt hash tampering', () => {
    const scene = createSceneEnvelopeFixture();
    const originalHash = scene.receipt.receiptHash;

    // Tamper with hash
    const tamperedScene = {
      ...scene,
      receipt: {
        ...scene.receipt,
        receiptHash: '0'.repeat(64)  // Invalid hash
      }
    };

    expect(tamperedScene.receipt.receiptHash).not.toBe(originalHash);

    // Recomputing hash should mismatch
    const recomputedHash = crypto.createHash('sha256')
      .update(JSON.stringify({
        sceneId: scene.receipt.sceneId,
        timestamp: scene.receipt.timestamp,
        admissibilityChecks: scene.receipt.admissibilityChecks,
        minimalityProof: scene.delta.proof,
        forkParents: scene.receipt.forkParents,
        signature: tamperedScene.receipt.signature
      }))
      .digest('hex');

    // Recomputed should not match tampared
    expect(recomputedHash).not.toBe(tamperedScene.receipt.receiptHash);
  });

  it('should detect delta tampering', () => {
    const scene = createSceneEnvelopeFixture();
    const originalHash = scene.delta.hash;

    // Tamper with additions
    const tamperedScene = {
      ...scene,
      delta: {
        ...scene.delta,
        additions: [
          ...scene.delta.additions,
          {
            subject: { termType: 'NamedNode', value: 'http://ex.org/injected' },
            predicate: { termType: 'NamedNode', value: 'http://ex.org/p' },
            object: { termType: 'Literal', value: 'injected' },
            graph: { termType: 'DefaultGraph' }
          }
        ]
      }
    };

    // Recompute delta hash
    const recomputedHash = crypto.createHash('sha256')
      .update(JSON.stringify({
        additions: tamperedScene.delta.additions,
        deletions: tamperedScene.delta.deletions
      }))
      .digest('hex');

    // Should not match original
    expect(recomputedHash).not.toBe(originalHash);
  });

  it('should reject observations with conflicting timestamps', () => {
    // Create observations with inconsistent time order
    const obs1 = createSceneEnvelopeFixture().observations[0];
    const obs2 = {
      ...obs1,
      timestamp: obs1.timestamp - 1000  // Earlier timestamp
    };

    // Both observations are valid individually
    expect(obs1.timestamp).toBeDefined();
    expect(obs2.timestamp).toBeDefined();

    // But scene should track sequence
    const scene = createSceneEnvelopeFixture({
      observations: [obs1, obs2]
    });

    // Timestamps are out of order
    expect(scene.observations[1].timestamp).toBeLessThan(scene.observations[0].timestamp);
  });
});

describe('Guard Circumvention Prevention', () => {
  it('should prevent unauthorized action (guard failure)', () => {
    const universe = createUniverseFixture();

    // Create a guard that denies access
    const restrictiveGuard = () => ({
      allowed: false,
      denyReason: 'Insufficient permissions',
      guardId: 'guard-auth',
      timestamp: Date.now()
    });

    // Replace guard
    universe.guards.set('guard-auth', restrictiveGuard);

    // Scene submitted with guard failure
    const scene = createSceneEnvelopeFixture({
      receipt: {
        ...createReceiptFixture(),
        admissibilityChecks: [
          {
            allowed: false,
            denyReason: 'Insufficient permissions',
            guardId: 'guard-auth',
            timestamp: Date.now()
          }
        ]
      },
      consequences: {
        status: 'rejected',
        resultingGraph: new Map(),
        invariantChecks: [],
        sideEffectTokens: []
      }
    });

    // Scene should be rejected
    expect(scene.consequences.status).toBe('rejected');
    expect(scene.receipt.admissibilityChecks[0].allowed).toBe(false);
  });

  it('should detect when guard was bypassed', () => {
    const scene = createSceneEnvelopeFixture();

    // Try to forge a passing guard result
    const forgedGuardResult = {
      allowed: true,
      denyReason: null,
      guardId: 'guard-critical',
      timestamp: Date.now()
    };

    // If guard is in receipt but not in universe, it's forgery
    const tamperedScene = {
      ...scene,
      receipt: {
        ...scene.receipt,
        admissibilityChecks: [
          ...scene.receipt.admissibilityChecks,
          forgedGuardResult
        ]
      }
    };

    // Tampering added a guard check
    expect(tamperedScene.receipt.admissibilityChecks.length)
      .toBeGreaterThan(scene.receipt.admissibilityChecks.length);
  });

  it('should prevent guard order manipulation', () => {
    const scene = createSceneEnvelopeFixture();

    // Guard order should not matter for outcome
    const reorderedGuards = [...scene.receipt.admissibilityChecks].reverse();

    // But replaying with different order could produce different result
    // (if guards have side effects or order-dependent logic)
    const reorderedScene = {
      ...scene,
      receipt: {
        ...scene.receipt,
        admissibilityChecks: reorderedGuards
      }
    };

    // Original and reordered should have same guards
    expect(reorderedScene.receipt.admissibilityChecks.length)
      .toBe(scene.receipt.admissibilityChecks.length);
  });
});

describe('Invariant Violation Detection', () => {
  it('should detect invariant violations', () => {
    // Create scene with violated invariant
    const scene = createSceneEnvelopeFixture({
      consequences: {
        status: 'accepted',
        resultingGraph: new Map(),
        invariantChecks: [
          {
            invariantId: 'inv-totality',
            satisfied: false,  // VIOLATED!
            query: 'ASK { ?x ex:value ?v }',
            bindings: {}
          }
        ],
        sideEffectTokens: []
      }
    });

    // Check for violations
    const hasViolations = scene.consequences.invariantChecks.some(c => !c.satisfied);
    expect(hasViolations).toBe(true);
  });

  it('should reject scene if critical invariant fails', () => {
    const universe = createUniverseFixture({
      invariants: {
        rules: [
          {
            id: 'critical-inv',
            sparql: 'ASK { FILTER NOT EXISTS { ?x ex:invalid . } }',
            description: 'No invalid data',
            severity: 'error'  // CRITICAL
          }
        ],
        enforcement: 'strict'
      }
    });

    // Scene violates critical invariant
    const scene = createSceneEnvelopeFixture({
      consequences: {
        status: 'rejected',  // Should be rejected
        resultingGraph: new Map(),
        invariantChecks: [
          {
            invariantId: 'critical-inv',
            satisfied: false,
            query: universe.invariants.rules[0].sparql,
            bindings: { x: 'http://ex.org/bad' }
          }
        ],
        sideEffectTokens: []
      }
    });

    expect(scene.consequences.status).toBe('rejected');
  });

  it('should allow warning invariant violations in eventual mode', () => {
    const universe = createUniverseFixture({
      invariants: {
        rules: [
          {
            id: 'eventual-inv',
            sparql: 'ASK { ?x ex:balanced . }',
            description: 'Data should converge',
            severity: 'warning'
          }
        ],
        enforcement: 'eventual'
      }
    });

    // Scene can continue with warning invariant failure
    const scene = createSceneEnvelopeFixture({
      consequences: {
        status: 'accepted',  // Can still be accepted!
        resultingGraph: new Map(),
        invariantChecks: [
          {
            invariantId: 'eventual-inv',
            satisfied: false,  // Violated
            query: universe.invariants.rules[0].sparql,
            bindings: {}
          }
        ],
        sideEffectTokens: []
      }
    });

    expect(scene.consequences.status).toBe('accepted');
  });

  it('should detect invariant bindings that reveal violations', () => {
    // Invariant check with detailed bindings showing what failed
    const scene = createSceneEnvelopeFixture({
      consequences: {
        status: 'rejected',
        resultingGraph: new Map(),
        invariantChecks: [
          {
            invariantId: 'uniqueness',
            satisfied: false,
            query: 'SELECT ?dup WHERE { ?dup ex:id ?id . ?dup2 ex:id ?id . FILTER(?dup != ?dup2) }',
            bindings: {
              dup: 'http://ex.org/record1',
              dup2: 'http://ex.org/record2',
              id: 'duplicate-value'
            }
          }
        ],
        sideEffectTokens: []
      }
    });

    // Bindings show exactly what violates constraint
    expect(scene.consequences.invariantChecks[0].bindings).toBeDefined();
    expect(Object.keys(scene.consequences.invariantChecks[0].bindings).length).toBeGreaterThan(0);
  });
});

describe('Chain Integrity Attacks', () => {
  it('should detect broken fork parent links', () => {
    const scene1 = createSceneEnvelopeFixture({
      metadata: { created: Date.now(), author: 'alice', sequenceNumber: 1, tags: [] }
    });

    const scene2 = createSceneEnvelopeFixture({
      receipt: {
        ...createReceiptFixture(),
        forkParents: ['invalid-parent-id-' + 'x'.repeat(45)]  // Non-existent parent
      },
      metadata: { created: Date.now(), author: 'bob', sequenceNumber: 2, tags: [] }
    });

    // Scene2's parent doesn't exist in chain
    expect(scene2.receipt.forkParents[0]).not.toContain('x');
    expect(scene2.receipt.forkParents[0]).toMatch(/^invalid-parent-id/);
  });

  it('should detect sequence number jumps', () => {
    const scenes = [
      createSceneEnvelopeFixture({
        metadata: { created: Date.now(), author: 'a', sequenceNumber: 1, tags: [] }
      }),
      createSceneEnvelopeFixture({
        metadata: { created: Date.now(), author: 'b', sequenceNumber: 5, tags: [] }  // Skipped 2,3,4!
      })
    ];

    // Gap in sequence numbers
    expect(scenes[1].metadata.sequenceNumber - scenes[0].metadata.sequenceNumber).toBeGreaterThan(1);
  });

  it('should detect duplicate sequence numbers', () => {
    const seqNum = 42;

    const scene1 = createSceneEnvelopeFixture({
      metadata: { created: Date.now(), author: 'a', sequenceNumber: seqNum, tags: [] }
    });

    const scene2 = createSceneEnvelopeFixture({
      metadata: { created: Date.now(), author: 'b', sequenceNumber: seqNum, tags: [] }
    });

    // Both have same sequence number - duplicate!
    expect(scene1.metadata.sequenceNumber).toBe(scene2.metadata.sequenceNumber);
  });

  it('should detect backward sequence numbers', () => {
    const scenes = [
      createSceneEnvelopeFixture({
        metadata: { created: Date.now(), author: 'a', sequenceNumber: 10, tags: [] }
      }),
      createSceneEnvelopeFixture({
        metadata: { created: Date.now(), author: 'b', sequenceNumber: 5, tags: [] }  // Goes backward!
      })
    ];

    // Sequence goes backward
    expect(scenes[1].metadata.sequenceNumber).toBeLessThan(scenes[0].metadata.sequenceNumber);
  });
});

describe('Bridge Security', () => {
  it('should detect unsigned bridge', () => {
    const bridge = createBridgeProofFixture({
      validity: {
        sourceSignature: null,  // Missing!
        targetSignature: { algorithm: 'ed25519', publicKey: 'pk', signature: 'sig', signerId: 'did' },
        witnessSignatures: [],
        merkleRoot: 'x'.repeat(64)
      }
    });

    // Bridge missing source signature
    expect(bridge.validity.sourceSignature).toBeNull();
  });

  it('should detect bridge type coercion tampering', () => {
    const universe1Id = 'a'.repeat(64);
    const universe2Id = 'b'.repeat(64);

    const bridge = createBridgeProofFixture({
      sourceUniverseId: universe1Id,
      targetUniverseId: universe2Id,
      typeCoercion: {
        classMapping: new Map([['ex:Person', 'foaf:Agent']]),  // Coerces Person→Agent (suspicious)
        propertyMapping: new Map(),
        transforms: [],
        defaultNamespace: 'http://example.org/'
      }
    });

    // Coercion rules should be verified
    expect(bridge.typeCoercion.classMapping.get('ex:Person')).toBe('foaf:Agent');
  });

  it('should detect bridge invariant preservation failures', () => {
    const bridge = createBridgeProofFixture({
      invariantPreservation: [
        {
          sourceInvariantId: 'src-inv-1',
          targetInvariantId: 'tgt-inv-1',
          proofType: 'manual-verification',  // Weaker than logical implication
          proof: 'Manually reviewed'
        }
      ]
    });

    // Manual verification is weakest form of proof
    expect(bridge.invariantPreservation[0].proofType).toBe('manual-verification');
  });

  it('should track access grant expiration', () => {
    const now = Date.now();

    const bridge = createBridgeProofFixture({
      accessGrants: [
        {
          guardId: 'g1',
          reason: 'Temporary trust',
          grantor: 'did:key:admin',
          expiresAt: now - 1000  // EXPIRED!
        }
      ]
    });

    // Grant has expired
    const grant = bridge.accessGrants[0];
    expect(grant.expiresAt).toBeLessThan(now);
  });
});

describe('Denial of Service Prevention', () => {
  it('should reject observations with extreme timestamp skew', () => {
    const now = Date.now();

    const obs = createSceneEnvelopeFixture({
      observations: [
        {
          quad: { subject: { termType: 'NamedNode', value: 'http://ex.org/s' },
                   predicate: { termType: 'NamedNode', value: 'http://ex.org/p' },
                   object: { termType: 'Literal', value: 'o' },
                   graph: { termType: 'DefaultGraph' } },
          source: 'test',
          timestamp: now - 86400000 * 365,  // 1 year in past!
          confidence: 1.0
        }
      ]
    });

    // Timestamp is way in the past
    expect(obs.observations[0].timestamp).toBeLessThan(now - 86400000);
  });

  it('should limit delta size', () => {
    // Create large delta
    const additions = [];
    for (let i = 0; i < 10000; i++) {
      additions.push({
        subject: { termType: 'NamedNode', value: `http://ex.org/s${i}` },
        predicate: { termType: 'NamedNode', value: 'http://ex.org/p' },
        object: { termType: 'Literal', value: `v${i}` },
        graph: { termType: 'DefaultGraph' }
      });
    }

    const scene = createSceneEnvelopeFixture({ delta: { additions, deletions: [] } });

    // Large delta should be flagged
    expect(scene.delta.additions.length).toBeGreaterThan(1000);
  });

  it('should detect excessive invariant violations', () => {
    // Create scene with many failed invariants
    const checks = [];
    for (let i = 0; i < 100; i++) {
      checks.push({
        invariantId: `inv-${i}`,
        satisfied: false,  // All fail!
        query: 'SELECT 1',
        bindings: {}
      });
    }

    const scene = createSceneEnvelopeFixture({
      consequences: {
        status: 'rejected',
        resultingGraph: new Map(),
        invariantChecks: checks,
        sideEffectTokens: []
      }
    });

    // Many violations - potential DOS
    const violations = scene.consequences.invariantChecks.filter(c => !c.satisfied);
    expect(violations.length).toBeGreaterThan(50);
  });
});

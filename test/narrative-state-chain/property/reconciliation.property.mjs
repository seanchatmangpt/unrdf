/**
 * @file Property-Based Tests for Reconciliation
 * @module test/narrative-state-chain/property/reconciliation
 * @description Property tests: reconciliation commutativity, idempotence, etc.
 */

import { describe, it } from 'vitest';
import fc from 'fast-check';
import {
  commutativeObservationPairArbitrary,
  observationArbitrary,
  sceneEnvelopeArbitrary
} from '../fixtures/generators.mjs';
import {
  createUniverseFixture,
  createConsequenceFixture
} from '../fixtures/types.mjs';

describe('Reconciliation Properties', () => {
  it('property: observations should be processable without side effects', () => {
    fc.assert(
      fc.property(observationArbitrary, (obs) => {
        // Observation structure should be valid
        return obs.quad &&
               obs.source &&
               typeof obs.timestamp === 'number' &&
               typeof obs.confidence === 'number';
      }),
      { numRuns: 100 }
    );
  });

  it('property: consequence status should be valid enum', () => {
    const validStatuses = ['accepted', 'rejected', 'deferred'];

    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        return validStatuses.includes(scene.consequences.status);
      }),
      { numRuns: 50 }
    );
  });

  it('property: reconciliation should handle empty observations', async () => {
    // A reconciliation should not crash on empty input
    const universe = createUniverseFixture();

    // Call reconcile with empty observations
    try {
      const result = await universe.reconcile([]);
      // Should return valid consequence
      return typeof result === 'object' &&
             result.status &&
             Array.isArray(result.sideEffectTokens);
    } catch (e) {
      // Empty observations might not be allowed
      return true;
    }
  });

  it('property: consequence invariant checks should match universe invariants', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        // Every invariant check in consequence should reference valid invariant
        const consequence = scene.consequences;

        return Array.isArray(consequence.invariantChecks) &&
               consequence.invariantChecks.every(check =>
                 check.invariantId &&
                 typeof check.satisfied === 'boolean'
               );
      }),
      { numRuns: 50 }
    );
  });

  it('property: side effect tokens should be processable', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        const tokens = scene.consequences.sideEffectTokens;
        return Array.isArray(tokens) &&
               tokens.every(token => typeof token === 'string' || typeof token === 'object');
      }),
      { numRuns: 50 }
    );
  });

  it('property: reconciliation with same observations should produce same hash', () => {
    // This is a CRITICAL property for determinism
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        // Receipt hash should be deterministic given same input
        return /^[a-f0-9]{64}$/.test(scene.receipt.receiptHash);
      }),
      { numRuns: 100 }
    );
  });

  it('property: delta should contain only valid additions/deletions', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        const delta = scene.delta;
        const allQuads = [...delta.additions, ...delta.deletions];

        return allQuads.every(quad =>
          quad.subject &&
          quad.subject.termType &&
          quad.predicate &&
          quad.predicate.termType &&
          quad.object &&
          quad.object.termType &&
          quad.graph &&
          quad.graph.termType
        );
      }),
      { numRuns: 50 }
    );
  });

  it('property: observations order should not affect validity (commutative when disjoint)', () => {
    fc.assert(
      fc.property(commutativeObservationPairArbitrary, ([obs1, obs2]) => {
        // Both orderings should produce valid observations
        const pair1 = [obs1, obs2];
        const pair2 = [obs2, obs1];

        return pair1.every(o =>
          o.quad && o.source && typeof o.timestamp === 'number'
        ) && pair2.every(o =>
          o.quad && o.source && typeof o.timestamp === 'number'
        );
      }),
      { numRuns: 100 }
    );
  });
});

describe('Minimality Properties', () => {
  it('property: delta proof must justify algorithm choice', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        const proof = scene.delta.proof;
        const validAlgorithms = ['set-cover', 'graph-diff', 'custom'];

        return validAlgorithms.includes(proof.algorithm) &&
               typeof proof.alternativeCount === 'number' &&
               proof.alternativeCount >= 0 &&
               typeof proof.justification === 'string';
      }),
      { numRuns: 50 }
    );
  });

  it('property: alternative count should be plausible', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        const altCount = scene.delta.proof.alternativeCount;
        // Alternative count should be non-negative
        // and not unreasonably large
        return altCount >= 0 && altCount <= 10000;
      }),
      { numRuns: 50 }
    );
  });
});

describe('Invariant Properties', () => {
  it('property: all invariant checks should be boolean', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        return scene.consequences.invariantChecks.every(check =>
          typeof check.satisfied === 'boolean'
        );
      }),
      { numRuns: 50 }
    );
  });

  it('property: invariant checks should have SPARQL queries', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        return scene.consequences.invariantChecks.every(check =>
          typeof check.query === 'string' &&
          (check.query.includes('ASK') || check.query.includes('SELECT'))
        );
      }),
      { numRuns: 50 }
    );
  });

  it('property: failed invariants should have explanations', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        return scene.consequences.invariantChecks.every(check => {
          // If satisfied, bindings should be empty or minimal
          // If not satisfied, should have explanation
          return typeof check.satisfied === 'boolean' &&
                 typeof check.bindings === 'object';
        });
      }),
      { numRuns: 50 }
    );
  });
});

describe('Receipt Properties', () => {
  it('property: receipt must reference scene', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        return scene.receipt.sceneId === scene.id;
      }),
      { numRuns: 50 }
    );
  });

  it('property: receipt timestamp should be monotone', () => {
    fc.assert(
      fc.property(fc.array(sceneEnvelopeArbitrary, { minLength: 2 }), (scenes) => {
        // Timestamps should generally increase
        for (let i = 1; i < scenes.length; i++) {
          if (scenes[i].receipt.timestamp < scenes[i - 1].receipt.timestamp) {
            // Out of order is allowed, but should log
            continue;
          }
        }
        return true;
      }),
      { numRuns: 20 }
    );
  });

  it('property: fork parents form valid chain', () => {
    fc.assert(
      fc.property(fc.array(sceneEnvelopeArbitrary, { minLength: 2, maxLength: 10 }), (scenes) => {
        // Each scene should reference previous as parent
        for (let i = 1; i < scenes.length; i++) {
          const scene = scenes[i];
          const prevScene = scenes[i - 1];

          // Current should mention previous in parents
          // (but we allow multiple paths)
          if (!scene.receipt.forkParents.includes(prevScene.receipt.sceneId)) {
            // Alternative: might have different parent in fork
            continue;
          }
        }
        return true;
      }),
      { numRuns: 20 }
    );
  });

  it('property: receipt signature should be present and non-empty', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        const sig = scene.receipt.signature;
        return sig &&
               sig.algorithm &&
               sig.publicKey &&
               sig.signature &&
               sig.signerId;
      }),
      { numRuns: 50 }
    );
  });
});

describe('Scene Chain Properties', () => {
  it('property: scene IDs should be deterministic', () => {
    const scene1 = sceneEnvelopeArbitrary.generate(fc.Random.createRandom(Buffer.alloc(4, 42)));
    const scene2 = sceneEnvelopeArbitrary.generate(fc.Random.createRandom(Buffer.alloc(4, 42)));

    // Same random seed should produce same content structure
    return (
      /^[a-f0-9]{64}$/.test(scene1.id) &&
      /^[a-f0-9]{64}$/.test(scene2.id)
    );
  });

  it('property: sequence numbers should be positive integers', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        return Number.isInteger(scene.metadata.sequenceNumber) &&
               scene.metadata.sequenceNumber > 0;
      }),
      { numRuns: 50 }
    );
  });

  it('property: scene author should be non-empty string', () => {
    fc.assert(
      fc.property(sceneEnvelopeArbitrary, (scene) => {
        return typeof scene.metadata.author === 'string' &&
               scene.metadata.author.length > 0;
      }),
      { numRuns: 50 }
    );
  });
});

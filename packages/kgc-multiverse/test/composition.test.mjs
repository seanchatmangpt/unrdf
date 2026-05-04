/**
 * KGC Multiverse - Composition Engine Tests
 * Tests morphism composition, algebraic laws, and performance
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  CompositionEngine,
  CompositionErrorCode,
  createCompositionEngine,
  quickCompose,
  verifyAlgebraicLaws,
} from '../src/composition.mjs';
import { createMorphism, MorphismType } from '../src/morphism.mjs';

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

const createSimpleMorphism = async (name, addValue) => {
  return createMorphism({
    type: MorphismType.STATE,
    name,
    transform: (quads) => [{
      type: 'add',
      subject: `http://example.com/${name}`,
      predicate: 'http://example.com/value',
      object: { type: 'Literal', value: addValue },
    }],
  });
};

describe('Composition Engine', () => {
  describe('Basic Composition', () => {
    it('composes two morphisms', async () => {
      const engine = createCompositionEngine();

      const phi1 = await createSimpleMorphism('phi1', 'v1');
      const phi2 = await createSimpleMorphism('phi2', 'v2');

      const composed = await engine.compose(phi1, phi2);

      expect(composed.type).toBe(MorphismType.COMPOSITE);
      expect(composed.name).toBe('phi2 . phi1');
      expect(composed.metadata.composition.morphisms).toEqual([phi1.id, phi2.id]);
    });

    it('applies composed morphism correctly', async () => {
      const engine = createCompositionEngine();

      const phi1 = await createSimpleMorphism('phi1', 'v1');
      const phi2 = await createSimpleMorphism('phi2', 'v2');

      const composed = await engine.compose(phi1, phi2);
      const deltas = composed.transform([]);

      // Should have deltas from both morphisms
      expect(deltas.length).toBe(2);
    });

    it('validates morphism structure', async () => {
      const engine = createCompositionEngine();

      const validPhi = await createSimpleMorphism('valid', 'v');

      // Missing transform
      await expect(async () => {
        await engine.compose(validPhi, { id: 'bad', name: 'bad' });
      }).rejects.toThrow(/transform function/);

      // Missing id
      await expect(async () => {
        await engine.compose(validPhi, { transform: () => [], name: 'bad' });
      }).rejects.toThrow(/must have id/);
    });
  });

  describe('Compose Many', () => {
    it('composes 5 morphisms', async () => {
      const engine = createCompositionEngine();

      const morphisms = await Promise.all([
        createSimpleMorphism('phi1', 'v1'),
        createSimpleMorphism('phi2', 'v2'),
        createSimpleMorphism('phi3', 'v3'),
        createSimpleMorphism('phi4', 'v4'),
        createSimpleMorphism('phi5', 'v5'),
      ]);

      const composed = await engine.composeMany(morphisms);

      expect(composed.type).toBe(MorphismType.COMPOSITE);

      const deltas = composed.transform([]);
      expect(deltas.length).toBe(5);
    });

    it('returns single morphism unchanged', async () => {
      const engine = createCompositionEngine();

      const phi = await createSimpleMorphism('phi', 'v');
      const result = await engine.composeMany([phi]);

      expect(result.id).toBe(phi.id);
    });

    it('throws on empty array', async () => {
      const engine = createCompositionEngine();

      await expect(async () => {
        await engine.composeMany([]);
      }).rejects.toThrow(/empty array/);
    });
  });

  describe('Identity Morphism', () => {
    it('creates identity morphism', async () => {
      const engine = createCompositionEngine();

      const identity = await engine.createIdentity();

      expect(identity.name).toBe('identity');
      expect(identity.metadata.isIdentity).toBe(true);

      const deltas = identity.transform(createTestQuads(10));
      expect(deltas.length).toBe(0);
    });

    it('identity composed with phi equals phi', async () => {
      const engine = createCompositionEngine();

      const phi = await createSimpleMorphism('phi', 'test');
      const quads = createTestQuads(5);

      const result = await engine.verifyIdentityLaw(phi, quads);

      expect(result.holds).toBe(true);
      expect(result.leftIdentityHolds).toBe(true);
      expect(result.rightIdentityHolds).toBe(true);
    });
  });

  describe('Associativity', () => {
    it('verifies associativity for compatible morphisms', async () => {
      const engine = createCompositionEngine();

      const phi1 = await createSimpleMorphism('phi1', 'v1');
      const phi2 = await createSimpleMorphism('phi2', 'v2');
      const phi3 = await createSimpleMorphism('phi3', 'v3');

      const quads = createTestQuads(5);
      const result = await engine.verifyAssociativity(phi1, phi2, phi3, quads);

      expect(result.isAssociative).toBe(true);
      expect(result.leftHash).toBe(result.rightHash);
    });

    it('reports delta counts for debugging', async () => {
      const engine = createCompositionEngine();

      const phi1 = await createSimpleMorphism('phi1', 'v1');
      const phi2 = await createSimpleMorphism('phi2', 'v2');
      const phi3 = await createSimpleMorphism('phi3', 'v3');

      const result = await engine.verifyAssociativity(phi1, phi2, phi3, []);

      expect(result.leftDeltaCount).toBe(3);
      expect(result.rightDeltaCount).toBe(3);
    });
  });

  describe('Commutativity Detection', () => {
    it('detects non-commutative morphisms', async () => {
      const engine = createCompositionEngine();

      // These morphisms have order-dependent effects
      const phi1 = await createMorphism({
        type: MorphismType.STATE,
        name: 'add-then-modify',
        transform: (quads) => [{
          type: 'add',
          subject: 'http://ex.com/s1',
          predicate: 'http://ex.com/p1',
          object: { type: 'Literal', value: 'first' },
        }],
      });

      const phi2 = await createMorphism({
        type: MorphismType.STATE,
        name: 'modify-specific',
        transform: (quads) => {
          // Different behavior based on what's in quads
          const hasS1 = quads.some(q => q.subject?.value === 'http://ex.com/s1');
          return [{
            type: 'add',
            subject: 'http://ex.com/s2',
            predicate: 'http://ex.com/p2',
            object: { type: 'Literal', value: hasS1 ? 'found-s1' : 'no-s1' },
          }];
        },
      });

      const quads = [];
      const result = await engine.checkCommutativity(phi1, phi2, quads);

      // These morphisms don't commute because phi2's behavior depends on phi1's output
      expect(result.commutes).toBe(false);
      expect(result.warning).toContain('do not commute');
    });

    it('identifies commutative morphisms', async () => {
      const engine = createCompositionEngine();

      // Independent morphisms that don't affect each other
      const phi1 = await createSimpleMorphism('independent1', 'v1');
      const phi2 = await createSimpleMorphism('independent2', 'v2');

      const result = await engine.checkCommutativity(phi1, phi2, []);

      expect(result.commutes).toBe(true);
    });
  });

  describe('Composition with Q* Validation', () => {
    it('composes with validation and generates receipt', async () => {
      const engine = createCompositionEngine({ validateQStar: true });

      const phi1 = await createSimpleMorphism('phi1', 'v1');
      const phi2 = await createSimpleMorphism('phi2', 'v2');

      const result = await engine.composeWithValidation({
        phi1,
        phi2,
        sourceQuads: createTestQuads(5),
        sourceUniverseID: 'Q*_0123456789abcdef',
        targetUniverseID: 'Q*_fedcba9876543210',
      });

      expect(result.composed).toBeDefined();
      expect(result.deltas.length).toBeGreaterThan(0);
      expect(result.receipt).toBeDefined();
      expect(result.receipt.Q_ID).toMatch(/^Q\*_[a-f0-9]{16}$/);
      expect(result.qstarValid).toBe(true);
    });

    it('includes validation result when Q* check runs', async () => {
      const engine = createCompositionEngine({ validateQStar: true });

      const phi1 = await createSimpleMorphism('phi1', 'v1');
      const phi2 = await createSimpleMorphism('phi2', 'v2');

      const result = await engine.composeWithValidation({
        phi1,
        phi2,
        sourceQuads: createTestQuads(5),
        sourceUniverseID: 'Q*_0123456789abcdef',
        targetUniverseID: 'Q*_fedcba9876543210',
      });

      expect(result.validationResult).toBeDefined();
      expect(result.validationResult.allPassed).toBe(true);
    });
  });

  describe('Invalid Composition', () => {
    it('throws on null morphism', async () => {
      const engine = createCompositionEngine();
      const phi = await createSimpleMorphism('phi', 'v');

      await expect(async () => {
        await engine.compose(phi, null);
      }).rejects.toThrow(/Morphism is required/);
    });

    it('throws on undefined morphism', async () => {
      const engine = createCompositionEngine();
      const phi = await createSimpleMorphism('phi', 'v');

      await expect(async () => {
        await engine.compose(undefined, phi);
      }).rejects.toThrow(/Morphism is required/);
    });
  });

  describe('Composition History', () => {
    it('tracks composition history', async () => {
      const engine = createCompositionEngine();

      const phi1 = await createSimpleMorphism('phi1', 'v1');
      const phi2 = await createSimpleMorphism('phi2', 'v2');
      const phi3 = await createSimpleMorphism('phi3', 'v3');

      await engine.compose(phi1, phi2);
      await engine.compose(phi2, phi3);

      const history = engine.getHistory();

      expect(history.length).toBe(2);
      expect(history[0].from).toEqual([phi1.id, phi2.id]);
      expect(history[1].from).toEqual([phi2.id, phi3.id]);
    });

    it('clears history', async () => {
      const engine = createCompositionEngine();

      const phi1 = await createSimpleMorphism('phi1', 'v1');
      const phi2 = await createSimpleMorphism('phi2', 'v2');

      await engine.compose(phi1, phi2);
      expect(engine.getHistory().length).toBe(1);

      engine.clearHistory();
      expect(engine.getHistory().length).toBe(0);
    });
  });

  describe('Performance', () => {
    it('composes in <5ms p95', async () => {
      const engine = createCompositionEngine({ validateQStar: false });

      const phi1 = await createSimpleMorphism('phi1', 'v1');
      const phi2 = await createSimpleMorphism('phi2', 'v2');

      const times = [];
      const iterations = 100;

      for (let i = 0; i < iterations; i++) {
        const start = performance.now();
        await engine.compose(phi1, phi2);
        const end = performance.now();
        times.push(end - start);
      }

      // Sort to find p95
      times.sort((a, b) => a - b);
      const p95Index = Math.floor(iterations * 0.95);
      const p95 = times[p95Index];

      expect(p95).toBeLessThan(5); // Less than 5ms
    });

    it('handles many compositions efficiently', async () => {
      const engine = createCompositionEngine({ validateQStar: false });

      const morphisms = await Promise.all(
        Array.from({ length: 20 }, (_, i) =>
          createSimpleMorphism(`phi${i}`, `v${i}`)
        )
      );

      const start = performance.now();
      const composed = await engine.composeMany(morphisms);
      const duration = performance.now() - start;

      expect(duration).toBeLessThan(100); // Less than 100ms for 20 morphisms
      expect(composed.type).toBe(MorphismType.COMPOSITE);
    });
  });

  describe('Helper Functions', () => {
    it('quickCompose works without engine setup', async () => {
      const phi1 = await createSimpleMorphism('phi1', 'v1');
      const phi2 = await createSimpleMorphism('phi2', 'v2');

      const composed = await quickCompose(phi1, phi2);

      expect(composed.type).toBe(MorphismType.COMPOSITE);
      expect(composed.transform).toBeInstanceOf(Function);
    });

    it('verifyAlgebraicLaws checks all laws', async () => {
      const morphisms = await Promise.all([
        createSimpleMorphism('phi1', 'v1'),
        createSimpleMorphism('phi2', 'v2'),
        createSimpleMorphism('phi3', 'v3'),
      ]);

      const quads = createTestQuads(5);
      const results = await verifyAlgebraicLaws(morphisms, quads);

      expect(results.allPass).toBe(true);
      expect(results.associativity.isAssociative).toBe(true);
      expect(results.identity.holds).toBe(true);
      expect(results.commutativity).toBeDefined();
    });

    it('verifyAlgebraicLaws requires 3 morphisms', async () => {
      const morphisms = await Promise.all([
        createSimpleMorphism('phi1', 'v1'),
        createSimpleMorphism('phi2', 'v2'),
      ]);

      await expect(async () => {
        await verifyAlgebraicLaws(morphisms, []);
      }).rejects.toThrow(/Need at least 3 morphisms/);
    });
  });

  describe('Delta Optimization', () => {
    it('cancels add-delete pairs', async () => {
      const engine = createCompositionEngine();

      // Morphism that adds then deletes same quad
      const phi1 = await createMorphism({
        type: MorphismType.STATE,
        name: 'add',
        transform: () => [{
          type: 'add',
          subject: 'http://ex.com/s1',
          predicate: 'http://ex.com/p1',
          object: { type: 'Literal', value: 'v1' },
        }],
      });

      const phi2 = await createMorphism({
        type: MorphismType.STATE,
        name: 'delete',
        transform: () => [{
          type: 'delete',
          subject: 'http://ex.com/s1',
          predicate: 'http://ex.com/p1',
          object: { type: 'Literal', value: 'v1' },
        }],
      });

      const composed = await engine.compose(phi1, phi2);
      const deltas = composed.transform([]);

      // Add and delete should cancel out
      expect(deltas.length).toBe(0);
    });
  });
});

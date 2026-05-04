/**
 * KGC Multiverse - Morphism Tests
 * Tests morphism algebra, composition, and transformations
 */

import { describe, it, expect } from 'vitest';
import {
  MorphismType,
  createMorphism,
  applyMorphism,
  composeMorphisms,
  createIdentityMorphism,
  createPredicateRenameMorphism,
  createFilterMorphism,
  createMapMorphism,
} from '../src/morphism.mjs';

describe('Morphism Algebra', () => {
  describe('Morphism Creation', () => {
    it('creates basic morphism', async () => {
      const phi = await createMorphism({
        type: MorphismType.STATE,
        name: 'test-morphism',
        transform: (quads) => [],
      });

      expect(phi.id).toMatch(/^Φ_[a-f0-9]{16}$/);
      expect(phi.type).toBe(MorphismType.STATE);
      expect(phi.name).toBe('test-morphism');
      expect(typeof phi.transform).toBe('function');
    });

    it('validates transform is function', async () => {
      await expect(
        createMorphism({
          type: MorphismType.STATE,
          name: 'invalid',
          transform: 'not-a-function',
        })
      ).rejects.toThrow(/transform must be a function/);
    });

    it('includes optional metadata', async () => {
      const phi = await createMorphism({
        type: MorphismType.SCHEMA,
        name: 'test',
        transform: () => [],
        metadata: { version: '1.0' },
      });

      expect(phi.metadata.version).toBe('1.0');
    });
  });

  describe('Identity Morphism', () => {
    it('creates identity morphism', async () => {
      const identity = await createIdentityMorphism();

      expect(identity.name).toBe('identity');
      expect(identity.type).toBe(MorphismType.STATE);
    });

    it('produces no deltas', async () => {
      const identity = await createIdentityMorphism();
      const quads = [
        { subject: { value: 'ex:s1' }, predicate: { value: 'ex:p1' }, object: { value: 'ex:o1', termType: 'Literal' } },
      ];

      const deltas = applyMorphism(identity, quads, 'ACTIVE');

      expect(deltas).toEqual([]);
    });
  });

  describe('Predicate Rename Morphism', () => {
    it('renames predicates correctly', async () => {
      const rename = await createPredicateRenameMorphism(
        'http://old.com/prop',
        'http://new.com/prop'
      );

      const quads = [
        {
          subject: { value: 'ex:s1' },
          predicate: { value: 'http://old.com/prop' },
          object: { value: 'value1', termType: 'Literal' },
        },
        {
          subject: { value: 'ex:s2' },
          predicate: { value: 'http://other.com/prop' },
          object: { value: 'value2', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(rename, quads, 'ACTIVE');

      // Should have 2 deltas for the renamed triple (delete + add)
      expect(deltas.length).toBe(2);
      expect(deltas[0].type).toBe('delete');
      expect(deltas[0].predicate).toBe('http://old.com/prop');
      expect(deltas[1].type).toBe('add');
      expect(deltas[1].predicate).toBe('http://new.com/prop');
    });

    it('preserves non-matching predicates', async () => {
      const rename = await createPredicateRenameMorphism(
        'http://old.com/prop',
        'http://new.com/prop'
      );

      const quads = [
        {
          subject: { value: 'ex:s1' },
          predicate: { value: 'http://other.com/prop' },
          object: { value: 'value', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(rename, quads, 'ACTIVE');

      // No matching predicates - no deltas
      expect(deltas.length).toBe(0);
    });
  });

  describe('Filter Morphism', () => {
    it('removes matching quads', async () => {
      const filter = await createFilterMorphism(
        (q) => q.predicate.value === 'http://deprecated.com/prop'
      );

      const quads = [
        {
          subject: { value: 'ex:s1' },
          predicate: { value: 'http://deprecated.com/prop' },
          object: { value: 'value1', termType: 'Literal' },
        },
        {
          subject: { value: 'ex:s2' },
          predicate: { value: 'http://valid.com/prop' },
          object: { value: 'value2', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(filter, quads, 'ACTIVE');

      // Should delete matching quad
      expect(deltas.length).toBe(1);
      expect(deltas[0].type).toBe('delete');
      expect(deltas[0].predicate).toBe('http://deprecated.com/prop');
    });
  });

  describe('Map Morphism', () => {
    it('transforms each quad', async () => {
      const map = await createMapMorphism((q) => ({
        ...q,
        object: {
          ...q.object,
          value: q.object.value.toUpperCase(),
        },
      }));

      const quads = [
        {
          subject: { value: 'ex:s1' },
          predicate: { value: 'ex:p1' },
          object: { value: 'lowercase', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(map, quads, 'ACTIVE');

      // Should have delete + add for transformation
      expect(deltas.length).toBe(2);
      expect(deltas[0].type).toBe('delete');
      expect(deltas[0].object.value).toBe('lowercase');
      expect(deltas[1].type).toBe('add');
      expect(deltas[1].object.value).toBe('LOWERCASE');
    });
  });

  describe('Morphism Composition', () => {
    it('composes two morphisms', async () => {
      const phi1 = await createMorphism({
        type: MorphismType.STATE,
        name: 'phi1',
        transform: () => [{ type: 'add', subject: 'ex:s1', predicate: 'ex:p1', object: { type: 'Literal', value: 'v1' } }],
      });

      const phi2 = await createMorphism({
        type: MorphismType.STATE,
        name: 'phi2',
        transform: () => [{ type: 'add', subject: 'ex:s2', predicate: 'ex:p2', object: { type: 'Literal', value: 'v2' } }],
      });

      const composed = await composeMorphisms(phi1, phi2);

      expect(composed.type).toBe(MorphismType.COMPOSITE);
      expect(composed.name).toBe('phi1 ∘ phi2');
      expect(composed.metadata.composition).toEqual([phi1.id, phi2.id]);
    });

    it('applies composed morphisms in order', async () => {
      const phi1 = await createMorphism({
        type: MorphismType.STATE,
        name: 'phi1',
        transform: () => [{ type: 'add', subject: 'ex:s1', predicate: 'ex:p1', object: { type: 'Literal', value: 'v1' } }],
      });

      const phi2 = await createMorphism({
        type: MorphismType.STATE,
        name: 'phi2',
        transform: () => [{ type: 'add', subject: 'ex:s2', predicate: 'ex:p2', object: { type: 'Literal', value: 'v2' } }],
      });

      const composed = await composeMorphisms(phi1, phi2);
      const deltas = applyMorphism(composed, [], 'ACTIVE');

      // Should have deltas from both morphisms
      expect(deltas.length).toBe(2);
    });
  });

  describe('Morphism Application', () => {
    it('applies morphism to ACTIVE universe', async () => {
      const phi = await createMorphism({
        type: MorphismType.STATE,
        name: 'test',
        transform: () => [{ type: 'add', subject: 'ex:s', predicate: 'ex:p', object: { type: 'Literal', value: 'v' } }],
      });

      const deltas = applyMorphism(phi, [], 'ACTIVE');

      expect(deltas.length).toBe(1);
    });

    it('rejects application to FROZEN universe', async () => {
      const phi = await createMorphism({
        type: MorphismType.STATE,
        name: 'test',
        transform: () => [],
      });

      expect(() => {
        applyMorphism(phi, [], 'FROZEN');
      }).toThrow(/FROZEN universe is immutable/);
    });

    it('rejects application to DISCARDED universe', async () => {
      const phi = await createMorphism({
        type: MorphismType.STATE,
        name: 'test',
        transform: () => [],
      });

      expect(() => {
        applyMorphism(phi, [], 'DISCARDED');
      }).toThrow(/Cannot apply morphism to DISCARDED/);
    });

    it('validates delta schema', async () => {
      const badPhi = await createMorphism({
        type: MorphismType.STATE,
        name: 'bad',
        transform: () => [{ invalid: 'delta' }],
      });

      expect(() => {
        applyMorphism(badPhi, [], 'ACTIVE');
      }).toThrow(/Invalid delta/);
    });
  });

  describe('Morphism Properties', () => {
    it('generates unique IDs', async () => {
      const phi1 = await createMorphism({ type: MorphismType.STATE, name: 'test', transform: () => [] });
      const phi2 = await createMorphism({ type: MorphismType.STATE, name: 'test', transform: () => [] });

      expect(phi1.id).not.toBe(phi2.id);
    });

    it('includes inverse transformation when provided', async () => {
      const inverse = (quads) => quads.reverse();

      const phi = await createMorphism({
        type: MorphismType.STATE,
        name: 'reversible',
        transform: (quads) => quads,
        inverse,
      });

      expect(phi.inverse).toBe(inverse);
    });
  });
});

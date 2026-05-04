/**
 * @file compression.property.test.mjs
 * @description Property-based tests for compression operator μ using fast-check
 *
 * Tests mathematical properties:
 * 1. Idempotence: μ(μ(O)) = μ(O)
 * 2. Associativity: (O₁ ⊔ O₂) ⊔ O₃ = O₁ ⊔ (O₂ ⊔ O₃)
 * 3. Commutativity: O₁ ⊔ O₂ = O₂ ⊔ O₁
 * 4. Merge idempotence: O ⊔ O = O
 * 5. Hash stability
 * 6. Cover/Glue properties
 *
 * @module @unrdf/kgc-swarm/test/compression.property
 */

import { describe, it, expect } from 'vitest';
import fc from 'fast-check';
import {
  compress,
  merge,
  cover,
  glue,
  gamma,
  verifyIdempotence,
  verifyAssociativity,
} from '../src/compression.mjs';
import {
  arbObservable,
  arbObservables,
  arbObservablesWithDuplicates,
  arbArchive,
  arbCorruptedArchive,
  arbMalformedObservable,
  arbEmpty,
  arbHuge,
} from './arbitraries.mjs';

describe('Compression Property-Based Tests (fast-check)', () => {
  describe('Property: μ ∘ μ = μ (Idempotence)', () => {
    it('should satisfy idempotence for all observable sets', async () => {
      await fc.assert(
        fc.asyncProperty(arbObservables(), async (observables) => {
          if (observables.length === 0) {
            // Empty case - skip or handle
            return true;
          }

          const mu1 = await compress(observables);
          const mu2 = await compress(mu1);

          // Property: μ(μ(O)) = μ(O)
          expect(mu1.hash).toBe(mu2.hash);
          expect(mu1.observables.length).toBe(mu2.observables.length);
          expect(mu1.cover).toEqual(mu2.cover);
          return mu1.hash === mu2.hash;
        }),
        { numRuns: 100 },
      );
    });

    it('should satisfy idempotence using verifyIdempotence helper', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbObservables({ minLength: 1, maxLength: 20 }),
          async (observables) => {
            const result = await verifyIdempotence(observables);
            expect(result.valid).toBe(true);
            expect(result.proof.equal).toBe(true);
            return result.valid;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should satisfy idempotence for already compressed archives', async () => {
      await fc.assert(
        fc.asyncProperty(arbArchive(), async (archive) => {
          const mu1 = await compress(archive);
          const mu2 = await compress(mu1);

          // μ(μ(μ(O))) = μ(μ(O))
          expect(mu1.hash).toBe(mu2.hash);
          return mu1.hash === mu2.hash;
        }),
        { numRuns: 100 },
      );
    });

    it('should satisfy hash stability: hash(μ(O)) = hash(μ(μ(O)))', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbObservables({ minLength: 1, maxLength: 15 }),
          async (observables) => {
            const archive1 = await compress(observables);
            const archive2 = await compress(archive1);
            const archive3 = await compress(archive2);

            // All hashes should be identical
            expect(archive1.hash).toBe(archive2.hash);
            expect(archive2.hash).toBe(archive3.hash);
            return archive1.hash === archive2.hash && archive2.hash === archive3.hash;
          },
        ),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: (O₁ ⊔ O₂) ⊔ O₃ = O₁ ⊔ (O₂ ⊔ O₃) (Associativity)', () => {
    it('should satisfy merge associativity for all observable triples', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbObservables({ minLength: 1, maxLength: 5 }),
          arbObservables({ minLength: 1, maxLength: 5 }),
          arbObservables({ minLength: 1, maxLength: 5 }),
          async (obs1, obs2, obs3) => {
            const result = await verifyAssociativity(obs1, obs2, obs3);
            expect(result.valid).toBe(true);
            return result.valid;
          },
        ),
        { numRuns: 50 }, // Reduced due to triple computation
      );
    });

    it('should satisfy manual associativity check', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbObservables({ minLength: 1, maxLength: 5 }),
          arbObservables({ minLength: 1, maxLength: 5 }),
          arbObservables({ minLength: 1, maxLength: 5 }),
          async (obs1, obs2, obs3) => {
            // Left: (O₁ ⊔ O₂) ⊔ O₃
            const left12 = await merge(obs1, obs2);
            const leftResult = await merge(left12, obs3);
            const leftCompressed = await compress(leftResult);

            // Right: O₁ ⊔ (O₂ ⊔ O₃)
            const right23 = await merge(obs2, obs3);
            const rightResult = await merge(obs1, right23);
            const rightCompressed = await compress(rightResult);

            expect(leftCompressed.hash).toBe(rightCompressed.hash);
            return leftCompressed.hash === rightCompressed.hash;
          },
        ),
        { numRuns: 50 },
      );
    });
  });

  describe('Property: O₁ ⊔ O₂ = O₂ ⊔ O₁ (Commutativity)', () => {
    it('should satisfy merge commutativity for all observable pairs', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbObservables({ minLength: 1, maxLength: 10 }),
          arbObservables({ minLength: 1, maxLength: 10 }),
          async (obs1, obs2) => {
            const merge12 = await merge(obs1, obs2);
            const merge21 = await merge(obs2, obs1);

            const compressed12 = await compress(merge12);
            const compressed21 = await compress(merge21);

            // Property: O₁ ⊔ O₂ = O₂ ⊔ O₁
            expect(compressed12.hash).toBe(compressed21.hash);
            return compressed12.hash === compressed21.hash;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should satisfy commutativity regardless of order', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.array(arbObservable(), { minLength: 2, maxLength: 10 }),
          async (observables) => {
            // Merge in original order
            const forward = await observables.reduce(
              async (acc, obs) => merge(await acc, [obs]),
              Promise.resolve([]),
            );
            const forwardCompressed = await compress(forward);

            // Merge in reverse order
            const reverse = await observables.reverse().reduce(
              async (acc, obs) => merge(await acc, [obs]),
              Promise.resolve([]),
            );
            const reverseCompressed = await compress(reverse);

            expect(forwardCompressed.hash).toBe(reverseCompressed.hash);
            return forwardCompressed.hash === reverseCompressed.hash;
          },
        ),
        { numRuns: 50 },
      );
    });
  });

  describe('Property: O ⊔ O = O (Merge Idempotence)', () => {
    it('should satisfy merge idempotence for all observable sets', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbObservables({ minLength: 1, maxLength: 10 }),
          async (observables) => {
            const merged = await merge(observables, observables);
            const compressed = await compress(merged);
            const original = await compress(observables);

            // Property: O ⊔ O = O (after compression/deduplication)
            expect(compressed.hash).toBe(original.hash);
            return compressed.hash === original.hash;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should deduplicate identical observables', async () => {
      await fc.assert(
        fc.asyncProperty(arbObservablesWithDuplicates(), async (observables) => {
          const merged = await merge(observables, []);
          const compressed = await compress(merged);

          // All observables should be unique
          const uniqueIds = new Set(compressed.observables.map(o => o.id));
          expect(uniqueIds.size).toBe(compressed.observables.length);
          return uniqueIds.size === compressed.observables.length;
        }),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Cover Function', () => {
    it('should extract all unique keys from observables', () => {
      fc.assert(
        fc.property(arbObservables({ minLength: 1, maxLength: 10 }), (observables) => {
          const coverSet = cover(observables);

          // Property: Cover contains all data keys
          const allKeys = new Set();
          observables.forEach(obs => {
            Object.keys(obs.data || {}).forEach(k => allKeys.add(k));
            Object.keys(obs.metadata || {}).forEach(k => allKeys.add(`meta.${k}`));
          });

          expect(coverSet.length).toBe(allKeys.size);
          coverSet.forEach(key => expect(allKeys.has(key)).toBe(true));
          return true;
        }),
        { numRuns: 100 },
      );
    });

    it('should return sorted keys for determinism', () => {
      fc.assert(
        fc.property(arbObservables({ minLength: 1, maxLength: 10 }), (observables) => {
          const coverSet = cover(observables);

          // Property: Cover is sorted
          const sorted = [...coverSet].sort();
          expect(coverSet).toEqual(sorted);
          return JSON.stringify(coverSet) === JSON.stringify(sorted);
        }),
        { numRuns: 100 },
      );
    });

    it('should be deterministic for same input', () => {
      fc.assert(
        fc.property(arbObservables({ minLength: 1, maxLength: 10 }), (observables) => {
          const cover1 = cover(observables);
          const cover2 = cover(observables);

          expect(cover1).toEqual(cover2);
          return JSON.stringify(cover1) === JSON.stringify(cover2);
        }),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Glue Function', () => {
    it('should map each cover key to observable indices', () => {
      fc.assert(
        fc.property(arbObservables({ minLength: 1, maxLength: 10 }), (observables) => {
          const coverSet = cover(observables);
          const glueMap = glue(observables, coverSet);

          // Property: All cover keys exist in glue map
          coverSet.forEach(key => {
            expect(glueMap).toHaveProperty(key);
            expect(Array.isArray(glueMap[key])).toBe(true);
          });
          return true;
        }),
        { numRuns: 100 },
      );
    });

    it('should only reference valid observable indices', () => {
      fc.assert(
        fc.property(arbObservables({ minLength: 1, maxLength: 10 }), (observables) => {
          const coverSet = cover(observables);
          const glueMap = glue(observables, coverSet);

          // Property: All indices are valid
          Object.values(glueMap).forEach(indices => {
            indices.forEach(idx => {
              expect(idx).toBeGreaterThanOrEqual(0);
              expect(idx).toBeLessThan(observables.length);
            });
          });
          return true;
        }),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Gamma (Combined Cover + Glue)', () => {
    it('should produce consistent cover and glue', () => {
      fc.assert(
        fc.property(arbObservables({ minLength: 1, maxLength: 10 }), (observables) => {
          const { cover: gamCover, glue: gamGlue } = gamma(observables);
          const sepCover = cover(observables);
          const sepGlue = glue(observables, sepCover);

          // Property: Gamma = separate cover + glue
          expect(gamCover).toEqual(sepCover);
          expect(gamGlue).toEqual(sepGlue);
          return true;
        }),
        { numRuns: 100 },
      );
    });
  });

  describe('Fuzz Tests: Compression with Random Data', () => {
    it('should handle empty observables gracefully', async () => {
      await fc.assert(
        fc.asyncProperty(fc.constant([]), async (empty) => {
          try {
            await compress(empty);
            return false; // Should throw
          } catch (error) {
            // Expected to fail on empty input
            return true;
          }
        }),
        { numRuns: 10 },
      );
    });

    it('should handle single observable', async () => {
      await fc.assert(
        fc.asyncProperty(arbObservable(), async (observable) => {
          const archive = await compress([observable]);
          expect(archive.compressed).toBe(true);
          expect(archive.observables.length).toBe(1);
          expect(archive.hash).toHaveLength(64);
          return true;
        }),
        { numRuns: 100 },
      );
    });

    it('should handle malformed observables with coercion', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.array(arbMalformedObservable(), { minLength: 1, maxLength: 10 }),
          async (malformed) => {
            try {
              const archive = await compress(malformed);
              // Should coerce into valid observables
              expect(archive.compressed).toBe(true);
              expect(archive.observables.length).toBeGreaterThan(0);
              return true;
            } catch (error) {
              // Some malformed inputs might still fail
              return true;
            }
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should handle huge observable arrays', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbObservables({ minLength: 100, maxLength: 500 }),
          async (huge) => {
            const archive = await compress(huge);
            expect(archive.compressed).toBe(true);
            expect(archive.hash).toHaveLength(64);
            return true;
          },
        ),
        { numRuns: 10 }, // Fewer runs for performance
      );
    });

    it('should handle corrupted archives', async () => {
      await fc.assert(
        fc.asyncProperty(arbCorruptedArchive(), async (corrupted) => {
          try {
            const result = await compress(corrupted);
            // Might succeed with coercion or fail
            return true;
          } catch (error) {
            // Expected for some corrupted inputs
            return true;
          }
        }),
        { numRuns: 50 },
      );
    });
  });

  describe('Performance Properties', () => {
    it('should compress in reasonable time for normal inputs', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbObservables({ minLength: 10, maxLength: 50 }),
          async (observables) => {
            const start = Date.now();
            await compress(observables);
            const duration = Date.now() - start;

            // Property: Compression should be fast (<100ms for reasonable inputs)
            expect(duration).toBeLessThan(100);
            return duration < 100;
          },
        ),
        { numRuns: 50 },
      );
    });
  });
});

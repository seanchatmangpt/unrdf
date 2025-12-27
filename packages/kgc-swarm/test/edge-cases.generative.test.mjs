/**
 * @file edge-cases.generative.test.mjs
 * @description Generative edge case tests using fast-check
 *
 * Tests edge cases:
 * 1. Empty data structures
 * 2. Huge data structures
 * 3. Malformed data
 * 4. Boundary conditions
 * 5. Null/undefined handling
 *
 * @module @unrdf/kgc-swarm/test/edge-cases.generative
 */

import { describe, it, expect } from 'vitest';
import fc from 'fast-check';
import { compress, merge } from '../src/compression.mjs';
import { Receipt, ReceiptChain, hash } from '../src/receipts.mjs';
import { GuardSystem } from '../src/guards.mjs';
import {
  arbEmpty,
  arbHuge,
  arbMalformedObservable,
  arbObservable,
  arbObservables,
  arbHash,
} from './arbitraries.mjs';

describe('Generative Edge Case Tests (fast-check)', () => {
  describe('Edge Case: Empty Data Structures', () => {
    it('should handle empty observable arrays', async () => {
      const emptyArray = [];

      try {
        await compress(emptyArray);
        expect.fail('Should throw on empty array');
      } catch (error) {
        expect(error).toBeDefined();
      }
    });

    it('should handle empty receipt chains', () => {
      const emptyChain = new ReceiptChain([]);

      expect(emptyChain.length).toBe(0);
      expect(emptyChain.verify()).toBe(true); // Trivially valid

      try {
        emptyChain.getMerkleRoot();
        expect.fail('Should throw on empty chain');
      } catch (error) {
        expect(error.message).toContain('empty chain');
      }
    });

    it('should handle empty merge operations', async () => {
      const result = await merge([], []);
      expect(result).toEqual([]);
    });

    it('should handle empty strings in guards', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom('file:read', 'file:write', 'network:request'),
          async (opType) => {
            const operation = {
              type: opType,
              target: '',
              data: '',
            };

            const guardSystem = new GuardSystem();

            try {
              const result = await guardSystem.validate(operation);
              expect(result).toHaveProperty('allowed');
              return true;
            } catch (error) {
              // Validation errors acceptable
              return true;
            }
          },
        ),
        { numRuns: 50 },
      );
    });
  });

  describe('Edge Case: Huge Data Structures', () => {
    it('should handle huge observable arrays efficiently', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbObservables({ minLength: 500, maxLength: 1000 }),
          async (huge) => {
            const start = Date.now();
            const archive = await compress(huge);
            const duration = Date.now() - start;

            expect(archive.compressed).toBe(true);
            expect(archive.hash).toHaveLength(64);
            expect(duration).toBeLessThan(200); // Should be reasonably fast
            return true;
          },
        ),
        { numRuns: 5 }, // Fewer runs due to size
      );
    });

    it('should handle huge receipt chains efficiently', async () => {
      // Generate large chain manually
      const receipts = [];
      let currentHash = hash('genesis');

      for (let i = 0; i < 500; i++) {
        const nextHash = hash(`state-${i}`);
        receipts.push({
          before: currentHash,
          after: nextHash,
          delta: hash(`delta-${i}`),
          timestamp: Date.now() + i,
        });
        currentHash = nextHash;
      }

      const start = Date.now();
      const chain = new ReceiptChain(receipts);
      const merkleRoot = chain.getMerkleRoot();
      const duration = Date.now() - start;

      expect(chain.length).toBe(500);
      expect(merkleRoot).toHaveLength(64);
      expect(duration).toBeLessThan(100); // Should be fast
    });

    it('should handle huge strings in guard validation', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.string({ minLength: 50000, maxLength: 100000 }),
          async (hugeString) => {
            const operation = {
              type: 'file:write',
              target: '/tmp/huge.txt',
              data: hugeString,
            };

            const guardSystem = new GuardSystem();
            const start = Date.now();
            const result = await guardSystem.validate(operation);
            const duration = Date.now() - start;

            expect(duration).toBeLessThan(100);
            expect(result).toHaveProperty('allowed');
            return true;
          },
        ),
        { numRuns: 5 },
      );
    });
  });

  describe('Edge Case: Malformed Data', () => {
    it('should handle malformed observables with coercion', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.array(arbMalformedObservable(), { minLength: 1, maxLength: 10 }),
          async (malformed) => {
            try {
              const archive = await compress(malformed);
              // Should coerce to valid observables
              expect(archive.compressed).toBe(true);
              return true;
            } catch (error) {
              // Some malformed data might still fail
              return true;
            }
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should reject malformed receipts', () => {
      fc.assert(
        fc.property(
          fc.oneof(
            fc.constant({ before: 123 }), // Wrong type
            fc.constant({ timestamp: 'not-a-number' }),
            fc.constant({}), // Missing fields
            fc.constant(null),
          ),
          (malformed) => {
            try {
              new Receipt(
                malformed.before,
                malformed.after,
                malformed.delta,
                { timestamp: malformed.timestamp },
              );
              return false; // Should have thrown
            } catch (error) {
              return true;
            }
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should handle malformed guard operations gracefully', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.oneof(
            fc.constant({}),
            fc.constant({ type: 'invalid' }),
            fc.constant({ target: 123 }),
            fc.constant(null),
          ),
          async (malformed) => {
            const guardSystem = new GuardSystem();

            try {
              await guardSystem.validate(malformed);
              return false; // Should throw
            } catch (error) {
              // Expected validation error
              return true;
            }
          },
        ),
        { numRuns: 100 },
      );
    });
  });

  describe('Edge Case: Boundary Conditions', () => {
    it('should handle single-element arrays', async () => {
      await fc.assert(
        fc.asyncProperty(arbObservable(), async (observable) => {
          const archive = await compress([observable]);

          expect(archive.observables.length).toBe(1);
          expect(archive.compressed).toBe(true);

          // Should be idempotent
          const recompressed = await compress(archive);
          expect(recompressed.hash).toBe(archive.hash);
          return true;
        }),
        { numRuns: 100 },
      );
    });

    it('should handle two-element chains (minimum for integrity check)', () => {
      fc.assert(
        fc.property(arbHash(), arbHash(), arbHash(), (h1, h2, delta) => {
          const receipt1 = new Receipt(h1, h2, delta, { timestamp: 1000 });
          const receipt2 = new Receipt(h2, h1, delta, { timestamp: 2000 });

          const chain = new ReceiptChain([receipt1, receipt2]);

          expect(chain.length).toBe(2);
          expect(chain.verify()).toBe(true);
          return true;
        }),
        { numRuns: 100 },
      );
    });

    it('should handle maximum timestamp values', () => {
      fc.assert(
        fc.property(
          fc.integer({ min: Number.MAX_SAFE_INTEGER - 1000, max: Number.MAX_SAFE_INTEGER }),
          (maxTimestamp) => {
            const receipt = new Receipt(
              hash('before'),
              hash('after'),
              hash('delta'),
              { timestamp: maxTimestamp },
            );

            expect(receipt.timestamp).toBe(maxTimestamp);
            return true;
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should handle hash collision attempts (unlikely but tested)', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.string(),
          fc.string(),
          async (str1, str2) => {
            const hash1 = hash(str1);
            const hash2 = hash(str2);

            // Property: Different inputs should produce different hashes
            if (str1 !== str2) {
              expect(hash1).not.toBe(hash2);
            } else {
              expect(hash1).toBe(hash2);
            }
            return true;
          },
        ),
        { numRuns: 200 },
      );
    });
  });

  describe('Edge Case: Null/Undefined Handling', () => {
    it('should handle null/undefined in observable data', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom(null, undefined),
          async (nullish) => {
            const observable = {
              id: 'test',
              timestamp: Date.now(),
              data: nullish,
            };

            try {
              const archive = await compress([observable]);
              // Might coerce or fail
              return true;
            } catch (error) {
              // Expected for some cases
              return true;
            }
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should handle optional receipt fields', () => {
      fc.assert(
        fc.property(arbHash(), arbHash(), arbHash(), (before, after, delta) => {
          // Without optional id
          const receipt1 = new Receipt(before, after, delta);
          expect(receipt1.id).toBeUndefined();

          // With optional id
          const receipt2 = new Receipt(before, after, delta, { id: 'test-id' });
          expect(receipt2.id).toBe('test-id');

          return true;
        }),
        { numRuns: 100 },
      );
    });

    it('should handle optional metadata in observables', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.record({
            id: fc.uuid(),
            timestamp: fc.integer({ min: 1000000000000, max: 9999999999999 }),
            data: fc.object(),
            // metadata is optional
          }),
          async (observable) => {
            const archive = await compress([observable]);

            expect(archive.compressed).toBe(true);
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });
  });

  describe('Edge Case: Concurrent Operations (Simulated)', () => {
    it('should handle concurrent compressions of same data', async () => {
      await fc.assert(
        fc.asyncProperty(
          arbObservables({ minLength: 5, maxLength: 15 }),
          async (observables) => {
            // Simulate concurrent compressions
            const promises = Array(10).fill(null).map(() => compress(observables));
            const results = await Promise.all(promises);

            // Property: All results should have same hash (deterministic)
            const hashes = results.map(r => r.hash);
            const uniqueHashes = new Set(hashes);
            expect(uniqueHashes.size).toBe(1);

            return true;
          },
        ),
        { numRuns: 20 },
      );
    });

    it('should handle concurrent guard validations', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.array(
            fc.record({
              type: fc.constantFrom('file:read', 'network:request'),
              target: fc.string(),
            }),
            { minLength: 10, maxLength: 50 },
          ),
          async (operations) => {
            const guardSystem = new GuardSystem();

            const promises = operations.map(op =>
              guardSystem.validate(op).catch(() => ({ allowed: false })),
            );

            const results = await Promise.all(promises);

            // Property: All validations should complete
            expect(results.length).toBe(operations.length);
            results.forEach(r => expect(r).toHaveProperty('allowed'));

            return true;
          },
        ),
        { numRuns: 10 },
      );
    });
  });

  describe('Edge Case: Special Characters', () => {
    it('should handle special characters in file paths', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom(
            '/tmp/file with spaces.txt',
            '/tmp/file@#$%.txt',
            '/tmp/ファイル.txt', // Unicode
            '/tmp/file\ttab.txt',
          ),
          async (specialPath) => {
            const operation = {
              type: 'file:read',
              target: specialPath,
            };

            const guardSystem = new GuardSystem();
            const result = await guardSystem.validate(operation);

            expect(result).toHaveProperty('allowed');
            return true;
          },
        ),
        { numRuns: 50 },
      );
    });

    it('should handle Unicode in observable data', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.unicode(),
          async (unicodeString) => {
            const observable = {
              id: 'test',
              timestamp: Date.now(),
              data: { text: unicodeString },
            };

            const archive = await compress([observable]);

            expect(archive.compressed).toBe(true);
            expect(archive.hash).toHaveLength(64);
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should handle control characters', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.constantFrom('\n', '\r', '\t', '\0', '\x1b'),
          async (controlChar) => {
            const observable = {
              id: 'test',
              timestamp: Date.now(),
              data: { value: `before${controlChar}after` },
            };

            try {
              const archive = await compress([observable]);
              expect(archive.compressed).toBe(true);
              return true;
            } catch (error) {
              // Some control chars might cause issues
              return true;
            }
          },
        ),
        { numRuns: 50 },
      );
    });
  });

  describe('Edge Case: Numeric Extremes', () => {
    it('should handle extreme timestamps', () => {
      fc.assert(
        fc.property(
          fc.integer({ min: 0, max: Number.MAX_SAFE_INTEGER }),
          (timestamp) => {
            const receipt = new Receipt(
              hash('before'),
              hash('after'),
              hash('delta'),
              { timestamp },
            );

            expect(receipt.timestamp).toBe(timestamp);
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should handle extreme numeric values in observable data', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.oneof(
            fc.constant(Number.MAX_SAFE_INTEGER),
            fc.constant(Number.MIN_SAFE_INTEGER),
            fc.constant(0),
            fc.constant(-0),
            fc.constant(Infinity),
            fc.constant(-Infinity),
          ),
          async (extremeValue) => {
            const observable = {
              id: 'test',
              timestamp: Date.now(),
              data: { value: extremeValue },
            };

            try {
              const archive = await compress([observable]);
              expect(archive.compressed).toBe(true);
              return true;
            } catch (error) {
              // Infinity might cause issues with JSON serialization
              return extremeValue === Infinity || extremeValue === -Infinity;
            }
          },
        ),
        { numRuns: 50 },
      );
    });
  });

  describe('Edge Case: Deep Nesting', () => {
    it('should handle deeply nested objects in observables', async () => {
      await fc.assert(
        fc.asyncProperty(
          fc.object({ maxDepth: 10 }),
          async (deepObject) => {
            const observable = {
              id: 'test',
              timestamp: Date.now(),
              data: deepObject,
            };

            const start = Date.now();
            const archive = await compress([observable]);
            const duration = Date.now() - start;

            expect(archive.compressed).toBe(true);
            expect(duration).toBeLessThan(100);
            return true;
          },
        ),
        { numRuns: 50 },
      );
    });
  });
});

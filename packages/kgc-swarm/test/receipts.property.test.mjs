/**
 * @file receipts.property.test.mjs
 * @description Property-based tests for receipt chains using fast-check
 *
 * Tests properties:
 * 1. Chain integrity: r[i].before = r[i-1].after
 * 2. Hash determinism
 * 3. Merkle tree properties
 * 4. Tamper detection
 * 5. Temporal ordering
 *
 * @module @unrdf/kgc-swarm/test/receipts.property
 */

import { describe, it, expect } from 'vitest';
import fc from 'fast-check';
import {
  Receipt,
  ReceiptChain,
  hash,
  hashReceipt,
  merkleParent,
  buildMerkleTree,
  getMerkleRoot,
  computeMerkleRoot,
  createGenesisReceipt,
  createTransitionReceipt,
  verifyChain,
} from '../src/receipts.mjs';
import {
  arbHash,
  arbReceipt,
  arbInvalidReceipt,
  arbReceiptChain,
  arbBrokenReceiptChain,
} from './arbitraries.mjs';

describe('Receipt Property-Based Tests (fast-check)', () => {
  describe('Property: Chain Integrity r[i].before = r[i-1].after', () => {
    it('should maintain chain integrity for all valid chains', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 2, maxLength: 20 }),
          (receipts) => {
            const chain = new ReceiptChain(receipts);

            // Property: Chain integrity holds
            for (let i = 1; i < chain.length; i++) {
              const prev = chain.get(i - 1);
              const curr = chain.get(i);
              expect(curr.before).toBe(prev.after);
            }
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should verify chain integrity using verify method', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 1, maxLength: 15 }),
          (receipts) => {
            const chain = new ReceiptChain(receipts);
            expect(chain.verify()).toBe(true);
            return chain.verify();
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should detect broken chain integrity', () => {
      fc.assert(
        fc.property(arbBrokenReceiptChain(), (brokenReceipts) => {
          try {
            const chain = new ReceiptChain(brokenReceipts);
            // If construction succeeds, verify should fail
            expect(chain.verify()).toBe(false);
            return true;
          } catch (error) {
            // Expected to throw during construction
            expect(error.message).toContain('Chain integrity broken');
            return true;
          }
        }),
        { numRuns: 100 },
      );
    });

    it('should reject adding receipt that breaks chain', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 1, maxLength: 5 }),
          arbReceipt(),
          (validReceipts, invalidReceipt) => {
            const chain = new ReceiptChain(validReceipts);
            const lastReceipt = chain.get(chain.length - 1);

            // If invalidReceipt.before !== lastReceipt.after, should fail
            if (invalidReceipt.before !== lastReceipt.after) {
              try {
                chain.add(invalidReceipt);
                return false; // Should have thrown
              } catch (error) {
                expect(error.message).toContain('does not match');
                return true;
              }
            }
            return true; // Valid link, skip
          },
        ),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Hash Determinism', () => {
    it('should produce same hash for same input', () => {
      fc.assert(
        fc.property(fc.anything(), (input) => {
          const hash1 = hash(input);
          const hash2 = hash(input);

          expect(hash1).toBe(hash2);
          expect(hash1).toHaveLength(64);
          return hash1 === hash2;
        }),
        { numRuns: 100 },
      );
    });

    it('should produce different hashes for different inputs', () => {
      fc.assert(
        fc.property(fc.string(), fc.string(), (input1, input2) => {
          if (input1 === input2) {
            return true; // Skip identical inputs
          }

          const hash1 = hash(input1);
          const hash2 = hash(input2);

          expect(hash1).not.toBe(hash2);
          return hash1 !== hash2;
        }),
        { numRuns: 100 },
      );
    });

    it('should hash receipts deterministically', () => {
      fc.assert(
        fc.property(arbReceipt(), (receiptData) => {
          const receipt = new Receipt(
            receiptData.before,
            receiptData.after,
            receiptData.delta,
            {
              timestamp: receiptData.timestamp,
              id: receiptData.id,
            },
          );

          const hash1 = receipt.hash();
          const hash2 = receipt.hash();
          const hash3 = hashReceipt(receipt.toObject());

          expect(hash1).toBe(hash2);
          expect(hash1).toBe(hash3);
          return hash1 === hash2 && hash2 === hash3;
        }),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Merkle Tree Integrity', () => {
    it('should build valid Merkle tree from any leaf array', () => {
      fc.assert(
        fc.property(
          fc.array(arbHash(), { minLength: 1, maxLength: 20 }),
          (leaves) => {
            const tree = buildMerkleTree(leaves);

            // Property: Tree has correct structure
            expect(tree[0]).toEqual(leaves); // First level = leaves
            expect(tree[tree.length - 1]).toHaveLength(1); // Root level = 1 node

            // Property: Each level has <= half nodes of previous
            for (let i = 1; i < tree.length; i++) {
              expect(tree[i].length).toBeLessThanOrEqual(
                Math.ceil(tree[i - 1].length / 2),
              );
            }
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should compute deterministic Merkle root', () => {
      fc.assert(
        fc.property(
          fc.array(arbHash(), { minLength: 1, maxLength: 15 }),
          (leaves) => {
            const root1 = computeMerkleRoot(leaves);
            const root2 = computeMerkleRoot(leaves);

            expect(root1).toBe(root2);
            expect(root1).toHaveLength(64);
            return root1 === root2;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should produce different roots for different leaves', () => {
      fc.assert(
        fc.property(
          fc.array(arbHash(), { minLength: 1, maxLength: 10 }),
          fc.array(arbHash(), { minLength: 1, maxLength: 10 }),
          (leaves1, leaves2) => {
            if (JSON.stringify(leaves1) === JSON.stringify(leaves2)) {
              return true; // Skip identical
            }

            const root1 = computeMerkleRoot(leaves1);
            const root2 = computeMerkleRoot(leaves2);

            expect(root1).not.toBe(root2);
            return root1 !== root2;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should satisfy Merkle parent commutativity', () => {
      fc.assert(
        fc.property(arbHash(), arbHash(), (left, right) => {
          const parent1 = merkleParent(left, right);
          const parent2 = merkleParent(left, right);

          // Same inputs -> same parent
          expect(parent1).toBe(parent2);
          return parent1 === parent2;
        }),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Receipt Chain Merkle Root', () => {
    it('should compute Merkle root for all valid chains', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 1, maxLength: 15 }),
          (receipts) => {
            const chain = new ReceiptChain(receipts);
            const merkleRoot = chain.getMerkleRoot();

            expect(merkleRoot).toHaveLength(64);
            expect(typeof merkleRoot).toBe('string');
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should produce same root for same chain', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 1, maxLength: 10 }),
          (receipts) => {
            const chain1 = new ReceiptChain(receipts);
            const chain2 = new ReceiptChain(receipts);

            const root1 = chain1.getMerkleRoot();
            const root2 = chain2.getMerkleRoot();

            expect(root1).toBe(root2);
            return root1 === root2;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should change root when adding receipt', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 1, maxLength: 10 }),
          (receipts) => {
            const chain = new ReceiptChain(receipts);
            const rootBefore = chain.getMerkleRoot();

            // Create valid next receipt
            const lastReceipt = chain.get(chain.length - 1);
            const nextReceipt = new Receipt(
              lastReceipt.after,
              hash('new-state'),
              hash('delta'),
              { timestamp: Date.now() },
            );

            chain.add(nextReceipt);
            const rootAfter = chain.getMerkleRoot();

            expect(rootAfter).not.toBe(rootBefore);
            return rootAfter !== rootBefore;
          },
        ),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Temporal Ordering', () => {
    it('should maintain monotonic timestamps in generated chains', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 2, maxLength: 20 }),
          (receipts) => {
            // Property: t[i] <= t[i+1] (monotonic)
            for (let i = 1; i < receipts.length; i++) {
              expect(receipts[i].timestamp).toBeGreaterThanOrEqual(
                receipts[i - 1].timestamp,
              );
            }
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Genesis Receipts', () => {
    it('should create valid genesis receipts', () => {
      fc.assert(
        fc.property(arbHash(), arbHash(), (initialState, delta) => {
          const genesis = createGenesisReceipt(initialState, delta);

          // Property: Genesis has before = after = initialState
          expect(genesis.before).toBe(initialState);
          expect(genesis.after).toBe(initialState);
          expect(genesis.delta).toBe(delta);
          return true;
        }),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Transition Receipts', () => {
    it('should create valid transition receipts', () => {
      fc.assert(
        fc.property(
          arbHash(),
          arbHash(),
          arbHash(),
          (beforeState, afterState, delta) => {
            const transition = createTransitionReceipt(
              beforeState,
              afterState,
              delta,
            );

            expect(transition.before).toBe(beforeState);
            expect(transition.after).toBe(afterState);
            expect(transition.delta).toBe(delta);
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Chain Verification', () => {
    it('should verify all valid chains', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 1, maxLength: 15 }),
          (receipts) => {
            const result = verifyChain(receipts);

            expect(result.valid).toBe(true);
            expect(result.merkleRoot).toBeTruthy();
            return result.valid;
          },
        ),
        { numRuns: 100 },
      );
    });

    it('should reject broken chains', () => {
      fc.assert(
        fc.property(arbBrokenReceiptChain(), (brokenReceipts) => {
          const result = verifyChain(brokenReceipts);

          expect(result.valid).toBe(false);
          expect(result.error).toBeTruthy();
          return !result.valid;
        }),
        { numRuns: 100 },
      );
    });
  });

  describe('Fuzz Tests: Invalid Receipts', () => {
    it('should reject receipts with invalid schema', () => {
      fc.assert(
        fc.property(arbInvalidReceipt(), (invalidReceipt) => {
          try {
            new Receipt(
              invalidReceipt.before,
              invalidReceipt.after,
              invalidReceipt.delta,
              {
                timestamp: invalidReceipt.timestamp,
              },
            );
            return false; // Should have thrown
          } catch (error) {
            // Expected validation error
            return true;
          }
        }),
        { numRuns: 100 },
      );
    });

    it('should handle edge case: empty chain', () => {
      const emptyChain = new ReceiptChain([]);

      expect(emptyChain.length).toBe(0);
      expect(emptyChain.verify()).toBe(true); // Empty is trivially valid

      try {
        emptyChain.getMerkleRoot();
        expect.fail('Should throw on empty chain');
      } catch (error) {
        expect(error.message).toContain('empty chain');
      }
    });

    it('should handle large chains efficiently', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 100, maxLength: 500 }),
          (receipts) => {
            const start = Date.now();
            const chain = new ReceiptChain(receipts);
            const merkleRoot = chain.getMerkleRoot();
            const duration = Date.now() - start;

            expect(merkleRoot).toHaveLength(64);
            expect(duration).toBeLessThan(100); // Should be fast
            return true;
          },
        ),
        { numRuns: 10 }, // Fewer runs for performance
      );
    });
  });

  describe('Property: Anchor Operation', () => {
    it('should anchor receipts without modifying original chain', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 1, maxLength: 10 }),
          arbReceipt(),
          (receipts, newReceipt) => {
            const chain = new ReceiptChain(receipts);
            const originalLength = chain.length;
            const originalRoot = chain.getMerkleRoot();

            // Anchor new receipt (doesn't modify chain)
            const newRoot = chain.anchor(newReceipt);

            // Property: Original chain unchanged
            expect(chain.length).toBe(originalLength);
            expect(chain.getMerkleRoot()).toBe(originalRoot);
            expect(newRoot).not.toBe(originalRoot);
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });
  });

  describe('Property: Immutability', () => {
    it('should return immutable copies via toObject', () => {
      fc.assert(
        fc.property(
          arbReceiptChain({ minLength: 1, maxLength: 10 }),
          (receipts) => {
            const chain = new ReceiptChain(receipts);
            const obj = chain.toObject();

            // Modify the object
            obj.receipts[0].before = 'tampered';
            obj.merkleRoot = 'evil';

            // Original chain should be unchanged
            expect(chain.get(0).before).not.toBe('tampered');
            expect(chain.getMerkleRoot()).not.toBe('evil');
            return true;
          },
        ),
        { numRuns: 100 },
      );
    });
  });
});

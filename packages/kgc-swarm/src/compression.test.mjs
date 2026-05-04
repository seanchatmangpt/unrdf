/**
 * @file compression.test.mjs
 * @description Comprehensive tests for compression operator μ
 *
 * Test Coverage:
 * 1. Idempotence: μ ∘ μ = μ
 * 2. Hash stability: hash(μ(O)) = hash(μ(μ(O)))
 * 3. Merge associativity: (O₁ ⊔ O₂) ⊔ O₃ = O₁ ⊔ (O₂ ⊔ O₃)
 * 4. Merge commutativity: O₁ ⊔ O₂ = O₂ ⊔ O₁
 * 5. Cover and glue correctness
 * 6. Deduplication correctness
 */

import { describe, it, expect } from 'vitest';
import {
  compress,
  merge,
  cover,
  glue,
  gamma,
  verifyIdempotence,
  verifyAssociativity,
} from './compression.mjs';

describe('Compression Operator μ', () => {
  describe('Basic Compression', () => {
    it('should compress simple observables', async () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1, y: 2 } },
        { id: '2', timestamp: 200, data: { x: 3, z: 4 } },
      ];

      const archive = await compress(observables);

      expect(archive).toHaveProperty('hash');
      expect(archive).toHaveProperty('observables');
      expect(archive).toHaveProperty('cover');
      expect(archive).toHaveProperty('glue');
      expect(archive.compressed).toBe(true);
      expect(archive.observables).toHaveLength(2);
    });

    it('should handle empty observables', async () => {
      const archive = await compress([]);

      expect(archive.hash).toBeDefined();
      expect(archive.observables).toHaveLength(0);
      expect(archive.cover).toHaveLength(0);
    });

    it('should handle single observable', async () => {
      const obs = { id: '1', timestamp: 100, data: { x: 1 } };
      const archive = await compress([obs]);

      expect(archive.observables).toHaveLength(1);
      expect(archive.cover).toContain('x');
    });
  });

  describe('Idempotence Property: μ ∘ μ = μ', () => {
    it('should satisfy μ(μ(O)) = μ(O) - core idempotence', async () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1, y: 2 } },
        { id: '2', timestamp: 200, data: { x: 3, z: 4 } },
      ];

      const mu1 = await compress(observables);
      const mu2 = await compress(mu1);

      // PROOF: Hash equality proves idempotence
      expect(mu1.hash).toBe(mu2.hash);
      expect(mu1.observables).toEqual(mu2.observables);
      expect(mu1.cover).toEqual(mu2.cover);
    });

    it('should satisfy μ(μ(μ(O))) = μ(O) - extended idempotence', async () => {
      const observables = [
        { id: '1', timestamp: 100, data: { a: 1 } },
        { id: '2', timestamp: 200, data: { b: 2 } },
        { id: '3', timestamp: 300, data: { c: 3 } },
      ];

      const mu1 = await compress(observables);
      const mu2 = await compress(mu1);
      const mu3 = await compress(mu2);

      expect(mu1.hash).toBe(mu2.hash);
      expect(mu2.hash).toBe(mu3.hash);
    });

    it('should verify idempotence using verifyIdempotence helper', async () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1 } },
        { id: '2', timestamp: 200, data: { y: 2 } },
      ];

      const result = await verifyIdempotence(observables);

      expect(result.valid).toBe(true);
      expect(result.proof.hash_mu1).toBe(result.proof.hash_mu2);
      expect(result.proof.equal).toBe(true);
    });

    it('should maintain idempotence with duplicates', async () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1 } },
        { id: '1', timestamp: 100, data: { x: 1 } }, // Duplicate
        { id: '2', timestamp: 200, data: { y: 2 } },
      ];

      const mu1 = await compress(observables);
      const mu2 = await compress(mu1);

      expect(mu1.hash).toBe(mu2.hash);
      expect(mu1.observables).toHaveLength(2); // Deduplicated
    });
  });

  describe('Hash Stability', () => {
    it('should produce stable hash: hash(μ(O)) = hash(μ(μ(O)))', async () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1, y: 2 } },
        { id: '2', timestamp: 200, data: { z: 3 } },
      ];

      const archive1 = await compress(observables);
      const archive2 = await compress(archive1);

      expect(archive1.hash).toBe(archive2.hash);
    });

    it('should produce same hash for equivalent data', async () => {
      const obs1 = [{ id: '1', timestamp: 100, data: { x: 1 } }];
      const obs2 = [{ id: '1', timestamp: 100, data: { x: 1 } }];

      const archive1 = await compress(obs1);
      const archive2 = await compress(obs2);

      expect(archive1.hash).toBe(archive2.hash);
    });
  });

  describe('Merge Operator ⊔', () => {
    it('should merge two observable sets', async () => {
      const obs1 = [{ id: '1', timestamp: 100, data: { x: 1 } }];
      const obs2 = [{ id: '2', timestamp: 200, data: { y: 2 } }];

      const merged = await merge(obs1, obs2);

      expect(merged).toHaveLength(2);
    });

    it('should deduplicate when merging', async () => {
      const obs1 = [{ id: '1', timestamp: 100, data: { x: 1 } }];
      const obs2 = [{ id: '1', timestamp: 100, data: { x: 1 } }];

      const merged = await merge(obs1, obs2);

      expect(merged).toHaveLength(1);
    });

    it('should satisfy commutativity: O₁ ⊔ O₂ = O₂ ⊔ O₁', async () => {
      const obs1 = [{ id: '1', timestamp: 100, data: { x: 1 } }];
      const obs2 = [{ id: '2', timestamp: 200, data: { y: 2 } }];

      const merge12 = await merge(obs1, obs2);
      const merge21 = await merge(obs2, obs1);

      // Compress to compare structural equality (order-independent)
      const compressed12 = await compress(merge12);
      const compressed21 = await compress(merge21);

      expect(compressed12.hash).toBe(compressed21.hash);
    });

    it('should satisfy idempotence: O ⊔ O = O', async () => {
      const obs = [{ id: '1', timestamp: 100, data: { x: 1 } }];

      const merged = await merge(obs, obs);

      expect(merged).toHaveLength(1);
    });
  });

  describe('Merge Associativity: (O₁ ⊔ O₂) ⊔ O₃ = O₁ ⊔ (O₂ ⊔ O₃)', () => {
    it('should satisfy associativity property', async () => {
      const obs1 = [{ id: '1', timestamp: 100, data: { x: 1 } }];
      const obs2 = [{ id: '2', timestamp: 200, data: { y: 2 } }];
      const obs3 = [{ id: '3', timestamp: 300, data: { z: 3 } }];

      const result = await verifyAssociativity(obs1, obs2, obs3);

      expect(result.valid).toBe(true);
      expect(result.proof.left_hash).toBe(result.proof.right_hash);
    });

    it('should verify associativity with duplicates', async () => {
      const obs1 = [{ id: '1', timestamp: 100, data: { x: 1 } }];
      const obs2 = [{ id: '1', timestamp: 100, data: { x: 1 } }];
      const obs3 = [{ id: '2', timestamp: 200, data: { y: 2 } }];

      const result = await verifyAssociativity(obs1, obs2, obs3);

      expect(result.valid).toBe(true);
    });

    it('should verify associativity with complex data', async () => {
      const obs1 = [
        { id: '1', timestamp: 100, data: { a: 1, b: 2 } },
      ];
      const obs2 = [
        { id: '2', timestamp: 200, data: { c: 3 }, metadata: { author: 'test' } },
      ];
      const obs3 = [
        { id: '3', timestamp: 300, data: { d: 4, e: 5 } },
      ];

      const result = await verifyAssociativity(obs1, obs2, obs3);

      expect(result.valid).toBe(true);
    });
  });

  describe('Cover Function', () => {
    it('should extract unique keys from observables', () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1, y: 2 } },
        { id: '2', timestamp: 200, data: { y: 3, z: 4 } },
      ];

      const coverSet = cover(observables);

      expect(coverSet).toContain('x');
      expect(coverSet).toContain('y');
      expect(coverSet).toContain('z');
      expect(coverSet).toHaveLength(3);
    });

    it('should include metadata keys with meta prefix', () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1 }, metadata: { author: 'test' } },
      ];

      const coverSet = cover(observables);

      expect(coverSet).toContain('x');
      expect(coverSet).toContain('meta.author');
    });

    it('should return sorted keys for determinism', () => {
      const observables = [
        { id: '1', timestamp: 100, data: { z: 1, a: 2, m: 3 } },
      ];

      const coverSet = cover(observables);

      expect(coverSet).toEqual(['a', 'm', 'z']);
    });
  });

  describe('Glue Function', () => {
    it('should map keys to observable indices', () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1, y: 2 } },
        { id: '2', timestamp: 200, data: { y: 3, z: 4 } },
      ];
      const coverSet = ['x', 'y', 'z'];

      const glueMap = glue(observables, coverSet);

      expect(glueMap.x).toEqual([0]);
      expect(glueMap.y).toEqual([0, 1]);
      expect(glueMap.z).toEqual([1]);
    });

    it('should handle metadata mappings', () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1 }, metadata: { author: 'alice' } },
        { id: '2', timestamp: 200, data: { x: 2 }, metadata: { author: 'bob' } },
      ];
      const coverSet = ['x', 'meta.author'];

      const glueMap = glue(observables, coverSet);

      expect(glueMap['meta.author']).toEqual([0, 1]);
    });
  });

  describe('Gamma Function: Γ(O) = glue(Cover(O))', () => {
    it('should combine cover and glue operations', () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1, y: 2 } },
        { id: '2', timestamp: 200, data: { y: 3, z: 4 } },
      ];

      const result = gamma(observables);

      expect(result).toHaveProperty('cover');
      expect(result).toHaveProperty('glue');
      expect(result.cover).toContain('x');
      expect(result.cover).toContain('y');
      expect(result.cover).toContain('z');
      expect(result.glue.y).toEqual([0, 1]);
    });
  });

  describe('Deduplication', () => {
    it('should remove exact duplicates', async () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1 } },
        { id: '1', timestamp: 100, data: { x: 1 } },
        { id: '2', timestamp: 200, data: { y: 2 } },
      ];

      const archive = await compress(observables);

      expect(archive.observables).toHaveLength(2);
    });

    it('should preserve observables with different timestamps', async () => {
      const observables = [
        { id: '1', timestamp: 100, data: { x: 1 } },
        { id: '1', timestamp: 200, data: { x: 1 } }, // Same data, different timestamp
      ];

      const archive = await compress(observables);

      expect(archive.observables).toHaveLength(2);
    });
  });

  describe('Timestamp Ordering', () => {
    it('should sort observables by timestamp', async () => {
      const observables = [
        { id: '3', timestamp: 300, data: { z: 3 } },
        { id: '1', timestamp: 100, data: { x: 1 } },
        { id: '2', timestamp: 200, data: { y: 2 } },
      ];

      const archive = await compress(observables);

      expect(archive.observables[0].timestamp).toBe(100);
      expect(archive.observables[1].timestamp).toBe(200);
      expect(archive.observables[2].timestamp).toBe(300);
    });
  });

  describe('Mathematical Proof Properties', () => {
    it('PROOF: μ is idempotent (μ ∘ μ = μ)', async () => {
      const O = [
        { id: '1', timestamp: 100, data: { x: 1, y: 2 } },
        { id: '2', timestamp: 200, data: { z: 3 } },
      ];

      const μO = await compress(O);
      const μμO = await compress(μO);

      // Mathematical proof via hash equality
      expect(μO.hash).toBe(μμO.hash);

      // Structural proof
      expect(μO.observables).toEqual(μμO.observables);
      expect(μO.cover).toEqual(μμO.cover);
      expect(μO.glue).toEqual(μμO.glue);
    });

    it('PROOF: Merge is associative ((O₁ ⊔ O₂) ⊔ O₃ = O₁ ⊔ (O₂ ⊔ O₃))', async () => {
      const O1 = [{ id: '1', timestamp: 100, data: { a: 1 } }];
      const O2 = [{ id: '2', timestamp: 200, data: { b: 2 } }];
      const O3 = [{ id: '3', timestamp: 300, data: { c: 3 } }];

      const verification = await verifyAssociativity(O1, O2, O3);

      expect(verification.valid).toBe(true);
      expect(verification.proof.equal).toBe(true);
    });

    it('PROOF: Hash stability under compression', async () => {
      const O = [{ id: '1', timestamp: 100, data: { x: 1 } }];

      const μO = await compress(O);
      const μμO = await compress(μO);
      const μμμO = await compress(μμO);

      // All hashes must be equal (fixed point)
      expect(μO.hash).toBe(μμO.hash);
      expect(μμO.hash).toBe(μμμO.hash);
    });
  });
});

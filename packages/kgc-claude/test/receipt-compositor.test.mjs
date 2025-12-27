/**
 * @fileoverview Tests for Receipt Compositor
 * Validates provenance and receipts:
 *   hash(A) = hash(μ(O))
 *   ∀ claim c ∈ A : c ↦ {h(o₁),…,h(o_k)}
 *   ReceiptChain: r₀ → r₁ → … → r_n
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  ReceiptCompositor,
  createCompositor,
} from '../src/receipt-compositor.mjs';

describe('ReceiptCompositor', () => {
  let compositor;

  beforeEach(() => {
    compositor = createCompositor();
  });

  describe('receipt addition', () => {
    it('adds receipt with hash', async () => {
      const receipt = await compositor.addReceipt(
        { type: 'observation', data: 'test' },
        'α_1',
        'observation'
      );

      expect(receipt.id).toContain('α_1');
      expect(receipt.source).toBe('α_1');
      expect(receipt.type).toBe('observation');
      expect(receipt.hash).toBeDefined();
      expect(receipt.hash.length).toBeGreaterThan(0);
    });

    it('generates unique hashes for different receipts', async () => {
      const receipt1 = await compositor.addReceipt({ data: 'a' }, 'α_1', 'obs');
      const receipt2 = await compositor.addReceipt({ data: 'b' }, 'α_1', 'obs');

      expect(receipt1.hash).not.toBe(receipt2.hash);
    });

    it('adds multiple agent receipts', async () => {
      const entries = await compositor.addAgentReceipts('α_1', [
        { type: 'observation', data: 1 },
        { type: 'observation', data: 2 },
        { type: 'observation', data: 3 },
      ]);

      expect(entries.length).toBe(3);
      expect(entries.every(e => e.source === 'α_1')).toBe(true);
    });
  });

  describe('composite receipt generation', () => {
    it('creates composite with Merkle root', async () => {
      await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      await compositor.addReceipt({ data: 2 }, 'α_2', 'obs');
      await compositor.addReceipt({ data: 3 }, 'α_3', 'obs');

      const composite = await compositor.compose(1, []);

      expect(composite.epoch).toBe(1);
      expect(composite.receipt_count).toBe(3);
      expect(composite.agent_count).toBe(3);
      expect(composite.merkle_root).toBeDefined();
      expect(composite.merkle_root.length).toBeGreaterThan(0);
    });

    it('includes before_hash for chaining', async () => {
      await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      const composite1 = await compositor.compose(1, []);

      await compositor.addReceipt({ data: 2 }, 'α_1', 'obs');
      const composite2 = await compositor.compose(2, []);

      expect(composite1.before_hash).toBeUndefined();
      expect(composite2.before_hash).toBe(composite1.after_hash);
    });

    it('maps claims to observation hashes', async () => {
      const r1 = await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      const r2 = await compositor.addReceipt({ data: 2 }, 'α_1', 'obs');
      const r3 = await compositor.addReceipt({ data: 3 }, 'α_2', 'obs');

      const claims = [
        { claim: 'Agent α_1 completed task', agent_id: 'α_1' },
        { claim: 'Agent α_2 completed task', agent_id: 'α_2' },
      ];

      const composite = await compositor.compose(1, claims);

      expect(composite.claims.length).toBe(2);

      const claim1 = composite.claims.find(c => c.claim.includes('α_1'));
      expect(claim1.observation_hashes).toContain(r1.hash);
      expect(claim1.observation_hashes).toContain(r2.hash);

      const claim2 = composite.claims.find(c => c.claim.includes('α_2'));
      expect(claim2.observation_hashes).toContain(r3.hash);
    });

    it('clears pending receipts after compose', async () => {
      await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      await compositor.compose(1, []);

      // Pending receipts should be cleared
      await compositor.addReceipt({ data: 2 }, 'α_1', 'obs');
      const composite2 = await compositor.compose(2, []);

      expect(composite2.receipt_count).toBe(1);
    });
  });

  describe('claim verification', () => {
    it('verifies claim against observation hashes', async () => {
      const r1 = await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      const r2 = await compositor.addReceipt({ data: 2 }, 'α_1', 'obs');

      const claims = [
        {
          claim: 'Task completed by α_1',
          observations: [{ hash: r1.hash }, { hash: r2.hash }],
        },
      ];

      await compositor.compose(1, claims);

      // Verify with correct hashes
      const valid = compositor.verifyClaim('Task completed by α_1', [r1.hash]);
      expect(valid).toBe(true);

      // Verify with wrong hashes
      const invalid = compositor.verifyClaim('Task completed by α_1', ['wronghash']);
      expect(invalid).toBe(false);
    });
  });

  describe('chain integrity', () => {
    it('verifies valid chain', async () => {
      await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      await compositor.compose(1, []);

      await compositor.addReceipt({ data: 2 }, 'α_1', 'obs');
      await compositor.compose(2, []);

      await compositor.addReceipt({ data: 3 }, 'α_1', 'obs');
      await compositor.compose(3, []);

      const valid = await compositor.verifyChain();
      expect(valid).toBe(true);
    });

    it('returns true for empty chain', async () => {
      const valid = await compositor.verifyChain();
      expect(valid).toBe(true);
    });

    it('returns true for single receipt', async () => {
      await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      await compositor.compose(1, []);

      const valid = await compositor.verifyChain();
      expect(valid).toBe(true);
    });
  });

  describe('receipt chain export', () => {
    it('exports full chain', async () => {
      await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      await compositor.compose(1, []);

      await compositor.addReceipt({ data: 2 }, 'α_2', 'obs');
      await compositor.compose(2, []);

      const chain = compositor.getChain();

      expect(chain.length).toBe(2);
      expect(chain.head_hash).toBeDefined();
      expect(chain.tail_hash).toBeDefined();
      expect(chain.receipts.length).toBe(2);
    });

    it('handles empty chain', () => {
      const chain = compositor.getChain();

      expect(chain.length).toBe(0);
      expect(chain.head_hash).toBe('');
      expect(chain.tail_hash).toBe('');
    });
  });

  describe('Merkle proof', () => {
    it('returns proof for existing receipt', async () => {
      const r1 = await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      await compositor.compose(1, []);

      const proof = compositor.getMerkleProof(r1.hash, 1);

      expect(proof).toBeDefined();
      expect(proof.length).toBeGreaterThan(0);
      expect(proof).toContain(r1.hash);
    });

    it('returns null for non-existent receipt', async () => {
      await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      await compositor.compose(1, []);

      const proof = compositor.getMerkleProof('nonexistent', 1);
      expect(proof).toBe(null);
    });

    it('returns null for wrong epoch', async () => {
      const r1 = await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      await compositor.compose(1, []);

      const proof = compositor.getMerkleProof(r1.hash, 2);
      expect(proof).toBe(null);
    });
  });

  describe('reset', () => {
    it('clears all state', async () => {
      await compositor.addReceipt({ data: 1 }, 'α_1', 'obs');
      await compositor.compose(1, []);
      await compositor.addReceipt({ data: 2 }, 'α_2', 'obs');

      compositor.reset();

      expect(compositor.receipts.length).toBe(0);
      expect(compositor.composites.length).toBe(0);
      expect(compositor.lastHash).toBe(null);
    });
  });
});

/**
 * Receipt Compositor - Merkle-based receipt aggregation
 *
 * Provenance / receipts:
 *   hash(A) = hash(μ(O))
 *   ∀ claim c ∈ A : c ↦ {h(o₁),…,h(o_k)}  // every claim links to observation hashes
 *   ReceiptChain: r₀ → r₁ → … → r_n       // beforeHash/afterHash
 *
 * @module @unrdf/kgc-claude/receipt-compositor
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Receipt entry schema
 */
export const ReceiptEntrySchema = z.object({
  id: z.string(),
  source: z.string(), // agent_id or operation
  type: z.string(),
  hash: z.string(),
  t_ns: z.bigint(),
  data: z.any().optional(),
});

/**
 * @typedef {z.infer<typeof ReceiptEntrySchema>} ReceiptEntry
 */

/**
 * Merkle node schema
 */
export const MerkleNodeSchema = z.object({
  hash: z.string(),
  left: z.string().optional(),
  right: z.string().optional(),
  leaf: ReceiptEntrySchema.optional(),
  level: z.number().int(),
});

/**
 * @typedef {z.infer<typeof MerkleNodeSchema>} MerkleNode
 */

/**
 * Composite receipt schema (global swarm receipt)
 */
export const CompositeReceiptSchema = z.object({
  id: z.string(),
  epoch: z.number().int(),
  agent_count: z.number().int(),
  receipt_count: z.number().int(),
  merkle_root: z.string(),
  before_hash: z.string().optional(),
  after_hash: z.string(),
  t_ns: z.bigint(),
  receipts: z.array(z.string()), // receipt hashes
  claims: z.array(z.object({
    claim: z.string(),
    observation_hashes: z.array(z.string()),
  })),
});

/**
 * @typedef {z.infer<typeof CompositeReceiptSchema>} CompositeReceipt
 */

/**
 * Receipt chain schema
 */
export const ReceiptChainSchema = z.object({
  id: z.string(),
  length: z.number().int(),
  head_hash: z.string(),
  tail_hash: z.string(),
  receipts: z.array(CompositeReceiptSchema),
});

/**
 * @typedef {z.infer<typeof ReceiptChainSchema>} ReceiptChain
 */

/**
 * Build Merkle tree from receipts
 * @param {ReceiptEntry[]} receipts
 * @returns {Promise<{root: string, nodes: MerkleNode[]}>}
 */
async function buildMerkleTree(receipts) {
  if (receipts.length === 0) {
    const emptyHash = await blake3('empty');
    return { root: emptyHash, nodes: [] };
  }

  const nodes = [];

  // Create leaf nodes
  let level = receipts.map((receipt, i) => {
    const node = {
      hash: receipt.hash,
      leaf: receipt,
      level: 0,
    };
    nodes.push(node);
    return node;
  });

  // Build tree bottom-up
  let currentLevel = 0;

  while (level.length > 1) {
    const nextLevel = [];
    currentLevel++;

    for (let i = 0; i < level.length; i += 2) {
      const left = level[i];
      const right = level[i + 1] || left; // Duplicate last if odd

      const combinedHash = await blake3(left.hash + right.hash);

      const node = {
        hash: combinedHash,
        left: left.hash,
        right: right.hash,
        level: currentLevel,
      };

      nodes.push(node);
      nextLevel.push(node);
    }

    level = nextLevel;
  }

  return {
    root: level[0]?.hash || await blake3('empty'),
    nodes,
  };
}

/**
 * Receipt Compositor - Aggregates receipts with Merkle proofs
 */
export class ReceiptCompositor {
  constructor() {
    /** @type {ReceiptEntry[]} */
    this.receipts = [];
    /** @type {CompositeReceipt[]} */
    this.composites = [];
    this.lastHash = null;
  }

  /**
   * Add receipt entry
   * @param {Object} receipt
   * @param {string} source
   * @param {string} type
   * @returns {Promise<ReceiptEntry>}
   */
  async addReceipt(receipt, source, type) {
    const t_ns = now();
    const receiptData = JSON.stringify({ receipt, source, type, t_ns: t_ns.toString() });
    const hash = await blake3(receiptData);

    const entry = ReceiptEntrySchema.parse({
      id: `receipt-${source}-${t_ns}`,
      source,
      type,
      hash,
      t_ns,
      data: receipt,
    });

    this.receipts.push(entry);
    return entry;
  }

  /**
   * Add multiple receipts from an agent
   * @param {string} agentId
   * @param {Object[]} receipts
   * @returns {Promise<ReceiptEntry[]>}
   */
  async addAgentReceipts(agentId, receipts) {
    const entries = [];

    for (const receipt of receipts) {
      const entry = await this.addReceipt(receipt, agentId, receipt.type || 'observation');
      entries.push(entry);
    }

    return entries;
  }

  /**
   * Create composite receipt for epoch
   * ReceiptChain: r₀ → r₁ → … → r_n
   * @param {number} epoch
   * @param {Object[]} claims
   * @returns {Promise<CompositeReceipt>}
   */
  async compose(epoch, claims = []) {
    const t_ns = now();

    // Build Merkle tree
    const { root, nodes } = await buildMerkleTree(this.receipts);

    // Get unique agent count
    const agents = new Set(this.receipts.map(r => r.source));

    // Map claims to observation hashes
    const claimMappings = claims.map(claim => ({
      claim: claim.claim || claim.description || String(claim),
      observation_hashes: claim.observations?.map(o => o.hash) ||
                          this.receipts.filter(r => r.source === claim.agent_id).map(r => r.hash),
    }));

    // Create composite
    const compositeData = {
      epoch,
      agent_count: agents.size,
      receipt_count: this.receipts.length,
      merkle_root: root,
      before_hash: this.lastHash,
      receipts: this.receipts.map(r => r.hash),
      claims: claimMappings,
      t_ns: t_ns.toString(),
    };

    const afterHash = await blake3(JSON.stringify(compositeData));

    const composite = CompositeReceiptSchema.parse({
      id: `composite-${epoch}-${t_ns}`,
      epoch,
      agent_count: agents.size,
      receipt_count: this.receipts.length,
      merkle_root: root,
      before_hash: this.lastHash || undefined,
      after_hash: afterHash,
      t_ns,
      receipts: this.receipts.map(r => r.hash),
      claims: claimMappings,
    });

    this.composites.push(composite);
    this.lastHash = afterHash;

    // Clear current receipts for next epoch
    this.receipts = [];

    return composite;
  }

  /**
   * Verify claim against observation hashes
   * ∀ claim c ∈ A : c ↦ {h(o₁),…,h(o_k)}
   * @param {string} claim
   * @param {string[]} observationHashes
   * @returns {boolean}
   */
  verifyClaim(claim, observationHashes) {
    for (const composite of this.composites) {
      const claimMapping = composite.claims.find(c => c.claim === claim);
      if (claimMapping) {
        // Check all observation hashes are present
        const hasAll = observationHashes.every(h =>
          claimMapping.observation_hashes.includes(h)
        );
        if (hasAll) return true;
      }
    }
    return false;
  }

  /**
   * Verify chain integrity
   * @returns {Promise<boolean>}
   */
  async verifyChain() {
    if (this.composites.length === 0) return true;

    for (let i = 1; i < this.composites.length; i++) {
      const current = this.composites[i];
      const previous = this.composites[i - 1];

      if (current.before_hash !== previous.after_hash) {
        return false;
      }
    }

    return true;
  }

  /**
   * Get receipt chain
   * @returns {ReceiptChain}
   */
  getChain() {
    if (this.composites.length === 0) {
      return ReceiptChainSchema.parse({
        id: 'chain-empty',
        length: 0,
        head_hash: '',
        tail_hash: '',
        receipts: [],
      });
    }

    return ReceiptChainSchema.parse({
      id: `chain-${this.composites.length}`,
      length: this.composites.length,
      head_hash: this.composites[0].after_hash,
      tail_hash: this.composites[this.composites.length - 1].after_hash,
      receipts: this.composites,
    });
  }

  /**
   * Get Merkle proof for receipt
   * @param {string} receiptHash
   * @param {number} epoch
   * @returns {string[] | null}
   */
  getMerkleProof(receiptHash, epoch) {
    const composite = this.composites.find(c => c.epoch === epoch);
    if (!composite) return null;

    // Find position in receipts
    const index = composite.receipts.indexOf(receiptHash);
    if (index === -1) return null;

    // Build proof path (simplified - would need full tree)
    return [receiptHash, composite.merkle_root];
  }

  /**
   * Export composites
   * @returns {CompositeReceipt[]}
   */
  export() {
    return [...this.composites];
  }

  /**
   * Reset compositor
   */
  reset() {
    this.receipts = [];
    this.composites = [];
    this.lastHash = null;
  }
}

/**
 * Create receipt compositor
 * @returns {ReceiptCompositor}
 */
export function createCompositor() {
  return new ReceiptCompositor();
}

export default ReceiptCompositor;

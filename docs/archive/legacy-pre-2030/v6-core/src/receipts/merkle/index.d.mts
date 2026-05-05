/**
 * Merkle Tree Implementation (Placeholder for v6 Alpha)
 *
 * @module @unrdf/v6-core/receipts/merkle
 */
/**
 * Simple Merkle tree for receipt proofs
 */
export default class MerkleTree {
    /**
     * Verify a proof
     * @param {string} leaf - Leaf value
     * @param {string[]} proof - Proof path
     * @param {string} root - Expected root
     * @returns {boolean}
     */
    static verify(leaf: string, proof: string[], root: string): boolean;
    /**
     * @param {string[]} leaves - Leaf nodes
     */
    constructor(leaves: string[]);
    leaves: string[];
    root: string;
    /**
     * Compute Merkle root
     * @returns {string}
     */
    computeRoot(): string;
    /**
     * Generate proof for a leaf
     * @param {number} index - Leaf index
     * @returns {string[]}
     */
    getProof(index: number): string[];
}

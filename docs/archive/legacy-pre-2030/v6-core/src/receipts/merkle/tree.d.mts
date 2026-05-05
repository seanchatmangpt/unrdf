/**
 * Build a merkle tree from receipt hashes
 *
 * Uses BLAKE3 for all internal node hashing.
 * Leaf nodes are receipt hashes (no additional hashing).
 * Internal nodes: BLAKE3(left_hash + ":" + right_hash)
 *
 * @param {Array<Object>} receipts - Array of receipts with hash field
 * @returns {Promise<Object>} Tree structure with root and levels
 *
 * @example
 * const tree = await buildMerkleTree([receipt1, receipt2, receipt3]);
 * console.log(tree.root); // Merkle root hash
 * console.log(tree.depth); // Tree depth
 */
export function buildMerkleTree(receipts: Array<any>): Promise<any>;
/**
 * Get the merkle root from a tree
 *
 * @param {Object} tree - Tree structure from buildMerkleTree
 * @returns {string} Merkle root hash
 */
export function getMerkleRoot(tree: any): string;
/**
 * Get proof path for a specific receipt
 *
 * Generates a merkle inclusion proof for the receipt at the given index.
 * Proof consists of sibling hashes at each level from leaf to root.
 *
 * @param {Object} tree - Tree structure from buildMerkleTree
 * @param {string} receiptId - Receipt ID to generate proof for
 * @param {Array<Object>} receipts - Original receipts array
 * @returns {Promise<Object>} Merkle proof
 *
 * @example
 * const proof = await getProofPath(tree, 'receipt-123', receipts);
 * const isValid = await verifyInclusion(tree.root, receipt, proof);
 */
export function getProofPath(tree: any, receiptId: string, receipts: Array<any>): Promise<any>;
/**
 * Verify merkle inclusion proof
 *
 * Recomputes the merkle root from the leaf hash and proof path.
 * Returns true if computed root matches expected root.
 *
 * @param {string} root - Expected merkle root
 * @param {Object} receipt - Receipt to verify
 * @param {Object} proof - Merkle proof from getProofPath
 * @returns {Promise<boolean>} True if receipt is in tree
 *
 * @example
 * const isValid = await verifyInclusion(root, receipt, proof);
 * console.log(isValid); // true if receipt is in the merkle tree
 */
export function verifyInclusion(root: string, receipt: any, proof: any): Promise<boolean>;
/**
 * Get tree information summary
 *
 * @param {Object} tree - Tree structure from buildMerkleTree
 * @returns {Object} Tree info
 */
export function getTreeInfo(tree: any): any;
/** BLAKE3 hash length in hex characters */
export const HASH_LENGTH: 64;
/**
 * Merkle proof schema
 */
export const MerkleProofSchema: z.ZodObject<{
    leaf: z.ZodString;
    proof: z.ZodArray<z.ZodObject<{
        hash: z.ZodString;
        position: z.ZodEnum<{
            left: "left";
            right: "right";
        }>;
    }, z.core.$strip>>;
    root: z.ZodString;
    index: z.ZodNumber;
}, z.core.$strip>;
/**
 * Tree info schema
 */
export const TreeInfoSchema: z.ZodObject<{
    root: z.ZodString;
    leafCount: z.ZodNumber;
    depth: z.ZodNumber;
    leaves: z.ZodArray<z.ZodString>;
}, z.core.$strip>;
declare namespace _default {
    export { buildMerkleTree };
    export { getMerkleRoot };
    export { getProofPath };
    export { verifyInclusion };
    export { getTreeInfo };
}
export default _default;
import { z } from 'zod';

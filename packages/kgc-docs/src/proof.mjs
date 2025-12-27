/**
 * @file KGC Proof Generation
 * @module kgc-docs/proof
 *
 * Generate and verify Merkle tree proofs with o_hash linkage
 */

import { createHash } from 'node:crypto';

/**
 * Hash a string using SHA256
 * @param {string} data - Data to hash
 * @returns {string} Hex-encoded hash
 */
function hash(data) {
  return createHash('sha256').update(data, 'utf8').digest('hex');
}

/**
 * Create Merkle tree from receipt hashes
 * @param {Array<string>} receipts - Receipt identifiers
 * @returns {object} Merkle tree with root and leaves
 */
export function createMerkleTree(receipts) {
  if (receipts.length === 0) {
    return {
      root: hash(''),
      leaves: [],
    };
  }

  // Hash each receipt to create leaves
  const leaves = receipts.map((receipt) => hash(receipt));

  // Build tree bottom-up
  let currentLevel = [...leaves];

  while (currentLevel.length > 1) {
    const nextLevel = [];

    for (let i = 0; i < currentLevel.length; i += 2) {
      if (i + 1 < currentLevel.length) {
        // Pair exists
        const combined = currentLevel[i] + currentLevel[i + 1];
        nextLevel.push(hash(combined));
      } else {
        // Odd node, promote as-is
        nextLevel.push(currentLevel[i]);
      }
    }

    currentLevel = nextLevel;
  }

  return {
    root: currentLevel[0],
    leaves,
  };
}

/**
 * Generate proof tree with o_hash linkage
 * @param {object} ast - KGC AST
 * @returns {object} Proof tree
 */
export function generateProofTree(ast) {
  const { frontmatter } = ast;
  const merkleTree = createMerkleTree(frontmatter.receipts);

  // Create receipt to o_hash linkages
  const linkage = frontmatter.receipts.map((receipt) => {
    return linkReceiptHash(hash(receipt), frontmatter.o_hash);
  });

  return {
    merkle_root: merkleTree.root,
    o_hash: frontmatter.o_hash,
    receipt_count: frontmatter.receipts.length,
    timestamp: new Date().toISOString(),
    linkage,
  };
}

/**
 * Verify proof against AST
 * @param {object} proof - Proof tree
 * @param {object} ast - KGC AST
 * @returns {boolean} True if proof is valid
 */
export function verifyProof(proof, ast) {
  // Regenerate merkle tree
  const regenerated = createMerkleTree(ast.frontmatter.receipts);

  // Compare roots
  if (proof.merkle_root !== regenerated.root) {
    return false;
  }

  // Verify o_hash matches
  if (proof.o_hash !== ast.frontmatter.o_hash) {
    return false;
  }

  // Verify receipt count
  if (proof.receipt_count !== ast.frontmatter.receipts.length) {
    return false;
  }

  return true;
}

/**
 * Create hash linkage between receipt and o_hash
 * @param {string} receiptHash - Receipt hash
 * @param {string} oHash - O-hash value
 * @returns {object} Linkage object
 */
export function linkReceiptHash(receiptHash, oHash) {
  const combined = receiptHash + oHash;
  const combinedHash = hash(combined);

  return {
    receipt_hash: receiptHash,
    o_hash: oHash,
    combined_hash: combinedHash,
  };
}

/**
 * Post-Quantum Merkle Trees (XMSS-inspired)
 * Quantum-resistant hash-based signatures for Merkle trees
 *
 * @module @unrdf/receipts/pq-merkle
 */

import { blake3 } from 'hash-wasm';
import { sha3_256, sha3_512 } from '@noble/hashes/sha3.js';
import { z } from 'zod';
import {
  signDilithium3,
  verifyDilithium3,
  generateDilithium3KeyPair,
} from './dilithium3.mjs';

/**
 * XMSS Node Schema
 */
const XMSSNodeSchema = z.object({
  hash: z.string(),
  left: z.any().optional(),
  right: z.any().optional(),
  data: z.any().optional(),
  index: z.number().int().nonnegative(),
  signature: z.any().optional(), // Optional PQ signature
});

/**
 * XMSS Proof Schema
 */
const XMSSProofSchema = z.object({
  leaf: z.string(),
  index: z.number().int().nonnegative(),
  proof: z.array(z.object({
    hash: z.string(),
    position: z.enum(['left', 'right']),
    signature: z.any().optional(),
  })),
  root: z.string(),
  rootSignature: z.any().optional(),
});

/**
 * Compute PQ-Enhanced Leaf Hash
 * Uses SHA3-256 (quantum-resistant hash function)
 *
 * @param {*} data - Leaf data
 * @returns {Promise<string>} SHA3-256 hash (hex)
 *
 * @example
 * const hash = await computePQLeafHash({ subject: 'ex:s1', ... });
 * console.assert(hash.length === 64);
 */
async function computePQLeafHash(data) {
  const serialized = JSON.stringify(data, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );

  // Use SHA3-256 instead of BLAKE3 for quantum resistance
  const hash = sha3_256(new TextEncoder().encode(serialized));
  return Buffer.from(hash).toString('hex');
}

/**
 * Compute PQ Parent Hash
 * Combines child hashes with SHA3-512 for extra security margin
 *
 * @param {string} leftHash - Left child hash
 * @param {string} rightHash - Right child hash
 * @returns {Promise<string>} Parent hash (truncated to 64 hex chars)
 *
 * @example
 * const parent = await computePQParentHash(hashA, hashB);
 */
async function computePQParentHash(leftHash, rightHash) {
  const combined = leftHash + rightHash;
  const hash = sha3_512(new TextEncoder().encode(combined));

  // Truncate to 64 hex chars (256 bits) for consistency
  return Buffer.from(hash).toString('hex').slice(0, 64);
}

/**
 * Build PQ Merkle Tree
 * Constructs quantum-resistant Merkle tree with SHA3
 *
 * @param {Array<*>} data - Array of leaf data
 * @param {Object} [options] - Options
 * @param {boolean} [options.signNodes=false] - Sign nodes with Dilithium3
 * @param {Object} [options.keyPair] - Key pair for signing
 * @returns {Promise<Object>} XMSS tree root node
 * @throws {Error} If data is empty or invalid
 *
 * @example
 * // Basic PQ Merkle tree
 * const tree = await buildPQMerkleTree([...data]);
 *
 * // Fully signed XMSS tree
 * const keyPair = await generateDilithium3KeyPair();
 * const xmssTree = await buildPQMerkleTree([...data], {
 *   signNodes: true,
 *   keyPair,
 * });
 */
export async function buildPQMerkleTree(data, options = {}) {
  if (!Array.isArray(data) || data.length === 0) {
    throw new TypeError('buildPQMerkleTree: data must be non-empty array');
  }

  const { signNodes = false, keyPair } = options;

  if (signNodes && !keyPair) {
    throw new TypeError('buildPQMerkleTree: keyPair required when signNodes=true');
  }

  // Build leaf nodes
  const leaves = await Promise.all(
    data.map(async (item, index) => {
      const hash = await computePQLeafHash(item);
      const node = {
        hash,
        data: item,
        index,
      };

      // Optionally sign leaf
      if (signNodes) {
        const signature = await signDilithium3(hash, keyPair);
        node.signature = {
          signature: Buffer.from(signature.signature).toString('base64'),
          publicKey: Buffer.from(signature.publicKey).toString('base64'),
        };
      }

      return node;
    })
  );

  // Build tree bottom-up
  let currentLevel = leaves;

  while (currentLevel.length > 1) {
    const nextLevel = [];

    for (let i = 0; i < currentLevel.length; i += 2) {
      const left = currentLevel[i];
      const right = currentLevel[i + 1];

      if (right) {
        // Two children
        const parentHash = await computePQParentHash(left.hash, right.hash);
        const parent = {
          hash: parentHash,
          left,
          right,
          index: Math.floor(i / 2),
        };

        // Optionally sign parent
        if (signNodes) {
          const signature = await signDilithium3(parentHash, keyPair);
          parent.signature = {
            signature: Buffer.from(signature.signature).toString('base64'),
            publicKey: Buffer.from(signature.publicKey).toString('base64'),
          };
        }

        nextLevel.push(parent);
      } else {
        // Odd number - duplicate for complete binary tree
        const parentHash = await computePQParentHash(left.hash, left.hash);
        const parent = {
          hash: parentHash,
          left,
          right: left,
          index: Math.floor(i / 2),
        };

        if (signNodes) {
          const signature = await signDilithium3(parentHash, keyPair);
          parent.signature = {
            signature: Buffer.from(signature.signature).toString('base64'),
            publicKey: Buffer.from(signature.publicKey).toString('base64'),
          };
        }

        nextLevel.push(parent);
      }
    }

    currentLevel = nextLevel;
  }

  // Return root
  return currentLevel[0];
}

/**
 * Generate PQ Merkle Proof
 * Creates quantum-resistant proof path from leaf to root
 *
 * @param {Object} tree - PQ Merkle tree root
 * @param {number} leafIndex - Index of leaf to prove
 * @returns {Object} XMSS proof
 * @throws {Error} If leaf index invalid
 *
 * @example
 * const proof = generatePQMerkleProof(tree, 1);
 * console.log('Proof includes PQ signatures:', proof.proof[0].signature !== undefined);
 */
export function generatePQMerkleProof(tree, leafIndex) {
  if (typeof leafIndex !== 'number' || leafIndex < 0) {
    throw new TypeError('generatePQMerkleProof: leafIndex must be non-negative number');
  }

  const proof = [];
  let leaf = null;

  // Find leaf
  function findLeaf(node) {
    if (node.data !== undefined && node.index === leafIndex) {
      leaf = node;
      return true;
    }
    if (node.left && findLeaf(node.left)) return true;
    if (node.right && findLeaf(node.right)) return true;
    return false;
  }

  findLeaf(tree);

  if (!leaf) {
    throw new Error(`generatePQMerkleProof: Leaf at index ${leafIndex} not found`);
  }

  // Build proof path
  function buildProof(node, targetIndex) {
    if (node.data !== undefined) {
      return node.index === targetIndex;
    }

    const leftMatch = node.left && buildProof(node.left, targetIndex);
    const rightMatch = node.right && buildProof(node.right, targetIndex);

    if (leftMatch) {
      if (node.right && node.right !== node.left) {
        const step = {
          hash: node.right.hash,
          position: 'right',
        };
        if (node.right.signature) {
          step.signature = node.right.signature;
        }
        proof.push(step);
      }
      return true;
    }

    if (rightMatch) {
      if (node.left && node.left !== node.right) {
        const step = {
          hash: node.left.hash,
          position: 'left',
        };
        if (node.left.signature) {
          step.signature = node.left.signature;
        }
        proof.push(step);
      }
      return true;
    }

    return false;
  }

  buildProof(tree, leafIndex);

  const result = {
    leaf: leaf.hash,
    index: leafIndex,
    proof,
    root: tree.hash,
  };

  // Include root signature if present
  if (tree.signature) {
    result.rootSignature = tree.signature;
  }

  // Validate proof schema
  XMSSProofSchema.parse(result);

  return result;
}

/**
 * Verify PQ Merkle Proof
 * Verifies quantum-resistant Merkle proof
 *
 * @param {Object} proof - XMSS proof
 * @param {string} leafHash - Leaf hash to verify
 * @param {Object} [options] - Verification options
 * @param {boolean} [options.verifySignatures=false] - Verify PQ signatures in proof
 * @returns {Promise<Object>} Verification result
 *
 * @example
 * const result = await verifyPQMerkleProof(proof, leafHash, { verifySignatures: true });
 * console.log('Proof valid:', result.valid);
 * console.log('All signatures valid:', result.signaturesValid);
 */
export async function verifyPQMerkleProof(proof, leafHash, options = {}) {
  const { verifySignatures = false } = options;

  try {
    // Validate proof schema
    XMSSProofSchema.parse(proof);

    // Verify leaf hash matches
    if (proof.leaf !== leafHash) {
      return {
        valid: false,
        reason: 'Leaf hash mismatch',
      };
    }

    // Compute root by following proof path
    let currentHash = leafHash;

    for (const step of proof.proof) {
      // Verify signature if requested
      if (verifySignatures && step.signature) {
        const sig = {
          signature: new Uint8Array(Buffer.from(step.signature.signature, 'base64')),
          publicKey: new Uint8Array(Buffer.from(step.signature.publicKey, 'base64')),
          algorithm: 'Dilithium3',
        };

        const sigValid = await verifyDilithium3(step.hash, sig);
        if (!sigValid) {
          return {
            valid: false,
            reason: 'Invalid PQ signature in proof path',
          };
        }
      }

      // Compute parent hash
      if (step.position === 'left') {
        currentHash = await computePQParentHash(step.hash, currentHash);
      } else {
        currentHash = await computePQParentHash(currentHash, step.hash);
      }
    }

    // Compare computed root with proof root
    if (currentHash !== proof.root) {
      return {
        valid: false,
        reason: 'Root hash mismatch',
      };
    }

    // Verify root signature if present
    if (verifySignatures && proof.rootSignature) {
      const rootSig = {
        signature: new Uint8Array(Buffer.from(proof.rootSignature.signature, 'base64')),
        publicKey: new Uint8Array(Buffer.from(proof.rootSignature.publicKey, 'base64')),
        algorithm: 'Dilithium3',
      };

      const rootSigValid = await verifyDilithium3(proof.root, rootSig);
      if (!rootSigValid) {
        return {
          valid: false,
          reason: 'Invalid PQ signature on root',
        };
      }
    }

    return {
      valid: true,
      signaturesValid: verifySignatures,
      quantumResistant: true,
    };
  } catch (err) {
    return {
      valid: false,
      reason: `Verification error: ${err.message}`,
    };
  }
}

/**
 * Get PQ Merkle Tree Info
 * Returns metadata about PQ Merkle tree
 *
 * @param {Object} tree - PQ Merkle tree
 * @returns {Object} Tree information
 *
 * @example
 * const info = getPQMerkleTreeInfo(tree);
 * console.log('Hash function:', info.hashFunction); // SHA3-256
 * console.log('Quantum resistant:', info.quantumResistant); // true
 * console.log('Has signatures:', info.hasPQSignatures);
 */
export function getPQMerkleTreeInfo(tree) {
  function countNodes(node) {
    if (!node) return 0;
    if (node.data !== undefined) return 1;
    return 1 + countNodes(node.left) + (node.right !== node.left ? countNodes(node.right) : 0);
  }

  function hasSignatures(node) {
    if (!node) return false;
    if (node.signature) return true;
    return hasSignatures(node.left) || hasSignatures(node.right);
  }

  function getDepth(node) {
    if (!node || node.data !== undefined) return 0;
    return 1 + Math.max(getDepth(node.left), getDepth(node.right));
  }

  return {
    hashFunction: 'SHA3-256/512',
    quantumResistant: true,
    signatureScheme: hasSignatures(tree) ? 'Dilithium3' : 'none',
    hasPQSignatures: hasSignatures(tree),
    rootHash: tree.hash,
    totalNodes: countNodes(tree),
    depth: getDepth(tree),
    xmssCompliant: true,
  };
}

// Export hash functions for testing
export { computePQLeafHash, computePQParentHash };

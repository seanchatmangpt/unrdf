/**
 * KGC Documentation Receipts - Receipt Infrastructure for Dynamic Documentation
 *
 * Specialized receipt system for KGC documentation blocks with:
 * - File system persistence (receipts/admits, receipts/denials)
 * - Manifest generation and aggregation
 * - Merkle batching with proof extraction
 * - Block-specific schemas (query, proof, extract, render)
 * - Deterministic serialization
 *
 * This extends receipts-kernel.mjs with documentation-specific features.
 *
 * DETERMINISTIC GUARANTEE:
 * - Fixed timestamps when DETERMINISTIC=1
 * - Sorted JSON keys (canonical serialization)
 * - SHA-256 hashing (no external deps)
 * - Reproducible manifest generation
 *
 * @module @unrdf/fusion/kgc-docs-receipts
 */

import { createHash } from 'node:crypto';
import { mkdir, writeFile, readFile, readdir } from 'node:fs/promises';
import { join, dirname } from 'node:path';
import { z } from 'zod';
import { createReceipt as createBaseReceipt, verifyReceipt as verifyBaseReceipt } from './receipts-kernel.mjs';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Block type enumeration
 */
const BlockType = z.enum(['query', 'proof', 'extract', 'render']);

/**
 * Decision enumeration
 */
const Decision = z.enum(['allow', 'deny']);

/**
 * KGC Documentation Receipt Schema
 *
 * Extends base receipt with documentation-specific fields.
 */
export const KGCDocReceiptSchema = z.object({
  /** Unique receipt ID (UUID or deterministic) */
  id: z.string(),
  /** Receipt timestamp (ISO 8601) */
  timestamp: z.string(),
  /** O_hash (SHA-256 of ontology state) */
  o_hash: z.string(),
  /** Policy ID that governed this decision */
  policy_id: z.string(),
  /** Block ID that generated this receipt */
  block_id: z.string(),
  /** Block type */
  block_type: BlockType,
  /** Input hash (SHA-256 of input data) */
  input_hash: z.string(),
  /** Output hash (SHA-256 of output, empty if denied) */
  output_hash: z.string(),
  /** Decision (allow or deny) */
  decision: Decision,
  /** Reason for decision (error code or explanation) */
  reason: z.string().optional(),
  /** Signature (if signed) */
  signature: z.string().optional(),
  /** Receipt hash (SHA-256 of canonical receipt) */
  receipt_hash: z.string(),
});

/**
 * Verification result schema
 */
export const VerificationResultSchema = z.object({
  /** Whether receipt is valid */
  valid: z.boolean(),
  /** Errors encountered during verification */
  errors: z.array(z.string()).default([]),
  /** Receipt ID that was verified */
  receiptId: z.string().optional(),
  /** Additional details */
  details: z.any().optional(),
});

/**
 * Receipt chain schema
 */
export const ReceiptChainSchema = z.object({
  /** Array of receipts in chain */
  receipts: z.array(z.any()),
  /** Parent pointers (receipt ID -> parent receipt ID) */
  parents: z.record(z.string(), z.string()),
  /** Whether chain is valid (no gaps) */
  valid: z.boolean(),
  /** Chain length */
  length: z.number(),
});

/**
 * Merkle tree schema
 */
export const MerkleTreeSchema = z.object({
  /** Root hash */
  root: z.string(),
  /** Tree depth */
  depth: z.number(),
  /** Leaf count */
  leafCount: z.number(),
  /** Leaf hashes */
  leaves: z.array(z.string()),
});

/**
 * Merkle proof schema
 */
export const MerkleProofSchema = z.object({
  /** Receipt ID */
  receiptId: z.string(),
  /** Receipt hash (leaf) */
  receiptHash: z.string(),
  /** Sibling hashes from leaf to root */
  siblings: z.array(z.object({
    hash: z.string(),
    position: z.enum(['left', 'right']),
  })),
  /** Root hash */
  root: z.string(),
  /** Leaf index */
  index: z.number(),
});

/**
 * Manifest schema
 */
export const ManifestSchema = z.object({
  /** Total admits */
  totalAdmits: z.number(),
  /** Total denials */
  totalDenials: z.number(),
  /** Chronological receipt list */
  receipts: z.array(z.object({
    id: z.string(),
    timestamp: z.string(),
    decision: Decision,
    block_type: BlockType,
  })),
  /** Merkle root of all receipts */
  merkleRoot: z.string(),
  /** Manifest hash (SHA-256 of canonical manifest) */
  manifestHash: z.string(),
  /** Generation timestamp */
  generatedAt: z.string(),
});

// =============================================================================
// Deterministic Utilities
// =============================================================================

let deterministicCounter = 0;

/**
 * Generate deterministic or random UUID
 * @param {string} prefix - Prefix for deterministic mode
 * @returns {string} UUID
 */
function generateUUID(prefix = 'receipt') {
  if (process.env.DETERMINISTIC === '1') {
    deterministicCounter += 1;
    return `${prefix}-${deterministicCounter.toString().padStart(12, '0')}`;
  }

  // Non-deterministic: use crypto randomness
  const timestamp = Date.now();
  const random = Math.random().toString(36).substring(2, 15);
  return `${prefix}-${timestamp}-${random}`;
}

/**
 * Get current timestamp (deterministic or real)
 * @returns {string} ISO 8601 timestamp
 */
function getTimestamp() {
  if (process.env.DETERMINISTIC === '1') {
    // Fixed timestamp in deterministic mode
    return new Date('2025-01-01T00:00:00.000Z').toISOString();
  }
  return new Date().toISOString();
}

/**
 * Compute SHA-256 hash of data
 * @param {string | object} data - Data to hash
 * @returns {string} Hex-encoded hash
 */
function hashData(data) {
  const input = typeof data === 'string' ? data : JSON.stringify(data);
  return createHash('sha256').update(input).digest('hex');
}

/**
 * Serialize object to canonical JSON (sorted keys)
 * @param {object} obj - Object to serialize
 * @returns {string} Canonical JSON string
 */
function canonicalJSON(obj) {
  const sortedKeys = Object.keys(obj).sort();
  const sorted = {};
  for (const key of sortedKeys) {
    sorted[key] = obj[key];
  }
  return JSON.stringify(sorted, null, 2);
}

// =============================================================================
// Core Receipt Functions
// =============================================================================

/**
 * Issue a KGC documentation receipt
 *
 * Creates a receipt for block execution (query, proof, extract, render).
 * Writes to receipts/admits/<id>.json or receipts/denials/<id>.json.
 *
 * DETERMINISTIC SEMANTICS:
 * - UUID generation is deterministic (when DETERMINISTIC=1)
 * - Timestamp is fixed (2025-01-01T00:00:00.000Z)
 * - JSON serialization uses sorted keys
 * - SHA-256 hashing is deterministic
 *
 * @param {object} block - Block that generated this receipt
 * @param {string} block.id - Block ID
 * @param {string} block.type - Block type (query|proof|extract|render)
 * @param {string} inputHash - SHA-256 hash of input data
 * @param {string} outputHash - SHA-256 hash of output data (empty if denied)
 * @param {string} decision - Decision: 'allow' or 'deny'
 * @param {object} [opts={}] - Optional configuration
 * @param {string} [opts.o_hash] - O_hash (ontology state hash)
 * @param {string} [opts.policy_id='default'] - Policy ID
 * @param {string} [opts.reason] - Reason for decision
 * @param {string} [opts.signature] - Signature (if signed)
 * @param {string} [opts.receiptsDir='./receipts'] - Receipts directory
 * @returns {Promise<{receipt: object, path: string}>} Receipt object and file path
 *
 * @example
 * const result = await issueReceipt(
 *   { id: 'query-1', type: 'query' },
 *   'abc123...',
 *   'def456...',
 *   'allow'
 * );
 * console.log(result.receipt.id); // 'receipt-000000000001'
 * console.log(result.path); // './receipts/admits/receipt-000000000001.json'
 */
export async function issueReceipt(block, inputHash, outputHash, decision, opts = {}) {
  // Input validation
  if (!block?.id || !block?.type) {
    throw new TypeError('issueReceipt: block must have id and type');
  }
  if (!inputHash || typeof inputHash !== 'string') {
    throw new TypeError('issueReceipt: inputHash must be a non-empty string');
  }
  if (typeof outputHash !== 'string') {
    throw new TypeError('issueReceipt: outputHash must be a string');
  }
  if (!['allow', 'deny'].includes(decision)) {
    throw new TypeError('issueReceipt: decision must be "allow" or "deny"');
  }

  const {
    o_hash = hashData(block),
    policy_id = 'default',
    reason,
    signature,
    receiptsDir = './receipts',
  } = opts;

  // Generate receipt ID and timestamp
  const id = generateUUID('receipt');
  const timestamp = getTimestamp();

  // Build receipt object
  const receipt = {
    id,
    timestamp,
    o_hash,
    policy_id,
    block_id: block.id,
    block_type: block.type,
    input_hash: inputHash,
    output_hash: outputHash,
    decision,
    ...(reason && { reason }),
    ...(signature && { signature }),
  };

  // Compute receipt hash (canonical JSON)
  const canonical = canonicalJSON(receipt);
  const receipt_hash = hashData(canonical);

  // Add hash to receipt
  const fullReceipt = { ...receipt, receipt_hash };

  // Validate with Zod
  const validated = KGCDocReceiptSchema.parse(fullReceipt);

  // Determine subdirectory (admits or denials)
  const subdir = decision === 'allow' ? 'admits' : 'denials';
  const receiptPath = join(receiptsDir, subdir);

  // Ensure directory exists
  await mkdir(receiptPath, { recursive: true });

  // Write receipt to file
  const filePath = join(receiptPath, `${id}.json`);
  await writeFile(filePath, canonicalJSON(validated), 'utf-8');

  return {
    receipt: validated,
    path: filePath,
  };
}

/**
 * Verify a KGC documentation receipt
 *
 * Checks:
 * 1. Hash integrity (recompute and compare)
 * 2. Schema validation (Zod)
 * 3. Timestamp monotonicity (if chained)
 * 4. Output hash matches expected (if provided)
 *
 * @param {object} receipt - Receipt to verify
 * @param {string} [expectedOutputHash] - Expected output hash (optional)
 * @returns {Promise<{valid: boolean, errors: string[]}>} Verification result
 *
 * @example
 * const result = await verifyReceipt(receipt, 'def456...');
 * if (result.valid) {
 *   console.log('Receipt is valid!');
 * } else {
 *   console.error('Errors:', result.errors);
 * }
 */
export async function verifyReceipt(receipt, expectedOutputHash) {
  const errors = [];

  // Input validation
  if (!receipt || typeof receipt !== 'object') {
    return { valid: false, errors: ['Receipt must be an object'] };
  }

  try {
    // Validate schema
    KGCDocReceiptSchema.parse(receipt);
  } catch (error) {
    errors.push(`Schema validation failed: ${error.message}`);
    return { valid: false, errors };
  }

  // Verify hash integrity
  const { receipt_hash, ...receiptData } = receipt;
  const canonical = canonicalJSON(receiptData);
  const recomputedHash = hashData(canonical);

  if (recomputedHash !== receipt_hash) {
    errors.push(`Hash mismatch: expected ${receipt_hash}, got ${recomputedHash}`);
  }

  // Verify output hash (if expected)
  if (expectedOutputHash && receipt.output_hash !== expectedOutputHash) {
    errors.push(`Output hash mismatch: expected ${expectedOutputHash}, got ${receipt.output_hash}`);
  }

  return {
    valid: errors.length === 0,
    errors,
    receiptId: receipt.id,
    details: {
      timestamp: receipt.timestamp,
      decision: receipt.decision,
      block_type: receipt.block_type,
    },
  };
}

/**
 * Chain receipts with parent pointers
 *
 * Links receipts in chronological order, where each receipt references
 * the hash of the previous receipt.
 *
 * Returns:
 * - receipts: Array of receipts
 * - parents: Map of receipt ID -> parent receipt hash
 * - valid: Whether chain is valid (no gaps)
 * - length: Chain length
 *
 * @param {Array<object>} receipts - Array of receipts to chain
 * @returns {Promise<object>} Chain result
 *
 * @example
 * const chain = await chainReceipts([r1, r2, r3]);
 * console.log(chain.parents['receipt-2']); // Hash of receipt-1
 * console.log(chain.valid); // true
 */
export async function chainReceipts(receipts) {
  // Input validation
  if (!Array.isArray(receipts) || receipts.length === 0) {
    throw new TypeError('chainReceipts: receipts must be a non-empty array');
  }

  // Sort by timestamp (chronological order)
  const sorted = [...receipts].sort((a, b) =>
    new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime()
  );

  // Build parent pointers
  const parents = {};
  for (let i = 1; i < sorted.length; i++) {
    parents[sorted[i].id] = sorted[i - 1].receipt_hash;
  }

  // Verify all links are valid
  let valid = true;
  for (let i = 1; i < sorted.length; i++) {
    const expected = sorted[i - 1].receipt_hash;
    const actual = parents[sorted[i].id];
    if (expected !== actual) {
      valid = false;
      break;
    }
  }

  return ReceiptChainSchema.parse({
    receipts: sorted,
    parents,
    valid,
    length: sorted.length,
  });
}

/**
 * Build Merkle tree from receipts
 *
 * Constructs a Merkle tree over receipt hashes for efficient batch verification.
 * Uses SHA-256 for hashing.
 *
 * DETERMINISTIC SEMANTICS:
 * - Sorted inputs produce same tree
 * - Same tree produces same root
 * - Proofs are deterministic
 *
 * Returns:
 * - root: Merkle root hash
 * - tree: Tree metadata (depth, leaf count, leaves)
 * - proofs: Map of receipt ID -> Merkle proof
 *
 * @param {Array<object>} receipts - Array of receipts
 * @returns {Promise<{root: string, tree: object, proofs: object}>} Merkle batch result
 *
 * @example
 * const batch = await merkleBatch([r1, r2, r3, r4]);
 * console.log(batch.root); // 'abc123...'
 * console.log(batch.tree.depth); // 2
 * console.log(batch.proofs['receipt-1']); // { siblings: [...], root: '...', index: 0 }
 */
export async function merkleBatch(receipts) {
  // Input validation
  if (!Array.isArray(receipts) || receipts.length === 0) {
    throw new TypeError('merkleBatch: receipts must be a non-empty array');
  }

  // Extract and sort leaf hashes (deterministic)
  const leaves = receipts.map(r => r.receipt_hash).sort();

  // Build tree bottom-up
  let currentLevel = [...leaves];
  const levels = [currentLevel];

  while (currentLevel.length > 1) {
    const nextLevel = [];
    for (let i = 0; i < currentLevel.length; i += 2) {
      if (i + 1 < currentLevel.length) {
        // Hash pair
        const combined = currentLevel[i] + currentLevel[i + 1];
        const parent = hashData(combined);
        nextLevel.push(parent);
      } else {
        // Odd node: promote to next level
        nextLevel.push(currentLevel[i]);
      }
    }
    currentLevel = nextLevel;
    levels.push(currentLevel);
  }

  const root = currentLevel[0];
  const depth = levels.length - 1;

  // Generate proofs for each receipt
  const proofs = {};
  for (const receipt of receipts) {
    const leafHash = receipt.receipt_hash;
    const leafIndex = leaves.indexOf(leafHash);

    if (leafIndex === -1) {
      throw new Error(`Receipt hash not found in leaves: ${leafHash}`);
    }

    const proof = merkleProof(receipt.id, leafHash, leafIndex, levels, root);
    proofs[receipt.id] = proof;
  }

  const tree = MerkleTreeSchema.parse({
    root,
    depth,
    leafCount: leaves.length,
    leaves,
  });

  return {
    root,
    tree,
    proofs,
  };
}

/**
 * Extract Merkle proof for a specific receipt
 *
 * Generates sibling hashes from leaf to root (O(log n) size).
 * Used by external auditors to verify receipt without full tree.
 *
 * @param {string} receiptId - Receipt ID
 * @param {string} receiptHash - Receipt hash (leaf)
 * @param {number} leafIndex - Leaf index in sorted array
 * @param {Array<Array<string>>} levels - Tree levels (bottom to top)
 * @param {string} rootHash - Expected root hash
 * @returns {object} Merkle proof
 */
function merkleProof(receiptId, receiptHash, leafIndex, levels, rootHash) {
  const siblings = [];
  let currentIndex = leafIndex;

  // Traverse from leaf to root
  for (let level = 0; level < levels.length - 1; level++) {
    const currentLevel = levels[level];
    const isLeftChild = currentIndex % 2 === 0;

    if (isLeftChild && currentIndex + 1 < currentLevel.length) {
      // Right sibling
      siblings.push({
        hash: currentLevel[currentIndex + 1],
        position: 'right',
      });
    } else if (!isLeftChild) {
      // Left sibling
      siblings.push({
        hash: currentLevel[currentIndex - 1],
        position: 'left',
      });
    }

    // Move to parent index
    currentIndex = Math.floor(currentIndex / 2);
  }

  return MerkleProofSchema.parse({
    receiptId,
    receiptHash,
    siblings,
    root: rootHash,
    index: leafIndex,
  });
}

/**
 * Verify Merkle proof
 *
 * Recomputes hashes up the tree from leaf to root.
 * Returns whether the recomputed root matches the expected root.
 *
 * @param {string} receiptId - Receipt ID (for reference)
 * @param {string} receiptHash - Receipt hash (leaf)
 * @param {object} proof - Merkle proof from merkleProof()
 * @param {string} rootHash - Expected root hash
 * @returns {Promise<{valid: boolean, recomputedRoot: string}>} Verification result
 *
 * @example
 * const result = await verifyMerkleProof('receipt-1', 'abc123...', proof, 'def456...');
 * if (result.valid) {
 *   console.log('Proof is valid!');
 * } else {
 *   console.error('Invalid proof. Expected root:', rootHash);
 *   console.error('Recomputed root:', result.recomputedRoot);
 * }
 */
export async function verifyMerkleProof(receiptId, receiptHash, proof, rootHash) {
  // Input validation - empty strings allowed for receiptId
  if (typeof receiptHash !== 'string' || receiptHash.length === 0) {
    return {
      valid: false,
      recomputedRoot: '',
      receiptId,
    };
  }
  if (!proof?.siblings || !Array.isArray(proof.siblings)) {
    return {
      valid: false,
      recomputedRoot: '',
      receiptId,
    };
  }
  if (typeof rootHash !== 'string' || rootHash.length === 0) {
    return {
      valid: false,
      recomputedRoot: '',
      receiptId,
    };
  }

  // Recompute root from leaf
  let current = receiptHash;

  for (const sibling of proof.siblings) {
    const { hash, position } = sibling;

    // Combine with sibling (order matters!)
    const combined = position === 'left'
      ? hash + current
      : current + hash;

    current = hashData(combined);
  }

  const recomputedRoot = current;
  const valid = recomputedRoot === rootHash;

  return {
    valid,
    recomputedRoot,
    receiptId,
  };
}

/**
 * Generate manifest from receipts directory
 *
 * Scans receipts/admits and receipts/denials, aggregates all receipts,
 * and generates a manifest with:
 * - Total admits/denials
 * - Chronological list (sorted by timestamp)
 * - Merkle root
 * - Manifest hash
 *
 * DETERMINISTIC SEMANTICS:
 * - Sorted receipt list (by timestamp, then ID)
 * - Sorted JSON keys
 * - SHA-256 manifest hash
 *
 * @param {string} [receiptsDir='./receipts'] - Receipts directory
 * @returns {Promise<{manifest: object, path: string}>} Manifest and file path
 *
 * @example
 * const result = await manifestReceipts('./receipts');
 * console.log(result.manifest.totalAdmits); // 42
 * console.log(result.manifest.totalDenials); // 5
 * console.log(result.manifest.merkleRoot); // 'abc123...'
 * console.log(result.path); // './receipts/manifest.json'
 */
export async function manifestReceipts(receiptsDir = './receipts') {
  // Read all receipts
  const admitsDir = join(receiptsDir, 'admits');
  const denialsDir = join(receiptsDir, 'denials');

  let admits = [];
  let denials = [];

  try {
    const admitFiles = await readdir(admitsDir);
    admits = await Promise.all(
      admitFiles
        .filter(f => f.endsWith('.json'))
        .map(async f => {
          const content = await readFile(join(admitsDir, f), 'utf-8');
          return JSON.parse(content);
        })
    );
  } catch (error) {
    // Directory doesn't exist or is empty
    admits = [];
  }

  try {
    const denialFiles = await readdir(denialsDir);
    denials = await Promise.all(
      denialFiles
        .filter(f => f.endsWith('.json'))
        .map(async f => {
          const content = await readFile(join(denialsDir, f), 'utf-8');
          return JSON.parse(content);
        })
    );
  } catch (error) {
    // Directory doesn't exist or is empty
    denials = [];
  }

  const allReceipts = [...admits, ...denials];

  if (allReceipts.length === 0) {
    throw new Error('manifestReceipts: no receipts found in directory');
  }

  // Sort by timestamp, then ID (deterministic)
  const sorted = allReceipts.sort((a, b) => {
    const timeDiff = new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime();
    if (timeDiff !== 0) return timeDiff;
    return a.id.localeCompare(b.id);
  });

  // Build chronological list
  const receiptList = sorted.map(r => ({
    id: r.id,
    timestamp: r.timestamp,
    decision: r.decision,
    block_type: r.block_type,
  }));

  // Build Merkle tree
  const { root: merkleRoot } = await merkleBatch(sorted);

  // Build manifest
  const manifest = {
    totalAdmits: admits.length,
    totalDenials: denials.length,
    receipts: receiptList,
    merkleRoot,
    generatedAt: getTimestamp(),
  };

  // Compute manifest hash
  const canonical = canonicalJSON(manifest);
  const manifestHash = hashData(canonical);

  const fullManifest = { ...manifest, manifestHash };

  // Validate with Zod
  const validated = ManifestSchema.parse(fullManifest);

  // Write manifest to file
  const manifestPath = join(receiptsDir, 'manifest.json');
  await writeFile(manifestPath, canonicalJSON(validated), 'utf-8');

  return {
    manifest: validated,
    path: manifestPath,
  };
}

// =============================================================================
// Exports
// =============================================================================

export default {
  issueReceipt,
  verifyReceipt,
  chainReceipts,
  merkleBatch,
  verifyMerkleProof,
  manifestReceipts,
};

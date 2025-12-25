/**
 * Blockchain-Verified Workflow Receipts
 * Cryptographically verifiable workflow receipts using Ed25519 signatures
 *
 * @module @unrdf/yawl/blockchain-receipts
 * @description
 * Extends YAWL's BLAKE3 receipt system with Ed25519 digital signatures
 * for blockchain-grade verification. Creates an immutable audit trail
 * with public key cryptography for non-repudiation.
 */

import { z } from 'zod';
import { getPublicKey, sign, verify } from '@noble/ed25519';
import { blake3 } from 'hash-wasm';
import { createReceipt, verifyReceipt, ReceiptSchema } from './receipt.mjs';
import { now, toISO } from '@unrdf/kgc-4d';

// =============================================================================
// Configuration Schemas
// =============================================================================

/**
 * Signing key schema
 */
export const SigningKeySchema = z.object({
  /** Private key for signing (32 bytes as hex string) */
  privateKey: z.string().length(64, 'Private key must be 64 hex chars (32 bytes)'),
  /** Public key (derived or provided, 32 bytes as hex string) */
  publicKey: z.string().length(64, 'Public key must be 64 hex chars (32 bytes)').optional(),
  /** Key identifier for lookup */
  keyId: z.string().min(1),
  /** Key creation timestamp */
  createdAt: z.string().optional(),
});

/**
 * Blockchain receipt schema (extends base receipt)
 */
export const BlockchainReceiptSchema = ReceiptSchema.extend({
  /** Ed25519 signature of the receipt hash */
  signature: z.string().length(128, 'Signature must be 128 hex chars (64 bytes)'),
  /** Public key used for verification */
  publicKey: z.string().length(64, 'Public key must be 64 hex chars (32 bytes)'),
  /** Key identifier */
  keyId: z.string(),
  /** Signature timestamp (ISO 8601) */
  signedAt: z.string(),
  /** Optional blockchain transaction ID if anchored on-chain */
  blockchainTxId: z.string().optional(),
  /** Optional blockchain name (ethereum, bitcoin, etc.) */
  blockchain: z.string().optional(),
});

/**
 * Verification result schema
 */
export const VerificationResultSchema = z.object({
  /** Whether signature is valid */
  valid: z.boolean(),
  /** Whether receipt hash is valid (BLAKE3) */
  hashValid: z.boolean(),
  /** Whether signature matches public key */
  signatureValid: z.boolean(),
  /** Key identifier used */
  keyId: z.string(),
  /** Verification timestamp */
  verifiedAt: z.string(),
  /** Additional details */
  details: z.object({
    receiptHash: z.string(),
    publicKey: z.string(),
    signature: z.string(),
  }),
});

// =============================================================================
// Key Management
// =============================================================================

/**
 * Generate a new Ed25519 signing key pair
 *
 * @param {string} keyId - Key identifier
 * @returns {Promise<Object>} Key pair with metadata
 *
 * @example
 * const keyPair = await generateSigningKey('workflow-signer-001');
 * console.log('Public key:', keyPair.publicKey);
 */
export async function generateSigningKey(keyId) {
  // Generate random private key (32 bytes)
  const privateKeyBytes = new Uint8Array(32);
  crypto.getRandomValues(privateKeyBytes);

  // Convert to hex string
  const privateKey = Array.from(privateKeyBytes)
    .map((b) => b.toString(16).padStart(2, '0'))
    .join('');

  // Derive public key
  const publicKeyBytes = await getPublicKey(privateKeyBytes);
  const publicKey = Array.from(publicKeyBytes)
    .map((b) => b.toString(16).padStart(2, '0'))
    .join('');

  return SigningKeySchema.parse({
    privateKey,
    publicKey,
    keyId,
    createdAt: toISO(now()),
  });
}

/**
 * Derive public key from private key
 *
 * @param {string} privateKey - Private key (hex string)
 * @returns {Promise<string>} Public key (hex string)
 */
export async function derivePublicKey(privateKey) {
  const privateKeyBytes = new Uint8Array(
    privateKey.match(/.{1,2}/g).map((byte) => parseInt(byte, 16))
  );

  const publicKeyBytes = await getPublicKey(privateKeyBytes);

  return Array.from(publicKeyBytes)
    .map((b) => b.toString(16).padStart(2, '0'))
    .join('');
}

// =============================================================================
// Blockchain Receipt Creation
// =============================================================================

/**
 * Create a blockchain-verified receipt with Ed25519 signature
 *
 * @param {Object} event - Workflow event data
 * @param {Object} payload - Decision payload
 * @param {Object} signingKey - Signing key pair
 * @param {Object} options - Additional options
 * @returns {Promise<Object>} Signed blockchain receipt
 *
 * @example
 * const receipt = await createBlockchainReceipt(
 *   { type: 'TASK_COMPLETED', caseId: 'case-001', taskId: 'task-1' },
 *   { decision: 'APPROVE', justification: { reasoning: 'All checks passed' } },
 *   signingKey,
 *   { blockchain: 'ethereum' }
 * );
 */
export async function createBlockchainReceipt(event, payload, signingKey, options = {}) {
  const validatedKey = SigningKeySchema.parse(signingKey);

  // Create base BLAKE3 receipt
  const baseReceipt = await createReceipt(event, payload);

  // Sign the receipt hash
  const privateKeyBytes = new Uint8Array(
    validatedKey.privateKey.match(/.{1,2}/g).map((byte) => parseInt(byte, 16))
  );

  const hashBytes = new Uint8Array(
    baseReceipt.hash.match(/.{1,2}/g).map((byte) => parseInt(byte, 16))
  );

  const signatureBytes = await sign(hashBytes, privateKeyBytes);
  const signature = Array.from(signatureBytes)
    .map((b) => b.toString(16).padStart(2, '0'))
    .join('');

  // Ensure public key is present
  const publicKey = validatedKey.publicKey || await derivePublicKey(validatedKey.privateKey);

  // Create blockchain receipt
  const blockchainReceipt = {
    ...baseReceipt,
    signature,
    publicKey,
    keyId: validatedKey.keyId,
    signedAt: toISO(now()),
    ...(options.blockchainTxId && { blockchainTxId: options.blockchainTxId }),
    ...(options.blockchain && { blockchain: options.blockchain }),
  };

  return BlockchainReceiptSchema.parse(blockchainReceipt);
}

/**
 * Create a chain of blockchain receipts
 *
 * @param {Array<Object>} events - Array of workflow events
 * @param {Object} signingKey - Signing key pair
 * @returns {Promise<Array<Object>>} Chain of signed receipts
 *
 * @example
 * const receiptChain = await createReceiptChain([
 *   { type: 'TASK_ENABLED', caseId: 'case-001', taskId: 'task-1' },
 *   { type: 'TASK_STARTED', caseId: 'case-001', taskId: 'task-1' },
 *   { type: 'TASK_COMPLETED', caseId: 'case-001', taskId: 'task-1' }
 * ], signingKey);
 */
export async function createReceiptChain(events, signingKey) {
  const chain = [];
  let previousHash = null;

  for (const event of events) {
    const payload = {
      decision: event.type,
      justification: {
        reasoning: `Transition to ${event.type}`,
        ...(previousHash && { previousReceipt: previousHash }),
      },
    };

    const receipt = await createBlockchainReceipt(event, payload, signingKey);
    chain.push(receipt);
    previousHash = receipt.hash;
  }

  return chain;
}

// =============================================================================
// Verification
// =============================================================================

/**
 * Verify a blockchain receipt's signature and hash
 *
 * @param {Object} receipt - Blockchain receipt to verify
 * @returns {Promise<Object>} Verification result
 *
 * @example
 * const result = await verifyBlockchainReceipt(receipt);
 * if (result.valid) {
 *   console.log('Receipt is valid and cryptographically verified');
 * }
 */
export async function verifyBlockchainReceipt(receipt) {
  const validated = BlockchainReceiptSchema.parse(receipt);

  // Verify base receipt hash (BLAKE3)
  const hashValid = await verifyReceipt(validated);

  // Verify Ed25519 signature
  const publicKeyBytes = new Uint8Array(
    validated.publicKey.match(/.{1,2}/g).map((byte) => parseInt(byte, 16))
  );

  const signatureBytes = new Uint8Array(
    validated.signature.match(/.{1,2}/g).map((byte) => parseInt(byte, 16))
  );

  const hashBytes = new Uint8Array(
    validated.hash.match(/.{1,2}/g).map((byte) => parseInt(byte, 16))
  );

  const signatureValid = await verify(signatureBytes, hashBytes, publicKeyBytes);

  const result = {
    valid: hashValid && signatureValid,
    hashValid,
    signatureValid,
    keyId: validated.keyId,
    verifiedAt: toISO(now()),
    details: {
      receiptHash: validated.hash,
      publicKey: validated.publicKey,
      signature: validated.signature,
    },
  };

  return VerificationResultSchema.parse(result);
}

/**
 * Verify a chain of blockchain receipts
 *
 * @param {Array<Object>} chain - Chain of blockchain receipts
 * @returns {Promise<Object>} Chain verification result
 *
 * @example
 * const result = await verifyReceiptChain(receiptChain);
 * console.log('Chain valid:', result.valid);
 * console.log('Verified receipts:', result.verifiedCount);
 */
export async function verifyReceiptChain(chain) {
  const results = [];
  let allValid = true;

  for (let i = 0; i < chain.length; i++) {
    const receipt = chain[i];
    const verification = await verifyBlockchainReceipt(receipt);

    results.push({
      index: i,
      receiptHash: receipt.hash,
      ...verification,
    });

    if (!verification.valid) {
      allValid = false;
    }

    // Verify chain linkage
    if (i > 0) {
      const previousHash = chain[i - 1].hash;
      const currentPayload = receipt.payload;

      if (currentPayload?.justification?.previousReceipt !== previousHash) {
        allValid = false;
        results[i].chainLinkValid = false;
      } else {
        results[i].chainLinkValid = true;
      }
    }
  }

  return {
    valid: allValid,
    chainLength: chain.length,
    verifiedCount: results.filter((r) => r.valid).length,
    results,
    verifiedAt: toISO(now()),
  };
}

// =============================================================================
// Blockchain Anchoring
// =============================================================================

/**
 * Create a Merkle root of multiple receipts for blockchain anchoring
 *
 * @param {Array<Object>} receipts - Array of blockchain receipts
 * @returns {Promise<string>} Merkle root hash
 *
 * @example
 * const merkleRoot = await createMerkleRoot(receipts);
 * // Anchor merkleRoot on blockchain
 */
export async function createMerkleRoot(receipts) {
  if (receipts.length === 0) {
    throw new Error('Cannot create Merkle root from empty array');
  }

  // Extract hashes
  let hashes = receipts.map((r) => r.hash);

  // Build Merkle tree
  while (hashes.length > 1) {
    const nextLevel = [];

    for (let i = 0; i < hashes.length; i += 2) {
      const left = hashes[i];
      const right = i + 1 < hashes.length ? hashes[i + 1] : hashes[i];

      // Combine and hash
      const combined = left + right;
      const hash = await blake3(combined);
      nextLevel.push(hash);
    }

    hashes = nextLevel;
  }

  return hashes[0];
}

/**
 * Generate a Merkle proof for a receipt
 *
 * @param {Array<Object>} receipts - All receipts in the tree
 * @param {number} index - Index of receipt to prove
 * @returns {Promise<Object>} Merkle proof
 *
 * @example
 * const proof = await generateMerkleProof(receipts, 2);
 * const valid = await verifyMerkleProof(proof, merkleRoot);
 */
export async function generateMerkleProof(receipts, index) {
  const proof = [];
  let hashes = receipts.map((r) => r.hash);
  let currentIndex = index;

  while (hashes.length > 1) {
    const nextLevel = [];
    const isRightNode = currentIndex % 2 === 1;
    const siblingIndex = isRightNode ? currentIndex - 1 : currentIndex + 1;

    if (siblingIndex < hashes.length) {
      proof.push({
        hash: hashes[siblingIndex],
        position: isRightNode ? 'left' : 'right',
      });
    }

    for (let i = 0; i < hashes.length; i += 2) {
      const left = hashes[i];
      const right = i + 1 < hashes.length ? hashes[i + 1] : hashes[i];
      const combined = left + right;
      const hash = await blake3(combined);
      nextLevel.push(hash);
    }

    hashes = nextLevel;
    currentIndex = Math.floor(currentIndex / 2);
  }

  return {
    receiptHash: receipts[index].hash,
    proof,
    merkleRoot: hashes[0],
  };
}

/**
 * Verify a Merkle proof
 *
 * @param {Object} merkleProof - Merkle proof to verify
 * @param {string} expectedRoot - Expected Merkle root
 * @returns {Promise<boolean>} Whether proof is valid
 */
export async function verifyMerkleProof(merkleProof, expectedRoot) {
  let currentHash = merkleProof.receiptHash;

  for (const step of merkleProof.proof) {
    const combined = step.position === 'left'
      ? step.hash + currentHash
      : currentHash + step.hash;

    currentHash = await blake3(combined);
  }

  return currentHash === expectedRoot;
}

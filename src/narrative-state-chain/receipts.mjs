/**
 * @fileoverview Receipt Generation - Cryptographic receipts for scenes
 *
 * **Purpose**: Generate tamper-proof receipts for scene admissibility
 * - generate_receipt(scene, admissibility_results, reconciliation_results) → Receipt
 * - sign_receipt(receipt, signing_key) → signed_receipt
 * - verify_receipt(receipt, public_key) → {valid, tamper_detected}
 * - hash_receipt(receipt) → commit_hash
 *
 * **Design**:
 * - BLAKE3 hashing for receipt integrity
 * - Optional crypto signing (Ed25519 or similar)
 * - Merkle-style chaining (previousReceiptHash)
 *
 * @module narrative-state-chain/receipts
 */

import { blake3 } from 'hash-wasm';
import { createSign, createVerify, randomBytes } from 'crypto';

/**
 * Generate a receipt for a scene
 *
 * **Process**:
 * 1. Collect admissibility checks
 * 2. Compute minimality proof
 * 3. Hash receipt content
 * 4. Link to previous receipt (if any)
 *
 * @param {Object} options - Receipt generation options
 * @param {string} options.sceneId - Scene UUID
 * @param {string} options.universeId - Universe UUID
 * @param {import('./types.mjs').GuardResult[]} options.admissibilityChecks - Guard results
 * @param {Object} options.delta - State delta
 * @param {import('./types.mjs').Receipt} [options.previousReceipt] - Previous receipt in chain
 * @returns {Promise<import('./types.mjs').Receipt>} Generated receipt
 *
 * @example
 * const receipt = await generateReceipt({
 *   sceneId: 'scene-uuid',
 *   universeId: 'universe-uuid',
 *   admissibilityChecks: guardResults,
 *   delta: { key: 'value' },
 *   previousReceipt: lastReceipt
 * });
 */
export async function generateReceipt(options) {
  const {
    sceneId,
    universeId,
    admissibilityChecks,
    delta,
    previousReceipt,
  } = options;

  const timestamp = new Date();

  // Compute minimality proof (hash of delta)
  const minimalityProof = await computeMinimalityProof(delta);

  // Build receipt
  const receiptData = {
    sceneId,
    universeId,
    timestamp,
    admissibilityChecks,
    minimalityProof,
    forkParents: [], // TODO: Handle forks
    previousReceiptHash: previousReceipt?.receiptHash || null,
  };

  // Compute receipt hash
  const receiptHash = await hashReceipt(receiptData);

  return {
    ...receiptData,
    receiptHash,
  };
}

/**
 * Compute minimality proof for a delta
 *
 * **Proof**: BLAKE3 hash of canonical delta JSON
 *
 * @param {Object} delta - State delta
 * @returns {Promise<string>} Minimality proof hash
 *
 * @example
 * const proof = await computeMinimalityProof({ key: 'value' });
 */
export async function computeMinimalityProof(delta) {
  const canonical = JSON.stringify(delta, Object.keys(delta).sort());
  return blake3(canonical);
}

/**
 * Hash a receipt (for integrity and chaining)
 *
 * **Algorithm**: BLAKE3(canonical JSON of receipt)
 *
 * @param {Object} receiptData - Receipt data to hash
 * @returns {Promise<string>} BLAKE3 hash
 *
 * @example
 * const hash = await hashReceipt(receipt);
 */
export async function hashReceipt(receiptData) {
  // Canonical JSON (sorted keys)
  const canonical = JSON.stringify({
    sceneId: receiptData.sceneId,
    universeId: receiptData.universeId,
    timestamp: receiptData.timestamp.toISOString(),
    admissibilityChecks: receiptData.admissibilityChecks,
    minimalityProof: receiptData.minimalityProof,
    forkParents: receiptData.forkParents,
    previousReceiptHash: receiptData.previousReceiptHash,
  });

  return blake3(canonical);
}

/**
 * Sign a receipt with a private key (Ed25519)
 *
 * **Note**: This is a placeholder implementation using Node's crypto.
 * For production, use a proper Ed25519 library or HSM.
 *
 * @param {import('./types.mjs').Receipt} receipt - Receipt to sign
 * @param {string|Buffer} signingKey - Private key (PEM format or Buffer)
 * @returns {Promise<import('./types.mjs').Receipt>} Signed receipt
 *
 * @example
 * const signedReceipt = await signReceipt(receipt, privateKey);
 */
export async function signReceipt(receipt, signingKey) {
  try {
    // Use SHA256 for signing (Node.js built-in)
    const sign = createSign('SHA256');
    sign.update(receipt.receiptHash);
    sign.end();

    const signature = sign.sign(signingKey, 'hex');

    return {
      ...receipt,
      signature,
    };
  } catch (error) {
    throw new Error(`Failed to sign receipt: ${error.message}`);
  }
}

/**
 * Verify a receipt's signature
 *
 * @param {import('./types.mjs').Receipt} receipt - Receipt to verify
 * @param {string|Buffer} publicKey - Public key (PEM format or Buffer)
 * @returns {Promise<{valid: boolean, tamperDetected: boolean}>} Verification result
 *
 * @example
 * const result = await verifyReceipt(signedReceipt, publicKey);
 * if (!result.valid) {
 *   console.error('Invalid signature!');
 * }
 */
export async function verifyReceipt(receipt, publicKey) {
  try {
    if (!receipt.signature) {
      return {
        valid: false,
        tamperDetected: false,
        error: 'No signature present',
      };
    }

    // Recompute hash
    const expectedHash = await hashReceipt(receipt);

    // Check if receipt hash matches
    if (expectedHash !== receipt.receiptHash) {
      return {
        valid: false,
        tamperDetected: true,
        error: 'Receipt hash mismatch',
      };
    }

    // Verify signature
    const verify = createVerify('SHA256');
    verify.update(receipt.receiptHash);
    verify.end();

    const valid = verify.verify(publicKey, receipt.signature, 'hex');

    return {
      valid,
      tamperDetected: !valid,
    };
  } catch (error) {
    return {
      valid: false,
      tamperDetected: false,
      error: error.message,
    };
  }
}

/**
 * Verify receipt chain integrity
 *
 * **Process**: Check that each receipt's previousReceiptHash matches the previous receipt's hash
 *
 * @param {import('./types.mjs').Receipt[]} receipts - Array of receipts in chronological order
 * @returns {Promise<{valid: boolean, errors: string[]}>} Chain verification result
 *
 * @example
 * const result = await verifyReceiptChain(scene.receipts);
 * if (!result.valid) {
 *   console.error('Chain broken:', result.errors);
 * }
 */
export async function verifyReceiptChain(receipts) {
  const errors = [];

  for (let i = 1; i < receipts.length; i++) {
    const current = receipts[i];
    const previous = receipts[i - 1];

    if (current.previousReceiptHash !== previous.receiptHash) {
      errors.push(
        `Chain break at index ${i}: expected ${previous.receiptHash}, got ${current.previousReceiptHash}`
      );
    }

    // Verify hash integrity
    const expectedHash = await hashReceipt(current);
    if (expectedHash !== current.receiptHash) {
      errors.push(`Receipt hash mismatch at index ${i}`);
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

/**
 * Generate a mock signing key pair (for testing)
 *
 * **CAUTION**: Not cryptographically secure. Use only for testing.
 *
 * @returns {{privateKey: string, publicKey: string}} Key pair in PEM format
 *
 * @example
 * const { privateKey, publicKey } = generateMockKeyPair();
 * const signed = await signReceipt(receipt, privateKey);
 * const verified = await verifyReceipt(signed, publicKey);
 */
export function generateMockKeyPair() {
  // This is a placeholder - in real implementation, use proper key generation
  const { generateKeyPairSync } = require('crypto');

  const { privateKey, publicKey } = generateKeyPairSync('rsa', {
    modulusLength: 2048,
    publicKeyEncoding: {
      type: 'spki',
      format: 'pem',
    },
    privateKeyEncoding: {
      type: 'pkcs8',
      format: 'pem',
    },
  });

  return { privateKey, publicKey };
}

/**
 * Compute Merkle root from multiple receipts
 *
 * **Use case**: Batch multiple scene receipts into a single commitment
 *
 * @param {import('./types.mjs').Receipt[]} receipts - Receipts to batch
 * @returns {Promise<string>} Merkle root hash
 *
 * @example
 * const root = await computeReceiptMerkleRoot([receipt1, receipt2, receipt3]);
 */
export async function computeReceiptMerkleRoot(receipts) {
  if (receipts.length === 0) {
    throw new Error('Cannot compute Merkle root of empty array');
  }

  if (receipts.length === 1) {
    return receipts[0].receiptHash;
  }

  // Build tree bottom-up
  let currentLevel = receipts.map(r => r.receiptHash);

  while (currentLevel.length > 1) {
    const nextLevel = [];

    for (let i = 0; i < currentLevel.length; i += 2) {
      const left = currentLevel[i];
      const right = currentLevel[i + 1] || currentLevel[i]; // Duplicate if odd

      const combined = left + right;
      const parentHash = await blake3(combined);
      nextLevel.push(parentHash);
    }

    currentLevel = nextLevel;
  }

  return currentLevel[0];
}

/**
 * Convert receipt to JSON-LD format
 *
 * @param {import('./types.mjs').Receipt} receipt - Receipt to convert
 * @returns {Object} JSON-LD representation
 *
 * @example
 * const jsonld = receiptToJSONLD(receipt);
 * console.log(JSON.stringify(jsonld, null, 2));
 */
export function receiptToJSONLD(receipt) {
  return {
    '@context': {
      '@vocab': 'http://example.org/narrative-state-chain#',
      'sceneId': 'http://example.org/narrative-state-chain#scene',
      'universeId': 'http://example.org/narrative-state-chain#universe',
      'timestamp': {
        '@id': 'http://purl.org/dc/terms/created',
        '@type': 'http://www.w3.org/2001/XMLSchema#dateTime',
      },
    },
    '@type': 'Receipt',
    sceneId: receipt.sceneId,
    universeId: receipt.universeId,
    timestamp: receipt.timestamp.toISOString(),
    admissibilityChecks: receipt.admissibilityChecks,
    minimalityProof: receipt.minimalityProof,
    forkParents: receipt.forkParents,
    receiptHash: receipt.receiptHash,
    signature: receipt.signature,
  };
}

/**
 * @fileoverview Receipt generation and verification for proof system.
 * Receipts form a hash chain linking capsules together.
 */

import { sha256Hex, sha256Prefixed, isValidSha256Hex } from './hash.mjs';
import { canonicalize } from './canonicalize.mjs';
import { extractCapsuleContent } from './capsule.mjs';

/**
 * @typedef {Object} Receipt
 * @property {string} id - Unique receipt identifier
 * @property {number} timestamp - Unix timestamp in milliseconds
 * @property {string} capsuleHash - SHA-256 hash of capsule content
 * @property {string|null} previousReceiptHash - Hash of previous receipt (null for first)
 * @property {number} chainIndex - Position in chain (0-based)
 * @property {string|null} signature - Optional cryptographic signature
 */

/**
 * Generate receipt for a capsule.
 * Creates hash chain by linking to previous receipt.
 *
 * @param {Object} capsule - Capsule to generate receipt for
 * @param {Receipt|null} previousReceipt - Previous receipt in chain (null for first)
 * @returns {Receipt} New receipt
 * @throws {Error} If capsule is invalid or previous receipt is malformed
 */
export function generateReceipt(capsule, previousReceipt = null) {
  if (!capsule || typeof capsule !== 'object') {
    throw new Error('generateReceipt requires a capsule object');
  }

  // Extract content without receipt to avoid circular dependency
  const content = extractCapsuleContent(capsule);
  const contentJson = canonicalize(content);
  const capsuleHash = sha256Prefixed(contentJson);

  const timestamp = Date.now();
  let chainIndex = 0;
  let previousReceiptHash = null;

  if (previousReceipt) {
    if (!isValidReceipt(previousReceipt)) {
      throw new Error('Invalid previous receipt');
    }

    chainIndex = previousReceipt.chainIndex + 1;

    // Hash the previous receipt
    const previousReceiptJson = canonicalize({
      id: previousReceipt.id,
      timestamp: previousReceipt.timestamp,
      capsuleHash: previousReceipt.capsuleHash,
      previousReceiptHash: previousReceipt.previousReceiptHash,
      chainIndex: previousReceipt.chainIndex,
    });
    previousReceiptHash = sha256Prefixed(previousReceiptJson);
  }

  const receiptId = generateReceiptId(timestamp, chainIndex);

  return {
    id: receiptId,
    timestamp,
    capsuleHash,
    previousReceiptHash,
    chainIndex,
    signature: null, // Signature not implemented yet
  };
}

/**
 * Generate unique receipt ID.
 *
 * @param {number} timestamp - Unix timestamp in milliseconds
 * @param {number} chainIndex - Position in chain
 * @returns {string} Receipt ID in format: receipt_{timestamp}_{index}
 */
function generateReceiptId(timestamp, chainIndex) {
  const timestampSec = Math.floor(timestamp / 1000);
  const indexHex = chainIndex.toString(16).padStart(8, '0');
  return `receipt_${timestampSec}_${indexHex}`;
}

/**
 * Verify receipt integrity against capsule.
 * Checks that receipt hash matches capsule content.
 *
 * @param {Receipt} receipt - Receipt to verify
 * @param {Object} capsule - Capsule to verify against
 * @returns {boolean} True if receipt is valid for capsule
 * @throws {Error} If receipt or capsule is invalid
 */
export function verifyReceipt(receipt, capsule) {
  if (!isValidReceipt(receipt)) {
    throw new Error('Invalid receipt structure');
  }

  if (!capsule || typeof capsule !== 'object') {
    throw new Error('verifyReceipt requires a capsule object');
  }

  // Recompute capsule hash
  const content = extractCapsuleContent(capsule);
  const contentJson = canonicalize(content);
  const expectedHash = sha256Prefixed(contentJson);

  // Compare with receipt's capsule hash
  return receipt.capsuleHash === expectedHash;
}

/**
 * Verify entire receipt chain.
 * Checks that each receipt correctly links to previous receipt.
 *
 * @param {Array<Receipt>} receipts - Array of receipts to verify (in order)
 * @returns {Object} Verification result
 * @property {boolean} valid - True if entire chain is valid
 * @property {number} verifiedCount - Number of receipts verified
 * @property {Array<string>} errors - Array of error messages
 */
export function verifyChain(receipts) {
  if (!Array.isArray(receipts)) {
    return {
      valid: false,
      verifiedCount: 0,
      errors: ['receipts must be an array'],
    };
  }

  if (receipts.length === 0) {
    return {
      valid: true,
      verifiedCount: 0,
      errors: [],
    };
  }

  const errors = [];

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];

    // Validate receipt structure
    try {
      if (!isValidReceipt(receipt)) {
        errors.push(`Receipt ${i}: Invalid structure`);
        continue;
      }
    } catch (error) {
      errors.push(`Receipt ${i}: ${error.message}`);
      continue;
    }

    // Check chain index
    if (receipt.chainIndex !== i) {
      errors.push(`Receipt ${i}: Expected chainIndex ${i}, got ${receipt.chainIndex}`);
    }

    // Check previous receipt hash
    if (i === 0) {
      if (receipt.previousReceiptHash !== null) {
        errors.push(`Receipt ${i}: First receipt should have null previousReceiptHash`);
      }
    } else {
      const previousReceipt = receipts[i - 1];

      // Compute expected previous hash
      const previousReceiptJson = canonicalize({
        id: previousReceipt.id,
        timestamp: previousReceipt.timestamp,
        capsuleHash: previousReceipt.capsuleHash,
        previousReceiptHash: previousReceipt.previousReceiptHash,
        chainIndex: previousReceipt.chainIndex,
      });
      const expectedPreviousHash = sha256Prefixed(previousReceiptJson);

      if (receipt.previousReceiptHash !== expectedPreviousHash) {
        errors.push(`Receipt ${i}: Previous receipt hash mismatch`);
      }
    }
  }

  return {
    valid: errors.length === 0,
    verifiedCount: receipts.length - errors.length,
    errors,
  };
}

/**
 * Check if receipt has valid structure.
 *
 * @param {any} receipt - Receipt to validate
 * @returns {boolean} True if structure is valid
 */
function isValidReceipt(receipt) {
  if (!receipt || typeof receipt !== 'object') {
    return false;
  }

  if (!receipt.id || typeof receipt.id !== 'string') {
    return false;
  }

  if (typeof receipt.timestamp !== 'number' || receipt.timestamp <= 0) {
    return false;
  }

  if (typeof receipt.capsuleHash !== 'string') {
    return false;
  }

  // Extract hash without prefix for validation
  const hashWithoutPrefix = receipt.capsuleHash.replace(/^sha256:/, '');
  if (!isValidSha256Hex(hashWithoutPrefix)) {
    return false;
  }

  if (receipt.previousReceiptHash !== null && typeof receipt.previousReceiptHash !== 'string') {
    return false;
  }

  if (receipt.previousReceiptHash !== null) {
    const prevHashWithoutPrefix = receipt.previousReceiptHash.replace(/^sha256:/, '');
    if (!isValidSha256Hex(prevHashWithoutPrefix)) {
      return false;
    }
  }

  if (typeof receipt.chainIndex !== 'number' || receipt.chainIndex < 0) {
    return false;
  }

  return true;
}

/**
 * Get receipt hash (for linking to next receipt).
 *
 * @param {Receipt} receipt - Receipt to hash
 * @returns {string} SHA-256 hash with prefix
 */
export function getReceiptHash(receipt) {
  if (!isValidReceipt(receipt)) {
    throw new Error('Invalid receipt');
  }

  const receiptJson = canonicalize({
    id: receipt.id,
    timestamp: receipt.timestamp,
    capsuleHash: receipt.capsuleHash,
    previousReceiptHash: receipt.previousReceiptHash,
    chainIndex: receipt.chainIndex,
  });

  return sha256Prefixed(receiptJson);
}

/**
 * Create receipt chain from capsules.
 * Generates receipts for all capsules, linking them together.
 *
 * @param {Array<Object>} capsules - Array of capsules
 * @returns {Array<Receipt>} Array of receipts
 */
export function createReceiptChain(capsules) {
  if (!Array.isArray(capsules)) {
    throw new Error('createReceiptChain requires an array of capsules');
  }

  const receipts = [];
  let previousReceipt = null;

  for (const capsule of capsules) {
    const receipt = generateReceipt(capsule, previousReceipt);
    receipts.push(receipt);
    previousReceipt = receipt;
  }

  return receipts;
}

/**
 * Get receipt summary for logging/debugging.
 *
 * @param {Receipt} receipt - Receipt to summarize
 * @returns {Object} Receipt summary
 */
export function getReceiptSummary(receipt) {
  return {
    id: receipt.id,
    chainIndex: receipt.chainIndex,
    timestamp: receipt.timestamp,
    timestampISO: new Date(receipt.timestamp).toISOString(),
    capsuleHash: receipt.capsuleHash.slice(0, 16) + '...',
    previousHash: receipt.previousReceiptHash
      ? receipt.previousReceiptHash.slice(0, 16) + '...'
      : null,
    hasPrevious: receipt.previousReceiptHash !== null,
    hasSignature: receipt.signature !== null,
  };
}

/**
 * Receipt Verifier - Cryptographic verification of operation receipts
 * @module agent-10/receipt-verifier
 */

import { createHash } from 'crypto';

/**
 * @typedef {Object} Receipt
 * @property {string} id - Receipt ID
 * @property {string} operation - Operation name
 * @property {*} input - Operation input
 * @property {*} output - Operation output
 * @property {number} timestamp - Operation timestamp
 * @property {string} hash - SHA-256 hash of receipt
 * @property {string|null} previousHash - Hash of previous receipt (null for first)
 */

/**
 * @typedef {Object} VerificationResult
 * @property {boolean} valid - Whether all receipts are valid
 * @property {number} totalReceipts - Total receipts verified
 * @property {number} validReceipts - Count of valid receipts
 * @property {number} tamperedReceipts - Count of tampered receipts
 * @property {Object[]} tamperedDetails - Details of tampered receipts
 * @property {boolean} chainIntact - Whether chain is intact
 * @property {string[]} chainErrors - Chain integrity errors
 */

/**
 * Compute hash for a receipt
 * @param {Receipt} receipt - Receipt object (without hash field)
 * @returns {string} SHA-256 hash
 */
export function computeReceiptHash(receipt) {
  // Create deterministic string representation
  const data = JSON.stringify({
    id: receipt.id,
    operation: receipt.operation,
    input: receipt.input,
    output: receipt.output,
    timestamp: receipt.timestamp,
    previousHash: receipt.previousHash || null,
  });

  return createHash('sha256').update(data).digest('hex');
}

/**
 * Verify a single receipt
 * @param {Receipt} receipt - Receipt to verify
 * @returns {Object} Verification result
 */
export function verifyReceipt(receipt) {
  if (!receipt || typeof receipt !== 'object') {
    return {
      valid: false,
      error: 'Receipt must be an object',
    };
  }

  if (!receipt.id) {
    return {
      valid: false,
      error: 'Receipt missing id',
    };
  }

  if (!receipt.hash) {
    return {
      valid: false,
      error: 'Receipt missing hash',
    };
  }

  // Compute expected hash
  const expectedHash = computeReceiptHash(receipt);

  if (receipt.hash !== expectedHash) {
    return {
      valid: false,
      error: 'Hash mismatch - receipt tampered',
      expectedHash,
      actualHash: receipt.hash,
    };
  }

  return {
    valid: true,
    hash: receipt.hash,
  };
}

/**
 * Verify receipt chain integrity
 * @param {Receipt[]} receipts - Array of receipts in order
 * @returns {Object} Chain verification result
 */
export function verifyChain(receipts) {
  if (!Array.isArray(receipts)) {
    return {
      valid: false,
      errors: ['Receipts must be an array'],
    };
  }

  if (receipts.length === 0) {
    return {
      valid: true,
      errors: [],
    };
  }

  const errors = [];

  // First receipt should have null previousHash
  if (receipts[0].previousHash !== null) {
    errors.push('First receipt must have previousHash: null');
  }

  // Verify chain links
  for (let i = 1; i < receipts.length; i++) {
    const current = receipts[i];
    const previous = receipts[i - 1];

    if (current.previousHash !== previous.hash) {
      errors.push(
        `Chain break at receipt ${i}: expected previousHash ${previous.hash}, got ${current.previousHash}`
      );
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

/**
 * Verify all receipts and chain integrity
 * @param {Receipt[]} receipts - Array of receipts
 * @returns {VerificationResult} Verification result
 */
export function verifyAllReceipts(receipts) {
  if (!Array.isArray(receipts)) {
    throw new Error('Receipts must be an array');
  }

  const result = {
    valid: true,
    totalReceipts: receipts.length,
    validReceipts: 0,
    tamperedReceipts: 0,
    tamperedDetails: [],
    chainIntact: true,
    chainErrors: [],
  };

  // Verify individual receipts
  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];
    const verification = verifyReceipt(receipt);

    if (verification.valid) {
      result.validReceipts++;
    } else {
      result.tamperedReceipts++;
      result.tamperedDetails.push({
        index: i,
        id: receipt.id,
        error: verification.error,
        expectedHash: verification.expectedHash,
        actualHash: verification.actualHash,
      });
    }
  }

  // Verify chain integrity
  const chainVerification = verifyChain(receipts);
  result.chainIntact = chainVerification.valid;
  result.chainErrors = chainVerification.errors;

  // Overall validity
  result.valid = result.tamperedReceipts === 0 && result.chainIntact;

  return result;
}

/**
 * Generate verification report
 * @param {Receipt[]} receipts - Array of receipts
 * @returns {Object} Detailed verification report
 */
export function generateVerificationReport(receipts) {
  const verification = verifyAllReceipts(receipts);

  return {
    timestamp: Date.now(),
    summary: {
      valid: verification.valid,
      totalReceipts: verification.totalReceipts,
      validReceipts: verification.validReceipts,
      tamperedReceipts: verification.tamperedReceipts,
      chainIntact: verification.chainIntact,
    },
    details: {
      tamperedReceipts: verification.tamperedDetails,
      chainErrors: verification.chainErrors,
    },
    verdict: verification.valid ? 'PASS' : 'FAIL',
  };
}

/**
 * Create a new receipt
 * @param {string} id - Receipt ID
 * @param {string} operation - Operation name
 * @param {*} input - Operation input
 * @param {*} output - Operation output
 * @param {string|null} previousHash - Previous receipt hash
 * @returns {Receipt} New receipt with computed hash
 */
export function createReceipt(id, operation, input, output, previousHash = null) {
  const receipt = {
    id,
    operation,
    input,
    output,
    timestamp: Date.now(),
    previousHash,
  };

  const hash = computeReceiptHash(receipt);

  return {
    ...receipt,
    hash,
  };
}

/**
 * Create receipt chain from scenario results
 * @param {Object[]} results - Array of scenario results
 * @returns {Receipt[]} Array of receipts forming a chain
 */
export function createReceiptChain(results) {
  const receipts = [];
  let previousHash = null;

  for (let i = 0; i < results.length; i++) {
    const result = results[i];

    const receipt = createReceipt(
      `receipt-${i + 1}`,
      result.name,
      result.legacyResult || result.legacyError,
      result.migratedResult || result.migratedError,
      previousHash
    );

    receipts.push(receipt);
    previousHash = receipt.hash;
  }

  return receipts;
}

/**
 * Export receipt chain as JSON
 * @param {Receipt[]} receipts - Receipt chain
 * @returns {string} JSON string
 */
export function exportReceiptChain(receipts) {
  return JSON.stringify(receipts, null, 2);
}

/**
 * Import receipt chain from JSON
 * @param {string} json - JSON string
 * @returns {Receipt[]} Receipt chain
 */
export function importReceiptChain(json) {
  const receipts = JSON.parse(json);

  if (!Array.isArray(receipts)) {
    throw new Error('Invalid receipt chain: must be an array');
  }

  return receipts;
}

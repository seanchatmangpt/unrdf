/**
 * @fileoverview Tamper detection for proof system.
 * Detects modifications to capsules and receipt chains.
 */

import { verifyReceipt, verifyChain, getReceiptHash } from './receipt.mjs';
import { sha256Prefixed } from './hash.mjs';
import { canonicalize } from './canonicalize.mjs';
import { extractCapsuleContent } from './capsule.mjs';

/**
 * @typedef {Object} TamperReport
 * @property {boolean} tampered - True if tampering detected
 * @property {Array<string>} issues - Array of detected issues
 * @property {Object} details - Detailed tamper information
 */

/**
 * Detect tampering in a single capsule-receipt pair.
 *
 * @param {Object} capsule - Capsule to check
 * @param {Object} receipt - Receipt to verify against
 * @returns {TamperReport} Tamper detection report
 */
export function detectTamper(capsule, receipt) {
  const issues = [];
  const details = {};

  // Check if capsule exists
  if (!capsule || typeof capsule !== 'object') {
    issues.push('Capsule is missing or invalid');
    return {
      tampered: true,
      issues,
      details: { error: 'No capsule to verify' },
    };
  }

  // Check if receipt exists
  if (!receipt || typeof receipt !== 'object') {
    issues.push('Receipt is missing or invalid');
    return {
      tampered: true,
      issues,
      details: { error: 'No receipt to verify' },
    };
  }

  // Verify receipt structure
  try {
    verifyReceipt(receipt, capsule);
  } catch (error) {
    issues.push(`Receipt verification failed: ${error.message}`);
  }

  // Compute expected capsule hash
  const content = extractCapsuleContent(capsule);
  const contentJson = canonicalize(content);
  const expectedHash = sha256Prefixed(contentJson);

  details.expectedHash = expectedHash;
  details.actualHash = receipt.capsuleHash;

  if (expectedHash !== receipt.capsuleHash) {
    issues.push('Capsule hash mismatch - capsule has been modified');
    details.hashMismatch = true;
  }

  // Check receipt attached to capsule matches provided receipt
  if (capsule.receipt) {
    const capsuleReceiptHash = getReceiptHash(capsule.receipt);
    const providedReceiptHash = getReceiptHash(receipt);

    if (capsuleReceiptHash !== providedReceiptHash) {
      issues.push('Attached receipt does not match verification receipt');
      details.receiptMismatch = true;
    }
  } else {
    issues.push('Capsule has no attached receipt');
    details.noReceipt = true;
  }

  return {
    tampered: issues.length > 0,
    issues,
    details,
  };
}

/**
 * Audit entire capsule chain for tampering.
 * Performs comprehensive integrity check of capsules and receipts.
 *
 * @param {Array<Object>} capsules - Array of capsules with receipts
 * @returns {Object} Audit report
 */
export function auditChain(capsules) {
  if (!Array.isArray(capsules)) {
    return {
      valid: false,
      tampered: true,
      totalCapsules: 0,
      validCapsules: 0,
      errors: ['capsules must be an array'],
      capsuleReports: [],
      chainReport: null,
    };
  }

  if (capsules.length === 0) {
    return {
      valid: true,
      tampered: false,
      totalCapsules: 0,
      validCapsules: 0,
      errors: [],
      capsuleReports: [],
      chainReport: { valid: true, verifiedCount: 0, errors: [] },
    };
  }

  const errors = [];
  const capsuleReports = [];

  // Verify each capsule individually
  for (let i = 0; i < capsules.length; i++) {
    const capsule = capsules[i];

    if (!capsule.receipt) {
      const report = {
        index: i,
        capsuleId: capsule.meta?.id || 'unknown',
        tampered: true,
        issues: ['Missing receipt'],
        details: {},
      };
      capsuleReports.push(report);
      errors.push(`Capsule ${i}: Missing receipt`);
      continue;
    }

    const tamperReport = detectTamper(capsule, capsule.receipt);
    capsuleReports.push({
      index: i,
      capsuleId: capsule.meta?.id || 'unknown',
      ...tamperReport,
    });

    if (tamperReport.tampered) {
      errors.push(`Capsule ${i}: ${tamperReport.issues.join(', ')}`);
    }
  }

  // Verify receipt chain
  const receipts = capsules
    .filter(c => c.receipt)
    .map(c => c.receipt);

  const chainReport = verifyChain(receipts);

  if (!chainReport.valid) {
    errors.push(...chainReport.errors.map(e => `Chain: ${e}`));
  }

  // Check for gaps in chain
  const expectedIndices = new Set(Array.from({ length: capsules.length }, (_, i) => i));
  const actualIndices = new Set(receipts.map(r => r.chainIndex));

  const missingIndices = [...expectedIndices].filter(i => !actualIndices.has(i));
  if (missingIndices.length > 0) {
    errors.push(`Chain: Missing indices ${missingIndices.join(', ')}`);
  }

  const validCapsules = capsuleReports.filter(r => !r.tampered).length;

  return {
    valid: errors.length === 0,
    tampered: errors.length > 0,
    totalCapsules: capsules.length,
    validCapsules,
    errors,
    capsuleReports,
    chainReport,
  };
}

/**
 * Find first point of tampering in chain.
 *
 * @param {Array<Object>} capsules - Array of capsules
 * @returns {Object|null} First tampered capsule info or null if none
 */
export function findFirstTamper(capsules) {
  const audit = auditChain(capsules);

  if (!audit.tampered) {
    return null;
  }

  const firstTampered = audit.capsuleReports.find(r => r.tampered);

  return firstTampered || null;
}

/**
 * Verify chain continuity (no gaps, correct ordering).
 *
 * @param {Array<Object>} capsules - Array of capsules
 * @returns {Object} Continuity check result
 */
export function verifyChainContinuity(capsules) {
  if (!Array.isArray(capsules)) {
    return {
      continuous: false,
      errors: ['capsules must be an array'],
    };
  }

  if (capsules.length === 0) {
    return {
      continuous: true,
      errors: [],
    };
  }

  const errors = [];

  // Check timestamps are increasing
  for (let i = 1; i < capsules.length; i++) {
    const prev = capsules[i - 1];
    const curr = capsules[i];

    if (!prev.meta || !curr.meta) {
      errors.push(`Capsule ${i}: Missing metadata`);
      continue;
    }

    if (curr.meta.timestamp < prev.meta.timestamp) {
      errors.push(
        `Capsule ${i}: Timestamp ${curr.meta.timestamp} is before previous ${prev.meta.timestamp}`
      );
    }
  }

  // Check receipt chain indices
  for (let i = 0; i < capsules.length; i++) {
    const capsule = capsules[i];

    if (!capsule.receipt) {
      errors.push(`Capsule ${i}: Missing receipt`);
      continue;
    }

    if (capsule.receipt.chainIndex !== i) {
      errors.push(
        `Capsule ${i}: Receipt chainIndex is ${capsule.receipt.chainIndex}, expected ${i}`
      );
    }
  }

  // Check previous receipt hash links
  for (let i = 1; i < capsules.length; i++) {
    const prev = capsules[i - 1];
    const curr = capsules[i];

    if (!prev.receipt || !curr.receipt) {
      continue; // Already reported as error above
    }

    const expectedPrevHash = getReceiptHash(prev.receipt);

    if (curr.receipt.previousReceiptHash !== expectedPrevHash) {
      errors.push(`Capsule ${i}: Previous receipt hash does not link to previous capsule`);
    }
  }

  return {
    continuous: errors.length === 0,
    errors,
  };
}

/**
 * Generate tamper-proof summary of chain.
 *
 * @param {Array<Object>} capsules - Array of capsules
 * @returns {Object} Chain summary with hash
 */
export function generateChainSummary(capsules) {
  const audit = auditChain(capsules);

  const summary = {
    totalCapsules: capsules.length,
    valid: audit.valid,
    tampered: audit.tampered,
    validCapsules: audit.validCapsules,
    invalidCapsules: capsules.length - audit.validCapsules,
    firstCapsuleId: capsules[0]?.meta?.id || null,
    lastCapsuleId: capsules[capsules.length - 1]?.meta?.id || null,
    firstTimestamp: capsules[0]?.meta?.timestamp || null,
    lastTimestamp: capsules[capsules.length - 1]?.meta?.timestamp || null,
    chainHash: null,
  };

  // Generate chain hash (hash of all receipt hashes)
  if (capsules.length > 0 && audit.valid) {
    const receiptHashes = capsules
      .filter(c => c.receipt)
      .map(c => getReceiptHash(c.receipt))
      .join('|');

    summary.chainHash = sha256Prefixed(receiptHashes);
  }

  return summary;
}

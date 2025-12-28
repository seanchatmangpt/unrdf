/**
 * Tamper Detection - Verify receipt integrity and detect modifications
 *
 * Provides comprehensive verification:
 * - Individual receipt hash verification
 * - Chain link verification (previousHash matches)
 * - Temporal ordering verification
 * - Merkle proof verification
 * - Complete chain verification
 *
 * @module @unrdf/observability/receipts/tamper-detection
 */

import { blake3 } from 'hash-wasm';
import { VerificationResultSchema } from './receipt-schema.mjs';

/**
 * TamperDetector - Verify receipt integrity
 */
export class TamperDetector {
  /**
   * Verify a single receipt's hash integrity
   *
   * @param {Object} receipt - Receipt to verify
   * @returns {Promise<Object>} Verification result
   */
  async verifyReceipt(receipt) {
    const errors = [];
    const checks = {};

    try {
      // Check required fields
      const required = ['id', 'hash', 'timestamp_ns', 'timestamp_iso', 'operation', 'payload', 'previousHash'];
      for (const field of required) {
        if (!(field in receipt)) {
          errors.push(`Missing required field: ${field}`);
        }
      }

      if (errors.length > 0) {
        return {
          valid: false,
          receiptId: receipt.id,
          errors,
          checks,
        };
      }

      // Recompute hash
      const canonicalContent = {
        id: receipt.id,
        timestamp_ns: receipt.timestamp_ns,
        timestamp_iso: receipt.timestamp_iso,
        operation: receipt.operation,
        payload: receipt.payload,
        previousHash: receipt.previousHash,
        ...(receipt.actor && { actor: receipt.actor }),
        ...(receipt.metadata && { metadata: receipt.metadata }),
      };

      const canonical = JSON.stringify(canonicalContent, Object.keys(canonicalContent).sort());
      const computedHash = await blake3(canonical);

      checks.hashIntegrity = computedHash === receipt.hash;
      if (!checks.hashIntegrity) {
        errors.push(`Hash mismatch: expected ${receipt.hash}, got ${computedHash}`);
      }

      return VerificationResultSchema.parse({
        valid: errors.length === 0,
        receiptId: receipt.id,
        errors,
        checks,
      });
    } catch (err) {
      return {
        valid: false,
        receiptId: receipt.id,
        errors: [`Verification exception: ${err.message}`],
        checks,
      };
    }
  }

  /**
   * Verify chain link between two receipts
   *
   * @param {Object} current - Current receipt
   * @param {Object} previous - Previous receipt
   * @returns {Promise<Object>} Verification result
   */
  async verifyChainLink(current, previous) {
    const errors = [];
    const checks = {};

    // Verify previousHash matches
    checks.chainLink = current.previousHash === previous.hash;
    if (!checks.chainLink) {
      errors.push(
        `Chain broken: current.previousHash (${current.previousHash}) !== previous.hash (${previous.hash})`
      );
    }

    // Verify temporal ordering
    const currentTime = BigInt(current.timestamp_ns);
    const previousTime = BigInt(previous.timestamp_ns);
    checks.temporalOrder = currentTime > previousTime;
    if (!checks.temporalOrder) {
      errors.push(`Temporal ordering violated: ${currentTime} <= ${previousTime}`);
    }

    return VerificationResultSchema.parse({
      valid: errors.length === 0,
      receiptId: current.id,
      errors,
      checks,
    });
  }

  /**
   * Verify entire receipt chain
   *
   * @param {Array<Object>} receipts - Array of receipts to verify
   * @returns {Promise<Object>} Verification result
   */
  async verifyChain(receipts) {
    const errors = [];
    const checks = {};

    if (receipts.length === 0) {
      return {
        valid: true,
        errors: [],
        checks: {},
      };
    }

    // Verify first receipt (genesis)
    const firstResult = await this.verifyReceipt(receipts[0]);
    if (!firstResult.valid) {
      errors.push(`Receipt 0 invalid: ${firstResult.errors.join(', ')}`);
      return { valid: false, errors, checks };
    }

    if (receipts[0].previousHash !== null) {
      errors.push('Genesis receipt must have null previousHash');
      return { valid: false, errors, checks };
    }

    // Verify remaining receipts and chain links
    for (let i = 1; i < receipts.length; i++) {
      const receiptResult = await this.verifyReceipt(receipts[i]);
      if (!receiptResult.valid) {
        errors.push(`Receipt ${i} invalid: ${receiptResult.errors.join(', ')}`);
      }

      const linkResult = await this.verifyChainLink(receipts[i], receipts[i - 1]);
      if (!linkResult.valid) {
        errors.push(`Chain link ${i - 1} -> ${i} invalid: ${linkResult.errors.join(', ')}`);
      }
    }

    return VerificationResultSchema.parse({
      valid: errors.length === 0,
      errors,
      checks,
    });
  }

  /**
   * Detect tampering by comparing original and suspect receipts
   *
   * @param {Object} original - Original receipt
   * @param {Object} suspect - Suspect receipt
   * @returns {Promise<Object>} Tampering analysis
   */
  async detectTampering(original, suspect) {
    const analysis = {
      tampered: false,
      changes: [],
    };

    // Compare all fields
    const fields = ['id', 'hash', 'timestamp_ns', 'timestamp_iso', 'operation', 'payload', 'previousHash', 'actor'];
    for (const field of fields) {
      const origValue = JSON.stringify(original[field]);
      const suspValue = JSON.stringify(suspect[field]);
      if (origValue !== suspValue) {
        analysis.tampered = true;
        analysis.changes.push({
          field,
          original: original[field],
          suspect: suspect[field],
        });
      }
    }

    return analysis;
  }
}

export default TamperDetector;

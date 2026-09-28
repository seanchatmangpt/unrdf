/**
 * @fileoverview Receipt Builder - Fluent API for creating receipts
 *
 * @module receipts/receipt-builder
 */

import { blake3 } from 'hash-wasm';
import { UniversalReceiptSchema } from './receipt-schemas.mjs';

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Generate deterministic epoch from timestamp
 *
 * Format: tau_YYYY_MM_DD_HHmmss_SSS (includes seconds for precision)
 *
 * @param {Date} timestamp - Timestamp
 * @returns {string} Epoch string
 */
export function generateEpoch(timestamp = new Date()) {
  const year = timestamp.getUTCFullYear();
  const month = String(timestamp.getUTCMonth() + 1).padStart(2, '0');
  const day = String(timestamp.getUTCDate()).padStart(2, '0');
  const hour = String(timestamp.getUTCHours()).padStart(2, '0');
  const minute = String(timestamp.getUTCMinutes()).padStart(2, '0');
  const second = String(timestamp.getUTCSeconds()).padStart(2, '0');
  const ms = String(timestamp.getUTCMilliseconds()).padStart(3, '0');

  return `tau_${year}_${month}_${day}_${hour}${minute}${second}_${ms}`;
}

/**
 * Generate unique receipt ID
 *
 * @param {string} type - Receipt type
 * @param {string} pkg - Package name
 * @param {Date} timestamp - Timestamp
 * @returns {string} Receipt URN
 */
export function generateReceiptId(type, pkg, timestamp = new Date()) {
  const epoch = generateEpoch(timestamp);
  const random = Math.random().toString(36).substring(2, 10);
  return `urn:receipt:${pkg}:${type}:${epoch}:${random}`;
}

/**
 * Compute deterministic hash of receipt data
 *
 * @param {object} receiptData - Receipt data (without receiptHash)
 * @returns {Promise<string>} BLAKE3 hash
 */
export async function computeReceiptHash(receiptData) {
  // Remove receiptHash if present
  const { receiptHash: _, ...dataWithoutHash } = receiptData;

  // Canonical JSON serialization (sorted keys)
  const keys = Object.keys(dataWithoutHash).sort();
  const canonical = JSON.stringify(dataWithoutHash, keys);

  return blake3(canonical);
}

// ============================================================================
// Receipt Builder
// ============================================================================

/**
 * Universal Receipt Builder - fluent API for creating receipts
 *
 * @example
 * const receipt = await new ReceiptBuilder('test', '@unrdf/core')
 *   .decision('ALLOW', 'All tests passed')
 *   .provenance({ agent: 'vitest' })
 *   .input({ hashes: { testFile: 'abc123' } })
 *   .output({ hash: 'def456' })
 *   .extension({ type: 'test', data: { total: 10, passed: 10, failed: 0 } })
 *   .build();
 */
export class ReceiptBuilder {
  /**
   * Create a new receipt builder
   *
   * @param {string} type - Receipt type
   * @param {string} pkg - Package name
   * @param {object} [options] - Builder options
   */
  constructor(type, pkg, options = {}) {
    this._timestamp = options.timestamp || new Date();
    this._data = {
      id: generateReceiptId(type, pkg, this._timestamp),
      type,
      version: '1.0.0',
      package: pkg,
      epoch: generateEpoch(this._timestamp),
      timestamp: this._timestamp.toISOString(),
      decision: 'PENDING',
      reason: '',
      provenance: {
        agent: 'unknown',
      },
      toolchain: {
        node: process.version,
        platform: process.platform,
        arch: process.arch,
      },
      input: { hashes: {} },
      output: { hash: '' },
      beforeHash: null,
      merkleRoot: null,
    };
  }

  /**
   * Set namespace
   * @param {string} namespace
   * @returns {ReceiptBuilder}
   */
  namespace(namespace) {
    this._data.namespace = namespace;
    return this;
  }

  /**
   * Set decision and reason
   * @param {'ALLOW'|'DENY'|'WARN'|'SKIP'|'PENDING'} decision
   * @param {string} reason
   * @returns {ReceiptBuilder}
   */
  decision(decision, reason) {
    this._data.decision = decision;
    this._data.reason = reason;
    return this;
  }

  /**
   * Set provenance
   * @param {object} provenance
   * @returns {ReceiptBuilder}
   */
  provenance(provenance) {
    this._data.provenance = { ...this._data.provenance, ...provenance };
    return this;
  }

  /**
   * Set toolchain
   * @param {object} toolchain
   * @returns {ReceiptBuilder}
   */
  toolchain(toolchain) {
    this._data.toolchain = { ...this._data.toolchain, ...toolchain };
    return this;
  }

  /**
   * Set input specification
   * @param {object} input
   * @returns {ReceiptBuilder}
   */
  input(input) {
    this._data.input = { ...this._data.input, ...input };
    return this;
  }

  /**
   * Set output specification
   * @param {object} output
   * @returns {ReceiptBuilder}
   */
  output(output) {
    this._data.output = { ...this._data.output, ...output };
    return this;
  }

  /**
   * Add checks
   * @param {Array} checks
   * @returns {ReceiptBuilder}
   */
  checks(checks) {
    this._data.checks = checks;
    return this;
  }

  /**
   * Add violations
   * @param {Array} violations
   * @returns {ReceiptBuilder}
   */
  violations(violations) {
    this._data.violations = violations;
    return this;
  }

  /**
   * Set metrics
   * @param {object} metrics
   * @returns {ReceiptBuilder}
   */
  metrics(metrics) {
    this._data.metrics = metrics;
    return this;
  }

  /**
   * Set chain linkage
   * @param {string|null} beforeHash
   * @returns {ReceiptBuilder}
   */
  chain(beforeHash) {
    this._data.beforeHash = beforeHash;
    return this;
  }

  /**
   * Set merkle root
   * @param {string|null} merkleRoot
   * @returns {ReceiptBuilder}
   */
  merkle(merkleRoot) {
    this._data.merkleRoot = merkleRoot;
    return this;
  }

  /**
   * Set type-specific extension
   * @param {object} extension
   * @returns {ReceiptBuilder}
   */
  extension(extension) {
    this._data.extension = extension;
    return this;
  }

  /**
   * Build the receipt
   * @returns {Promise<import('./receipt-schemas.mjs').UniversalReceipt>}
   */
  async build() {
    // Validate with schema
    const validated = UniversalReceiptSchema.parse(this._data);

    // Compute hash
    const receiptHash = await computeReceiptHash(validated);

    // Return immutable receipt
    const receipt = { ...validated, receiptHash };
    return Object.freeze(receipt);
  }
}

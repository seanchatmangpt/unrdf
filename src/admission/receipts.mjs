/**
 * @file Receipt System - Deterministic receipts with chaining and merkle batching
 * @module admission/receipts
 */

import { createHash } from 'crypto';

/**
 * Validate receipt config
 * @param {object} config
 * @returns {object}
 */
function validateReceipt(config) {
  if (!config || typeof config !== 'object') {
    throw new Error('Receipt config must be an object');
  }
  if (!['ALLOW', 'DENY'].includes(config.decision)) {
    throw new Error('Decision must be ALLOW or DENY');
  }
  return {
    id: String(config.id),
    decision: config.decision,
    deltaHash: String(config.deltaHash),
    beforeHash: String(config.beforeHash),
    afterHash: String(config.afterHash),
    epoch: Number(config.epoch),
    timestamp: Number(config.timestamp),
    toolchainVersion: String(config.toolchainVersion),
    violations: Array.isArray(config.violations) ? config.violations : [],
    reason: String(config.reason),
  };
}

/**
 * @class Receipt
 * @description Immutable receipt for admission decisions
 */
export class Receipt {
  /**
   * @param {object} config
   * @param {string} config.id - Receipt ID
   * @param {'ALLOW' | 'DENY'} config.decision - Admission decision
   * @param {string} config.deltaHash - Hash of delta
   * @param {string} config.beforeHash - Hash before delta
   * @param {string} config.afterHash - Hash after delta
   * @param {number} config.epoch - Epoch number
   * @param {number} config.timestamp - Timestamp
   * @param {string} config.toolchainVersion - Toolchain version
   * @param {string[]} [config.violations] - Violations list
   * @param {string} config.reason - Decision reason
   */
  constructor(config) {
    const validated = validateReceipt(config);
    Object.assign(this, validated);
    Object.freeze(this);
  }

  /**
   * Convert to JSON-LD
   * @returns {object}
   */
  toJSONLD() {
    return {
      '@context': {
        '@vocab': 'https://unrdf.org/receipts#',
        'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
        'xsd': 'http://www.w3.org/2001/XMLSchema#',
      },
      '@id': this.id,
      '@type': 'AdmissionReceipt',
      'decision': this.decision,
      'deltaHash': this.deltaHash,
      'beforeHash': this.beforeHash,
      'afterHash': this.afterHash,
      'epoch': {
        '@type': 'xsd:integer',
        '@value': this.epoch,
      },
      'timestamp': {
        '@type': 'xsd:dateTime',
        '@value': new Date(this.timestamp).toISOString(),
      },
      'toolchainVersion': this.toolchainVersion,
      'violations': this.violations,
      'reason': this.reason,
    };
  }

  /**
   * Create from JSON-LD
   * @param {object} jsonld - JSON-LD representation
   * @returns {Receipt}
   */
  static fromJSONLD(jsonld) {
    return new Receipt({
      id: jsonld['@id'],
      decision: jsonld.decision,
      deltaHash: jsonld.deltaHash,
      beforeHash: jsonld.beforeHash,
      afterHash: jsonld.afterHash,
      epoch: typeof jsonld.epoch === 'object' ? jsonld.epoch['@value'] : jsonld.epoch,
      timestamp: typeof jsonld.timestamp === 'object'
        ? new Date(jsonld.timestamp['@value']).getTime()
        : jsonld.timestamp,
      toolchainVersion: jsonld.toolchainVersion,
      violations: jsonld.violations || [],
      reason: jsonld.reason,
    });
  }

  /**
   * Get receipt hash
   * @returns {string}
   */
  getHash() {
    const content = JSON.stringify({
      decision: this.decision,
      deltaHash: this.deltaHash,
      beforeHash: this.beforeHash,
      afterHash: this.afterHash,
      epoch: this.epoch,
      timestamp: this.timestamp,
      toolchainVersion: this.toolchainVersion,
      violations: this.violations,
      reason: this.reason,
    });
    return createHash('sha256').update(content).digest('hex');
  }
}

/**
 * @class ReceiptGenerator
 * @description Generates deterministic receipts for admission decisions
 */
export class ReceiptGenerator {
  /**
   * @param {object} [config={}]
   * @param {string} [config.toolchainVersion='1.0.0'] - Toolchain version
   */
  constructor(config = {}) {
    this.toolchainVersion = config.toolchainVersion || '1.0.0';
    this.currentEpoch = 0;
  }

  /**
   * Generate receipt
   * @param {object} admissionResult - Admission result
   * @param {object} delta - Delta
   * @param {string} beforeHash - Hash before delta
   * @returns {Receipt}
   */
  generate(admissionResult, delta, beforeHash) {
    const deltaHash = this._hashDelta(delta);
    const afterHash = admissionResult.decision === 'ALLOW'
      ? this._computeAfterHash(beforeHash, deltaHash)
      : beforeHash;

    this.currentEpoch++;

    const receipt = new Receipt({
      id: `urn:receipt:${this.currentEpoch}:${deltaHash.slice(0, 8)}`,
      decision: admissionResult.decision,
      deltaHash,
      beforeHash,
      afterHash,
      epoch: this.currentEpoch,
      timestamp: Date.now(),
      toolchainVersion: this.toolchainVersion,
      violations: admissionResult.violations || [],
      reason: admissionResult.reason,
    });

    return receipt;
  }

  /**
   * Hash delta deterministically
   * @param {object} delta
   * @returns {string}
   * @private
   */
  _hashDelta(delta) {
    // Deterministic serialization
    const normalized = {
      additions: (delta.additions || []).sort((a, b) => {
        return (a.subject + a.predicate + a.object).localeCompare(
          b.subject + b.predicate + b.object
        );
      }),
      deletions: (delta.deletions || []).sort((a, b) => {
        return (a.subject + a.predicate + a.object).localeCompare(
          b.subject + b.predicate + b.object
        );
      }),
    };
    return createHash('sha256').update(JSON.stringify(normalized)).digest('hex');
  }

  /**
   * Compute hash after applying delta
   * @param {string} beforeHash
   * @param {string} deltaHash
   * @returns {string}
   * @private
   */
  _computeAfterHash(beforeHash, deltaHash) {
    return createHash('sha256').update(beforeHash + deltaHash).digest('hex');
  }

  /**
   * Get current epoch
   * @returns {number}
   */
  getCurrentEpoch() {
    return this.currentEpoch;
  }
}

/**
 * @class ReceiptChain
 * @description Maintains chain of receipts
 */
export class ReceiptChain {
  constructor() {
    this.receipts = [];
  }

  /**
   * Add receipt to chain
   * @param {Receipt} receipt
   */
  addReceipt(receipt) {
    this.receipts.push(receipt);
  }

  /**
   * Verify chain integrity
   * @returns {boolean}
   */
  verifyChain() {
    for (let i = 1; i < this.receipts.length; i++) {
      const prev = this.receipts[i - 1];
      const curr = this.receipts[i];

      // Verify beforeHash of current matches afterHash of previous
      if (curr.beforeHash !== prev.afterHash) {
        return false;
      }

      // Verify epochs are monotonically increasing
      if (curr.epoch <= prev.epoch) {
        return false;
      }
    }
    return true;
  }

  /**
   * Get all receipts
   * @returns {Receipt[]}
   */
  getReceipts() {
    return this.receipts;
  }
}

/**
 * @class MerkleBatcher
 * @description Computes merkle roots over receipt batches
 */
export class MerkleBatcher {
  /**
   * Compute merkle root
   * @param {Receipt[]} receipts - Receipts to batch
   * @returns {string} - Merkle root hash
   */
  computeMerkleRoot(receipts) {
    if (receipts.length === 0) {
      return createHash('sha256').update('').digest('hex');
    }

    if (receipts.length === 1) {
      return receipts[0].getHash();
    }

    let hashes = receipts.map(r => r.getHash());

    while (hashes.length > 1) {
      const nextLevel = [];
      for (let i = 0; i < hashes.length; i += 2) {
        if (i + 1 < hashes.length) {
          const combined = hashes[i] + hashes[i + 1];
          nextLevel.push(createHash('sha256').update(combined).digest('hex'));
        } else {
          nextLevel.push(hashes[i]);
        }
      }
      hashes = nextLevel;
    }

    return hashes[0];
  }

  /**
   * Create merkle batch
   * @param {Receipt[]} receipts
   * @returns {object}
   */
  createBatch(receipts) {
    return {
      receipts,
      merkleRoot: this.computeMerkleRoot(receipts),
      timestamp: Date.now(),
      count: receipts.length,
    };
  }
}

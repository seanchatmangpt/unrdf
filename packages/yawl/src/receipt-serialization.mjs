/**
 * YAWL Receipt Serialization - Legacy Receipt Class and JSON Export
 *
 * Provides backward-compatible YawlReceipt class and serialization utilities.
 *
 * @module @unrdf/yawl/receipt-serialization
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { now, toISO } from '@unrdf/kgc-4d';
import { ReceiptError } from '../errors.mjs';

// =============================================================================
// Legacy YawlReceipt Class (Backward Compatibility)
// =============================================================================

/**
 * Legacy receipt data schema
 */
const LegacyReceiptDataSchema = z.object({
  id: z.string().min(1),
  caseId: z.string().min(1),
  taskId: z.string().min(1),
  action: z.enum(['enable', 'start', 'complete', 'cancel', 'fail', 'timeout']),
  timestamp: z.bigint(),
  actor: z.string().optional(),
  beforeHash: z.string().min(1),
  afterHash: z.string().min(1),
  previousReceiptHash: z.string().optional(),
  sparqlQuery: z.string().optional(),
  sparqlResult: z.unknown().optional(),
  input: z.record(z.unknown()).optional(),
  output: z.record(z.unknown()).optional(),
  downstreamEnabled: z
    .array(
      z.object({
        taskId: z.string(),
        enabledAt: z.bigint(),
      })
    )
    .optional(),
});

/**
 * YawlReceipt - Legacy receipt class for backward compatibility
 *
 * @deprecated Use generateReceipt() and ProofChain instead
 */
export class YawlReceipt {
  /**
   * @param {Object} data - Receipt data
   */
  constructor(data) {
    const validated = LegacyReceiptDataSchema.parse(data);
    this.id = validated.id;
    this.caseId = validated.caseId;
    this.taskId = validated.taskId;
    this.action = validated.action;
    this.timestamp = validated.timestamp;
    this.actor = validated.actor;
    this.beforeHash = validated.beforeHash;
    this.afterHash = validated.afterHash;
    this.previousReceiptHash = validated.previousReceiptHash;
    this.sparqlQuery = validated.sparqlQuery;
    this.sparqlResult = validated.sparqlResult;
    this.input = validated.input;
    this.output = validated.output;
    this.downstreamEnabled = validated.downstreamEnabled ?? [];
    /** @type {string|null} Computed hash of this receipt */
    this._hash = null;
  }

  /**
   * Compute the hash of this receipt
   * @returns {Promise<string>} BLAKE3 hash
   */
  async computeHash() {
    if (this._hash) return this._hash;

    const payload = JSON.stringify({
      id: this.id,
      caseId: this.caseId,
      taskId: this.taskId,
      action: this.action,
      timestamp: this.timestamp.toString(),
      actor: this.actor,
      beforeHash: this.beforeHash,
      afterHash: this.afterHash,
      previousReceiptHash: this.previousReceiptHash,
    });

    this._hash = await blake3(payload);
    return this._hash;
  }

  /**
   * Get cached hash or compute if not available
   * @returns {Promise<string>}
   */
  async getHash() {
    return this._hash ?? this.computeHash();
  }

  /**
   * Verify this receipt's hash integrity
   * @returns {Promise<boolean>}
   */
  async verify() {
    const storedHash = this._hash;
    this._hash = null;
    const computedHash = await this.computeHash();

    if (storedHash && storedHash !== computedHash) {
      return false;
    }
    return true;
  }

  /**
   * Verify receipt chain integrity
   * @param {YawlReceipt|null} previousReceipt - Previous receipt in chain
   * @returns {Promise<{valid: boolean, reason?: string}>}
   */
  async verifyChain(previousReceipt) {
    if (!(await this.verify())) {
      return { valid: false, reason: 'Receipt hash mismatch' };
    }

    if (!previousReceipt) {
      if (this.previousReceiptHash) {
        return {
          valid: false,
          reason: 'Expected no previous receipt but previousReceiptHash is set',
        };
      }
      return { valid: true };
    }

    const previousHash = await previousReceipt.getHash();
    if (this.previousReceiptHash !== previousHash) {
      return {
        valid: false,
        reason: `Chain broken: expected ${previousHash}, got ${this.previousReceiptHash}`,
      };
    }

    return { valid: true };
  }

  /**
   * Check if this receipt is valid (has required fields)
   * @returns {boolean}
   */
  get valid() {
    return !!(
      this.id &&
      this.caseId &&
      this.taskId &&
      this.action &&
      this.timestamp &&
      this.beforeHash &&
      this.afterHash
    );
  }

  /**
   * Get timestamp as ISO string
   * @returns {string}
   */
  get timestampISO() {
    return toISO(this.timestamp);
  }

  /**
   * Serialize to JSON-compatible object
   * @returns {Object}
   */
  toJSON() {
    return {
      id: this.id,
      caseId: this.caseId,
      taskId: this.taskId,
      action: this.action,
      timestamp: this.timestamp.toString(),
      timestampISO: this.timestampISO,
      actor: this.actor,
      beforeHash: this.beforeHash,
      afterHash: this.afterHash,
      previousReceiptHash: this.previousReceiptHash,
      hash: this._hash,
      sparqlQuery: this.sparqlQuery,
      sparqlResult: this.sparqlResult,
      input: this.input,
      output: this.output,
      downstreamEnabled: this.downstreamEnabled?.map((d) => ({
        taskId: d.taskId,
        enabledAt: d.enabledAt.toString(),
      })),
      valid: this.valid,
    };
  }

  /**
   * Create from JSON object
   * @param {Object} json
   * @returns {YawlReceipt}
   */
  static fromJSON(json) {
    const receipt = new YawlReceipt({
      ...json,
      timestamp: BigInt(json.timestamp),
      downstreamEnabled: json.downstreamEnabled?.map((d) => ({
        taskId: d.taskId,
        enabledAt: BigInt(d.enabledAt),
      })),
    });
    receipt._hash = json.hash;
    return receipt;
  }
}

// =============================================================================
// Legacy Receipt Builder (Backward Compatibility)
// =============================================================================

/**
 * Build a legacy receipt for a task transition
 *
 * @deprecated Use generateReceipt() instead
 * @param {Object} params
 * @returns {Promise<YawlReceipt>}
 */
export async function buildReceipt({
  caseId,
  taskId,
  action,
  actor,
  beforeState,
  afterState,
  previousReceipt,
  sparqlQuery,
  sparqlResult,
  input,
  output,
  downstreamEnabled = [],
}) {
  const beforeHash = await blake3(JSON.stringify(beforeState));
  const afterHash = await blake3(JSON.stringify(afterState));

  let previousReceiptHash;
  if (previousReceipt) {
    previousReceiptHash = await previousReceipt.getHash();
  }

  const receipt = new YawlReceipt({
    id: `receipt-${Date.now()}-${Math.random().toString(36).slice(2, 8)}`,
    caseId,
    taskId,
    action,
    timestamp: now(),
    actor,
    beforeHash,
    afterHash,
    previousReceiptHash,
    sparqlQuery,
    sparqlResult,
    input,
    output,
    downstreamEnabled,
  });

  await receipt.computeHash();

  return receipt;
}

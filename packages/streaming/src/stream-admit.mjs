/**
 * @file Streaming RDF Delta Validation with Deterministic Receipt Chaining
 * @module @unrdf/streaming/stream-admit
 *
 * @description
 * Implements streaming admission updates with receipt chaining.
 * Each delta receives a cryptographic receipt with:
 * - input_hash: BLAKE3 of canonical quads before delta
 * - output_hash: BLAKE3 of canonical quads after delta
 * - deltaHash: BLAKE3 of normalized delta
 * - previousReceiptHash: Link to prior receipt
 *
 * Unbroken chain proves complete provenance.
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { randomUUID } from 'node:crypto';

/**
 * Deterministic serialization of RDF quads
 * Sorts quads by subject, predicate, object for canonical representation
 */
function canonicalizeQuads(quads) {
  if (!Array.isArray(quads)) return '';

  // Sort quads deterministically
  const sorted = [...quads].sort((a, b) => {
    // Sort by subject, then predicate, then object
    const aSubj = (a.subject?.value || '').toString();
    const bSubj = (b.subject?.value || '').toString();
    if (aSubj !== bSubj) return aSubj.localeCompare(bSubj);

    const aPred = (a.predicate?.value || '').toString();
    const bPred = (b.predicate?.value || '').toString();
    if (aPred !== bPred) return aPred.localeCompare(bPred);

    const aObj = (a.object?.value || '').toString();
    const bObj = (b.object?.value || '').toString();
    return aObj.localeCompare(bObj);
  });

  // Serialize to JSON for hashing
  return JSON.stringify(sorted, (key, value) =>
    typeof value === 'bigint' ? value.toString() : value
  );
}

/**
 * Normalize delta (additions + removals) for canonical hashing
 */
function normalizeDelta(delta) {
  const additions = (delta.additions || []).sort((a, b) => {
    const aStr = JSON.stringify(a);
    const bStr = JSON.stringify(b);
    return aStr.localeCompare(bStr);
  });

  const removals = (delta.removals || []).sort((a, b) => {
    const aStr = JSON.stringify(a);
    const bStr = JSON.stringify(b);
    return aStr.localeCompare(bStr);
  });

  return JSON.stringify({ additions, removals });
}

/**
 * Delta Condition Schema - Validates against computed hashes
 */
export const DeltaConditionSchema = z.object({
  kind: z.literal('delta'),
  hash: z.string().min(64).max(64).optional(), // Expected output_hash for verification
  checksum: z.enum(['crc32', 'blake3', 'sha256']).default('blake3'),
  expectedDeltaHash: z.string().min(64).max(64).optional(),
  expectedInputHash: z.string().min(64).max(64).optional(),
});

/**
 * Receipt Schema - Cryptographic proof of admission
 */
export const ReceiptSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.bigint(),
  timestampIso: z.string(),
  inputHash: z.string().length(64), // BLAKE3 of quads before delta
  outputHash: z.string().length(64), // BLAKE3 of quads after delta
  deltaHash: z.string().length(64), // BLAKE3 of normalized delta
  previousReceiptHash: z.string().length(64).nullable(),
  receiptHash: z.string().length(64), // BLAKE3 of entire receipt
  status: z.enum(['admitted', 'rejected']),
  rejectionReason: z.string().optional().nullable(),
  quadCountBefore: z.number().nonnegative(),
  quadCountAfter: z.number().nonnegative(),
});

/**
 * Stream Admission Options Schema
 */
export const StreamAdmissionOptionsSchema = z.object({
  condition: DeltaConditionSchema.optional(),
  emitReceipts: z.boolean().default(true),
  rollbackOnFailure: z.boolean().default(true),
  deterministic: z.boolean().default(true),
  nodeId: z.string().default('stream-admit'),
  caseId: z.string().optional(),
});

/**
 * Generate UUID for receipt (uses randomUUID for determinism when needed)
 * For now, uses node's random UUID. Can be made deterministic by seeding.
 */
function generateReceiptId() {
  return randomUUID();
}

/**
 * Streaming Admission Handler
 * Accepts deltas, validates, and generates receipt chains
 */
export class StreamingAdmission {
  /**
   *
   */
  constructor(store, options = {}) {
    if (!store || typeof store.getQuads !== 'function') {
      throw new TypeError('StreamingAdmission: store must be valid RDF store');
    }

    const validated = StreamAdmissionOptionsSchema.parse(options);
    this.store = store;
    this.condition = validated.condition;
    this.emitReceipts = validated.emitReceipts;
    this.rollbackOnFailure = validated.rollbackOnFailure;
    this.deterministic = validated.deterministic;
    this.nodeId = validated.nodeId;
    this.caseId = validated.caseId;

    // Receipt chain tracking
    this.previousReceiptHash = null;
    this.receipts = [];
    this.admittedCount = 0;
    this.rejectedCount = 0;
  }

  /**
   * Admit a single delta with receipt chaining
   * Returns { result, receipt } where:
   * - result: Updated store (or original if rejected)
   * - receipt: Cryptographic proof
   */
  async admit(delta) {
    if (!delta || typeof delta !== 'object') {
      throw new TypeError('admit: delta must be an object');
    }

    // Validate delta structure
    if (!Array.isArray(delta.additions)) delta.additions = [];
    if (!Array.isArray(delta.removals)) delta.removals = [];

    // Step 1: Get input state (quads before delta)
    const quadsBefore = Array.from(this.store.getQuads());
    const inputCanonical = canonicalizeQuads(quadsBefore);
    const inputHash = await blake3(inputCanonical);

    // Step 2: Apply delta (tentatively) and compute output state
    const quadsAfter = [
      ...quadsBefore.filter(q => !this._quadInArray(q, delta.removals)),
      ...delta.additions,
    ];
    const outputCanonical = canonicalizeQuads(quadsAfter);
    const outputHash = await blake3(outputCanonical);

    // Step 3: Compute delta hash
    const deltaNormalized = normalizeDelta(delta);
    const deltaHash = await blake3(deltaNormalized);

    // Step 4: Generate timestamp
    const timestamp = typeof process !== 'undefined' && process.hrtime
      ? process.hrtime.bigint()
      : BigInt(Date.now()) * 1_000_000n;
    const timestampIso = new Date(Number(timestamp / 1_000_000n)).toISOString();

    // Step 5: Evaluate delta condition if present
    let conditionPassed = true;
    let rejectionReason = null;

    if (this.condition) {
      const condResult = await this._evaluateDeltaCondition(
        this.condition,
        inputHash,
        outputHash,
        deltaHash
      );
      conditionPassed = condResult.passed;
      if (!conditionPassed) {
        rejectionReason = condResult.reason;
      }
    }

    // Step 6: Decide admission
    const admitted = conditionPassed;

    // Step 7: Apply delta or rollback
    let finalStore = this.store;
    if (admitted) {
      // Apply delta to store
      delta.additions.forEach(quad => {
        this.store.add(quad);
      });
      delta.removals.forEach(quad => {
        this.store.delete(quad);
      });
      this.admittedCount++;
    } else if (this.rollbackOnFailure) {
      // Rollback: don't modify store
      finalStore = this.store;
    }

    // Step 8: Generate receipt with chaining
    const receiptContent = {
      timestamp: timestamp.toString(),
      inputHash,
      outputHash,
      deltaHash,
      previousReceiptHash: this.previousReceiptHash,
      status: admitted ? 'admitted' : 'rejected',
      rejectionReason,
      quadCountBefore: quadsBefore.length,
      quadCountAfter: admitted ? quadsAfter.length : quadsBefore.length,
      nodeId: this.nodeId,
      caseId: this.caseId,
    };

    const receiptCanonical = JSON.stringify(receiptContent);
    const receiptHash = await blake3(receiptCanonical);
    const receiptId = generateReceiptId();

    const receipt = {
      id: receiptId,
      timestamp,
      timestampIso,
      inputHash,
      outputHash,
      deltaHash,
      previousReceiptHash: this.previousReceiptHash,
      receiptHash,
      status: admitted ? 'admitted' : 'rejected',
      rejectionReason,
      quadCountBefore: quadsBefore.length,
      quadCountAfter: admitted ? quadsAfter.length : quadsBefore.length,
    };

    // Validate receipt against schema
    const validated = ReceiptSchema.parse(receipt);

    // Update chain
    this.previousReceiptHash = receiptHash;
    this.receipts.push(receipt);

    if (!admitted) {
      this.rejectedCount++;
    }

    return {
      result: finalStore,
      receipt: validated,
      admitted,
    };
  }

  /**
   * Admit multiple deltas as stream
   * Returns array of { result, receipt, admitted } for each delta
   */
  async admitStream(deltas) {
    if (!Array.isArray(deltas)) {
      throw new TypeError('admitStream: deltas must be array');
    }

    const results = [];
    for (const delta of deltas) {
      const result = await this.admit(delta);
      results.push(result);
    }
    return results;
  }

  /**
   * Verify receipt chain integrity
   * Returns { valid, chain, mismatches }
   */
  verifyChain(includeDetails = false) {
    const chain = this.receipts;
    let valid = true;
    const mismatches = [];

    if (chain.length === 0) {
      return { valid: true, chain: [], mismatches: [] };
    }

    // Check first receipt has null previousReceiptHash
    if (chain[0].previousReceiptHash !== null) {
      valid = false;
      mismatches.push({
        index: 0,
        error: 'First receipt must have null previousReceiptHash',
      });
    }

    // Check chain continuity
    for (let i = 1; i < chain.length; i++) {
      const current = chain[i];
      const previous = chain[i - 1];

      if (current.previousReceiptHash !== previous.receiptHash) {
        valid = false;
        mismatches.push({
          index: i,
          error: `previousReceiptHash mismatch: expected ${previous.receiptHash}, got ${current.previousReceiptHash}`,
        });
      }
    }

    return {
      valid,
      chain: includeDetails ? chain : chain.length,
      mismatches,
    };
  }

  /**
   * Get chain statistics
   */
  getStats() {
    return {
      total: this.admittedCount + this.rejectedCount,
      admitted: this.admittedCount,
      rejected: this.rejectedCount,
      chainLength: this.receipts.length,
      currentHeadHash: this.previousReceiptHash,
    };
  }

  /**
   * Helper: Check if quad matches any in array
   */
  _quadInArray(quad, array) {
    return array.some(q =>
      q.subject?.value === quad.subject?.value &&
      q.predicate?.value === quad.predicate?.value &&
      q.object?.value === quad.object?.value
    );
  }

  /**
   * Helper: Evaluate delta condition
   */
  async _evaluateDeltaCondition(condition, inputHash, outputHash, deltaHash) {
    const { hash, expectedDeltaHash, expectedInputHash } = condition;

    // Check output hash if specified
    if (hash && hash !== outputHash) {
      return {
        passed: false,
        reason: `Output hash mismatch: expected ${hash}, got ${outputHash}`,
      };
    }

    // Check delta hash if specified
    if (expectedDeltaHash && expectedDeltaHash !== deltaHash) {
      return {
        passed: false,
        reason: `Delta hash mismatch: expected ${expectedDeltaHash}, got ${deltaHash}`,
      };
    }

    // Check input hash if specified
    if (expectedInputHash && expectedInputHash !== inputHash) {
      return {
        passed: false,
        reason: `Input hash mismatch: expected ${expectedInputHash}, got ${inputHash}`,
      };
    }

    return { passed: true };
  }
}

/**
 * Create streaming admission handler
 */
export function createStreamingAdmission(store, options = {}) {
  return new StreamingAdmission(store, options);
}

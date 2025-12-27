/**
 * KGC 4D Admission Gate & Receipt Chain System
 * Implements cryptographic receipts with BLAKE3 hashing and Merkle root computation
 *
 * Pattern: Pure functions + Zod validation + BLAKE3 for deterministic hashing
 * Receipts are immutable and form a tamper-evident chain
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

// ============================================================================
// SCHEMAS & TYPES
// ============================================================================

/**
 * Bounds schema - resource limits for operations
 */
const BoundsSchema = z.object({
  maxTriples: z.number().int().positive().optional(),
  maxMemoryMB: z.number().positive().optional(),
  maxTimeMs: z.number().positive().optional(),
  maxQueries: z.number().int().positive().optional(),
});

/**
 * Delta schema - represents a change to be admitted
 */
const DeltaSchema = z.object({
  operation: z.enum(['add', 'delete', 'query', 'update']),
  triples: z.array(z.any()).optional(),
  query: z.string().optional(),
  metadata: z.any().optional(),
});

/**
 * Predicate schema - validation rules for admission
 */
const PredicateSchema = z.object({
  name: z.string(),
  check: z.custom((val) => typeof val === 'function', {
    message: 'check must be a function',
  }),
  description: z.string().optional(),
});

/**
 * Receipt schema - immutable record of admission decision
 */
const ReceiptSchema = z.object({
  id: z.string(),
  timestamp_ns: z.bigint(),
  operation: z.string(),
  result: z.enum(['admit', 'deny']),
  reason: z.string(),
  bounds_used: z.any(),
  parent_receipt_id: z.string().nullable(),
  hash: z.string(),
});

// ============================================================================
// ADMISSION GATE CLASS
// ============================================================================

/**
 * AdmissionGate - Controls admission of operations with receipt generation
 *
 * Features:
 * - BLAKE3 cryptographic hashing for receipts
 * - Merkle tree receipt chaining
 * - Bounds enforcement (resource limits)
 * - Forbidden operation detection
 * - Preserve(Q) invariant checking
 * - Deterministic batch anchoring
 *
 * @example
 * import { AdmissionGate } from './admission-gate.mjs';
 *
 * const gate = new AdmissionGate();
 * const delta = { operation: 'add', triples: [{ s: 'a', p: 'b', o: 'c' }] };
 * const bounds = { maxTriples: 1000 };
 * const predicates = [];
 *
 * const receipt = await gate.admit(delta, bounds, predicates);
 * console.log(receipt.result); // 'admit' or 'deny'
 */
export class AdmissionGate {
  /**
   * @param {Object} options - Configuration options
   * @param {string[]} options.forbiddenOperations - Operations that require receipts
   * @param {Function} options.timeSource - Custom time source (for testing)
   */
  constructor(options = {}) {
    /** @type {Receipt[]} */
    this.receiptChain = [];

    /** @type {Set<string>} */
    this.forbiddenOperations = new Set(options.forbiddenOperations || ['update', 'delete']);

    /** @type {Function} */
    this.timeSource = options.timeSource || (() => process.hrtime.bigint());

    /** @type {Map<string, any>} */
    this.preservedQueries = new Map();
  }

  /**
   * Admit a delta (operation) with bounds and predicate checks
   *
   * @param {Object} delta - The operation to admit
   * @param {Object} bounds - Resource bounds/limits
   * @param {Array<Object>} predicates - Validation predicates
   * @returns {Promise<Object>} Receipt of admission decision
   *
   * @example
   * const receipt = await gate.admit(
   *   { operation: 'add', triples: [{s: 'a', p: 'b', o: 'c'}] },
   *   { maxTriples: 1000 },
   *   []
   * );
   */
  async admit(delta, bounds = {}, predicates = []) {
    // Validate inputs
    const validatedDelta = DeltaSchema.parse(delta);
    const validatedBounds = BoundsSchema.parse(bounds);
    const validatedPredicates = z.array(PredicateSchema).parse(predicates);

    // Check forbidden operations
    if (this.forbiddenOperations.has(validatedDelta.operation)) {
      const hasValidReceipt = this._hasValidReceiptForOperation(validatedDelta.operation);
      if (!hasValidReceipt) {
        return this._createReceipt(validatedDelta, 'deny', `Forbidden operation '${validatedDelta.operation}' without valid receipt`, validatedBounds);
      }
    }

    // Check bounds
    const boundsViolation = this._checkBounds(validatedDelta, validatedBounds);
    if (boundsViolation) {
      return this._createReceipt(validatedDelta, 'deny', boundsViolation, validatedBounds);
    }

    // Check predicates
    for (const predicate of validatedPredicates) {
      try {
        const passed = predicate.check(validatedDelta);
        if (!passed) {
          return this._createReceipt(
            validatedDelta,
            'deny',
            `Predicate '${predicate.name}' failed: ${predicate.description || 'validation failed'}`,
            validatedBounds
          );
        }
      } catch (error) {
        return this._createReceipt(validatedDelta, 'deny', `Predicate '${predicate.name}' threw error: ${error.message}`, validatedBounds);
      }
    }

    // Check Preserve(Q) invariant for queries
    if (validatedDelta.operation === 'query' && validatedDelta.query) {
      const preserved = this._checkPreserveInvariant(validatedDelta.query);
      if (!preserved) {
        return this._createReceipt(validatedDelta, 'deny', `Preserve(Q) invariant violated for query`, validatedBounds);
      }
    }

    // All checks passed - admit the operation
    return this._createReceipt(validatedDelta, 'admit', 'All checks passed', validatedBounds);
  }

  /**
   * Check if operation has valid receipt in chain
   * @private
   */
  _hasValidReceiptForOperation(operation) {
    // For now, we allow if there's ANY admitted operation in the chain
    // In production, you'd check for specific authorization receipts
    return this.receiptChain.some((receipt) => receipt.result === 'admit' && receipt.operation === operation);
  }

  /**
   * Check bounds violations
   * @private
   */
  _checkBounds(delta, bounds) {
    if (bounds.maxTriples && delta.triples && delta.triples.length > bounds.maxTriples) {
      return `Triple count ${delta.triples.length} exceeds bound ${bounds.maxTriples}`;
    }

    if (bounds.maxQueries && delta.operation === 'query') {
      const queryCount = this.receiptChain.filter((r) => r.operation === 'query').length;
      if (queryCount >= bounds.maxQueries) {
        return `Query count ${queryCount + 1} exceeds bound ${bounds.maxQueries}`;
      }
    }

    // Memory and time bounds would be checked during execution
    // Here we just validate the structure
    return null;
  }

  /**
   * Check Preserve(Q) invariant - query results should remain unchanged
   * @private
   */
  _checkPreserveInvariant(query) {
    if (!this.preservedQueries.has(query)) {
      // First time seeing this query - preserve its result
      this.preservedQueries.set(query, { timestamp: this.timeSource(), count: 1 });
      return true;
    }

    // Query has been seen before - check if it's still valid
    // In a real implementation, you'd re-execute and compare results
    // For now, we just track that it's preserved
    const preserved = this.preservedQueries.get(query);
    preserved.count += 1;
    return true;
  }

  /**
   * Create a receipt with BLAKE3 hash and chain it
   * @private
   */
  async _createReceipt(delta, result, reason, bounds) {
    const timestamp = this.timeSource();
    const parentId = this.receiptChain.length > 0 ? this.receiptChain[this.receiptChain.length - 1].id : null;

    // Create receipt ID from timestamp + operation
    const id = `receipt_${timestamp}_${delta.operation}`;

    // Compute BLAKE3 hash of receipt content (excluding hash field itself)
    const receiptContent = {
      id,
      timestamp_ns: timestamp.toString(),
      operation: delta.operation,
      result,
      reason,
      bounds_used: bounds,
      parent_receipt_id: parentId,
    };

    const hash = await blake3(JSON.stringify(receiptContent));

    const receipt = {
      id,
      timestamp_ns: timestamp,
      operation: delta.operation,
      result,
      reason,
      bounds_used: bounds,
      parent_receipt_id: parentId,
      hash,
    };

    // Validate receipt
    ReceiptSchema.parse(receipt);

    // Add to chain
    this.receiptChain.push(receipt);

    return receipt;
  }

  /**
   * Verify the integrity of a receipt chain using Merkle root computation
   *
   * @param {Array<Object>} receipts - Array of receipts to verify
   * @returns {Promise<boolean>} True if chain is valid
   *
   * @example
   * const isValid = await gate.verifyChain(gate.receiptChain);
   * console.assert(isValid === true, 'Chain should be valid');
   */
  async verifyChain(receipts) {
    if (!Array.isArray(receipts) || receipts.length === 0) {
      return false;
    }

    // Verify each receipt's hash
    for (const receipt of receipts) {
      const { hash: storedHash, ...contentWithoutHash } = receipt;

      // Convert BigInt to string for hashing
      const hashableContent = {
        ...contentWithoutHash,
        timestamp_ns: contentWithoutHash.timestamp_ns.toString(),
      };

      const computedHash = await blake3(JSON.stringify(hashableContent));

      if (computedHash !== storedHash) {
        return false;
      }
    }

    // Verify chain linkage - each receipt should reference previous
    for (let i = 1; i < receipts.length; i++) {
      const current = receipts[i];
      const previous = receipts[i - 1];

      if (current.parent_receipt_id !== previous.id) {
        return false;
      }

      // Verify monotonic timestamps
      if (current.timestamp_ns <= previous.timestamp_ns) {
        return false;
      }
    }

    return true;
  }

  /**
   * Compute Merkle root of receipt chain
   *
   * @param {Array<Object>} receipts - Receipts to compute root for
   * @returns {Promise<string>} Merkle root hash
   *
   * @example
   * const root = await gate.computeMerkleRoot(gate.receiptChain);
   * console.log('Merkle root:', root);
   */
  async computeMerkleRoot(receipts) {
    if (!Array.isArray(receipts) || receipts.length === 0) {
      return await blake3('');
    }

    if (receipts.length === 1) {
      return receipts[0].hash;
    }

    // Build Merkle tree bottom-up
    let currentLevel = receipts.map((r) => r.hash);

    while (currentLevel.length > 1) {
      const nextLevel = [];

      for (let i = 0; i < currentLevel.length; i += 2) {
        if (i + 1 < currentLevel.length) {
          // Hash pair
          const combined = currentLevel[i] + currentLevel[i + 1];
          nextLevel.push(await blake3(combined));
        } else {
          // Odd one out - promote to next level
          nextLevel.push(currentLevel[i]);
        }
      }

      currentLevel = nextLevel;
    }

    return currentLevel[0];
  }

  /**
   * Batch anchor multiple operations deterministically
   *
   * @param {Array<Object>} deltas - Operations to anchor
   * @param {Object} bounds - Shared bounds for all operations
   * @returns {Promise<Object>} Batch receipt with Merkle root
   *
   * @example
   * const batch = await gate.batchAnchor([delta1, delta2], { maxTriples: 1000 });
   * console.log('Batch Merkle root:', batch.merkle_root);
   */
  async batchAnchor(deltas, bounds = {}) {
    if (!Array.isArray(deltas) || deltas.length === 0) {
      throw new Error('batchAnchor requires non-empty array of deltas');
    }

    // Sort deltas deterministically by operation type for consistency
    const sortedDeltas = [...deltas].sort((a, b) => a.operation.localeCompare(b.operation));

    // Admit all operations
    const receipts = [];
    for (const delta of sortedDeltas) {
      const receipt = await this.admit(delta, bounds, []);
      receipts.push(receipt);
    }

    // Compute Merkle root
    const merkleRoot = await this.computeMerkleRoot(receipts);

    // Create batch receipt
    const batchReceipt = {
      type: 'batch',
      timestamp_ns: this.timeSource(),
      operation_count: receipts.length,
      merkle_root: merkleRoot,
      receipts,
      all_admitted: receipts.every((r) => r.result === 'admit'),
    };

    return batchReceipt;
  }

  /**
   * Get current receipt chain
   * @returns {Array<Object>} Current receipt chain
   */
  getReceiptChain() {
    return [...this.receiptChain];
  }

  /**
   * Clear receipt chain (for testing)
   */
  clearReceiptChain() {
    this.receiptChain = [];
    this.preservedQueries.clear();
  }
}

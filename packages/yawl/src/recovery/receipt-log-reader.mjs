/**
 * Receipt Log Reader - Read and validate receipt chains for case recovery
 *
 * Reads receipts from KGC-4D store and validates cryptographic chains.
 * Novel approach: Uses receipts instead of database for state reconstruction.
 *
 * @module @unrdf/yawl/recovery/receipt-log-reader
 */

import { z } from 'zod';
import { computeBlake3, computeChainHash, ReceiptSchema } from '../receipt-core.mjs';

// =============================================================================
// Schemas
// =============================================================================

const ReadReceiptChainOptionsSchema = z.object({
  caseId: z.string().min(1),
  store: z.any(), // KGCStore instance
  fromCheckpoint: z.string().optional(), // Receipt ID to start from
  validateChain: z.boolean().default(true),
});

const ChainValidationResultSchema = z.object({
  valid: z.boolean(),
  receipts: z.array(ReceiptSchema),
  errors: z.array(z.object({
    receiptId: z.string(),
    error: z.string(),
    receiptIndex: z.number(),
  })),
  stats: z.object({
    totalReceipts: z.number(),
    validReceipts: z.number(),
    invalidReceipts: z.number(),
    missingReceipts: z.number(),
  }),
});

// =============================================================================
// Receipt Chain Reader
// =============================================================================

/**
 * Read all receipts for a case from the KGC-4D store
 *
 * @param {Object} options - Read options
 * @param {string} options.caseId - Case ID to read receipts for
 * @param {Object} options.store - KGC-4D store instance
 * @param {string} [options.fromCheckpoint] - Receipt ID to start from
 * @param {boolean} [options.validateChain=true] - Validate chain integrity
 * @returns {Promise<Object>} Receipt chain and validation result
 *
 * @example
 * const result = await readReceiptChain({
 *   caseId: 'case-123',
 *   store: kgcStore,
 *   validateChain: true,
 * });
 *
 * if (result.valid) {
 *   console.log(`Read ${result.receipts.length} valid receipts`);
 * }
 */
export async function readReceiptChain(options) {
  const validated = ReadReceiptChainOptionsSchema.parse(options);
  const { caseId, store, fromCheckpoint, validateChain } = validated;

  // Query KGC-4D store for all receipts for this case
  const receipts = await queryReceiptsFromStore(store, caseId);

  // Sort receipts by timestamp (oldest first)
  receipts.sort((a, b) => Number(a.t_ns - b.t_ns));

  // Filter from checkpoint if specified
  let filteredReceipts = receipts;
  if (fromCheckpoint) {
    const checkpointIndex = receipts.findIndex(r => r.id === fromCheckpoint);
    if (checkpointIndex === -1) {
      throw new Error(`Checkpoint receipt ${fromCheckpoint} not found`);
    }
    filteredReceipts = receipts.slice(checkpointIndex);
  }

  // Validate chain if requested
  if (validateChain) {
    return await validateReceiptChain(filteredReceipts);
  }

  return {
    valid: true,
    receipts: filteredReceipts,
    errors: [],
    stats: {
      totalReceipts: filteredReceipts.length,
      validReceipts: filteredReceipts.length,
      invalidReceipts: 0,
      missingReceipts: 0,
    },
  };
}

/**
 * Query receipts from KGC-4D store for a specific case
 *
 * @param {Object} store - KGC-4D store instance
 * @param {string} caseId - Case ID
 * @returns {Promise<Array>} Array of receipts
 * @private
 */
async function queryReceiptsFromStore(store, caseId) {
  // If store has a queryReceipts method, use it
  if (typeof store.queryReceipts === 'function') {
    return await store.queryReceipts({ caseId });
  }

  // Otherwise, query via SPARQL if available
  if (typeof store.query === 'function') {
    const sparql = `
      PREFIX yawl: <http://unrdf.org/yawl#>

      SELECT ?receipt WHERE {
        ?receipt a yawl:Receipt ;
                 yawl:caseId "${caseId}" .
      }
      ORDER BY ?timestamp
    `;

    const results = await store.query(sparql);
    // Parse results and extract receipts
    // This is a simplified version - real implementation would deserialize from RDF
    return results.map(r => r.receipt);
  }

  // Fallback: check if store has receipts stored directly
  if (store.receipts && Array.isArray(store.receipts)) {
    return store.receipts.filter(r => r.caseId === caseId);
  }

  return [];
}

/**
 * Validate cryptographic chain integrity of receipts
 *
 * Verifies:
 * 1. Each receipt's payloadHash matches recomputed hash
 * 2. Each receipt's receiptHash chains to previous receipt
 * 3. No missing receipts in chain
 * 4. Timestamps are monotonically increasing
 *
 * @param {Array<Object>} receipts - Receipts to validate
 * @returns {Promise<Object>} Validation result
 *
 * @example
 * const result = await validateReceiptChain(receipts);
 * if (!result.valid) {
 *   console.error('Chain validation failed:', result.errors);
 * }
 */
export async function validateReceiptChain(receipts) {
  const errors = [];
  let validCount = 0;
  let previousReceipt = null;

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];
    let isValid = true;

    try {
      // 1. Validate receipt schema
      ReceiptSchema.parse(receipt);

      // 2. Verify payload hash
      const payloadToHash = {
        eventType: receipt.eventType,
        caseId: receipt.caseId,
        taskId: receipt.taskId,
        workItemId: receipt.workItemId || null,
        payload: receipt.payload,
        t_ns: receipt.t_ns.toString(),
      };
      const recomputedPayloadHash = await computeBlake3(payloadToHash);

      if (recomputedPayloadHash !== receipt.payloadHash) {
        errors.push({
          receiptId: receipt.id,
          error: `Payload hash mismatch: expected ${receipt.payloadHash}, got ${recomputedPayloadHash}`,
          receiptIndex: i,
        });
        isValid = false;
      }

      // 3. Verify chain hash links to previous receipt
      const expectedPreviousHash = previousReceipt ? previousReceipt.receiptHash : null;
      if (receipt.previousReceiptHash !== expectedPreviousHash) {
        errors.push({
          receiptId: receipt.id,
          error: `Previous hash mismatch: expected ${expectedPreviousHash}, got ${receipt.previousReceiptHash}`,
          receiptIndex: i,
        });
        isValid = false;
      }

      // 4. Verify receipt hash
      const recomputedReceiptHash = await computeChainHash(
        receipt.previousReceiptHash,
        receipt.payloadHash
      );
      if (recomputedReceiptHash !== receipt.receiptHash) {
        errors.push({
          receiptId: receipt.id,
          error: `Receipt hash mismatch: expected ${receipt.receiptHash}, got ${recomputedReceiptHash}`,
          receiptIndex: i,
        });
        isValid = false;
      }

      // 5. Verify timestamp is monotonically increasing
      if (previousReceipt && receipt.t_ns <= previousReceipt.t_ns) {
        errors.push({
          receiptId: receipt.id,
          error: `Timestamp not monotonic: ${receipt.t_ns} <= ${previousReceipt.t_ns}`,
          receiptIndex: i,
        });
        isValid = false;
      }

      if (isValid) {
        validCount++;
      }

      previousReceipt = receipt;
    } catch (error) {
      errors.push({
        receiptId: receipt.id,
        error: `Validation error: ${error.message}`,
        receiptIndex: i,
      });
    }
  }

  return {
    valid: errors.length === 0,
    receipts,
    errors,
    stats: {
      totalReceipts: receipts.length,
      validReceipts: validCount,
      invalidReceipts: receipts.length - validCount,
      missingReceipts: 0,
    },
  };
}

/**
 * Get latest state from receipt chain
 *
 * Returns the most recent receipt and basic state information.
 *
 * @param {Array<Object>} receipts - Receipt chain
 * @returns {Object} Latest state information
 *
 * @example
 * const latestState = getLatestState(receipts);
 * console.log(`Case status: ${latestState.status}`);
 * console.log(`Last event: ${latestState.lastEvent.eventType}`);
 */
export function getLatestState(receipts) {
  if (receipts.length === 0) {
    return {
      status: 'UNKNOWN',
      lastEvent: null,
      receiptCount: 0,
    };
  }

  const lastReceipt = receipts[receipts.length - 1];

  // Infer case status from last event type
  let status = 'RUNNING';
  if (lastReceipt.eventType === 'CASE_CREATED') {
    status = 'CREATED';
  } else if (lastReceipt.eventType === 'TASK_COMPLETED') {
    // Check if payload indicates case completion
    if (lastReceipt.payload?.context?.caseCompleted) {
      status = 'COMPLETED';
    }
  } else if (lastReceipt.eventType === 'TASK_CANCELLED') {
    if (lastReceipt.payload?.context?.caseCancelled) {
      status = 'CANCELLED';
    }
  }

  return {
    status,
    lastEvent: {
      eventType: lastReceipt.eventType,
      taskId: lastReceipt.taskId,
      workItemId: lastReceipt.workItemId,
      timestamp: lastReceipt.timestamp_iso,
    },
    receiptCount: receipts.length,
    lastReceiptHash: lastReceipt.receiptHash,
  };
}

/**
 * Handle missing or corrupted receipts
 *
 * Attempts to recover partial chains and identify gaps.
 *
 * @param {Array<Object>} receipts - Potentially incomplete receipt chain
 * @returns {Object} Recovery analysis
 *
 * @example
 * const analysis = handleMissingReceipts(receipts);
 * if (analysis.hasGaps) {
 *   console.warn(`Found ${analysis.gaps.length} gaps in receipt chain`);
 * }
 */
export function handleMissingReceipts(receipts) {
  const gaps = [];
  let previousReceipt = null;

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];

    if (previousReceipt) {
      // Check if previous hash matches
      const expectedPreviousHash = previousReceipt.receiptHash;
      if (receipt.previousReceiptHash !== expectedPreviousHash) {
        gaps.push({
          afterReceiptId: previousReceipt.id,
          beforeReceiptId: receipt.id,
          position: i,
          expectedHash: expectedPreviousHash,
          actualHash: receipt.previousReceiptHash,
        });
      }
    }

    previousReceipt = receipt;
  }

  return {
    hasGaps: gaps.length > 0,
    gaps,
    validSegments: gaps.length + 1,
    canRecover: gaps.length === 0,
  };
}

// =============================================================================
// Exports
// =============================================================================

export const schemas = {
  ReadReceiptChainOptionsSchema,
  ChainValidationResultSchema,
};

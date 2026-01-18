/**
 * @file v6 DeltaGate Receipt Generation
 * @module @unrdf/daemon/integrations/v6-deltagate-receipts
 * @description Receipt generation and proof chain logic
 */

import { hashData, generateUUID, getNs, getISOTimestamp } from './v6-deltagate.helpers.mjs';
import { DeltaReceiptSchema } from './v6-deltagate.schema.mjs';

/**
 * Generate receipt with proof chain
 * @param {Object} receiptData - Receipt data
 * @param {string} receiptData.deltaId - Delta ID
 * @param {boolean} receiptData.applied - Whether delta was applied
 * @param {string} [receiptData.reason] - Rejection reason
 * @param {string} [receiptData.stateHash] - State hash after application
 * @param {number} receiptData.operationsApplied - Number of operations applied
 * @param {number} [receiptData.operationsFailed] - Number of operations failed
 * @param {string|null} lastReceiptHash - Previous receipt hash for chain
 * @returns {Promise<Object>} Generated receipt
 */
export async function generateReceipt(receiptData, lastReceiptHash) {
  const receiptId = generateUUID();
  const timestamp_ns = getNs();
  const timestamp_iso = getISOTimestamp();

  const payloadHash = hashData({
    deltaId: receiptData.deltaId,
    stateHash: receiptData.stateHash,
    operationsApplied: receiptData.operationsApplied,
    timestamp_ns,
  });

  const receiptHash = hashData({
    id: receiptId,
    previousHash: lastReceiptHash,
    payloadHash,
    timestamp_iso,
  });

  const receipt = DeltaReceiptSchema.parse({
    id: receiptId,
    deltaId: receiptData.deltaId,
    timestamp_ns,
    timestamp_iso,
    applied: receiptData.applied,
    reason: receiptData.reason,
    stateHash: receiptData.stateHash,
    operationsApplied: receiptData.operationsApplied,
    operationsFailed: receiptData.operationsFailed || 0,
    previousReceiptHash: lastReceiptHash,
    receiptHash,
  });

  return receipt;
}

/**
 * @fileoverview Receipt-based operation executor
 * Ensures all operations are deterministic and produce verifiable receipts
 */

import { generateReceipt } from './receipt.mjs';

/**
 * Execute an operation with receipt generation
 * @template T
 * @param {string} operation - Operation name
 * @param {Record<string, any>} inputs - Operation inputs
 * @param {() => Promise<T>} fn - Operation function
 * @param {string} [parentHash] - Parent receipt hash
 * @returns {Promise<{result: T, receipt: import('./receipt.mjs').Receipt}>}
 */
export async function executeWithReceipt(operation, inputs, fn, parentHash) {
  try {
    const result = await fn();

    const receipt = await generateReceipt(
      operation,
      inputs,
      { result, success: true },
      parentHash
    );

    return { result, receipt };
  } catch (error) {
    const receipt = await generateReceipt(
      operation,
      inputs,
      {
        error: error.message,
        stack: error.stack,
        success: false
      },
      parentHash
    );

    return {
      result: null,
      receipt,
      error
    };
  }
}

/**
 * Batch executor for multiple operations
 * @param {Array<{operation: string, inputs: Record<string, any>, fn: () => Promise<any>}>} operations
 * @returns {Promise<{results: any[], receipts: import('./receipt.mjs').Receipt[]}>}
 */
export async function executeBatch(operations) {
  const results = [];
  const receipts = [];
  let lastHash;

  for (const op of operations) {
    const { result, receipt } = await executeWithReceipt(
      op.operation,
      op.inputs,
      op.fn,
      lastHash
    );

    results.push(result);
    receipts.push(receipt);
    lastHash = receipt.hash;
  }

  return { results, receipts };
}

/**
 * @fileoverview Receipt generation and validation for KGC operations
 * All operations produce receipts with cryptographic hashes for verification
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { now } from '@unrdf/kgc-4d';

/**
 * Receipt schema for operations
 */
export const ReceiptSchema = z.object({
  id: z.string(),
  timestamp: z.string(),
  operation: z.string(),
  inputs: z.record(z.any()),
  outputs: z.record(z.any()),
  hash: z.string(),
  parentHash: z.string().optional(),
});

/**
 * @typedef {z.infer<typeof ReceiptSchema>} Receipt
 */

/**
 * Generate a receipt for an operation
 * @param {string} operation - Operation name
 * @param {Record<string, any>} inputs - Operation inputs
 * @param {Record<string, any>} outputs - Operation outputs
 * @param {string} [parentHash] - Parent receipt hash for chaining
 * @returns {Promise<Receipt>} Generated receipt
 */
export async function generateReceipt(operation, inputs, outputs, parentHash) {
  const timestamp = now();
  const id = `receipt-${timestamp}-${operation}`;

  // Create deterministic hash of operation
  const data = JSON.stringify({
    operation,
    timestamp,
    inputs,
    outputs,
    parentHash: parentHash || null,
  }, null, 0); // No whitespace for determinism

  const hash = await blake3(data);

  const receipt = {
    id,
    timestamp,
    operation,
    inputs,
    outputs,
    hash,
    ...(parentHash && { parentHash }),
  };

  return ReceiptSchema.parse(receipt);
}

/**
 * Verify a receipt's hash
 * @param {Receipt} receipt - Receipt to verify
 * @returns {Promise<boolean>} True if valid
 */
export async function verifyReceiptHash(receipt) {
  const { hash: originalHash, ...rest } = receipt;

  // Reconstruct hash from receipt data
  const data = JSON.stringify({
    operation: rest.operation,
    timestamp: rest.timestamp,
    inputs: rest.inputs,
    outputs: rest.outputs,
    parentHash: rest.parentHash || null,
  }, null, 0);

  const computedHash = await blake3(data);

  return computedHash === originalHash;
}

/**
 * Verify a chain of receipts
 * @param {Receipt[]} receipts - Receipt chain to verify
 * @returns {Promise<{valid: boolean, errors: string[]}>} Verification result
 */
export async function verifyReceiptChain(receipts) {
  const errors = [];

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];

    // Verify hash
    const hashValid = await verifyReceiptHash(receipt);
    if (!hashValid) {
      errors.push(`Receipt ${receipt.id} has invalid hash`);
    }

    // Verify chain linkage
    if (i > 0) {
      const prevReceipt = receipts[i - 1];
      if (receipt.parentHash !== prevReceipt.hash) {
        errors.push(`Receipt ${receipt.id} has invalid parent hash`);
      }
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

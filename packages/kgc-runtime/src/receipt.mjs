/**
 * @fileoverview Receipt generation and validation for KGC operations
 * All operations produce receipts with cryptographic hashes for verification
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { mkdirSync, writeFileSync, readFileSync, readdirSync, existsSync } from 'node:fs';
import { join } from 'node:path';

/**
 * Get current timestamp in ISO format
 * @returns {string} ISO timestamp
 */
const now = () => new Date().toISOString();

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

/**
 * ReceiptStore - Persistent storage for receipts
 */
export class ReceiptStore {
  /**
   * @param {string} [baseDir='./var/kgc/receipts'] - Storage directory
   */
  constructor(baseDir = './var/kgc/receipts') {
    this.baseDir = baseDir;
    this._ensureDirectory();
  }

  /**
   * Ensure storage directory exists
   * @private
   */
  _ensureDirectory() {
    if (!existsSync(this.baseDir)) {
      mkdirSync(this.baseDir, { recursive: true });
    }
  }

  /**
   * Save a receipt to storage
   * @param {Receipt} receipt - Receipt to save
   * @returns {Promise<string>} Path to saved receipt
   */
  async save(receipt) {
    // Validate receipt
    const validated = ReceiptSchema.parse(receipt);

    // Ensure directory exists
    this._ensureDirectory();

    // Write receipt file
    const receiptPath = join(this.baseDir, `${validated.id}.json`);
    writeFileSync(receiptPath, JSON.stringify(validated, null, 2), 'utf-8');

    // Update manifest
    await this._updateManifest(validated);

    return receiptPath;
  }

  /**
   * Load a receipt from storage
   * @param {string} receiptId - Receipt ID to load
   * @returns {Promise<Receipt|null>} Loaded receipt or null if not found
   */
  async load(receiptId) {
    const receiptPath = join(this.baseDir, `${receiptId}.json`);

    if (!existsSync(receiptPath)) {
      return null;
    }

    try {
      const content = readFileSync(receiptPath, 'utf-8');
      const data = JSON.parse(content);
      return ReceiptSchema.parse(data);
    } catch (error) {
      throw new Error(`Failed to load receipt ${receiptId}: ${error.message}`);
    }
  }

  /**
   * List all receipts in storage
   * @returns {Promise<Receipt[]>} Array of all receipts
   */
  async list() {
    if (!existsSync(this.baseDir)) {
      return [];
    }

    const files = readdirSync(this.baseDir).filter(
      (f) => f.endsWith('.json') && f !== 'manifest.json'
    );

    const receipts = [];

    for (const file of files) {
      try {
        const content = readFileSync(join(this.baseDir, file), 'utf-8');
        const receipt = JSON.parse(content);
        receipts.push(ReceiptSchema.parse(receipt));
      } catch (error) {
        // Skip corrupt files
        continue;
      }
    }

    return receipts;
  }

  /**
   * Load a chain of receipts following parent links
   * @param {string} receiptId - Starting receipt ID
   * @returns {Promise<Receipt[]>} Chain of receipts from root to specified receipt
   */
  async loadChain(receiptId) {
    const chain = [];
    let currentId = receiptId;

    while (currentId) {
      const receipt = await this.load(currentId);

      if (!receipt) {
        break;
      }

      chain.unshift(receipt); // Add to beginning
      currentId = receipt.parentHash ? this._findReceiptByHash(receipt.parentHash) : null;
    }

    return chain;
  }

  /**
   * Find receipt ID by hash
   * @param {string} hash - Receipt hash to find
   * @returns {string|null} Receipt ID or null
   * @private
   */
  _findReceiptByHash(hash) {
    const manifestPath = join(this.baseDir, 'manifest.json');

    if (!existsSync(manifestPath)) {
      return null;
    }

    try {
      const manifest = JSON.parse(readFileSync(manifestPath, 'utf-8'));
      const entry = manifest.receipts?.find((r) => r.hash === hash);
      return entry?.id || null;
    } catch (error) {
      return null;
    }
  }

  /**
   * Update manifest with receipt entry
   * @param {Receipt} receipt - Receipt to add to manifest
   * @private
   */
  async _updateManifest(receipt) {
    const manifestPath = join(this.baseDir, 'manifest.json');
    let manifest = { receipts: [] };

    if (existsSync(manifestPath)) {
      try {
        manifest = JSON.parse(readFileSync(manifestPath, 'utf-8'));
      } catch (error) {
        // Manifest corrupted, start fresh
        manifest = { receipts: [] };
      }
    }

    // Add entry if not already present
    if (!manifest.receipts.some((r) => r.id === receipt.id)) {
      manifest.receipts.push({
        id: receipt.id,
        hash: receipt.hash,
        operation: receipt.operation,
        timestamp: receipt.timestamp,
        parentHash: receipt.parentHash,
      });
    }

    writeFileSync(manifestPath, JSON.stringify(manifest, null, 2), 'utf-8');
  }

  /**
   * Delete a receipt from storage
   * @param {string} receiptId - Receipt ID to delete
   * @returns {Promise<boolean>} True if deleted, false if not found
   */
  async delete(receiptId) {
    const receiptPath = join(this.baseDir, `${receiptId}.json`);

    if (!existsSync(receiptPath)) {
      return false;
    }

    try {
      const { unlinkSync } = await import('node:fs');
      unlinkSync(receiptPath);

      // Update manifest
      await this._removeFromManifest(receiptId);

      return true;
    } catch (error) {
      throw new Error(`Failed to delete receipt ${receiptId}: ${error.message}`);
    }
  }

  /**
   * Remove receipt from manifest
   * @param {string} receiptId - Receipt ID to remove
   * @private
   */
  async _removeFromManifest(receiptId) {
    const manifestPath = join(this.baseDir, 'manifest.json');

    if (!existsSync(manifestPath)) {
      return;
    }

    try {
      const manifest = JSON.parse(readFileSync(manifestPath, 'utf-8'));
      manifest.receipts = manifest.receipts.filter((r) => r.id !== receiptId);
      writeFileSync(manifestPath, JSON.stringify(manifest, null, 2), 'utf-8');
    } catch (error) {
      // Ignore manifest errors
    }
  }
}

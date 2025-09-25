/**
 * @fileoverview Storage utilities for UNRDF - File-based persistence for hooks and receipts
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { readFile, writeFile, mkdir, access, readdir } from 'node:fs/promises';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import crypto from 'node:crypto';

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * Hash a string using SHA-256
 * @param {string} input - Input string
 * @returns {Promise<string>} Hash string
 */
async function hashString(input) {
  if (input === undefined || input === null) {
    return '';
  }
  const hash = crypto.createHash('sha256');
  hash.update(String(input), 'utf8');
  return hash.digest('hex');
}

/**
 * Hook storage manager
 */
export class HookStorage {
  constructor(storagePath = './.unrdf') {
    this.storagePath = storagePath;
    this.hooksPath = join(storagePath, 'hooks');
    this.receiptsPath = join(storagePath, 'receipts');
    this.baselinesPath = join(storagePath, 'baselines');
  }

  /**
   * Initialize storage directories
   */
  async init() {
    const dirs = [this.storagePath, this.hooksPath, this.receiptsPath, this.baselinesPath];
    for (const dir of dirs) {
      try {
        await access(dir);
      } catch {
        await mkdir(dir, { recursive: true });
      }
    }
  }

  /**
   * Save a hook definition
   * @param {Object} hook - Hook definition
   * @returns {Promise<string>} Hook ID
   */
  async saveHook(hook) {
    await this.init();

    const hookId = hook.id;
    const fileName = `${hookId.replace(/[^a-zA-Z0-9-_]/g, '_')}.json`;
    const filePath = join(this.hooksPath, fileName);

    // Add metadata
    const hookWithMeta = {
      ...hook,
      _metadata: {
        created: new Date().toISOString(),
        updated: new Date().toISOString(),
        version: '1.0.0'
      }
    };

    await writeFile(filePath, JSON.stringify(hookWithMeta, null, 2));
    return hookId;
  }

  /**
   * Load a hook definition
   * @param {string} hookId - Hook ID
   * @returns {Promise<Object|null>} Hook definition or null if not found
   */
  async loadHook(hookId) {
    await this.init();

    const fileName = `${hookId.replace(/[^a-zA-Z0-9-_]/g, '_')}.json`;
    const filePath = join(this.hooksPath, fileName);

    try {
      const content = await readFile(filePath, 'utf-8');
      const hookWithMeta = JSON.parse(content);
      // Remove metadata before returning
      const { _metadata, ...hook } = hookWithMeta;
      return hook;
    } catch {
      return null;
    }
  }

  /**
   * List all stored hooks
   * @returns {Promise<Array<Object>>} Array of hook summaries
   */
  async listHooks() {
    await this.init();

    try {
      const files = await readdir(this.hooksPath);
      const hooks = [];

      for (const file of files) {
        if (file.endsWith('.json')) {
          try {
            const filePath = join(this.hooksPath, file);
            const content = await readFile(filePath, 'utf-8');
            const hookWithMeta = JSON.parse(content);

            hooks.push({
              id: hookWithMeta.id,
              name: hookWithMeta.name,
              description: hookWithMeta.description,
              created: hookWithMeta._metadata?.created,
              updated: hookWithMeta._metadata?.updated,
              predicateCount: hookWithMeta.predicates?.length || 0
            });
          } catch {
            // Skip invalid files
          }
        }
      }

      return hooks.sort((a, b) => new Date(b.updated) - new Date(a.updated));
    } catch {
      return [];
    }
  }

  /**
   * Delete a hook
   * @param {string} hookId - Hook ID
   * @returns {Promise<boolean>} True if deleted successfully
   */
  async deleteHook(hookId) {
    await this.init();

    const fileName = `${hookId.replace(/[^a-zA-Z0-9-_]/g, '_')}.json`;
    const filePath = join(this.hooksPath, fileName);

    try {
      await access(filePath);
      // Don't actually delete, just rename to .deleted
      const deletedPath = join(this.hooksPath, `${fileName}.deleted`);
      await writeFile(deletedPath, '');
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Save a receipt
   * @param {Object} receipt - Receipt object
   * @returns {Promise<string>} Receipt ID
   */
  async saveReceipt(receipt) {
    await this.init();

    const receiptId = `${receipt.hookId}_${Date.now()}_${receipt.provenance.queryHash.substring(0, 8)}`;
    const fileName = `${receiptId}.json`;
    const filePath = join(this.receiptsPath, fileName);

    // Add metadata
    const receiptWithMeta = {
      ...receipt,
      _metadata: {
        saved: new Date().toISOString(),
        receiptId
      }
    };

    await writeFile(filePath, JSON.stringify(receiptWithMeta, null, 2));
    return receiptId;
  }

  /**
   * Load receipts for a hook
   * @param {string} hookId - Hook ID
   * @param {number} [limit=100] - Maximum number of receipts to return
   * @returns {Promise<Array<Object>>} Array of receipts
   */
  async loadReceipts(hookId, limit = 100) {
    await this.init();

    try {
      const files = await readdir(this.receiptsPath);
      const receipts = [];

      for (const file of files) {
        if (file.endsWith('.json') && file.startsWith(hookId.replace(/[^a-zA-Z0-9-_]/g, '_'))) {
          try {
            const filePath = join(this.receiptsPath, file);
            const content = await readFile(filePath, 'utf-8');
            const receiptWithMeta = JSON.parse(content);

            // Remove metadata before returning
            const { _metadata, ...receipt } = receiptWithMeta;
            receipts.push(receipt);
          } catch {
            // Skip invalid files
          }
        }
      }

      return receipts
        .sort((a, b) => new Date(b.timestamp) - new Date(a.timestamp))
        .slice(0, limit);
    } catch {
      return [];
    }
  }

  /**
   * Save baseline data for a hook
   * @param {string} hookId - Hook ID
   * @param {Object} baseline - Baseline data
   * @returns {Promise<void>}
   */
  async saveBaseline(hookId, baseline) {
    await this.init();

    const fileName = `${hookId.replace(/[^a-zA-Z0-9-_]/g, '_')}_baseline.json`;
    const filePath = join(this.baselinesPath, fileName);

    const baselineWithMeta = {
      ...baseline,
      _metadata: {
        hookId,
        saved: new Date().toISOString()
      }
    };

    await writeFile(filePath, JSON.stringify(baselineWithMeta, null, 2));
  }

  /**
   * Load baseline data for a hook
   * @param {string} hookId - Hook ID
   * @returns {Promise<Object|null>} Baseline data or null if not found
   */
  async loadBaseline(hookId) {
    await this.init();

    const fileName = `${hookId.replace(/[^a-zA-Z0-9-_]/g, '_')}_baseline.json`;
    const filePath = join(this.baselinesPath, fileName);

    try {
      const content = await readFile(filePath, 'utf-8');
      const baselineWithMeta = JSON.parse(content);

      // Remove metadata before returning
      const { _metadata, ...baseline } = baselineWithMeta;
      return baseline;
    } catch {
      return null;
    }
  }

  /**
   * Get storage statistics
   * @returns {Promise<Object>} Storage statistics
   */
  async getStats() {
    await this.init();

    try {
      const [hookFiles, receiptFiles, baselineFiles] = await Promise.all([
        readdir(this.hooksPath),
        readdir(this.receiptsPath),
        readdir(this.baselinesPath)
      ]);

      const validHooks = hookFiles.filter(f => f.endsWith('.json') && !f.endsWith('.deleted.json')).length;
      const receipts = receiptFiles.filter(f => f.endsWith('.json')).length;
      const baselines = baselineFiles.filter(f => f.endsWith('.json')).length;

      return {
        hooks: validHooks,
        receipts,
        baselines,
        totalFiles: hookFiles.length + receiptFiles.length + baselineFiles.length
      };
    } catch {
      return { hooks: 0, receipts: 0, baselines: 0, totalFiles: 0 };
    }
  }
}

/**
 * Default storage instance
 */
export const defaultStorage = new HookStorage();

/**
 * Storage configuration
 */
export const storageConfig = {
  defaultPath: './.unrdf',
  maxReceiptsPerHook: 1000,
  cleanupInterval: 24 * 60 * 60 * 1000, // 24 hours
  retentionDays: 30
};

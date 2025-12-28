/**
 * @fileoverview Delta Store - In-memory delta persistence with adapter pattern
 *
 * Provides storage for v6 deltas with:
 * - Delta CRUD operations
 * - Status tracking (proposed, applied, rejected)
 * - State management integration
 * - File system adapter for future persistence
 *
 * @module @unrdf/v6-core/delta/store
 */

import { z } from 'zod';
import { readFile } from 'node:fs/promises';

/**
 * Delta status enum
 */
export const DeltaStatus = {
  PROPOSED: 'proposed',
  APPLIED: 'applied',
  REJECTED: 'rejected',
};

/**
 * Stored delta schema with metadata
 */
export const StoredDeltaSchema = z.object({
  id: z.string(),
  from: z.string().describe('Source state hash'),
  to: z.string().describe('Target state hash'),
  operations: z.array(z.object({
    type: z.enum(['add', 'remove', 'modify']),
    subject: z.string(),
    predicate: z.string(),
    object: z.any(),
    oldValue: z.any().optional()
  })),
  metadata: z.object({
    description: z.string().optional(),
    proposedAt: z.string(),
    appliedAt: z.string().optional(),
    status: z.enum(['proposed', 'applied', 'rejected']),
    stateHashBefore: z.string().optional(),
    stateHashAfter: z.string().optional()
  }).passthrough()
});

/**
 * Delta Store - In-memory storage for deltas
 *
 * Provides:
 * - CRUD operations for deltas
 * - Status tracking and updates
 * - State hash management
 * - Query capabilities
 *
 * @class
 * @example
 * const store = new DeltaStore();
 * await store.store(deltaId, delta);
 * const delta = await store.get(deltaId);
 * const allDeltas = await store.list();
 */
export class DeltaStore {
  /**
   * @param {Object} [options] - Store options
   * @param {Object} [options.stateStore] - Optional state store for validation
   */
  constructor(options = {}) {
    /** @type {Map<string, Object>} */
    this.deltas = new Map();
    this.stateStore = options.stateStore || null;
  }

  /**
   * Store a delta
   *
   * @param {string} deltaId - Delta identifier
   * @param {Object} delta - Delta object
   * @returns {Promise<void>}
   * @throws {Error} If delta is invalid
   */
  async store(deltaId, delta) {
    // Validate delta structure
    const validated = StoredDeltaSchema.parse(delta);

    // Store with ID as key
    this.deltas.set(deltaId, validated);
  }

  /**
   * Get a delta by ID
   *
   * @param {string} deltaId - Delta identifier
   * @returns {Promise<Object|null>} Delta or null if not found
   */
  async get(deltaId) {
    return this.deltas.get(deltaId) || null;
  }

  /**
   * Check if delta exists
   *
   * @param {string} deltaId - Delta identifier
   * @returns {Promise<boolean>}
   */
  async has(deltaId) {
    return this.deltas.has(deltaId);
  }

  /**
   * List all deltas
   *
   * @param {Object} [options] - Query options
   * @param {string} [options.status] - Filter by status
   * @param {string} [options.from] - Filter by source state hash
   * @returns {Promise<Array<Object>>} Array of deltas
   */
  async list(options = {}) {
    let deltas = Array.from(this.deltas.values());

    // Apply filters
    if (options.status) {
      deltas = deltas.filter(d => d.metadata.status === options.status);
    }

    if (options.from) {
      deltas = deltas.filter(d => d.from === options.from);
    }

    // Sort by proposed timestamp (newest first)
    deltas.sort((a, b) => {
      const aTime = new Date(a.metadata.proposedAt).getTime();
      const bTime = new Date(b.metadata.proposedAt).getTime();
      return bTime - aTime;
    });

    return deltas;
  }

  /**
   * Mark delta as applied
   *
   * Updates delta status and records application metadata.
   *
   * @param {string} deltaId - Delta identifier
   * @param {Object} [metadata] - Additional metadata
   * @param {string} [metadata.appliedAt] - Application timestamp
   * @param {string} [metadata.stateHashAfter] - State hash after application
   * @returns {Promise<Object>} Updated delta
   * @throws {Error} If delta not found
   */
  async markApplied(deltaId, metadata = {}) {
    const delta = await this.get(deltaId);
    if (!delta) {
      throw new Error(`Delta not found: ${deltaId}`);
    }

    delta.metadata.status = DeltaStatus.APPLIED;
    delta.metadata.appliedAt = metadata.appliedAt || new Date().toISOString();

    if (metadata.stateHashAfter) {
      delta.metadata.stateHashAfter = metadata.stateHashAfter;
    }

    this.deltas.set(deltaId, delta);
    return delta;
  }

  /**
   * Mark delta as rejected
   *
   * @param {string} deltaId - Delta identifier
   * @param {string} reason - Rejection reason
   * @returns {Promise<Object>} Updated delta
   * @throws {Error} If delta not found
   */
  async markRejected(deltaId, reason) {
    const delta = await this.get(deltaId);
    if (!delta) {
      throw new Error(`Delta not found: ${deltaId}`);
    }

    delta.metadata.status = DeltaStatus.REJECTED;
    delta.metadata.rejectedAt = new Date().toISOString();
    delta.metadata.rejectionReason = reason;

    this.deltas.set(deltaId, delta);
    return delta;
  }

  /**
   * Delete a delta
   *
   * @param {string} deltaId - Delta identifier
   * @returns {Promise<boolean>} True if deleted, false if not found
   */
  async delete(deltaId) {
    return this.deltas.delete(deltaId);
  }

  /**
   * Clear all deltas
   *
   * @returns {Promise<void>}
   */
  async clear() {
    this.deltas.clear();
  }

  /**
   * Get store statistics
   *
   * @returns {Promise<Object>} Statistics
   */
  async getStats() {
    const deltas = Array.from(this.deltas.values());

    const stats = {
      total: deltas.length,
      byStatus: {
        proposed: 0,
        applied: 0,
        rejected: 0
      }
    };

    for (const delta of deltas) {
      stats.byStatus[delta.metadata.status]++;
    }

    return stats;
  }

  /**
   * Get current state hash from state store
   *
   * @returns {Promise<string|null>} Current state hash or null if no state store
   */
  async getCurrentStateHash() {
    if (!this.stateStore || typeof this.stateStore.getStateHash !== 'function') {
      return null;
    }

    return this.stateStore.getStateHash();
  }

  /**
   * Verify operation against state store
   *
   * @param {Object} operation - Operation to verify
   * @returns {Promise<boolean>} True if operation is valid
   */
  async verifyOperation(operation) {
    if (!this.stateStore || typeof this.stateStore.verifyOperation !== 'function') {
      // No state store, cannot verify
      return true;
    }

    return this.stateStore.verifyOperation(operation);
  }

  /**
   * Apply operations to state store
   *
   * @param {Array<Object>} operations - Operations to apply
   * @returns {Promise<Object>} Application result with new state hash
   */
  async applyOperations(operations) {
    if (!this.stateStore || typeof this.stateStore.applyOperations !== 'function') {
      // No state store, simulate success
      return {
        applied: true,
        operationCount: operations.length,
        stateHash: null
      };
    }

    return this.stateStore.applyOperations(operations);
  }
}

/**
 * Read delta from file
 *
 * @param {string} filePath - Path to delta JSON file
 * @returns {Promise<Object>} Parsed delta
 * @throws {Error} If file cannot be read or JSON is invalid
 */
export async function readDeltaFromFile(filePath) {
  try {
    const content = await readFile(filePath, 'utf-8');
    const delta = JSON.parse(content);
    return delta;
  } catch (error) {
    throw new Error(`Failed to read delta from file ${filePath}: ${error.message}`);
  }
}

/**
 * Create delta store
 *
 * @param {Object} [options] - Store options
 * @returns {DeltaStore}
 */
export function createDeltaStore(options = {}) {
  return new DeltaStore(options);
}

// Singleton instance for CLI usage
let defaultStore = null;

/**
 * Get default delta store instance
 *
 * @returns {DeltaStore}
 */
export function getDefaultStore() {
  if (!defaultStore) {
    defaultStore = new DeltaStore();
  }
  return defaultStore;
}

/**
 * Reset default store (mainly for testing)
 *
 * @returns {void}
 */
export function resetDefaultStore() {
  defaultStore = null;
}

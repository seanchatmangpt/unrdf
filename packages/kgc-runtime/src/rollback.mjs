/**
 * @fileoverview Rollback Event Log System for KGC Runtime
 * Stores undo operations and enables deterministic replay
 *
 * Pattern: Event sourcing + JSON log files
 * Format: {transaction_id, operations[], timestamp}
 */

import { z } from 'zod';
import { promises as fs } from 'fs';
import path from 'path';

// ============================================================================
// Schemas
// ============================================================================

/**
 * Undo log entry schema
 */
const UndoLogEntrySchema = z.object({
  transaction_id: z.string(),
  operations: z.array(z.object({
    id: z.string(),
    type: z.string(),
    data: z.any(),
  })),
  timestamp: z.string(),
  hash: z.string().optional(),
});

/**
 * @typedef {z.infer<typeof UndoLogEntrySchema>} UndoLogEntry
 */

// ============================================================================
// RollbackLog Class
// ============================================================================

/**
 * RollbackLog - Event log for undo operations
 *
 * Features:
 * - Persistent JSON log storage
 * - Replay transactions by ID
 * - Deterministic rollback
 * - Transaction history tracking
 *
 * @example
 * const log = new RollbackLog('./var/kgc/undo-log.json');
 * await log.append(transaction_id, undoOperations);
 * const result = await log.replay(transaction_id);
 */
export class RollbackLog {
  /**
   * @param {string} logPath - Path to log file
   */
  constructor(logPath = './var/kgc/undo-log.json') {
    /** @type {string} */
    this.logPath = logPath;

    /** @type {UndoLogEntry[]} */
    this.entries = [];

    /** @type {boolean} */
    this.loaded = false;
  }

  /**
   * Ensure log directory exists
   * @private
   */
  async _ensureDir() {
    const dir = path.dirname(this.logPath);
    try {
      await fs.mkdir(dir, { recursive: true });
    } catch (error) {
      if (error.code !== 'EEXIST') {
        throw error;
      }
    }
  }

  /**
   * Load log from disk
   * @returns {Promise<void>}
   */
  async load() {
    if (this.loaded) return;

    await this._ensureDir();

    try {
      const data = await fs.readFile(this.logPath, 'utf-8');
      const parsed = JSON.parse(data);
      this.entries = z.array(UndoLogEntrySchema).parse(parsed);
      this.loaded = true;
    } catch (error) {
      if (error.code === 'ENOENT') {
        // File doesn't exist yet - start with empty log
        this.entries = [];
        this.loaded = true;
      } else {
        throw new Error(`Failed to load rollback log: ${error.message}`);
      }
    }
  }

  /**
   * Save log to disk
   * @returns {Promise<void>}
   */
  async save() {
    await this._ensureDir();

    const data = JSON.stringify(this.entries, null, 2);
    await fs.writeFile(this.logPath, data, 'utf-8');
  }

  /**
   * Append transaction undo operations to log
   *
   * @param {string} transactionId - Transaction ID
   * @param {any[]} operations - Undo operations
   * @param {string} [hash] - Optional transaction hash
   * @returns {Promise<UndoLogEntry>} Created log entry
   */
  async append(transactionId, operations, hash = null) {
    await this.load();

    const entry = {
      transaction_id: transactionId,
      operations,
      timestamp: new Date().toISOString(),
      ...(hash && { hash }),
    };

    const validated = UndoLogEntrySchema.parse(entry);
    this.entries.push(validated);

    await this.save();

    return validated;
  }

  /**
   * Replay (undo) a transaction by ID
   *
   * @param {string} transactionId - Transaction ID to replay
   * @param {Function} applyOp - Function to apply undo operations
   * @returns {Promise<{success: boolean, operations_applied: number, errors: string[]}>} Replay result
   */
  async replay(transactionId, applyOp) {
    await this.load();

    const entry = this.entries.find(e => e.transaction_id === transactionId);
    if (!entry) {
      return {
        success: false,
        operations_applied: 0,
        errors: [`Transaction ${transactionId} not found in undo log`],
      };
    }

    const errors = [];
    let applied = 0;

    // Apply operations in reverse order (newest to oldest)
    for (const op of entry.operations.slice().reverse()) {
      try {
        await applyOp(op);
        applied++;
      } catch (error) {
        errors.push(`Failed to apply operation ${op.id}: ${error.message}`);
      }
    }

    return {
      success: errors.length === 0,
      operations_applied: applied,
      errors,
    };
  }

  /**
   * Get all log entries
   * @returns {Promise<UndoLogEntry[]>} All log entries
   */
  async getAll() {
    await this.load();
    return [...this.entries];
  }

  /**
   * Get log entry by transaction ID
   *
   * @param {string} transactionId - Transaction ID
   * @returns {Promise<UndoLogEntry|null>} Log entry or null
   */
  async getByTransactionId(transactionId) {
    await this.load();
    return this.entries.find(e => e.transaction_id === transactionId) || null;
  }

  /**
   * Get log entries in time range
   *
   * @param {Date} start - Start time
   * @param {Date} end - End time
   * @returns {Promise<UndoLogEntry[]>} Matching entries
   */
  async getByTimeRange(start, end) {
    await this.load();

    return this.entries.filter(e => {
      const timestamp = new Date(e.timestamp);
      return timestamp >= start && timestamp <= end;
    });
  }

  /**
   * Clear all log entries
   * @returns {Promise<void>}
   */
  async clear() {
    this.entries = [];
    await this.save();
  }

  /**
   * Get log statistics
   * @returns {Promise<{total: number, oldest: string, newest: string}>} Statistics
   */
  async getStats() {
    await this.load();

    if (this.entries.length === 0) {
      return {
        total: 0,
        oldest: null,
        newest: null,
      };
    }

    const timestamps = this.entries.map(e => e.timestamp).sort();

    return {
      total: this.entries.length,
      oldest: timestamps[0],
      newest: timestamps[timestamps.length - 1],
    };
  }
}

// ============================================================================
// Exports
// ============================================================================

export default RollbackLog;

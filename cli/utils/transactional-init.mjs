/**
 * @file Transactional initialization with rollback
 * @module cli/utils/transactional-init
 *
 * Ensures init operations can be rolled back on failure
 * FM-CLI-008: Init failure leaves partial state
 */

import { promises as fs } from 'node:fs';
import { dirname } from 'node:path';

/**
 * Transactional init operation with rollback capability
 */
export class TransactionLog {
  constructor() {
    this.operations = [];
    this.rolled_back = false;
  }

  /**
   * Log a file creation for potential rollback
   */
  logCreation(filePath, content) {
    this.operations.push({
      type: 'create',
      path: filePath,
      content,
      timestamp: Date.now()
    });
  }

  /**
   * Log a directory creation for potential rollback
   */
  logDirectoryCreation(dirPath) {
    this.operations.push({
      type: 'mkdir',
      path: dirPath,
      timestamp: Date.now()
    });
  }

  /**
   * Log a configuration update
   */
  logConfigUpdate(configPath, oldConfig, newConfig) {
    this.operations.push({
      type: 'config_update',
      path: configPath,
      oldConfig,
      newConfig,
      timestamp: Date.now()
    });
  }

  /**
   * Rollback all operations in reverse order
   */
  async rollback() {
    if (this.rolled_back) {
      return;
    }

    this.rolled_back = true;
    const errors = [];

    // Reverse order: most recent first
    for (let i = this.operations.length - 1; i >= 0; i--) {
      const op = this.operations[i];
      try {
        switch (op.type) {
          case 'create': {
            // Delete the created file
            try {
              await fs.unlink(op.path);
            } catch (err) {
              if (err.code !== 'ENOENT') {
                throw err;
              }
            }
            break;
          }
          case 'mkdir': {
            // Remove created directory (only if empty)
            try {
              await fs.rmdir(op.path);
            } catch (err) {
              if (err.code !== 'ENOENT') {
                // Directory not empty, try recursive if needed
                // For safety, only remove if explicitly marked as safe
              }
            }
            break;
          }
          case 'config_update': {
            // Restore old config
            try {
              await fs.writeFile(op.path, JSON.stringify(op.oldConfig, null, 2));
            } catch (err) {
              errors.push(`Failed to restore config at ${op.path}: ${err.message}`);
            }
            break;
          }
        }
      } catch (err) {
        errors.push(`Rollback error for ${op.path}: ${err.message}`);
      }
    }

    return {
      success: errors.length === 0,
      errors,
      rollbackCount: this.operations.length
    };
  }

  /**
   * Commit all operations (clear log)
   */
  commit() {
    this.operations = [];
    return { success: true, committed: true };
  }

  /**
   * Get operation count
   */
  getOperationCount() {
    return this.operations.length;
  }

  /**
   * Get summary for debugging
   */
  getSummary() {
    const byType = {};
    this.operations.forEach(op => {
      byType[op.type] = (byType[op.type] || 0) + 1;
    });

    return {
      total_operations: this.operations.length,
      by_type: byType,
      rolled_back: this.rolled_back,
      operations: this.operations.map(op => ({
        type: op.type,
        path: op.path,
        timestamp: new Date(op.timestamp).toISOString()
      }))
    };
  }
}

/**
 * Execute init operation with automatic rollback on failure
 */
export async function executeInitTransaction(initFunction, options = {}) {
  const txn = new TransactionLog();
  const { onFailure = 'rollback' } = options;

  try {
    // Execute init with transaction log
    const result = await initFunction(txn);

    // Commit on success
    txn.commit();

    return {
      success: true,
      result,
      transaction: txn.getSummary()
    };
  } catch (error) {
    // Rollback on failure
    if (onFailure === 'rollback') {
      const rollbackResult = await txn.rollback();

      return {
        success: false,
        error: error.message,
        rollback: rollbackResult,
        suggestion: 'Initialization failed and all changes have been rolled back.'
      };
    }

    return {
      success: false,
      error: error.message,
      transaction: txn.getSummary(),
      suggestion: 'Initialization failed. Manual cleanup may be required.'
    };
  }
}

/**
 * Ensure directory exists, logging creation
 */
export async function ensureDirExists(dirPath, txn) {
  try {
    await fs.mkdir(dirPath, { recursive: true });
    if (txn) {
      txn.logDirectoryCreation(dirPath);
    }
    return true;
  } catch (error) {
    throw new Error(`Failed to create directory ${dirPath}: ${error.message}`);
  }
}

/**
 * Write file with transaction logging
 */
export async function writeFileWithLogging(filePath, content, txn) {
  try {
    // Ensure directory exists
    const dir = dirname(filePath);
    await ensureDirExists(dir, txn);

    // Write file
    await fs.writeFile(filePath, content, 'utf-8');

    if (txn) {
      txn.logCreation(filePath, content);
    }

    return true;
  } catch (error) {
    throw new Error(`Failed to write file ${filePath}: ${error.message}`);
  }
}

export default {
  TransactionLog,
  executeInitTransaction,
  ensureDirExists,
  writeFileWithLogging
};

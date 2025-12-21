/**
 * KGEN Atomic Writer
 *
 * Provides atomic file write operations with backup, rollback,
 * and transactional capabilities for deterministic file modifications.
 */

import { promises as fs, createReadStream } from 'fs';
import { join, dirname, basename, extname } from 'path';
import { createHash } from 'crypto';
import { pipeline } from 'stream/promises';

import { ERROR_CODES, CHECKSUM_ALGORITHMS, LOCK_CONFIG } from './constants.js';

/**
 *
 */
export class AtomicWriter {
  /**
   *
   */
  constructor(config = {}) {
    this.config = config;
    this.activeLocks = new Map();
    this.activeTransactions = new Map();
  }

  /**
   * Write file atomically using temporary file + rename
   */
  async writeAtomic(filePath, content, options = {}) {
    const {
      backup = this.config.backupEnabled,
      operationId,
      preserveMetadata = this.config.preservePermissions,
      encoding = 'utf8'
    } = options;

    // Acquire file lock
    const lockId = await this._acquireLock(filePath);

    try {
      let backupPath = null;
      let originalStats = null;

      // Create backup if enabled and file exists
      if (backup) {
        try {
          originalStats = await fs.stat(filePath);
          backupPath = await this._createBackup(filePath, operationId);
        } catch (error) {
          if (error.code !== 'ENOENT') throw error;
          // File doesn't exist, no backup needed
        }
      }

      // Create temporary file in same directory for atomic rename
      const tempPath = await this._createTempFile(filePath);

      try {
        // Write content to temporary file
        await fs.writeFile(tempPath, content, encoding);

        // Preserve original file metadata
        if (preserveMetadata && originalStats) {
          await fs.chmod(tempPath, originalStats.mode);
          await fs.utimes(tempPath, originalStats.atime, originalStats.mtime);
        }

        // Calculate checksum
        const checksum = await this._calculateChecksum(tempPath);

        // Atomic rename (this is the atomic operation)
        await fs.rename(tempPath, filePath);

        return {
          success: true,
          filePath,
          backupPath,
          checksum,
          tempPath: null // Cleaned up by rename
        };

      } catch (error) {
        // Clean up temp file on failure
        try {
          await fs.unlink(tempPath);
        } catch (cleanupError) {
          console.warn('Failed to cleanup temp file:', tempPath, cleanupError.message);
        }

        // Restore from backup if available
        if (backupPath) {
          try {
            await fs.copyFile(backupPath, filePath);
          } catch (restoreError) {
            console.error('Failed to restore from backup:', restoreError);
          }
        }

        throw error;
      }

    } finally {
      await this._releaseLock(lockId);
    }
  }

  /**
   * Begin transaction for multi-file atomic operations
   */
  async beginTransaction(operationId) {
    if (this.activeTransactions.has(operationId)) {
      throw new Error(`Transaction ${operationId} already active`);
    }

    const transaction = new AtomicTransaction(operationId, this.config);
    this.activeTransactions.set(operationId, transaction);

    return transaction;
  }

  /**
   * Private Methods
   */

  /**
   *
   */
  async _acquireLock(filePath) {
    const lockId = `${filePath}-${Date.now()}-${Math.random()}`;
    const startTime = Date.now();

    while (Date.now() - startTime < LOCK_CONFIG.TIMEOUT) {
      // Simple file-based locking
      const lockFile = `${filePath}.kgen-lock`;

      try {
        await fs.writeFile(lockFile, lockId, { flag: 'wx' }); // Exclusive create
        this.activeLocks.set(lockId, lockFile);
        return lockId;
      } catch (error) {
        if (error.code === 'EEXIST') {
          // Lock exists, check if stale
          try {
            const lockStat = await fs.stat(lockFile);
            if (Date.now() - lockStat.mtime > LOCK_CONFIG.TIMEOUT) {
              // Stale lock, remove it
              await fs.unlink(lockFile);
            }
          } catch (statError) {
            // Lock file might have been removed, continue
          }

          // Wait and retry
          await new Promise(resolve => setTimeout(resolve, LOCK_CONFIG.RETRY_DELAY));
          continue;
        }
        throw error;
      }
    }

    throw new Error(`Failed to acquire lock for ${filePath} within timeout`);
  }

  /**
   *
   */
  async _releaseLock(lockId) {
    const lockFile = this.activeLocks.get(lockId);
    if (lockFile) {
      try {
        await fs.unlink(lockFile);
      } catch (error) {
        console.warn('Failed to release lock:', lockFile, error.message);
      }
      this.activeLocks.delete(lockId);
    }
  }

  /**
   *
   */
  async _createBackup(filePath, operationId) {
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-');
    const ext = extname(filePath);
    const base = basename(filePath, ext);
    const dir = dirname(filePath);

    const backupName = operationId
      ? `${base}${this.config.backupSuffix}-${operationId}${ext}`
      : `${base}${this.config.backupSuffix}-${timestamp}${ext}`;

    const backupPath = join(dir, backupName);

    await fs.copyFile(filePath, backupPath);
    return backupPath;
  }

  /**
   *
   */
  async _createTempFile(filePath) {
    const dir = dirname(filePath);
    const base = basename(filePath);
    const tempName = `.kgen-temp-${base}-${Date.now()}-${Math.random().toString(36)}`;
    return join(dir, tempName);
  }

  /**
   *
   */
  async _calculateChecksum(filePath, algorithm = CHECKSUM_ALGORITHMS.SHA256) {
    const hash = createHash(algorithm);
    const stream = createReadStream(filePath);

    await pipeline(stream, hash);
    return hash.digest('hex');
  }
}

/**
 * Transaction class for multi-file atomic operations
 */
class AtomicTransaction {
  constructor(operationId, config) {
    this.operationId = operationId;
    this.config = config;
    this.preparedWrites = [];
    this.backups = [];
    this.locks = [];
    this.committed = false;
  }

  async prepareWrite(filePath, content, options = {}) {
    if (this.committed) {
      throw new Error('Transaction already committed');
    }

    // Acquire lock
    const atomicWriter = new AtomicWriter(this.config);
    const lockId = await atomicWriter._acquireLock(filePath);
    this.locks.push({ lockId, filePath, writer: atomicWriter });

    // Create backup
    let backupPath = null;
    try {
      backupPath = await atomicWriter._createBackup(filePath, this.operationId);
      this.backups.push({ filePath, backupPath });
    } catch (error) {
      if (error.code !== 'ENOENT') throw error;
    }

    // Create temp file with content
    const tempPath = await atomicWriter._createTempFile(filePath);
    await fs.writeFile(tempPath, content, options.encoding || 'utf8');

    // Calculate checksum
    const checksum = await atomicWriter._calculateChecksum(tempPath);

    const preparedWrite = {
      filePath,
      tempPath,
      backupPath,
      checksum,
      options
    };

    this.preparedWrites.push(preparedWrite);

    return { checksum };
  }

  async commit() {
    if (this.committed) {
      throw new Error('Transaction already committed');
    }

    try {
      // Validate all temp files exist and are ready
      for (const write of this.preparedWrites) {
        await fs.access(write.tempPath);
      }

      // Atomically rename all temp files to their targets
      for (const write of this.preparedWrites) {
        await fs.rename(write.tempPath, write.filePath);
      }

      this.committed = true;

      // Clean up locks
      await this._releaseLocks();

      return {
        success: true,
        filesWritten: this.preparedWrites.length,
        backups: this.backups.map(b => b.backupPath)
      };

    } catch (error) {
      await this.rollback();
      throw new Error(`Transaction commit failed: ${error.message}`);
    }
  }

  async rollback() {
    try {
      // Clean up temp files
      for (const write of this.preparedWrites) {
        try {
          await fs.unlink(write.tempPath);
        } catch (error) {
          console.warn('Failed to cleanup temp file:', write.tempPath);
        }
      }

      // Restore from backups if any files were partially written
      for (const backup of this.backups) {
        try {
          const targetExists = await fs.access(backup.filePath).then(() => true, () => false);
          if (targetExists) {
            await fs.copyFile(backup.backupPath, backup.filePath);
          }
        } catch (error) {
          console.error('Failed to restore backup:', backup, error);
        }
      }

      await this._releaseLocks();

    } catch (error) {
      console.error('Rollback failed:', error);
      throw new Error(`Transaction rollback failed: ${error.message}`);
    }
  }

  async _releaseLocks() {
    for (const { lockId, writer } of this.locks) {
      try {
        await writer._releaseLock(lockId);
      } catch (error) {
        console.warn('Failed to release lock:', lockId, error);
      }
    }
    this.locks = [];
  }
}
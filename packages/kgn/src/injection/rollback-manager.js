/**
 * KGEN Rollback Manager
 *
 * Manages rollback operations for failed injections, including
 * backup restoration and operation undo capabilities.
 */

import { promises as fs } from 'fs';
import { join, dirname, basename } from 'path';
import { createHash } from 'crypto';

import { ERROR_CODES, CHECKSUM_ALGORITHMS } from './constants.js';

export class RollbackManager {
  constructor(config = {}) {
    this.config = config;
    this.rollbackHistory = new Map();
    this.operationBackups = new Map();
  }

  /**
   * Rollback a failed operation
   */
  async rollbackOperation(operationId, operationData) {
    const rollbackEntry = {
      operationId,
      timestamp: Date.now(),
      phase: 'starting',
      restoredFiles: [],
      errors: []
    };

    try {
      rollbackEntry.phase = 'restoring-files';

      // Restore files from backups
      if (operationData.backups && operationData.backups.length > 0) {
        for (const backup of operationData.backups) {
          try {
            await this._restoreFromBackup(backup.filePath, backup.backupPath);
            rollbackEntry.restoredFiles.push({
              file: backup.filePath,
              backup: backup.backupPath,
              success: true
            });
          } catch (error) {
            rollbackEntry.errors.push({
              file: backup.filePath,
              error: error.message
            });
          }
        }
      }

      // Clean up temporary files
      rollbackEntry.phase = 'cleanup';
      await this._cleanupTemporaryFiles(operationData);

      // Release locks
      await this._releaseLocks(operationData);

      rollbackEntry.phase = 'completed';
      rollbackEntry.success = rollbackEntry.errors.length === 0;

      this.rollbackHistory.set(operationId, rollbackEntry);

      return {
        success: rollbackEntry.success,
        filesRestored: rollbackEntry.restoredFiles.length,
        errors: rollbackEntry.errors
      };

    } catch (error) {
      rollbackEntry.phase = 'failed';
      rollbackEntry.error = error.message;
      this.rollbackHistory.set(operationId, rollbackEntry);

      throw new Error(`Rollback failed: ${error.message}`);
    }
  }

  /**
   * Undo a completed operation
   */
  async undoOperation(operationId) {
    const operationHistory = this._getOperationHistory(operationId);
    if (!operationHistory) {
      throw new Error(`Operation ${operationId} not found in history`);
    }

    if (operationHistory.phase !== 'committed') {
      throw new Error(`Cannot undo operation ${operationId} - not in committed state`);
    }

    const undoEntry = {
      originalOperationId: operationId,
      undoOperationId: `undo-${operationId}`,
      timestamp: Date.now(),
      phase: 'starting',
      restoredFiles: [],
      errors: []
    };

    try {
      // Find backups for this operation
      const backups = this._findOperationBackups(operationId);
      if (backups.length === 0) {
        throw new Error(`No backups found for operation ${operationId}`);
      }

      undoEntry.phase = 'restoring';

      for (const backup of backups) {
        try {
          await this._restoreFromBackup(backup.targetPath, backup.backupPath);
          undoEntry.restoredFiles.push({
            file: backup.targetPath,
            backup: backup.backupPath,
            success: true
          });
        } catch (error) {
          undoEntry.errors.push({
            file: backup.targetPath,
            error: error.message
          });
        }
      }

      undoEntry.phase = 'completed';
      undoEntry.success = undoEntry.errors.length === 0;

      // Mark original operation as undone
      operationHistory.undone = true;
      operationHistory.undoTimestamp = Date.now();

      this.rollbackHistory.set(undoEntry.undoOperationId, undoEntry);

      return {
        success: undoEntry.success,
        filesRestored: undoEntry.restoredFiles.length,
        errors: undoEntry.errors,
        operationId: undoEntry.undoOperationId
      };

    } catch (error) {
      undoEntry.phase = 'failed';
      undoEntry.error = error.message;
      this.rollbackHistory.set(undoEntry.undoOperationId, undoEntry);

      throw error;
    }
  }

  /**
   * Register backup for tracking
   */
  registerBackup(operationId, targetPath, backupPath, checksum) {
    if (!this.operationBackups.has(operationId)) {
      this.operationBackups.set(operationId, []);
    }

    this.operationBackups.get(operationId).push({
      targetPath,
      backupPath,
      checksum,
      timestamp: Date.now()
    });
  }

  /**
   * Get rollback history
   */
  getRollbackHistory(operationId = null) {
    if (operationId) {
      return this.rollbackHistory.get(operationId);
    }
    return Array.from(this.rollbackHistory.values());
  }

  /**
   * Clean up old backups
   */
  async cleanupOldBackups(maxAge = 24 * 60 * 60 * 1000) { // 24 hours default
    const cutoffTime = Date.now() - maxAge;
    const toCleanup = [];

    for (const [operationId, backups] of this.operationBackups.entries()) {
      const oldBackups = backups.filter(backup => backup.timestamp < cutoffTime);

      for (const backup of oldBackups) {
        try {
          await fs.unlink(backup.backupPath);
          toCleanup.push(backup);
        } catch (error) {
          console.warn(`Failed to cleanup backup ${backup.backupPath}:`, error.message);
        }
      }

      // Remove cleaned up backups from tracking
      const remainingBackups = backups.filter(backup => backup.timestamp >= cutoffTime);
      if (remainingBackups.length === 0) {
        this.operationBackups.delete(operationId);
      } else {
        this.operationBackups.set(operationId, remainingBackups);
      }
    }

    return {
      cleanedBackups: toCleanup.length,
      remainingOperations: this.operationBackups.size
    };
  }

  /**
   * Private Methods
   */

  async _restoreFromBackup(targetPath, backupPath) {
    // Verify backup exists
    try {
      await fs.access(backupPath);
    } catch (error) {
      throw new Error(`Backup file not found: ${backupPath}`);
    }

    // Verify backup integrity if checksum available
    const backupChecksum = await this._calculateFileChecksum(backupPath);

    // Create target directory if it doesn't exist
    const targetDir = dirname(targetPath);
    await fs.mkdir(targetDir, { recursive: true });

    // Copy backup to target (atomic operation)
    const tempPath = `${targetPath}.kgen-restore-${Date.now()}`;

    try {
      await fs.copyFile(backupPath, tempPath);
      await fs.rename(tempPath, targetPath);

      // Verify restoration
      const restoredChecksum = await this._calculateFileChecksum(targetPath);
      if (restoredChecksum !== backupChecksum) {
        throw new Error('Restored file checksum does not match backup');
      }

    } catch (error) {
      // Clean up temp file on failure
      try {
        await fs.unlink(tempPath);
      } catch (cleanupError) {
        console.warn('Failed to cleanup temp restore file:', tempPath);
      }
      throw error;
    }
  }

  async _cleanupTemporaryFiles(operationData) {
    // Clean up any temporary files created during operation
    if (operationData.tempFiles) {
      for (const tempFile of operationData.tempFiles) {
        try {
          await fs.unlink(tempFile);
        } catch (error) {
          console.warn(`Failed to cleanup temp file ${tempFile}:`, error.message);
        }
      }
    }
  }

  async _releaseLocks(operationData) {
    // Release any locks held by the operation
    if (operationData.locks) {
      for (const lockFile of operationData.locks) {
        try {
          await fs.unlink(lockFile);
        } catch (error) {
          console.warn(`Failed to release lock ${lockFile}:`, error.message);
        }
      }
    }
  }

  _getOperationHistory(operationId) {
    // This would normally come from the injection engine's history
    // For now, return a mock structure
    return {
      id: operationId,
      phase: 'committed',
      results: [],
      metadata: {}
    };
  }

  _findOperationBackups(operationId) {
    return this.operationBackups.get(operationId) || [];
  }

  async _calculateFileChecksum(filePath, algorithm = CHECKSUM_ALGORITHMS.SHA256) {
    const content = await fs.readFile(filePath);
    const hash = createHash(algorithm);
    hash.update(content);
    return hash.digest('hex');
  }

  /**
   * Verify operation can be undone
   */
  async verifyUndoable(operationId) {
    const operationHistory = this._getOperationHistory(operationId);
    if (!operationHistory) {
      return { undoable: false, reason: 'Operation not found' };
    }

    if (operationHistory.undone) {
      return { undoable: false, reason: 'Operation already undone' };
    }

    if (operationHistory.phase !== 'committed') {
      return { undoable: false, reason: 'Operation not completed successfully' };
    }

    const backups = this._findOperationBackups(operationId);
    if (backups.length === 0) {
      return { undoable: false, reason: 'No backups available' };
    }

    // Verify all backup files exist
    for (const backup of backups) {
      try {
        await fs.access(backup.backupPath);
      } catch (error) {
        return { undoable: false, reason: `Backup file missing: ${backup.backupPath}` };
      }
    }

    return { undoable: true, backupsCount: backups.length };
  }

  /**
   * Create operation snapshot for recovery
   */
  async createOperationSnapshot(operationId, targets) {
    const snapshot = {
      operationId,
      timestamp: Date.now(),
      targets: [],
      checksums: new Map()
    };

    for (const target of targets) {
      try {
        const exists = await fs.access(target.resolvedPath).then(() => true, () => false);
        const targetInfo = {
          path: target.resolvedPath,
          exists,
          checksum: null,
          stats: null
        };

        if (exists) {
          targetInfo.checksum = await this._calculateFileChecksum(target.resolvedPath);
          targetInfo.stats = await fs.stat(target.resolvedPath);
        }

        snapshot.targets.push(targetInfo);
      } catch (error) {
        console.warn(`Failed to snapshot target ${target.resolvedPath}:`, error.message);
      }
    }

    return snapshot;
  }
}
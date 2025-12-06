/**
 * KGEN Injection Engine
 *
 * Main orchestrator for all injection operations. Provides atomic,
 * idempotent, and deterministic file modification capabilities.
 */

import { promises as fs } from 'fs';
import { join, dirname, resolve, relative } from 'path';
import { createHash } from 'crypto';

import { TargetResolver } from './target-resolver.js';
import { AtomicWriter } from './atomic-writer.js';
import { IdempotencyManager } from './idempotency-manager.js';
import { ValidationEngine } from './validation-engine.js';
import { RollbackManager } from './rollback-manager.js';
import { InjectionModes } from './modes/index.js';

import {
  INJECTION_MODES,
  ERROR_CODES,
  DEFAULT_CONFIG,
  OPERATION_METADATA
} from './constants.js';

export class InjectionEngine {
  constructor(config = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config };

    // Initialize components
    this.targetResolver = new TargetResolver(this.config);
    this.atomicWriter = new AtomicWriter(this.config);
    this.idempotencyManager = new IdempotencyManager(this.config);
    this.validationEngine = new ValidationEngine(this.config);
    this.rollbackManager = new RollbackManager(this.config);
    this.injectionModes = new InjectionModes(this.config);

    // Operation state
    this.activeOperations = new Map();
    this.operationHistory = [];
  }

  /**
   * Main injection method - atomic, idempotent, deterministic
   */
  async inject(templateConfig, content, variables = {}) {
    const operationId = this._generateOperationId(templateConfig, content);

    try {
      // Start atomic operation
      await this._beginOperation(operationId, templateConfig);

      // Resolve targets deterministically
      const targets = await this.targetResolver.resolveTargets(templateConfig, variables);

      // Validate all targets before any modifications
      await this._validateAllTargets(targets);

      // Check idempotency for all targets
      const filteredTargets = await this._filterIdempotentTargets(targets, content, variables);

      if (filteredTargets.length === 0) {
        return {
          success: true,
          skipped: true,
          message: 'All injections skipped - idempotent conditions met',
          targets: targets.length
        };
      }

      // Execute atomic multi-target injection
      const results = await this._executeAtomicInjection(filteredTargets, content, variables, operationId);

      // Commit operation
      await this._commitOperation(operationId, results);

      return {
        success: true,
        operationId,
        results,
        targets: filteredTargets.length,
        skipped: targets.length - filteredTargets.length
      };

    } catch (error) {
      // Atomic rollback on any failure
      await this._rollbackOperation(operationId, error);
      throw this._wrapError(error, operationId);
    }
  }

  /**
   * Dry run - shows what would be done without making changes
   */
  async dryRun(templateConfig, content, variables = {}) {
    const targets = await this.targetResolver.resolveTargets(templateConfig, variables);
    const validationResults = await this._validateAllTargets(targets, false); // Non-failing validation
    const idempotencyResults = await this._checkIdempotency(targets, content, variables);

    return {
      targets: targets.map((target, index) => ({
        path: target.resolvedPath,
        mode: target.mode,
        valid: validationResults[index].valid,
        wouldSkip: idempotencyResults[index].skip,
        reason: idempotencyResults[index].reason,
        validation: validationResults[index]
      }))
    };
  }

  /**
   * Undo previous injection
   */
  async undo(operationId) {
    return await this.rollbackManager.undoOperation(operationId);
  }

  /**
   * Get operation history
   */
  getOperationHistory() {
    return [...this.operationHistory];
  }

  /**
   * Private Methods
   */

  _generateOperationId(templateConfig, content) {
    const hash = createHash('sha256');
    hash.update(JSON.stringify(templateConfig, null, 0));
    hash.update(content);
    hash.update(Date.now().toString());
    return `injection-${hash.digest('hex').substring(0, 16)}`;
  }

  async _beginOperation(operationId, templateConfig) {
    this.activeOperations.set(operationId, {
      id: operationId,
      config: templateConfig,
      startTime: Date.now(),
      phase: 'initializing',
      targets: [],
      backups: []
    });
  }

  async _validateAllTargets(targets, throwOnError = true) {
    const results = [];

    for (const target of targets) {
      try {
        const result = await this.validationEngine.validateTarget(target);
        results.push(result);

        if (throwOnError && !result.valid) {
          throw new Error(`Target validation failed: ${result.errors.join(', ')}`);
        }
      } catch (error) {
        if (throwOnError) throw error;
        results.push({ valid: false, errors: [error.message] });
      }
    }

    return results;
  }

  async _filterIdempotentTargets(targets, content, variables) {
    const filtered = [];

    for (const target of targets) {
      const shouldSkip = await this.idempotencyManager.shouldSkipInjection(
        target, content, variables
      );

      if (!shouldSkip.skip) {
        filtered.push(target);
      } else {
        console.log(`Skipping ${target.resolvedPath}: ${shouldSkip.reason}`);
      }
    }

    return filtered;
  }

  async _checkIdempotency(targets, content, variables) {
    const results = [];

    for (const target of targets) {
      const result = await this.idempotencyManager.shouldSkipInjection(
        target, content, variables
      );
      results.push(result);
    }

    return results;
  }

  async _executeAtomicInjection(targets, content, variables, operationId) {
    const operation = this.activeOperations.get(operationId);
    operation.phase = 'executing';
    operation.targets = targets;

    // If single target, use simple atomic write
    if (targets.length === 1) {
      return [await this._injectSingleTarget(targets[0], content, variables, operationId)];
    }

    // Multiple targets require transaction
    return await this._executeTransaction(targets, content, variables, operationId);
  }

  async _injectSingleTarget(target, content, variables, operationId) {
    // Read current file content
    const currentContent = await this._readTargetFile(target);

    // Apply injection mode
    const modifiedContent = await this.injectionModes.applyMode(
      target.mode,
      currentContent,
      content,
      target,
      variables
    );

    // Validate resulting content
    await this.validationEngine.validateContent(modifiedContent, target);

    // Write atomically
    const result = await this.atomicWriter.writeAtomic(
      target.resolvedPath,
      modifiedContent,
      {
        backup: this.config.backupEnabled,
        operationId,
        preserveMetadata: this.config.preservePermissions
      }
    );

    return {
      target: target.resolvedPath,
      mode: target.mode,
      success: true,
      backup: result.backupPath,
      checksum: result.checksum
    };
  }

  async _executeTransaction(targets, content, variables, operationId) {
    const transaction = await this.atomicWriter.beginTransaction(operationId);

    try {
      const results = [];

      // Prepare all operations
      for (const target of targets) {
        const currentContent = await this._readTargetFile(target);
        const modifiedContent = await this.injectionModes.applyMode(
          target.mode,
          currentContent,
          content,
          target,
          variables
        );

        await this.validationEngine.validateContent(modifiedContent, target);

        const result = await transaction.prepareWrite(
          target.resolvedPath,
          modifiedContent
        );

        results.push({
          target: target.resolvedPath,
          mode: target.mode,
          prepared: true,
          checksum: result.checksum
        });
      }

      // Commit all changes atomically
      await transaction.commit();

      return results.map(r => ({ ...r, success: true, prepared: false }));

    } catch (error) {
      await transaction.rollback();
      throw error;
    }
  }

  async _readTargetFile(target) {
    if (target.mode === INJECTION_MODES.CREATE && target.createIfMissing) {
      try {
        return await fs.readFile(target.resolvedPath, 'utf8');
      } catch (error) {
        if (error.code === 'ENOENT') {
          // Create directory structure if needed
          if (target.createDirectories) {
            await fs.mkdir(dirname(target.resolvedPath), { recursive: true });
          }
          return '';
        }
        throw error;
      }
    }

    return await fs.readFile(target.resolvedPath, 'utf8');
  }

  async _commitOperation(operationId, results) {
    const operation = this.activeOperations.get(operationId);
    operation.phase = 'committed';
    operation.endTime = Date.now();
    operation.results = results;

    // Record in history
    this.operationHistory.push({
      ...operation,
      metadata: {
        ...OPERATION_METADATA,
        timestamp: new Date().toISOString(),
        duration: operation.endTime - operation.startTime
      }
    });

    // Clean up active operation
    this.activeOperations.delete(operationId);
  }

  async _rollbackOperation(operationId, error) {
    const operation = this.activeOperations.get(operationId);

    if (operation) {
      operation.phase = 'rolling-back';

      try {
        await this.rollbackManager.rollbackOperation(operationId, operation);
        operation.phase = 'rolled-back';
      } catch (rollbackError) {
        operation.phase = 'rollback-failed';
        operation.rollbackError = rollbackError.message;
        console.error('Rollback failed:', rollbackError);
      }

      operation.error = error.message;
      operation.endTime = Date.now();

      // Record failed operation in history
      this.operationHistory.push({ ...operation });
      this.activeOperations.delete(operationId);
    }
  }

  _wrapError(error, operationId) {
    return new InjectionError(
      error.message,
      error.code || ERROR_CODES.ATOMIC_FAILURE,
      operationId,
      error
    );
  }
}

/**
 * Custom error class for injection operations
 */
export class InjectionError extends Error {
  constructor(message, code, operationId, originalError = null) {
    super(message);
    this.name = 'InjectionError';
    this.code = code;
    this.operationId = operationId;
    this.originalError = originalError;
    this.timestamp = new Date().toISOString();
  }
}
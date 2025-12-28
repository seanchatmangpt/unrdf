/**
 * @fileoverview Error Recovery with Checkpoint Support
 *
 * **Purpose**: Enable recovery from failures using checkpoints
 * - Transaction-style rollback on failures
 * - Checkpoint-based recovery (resume from A_τ-1)
 * - State persistence for crash recovery
 * - Recovery strategies per error type
 *
 * @module resilience/error-recovery
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

/**
 * Checkpoint schema
 */
export const CheckpointSchema = z.object({
  id: z.string(),
  epoch: z.string(),
  timestamp: z.string(),
  state: z.record(z.any()),
  metadata: z.record(z.any()).optional(),
});

/**
 * Recovery strategy enumeration
 */
export const RecoveryStrategy = {
  ROLLBACK: 'rollback', // Undo changes and return to checkpoint
  RETRY: 'retry', // Retry from checkpoint
  CONTINUE: 'continue', // Continue from checkpoint with degraded state
  SKIP: 'skip', // Skip failed operation and continue
};

/**
 * Error Recovery Manager
 *
 * @example
 * const recovery = new ErrorRecoveryManager();
 * const checkpoint = recovery.createCheckpoint('epoch_001', { data: [...] });
 *
 * try {
 *   await dangerousOperation();
 * } catch (error) {
 *   await recovery.recoverFromCheckpoint(checkpoint.id);
 * }
 */
export class ErrorRecoveryManager {
  /**
   * Create a new error recovery manager
   * @param {Object} [config] - Manager configuration
   */
  constructor(config = {}) {
    this.config = {
      maxCheckpoints: config.maxCheckpoints ?? 100,
      checkpointTTLMs: config.checkpointTTLMs ?? 3600000, // 1 hour
      enablePersistence: config.enablePersistence ?? false,
      persistencePath: config.persistencePath ?? null,
    };

    this.checkpoints = new Map();
    this.checkpointHistory = [];
    this.tracer = trace.getTracer('unrdf-error-recovery');

    this.metrics = {
      checkpointsCreated: 0,
      recoveriesAttempted: 0,
      recoveriesSucceeded: 0,
      recoveriesFailed: 0,
      rollbacksPerformed: 0,
    };
  }

  /**
   * Create a checkpoint
   * @param {string} epoch - Epoch identifier
   * @param {Object} state - State to checkpoint
   * @param {Object} [metadata] - Additional metadata
   * @returns {Object} Checkpoint
   */
  createCheckpoint(epoch, state, metadata = {}) {
    const checkpoint = {
      id: `checkpoint_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`,
      epoch,
      timestamp: new Date().toISOString(),
      state: structuredClone(state), // Deep copy
      metadata: {
        ...metadata,
        createdAt: Date.now(),
      },
    };

    // Validate checkpoint
    const validated = CheckpointSchema.parse(checkpoint);

    // Store checkpoint
    this.checkpoints.set(validated.id, validated);
    this.checkpointHistory.push({
      id: validated.id,
      epoch: validated.epoch,
      timestamp: validated.timestamp,
    });

    this.metrics.checkpointsCreated++;

    // Cleanup old checkpoints
    this._cleanupOldCheckpoints();

    console.log(`[Recovery] Checkpoint created: ${validated.id} for epoch ${epoch}`);

    return validated;
  }

  /**
   * Recover from a checkpoint
   * @param {string} checkpointId - Checkpoint ID
   * @param {string} [strategy='rollback'] - Recovery strategy
   * @returns {Object} Recovered state
   */
  async recoverFromCheckpoint(checkpointId, strategy = RecoveryStrategy.ROLLBACK) {
    return await this.tracer.startActiveSpan(
      'recovery.recover_from_checkpoint',
      async (span) => {
        span.setAttributes({
          'recovery.checkpoint_id': checkpointId,
          'recovery.strategy': strategy,
        });

        this.metrics.recoveriesAttempted++;

        const checkpoint = this.checkpoints.get(checkpointId);

        if (!checkpoint) {
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: 'Checkpoint not found',
          });
          span.end();
          throw new Error(`Checkpoint not found: ${checkpointId}`);
        }

        try {
          let recoveredState;

          switch (strategy) {
            case RecoveryStrategy.ROLLBACK:
              recoveredState = await this._performRollback(checkpoint);
              this.metrics.rollbacksPerformed++;
              break;

            case RecoveryStrategy.RETRY:
              recoveredState = checkpoint.state;
              break;

            case RecoveryStrategy.CONTINUE:
              recoveredState = { ...checkpoint.state, degraded: true };
              break;

            case RecoveryStrategy.SKIP:
              recoveredState = { ...checkpoint.state, skipped: true };
              break;

            default:
              throw new Error(`Unknown recovery strategy: ${strategy}`);
          }

          this.metrics.recoveriesSucceeded++;

          span.setAttributes({
            'recovery.success': true,
            'recovery.epoch': checkpoint.epoch,
          });
          span.setStatus({ code: SpanStatusCode.OK });

          console.log(
            `[Recovery] Recovered from checkpoint ${checkpointId} using strategy '${strategy}'`
          );

          return {
            checkpoint,
            recoveredState,
            strategy,
            timestamp: new Date().toISOString(),
          };
        } catch (error) {
          this.metrics.recoveriesFailed++;

          span.recordException(error);
          span.setStatus({
            code: SpanStatusCode.ERROR,
            message: 'Recovery failed',
          });
          throw error;
        } finally {
          span.end();
        }
      }
    );
  }

  /**
   * Perform rollback to checkpoint state
   * @param {Object} checkpoint - Checkpoint to rollback to
   * @returns {Object} Rolled back state
   * @private
   */
  async _performRollback(checkpoint) {
    console.log(
      `[Recovery] Rolling back to checkpoint ${checkpoint.id} (epoch: ${checkpoint.epoch})`
    );

    // Return deep copy of checkpoint state
    return structuredClone(checkpoint.state);
  }

  /**
   * Execute operation with automatic checkpoint and recovery
   * @param {string} epoch - Epoch identifier
   * @param {Object} initialState - Initial state
   * @param {Function} operation - Operation to execute
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Operation result
   */
  async executeWithRecovery(epoch, initialState, operation, options = {}) {
    const {
      createCheckpoint = true,
      recoveryStrategy = RecoveryStrategy.ROLLBACK,
      retryOnFailure = false,
      maxRetries = 3,
    } = options;

    return await this.tracer.startActiveSpan(
      'recovery.execute_with_recovery',
      async (span) => {
        span.setAttributes({
          'recovery.epoch': epoch,
          'recovery.create_checkpoint': createCheckpoint,
          'recovery.strategy': recoveryStrategy,
        });

        let checkpoint = null;

        // Create checkpoint if requested
        if (createCheckpoint) {
          checkpoint = this.createCheckpoint(epoch, initialState);
          span.setAttributes({ 'recovery.checkpoint_id': checkpoint.id });
        }

        let attempts = 0;
        let lastError = null;

        while (attempts <= maxRetries) {
          attempts++;

          try {
            const result = await operation(initialState);

            span.setStatus({ code: SpanStatusCode.OK });
            span.end();

            return {
              result,
              checkpoint,
              recovered: attempts > 1,
              attempts,
            };
          } catch (error) {
            lastError = error;

            span.recordException(error);
            span.setAttributes({
              'recovery.error': error.message,
              'recovery.attempt': attempts,
            });

            // If no checkpoint or last retry, fail
            if (!checkpoint || attempts > maxRetries) {
              span.setStatus({
                code: SpanStatusCode.ERROR,
                message: 'Recovery failed',
              });
              span.end();
              throw error;
            }

            // Recover from checkpoint
            if (retryOnFailure) {
              console.warn(
                `[Recovery] Operation failed (attempt ${attempts}/${maxRetries + 1}), recovering from checkpoint...`
              );

              const recovery = await this.recoverFromCheckpoint(
                checkpoint.id,
                recoveryStrategy
              );
              initialState = recovery.recoveredState;

              // Continue to next retry
              continue;
            } else {
              // No retry, just recover state and fail
              await this.recoverFromCheckpoint(checkpoint.id, recoveryStrategy);
              throw error;
            }
          }
        }

        // Should never reach here
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Unexpected recovery exit',
        });
        span.end();
        throw lastError;
      }
    );
  }

  /**
   * Get checkpoint by ID
   * @param {string} checkpointId - Checkpoint ID
   * @returns {Object|null} Checkpoint or null
   */
  getCheckpoint(checkpointId) {
    return this.checkpoints.get(checkpointId) || null;
  }

  /**
   * Get latest checkpoint for epoch
   * @param {string} epoch - Epoch identifier
   * @returns {Object|null} Latest checkpoint or null
   */
  getLatestCheckpointForEpoch(epoch) {
    const checkpoints = Array.from(this.checkpoints.values()).filter(
      (cp) => cp.epoch === epoch
    );

    if (checkpoints.length === 0) return null;

    // Sort by timestamp descending
    checkpoints.sort(
      (a, b) => new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime()
    );

    return checkpoints[0];
  }

  /**
   * Delete checkpoint
   * @param {string} checkpointId - Checkpoint ID
   * @returns {boolean} True if deleted
   */
  deleteCheckpoint(checkpointId) {
    return this.checkpoints.delete(checkpointId);
  }

  /**
   * Cleanup old checkpoints
   * @private
   */
  _cleanupOldCheckpoints() {
    const now = Date.now();

    // Remove expired checkpoints
    for (const [id, checkpoint] of this.checkpoints.entries()) {
      const age = now - checkpoint.metadata.createdAt;
      if (age > this.config.checkpointTTLMs) {
        this.checkpoints.delete(id);
        console.log(`[Recovery] Deleted expired checkpoint: ${id}`);
      }
    }

    // Keep only latest N checkpoints
    if (this.checkpoints.size > this.config.maxCheckpoints) {
      const sorted = Array.from(this.checkpoints.entries()).sort(
        (a, b) =>
          b[1].metadata.createdAt - a[1].metadata.createdAt
      );

      const toDelete = sorted.slice(this.config.maxCheckpoints);
      for (const [id] of toDelete) {
        this.checkpoints.delete(id);
      }
    }
  }

  /**
   * Get recovery metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      activeCheckpoints: this.checkpoints.size,
      recoverySuccessRate:
        this.metrics.recoveriesAttempted > 0
          ? (
              (this.metrics.recoveriesSucceeded /
                this.metrics.recoveriesAttempted) *
              100
            ).toFixed(2) + '%'
          : 'N/A',
    };
  }

  /**
   * Reset metrics
   */
  resetMetrics() {
    this.metrics = {
      checkpointsCreated: 0,
      recoveriesAttempted: 0,
      recoveriesSucceeded: 0,
      recoveriesFailed: 0,
      rollbacksPerformed: 0,
    };
  }

  /**
   * Clear all checkpoints
   */
  clearAllCheckpoints() {
    this.checkpoints.clear();
    this.checkpointHistory = [];
    console.log('[Recovery] All checkpoints cleared');
  }
}

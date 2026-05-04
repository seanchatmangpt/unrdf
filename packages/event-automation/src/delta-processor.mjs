/**
 * @file Delta Processor
 * @module @unrdf/event-automation/delta-processor
 * @description Processes deltas from V6-Core ΔGate with validation and transformation
 */

import { DeltaSchema, DeltaOperationSchema } from './schemas.mjs';
import { sha256 } from 'hash-wasm';

/**
 * Delta processor for handling delta operations
 */
export class DeltaProcessor {
  /**
   * Creates a new delta processor
   * @param {Object} [options] - Processor options
   * @param {Object} [options.logger] - Logger instance
   * @param {boolean} [options.validateOperations=true] - Validate operations
   */
  constructor(options = {}) {
    this.logger = options.logger || console;
    this.validateOperations = options.validateOperations !== false;
    this.processedDeltas = new Map();
    this.metrics = {
      totalProcessed: 0,
      totalSucceeded: 0,
      totalFailed: 0,
      durations: [],
    };
  }

  /**
   * Process a delta
   * @param {Object} delta - Delta to process
   * @returns {Promise<Object>} Processing result
   * @throws {Error} If delta is invalid
   */
  async processDelta(delta) {
    const startTime = performance.now();

    try {
      // Validate delta structure
      const validated = DeltaSchema.parse(delta);

      // Check for duplicate processing
      if (this.processedDeltas.has(validated.id)) {
        return {
          success: true,
          deltaId: validated.id,
          duplicate: true,
          duration: performance.now() - startTime,
        };
      }

      // Validate operations if enabled
      if (this.validateOperations) {
        for (const op of validated.operations) {
          this._validateOperation(op);
        }
      }

      // Generate delta hash
      const hash = await this._generateDeltaHash(validated);

      // Mark as processed
      this.processedDeltas.set(validated.id, {
        timestamp: Date.now(),
        hash,
      });

      // Update metrics
      const duration = performance.now() - startTime;
      this.metrics.totalProcessed++;
      this.metrics.totalSucceeded++;
      this.metrics.durations.push(duration);

      return {
        success: true,
        deltaId: validated.id,
        hash,
        operationCount: validated.operations.length,
        duration,
      };
    } catch (error) {
      this.metrics.totalProcessed++;
      this.metrics.totalFailed++;

      this.logger.error('Delta processing failed:', {
        deltaId: delta?.id,
        error: error.message,
      });

      return {
        success: false,
        deltaId: delta?.id || 'unknown',
        error: error.message,
        duration: performance.now() - startTime,
      };
    }
  }

  /**
   * Batch process multiple deltas
   * @param {Array<Object>} deltas - Deltas to process
   * @param {Object} [options] - Batch options
   * @param {boolean} [options.parallel=false] - Process in parallel
   * @returns {Promise<Array<Object>>} Processing results
   */
  async batchProcess(deltas, options = {}) {
    if (options.parallel) {
      return Promise.all(deltas.map((delta) => this.processDelta(delta)));
    }

    const results = [];
    for (const delta of deltas) {
      results.push(await this.processDelta(delta));
    }
    return results;
  }

  /**
   * Validate a single operation
   * @private
   * @param {Object} operation - Operation to validate
   * @throws {Error} If operation is invalid
   */
  _validateOperation(operation) {
    DeltaOperationSchema.parse(operation);

    // Additional semantic validation
    if (operation.subject.trim() === '') {
      throw new Error('Subject cannot be empty');
    }
    if (operation.predicate.trim() === '') {
      throw new Error('Predicate cannot be empty');
    }
    if (operation.object.trim() === '') {
      throw new Error('Object cannot be empty');
    }
  }

  /**
   * Generate hash for delta
   * @private
   * @param {Object} delta - Delta to hash
   * @returns {Promise<string>} Delta hash
   */
  async _generateDeltaHash(delta) {
    const content = JSON.stringify({
      id: delta.id,
      operations: delta.operations,
      timestamp: delta.timestamp,
    });
    return sha256(content);
  }

  /**
   * Get processing metrics
   * @returns {Object} Processing metrics
   */
  getMetrics() {
    const durations = this.metrics.durations;
    const avgDuration =
      durations.length > 0
        ? durations.reduce((a, b) => a + b, 0) / durations.length
        : 0;

    const sorted = [...durations].sort((a, b) => a - b);
    const p95Index = Math.floor(sorted.length * 0.95);
    const p99Index = Math.floor(sorted.length * 0.99);

    return {
      totalProcessed: this.metrics.totalProcessed,
      totalSucceeded: this.metrics.totalSucceeded,
      totalFailed: this.metrics.totalFailed,
      averageDuration: avgDuration,
      p95Duration: sorted[p95Index] || 0,
      p99Duration: sorted[p99Index] || 0,
    };
  }

  /**
   * Reset processor state
   */
  reset() {
    this.processedDeltas.clear();
    this.metrics = {
      totalProcessed: 0,
      totalSucceeded: 0,
      totalFailed: 0,
      durations: [],
    };
  }

  /**
   * Check if delta was processed
   * @param {string} deltaId - Delta ID to check
   * @returns {boolean} Whether delta was processed
   */
  wasProcessed(deltaId) {
    return this.processedDeltas.has(deltaId);
  }

  /**
   * Get processed delta info
   * @param {string} deltaId - Delta ID
   * @returns {Object|undefined} Processed delta info
   */
  getProcessedInfo(deltaId) {
    return this.processedDeltas.get(deltaId);
  }
}

/**
 * Create a delta processor instance
 * @param {Object} [options] - Processor options
 * @returns {DeltaProcessor} Delta processor instance
 */
export function createDeltaProcessor(options = {}) {
  return new DeltaProcessor(options);
}

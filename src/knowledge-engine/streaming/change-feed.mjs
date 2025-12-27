/**
 * @file Change Feed for RDF graph change notifications
 * @module streaming/change-feed
 *
 * @description
 * Captures RDF graph changes (adds, deletes, updates) and generates delta
 * objects for streaming to subscribers. Supports batch and streaming modes,
 * change history tracking, and integration with transactions.
 */

import { EventEmitter } from 'events';
import { randomUUID } from 'crypto';
import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { createObservabilityManager } from '../observability.mjs';

const tracer = trace.getTracer('unrdf-streaming');

/**
 * Change types
 * @enum {string}
 */
export const ChangeType = {
  ADD: 'add',
  DELETE: 'delete',
  UPDATE: 'update',
  TRANSACTION: 'transaction',
};

/**
 * Change event schema
 */
const ChangeEventSchema = z.object({
  id: z.string(),
  type: z.enum([ChangeType.ADD, ChangeType.DELETE, ChangeType.UPDATE, ChangeType.TRANSACTION]),
  timestamp: z.number(),
  delta: z.object({
    additions: z.array(z.any()),
    removals: z.array(z.any()),
  }),
  metadata: z
    .object({
      actor: z.string().optional(),
      transactionId: z.string().optional(),
      source: z.string().optional(),
    })
    .optional(),
});

/**
 * Change feed configuration schema
 */
const ChangeFeedConfigSchema = z.object({
  enableHistory: z.boolean().default(true),
  historySize: z.number().min(1).default(1000),
  batchMode: z.boolean().default(false),
  batchSize: z.number().min(1).default(10),
  batchInterval: z.number().min(10).default(1000),
  enableCompression: z.boolean().default(false),
  observability: z
    .object({
      serviceName: z.string().default('unrdf-change-feed'),
      enableTracing: z.boolean().default(true),
      enableMetrics: z.boolean().default(true),
    })
    .optional(),
});

/**
 * Change Feed
 * Captures and broadcasts RDF graph changes
 */
export class ChangeFeed extends EventEmitter {
  /**
   * Create a new change feed
   * @param {Object} config - Feed configuration
   */
  constructor(config = {}) {
    super();
    this.config = ChangeFeedConfigSchema.parse(config);
    this.history = [];
    this.batchBuffer = [];
    this.batchTimer = null;
    this.isActive = false;

    // Observability
    this.observability = createObservabilityManager(this.config.observability || {});

    // Metrics
    this.metrics = {
      changesProcessed: 0,
      changesEmitted: 0,
      batchesSent: 0,
      errorCount: 0,
    };

    // Initialize observability
    this.observability.initialize().catch(err => {
      console.warn('[ChangeFeed] Failed to initialize observability:', err.message);
    });
  }

  /**
   * Start the change feed
   */
  start() {
    if (this.isActive) {
      console.log('[ChangeFeed] Already active');
      return;
    }

    this.isActive = true;

    if (this.config.batchMode) {
      this._startBatchTimer();
    }

    this.emit('started');
    console.log('[ChangeFeed] Started');
  }

  /**
   * Stop the change feed
   */
  stop() {
    if (!this.isActive) {
      return;
    }

    this.isActive = false;

    // Flush any pending batch
    if (this.config.batchMode && this.batchBuffer.length > 0) {
      this._flushBatch();
    }

    this._stopBatchTimer();

    this.emit('stopped');
    console.log('[ChangeFeed] Stopped');
  }

  /**
   * Record a change event
   * @param {Object} delta - Transaction delta
   * @param {Object} [metadata] - Change metadata
   * @returns {string} Change ID
   */
  recordChange(delta, metadata = {}) {
    return tracer.startActiveSpan('change-feed.record', span => {
      try {
        if (!this.isActive) {
          throw new Error('Change feed is not active');
        }

        // Determine change type
        let changeType = ChangeType.TRANSACTION;
        if (delta.additions.length > 0 && delta.removals.length === 0) {
          changeType = ChangeType.ADD;
        } else if (delta.additions.length === 0 && delta.removals.length > 0) {
          changeType = ChangeType.DELETE;
        } else if (delta.additions.length > 0 && delta.removals.length > 0) {
          changeType = ChangeType.UPDATE;
        }

        const change = ChangeEventSchema.parse({
          id: randomUUID(),
          type: changeType,
          timestamp: Date.now(),
          delta: {
            additions: delta.additions || [],
            removals: delta.removals || [],
          },
          metadata,
        });

        span.setAttributes({
          'change.id': change.id,
          'change.type': change.type,
          'change.additions_count': change.delta.additions.length,
          'change.removals_count': change.delta.removals.length,
        });

        this.metrics.changesProcessed++;

        // Add to history if enabled
        if (this.config.enableHistory) {
          this._addToHistory(change);
        }

        // Process change (batch or immediate)
        if (this.config.batchMode) {
          this._addToBatch(change);
        } else {
          this._emitChange(change);
        }

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return change.id;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        span.end();
        this.metrics.errorCount++;
        this.observability.recordError(error, { context: 'record-change' });
        throw error;
      }
    });
  }

  /**
   * Add change to history
   * @private
   */
  _addToHistory(change) {
    this.history.push(change);

    // Trim history if it exceeds size limit
    if (this.history.length > this.config.historySize) {
      this.history.shift();
    }
  }

  /**
   * Add change to batch buffer
   * @private
   */
  _addToBatch(change) {
    this.batchBuffer.push(change);

    // Flush if batch is full
    if (this.batchBuffer.length >= this.config.batchSize) {
      this._flushBatch();
    }
  }

  /**
   * Emit a single change event
   * @private
   */
  _emitChange(change) {
    this.emit('change', change);
    this.metrics.changesEmitted++;
  }

  /**
   * Flush batch buffer
   * @private
   */
  _flushBatch() {
    if (this.batchBuffer.length === 0) {
      return;
    }

    return tracer.startActiveSpan('change-feed.flush-batch', span => {
      try {
        span.setAttribute('batch.size', this.batchBuffer.length);

        const batch = {
          id: randomUUID(),
          changes: [...this.batchBuffer],
          timestamp: Date.now(),
          count: this.batchBuffer.length,
        };

        this.emit('batch', batch);
        this.metrics.batchesSent++;
        this.metrics.changesEmitted += batch.count;

        // Clear buffer
        this.batchBuffer = [];

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        span.end();
        this.observability.recordError(error, { context: 'flush-batch' });
      }
    });
  }

  /**
   * Start batch timer
   * @private
   */
  _startBatchTimer() {
    this._stopBatchTimer();

    this.batchTimer = setInterval(() => {
      if (this.batchBuffer.length > 0) {
        this._flushBatch();
      }
    }, this.config.batchInterval);
  }

  /**
   * Stop batch timer
   * @private
   */
  _stopBatchTimer() {
    if (this.batchTimer) {
      clearInterval(this.batchTimer);
      this.batchTimer = null;
    }
  }

  /**
   * Get change history
   * @param {Object} [options] - Query options
   * @param {number} [options.since] - Timestamp filter
   * @param {string} [options.type] - Change type filter
   * @param {number} [options.limit] - Result limit
   * @returns {Array<Object>} Change events
   */
  getHistory(options = {}) {
    let results = [...this.history];

    // Filter by timestamp
    if (options.since) {
      results = results.filter(c => c.timestamp >= options.since);
    }

    // Filter by type
    if (options.type) {
      results = results.filter(c => c.type === options.type);
    }

    // Apply limit
    if (options.limit) {
      results = results.slice(-options.limit);
    }

    return results;
  }

  /**
   * Get change by ID
   * @param {string} id - Change ID
   * @returns {Object|null} Change event or null
   */
  getChange(id) {
    return this.history.find(c => c.id === id) || null;
  }

  /**
   * Clear history
   */
  clearHistory() {
    this.history = [];
  }

  /**
   * Get performance metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      historySize: this.history.length,
      batchBufferSize: this.batchBuffer.length,
      isActive: this.isActive,
    };
  }

  /**
   * Create a compacted delta from multiple changes
   * @param {Array<Object>} changes - Array of change events
   * @returns {Object} Compacted delta
   */
  static compactChanges(changes) {
    const additions = [];
    const removals = [];

    for (const change of changes) {
      if (change.delta) {
        additions.push(...(change.delta.additions || []));
        removals.push(...(change.delta.removals || []));
      }
    }

    return {
      additions,
      removals,
      count: changes.length,
      startTime: changes[0]?.timestamp,
      endTime: changes[changes.length - 1]?.timestamp,
    };
  }

  /**
   * Create a streaming iterator for changes
   * @param {Object} [options] - Iterator options
   * @returns {AsyncIterator<Object>} Change iterator
   */
  async *stream(options = {}) {
    const { batchSize = 1 } = options;
    let buffer = [];

    const changeHandler = change => {
      buffer.push(change);
    };

    const batchHandler = batch => {
      buffer.push(...batch.changes);
    };

    this.on('change', changeHandler);
    this.on('batch', batchHandler);

    try {
      while (this.isActive) {
        if (buffer.length >= batchSize) {
          const batch = buffer.splice(0, batchSize);
          yield* batch;
        } else {
          // Wait for more changes
          await new Promise(resolve => setTimeout(resolve, 100));
        }
      }
    } finally {
      this.off('change', changeHandler);
      this.off('batch', batchHandler);

      // Yield remaining buffered changes
      yield* buffer;
    }
  }

  /**
   * Cleanup resources
   * @returns {Promise<void>}
   */
  async cleanup() {
    this.stop();
    this.clearHistory();
    this.removeAllListeners();
    await this.observability.shutdown();
  }
}

/**
 * Create a change feed instance
 * @param {Object} config - Feed configuration
 * @returns {ChangeFeed} Change feed
 */
export function createChangeFeed(config = {}) {
  return new ChangeFeed(config);
}

/**
 * Create a transaction hook that records changes
 * @param {ChangeFeed} feed - Change feed instance
 * @param {Object} [options] - Hook options
 * @returns {Object} Transaction hook
 */
export function createChangeFeedHook(feed, options = {}) {
  return {
    id: 'change-feed-recorder',
    mode: 'post',
    condition: async () => true,
    effect: async (store, delta) => {
      try {
        feed.recordChange(delta, {
          source: 'transaction-hook',
          ...options.metadata,
        });
      } catch (error) {
        console.error('[ChangeFeedHook] Failed to record change:', error.message);
        if (options.strict) {
          throw error;
        }
      }
    },
  };
}

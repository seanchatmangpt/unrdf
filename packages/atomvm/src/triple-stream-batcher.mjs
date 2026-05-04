/**
 * Triple Stream Batcher
 *
 * Provides batching and streaming layer for bulk RDF triple operations.
 * Optimized for efficient BEAM <-> Oxigraph transfer with configurable
 * batch sizes, timeouts, and backpressure handling.
 *
 * **Performance Target**: >= 10,000 triples/second throughput
 *
 * **Poka-Yoke Design**: Validates all inputs and prevents invalid states.
 *
 * @module triple-stream-batcher
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';

/**
 * Get tracer lazily to ensure provider is registered first
 * @returns {import('@opentelemetry/api').Tracer} OTEL tracer
 */
function getTracer() {
  return trace.getTracer('triple-stream-batcher', '1.0.0');
}

/**
 * @constant {number} Default batch size for triple accumulation
 */
const DEFAULT_BATCH_SIZE = 100;

/**
 * @constant {number} Default timeout in milliseconds before flushing partial batch
 */
const DEFAULT_TIMEOUT_MS = 50;

/**
 * @constant {number} Maximum queue size before applying backpressure
 */
const MAX_QUEUE_SIZE = 10000;

/**
 * Batcher state machine states
 *
 * @typedef {'Idle' | 'Accumulating' | 'Flushing' | 'Paused' | 'Destroyed'} BatcherState
 */

/**
 * Triple object structure for RDF triples
 *
 * @typedef {Object} Triple
 * @property {Object} subject - RDF subject term
 * @property {Object} predicate - RDF predicate term
 * @property {Object} object - RDF object term
 * @property {Object} [graph] - Optional RDF graph term
 */

/**
 * Batch callback result indicating consumer status
 *
 * @typedef {Object} BatchResult
 * @property {boolean} success - Whether batch was processed successfully
 * @property {boolean} [slow] - Whether consumer is slow (triggers backpressure)
 * @property {string} [error] - Error message if failed
 */

/**
 * Batcher options configuration
 *
 * @typedef {Object} BatcherOptions
 * @property {number} [batchSize=100] - Number of triples per batch
 * @property {number} [timeout=50] - Timeout in ms before flushing partial batch
 * @property {number} [maxQueueSize=10000] - Maximum queue size before backpressure
 */

/**
 * Metrics for batch operations
 *
 * @typedef {Object} BatchMetrics
 * @property {number} totalTriples - Total triples processed
 * @property {number} totalBatches - Total batches sent
 * @property {number} avgBatchSize - Average batch size
 * @property {number} avgLatencyMs - Average batch latency in milliseconds
 * @property {number} throughput - Triples per second
 * @property {number} backpressureEvents - Number of backpressure events
 */

/**
 * Validates that a value is a valid triple
 *
 * @param {Triple} triple - Triple to validate
 * @throws {TypeError} If triple is invalid
 */
function validateTriple(triple) {
  if (!triple || typeof triple !== 'object') {
    throw new TypeError('Triple must be a non-null object');
  }
  if (!triple.subject || !triple.predicate || !triple.object) {
    throw new TypeError('Triple must have subject, predicate, and object properties');
  }
}

/**
 * TripleStreamBatcher - Efficient batching for bulk RDF triple operations
 *
 * Accumulates triples into batches for efficient transfer between BEAM and Oxigraph.
 * Supports configurable batch sizes, timeouts, backpressure, and async streaming.
 *
 * @example
 * const batcher = new TripleStreamBatcher({ batchSize: 100, timeout: 50 });
 *
 * batcher.onBatch(async (batch) => {
 *   await oxigraphBridge.insertBatch(batch);
 *   return { success: true };
 * });
 *
 * for (const triple of triples) {
 *   batcher.addTriple(triple);
 * }
 *
 * await batcher.flush();
 */
export class TripleStreamBatcher {
  /**
   * Create a new TripleStreamBatcher
   *
   * @param {BatcherOptions} [options={}] - Batcher configuration
   */
  constructor(options = {}) {
    /** @type {number} */
    this.batchSize = options.batchSize ?? DEFAULT_BATCH_SIZE;

    /** @type {number} */
    this.timeout = options.timeout ?? DEFAULT_TIMEOUT_MS;

    /** @type {number} */
    this.maxQueueSize = options.maxQueueSize ?? MAX_QUEUE_SIZE;

    /** @type {Triple[]} */
    this.queue = [];

    /** @type {BatcherState} */
    this.state = 'Idle';

    /** @type {NodeJS.Timeout|null} */
    this.timeoutHandle = null;

    /** @type {((batch: Triple[]) => Promise<BatchResult>)|null} */
    this.batchCallback = null;

    /** @type {BatchMetrics} */
    this.metrics = {
      totalTriples: 0,
      totalBatches: 0,
      avgBatchSize: 0,
      avgLatencyMs: 0,
      throughput: 0,
      backpressureEvents: 0,
    };

    /** @type {number} */
    this.startTime = Date.now();

    /** @type {number[]} */
    this.latencies = [];

    /** @type {boolean} */
    this.backpressureActive = false;
  }

  /**
   * Check if batcher is ready to accept triples
   *
   * @returns {boolean} True if batcher can accept triples
   */
  isReady() {
    return this.state !== 'Destroyed' && this.state !== 'Paused';
  }

  /**
   * Add a single triple to the batch queue
   *
   * @param {Triple} triple - Triple to add
   * @throws {Error} If batcher is destroyed or paused
   * @throws {TypeError} If triple is invalid
   */
  addTriple(triple) {
    if (this.state === 'Destroyed') {
      throw new Error('Cannot add triple: batcher has been destroyed');
    }

    if (this.state === 'Paused') {
      throw new Error('Cannot add triple: batcher is paused due to backpressure');
    }

    validateTriple(triple);

    this.queue.push(triple);
    this.state = 'Accumulating';

    // Check for batch size threshold
    if (this.queue.length >= this.batchSize) {
      this._sendBatch();
    } else if (!this.timeoutHandle && this.queue.length > 0) {
      // Start timeout timer for partial batch
      this._startTimeout();
    }

    // Check for backpressure
    if (this.queue.length >= this.maxQueueSize) {
      this.state = 'Paused';
      this.backpressureActive = true;
      this.metrics.backpressureEvents++;
    }
  }

  /**
   * Add multiple triples to the batch queue
   *
   * @param {Triple[]} triples - Array of triples to add
   * @throws {Error} If batcher is destroyed
   * @throws {TypeError} If triples is not an array or contains invalid triples
   */
  addTriples(triples) {
    if (!Array.isArray(triples)) {
      throw new TypeError('addTriples requires an array of triples');
    }

    for (const triple of triples) {
      this.addTriple(triple);
    }
  }

  /**
   * Force flush any pending triples in the queue
   *
   * @returns {Promise<void>} Resolves when flush is complete
   */
  async flush() {
    return getTracer().startActiveSpan(
      'triple-batcher.flush',
      {
        attributes: {
          'batcher.queue_size': this.queue.length,
          'batcher.state': this.state,
        },
      },
      async (span) => {
        try {
          this._clearTimeout();

          if (this.queue.length > 0) {
            await this._sendBatch();
          }

          // Wait for any in-flight batches to complete
          while (this.state === 'Flushing') {
            await new Promise((resolve) => setTimeout(resolve, 1));
          }

          span.setStatus({ code: SpanStatusCode.OK });
        } catch (error) {
          span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
          throw error;
        } finally {
          span.end();
        }
      }
    );
  }

  /**
   * Register a callback for when a batch is ready
   *
   * The callback receives the batch of triples and should return a BatchResult.
   * If the callback returns { slow: true }, backpressure will be applied.
   *
   * @param {(batch: Triple[]) => Promise<BatchResult>} callback - Batch handler
   */
  onBatch(callback) {
    if (typeof callback !== 'function') {
      throw new TypeError('onBatch callback must be a function');
    }
    this.batchCallback = callback;
  }

  /**
   * Stream triples from an async iterable source
   *
   * Consumes an async iterable of triples and batches them efficiently.
   * Handles backpressure automatically.
   *
   * @param {AsyncIterable<Triple>} asyncIterable - Async source of triples
   * @returns {Promise<BatchMetrics>} Final metrics after streaming completes
   */
  async streamTriples(asyncIterable) {
    return getTracer().startActiveSpan(
      'triple-batcher.stream',
      {
        attributes: {
          'batcher.batch_size': this.batchSize,
          'batcher.timeout_ms': this.timeout,
        },
      },
      async (span) => {
        try {
          let count = 0;

          for await (const triple of asyncIterable) {
            // Handle backpressure
            while (this.state === 'Paused') {
              await new Promise((resolve) => setTimeout(resolve, 1));
            }

            if (this.state === 'Destroyed') {
              break;
            }

            this.addTriple(triple);
            count++;
          }

          // Flush remaining triples
          await this.flush();

          span.setAttributes({
            'batcher.triples_processed': count,
            'batcher.batches_sent': this.metrics.totalBatches,
            'batcher.throughput': this.metrics.throughput,
          });
          span.setStatus({ code: SpanStatusCode.OK });

          return this.getMetrics();
        } catch (error) {
          span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
          throw error;
        } finally {
          span.end();
        }
      }
    );
  }

  /**
   * Get current batcher metrics
   *
   * @returns {BatchMetrics} Current metrics
   */
  getMetrics() {
    const elapsedSeconds = (Date.now() - this.startTime) / 1000;
    const throughput = elapsedSeconds > 0 ? this.metrics.totalTriples / elapsedSeconds : 0;

    return {
      ...this.metrics,
      throughput: Math.round(throughput),
      avgBatchSize:
        this.metrics.totalBatches > 0
          ? Math.round(this.metrics.totalTriples / this.metrics.totalBatches)
          : 0,
      avgLatencyMs:
        this.latencies.length > 0
          ? this.latencies.reduce((a, b) => a + b, 0) / this.latencies.length
          : 0,
    };
  }

  /**
   * Get current queue size
   *
   * @returns {number} Number of triples in queue
   */
  getQueueSize() {
    return this.queue.length;
  }

  /**
   * Get current batcher state
   *
   * @returns {BatcherState} Current state
   */
  getState() {
    return this.state;
  }

  /**
   * Resume batcher after backpressure pause
   */
  resume() {
    if (this.state === 'Paused') {
      this.state = 'Idle';
      this.backpressureActive = false;

      // Continue processing if queue has items
      if (this.queue.length > 0) {
        this.state = 'Accumulating';
        if (this.queue.length >= this.batchSize) {
          this._sendBatch();
        } else {
          this._startTimeout();
        }
      }
    }
  }

  /**
   * Destroy the batcher and release resources
   */
  destroy() {
    this._clearTimeout();
    this.queue = [];
    this.state = 'Destroyed';
    this.batchCallback = null;
  }

  /**
   * Start the timeout timer for partial batch flushing
   *
   * @private
   */
  _startTimeout() {
    if (this.timeoutHandle) {
      return;
    }

    this.timeoutHandle = setTimeout(() => {
      this.timeoutHandle = null;
      if (this.queue.length > 0 && this.state !== 'Destroyed') {
        this._sendBatch();
      }
    }, this.timeout);
  }

  /**
   * Clear the timeout timer
   *
   * @private
   */
  _clearTimeout() {
    if (this.timeoutHandle) {
      clearTimeout(this.timeoutHandle);
      this.timeoutHandle = null;
    }
  }

  /**
   * Send the current batch to the callback
   *
   * @private
   * @returns {Promise<void>}
   */
  async _sendBatch() {
    if (this.queue.length === 0 || !this.batchCallback) {
      return;
    }

    return getTracer().startActiveSpan(
      'triple-batcher.send-batch',
      {
        attributes: {
          'batch.size': Math.min(this.queue.length, this.batchSize),
          'batcher.queue_remaining': Math.max(0, this.queue.length - this.batchSize),
        },
      },
      async (span) => {
        this._clearTimeout();
        this.state = 'Flushing';

        // Extract batch from queue
        const batch = this.queue.splice(0, this.batchSize);
        const startTime = performance.now();

        try {
          const result = await this.batchCallback(batch);
          const latency = performance.now() - startTime;

          // Update metrics
          this.metrics.totalTriples += batch.length;
          this.metrics.totalBatches++;
          this.latencies.push(latency);

          // Keep only last 100 latencies for average calculation
          if (this.latencies.length > 100) {
            this.latencies.shift();
          }

          // Handle slow consumer backpressure
          if (result && result.slow) {
            this.state = 'Paused';
            this.backpressureActive = true;
            this.metrics.backpressureEvents++;
          } else {
            this.state = this.queue.length > 0 ? 'Accumulating' : 'Idle';

            // Resume if was paused due to queue size
            if (this.queue.length < this.maxQueueSize && this.backpressureActive) {
              this.backpressureActive = false;
            }
          }

          span.setAttributes({
            'batch.latency_ms': latency,
            'batch.success': result?.success ?? true,
          });
          span.setStatus({ code: SpanStatusCode.OK });

          // Continue with next batch if queue has items
          if (this.queue.length >= this.batchSize && this.state === 'Accumulating') {
            setImmediate(() => this._sendBatch());
          } else if (this.queue.length > 0 && this.state === 'Accumulating') {
            this._startTimeout();
          }
        } catch (error) {
          this.state = 'Idle';
          span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
          throw error;
        } finally {
          span.end();
        }
      }
    );
  }
}

/**
 * Create a TripleStreamBatcher with the given options
 *
 * @param {BatcherOptions} [options={}] - Batcher configuration
 * @returns {TripleStreamBatcher} New batcher instance
 */
export function createTripleStreamBatcher(options = {}) {
  return new TripleStreamBatcher(options);
}

export default TripleStreamBatcher;

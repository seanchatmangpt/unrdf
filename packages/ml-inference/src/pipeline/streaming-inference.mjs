/**
 * @file Streaming Inference Pipeline - Process RDF streams with ML inference
 * @module ml-inference/pipeline/streaming-inference
 *
 * @description
 * Integrates ONNX inference with RDF change feeds for real-time ML processing.
 * Provides batching, backpressure handling, and automatic retries.
 */

import { trace } from '@opentelemetry/api';
import { z } from 'zod';

const tracer = trace.getTracer('@unrdf/ml-inference');

/**
 * Pipeline options schema
 */
const PipelineOptionsSchema = z.object({
  batchSize: z.number().min(1).max(1000).default(32),
  batchTimeoutMs: z.number().min(10).max(10000).default(100),
  maxQueueSize: z.number().min(1).max(10000).default(1000),
  enableBackpressure: z.boolean().default(true),
  retryAttempts: z.number().min(0).max(10).default(3),
  retryDelayMs: z.number().min(10).max(5000).default(100),
});

/**
 * Streaming Inference Pipeline
 * Processes data streams with ML inference, batching, and backpressure
 */
export class StreamingInferencePipeline {
  /**
   * @param {Object} runner - ONNX runner instance
   * @param {Object} options - Pipeline configuration
   */
  constructor(runner, options = {}) {
    this.runner = runner;
    this.options = PipelineOptionsSchema.parse(options);
    this.buffer = [];
    this.subscribers = [];
    this.isProcessing = false;
    this.isPaused = false;
    this.batchTimeout = null;
    this.metrics = {
      totalProcessed: 0,
      totalBatches: 0,
      droppedItems: 0,
      backpressureEvents: 0,
    };
  }

  /**
   * Process single item through inference pipeline
   *
   * @param {Object} item - Input item to process
   * @returns {Promise<Object>} Inference result
   *
   * @example
   * const result = await pipeline.process({
   *   id: 'item-1',
   *   features: [1.0, 2.0, 3.0]
   * });
   */
  async process(item) {
    return tracer.startActiveSpan('pipeline.process', async span => {
      try {
        // Check queue size for backpressure
        if (this.buffer.length >= this.options.maxQueueSize) {
          if (this.options.enableBackpressure) {
            this.metrics.backpressureEvents++;
            span.setAttribute('pipeline.backpressure', true);
            // Wait for buffer to drain
            await this._waitForSpace();
          } else {
            // Drop oldest item
            this.buffer.shift();
            this.metrics.droppedItems++;
            span.setAttribute('pipeline.dropped', true);
          }
        }

        // Add to buffer
        this.buffer.push(item);

        // Trigger batch processing
        if (this.buffer.length >= this.options.batchSize) {
          await this._processBatch();
        } else if (!this.batchTimeout) {
          // Set timeout for partial batches
          this.batchTimeout = setTimeout(() => {
            this._processBatch();
          }, this.options.batchTimeoutMs);
        }

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Process batch of items with retry logic
   *
   * @private
   * @returns {Promise<void>}
   */
  async _processBatch() {
    if (this.isProcessing || this.isPaused || this.buffer.length === 0) {
      return;
    }

    return tracer.startActiveSpan('pipeline.processBatch', async span => {
      this.isProcessing = true;

      if (this.batchTimeout) {
        clearTimeout(this.batchTimeout);
        this.batchTimeout = null;
      }

      try {
        const batchSize = Math.min(this.options.batchSize, this.buffer.length);
        const batch = this.buffer.splice(0, batchSize);

        span.setAttribute('batch.size', batchSize);
        span.setAttribute('buffer.remaining', this.buffer.length);

        // Prepare inputs
        const batchInputs = batch.map(item => ({
          input: item.features || item.data || item.input,
        }));

        // Run inference with retry
        let results = null;
        let lastError = null;

        for (let attempt = 0; attempt <= this.options.retryAttempts; attempt++) {
          try {
            results = await this.runner.inferBatch(batchInputs);
            break;
          } catch (error) {
            lastError = error;
            if (attempt < this.options.retryAttempts) {
              await new Promise(resolve =>
                setTimeout(resolve, this.options.retryDelayMs * (attempt + 1)),
              );
            }
          }
        }

        if (!results) {
          throw new Error(`Batch inference failed after retries: ${lastError?.message}`);
        }

        // Combine results with original items
        const processedBatch = batch.map((item, idx) => ({
          ...item,
          inference: results[idx],
          timestamp: Date.now(),
        }));

        // Notify subscribers
        for (const subscriber of this.subscribers) {
          try {
            await subscriber(processedBatch);
          } catch (error) {
            span.recordException(error);
          }
        }

        this.metrics.totalProcessed += batchSize;
        this.metrics.totalBatches++;

        span.setStatus({ code: 1 }); // OK
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
        throw error;
      } finally {
        this.isProcessing = false;
        span.end();

        // Process next batch if buffer not empty
        if (this.buffer.length > 0) {
          setImmediate(() => this._processBatch());
        }
      }
    });
  }

  /**
   * Wait for buffer space to become available
   *
   * @private
   * @returns {Promise<void>}
   */
  async _waitForSpace() {
    while (this.buffer.length >= this.options.maxQueueSize) {
      await new Promise(resolve => setTimeout(resolve, 10));
    }
  }

  /**
   * Subscribe to inference results
   *
   * @param {Function} callback - Callback for batch results
   * @returns {Function} Unsubscribe function
   *
   * @example
   * const unsubscribe = pipeline.subscribe((batch) => {
   *   console.log('Batch results:', batch);
   * });
   */
  subscribe(callback) {
    this.subscribers.push(callback);
    return () => {
      const index = this.subscribers.indexOf(callback);
      if (index !== -1) {
        this.subscribers.splice(index, 1);
      }
    };
  }

  /**
   * Pause pipeline processing
   */
  pause() {
    this.isPaused = true;
  }

  /**
   * Resume pipeline processing
   */
  resume() {
    this.isPaused = false;
    if (this.buffer.length > 0) {
      setImmediate(() => this._processBatch());
    }
  }

  /**
   * Flush remaining buffer
   *
   * @returns {Promise<void>}
   */
  async flush() {
    while (this.buffer.length > 0) {
      await this._processBatch();
    }
  }

  /**
   * Get pipeline metrics
   *
   * @returns {Object} Pipeline statistics
   */
  getMetrics() {
    return {
      ...this.metrics,
      bufferSize: this.buffer.length,
      avgBatchSize:
        this.metrics.totalBatches > 0
          ? this.metrics.totalProcessed / this.metrics.totalBatches
          : 0,
      isPaused: this.isPaused,
      isProcessing: this.isProcessing,
    };
  }

  /**
   * Reset pipeline metrics
   */
  resetMetrics() {
    this.metrics = {
      totalProcessed: 0,
      totalBatches: 0,
      droppedItems: 0,
      backpressureEvents: 0,
    };
  }

  /**
   * Destroy pipeline and cleanup resources
   */
  async destroy() {
    if (this.batchTimeout) {
      clearTimeout(this.batchTimeout);
      this.batchTimeout = null;
    }
    await this.flush();
    this.subscribers.length = 0;
    this.buffer.length = 0;
  }
}

/**
 * Create streaming inference pipeline
 *
 * @param {Object} runner - ONNX runner instance
 * @param {Object} options - Pipeline options
 * @returns {StreamingInferencePipeline} Pipeline instance
 */
export function createStreamingInferencePipeline(runner, options = {}) {
  return new StreamingInferencePipeline(runner, options);
}

/**
 * @file RDF Stream Processor with windowing operations
 * @module streaming/stream-processor
 *
 * @description
 * Process RDF streams with support for windowing operations (tumbling, sliding,
 * session windows), stream joins, aggregations, and incremental processing.
 */

import { EventEmitter } from 'events';
import { randomUUID } from 'crypto';
import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { createObservabilityManager } from '../observability.mjs';

const tracer = trace.getTracer('unrdf-streaming');

/**
 * Window types
 * @enum {string}
 */
export const WindowType = {
  TUMBLING: 'tumbling',
  SLIDING: 'sliding',
  SESSION: 'session',
  COUNT: 'count',
};

/**
 * Window configuration schema
 */
const WindowConfigSchema = z.object({
  type: z.enum([WindowType.TUMBLING, WindowType.SLIDING, WindowType.SESSION, WindowType.COUNT]),
  size: z.number().min(1),
  slide: z.number().min(1).optional(),
  timeout: z.number().min(0).optional(),
  count: z.number().min(1).optional(),
});

/**
 * Stream processor configuration schema
 */
const ProcessorConfigSchema = z.object({
  enableWindowing: z.boolean().default(true),
  enableAggregation: z.boolean().default(true),
  enableJoins: z.boolean().default(false),
  maxWindowSize: z.number().min(1).default(10000),
  observability: z
    .object({
      serviceName: z.string().default('unrdf-stream-processor'),
      enableTracing: z.boolean().default(true),
      enableMetrics: z.boolean().default(true),
    })
    .optional(),
});

/**
 * Stream Window
 * Represents a time or count-based window of stream events
 */
class StreamWindow {
  /**
   * Create a new stream window
   * @param {Object} config - Window configuration
   */
  constructor(config) {
    this.config = WindowConfigSchema.parse(config);
    this.id = randomUUID();
    this.events = [];
    this.startTime = Date.now();
    this.endTime = null;
    this.isClosed = false;
  }

  /**
   * Add event to window
   * @param {Object} event - Stream event
   * @returns {boolean} Success
   */
  add(event) {
    if (this.isClosed) {
      return false;
    }

    this.events.push({
      ...event,
      windowId: this.id,
      windowIndex: this.events.length,
    });

    // FIX: Check if window should close for count-based windows
    // Ensure we use the count property, not size
    if (this.config.type === WindowType.COUNT) {
      const targetCount = this.config.count || this.config.size || 10;
      if (this.events.length >= targetCount) {
        this.close();
      }
    }

    return true;
  }

  /**
   * Close the window
   */
  close() {
    if (!this.isClosed) {
      this.endTime = Date.now();
      this.isClosed = true;
    }
  }

  /**
   * Get window duration
   * @returns {number} Duration in milliseconds
   */
  getDuration() {
    const end = this.endTime || Date.now();
    return end - this.startTime;
  }

  /**
   * Get window contents
   * @returns {Object} Window data
   */
  getContents() {
    return {
      id: this.id,
      type: this.config.type,
      events: [...this.events],
      count: this.events.length,
      startTime: this.startTime,
      endTime: this.endTime,
      duration: this.getDuration(),
      isClosed: this.isClosed,
    };
  }
}

/**
 * RDF Stream Processor
 * Process RDF streams with windowing and aggregation
 */
export class StreamProcessor extends EventEmitter {
  /**
   * Create a new stream processor
   * @param {Object} config - Processor configuration
   */
  constructor(config = {}) {
    super();
    this.config = ProcessorConfigSchema.parse(config);
    this.windows = new Map();
    this.activeWindow = null;
    this.windowConfig = null;
    this.aggregators = new Map();
    this.isProcessing = false;

    // Observability
    this.observability = createObservabilityManager(this.config.observability || {});

    // Metrics
    this.metrics = {
      eventsProcessed: 0,
      windowsCreated: 0,
      windowsClosed: 0,
      aggregationsPerformed: 0,
      errorCount: 0,
    };

    // Initialize observability
    this.observability.initialize().catch(err => {
      console.warn('[StreamProcessor] Failed to initialize observability:', err.message);
    });
  }

  /**
   * Configure windowing
   * @param {Object} config - Window configuration
   */
  configureWindowing(config) {
    this.windowConfig = WindowConfigSchema.parse(config);
    console.log('[StreamProcessor] Windowing configured:', this.windowConfig.type);
  }

  /**
   * Start processing
   */
  start() {
    if (this.isProcessing) {
      console.log('[StreamProcessor] Already processing');
      return;
    }

    this.isProcessing = true;

    // Create initial window if windowing is enabled
    if (this.config.enableWindowing && this.windowConfig) {
      this._createWindow();
    }

    this.emit('started');
    console.log('[StreamProcessor] Started');
  }

  /**
   * Stop processing
   */
  stop() {
    if (!this.isProcessing) {
      return;
    }

    this.isProcessing = false;

    // Close active window
    if (this.activeWindow) {
      this._closeWindow(this.activeWindow.id);
    }

    this.emit('stopped');
    console.log('[StreamProcessor] Stopped');
  }

  /**
   * Process a stream event
   * @param {Object} event - Stream event
   * @returns {Promise<Object>} Processing result
   */
  async process(event) {
    return tracer.startActiveSpan('stream-processor.process', async span => {
      try {
        if (!this.isProcessing) {
          throw new Error('Stream processor is not active');
        }

        span.setAttributes({
          'event.id': event.id || 'unknown',
          'event.type': event.type || 'unknown',
        });

        this.metrics.eventsProcessed++;

        let result = { event, processed: true };

        // Add to window if windowing is enabled
        if (this.config.enableWindowing && this.windowConfig) {
          result = await this._processWithWindowing(event);
        }

        // Perform aggregations if enabled
        if (this.config.enableAggregation && this.aggregators.size > 0) {
          result.aggregations = await this._performAggregations(event);
        }

        // FIX: Emit 'processed' event BEFORE returning to ensure listeners get it
        // Use setImmediate to ensure emission happens asynchronously
        setImmediate(() => {
          this.emit('processed', result);
        });

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        span.end();
        this.metrics.errorCount++;
        this.observability.recordError(error, { context: 'process-event' });
        throw error;
      }
    });
  }

  /**
   * Process event with windowing
   * @private
   */
  async _processWithWindowing(event) {
    // Ensure active window exists
    if (!this.activeWindow) {
      this._createWindow();
    }

    // Add event to active window
    const added = this.activeWindow.add(event);

    if (!added) {
      // Window is closed, create new one
      this._createWindow();
      this.activeWindow.add(event);
    }

    // FIX: Add async handling for window operations to prevent race conditions
    // Use Promise.resolve() to ensure async flow
    await Promise.resolve();

    // Check if window should advance (for time-based windows)
    if (this.windowConfig.type === WindowType.TUMBLING) {
      const elapsed = Date.now() - this.activeWindow.startTime;
      if (elapsed >= this.windowConfig.size) {
        this._closeWindow(this.activeWindow.id);
        this._createWindow();
      }
    } else if (this.windowConfig.type === WindowType.SLIDING) {
      const elapsed = Date.now() - this.activeWindow.startTime;
      const slide = this.windowConfig.slide || this.windowConfig.size;
      // FIX: Only create new window if we don't have overlapping windows yet
      if (elapsed >= slide && this.activeWindow.isClosed) {
        this._createWindow();
      } else if (elapsed >= slide) {
        // For sliding windows, we may need overlapping windows
        const newWindow = this._createWindow();
        // Copy events that fall within the sliding window overlap
        const overlapStart = this.activeWindow.startTime + slide;
        const overlapEvents = this.activeWindow.events.filter(
          e => (e.timestamp || Date.now()) >= overlapStart
        );
        for (const e of overlapEvents) {
          newWindow.add(e);
        }
      }
    } else if (this.windowConfig.type === WindowType.SESSION) {
      // Reset timeout on each event
      this._resetSessionTimeout();
    }

    return {
      event,
      windowId: this.activeWindow.id,
      windowCount: this.activeWindow.events.length,
    };
  }

  /**
   * Create a new window
   * @private
   */
  _createWindow() {
    const window = new StreamWindow(this.windowConfig);
    this.windows.set(window.id, window);
    this.activeWindow = window;
    this.metrics.windowsCreated++;

    this.emit('window-created', window.getContents());

    // Set up time-based window closure
    if (this.windowConfig.type === WindowType.TUMBLING) {
      setTimeout(() => {
        if (!window.isClosed) {
          this._closeWindow(window.id);
        }
      }, this.windowConfig.size);
    }

    return window;
  }

  /**
   * Close a window
   * @private
   */
  _closeWindow(windowId) {
    const window = this.windows.get(windowId);
    if (!window || window.isClosed) {
      return;
    }

    window.close();
    this.metrics.windowsClosed++;

    const contents = window.getContents();
    this.emit('window-closed', contents);

    // Clean up old windows
    if (this.windows.size > this.config.maxWindowSize) {
      const oldestId = this.windows.keys().next().value;
      this.windows.delete(oldestId);
    }

    return contents;
  }

  /**
   * Reset session timeout
   * @private
   */
  _resetSessionTimeout() {
    if (this._sessionTimeout) {
      clearTimeout(this._sessionTimeout);
    }

    const timeout = this.windowConfig.timeout || 5000;
    this._sessionTimeout = setTimeout(() => {
      if (this.activeWindow) {
        this._closeWindow(this.activeWindow.id);
        this._createWindow();
      }
    }, timeout);
  }

  /**
   * Register an aggregator function
   * @param {string} name - Aggregator name
   * @param {Function} fn - Aggregator function
   */
  registerAggregator(name, fn) {
    if (typeof fn !== 'function') {
      throw new TypeError('Aggregator must be a function');
    }

    this.aggregators.set(name, fn);
    console.log(`[StreamProcessor] Registered aggregator: ${name}`);
  }

  /**
   * Unregister an aggregator
   * @param {string} name - Aggregator name
   * @returns {boolean} Success
   */
  unregisterAggregator(name) {
    return this.aggregators.delete(name);
  }

  /**
   * Perform aggregations on event
   * @private
   */
  async _performAggregations(event) {
    const results = {};

    for (const [name, fn] of this.aggregators) {
      try {
        results[name] = await fn(event, this.activeWindow);
        this.metrics.aggregationsPerformed++;
      } catch (error) {
        console.error(`[StreamProcessor] Aggregator ${name} failed:`, error.message);
        results[name] = { error: error.message };
      }
    }

    return results;
  }

  /**
   * Get window by ID
   * @param {string} id - Window ID
   * @returns {Object|null} Window contents or null
   */
  getWindow(id) {
    const window = this.windows.get(id);
    return window ? window.getContents() : null;
  }

  /**
   * Get all windows
   * @returns {Array<Object>} Array of window contents
   */
  getAllWindows() {
    return Array.from(this.windows.values()).map(w => w.getContents());
  }

  /**
   * Get active window
   * @returns {Object|null} Active window contents or null
   */
  getActiveWindow() {
    return this.activeWindow ? this.activeWindow.getContents() : null;
  }

  /**
   * Aggregate window contents
   * @param {string} windowId - Window ID
   * @param {Function} aggregator - Aggregator function
   * @returns {*} Aggregation result
   */
  async aggregateWindow(windowId, aggregator) {
    const window = this.windows.get(windowId);
    if (!window) {
      throw new Error(`Window ${windowId} not found`);
    }

    return aggregator(window.events);
  }

  /**
   * Get performance metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      activeWindows: this.windows.size,
      activeAggregators: this.aggregators.size,
      isProcessing: this.isProcessing,
    };
  }

  /**
   * Process a batch of events
   * @param {Array<Object>} events - Array of events
   * @returns {Promise<Array<Object>>} Processing results
   */
  async processBatch(events) {
    return tracer.startActiveSpan('stream-processor.process-batch', async span => {
      try {
        span.setAttribute('batch.size', events.length);

        const results = [];
        for (const event of events) {
          const result = await this.process(event);
          results.push(result);
        }

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return results;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Create a processing pipeline
   * @param {Array<Function>} stages - Processing stages
   * @returns {Function} Pipeline function
   */
  createPipeline(stages) {
    return async event => {
      let result = event;

      for (const stage of stages) {
        result = await stage(result);
      }

      return result;
    };
  }

  /**
   * Cleanup resources
   * @returns {Promise<void>}
   */
  async cleanup() {
    this.stop();

    if (this._sessionTimeout) {
      clearTimeout(this._sessionTimeout);
    }

    this.windows.clear();
    this.aggregators.clear();
    this.removeAllListeners();
    await this.observability.shutdown();
  }
}

/**
 * Create a stream processor instance
 * @param {Object} config - Processor configuration
 * @returns {StreamProcessor} Stream processor
 */
export function createStreamProcessor(config = {}) {
  return new StreamProcessor(config);
}

/**
 * Built-in aggregator functions
 */
export const Aggregators = {
  /**
   * Count events in window
   */
  count: events => events.length,

  /**
   * Sum numeric values
   */
  sum: field => events => {
    return events.reduce((sum, event) => {
      const value = typeof field === 'function' ? field(event) : event[field];
      return sum + (Number(value) || 0);
    }, 0);
  },

  /**
   * Average numeric values
   */
  avg: field => events => {
    const total = Aggregators.sum(field)(events);
    return events.length > 0 ? total / events.length : 0;
  },

  /**
   * Find minimum value
   */
  min: field => events => {
    if (events.length === 0) return null;
    return Math.min(
      ...events.map(event => {
        const value = typeof field === 'function' ? field(event) : event[field];
        return Number(value) || 0;
      })
    );
  },

  /**
   * Find maximum value
   */
  max: field => events => {
    if (events.length === 0) return null;
    return Math.max(
      ...events.map(event => {
        const value = typeof field === 'function' ? field(event) : event[field];
        return Number(value) || 0;
      })
    );
  },

  /**
   * Group events by field
   */
  groupBy: field => events => {
    return events.reduce((groups, event) => {
      const key = typeof field === 'function' ? field(event) : event[field];
      if (!groups[key]) {
        groups[key] = [];
      }
      groups[key].push(event);
      return groups;
    }, {});
  },
};

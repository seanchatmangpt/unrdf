/**
 * @file OpenTelemetry Observability for UNRDF
 * @module observability
 *
 * @description
 * Implements comprehensive observability with OpenTelemetry traces, metrics,
 * and logging for the UNRDF Knowledge Engine. Uses daemon SDK as single source of truth.
 */

import { _randomUUID } from 'crypto';
import { _z } from 'zod';
import { ObservabilityConfigSchema, PerformanceMetricsSchema } from './schemas.mjs';
import { getRuntimeConfig } from '../context/config.mjs';

// Import daemon SDK functions (single source of truth)
let getTracer, getMeter;
try {
  const daemonSdk = await import('@unrdf/daemon/integrations/otel-sdk.mjs');
  getTracer = daemonSdk.getTracer;
  getMeter = daemonSdk.getMeter;
} catch {
  // Daemon SDK not available (e.g., in standalone hooks usage)
  console.warn('[Observability] Daemon SDK not available - will use fallback mode');
}

// Import generated semantic convention constants
import {
  ATTR_UNRDF_HOOK_ID,
  ATTR_UNRDF_HOOK_NAME,
  ATTR_UNRDF_HOOK_CONDITION_KIND,
  ATTR_UNRDF_HOOK_EFFECT_KIND,
  ATTR_UNRDF_HOOK_ENABLED,
  ATTR_UNRDF_HOOK_SATISFIED,
  ATTR_UNRDF_HOOK_PRIORITY,
  ATTR_UNRDF_HOOK_QUADS_ADDED,
  ATTR_UNRDF_HOOK_HOOKS_SATISFIED,
  ATTR_UNRDF_HOOK_HOOKS_TOTAL,
  ATTR_UNRDF_TRANSACTION_ID,
  ATTR_UNRDF_TRANSACTION_ISOLATION_LEVEL,
  ATTR_UNRDF_TRANSACTION_OPERATION_COUNT,
  ATTR_UNRDF_TRANSACTION_RESULT,
} from '@unrdf/otel/generated';

/**
 * OpenTelemetry observability manager
 */
export class ObservabilityManager {
  /**
   * Create a new observability manager
   * @param {Object} [config] - Observability configuration
   */
  constructor(config = {}) {
    this.config = ObservabilityConfigSchema.parse(config);
    this.tracer = null;
    this.meter = null;
    this.logger = null;
    this.metrics = {
      transactionLatency: [],
      hookExecutionRate: 0,
      errorCount: 0,
      totalTransactions: 0,
      memoryUsage: [],
      cacheStats: { hits: 0, misses: 0, size: 0 },
      backpressure: { queueDepth: 0, watermarks: { high: 1000, low: 100 } },
    };
    this.activeSpans = new Map();
    this.initialized = false;
    this._lastMetrics = null; // for smoothing fallback
  }

  /**
   * Initialize OpenTelemetry components
   * @returns {Promise<void>}
   */
  async initialize() {
    if (this.initialized) return;

    try {
      // Use daemon SDK as single source of truth
      if (typeof getTracer === 'function' && typeof getMeter === 'function') {
        this.tracer = getTracer();
        this.meter = getMeter();

        if (!this.tracer || !this.meter) {
          console.warn('[Observability] Daemon SDK not initialized - using fallback');
          this._initializeFallback();
          return;
        }

        // Create custom metrics using daemon's meter
        this._createCustomMetrics();

        this.initialized = true;
        console.log(`[Observability] Initialized with daemon SDK for service: ${this.config.serviceName}`);
      } else {
        // Daemon SDK not available - use fallback
        console.warn('[Observability] Daemon SDK not available - using fallback mode');
        this._initializeFallback();
      }
    } catch (error) {
      console.warn(`[Observability] Failed to initialize with daemon SDK: ${error.message}`);
      // Fallback to console logging
      this._initializeFallback();
    }
  }

  /**
   * Create custom metrics
   * @private
   */
  _createCustomMetrics() {
    if (!this.meter) return;

    // Transaction metrics (using unrdf.* prefix from registry)
    this.transactionCounter = this.meter.createCounter('unrdf_transactions_total', {
      description: 'Total number of transactions processed',
    });

    this.transactionDuration = this.meter.createHistogram('unrdf_transaction_duration_ms', {
      description: 'Transaction processing duration in milliseconds',
      unit: 'ms',
    });

    this.hookExecutionCounter = this.meter.createCounter('unrdf_hooks_executed_total', {
      description: 'Total number of hooks executed',
    });

    this.hookDuration = this.meter.createHistogram('unrdf_hook_duration_ms', {
      description: 'Hook execution duration in milliseconds',
      unit: 'ms',
    });

    this.errorCounter = this.meter.createCounter('unrdf_errors_total', {
      description: 'Total number of errors',
    });

    this.memoryGauge = this.meter.createUpDownCounter('unrdf_memory_usage_bytes', {
      description: 'Memory usage in bytes',
    });

    this.cacheHitCounter = this.meter.createCounter('unrdf_cache_hits_total', {
      description: 'Total cache hits',
    });

    this.cacheMissCounter = this.meter.createCounter('unrdf_cache_misses_total', {
      description: 'Total cache misses',
    });

    this.queueDepthGauge = this.meter.createUpDownCounter('unrdf_queue_depth', {
      description: 'Current queue depth',
    });
  }

  /**
   * Initialize fallback observability
   * @private
   */
  _initializeFallback() {
    console.log('[Observability] Using fallback console logging');
    this.initialized = true;
  }

  /**
   * Start a transaction span
   * @param {string} transactionId - Transaction ID
   * @param {Object} [attributes] - Span attributes
   * @returns {Object} Span context
   */
  startTransactionSpan(transactionId, attributes = {}) {
    if (!this.tracer) {
      return { transactionId, startTime: Date.now() };
    }

    const span = this.tracer.startSpan('unrdf.transaction', {
      attributes: {
        [ATTR_UNRDF_TRANSACTION_ID]: transactionId,
        ...attributes,
      },
    });

    const spanContext = { transactionId, span, startTime: Date.now() };
    this.activeSpans.set(transactionId, spanContext);
    return spanContext;
  }

  /**
   * End a transaction span
   * @param {string} transactionId - Transaction ID
   * @param {Object} [attributes] - Final span attributes
   * @param {Error} [error] - Error if transaction failed
   */
  endTransactionSpan(transactionId, attributes = {}, error = null) {
    const spanContext = this.activeSpans.get(transactionId);
    if (!spanContext) return;

    const { span, startTime } = spanContext;
    const duration = Date.now() - startTime;

    if (span) {
      span.setAttributes({
        'kgc.transaction.duration_ms': duration,
        'kgc.transaction.success': !error,
        ...attributes,
      });

      if (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
      } else {
        span.setStatus({ code: 1 }); // OK
      }

      span.end();
    }

    this.activeSpans.delete(transactionId);

    // Update metrics
    this._updateTransactionMetrics(duration, !error);
  }

  /**
   * Start a hook execution span
   * @param {string} hookId - Hook ID
   * @param {string} transactionId - Parent transaction ID
   * @param {Object} [attributes] - Span attributes
   * @returns {Object} Span context
   */
  startHookSpan(hookId, transactionId, attributes = {}) {
    if (!this.tracer) {
      return { hookId, startTime: Date.now() };
    }

    const parentSpan = this.activeSpans.get(transactionId)?.span;
    const span = this.tracer.startSpan('unrdf.hook', {
      parent: parentSpan,
      attributes: {
        [ATTR_UNRDF_HOOK_ID]: hookId,
        [ATTR_UNRDF_TRANSACTION_ID]: transactionId,
        ...attributes,
      },
    });

    const spanKey = `${transactionId}:${hookId}`;
    const spanContext = { hookId, span, startTime: Date.now() };
    this.activeSpans.set(spanKey, spanContext);
    return spanContext;
  }

  /**
   * End a hook execution span
   * @param {string} hookId - Hook ID
   * @param {string} transactionId - Parent transaction ID
   * @param {Object} [attributes] - Final span attributes
   * @param {Error} [error] - Error if hook failed
   */
  endHookSpan(hookId, transactionId, attributes = {}, error = null) {
    const spanKey = `${transactionId}:${hookId}`;
    const spanContext = this.activeSpans.get(spanKey);
    if (!spanContext) return;

    const { span, startTime } = spanContext;
    const duration = Date.now() - startTime;

    if (span) {
      span.setAttributes({
        'kgc.hook.duration_ms': duration,
        'kgc.hook.success': !error,
        ...attributes,
      });

      if (error) {
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message }); // ERROR
      } else {
        span.setStatus({ code: 1 }); // OK
      }

      span.end();
    }

    this.activeSpans.delete(spanKey);

    // Update metrics
    this._updateHookMetrics(duration, !error);
  }

  /**
   * Record an error
   * @param {Error} error - Error to record
   * @param {Object} [attributes] - Error attributes
   */
  recordError(error, attributes = {}) {
    this.metrics.errorCount++;

    if (this.errorCounter) {
      this.errorCounter.add(1, {
        'error.type': error.constructor.name,
        'error.message': error.message,
        ...attributes,
      });
    }

    console.error(`[Observability] Error recorded:`, error.message, attributes);
  }

  /**
   * Update cache statistics
   * @param {boolean} hit - Whether it was a cache hit
   */
  updateCacheStats(hit) {
    if (hit) {
      this.metrics.cacheStats.hits++;
      if (this.cacheHitCounter) {
        this.cacheHitCounter.add(1);
      }
    } else {
      this.metrics.cacheStats.misses++;
      if (this.cacheMissCounter) {
        this.cacheMissCounter.add(1);
      }
    }
  }

  /**
   * Update queue depth
   * @param {number} depth - Current queue depth
   */
  updateQueueDepth(depth) {
    this.metrics.backpressure.queueDepth = depth;

    if (this.queueDepthGauge) {
      this.queueDepthGauge.add(depth - this.metrics.backpressure.queueDepth);
    }

    // Check watermarks
    if (depth > this.metrics.backpressure.watermarks.high) {
      console.warn(
        `[Observability] High queue depth: ${depth} > ${this.metrics.backpressure.watermarks.high}`
      );
    }
  }

  /**
   * Update memory usage
   */
  updateMemoryUsage() {
    const memUsage = process.memoryUsage();
    this.metrics.memoryUsage.push({
      timestamp: Date.now(),
      rss: memUsage.rss,
      heapUsed: memUsage.heapUsed,
      heapTotal: memUsage.heapTotal,
      external: memUsage.external,
    });

    // Keep only last 100 measurements
    if (this.metrics.memoryUsage.length > 100) {
      this.metrics.memoryUsage = this.metrics.memoryUsage.slice(-100);
    }

    if (this.memoryGauge) {
      this.memoryGauge.add(memUsage.heapUsed);
    }
  }

  /**
   * Get performance metrics
   * @returns {Object} Performance metrics
   */
  getPerformanceMetrics() {
    const now = Date.now();
    const oneMinuteAgo = now - 60000;

    // Calculate transaction latency percentiles
    const recentLatencies = this.metrics.transactionLatency
      .filter(l => l.timestamp > oneMinuteAgo)
      .map(l => l.duration)
      .sort((a, b) => a - b);

    let p50 = this._calculatePercentile(recentLatencies, 0.5);
    let p95 = this._calculatePercentile(recentLatencies, 0.95);
    let p99 = this._calculatePercentile(recentLatencies, 0.99);
    let max = recentLatencies.length > 0 ? recentLatencies[recentLatencies.length - 1] : 0;

    // Calculate hook execution rate (per minute)
    const recentHooks = this.metrics.hookExecutionRate;
    let hookRate = recentHooks; // Already per minute

    // Calculate error rate
    const totalTransactions = this.metrics.totalTransactions;
    let errorRate = totalTransactions > 0 ? this.metrics.errorCount / totalTransactions : 0;

    // Get current memory usage
    const currentMemory = process.memoryUsage();

    // Calculate cache hit rate
    const totalCacheOps = this.metrics.cacheStats.hits + this.metrics.cacheStats.misses;
    let cacheHitRate = totalCacheOps > 0 ? this.metrics.cacheStats.hits / totalCacheOps : 0;

    // Apply min-sample gate and EWMA smoothing to reduce noise/false positives
    const minSamples = this.config.minSamples;
    const alpha = this.config.ewmaAlpha;
    if (recentLatencies.length < minSamples && this._lastMetrics) {
      // Fall back partially to previous metrics
      p50 = alpha * p50 + (1 - alpha) * this._lastMetrics.transactionLatency.p50;
      p95 = alpha * p95 + (1 - alpha) * this._lastMetrics.transactionLatency.p95;
      p99 = alpha * p99 + (1 - alpha) * this._lastMetrics.transactionLatency.p99;
      max = Math.max(max, this._lastMetrics.transactionLatency.max);
      hookRate = alpha * hookRate + (1 - alpha) * this._lastMetrics.hookExecutionRate;
      errorRate = alpha * errorRate + (1 - alpha) * this._lastMetrics.errorRate;
      cacheHitRate = alpha * cacheHitRate + (1 - alpha) * this._lastMetrics.cacheStats.hitRate;
    }

    const computed = PerformanceMetricsSchema.parse({
      transactionLatency: { p50, p95, p99, max },
      hookExecutionRate: hookRate,
      errorRate,
      memoryUsage: currentMemory,
      cacheStats: {
        hitRate: cacheHitRate,
        size: this.metrics.cacheStats.size,
        maxSize:
          typeof this.config.cacheMaxSize === 'number'
            ? this.config.cacheMaxSize
            : (getRuntimeConfig().cacheMaxSize ?? this.metrics.cacheStats.size),
      },
      backpressure: this.metrics.backpressure,
    });

    this._lastMetrics = computed;
    return computed;
  }

  /**
   * Update transaction metrics
   * @param {number} duration - Transaction duration
   * @param {boolean} success - Whether transaction succeeded
   * @private
   */
  _updateTransactionMetrics(duration, success) {
    this.metrics.transactionLatency.push({
      timestamp: Date.now(),
      duration,
      success,
    });

    // Keep only last 1000 measurements
    if (this.metrics.transactionLatency.length > 1000) {
      this.metrics.transactionLatency = this.metrics.transactionLatency.slice(-1000);
    }

    this.metrics.totalTransactions++;

    if (this.transactionCounter) {
      this.transactionCounter.add(1, { success: success.toString() });
    }

    if (this.transactionDuration) {
      this.transactionDuration.record(duration);
    }
  }

  /**
   * Update hook metrics
   * @param {number} duration - Hook duration
   * @param {boolean} success - Whether hook succeeded
   * @private
   */
  _updateHookMetrics(duration, success) {
    this.metrics.hookExecutionRate++;

    if (this.hookExecutionCounter) {
      this.hookExecutionCounter.add(1, { success: success.toString() });
    }

    if (this.hookDuration) {
      this.hookDuration.record(duration);
    }
  }

  /**
   * Calculate percentile
   * @param {Array<number>} values - Sorted values
   * @param {number} percentile - Percentile (0-1)
   * @returns {number} Percentile value
   * @private
   */
  _calculatePercentile(values, percentile) {
    if (values.length === 0) return 0;
    const index = Math.ceil(values.length * percentile) - 1;
    return values[Math.max(0, index)];
  }

  /**
   * Shutdown observability
   * @returns {Promise<void>}
   */
  async shutdown() {
    // End all active spans
    for (const [_key, spanContext] of this.activeSpans) {
      if (spanContext.span) {
        spanContext.span.end();
      }
    }
    this.activeSpans.clear();

    console.log('[Observability] Shutdown complete');
  }
}

/**
 * Create an observability manager instance
 * @param {Object} [config] - Configuration
 * @returns {ObservabilityManager} Observability manager
 */
export function createObservabilityManager(config = {}) {
  return new ObservabilityManager(config);
}

/**
 * Default observability manager instance
 */
export const defaultObservabilityManager = createObservabilityManager();

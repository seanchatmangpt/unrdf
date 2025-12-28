/**
 * @file OpenTelemetry Observability for UNRDF
 * @module observability
 *
 * @description
 * Implements comprehensive observability with OpenTelemetry traces, metrics,
 * and logging for the UNRDF Knowledge Engine. Provides backpressure monitoring,
 * error isolation, and performance tracking.
 */

import { randomUUID } from 'crypto';
import { z } from 'zod';
import { ObservabilityConfigSchema, PerformanceMetricsSchema } from './schemas.mjs';

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
    this.samplingRate = config.samplingRate ?? 0.1; // Default 10% sampling
    this.logSamplingRate = config.logSamplingRate ?? 0.01; // Default 1% DEBUG log sampling
    this._debugLogCounter = 0;
  }

  /**
   * Initialize OpenTelemetry components
   * @returns {Promise<void>}
   */
  async initialize() {
    if (this.initialized) return;

    try {
      // Dynamic import of OpenTelemetry packages
      const { NodeSDK } = await import('@opentelemetry/sdk-node');
      const { getNodeAutoInstrumentations } =
        await import('@opentelemetry/auto-instrumentations-node');
      const { Resource } = await import('@opentelemetry/resources');
      const { SemanticResourceAttributes } = await import('@opentelemetry/semantic-conventions');
      const { OTLPTraceExporter } = await import('@opentelemetry/exporter-otlp-http');
      const { OTLPMetricExporter } = await import('@opentelemetry/exporter-otlp-http');
      const { PeriodicExportingMetricReader } = await import('@opentelemetry/sdk-metrics');
      const { trace, metrics } = await import('@opentelemetry/api');

      // Create resource
      const resource = new Resource({
        [SemanticResourceAttributes.SERVICE_NAME]: this.config.serviceName,
        [SemanticResourceAttributes.SERVICE_VERSION]: this.config.serviceVersion,
        ...this.config.resourceAttributes,
      });

      // Create exporters
      const traceExporter = new OTLPTraceExporter({
        url: this.config.endpoint ? `${this.config.endpoint}/v1/traces` : undefined,
        headers: this.config.headers,
      });

      const metricExporter = new OTLPMetricExporter({
        url: this.config.endpoint ? `${this.config.endpoint}/v1/metrics` : undefined,
        headers: this.config.headers,
      });

      // Create metric reader
      const metricReader = new PeriodicExportingMetricReader({
        exporter: metricExporter,
        exportIntervalMillis: this.config.scheduledDelayMillis,
        exportTimeoutMillis: this.config.exportTimeoutMillis,
      });

      // Create sampler for head-based sampling
      const { TraceIdRatioBasedSampler } = await import('@opentelemetry/sdk-trace-base');
      const sampler = new TraceIdRatioBasedSampler(this.samplingRate);

      // Initialize SDK with sampling
      const sdk = new NodeSDK({
        resource,
        traceExporter: this.config.enableTracing ? traceExporter : undefined,
        metricReader: this.config.enableMetrics ? metricReader : undefined,
        instrumentations: this.config.enableTracing ? [getNodeAutoInstrumentations()] : [],
        sampler: this.config.enableTracing ? sampler : undefined,
      });

      await sdk.start();

      // Get tracer and meter
      this.tracer = trace.getTracer(this.config.serviceName, this.config.serviceVersion);
      this.meter = metrics.getMeter(this.config.serviceName, this.config.serviceVersion);

      // Create custom metrics
      this._createCustomMetrics();

      this.initialized = true;
      console.log(`[Observability] Initialized with service: ${this.config.serviceName}`);
    } catch (error) {
      console.warn(`[Observability] Failed to initialize OpenTelemetry: ${error.message}`);
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

    // Transaction metrics
    this.transactionCounter = this.meter.createCounter('kgc_transactions_total', {
      description: 'Total number of transactions processed',
    });

    this.transactionDuration = this.meter.createHistogram('kgc_transaction_duration_ms', {
      description: 'Transaction processing duration in milliseconds',
      unit: 'ms',
    });

    this.hookExecutionCounter = this.meter.createCounter('kgc_hooks_executed_total', {
      description: 'Total number of hooks executed',
    });

    this.hookDuration = this.meter.createHistogram('kgc_hook_duration_ms', {
      description: 'Hook execution duration in milliseconds',
      unit: 'ms',
    });

    this.errorCounter = this.meter.createCounter('kgc_errors_total', {
      description: 'Total number of errors',
    });

    this.memoryGauge = this.meter.createUpDownCounter('kgc_memory_usage_bytes', {
      description: 'Memory usage in bytes',
    });

    this.cacheHitCounter = this.meter.createCounter('kgc_cache_hits_total', {
      description: 'Total cache hits',
    });

    this.cacheMissCounter = this.meter.createCounter('kgc_cache_misses_total', {
      description: 'Total cache misses',
    });

    this.queueDepthGauge = this.meter.createUpDownCounter('kgc_queue_depth', {
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

    const span = this.tracer.startSpan('kgc.transaction', {
      attributes: {
        'kgc.transaction.id': transactionId,
        'kgc.service.name': this.config.serviceName,
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
    const span = this.tracer.startSpan('kgc.hook', {
      parent: parentSpan,
      attributes: {
        'kgc.hook.id': hookId,
        'kgc.transaction.id': transactionId,
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
   * Log with sampling (DEBUG logs sampled, WARN/ERROR always logged)
   * @param {string} level - Log level (DEBUG, INFO, WARN, ERROR)
   * @param {string} message - Log message
   * @param {Object} [context] - Additional context
   */
  log(level, message, context = {}) {
    const upperLevel = level.toUpperCase();

    // Always log WARN and ERROR
    if (upperLevel === 'WARN' || upperLevel === 'ERROR') {
      console[upperLevel === 'ERROR' ? 'error' : 'warn'](`[${upperLevel}] ${message}`, context);
      return;
    }

    // Sample DEBUG logs
    if (upperLevel === 'DEBUG') {
      this._debugLogCounter++;
      const sampleThreshold = Math.floor(1 / this.logSamplingRate);
      if (this._debugLogCounter % sampleThreshold === 0) {
        console.log(`[DEBUG:SAMPLED] ${message}`, context);
      }
      return;
    }

    // INFO and other levels always logged
    console.log(`[${upperLevel}] ${message}`, context);
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

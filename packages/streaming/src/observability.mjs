/**
 * @file Observability Manager - OTEL instrumentation for streaming operations
 * @module streaming/observability
 *
 * @description
 * Provides OpenTelemetry observability for streaming RDF operations including
 * traces, metrics, and error tracking.
 */

import { trace, metrics, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

/**
 * Observability configuration schema
 */
export const ObservabilityConfigSchema = z.object({
  serviceName: z.string().default('unrdf-streaming'),
  serviceVersion: z.string().default('1.0.0'),
  enableTracing: z.boolean().default(true),
  enableMetrics: z.boolean().default(true),
  enableLogging: z.boolean().default(true),
});

/**
 * Observability manager for streaming operations
 */
export class ObservabilityManager {
  /**
   * Create a new observability manager
   *
   * @param {Object} [config] - Observability configuration
   * @param {string} [config.serviceName='unrdf-streaming'] - Service name
   * @param {string} [config.serviceVersion='1.0.0'] - Service version
   * @param {boolean} [config.enableTracing=true] - Enable tracing
   * @param {boolean} [config.enableMetrics=true] - Enable metrics
   * @param {boolean} [config.enableLogging=true] - Enable logging
   */
  constructor(config = {}) {
    this.config = ObservabilityConfigSchema.parse(config);
    this.tracer = null;
    this.meter = null;
    this.initialized = false;
    this.activeSpans = new Map();

    // Metrics storage
    this.metrics = {
      operationCounter: null,
      errorCounter: null,
      operationDuration: null,
      cacheHitCounter: null,
      cacheMissCounter: null,
    };
  }

  /**
   * Initialize observability components
   *
   * @returns {Promise<void>}
   */
  async initialize() {
    if (this.initialized) return;

    try {
      if (this.config.enableTracing) {
        this.tracer = trace.getTracer(this.config.serviceName, this.config.serviceVersion);
      }

      if (this.config.enableMetrics) {
        this.meter = metrics.getMeter(this.config.serviceName, this.config.serviceVersion);
        this._createMetrics();
      }

      this.initialized = true;

      if (this.config.enableLogging) {
        console.log(`[ObservabilityManager] Initialized for ${this.config.serviceName}`);
      }
    } catch (error) {
      console.warn('[ObservabilityManager] Failed to initialize:', error.message);
      this._initializeFallback();
    }
  }

  /**
   * Create OTEL metrics
   *
   * @private
   */
  _createMetrics() {
    if (!this.meter) return;

    this.metrics.operationCounter = this.meter.createCounter('streaming.operations.total', {
      description: 'Total number of streaming operations',
    });

    this.metrics.errorCounter = this.meter.createCounter('streaming.errors.total', {
      description: 'Total number of streaming errors',
    });

    this.metrics.operationDuration = this.meter.createHistogram('streaming.operation.duration', {
      description: 'Streaming operation duration in milliseconds',
      unit: 'ms',
    });

    this.metrics.cacheHitCounter = this.meter.createCounter('streaming.cache.hits', {
      description: 'Cache hit count',
    });

    this.metrics.cacheMissCounter = this.meter.createCounter('streaming.cache.misses', {
      description: 'Cache miss count',
    });
  }

  /**
   * Initialize fallback mode (console logging)
   *
   * @private
   */
  _initializeFallback() {
    if (this.config.enableLogging) {
      console.log('[ObservabilityManager] Using fallback console logging');
    }
    this.initialized = true;
  }

  /**
   * Start a trace span
   *
   * @param {string} name - Span name
   * @param {Object} [attributes] - Span attributes
   * @returns {Object} Span context
   */
  startSpan(name, attributes = {}) {
    if (!this.tracer) {
      return {
        spanId: `${name}-${Date.now()}`,
        startTime: Date.now(),
        end: () => {},
        setAttributes: () => {},
        recordException: () => {},
        setStatus: () => {},
      };
    }

    const span = this.tracer.startSpan(name, {
      attributes: {
        'service.name': this.config.serviceName,
        ...attributes,
      },
    });

    const spanId = `${name}-${Date.now()}`;
    this.activeSpans.set(spanId, { span, startTime: Date.now() });

    return {
      spanId,
      span,
      startTime: Date.now(),
      end: () => {
        span.end();
        this.activeSpans.delete(spanId);
      },
      setAttributes: attrs => span.setAttributes(attrs),
      recordException: error => span.recordException(error),
      setStatus: status => span.setStatus(status),
    };
  }

  /**
   * Record an operation
   *
   * @param {string} operation - Operation name
   * @param {number} duration - Operation duration in milliseconds
   * @param {Object} [attributes] - Additional attributes
   * @returns {void}
   */
  recordOperation(operation, duration, attributes = {}) {
    if (this.metrics.operationCounter) {
      this.metrics.operationCounter.add(1, {
        operation,
        ...attributes,
      });
    }

    if (this.metrics.operationDuration) {
      this.metrics.operationDuration.record(duration, {
        operation,
        ...attributes,
      });
    }
  }

  /**
   * Record an error
   *
   * @param {Error} error - Error object
   * @param {Object} [context] - Error context
   * @returns {void}
   */
  recordError(error, context = {}) {
    if (this.metrics.errorCounter) {
      this.metrics.errorCounter.add(1, {
        error_type: error.name || 'Error',
        ...context,
      });
    }

    if (this.config.enableLogging) {
      console.error('[ObservabilityManager] Error:', error.message, context);
    }
  }

  /**
   * Record a cache hit
   *
   * @param {string} cacheKey - Cache key
   * @returns {void}
   */
  recordCacheHit(cacheKey) {
    if (this.metrics.cacheHitCounter) {
      this.metrics.cacheHitCounter.add(1, {
        cache_key: cacheKey,
      });
    }
  }

  /**
   * Record a cache miss
   *
   * @param {string} cacheKey - Cache key
   * @returns {void}
   */
  recordCacheMiss(cacheKey) {
    if (this.metrics.cacheMissCounter) {
      this.metrics.cacheMissCounter.add(1, {
        cache_key: cacheKey,
      });
    }
  }

  /**
   * Execute an operation with automatic tracing
   *
   * @template T
   * @param {string} operationName - Operation name
   * @param {Function} fn - Function to execute
   * @param {Object} [attributes] - Span attributes
   * @returns {Promise<T>} Operation result
   */
  async withSpan(operationName, fn, attributes = {}) {
    const spanContext = this.startSpan(operationName, attributes);
    const startTime = Date.now();

    try {
      const result = await fn(spanContext);
      const duration = Date.now() - startTime;

      spanContext.setAttributes({
        'operation.success': true,
        'operation.duration_ms': duration,
      });

      spanContext.setStatus({ code: SpanStatusCode.OK });
      this.recordOperation(operationName, duration, { status: 'success' });

      return result;
    } catch (error) {
      const duration = Date.now() - startTime;

      spanContext.recordException(error);
      spanContext.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });

      this.recordOperation(operationName, duration, { status: 'error' });
      this.recordError(error, { operation: operationName });

      throw error;
    } finally {
      spanContext.end();
    }
  }

  /**
   * Get active span count
   *
   * @returns {number} Number of active spans
   */
  getActiveSpanCount() {
    return this.activeSpans.size;
  }

  /**
   * Shutdown observability manager
   *
   * @returns {Promise<void>}
   */
  async shutdown() {
    // End all active spans
    for (const [spanId, { span }] of this.activeSpans) {
      try {
        span.end();
      } catch (error) {
        console.warn(`[ObservabilityManager] Failed to end span ${spanId}:`, error.message);
      }
    }

    this.activeSpans.clear();

    if (this.config.enableLogging) {
      console.log('[ObservabilityManager] Shutdown complete');
    }
  }
}

/**
 * Create a new observability manager instance
 *
 * @param {Object} [config] - Observability configuration
 * @returns {ObservabilityManager} Observability manager instance
 */
export function createObservabilityManager(config = {}) {
  return new ObservabilityManager(config);
}

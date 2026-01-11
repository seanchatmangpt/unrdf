/**
 * @file Distributed Tracing Utilities
 * @module observability/distributed-tracing
 *
 * @description
 * W3C Trace Context propagation, sampling strategies, and cross-service
 * correlation for distributed RDF operations.
 */

import { trace, propagation, context, SpanKind, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

/**
 * Trace context schema (W3C Trace Context format)
 */
export const TraceContextSchema = z.object({
  traceparent: z.string().regex(/^00-[a-f0-9]{32}-[a-f0-9]{16}-[0-9]{2}$/),
  tracestate: z.string().optional(),
});

/**
 * Sampling config schema
 */
export const SamplingConfigSchema = z.object({
  defaultRate: z.number().min(0).max(1).default(0.01),
  errorRate: z.number().min(0).max(1).default(1.0),
  slowThreshold: z.number().default(1000),
  slowRate: z.number().min(0).max(1).default(0.1),
});

/**
 * Distributed tracing manager
 *
 * Provides:
 * - W3C Trace Context propagation
 * - Adaptive sampling (errors, slow operations)
 * - Parent-child span relationships
 * - Cross-service correlation
 */
export class DistributedTracing {
  /**
   * Create distributed tracing manager
   *
   * @param {Object} [config] - Configuration
   * @param {string} [config.serviceName='unrdf'] - Service name
   * @param {Object} [config.sampling] - Sampling configuration
   */
  constructor(config = {}) {
    this.serviceName = config.serviceName || 'unrdf';
    this.samplingConfig = SamplingConfigSchema.parse(config.sampling || {});
    this.tracer = trace.getTracer(this.serviceName);

    // Active contexts for correlation
    this.activeContexts = new Map();
  }

  /**
   * Start a distributed trace
   *
   * @param {string} spanName - Span name
   * @param {Object} [options] - Span options
   * @param {Object} [options.attributes] - Span attributes
   * @param {SpanKind} [options.kind] - Span kind
   * @param {Object} [options.parentContext] - Parent trace context
   * @returns {Object} Span context with propagation headers
   */
  startSpan(spanName, options = {}) {
    const { attributes = {}, kind = SpanKind.INTERNAL, parentContext } = options;

    // Extract parent context if provided
    let ctx = context.active();
    if (parentContext) {
      ctx = this._extractContext(parentContext);
    }

    // Determine if should sample
    const shouldSample = this._shouldSample(attributes);

    // Start span
    const span = this.tracer.startSpan(
      spanName,
      {
        kind,
        attributes: {
          'service.name': this.serviceName,
          'sampling.decision': shouldSample ? 'sampled' : 'not_sampled',
          ...attributes,
        },
      },
      ctx
    );

    // Generate W3C Trace Context headers for propagation
    const traceHeaders = this._generateTraceHeaders(span);

    // Store active context
    const spanContext = {
      span,
      spanName,
      startTime: Date.now(),
      traceHeaders,
      sampled: shouldSample,
    };

    const spanId = span.spanContext().spanId;
    this.activeContexts.set(spanId, spanContext);

    return spanContext;
  }

  /**
   * End a distributed trace span
   *
   * @param {Object} spanContext - Span context from startSpan
   * @param {Object} [options] - End options
   * @param {Error} [options.error] - Error if operation failed
   * @param {Object} [options.attributes] - Final attributes
   */
  endSpan(spanContext, options = {}) {
    const { error, attributes = {} } = options;
    const { span, startTime } = spanContext;

    const duration = Date.now() - startTime;

    // Set final attributes
    span.setAttributes({
      'span.duration_ms': duration,
      ...attributes,
    });

    // Record error if present
    if (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
    } else {
      span.setStatus({ code: SpanStatusCode.OK });
    }

    span.end();

    // Cleanup
    const spanId = span.spanContext().spanId;
    this.activeContexts.delete(spanId);
  }

  /**
   * Create child span with parent relationship
   *
   * @param {Object} parentSpanContext - Parent span context
   * @param {string} childSpanName - Child span name
   * @param {Object} [options] - Child span options
   * @returns {Object} Child span context
   */
  createChildSpan(parentSpanContext, childSpanName, options = {}) {
    const { span: parentSpan } = parentSpanContext;

    // Create context from parent span
    const parentContext = trace.setSpan(context.active(), parentSpan);

    return this.startSpan(childSpanName, {
      ...options,
      parentContext: { context: parentContext },
    });
  }

  /**
   * Extract trace context from HTTP headers
   *
   * @param {Object} headers - HTTP headers
   * @returns {Object} Extracted context
   */
  extractFromHeaders(headers) {
    const carrier = {};

    // Normalize header names (case-insensitive)
    for (const [key, value] of Object.entries(headers)) {
      carrier[key.toLowerCase()] = value;
    }

    // Extract context using W3C propagator
    const ctx = propagation.extract(context.active(), carrier);

    return { context: ctx, headers: carrier };
  }

  /**
   * Inject trace context into HTTP headers
   *
   * @param {Object} spanContext - Span context
   * @returns {Object} HTTP headers with trace context
   */
  injectIntoHeaders(spanContext) {
    return spanContext.traceHeaders;
  }

  /**
   * Execute operation with distributed tracing
   *
   * @template T
   * @param {string} operationName - Operation name
   * @param {Function} fn - Async function to execute
   * @param {Object} [options] - Tracing options
   * @returns {Promise<T>} Operation result
   */
  async withSpan(operationName, fn, options = {}) {
    const spanContext = this.startSpan(operationName, options);

    try {
      const result = await fn(spanContext);
      this.endSpan(spanContext);
      return result;
    } catch (error) {
      this.endSpan(spanContext, { error });
      throw error;
    }
  }

  /**
   * Correlate spans by business ID
   *
   * @param {string} businessId - Business correlation ID
   * @param {Object} spanContext - Span context to correlate
   */
  correlateByBusinessId(businessId, spanContext) {
    const { span } = spanContext;

    span.setAttributes({
      'correlation.business_id': businessId,
      'correlation.type': 'business',
    });
  }

  /**
   * Correlate spans by user ID
   *
   * @param {string} userId - User ID
   * @param {Object} spanContext - Span context to correlate
   */
  correlateByUserId(userId, spanContext) {
    const { span } = spanContext;

    span.setAttributes({
      'correlation.user_id': userId,
      'correlation.type': 'user',
    });
  }

  /**
   * Generate W3C Trace Context headers
   *
   * @param {Span} span - OpenTelemetry span
   * @returns {Object} Trace context headers
   * @private
   */
  _generateTraceHeaders(span) {
    const carrier = {};

    // Inject context into carrier using W3C propagator
    const ctx = trace.setSpan(context.active(), span);
    propagation.inject(ctx, carrier);

    return carrier;
  }

  /**
   * Extract context from trace context object
   *
   * @param {Object} traceContext - Trace context
   * @returns {Context} OpenTelemetry context
   * @private
   */
  _extractContext(traceContext) {
    if (traceContext.context) {
      return traceContext.context;
    }

    // Extract from headers if provided
    if (traceContext.headers) {
      return propagation.extract(context.active(), traceContext.headers);
    }

    return context.active();
  }

  /**
   * Determine if span should be sampled
   *
   * Uses adaptive sampling:
   * - Always sample errors
   * - Sample slow operations at higher rate
   * - Sample normal operations at base rate
   *
   * @param {Object} attributes - Span attributes
   * @returns {boolean} True if should sample
   * @private
   */
  _shouldSample(attributes) {
    // Always sample if error expected
    if (attributes.error || attributes['error.expected']) {
      return Math.random() < this.samplingConfig.errorRate;
    }

    // Sample slow operations at higher rate
    if (attributes['operation.slow'] || attributes.slow) {
      return Math.random() < this.samplingConfig.slowRate;
    }

    // Default sampling rate
    return Math.random() < this.samplingConfig.defaultRate;
  }

  /**
   * Get active span count
   *
   * @returns {number} Number of active spans
   */
  getActiveSpanCount() {
    return this.activeContexts.size;
  }

  /**
   * Shutdown and cleanup
   */
  shutdown() {
    // End all active spans
    for (const [_spanId, spanContext] of this.activeContexts) {
      this.endSpan(spanContext, {
        attributes: { 'shutdown': true },
      });
    }

    this.activeContexts.clear();
  }
}

/**
 * Create distributed tracing instance
 *
 * @param {Object} [config] - Configuration
 * @returns {DistributedTracing} Tracing instance
 */
export function createDistributedTracing(config = {}) {
  return new DistributedTracing(config);
}

/**
 * Default distributed tracing instance
 */
export const defaultDistributedTracing = createDistributedTracing();

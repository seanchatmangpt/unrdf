/**
 * @fileoverview Centralized OTEL Span Creation and Management
 * @module otel-instrumentation
 *
 * @description
 * Provides comprehensive OpenTelemetry tracing utilities for all distributed
 * operations across the UNRDF system. This module centralizes span creation,
 * attribute recording, and error handling to ensure consistent observability.
 *
 * **Design Principles**:
 * - Keep spans lightweight (only at boundaries, not in business logic)
 * - Standard attributes: duration, error, result count
 * - Async support via async context propagation
 * - Minimal overhead for production use
 *
 * @version 1.0.0
 * @author UNRDF Team
 * @license MIT
 */

import { trace, SpanStatusCode, context } from '@opentelemetry/api';

/**
 * Service name for OTEL tracer
 * @constant {string}
 */
export const SERVICE_NAME = 'unrdf';

/**
 * Default tracer version
 * @constant {string}
 */
export const TRACER_VERSION = '6.0.0';

/**
 * Get the default tracer for UNRDF operations
 * @returns {import('@opentelemetry/api').Tracer} OTEL tracer instance
 */
export function getTracer() {
  return trace.getTracer(SERVICE_NAME, TRACER_VERSION);
}

/**
 * Re-export tracer for convenience
 * Lazy initialization to ensure provider is registered first
 */
export const tracer = {
  /**
   * Get the underlying tracer
   * @returns {import('@opentelemetry/api').Tracer}
   */
  get() {
    return getTracer();
  },
};

/**
 * @typedef {Object} SpanOptions
 * @property {Record<string, string|number|boolean>} [attributes] - Initial span attributes
 * @property {import('@opentelemetry/api').SpanKind} [kind] - Span kind (internal, server, client, etc.)
 */

/**
 * Create a new span with the given name and attributes
 * @param {string} name - Span name (e.g., 'query.sparql', 'rpc.call')
 * @param {Record<string, string|number|boolean>} [attributes={}] - Initial attributes
 * @returns {import('@opentelemetry/api').Span} Created span
 */
export function createSpan(name, attributes = {}) {
  const span = getTracer().startSpan(name, {
    attributes: {
      'service.name': SERVICE_NAME,
      'span.created_at': Date.now(),
      ...attributes,
    },
  });
  return span;
}

/**
 * Execute a function within a span context
 * Automatically handles span lifecycle, timing, and error recording
 *
 * @template T
 * @param {string} name - Span name
 * @param {() => T | Promise<T>} fn - Function to execute
 * @param {Record<string, string|number|boolean>} [attrs={}] - Span attributes
 * @returns {Promise<T>} Function result
 * @throws {Error} Rethrows any error from fn after recording to span
 */
export async function withSpan(name, fn, attrs = {}) {
  const span = createSpan(name, attrs);
  const startTime = Date.now();

  try {
    return await context.with(trace.setSpan(context.active(), span), async () => {
      const result = await fn();

      // Record success
      span.setAttributes({
        'span.duration_ms': Date.now() - startTime,
        'span.success': true,
      });
      span.setStatus({ code: SpanStatusCode.OK });

      return result;
    });
  } catch (error) {
    recordError(span, error);
    span.setAttributes({
      'span.duration_ms': Date.now() - startTime,
      'span.success': false,
    });
    throw error;
  } finally {
    span.end();
  }
}

/**
 * Record a custom attribute on a span
 * @param {import('@opentelemetry/api').Span} span - Target span
 * @param {string} key - Attribute key
 * @param {string|number|boolean} value - Attribute value
 * @returns {void}
 */
export function recordAttribute(span, key, value) {
  if (span && typeof span.setAttribute === 'function') {
    span.setAttribute(key, value);
  }
}

/**
 * Record multiple attributes on a span
 * @param {import('@opentelemetry/api').Span} span - Target span
 * @param {Record<string, string|number|boolean>} attributes - Attributes to record
 * @returns {void}
 */
export function recordAttributes(span, attributes) {
  if (span && typeof span.setAttributes === 'function') {
    span.setAttributes(attributes);
  }
}

/**
 * Record an error on a span
 * Sets span status to ERROR and records exception details
 *
 * @param {import('@opentelemetry/api').Span} span - Target span
 * @param {Error} error - Error to record
 * @returns {void}
 */
export function recordError(span, error) {
  if (!span || typeof span.recordException !== 'function') {
    return;
  }

  span.recordException(error);
  span.setStatus({
    code: SpanStatusCode.ERROR,
    message: error.message,
  });
  span.setAttributes({
    'error.type': error.constructor?.name || 'Error',
    'error.message': error.message,
    'error.stack': error.stack?.substring(0, 500) || '',
  });
}

/**
 * Record a metric value on a span
 * Useful for tracking counts, sizes, and durations
 *
 * @param {import('@opentelemetry/api').Span} span - Target span
 * @param {string} name - Metric name (e.g., 'result.count', 'duration.ms')
 * @param {number} value - Metric value
 * @returns {void}
 */
export function recordMetric(span, name, value) {
  if (span && typeof span.setAttribute === 'function') {
    span.setAttribute(`metric.${name}`, value);
  }
}

// ============================================================================
// Pre-configured span creators for common operations
// ============================================================================

/**
 * Trace a triple pattern match operation
 * @template T
 * @param {string|null} subject - Subject pattern
 * @param {string|null} predicate - Predicate pattern
 * @param {string|null} object - Object pattern
 * @param {() => T | Promise<T>} fn - Function to execute
 * @returns {Promise<T>} Function result
 */
export async function traceTriplePattern(subject, predicate, object, fn) {
  return withSpan('triple.pattern', fn, {
    'triple.subject': subject || '*',
    'triple.predicate': predicate || '*',
    'triple.object': object || '*',
    'operation.type': 'pattern_match',
  });
}

/**
 * Trace a SPARQL query execution
 * @template T
 * @param {string} query - SPARQL query string
 * @param {() => T | Promise<T>} fn - Function to execute
 * @returns {Promise<T>} Function result
 */
export async function traceSPARQLQuery(query, fn) {
  const queryType = detectQueryType(query);
  const span = createSpan('query.sparql', {
    'query.type': queryType,
    'query.length': query.length,
    'query.hash': hashQuery(query),
    'operation.type': 'sparql_query',
  });

  const startTime = Date.now();

  try {
    const result = await context.with(trace.setSpan(context.active(), span), fn);

    // Record result count if available
    const resultCount = Array.isArray(result)
      ? result.length
      : typeof result === 'boolean'
        ? 1
        : 0;

    span.setAttributes({
      'query.result_count': resultCount,
      'query.duration_ms': Date.now() - startTime,
    });
    span.setStatus({ code: SpanStatusCode.OK });

    return result;
  } catch (error) {
    recordError(span, error);
    throw error;
  } finally {
    span.end();
  }
}

/**
 * Trace an RPC call to a remote target
 * @template T
 * @param {string} target - RPC target address
 * @param {string} module - Module name
 * @param {() => T | Promise<T>} fn - Function to execute
 * @returns {Promise<T>} Function result
 */
export async function traceRPCCall(target, module, fn) {
  return withSpan('rpc.call', fn, {
    'rpc.target': target,
    'rpc.module': module,
    'rpc.system': 'atomvm',
    'operation.type': 'rpc',
  });
}

/**
 * Trace a message validation operation
 * @template T
 * @param {() => T | Promise<T>} fn - Function to execute
 * @returns {Promise<T>} Function result
 */
export async function traceMessageValidation(fn) {
  return withSpan('message.validate', fn, {
    'operation.type': 'validation',
    'validation.schema': 'msgpack',
  });
}

/**
 * Trace a cache operation (hit or miss)
 * @template T
 * @param {string} cacheKey - Cache key
 * @param {boolean} isHit - Whether cache hit
 * @param {() => T | Promise<T>} fn - Function to execute
 * @returns {Promise<T>} Function result
 */
export async function traceCacheOperation(cacheKey, isHit, fn) {
  return withSpan('cache.operation', fn, {
    'cache.key': cacheKey,
    'cache.hit': isHit,
    'operation.type': isHit ? 'cache_hit' : 'cache_miss',
  });
}

/**
 * Trace a batch operation (e.g., bulk triple insert)
 * @template T
 * @param {string} operationType - Type of batch operation
 * @param {number} batchSize - Number of items in batch
 * @param {() => T | Promise<T>} fn - Function to execute
 * @returns {Promise<T>} Function result
 */
export async function traceBatchOperation(operationType, batchSize, fn) {
  return withSpan('batch.operation', fn, {
    'batch.type': operationType,
    'batch.size': batchSize,
    'operation.type': 'batch',
  });
}

/**
 * Trace a performance-sensitive operation with timing
 * @template T
 * @param {string} operationName - Operation name
 * @param {() => T | Promise<T>} fn - Function to execute
 * @returns {Promise<{result: T, duration: number}>} Result with duration
 */
export async function traceWithTiming(operationName, fn) {
  const startTime = performance.now();
  const result = await withSpan(`perf.${operationName}`, fn, {
    'perf.operation': operationName,
    'perf.start_time': startTime,
  });
  const duration = performance.now() - startTime;

  return { result, duration };
}

// ============================================================================
// Utility functions
// ============================================================================

/**
 * Detect SPARQL query type from query string
 * @param {string} query - SPARQL query
 * @returns {string} Query type (SELECT, ASK, CONSTRUCT, DESCRIBE, INSERT, DELETE)
 */
function detectQueryType(query) {
  const normalized = query.trim().toUpperCase();
  const types = ['SELECT', 'ASK', 'CONSTRUCT', 'DESCRIBE', 'INSERT', 'DELETE'];

  for (const type of types) {
    if (normalized.startsWith(type) || normalized.includes(`${type} `)) {
      return type;
    }
  }

  return 'UNKNOWN';
}

/**
 * Create a simple hash of a query for deduplication
 * @param {string} query - Query string
 * @returns {string} Short hash
 */
function hashQuery(query) {
  let hash = 0;
  for (let i = 0; i < query.length; i++) {
    const char = query.charCodeAt(i);
    hash = (hash << 5) - hash + char;
    hash = hash & hash;
  }
  return Math.abs(hash).toString(36).substring(0, 8);
}

/**
 * Get the current active span from context
 * @returns {import('@opentelemetry/api').Span|undefined} Active span or undefined
 */
export function getActiveSpan() {
  return trace.getSpan(context.active());
}

/**
 * Check if tracing is enabled
 * @returns {boolean} True if tracing is available
 */
export function isTracingEnabled() {
  try {
    const testTracer = trace.getTracer('test');
    return testTracer !== undefined;
  } catch {
    return false;
  }
}

/**
 * Create a child span under the current active span
 * @param {string} name - Child span name
 * @param {Record<string, string|number|boolean>} [attributes={}] - Span attributes
 * @returns {import('@opentelemetry/api').Span} Child span
 */
export function createChildSpan(name, attributes = {}) {
  const parentContext = context.active();
  return getTracer().startSpan(
    name,
    {
      attributes: {
        'service.name': SERVICE_NAME,
        ...attributes,
      },
    },
    parentContext
  );
}

// ============================================================================
// Exports for all agents
// ============================================================================

export default {
  // Core utilities
  getTracer,
  tracer,
  createSpan,
  withSpan,
  recordAttribute,
  recordAttributes,
  recordError,
  recordMetric,

  // Pre-configured tracers
  traceTriplePattern,
  traceSPARQLQuery,
  traceRPCCall,
  traceMessageValidation,
  traceCacheOperation,
  traceBatchOperation,
  traceWithTiming,

  // Helpers
  getActiveSpan,
  isTracingEnabled,
  createChildSpan,

  // Constants
  SpanStatusCode,
  SERVICE_NAME,
  TRACER_VERSION,
};

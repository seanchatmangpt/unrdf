/**
 * @file OpenTelemetry Context Propagation Utilities
 * @module @unrdf/daemon/integrations/otel-context
 *
 * @description
 * W3C Trace Context propagation utilities for distributed tracing across daemon → sidecar → knowledge graph
 */

import { trace, context, propagation, SpanStatusCode } from '@opentelemetry/api';

/**
 * W3C Trace Context Header Names
 * @see https://www.w3.org/TR/trace-context/
 */
export const TRACE_CONTEXT_HEADERS = {
  TRACEPARENT: 'traceparent',
  TRACESTATE: 'tracestate',
};

/**
 * gRPC Metadata Keys for Trace Context
 */
export const GRPC_TRACE_METADATA = {
  TRACE_ID: 'x-trace-id',
  SPAN_ID: 'x-span-id',
  TRACE_FLAGS: 'x-trace-flags',
  TRACE_STATE: 'x-trace-state',
};

/**
 * Extract trace context from W3C traceparent header
 * @param {string} traceparent - W3C traceparent header value
 * @returns {Object|null} Parsed trace context or null if invalid
 *
 * @example
 * const ctx = parseTraceparent('00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01');
 * // { version: '00', traceId: '4bf92f...', spanId: '00f067...', traceFlags: '01' }
 */
export function parseTraceparent(traceparent) {
  if (!traceparent || typeof traceparent !== 'string') {
    return null;
  }

  const parts = traceparent.split('-');
  if (parts.length !== 4) {
    return null;
  }

  const [version, traceId, spanId, traceFlags] = parts;

  // Validate version (only 00 is currently supported)
  if (version !== '00') {
    return null;
  }

  // Validate trace ID (32 hex chars, not all zeros)
  if (!/^[0-9a-f]{32}$/.test(traceId) || traceId === '00000000000000000000000000000000') {
    return null;
  }

  // Validate span ID (16 hex chars, not all zeros)
  if (!/^[0-9a-f]{16}$/.test(spanId) || spanId === '0000000000000000') {
    return null;
  }

  // Validate trace flags (2 hex chars)
  if (!/^[0-9a-f]{2}$/.test(traceFlags)) {
    return null;
  }

  return {
    version,
    traceId,
    spanId,
    traceFlags,
  };
}

/**
 * Format trace context as W3C traceparent header
 * @param {Object} ctx - Trace context
 * @param {string} ctx.traceId - Trace ID (32 hex chars)
 * @param {string} ctx.spanId - Span ID (16 hex chars)
 * @param {string} [ctx.traceFlags='01'] - Trace flags (2 hex chars)
 * @returns {string} W3C traceparent header value
 *
 * @example
 * const header = formatTraceparent({
 *   traceId: '4bf92f3577b34da6a3ce929d0e0e4736',
 *   spanId: '00f067aa0ba902b7',
 *   traceFlags: '01'
 * });
 * // '00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01'
 */
export function formatTraceparent({ traceId, spanId, traceFlags = '01' }) {
  return `00-${traceId}-${spanId}-${traceFlags}`;
}

/**
 * Get trace context from current OpenTelemetry context
 * @returns {Object|null} Current trace context
 */
export function getCurrentTraceContext() {
  const span = trace.getSpan(context.active());

  if (!span) {
    return null;
  }

  const spanContext = span.spanContext();

  return {
    traceId: spanContext.traceId,
    spanId: spanContext.spanId,
    traceFlags: spanContext.traceFlags.toString(16).padStart(2, '0'),
    traceState: spanContext.traceState?.serialize(),
  };
}

/**
 * Extract trace ID from current context for logging
 * @returns {string} Trace ID or empty string
 */
export function getTraceIdForLogging() {
  const ctx = getCurrentTraceContext();
  return ctx?.traceId || '';
}

/**
 * Extract span ID from current context for logging
 * @returns {string} Span ID or empty string
 */
export function getSpanIdForLogging() {
  const ctx = getCurrentTraceContext();
  return ctx?.spanId || '';
}

/**
 * Add trace context to log entry
 * @param {Object} logEntry - Log entry object
 * @returns {Object} Log entry with trace context
 */
export function enrichLogWithTraceContext(logEntry) {
  const ctx = getCurrentTraceContext();

  if (!ctx) {
    return logEntry;
  }

  return {
    ...logEntry,
    trace_id: ctx.traceId,
    span_id: ctx.spanId,
    trace_flags: ctx.traceFlags,
  };
}

/**
 * Create a child span with proper context propagation
 * @param {string} name - Span name
 * @param {Object} attributes - Span attributes
 * @param {Object} [parentCtx] - Optional parent trace context
 * @returns {Object} Span object
 */
export function createChildSpan(name, attributes = {}, parentCtx = null) {
  const tracer = trace.getTracer('unrdf-context-propagation');

  // If parent context is provided, create span from it
  if (parentCtx) {
    // TODO: Create span from extracted context
    // This requires converting the context to OpenTelemetry SpanContext
    // For now, create span from current context
  }

  const span = tracer.startSpan(name, {
    attributes: {
      'service.name': 'unrdf-daemon',
      ...attributes,
    },
  });

  return span;
}

/**
 * Record exception in current span with trace context
 * @param {Error} error - Error to record
 * @param {Object} [attributes] - Additional attributes
 */
export function recordExceptionWithContext(error, attributes = {}) {
  const span = trace.getSpan(context.active());

  if (!span) {
    return;
  }

  span.recordException(error);
  span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });

  if (Object.keys(attributes).length > 0) {
    span.setAttributes(attributes);
  }
}

/**
 * Add metric exemplar linking to current trace
 * @param {Object} metric - Metric object
 * @returns {Object} Metric with exemplar
 */
export function addMetricExemplar(metric) {
  const ctx = getCurrentTraceContext();

  if (!ctx) {
    return metric;
  }

  return {
    ...metric,
    exemplar: {
      traceId: ctx.traceId,
      spanId: ctx.spanId,
      timestamp: Date.now(),
    },
  };
}

/**
 * Create trace context carrier for propagation
 * @returns {Object} Carrier object for context propagation
 */
export function createContextCarrier() {
  const carrier = {};
  propagation.inject(context.active(), carrier);
  return carrier;
}

/**
 * Extract context from carrier
 * @param {Object} carrier - Carrier object with propagated context
 * @returns {Object} Extracted context
 */
export function extractContextFromCarrier(carrier) {
  return propagation.extract(context.active(), carrier);
}

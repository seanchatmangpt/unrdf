/**
 * @file OpenTelemetry Tracer Utilities
 * @module @unrdf/daemon/integrations/otel-tracer
 *
 * @description
 * Wrapper utilities for OpenTelemetry trace API
 */

import { trace, context, SpanStatusCode } from '@opentelemetry/api';

/**
 * Create a tracer instance
 * @param {string} name - Tracer name
 * @returns {import('@opentelemetry/api').Tracer} Tracer instance
 */
export function createTracer(name = 'unrdf-daemon') {
  return trace.getTracer(name);
}

/**
 * Execute a function within a span with automatic error handling
 * @param {import('@opentelemetry/api').Tracer} tracer - Tracer instance
 * @param {string} name - Span name
 * @param {Function} fn - Function to execute
 * @param {Object} [attributes] - Initial span attributes
 * @returns {Promise<any>} Function result
 */
export function withSpan(tracer, name, fn, attributes = {}) {
  return tracer.startActiveSpan(name, { attributes }, async (span) => {
    try {
      const result = await fn(span);
      span.setStatus({ code: SpanStatusCode.OK });
      return result;
    } catch (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      throw error;
    } finally {
      span.end();
    }
  });
}

/**
 * Set attributes on a span
 * @param {import('@opentelemetry/api').Span} span - Span instance
 * @param {Object} attributes - Attributes to set
 */
export function setSpanAttributes(span, attributes) {
  if (span) {
    span.setAttributes(attributes);
  }
}

/**
 * Add an event to a span
 * @param {import('@opentelemetry/api').Span} span - Span instance
 * @param {string} name - Event name
 * @param {Object} [attributes] - Event attributes
 */
export function addSpanEvent(span, name, attributes = {}) {
  if (span) {
    span.addEvent(name, attributes);
  }
}

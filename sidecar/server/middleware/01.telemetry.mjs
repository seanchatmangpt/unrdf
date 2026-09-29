/**
 * @file OpenTelemetry Middleware
 * @description Request instrumentation with OTel tracing and context propagation
 */

import { defineEventHandler, useRuntimeConfig } from '#imports'
import { trace, context, SpanStatusCode } from '@opentelemetry/api'
import {
  extractTraceContextFromHeaders,
  getTraceIdForLogging,
  getSpanIdForLogging,
  enrichLogWithTraceContext,
  addMetricExemplar
} from '../utils/otel-context-propagation.mjs'

const tracer = trace.getTracer('kgc-sidecar-http')

/**
 * OpenTelemetry request tracing middleware with W3C Trace Context propagation
 * @param {Object} event - H3 event
 */
export default defineEventHandler(async (event) => {
  const config = useRuntimeConfig()

  // Skip if telemetry disabled
  if (!config.kgcEnableTelemetry) {
    return
  }

  const method = event.node.req.method
  const url = event.node.req.url
  const path = event.path || url

  // Extract trace context from HTTP headers (W3C Trace Context propagation)
  const incomingTraceContext = extractTraceContextFromHeaders(event.node.req.headers)

  // Create span for this request
  const span = tracer.startSpan(`HTTP ${method} ${path}`, {
    attributes: {
      'http.method': method,
      'http.url': url,
      'http.target': path,
      'http.user_agent': event.node.req.headers['user-agent'] || 'unknown',
      'service.name': 'unrdf-sidecar',
      'service.version': config.kgcVersion || '1.0.0',
      'deployment.environment': config.kgcEnvironment || 'development',
    }
  })

  // Add incoming trace context as attributes if present
  if (incomingTraceContext) {
    span.setAttributes({
      'parent.trace_id': incomingTraceContext.traceId,
      'parent.span_id': incomingTraceContext.spanId,
      'parent.trace_flags': incomingTraceContext.traceFlags,
    })
  }

  // Store span in event context for use in handlers
  event.context.span = span
  event.context.tracer = tracer

  // Extract trace ID and span ID for logging
  const traceId = getTraceIdForLogging()
  const spanId = getSpanIdForLogging()

  // Add trace context to all logs in this request
  event.context.logContext = {
    trace_id: traceId,
    span_id: spanId,
  }

  try {
    // Continue with request in trace context
    return await context.with(trace.setSpan(context.active(), span), async () => {
      return
    })
  } catch (error) {
    // Mark span as error
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message })
    span.recordException(error)

    // Log error with trace context
    console.error('[OpenTelemetry] Error recorded', enrichLogWithTraceContext({
      error: error.message,
      stack: error.stack,
    }))

    throw error
  } finally {
    // End span with status code
    const statusCode = event.node.res.statusCode || 200
    span.setAttribute('http.status_code', statusCode)

    if (statusCode >= 400) {
      span.setStatus({ code: SpanStatusCode.ERROR })
    } else {
      span.setStatus({ code: SpanStatusCode.OK })
    }

    // Add metric exemplar linking trace to metrics
    const requestMetric = addMetricExemplar({
      name: 'http.server.request.duration',
      value: Date.now() - span.startTime,
      attributes: {
        'http.method': method,
        'http.status_code': statusCode,
      },
    })

    // Log request completion with trace context
    console.info('[OpenTelemetry] Request completed', enrichLogWithTraceContext({
      method,
      path,
      statusCode,
      duration_ms: requestMetric.value,
      exemplar: requestMetric.exemplar,
    }))

    span.end()
  }
})

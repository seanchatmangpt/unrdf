/**
 * @file OTEL Trace Query Endpoint
 * @description GET /api/traces - Query OpenTelemetry traces
 *
 * Features:
 * - Query recent traces by service
 * - Filter by trace ID, span ID
 * - Export trace context for debugging
 */

import { defineEventHandler, getQuery } from '#imports'
import { sendSuccess } from '../utils/response.mjs'
import { trace, context } from '@opentelemetry/api'
import { getCurrentTraceContext, getTraceIdForLogging } from '../utils/otel-context-propagation.mjs'

/**
 * Trace query endpoint handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Trace query results
 */
export default defineEventHandler(async (event) => {
  const query = getQuery(event)
  const traceId = query.traceId
  const spanId = query.spanId
  const service = query.service || 'unrdf-sidecar'
  const limit = Math.min(parseInt(query.limit || '10'), 100)

  // Get current trace context
  const currentContext = getCurrentTraceContext()

  // In a production system, this would query a trace backend (Jaeger, Zipkin, etc.)
  // For now, we return the current trace context and mock data

  const traces = {
    current: currentContext,
    query: {
      traceId: traceId || 'all',
      spanId: spanId || 'all',
      service,
      limit
    },
    results: [
      // Mock trace data - in production, query from trace backend
      ...(currentContext ? [{
        traceId: currentContext.traceId,
        spanId: currentContext.spanId,
        service: 'unrdf-sidecar',
        operation: event.path,
        startTime: new Date().toISOString(),
        duration: 0, // Still in progress
        tags: {
          'http.method': event.node.req.method,
          'http.path': event.path,
          'http.request_id': event.context.requestId
        }
      }] : [])
    ],
    exporters: {
      jaeger: process.env.OTEL_EXPORTER_JAEGER_ENDPOINT || 'not configured',
      prometheus: '/api/metrics'
    },
    trace_context_propagation: {
      w3c_traceparent_header: currentContext ? `00-${currentContext.traceId}-${currentContext.spanId}-${currentContext.traceFlags}` : null,
      grpc_metadata: currentContext ? {
        'x-trace-id': currentContext.traceId,
        'x-span-id': currentContext.spanId
      } : null
    }
  }

  return sendSuccess(event, traces)
})

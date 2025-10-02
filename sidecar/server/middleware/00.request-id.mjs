/**
 * @file Request ID Middleware
 * @description Generates and tracks request IDs for distributed tracing correlation
 *
 * Features:
 * - Generates unique request IDs (UUIDs)
 * - Accepts X-Request-ID from client
 * - Propagates request ID to OTEL context
 * - Adds request ID to response headers
 */

import { defineEventHandler } from '#imports'
import { randomUUID } from 'node:crypto'
import { trace, context } from '@opentelemetry/api'

/**
 * Request ID middleware - runs first to establish request tracking
 * @param {Object} event - H3 event
 */
export default defineEventHandler(async (event) => {
  // Extract request ID from header or generate new one
  const requestId = event.node.req.headers['x-request-id'] || randomUUID()

  // Store in event context for access by handlers
  event.context.requestId = requestId

  // Add to OTEL span attributes for trace correlation
  const span = trace.getSpan(context.active())
  if (span) {
    span.setAttribute('http.request_id', requestId)
    span.setAttribute('request.id', requestId)
  }

  // Add request ID to response headers
  event.node.res.setHeader('X-Request-ID', requestId)

  // Continue to next middleware
  return
})

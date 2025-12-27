/**
 * @file server/api/otel/metrics.get.mjs
 * @description API endpoint for fetching OpenTelemetry metrics
 */

import { metrics } from '../../utils/otel-metrics.mjs'
import { getCurrentTraceContext, recordExceptionWithContext } from '../../utils/otel-context-propagation.mjs'

/**
 * Mock metrics data generator
 * In production, this would read from actual OTEL metrics collector
 * @returns {Object} Current metrics snapshot
 */
function getMetricsSnapshot() {
  // Mock data - in production, query OTEL collector or Prometheus
  return {
    rateLimit: {
      requests: Math.floor(Math.random() * 10000) + 5000,
      allowed: Math.floor(Math.random() * 8000) + 4000,
      blocked: Math.floor(Math.random() * 2000) + 500,
      rate: Math.random() * 100 + 20
    },
    ddos: {
      threatScore: Math.random() * 0.5, // 0-0.5 for normal operation
      blacklistAdditions: Math.floor(Math.random() * 10),
      requestsBlocked: Math.floor(Math.random() * 50)
    },
    query: {
      avgCost: Math.random() * 100 + 10,
      rejected: Math.floor(Math.random() * 20),
      total: Math.floor(Math.random() * 5000) + 1000
    },
    backpressure: {
      queueDepth: Math.floor(Math.random() * 100),
      systemLoad: Math.random() * 0.8, // 0-0.8 for healthy operation
      rejected: Math.floor(Math.random() * 30)
    },
    timestamp: new Date().toISOString(),
    traceContext: getCurrentTraceContext()
  }
}

/**
 * API endpoint for OTEL metrics
 * @param {import('h3').H3Event} event - H3 event object
 * @returns {Promise<Object>}
 */
export default eventHandler(async (event) => {
  try {
    // Get current trace context for correlation
    const traceContext = getCurrentTraceContext()

    // Record metric access
    if (metrics.rateLimitCounter) {
      metrics.rateLimitCounter.add(1, {
        endpoint: '/api/otel/metrics',
        method: 'GET'
      })
    }

    // Get metrics snapshot
    const metricsData = getMetricsSnapshot()

    // Set cache headers (cache for 3 seconds)
    setResponseHeaders(event, {
      'Cache-Control': 'public, max-age=3',
      'Content-Type': 'application/json'
    })

    return metricsData

  } catch (error) {
    // Record error in trace context
    recordExceptionWithContext(error, {
      endpoint: '/api/otel/metrics',
      method: 'GET'
    })

    throw createError({
      statusCode: 500,
      message: 'Failed to fetch OTEL metrics',
      data: {
        error: error.message
      }
    })
  }
})

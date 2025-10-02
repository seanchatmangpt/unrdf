// @ts-check
/**
 * Rate Limiter Status Endpoint
 *
 * GET /api/health/rate-limits
 *
 * Returns real-time rate limiter metrics
 */

import { rateLimiterRegistry } from '../../utils/rate-limiter.mjs'
import { createSuccessResponse } from '../../utils/response.mjs'

export default defineEventHandler(async (event) => {
  const metrics = rateLimiterRegistry.getMetricsSummary()

  return createSuccessResponse({
    metrics,
    timestamp: new Date().toISOString()
  })
})

// @ts-check
/**
 * Circuit Breaker Health Status Endpoint
 *
 * GET /api/health/circuits
 *
 * Returns real-time status of all circuit breakers
 */

import { circuitBreakerRegistry } from '../../utils/circuit-breaker.mjs'
import { createSuccessResponse } from '../../utils/response.mjs'

export default defineEventHandler(async (event) => {
  // Get all circuit breaker metrics
  const summary = circuitBreakerRegistry.getHealthSummary()

  // Calculate overall system health
  const breakers = Object.values(summary)
  const totalHealth = breakers.reduce((sum, b) => sum + b.healthScore, 0)
  const avgHealth = breakers.length > 0 ? totalHealth / breakers.length : 100

  return createSuccessResponse({
    systemHealth: Math.round(avgHealth),
    breakers: summary,
    breakerCount: breakers.length,
    openCircuits: breakers.filter(b => b.state === 'OPEN').length,
    timestamp: new Date().toISOString()
  })
})

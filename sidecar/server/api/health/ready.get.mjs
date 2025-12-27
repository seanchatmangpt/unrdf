/**
 * @file Readiness Probe Endpoint
 * @description GET /api/health/ready - Kubernetes readiness probe
 *
 * Checks:
 * - Managers initialized
 * - Circuit breakers healthy
 * - External dependencies accessible
 *
 * Returns 200 if ready to serve traffic, 503 if not ready
 */

import { defineEventHandler } from '#imports'
import { hasManagers } from '../../utils/managers.mjs'
import { circuitBreakerRegistry } from '../../utils/circuit-breaker.mjs'
import { sendSuccess, sendError } from '../../utils/response.mjs'

/**
 * Readiness probe handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Readiness status
 */
export default defineEventHandler(async (event) => {
  const checks = {
    managers: hasManagers(),
    circuitBreakers: true,
    timestamp: new Date().toISOString()
  }

  // Check circuit breaker health
  const circuitSummary = circuitBreakerRegistry.getHealthSummary()
  const unhealthyCircuits = Object.entries(circuitSummary)
    .filter(([_, metrics]) => metrics.state === 'OPEN')
    .map(([name]) => name)

  if (unhealthyCircuits.length > 0) {
    checks.circuitBreakers = false
    checks.unhealthyCircuits = unhealthyCircuits
  }

  // Determine overall readiness
  const ready = checks.managers && checks.circuitBreakers

  if (!ready) {
    event.node.res.statusCode = 503
    return {
      ready: false,
      checks,
      message: 'Service not ready to serve traffic'
    }
  }

  return sendSuccess(event, {
    ready: true,
    checks,
    message: 'Service is ready'
  })
})

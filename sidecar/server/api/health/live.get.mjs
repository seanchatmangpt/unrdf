/**
 * @file Liveness Probe Endpoint
 * @description GET /api/health/live - Kubernetes liveness probe
 *
 * Checks:
 * - Process is running
 * - Event loop not blocked
 * - Memory not exhausted
 *
 * Returns 200 if alive, 503 if should be restarted
 */

import { defineEventHandler } from '#imports'
import { sendSuccess } from '../../utils/response.mjs'

/**
 * Check if event loop is responsive
 * @returns {Promise<boolean>} True if event loop is responsive
 */
function checkEventLoop() {
  return new Promise((resolve) => {
    const start = Date.now()
    setImmediate(() => {
      const delay = Date.now() - start
      // Event loop is considered blocked if delay > 1000ms
      resolve(delay < 1000)
    })
  })
}

/**
 * Check memory health
 * @returns {Object} Memory health status
 */
function checkMemory() {
  const mem = process.memoryUsage()
  const heapUsedPercent = (mem.heapUsed / mem.heapTotal) * 100

  // Consider unhealthy if heap used > 95%
  const healthy = heapUsedPercent < 95

  return {
    healthy,
    heapUsedPercent: Math.round(heapUsedPercent * 100) / 100,
    heapUsed: mem.heapUsed,
    heapTotal: mem.heapTotal
  }
}

/**
 * Liveness probe handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Liveness status
 */
export default defineEventHandler(async (event) => {
  const checks = {
    process: true, // If we're here, process is running
    eventLoop: await checkEventLoop(),
    memory: checkMemory(),
    timestamp: new Date().toISOString()
  }

  // Determine overall liveness
  const alive = checks.process && checks.eventLoop && checks.memory.healthy

  if (!alive) {
    event.node.res.statusCode = 503
    return {
      alive: false,
      checks,
      message: 'Service should be restarted'
    }
  }

  return sendSuccess(event, {
    alive: true,
    checks,
    message: 'Service is alive',
    uptime: process.uptime()
  })
})

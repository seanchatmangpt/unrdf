/**
 * @file Health Check Endpoint
 * @description GET /api/health - Service health status
 */

import { defineEventHandler, useRuntimeConfig } from '#imports'
import { hasManagers } from '../utils/managers.mjs'
import { sendSuccess } from '../utils/response.mjs'

/**
 * Health check handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Health status
 */
export default defineEventHandler(async (event) => {
  const config = useRuntimeConfig()

  const checks = {
    managers: hasManagers(),
    telemetry: config.kgcEnableTelemetry,
    lockchain: !!config.kgcGitRepoUrl
  }

  const healthy = checks.managers

  return sendSuccess(event, {
    healthy,
    version: process.env.npm_package_version || '1.0.0',
    service: config.otelServiceName,
    checks
  })
})

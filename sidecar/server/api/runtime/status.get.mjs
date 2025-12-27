/**
 * @file server/api/runtime/status.get.mjs
 * @description Runtime status API endpoint with real hook manager integration
 * @module api/runtime/status
 */

import { defineEventHandler, createError } from 'h3'
import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, sendError } from '../../utils/response.mjs'
import { trace, SpanStatusCode } from '@opentelemetry/api'

const tracer = trace.getTracer('runtime-status-api')

/**
 * GET /api/runtime/status
 * Returns current runtime status including uptime, memory, hooks, and activity
 *
 * @param {Object} event - H3 event object
 * @returns {Promise<Object>} Runtime status with metrics
 */
export default defineEventHandler(async (event) => {
  return tracer.startActiveSpan('runtime.status.get', async (span) => {
    try {
      const { hookManager, transactionManager } = getManagers()

      // Get process uptime
      const uptime = process.uptime()

      // Get memory usage
      const memoryUsage = process.memoryUsage()
      const memory = {
        heapUsed: Math.round(memoryUsage.heapUsed / 1024 / 1024 * 100) / 100, // MB
        heapTotal: Math.round(memoryUsage.heapTotal / 1024 / 1024 * 100) / 100, // MB
        rss: Math.round(memoryUsage.rss / 1024 / 1024 * 100) / 100, // MB
        external: Math.round(memoryUsage.external / 1024 / 1024 * 100) / 100, // MB
        arrayBuffers: Math.round((memoryUsage.arrayBuffers || 0) / 1024 / 1024 * 100) / 100 // MB
      }

      // Get hook statistics from knowledge hook manager
      const allHooks = await hookManager.listHooks()
      const hookStats = allHooks.reduce((acc, hook) => {
        acc.total++
        if (hook.disabled) {
          acc.disabled++
        } else {
          acc.active++
        }

        // Count by phase
        const phase = hook.phase || 'unknown'
        if (!acc.byPhase[phase]) {
          acc.byPhase[phase] = 0
        }
        acc.byPhase[phase]++

        return acc
      }, {
        total: 0,
        active: 0,
        disabled: 0,
        byPhase: {}
      })

      // Get manager statistics
      const managerStats = hookManager.getStats ? hookManager.getStats() : {
        hooksRegistered: allHooks.length,
        hooksExecuted: 0,
        totalExecutionTime: 0,
        avgExecutionTime: 0
      }

      // Get transaction statistics
      const transactionStats = transactionManager?.getStats ? transactionManager.getStats() : {
        totalTransactions: 0,
        successfulTransactions: 0,
        failedTransactions: 0
      }

      // Get recent activity from hook executions (if available)
      const recentActivity = (hookManager.getRecentActivity && hookManager.getRecentActivity(10)) || []

      // Get active policy packs
      const activePolicyPacks = hookManager.getActivePolicyPacks ?
        hookManager.getActivePolicyPacks() : []

      // Build comprehensive status response
      const status = {
        uptime: Math.round(uptime),
        memory,
        hooks: {
          ...hookStats,
          policyPacks: {
            active: activePolicyPacks.length,
            names: activePolicyPacks.map(p => p.name || p)
          }
        },
        manager: managerStats,
        transactions: transactionStats,
        recentActivity: recentActivity.slice(0, 10).map(activity => ({
          hookId: activity.hookId || activity.id,
          timestamp: activity.timestamp || new Date().toISOString(),
          duration: activity.duration || 0,
          success: activity.success !== false,
          phase: activity.phase
        })),
        system: {
          nodeVersion: process.version,
          platform: process.platform,
          arch: process.arch,
          appVersion: process.env.APP_VERSION || process.env.npm_package_version || '3.0.0'
        },
        timestamp: new Date().toISOString()
      }

      // Add OTEL attributes
      span.setAttributes({
        'runtime.hooks.total': hookStats.total,
        'runtime.hooks.active': hookStats.active,
        'runtime.memory.heapUsed': memory.heapUsed,
        'runtime.uptime': uptime
      })

      span.setStatus({ code: SpanStatusCode.OK })
      return sendSuccess(event, status)

    } catch (error) {
      span.recordException(error)
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message
      })

      console.error('[runtime/status] Error fetching runtime status:', error)
      return sendError(event, error, {
        statusCode: 500,
        message: 'Failed to fetch runtime status'
      })
    } finally {
      span.end()
    }
  })
})

/**
 * @file Execute Effect Endpoint
 * @description POST /api/effects/execute - Execute registered effect with security isolation
 */
import { defineEventHandler, readBody } from '#imports'
import { trace } from '@opentelemetry/api'

import { sendSuccess, sendValidationError, sendError, asyncHandler } from '../../utils/response.mjs'
import { executeEffectSchema } from '../../utils/validation.mjs'
import { SecureSandbox } from '../../utils/secure-sandbox.mjs'

const tracer = trace.getTracer('execute-effect')

// Shared sandbox instance
const secureSandbox = new SecureSandbox({
  memoryLimit: 128,
  timeout: 5000,
  enableWasm: true
})

/**
 * Execute effect handler with security isolation
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Execution response
 */
export default defineEventHandler(asyncHandler(async (event) => {
  return tracer.startActiveSpan('executeEffect', async (span) => {
    try {
      const body = await readBody(event)

      // Validate request
      const validation = executeEffectSchema.safeParse(body)
      if (!validation.success) {
        return sendValidationError(event, validation.error)
      }

      const { effectId, input } = validation.data

      span.setAttribute('effectId', effectId)
      span.setAttribute('inputSize', JSON.stringify(input).length)

      // Execute effect in isolated sandbox
      const startTime = Date.now()
      const result = await secureSandbox.executeEffect(effectId, input)
      const executionTime = Date.now() - startTime

      // Get memory usage
      const memoryUsage = await secureSandbox.getMemoryUsage(effectId)

      span.setAttribute('executionTime', executionTime)
      span.setAttribute('memoryUsed', memoryUsage.used)
      span.setStatus({ code: 1 })
      span.end()

      return sendSuccess(event, {
        effectId,
        result,
        metrics: {
          executionTime,
          memoryUsage: {
            used: memoryUsage.used,
            percentage: memoryUsage.percentage
          }
        }
      })

    } catch (error) {
      span.recordException(error)
      span.setStatus({ code: 2, message: error.message })
      span.end()

      if (error.message?.includes('not found')) {
        return sendError(event, {
          code: 'EFFECT_NOT_FOUND',
          message: `Effect ${body.effectId} not registered`
        }, 404)
      }

      throw error
    }
  })
}))

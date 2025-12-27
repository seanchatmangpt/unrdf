/**
 * @file Register Effect Endpoint
 * @description POST /api/effects/register - Register JavaScript effect with security validation
 */
import { defineEventHandler, readBody, createError } from '#imports'
import { trace } from '@opentelemetry/api'

import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, sendValidationError, sendError, asyncHandler } from '../../utils/response.mjs'
import { registerEffectSchema } from '../../utils/validation.mjs'
import { SecureSandbox } from '../../utils/secure-sandbox.mjs'
import { SandboxThreatDetector } from '../../utils/sandbox-threat-detector.mjs'

const tracer = trace.getTracer('register-effect')

// Global instances (singleton pattern for performance)
let secureSandbox = null
let threatDetector = null

function getSecureSandbox() {
  if (!secureSandbox) {
    secureSandbox = new SecureSandbox({
      memoryLimit: 128,
      timeout: 5000,
      enableWasm: true
    })
  }
  return secureSandbox
}

function getThreatDetector() {
  if (!threatDetector) {
    threatDetector = new SandboxThreatDetector({
      blockThreshold: 80,
      enableCodeSigning: true,
      logAllThreats: true
    })
  }
  return threatDetector
}

/**
 * Register effect handler with security validation
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Registration response
 */
export default defineEventHandler(asyncHandler(async (event) => {
  return tracer.startActiveSpan('registerEffect', async (span) => {
    try {
      // Check authentication
      if (!event.context.auth?.authenticated) {
        throw createError({
          statusCode: 401,
          statusMessage: 'Unauthorized',
          message: 'Authentication required'
        })
      }

      const body = await readBody(event)

      // Validate request schema
      const validation = registerEffectSchema.safeParse(body)
      if (!validation.success) {
        return sendValidationError(event, validation.error)
      }

      const { id, code, timeout, memoryLimit, signature, publicKey } = validation.data
      const config = useRuntimeConfig()

      span.setAttribute('effectId', id)
      span.setAttribute('codeLength', code.length)
      span.setAttribute('userId', event.context.auth.userId)

      // SECURITY: Threat detection
      const detector = getThreatDetector()
      const threatAnalysis = await detector.analyzeCode(code, {
        signature,
        publicKey
      })

      span.setAttribute('threatScore', threatAnalysis.score)
      span.setAttribute('threatSeverity', threatAnalysis.severity)

      // Block high-threat code
      if (threatAnalysis.blocked) {
        span.setAttribute('blocked', true)
        span.setStatus({ code: 2, message: 'Code blocked by threat detection' })

        return sendError(event, {
          code: 'THREAT_DETECTED',
          message: 'Code contains potential security threats',
          details: {
            score: threatAnalysis.score,
            severity: threatAnalysis.severity,
            patterns: threatAnalysis.patterns.map(p => ({
              name: p.name,
              description: p.description,
              severity: p.severity
            }))
          }
        }, 403)
      }

      // Log medium/high threats for audit
      if (threatAnalysis.score > 40) {
        console.warn('[SecurityAudit] Medium-threat code registered:', {
          effectId: id,
          userId: event.context.auth.userId,
          score: threatAnalysis.score,
          patterns: threatAnalysis.patterns.map(p => p.name)
        })
      }

      // Register effect in secure sandbox
      const sandbox = getSecureSandbox()
      await sandbox.registerEffect(id, code)

      const effectTimeout = timeout || config.kgcSandboxTimeout || 5000
      const effectMemoryLimit = memoryLimit || config.kgcSandboxMemoryLimit || 128

      span.setStatus({ code: 1 })
      span.end()

      return sendSuccess(event, {
        effectId: id,
        timeout: effectTimeout,
        memoryLimit: effectMemoryLimit,
        registeredBy: event.context.auth.userId,
        security: {
          threatScore: threatAnalysis.score,
          severity: threatAnalysis.severity,
          signatureValid: threatAnalysis.signatureValid || false
        }
      }, 201)

    } catch (error) {
      span.recordException(error)
      span.setStatus({ code: 2, message: error.message })
      span.end()
      throw error
    }
  })
}))

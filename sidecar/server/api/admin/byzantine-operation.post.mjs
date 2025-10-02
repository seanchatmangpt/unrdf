/**
 * @file Byzantine Admin Operation Endpoint
 * @description POST /api/admin/byzantine-operation - Execute admin operation with Byzantine consensus
 */
import { defineEventHandler, readBody, createError } from '#imports'
import { z } from 'zod'
import {
  createAdminOperation,
  verifyByzantineConsensus,
  hasRole
} from '../../utils/auth.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../../utils/response.mjs'
import { trace, SpanStatusCode } from '@opentelemetry/api'

const tracer = trace.getTracer('unrdf-sidecar-admin')

/**
 * Byzantine operation schema
 */
const byzantineOperationSchema = z.object({
  operation: z.string().min(1, 'Operation identifier required'),
  data: z.record(z.any()),
  requireConsensus: z.boolean().optional().default(true)
})

/**
 * Byzantine operation handler with multi-signature consensus
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Operation result with consensus proof
 */
export default defineEventHandler(asyncHandler(async (event) => {
  return tracer.startActiveSpan('admin.byzantine_operation', async (span) => {
    try {
      // Check authentication
      if (!event.context.auth?.authenticated) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Unauthorized'
        })
        throw createError({
          statusCode: 401,
          statusMessage: 'Unauthorized',
          message: 'Authentication required'
        })
      }

      // Check admin role
      const token = event.node.req.headers.authorization?.substring(7)
      if (!token || !hasRole(token, 'admin')) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Forbidden - admin role required'
        })
        throw createError({
          statusCode: 403,
          statusMessage: 'Forbidden',
          message: 'Admin access required for Byzantine operations'
        })
      }

      const body = await readBody(event)

      span.setAttributes({
        'admin.operation': body.operation,
        'admin.user_id': event.context.auth.userId,
        'admin.require_consensus': body.requireConsensus !== false
      })

      // Validate request
      const validation = byzantineOperationSchema.safeParse(body)
      if (!validation.success) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Validation failed'
        })
        return sendValidationError(event, validation.error)
      }

      const { operation, data, requireConsensus } = validation.data

      if (!requireConsensus) {
        // Execute without consensus (for testing)
        span.setAttributes({
          'admin.consensus_bypassed': true
        })

        return sendSuccess(event, {
          operation,
          data,
          executedBy: event.context.auth.userId,
          consensusRequired: false
        })
      }

      // Create Byzantine consensus operation
      const { signatures, consensus } = createAdminOperation(operation, data)

      span.setAttributes({
        'admin.consensus.valid': consensus.valid,
        'admin.consensus.valid_count': consensus.validCount,
        'admin.consensus.threshold': consensus.threshold,
        'admin.consensus.validators': consensus.validators.join(',')
      })

      if (!consensus.valid) {
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Byzantine consensus failed'
        })
        throw createError({
          statusCode: 500,
          statusMessage: 'Consensus Failed',
          message: `Byzantine consensus failed. Required: ${consensus.threshold}, Valid: ${consensus.validCount}`
        })
      }

      // Log successful consensus
      span.addEvent('Byzantine consensus achieved', {
        validators: consensus.validators.join(','),
        signature_count: signatures.length
      })

      span.setStatus({ code: SpanStatusCode.OK })

      return sendSuccess(event, {
        operation,
        data,
        executedBy: event.context.auth.userId,
        consensus: {
          valid: consensus.valid,
          validSignatures: consensus.validCount,
          threshold: consensus.threshold,
          validators: consensus.validators
        },
        signatures: signatures.map(sig => ({
          validator: sig.validator,
          timestamp: sig.timestamp,
          publicKey: sig.publicKey
        })),
        timestamp: Date.now()
      })

    } catch (error) {
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message
      })
      span.recordException(error)
      throw error
    } finally {
      span.end()
    }
  })
}))

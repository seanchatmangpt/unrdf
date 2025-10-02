/**
 * @file Request Validation Middleware
 * @description Zod-based request validation for all API endpoints
 *
 * Features:
 * - Validates request body, query, and params
 * - OpenAPI-compliant error responses
 * - Integration with OTEL for validation metrics
 */

import { defineEventHandler, readBody } from '#imports'
import { z } from 'zod'
import { sendValidationError } from '../utils/response.mjs'
import { trace, context } from '@opentelemetry/api'

/**
 * Common validation schemas
 */
export const schemas = {
  // Hook registration schema
  hookRegister: z.object({
    id: z.string().min(1, 'Hook ID is required'),
    name: z.string().optional(),
    description: z.string().optional(),
    select: z.string().min(1, 'SPARQL SELECT query is required'),
    predicates: z.array(z.object({
      kind: z.string(),
      threshold: z.number().optional(),
      spec: z.any().optional()
    })).default([]),
    combine: z.enum(['AND', 'OR']).default('AND'),
    effect: z.function().optional()
  }),

  // Policy registration schema
  policyRegister: z.object({
    id: z.string().min(1, 'Policy ID is required'),
    name: z.string().optional(),
    rules: z.array(z.object({
      subject: z.string(),
      predicate: z.string(),
      object: z.any()
    })).min(1, 'At least one rule is required'),
    priority: z.number().int().min(0).default(0)
  }),

  // Transaction apply schema
  transactionApply: z.object({
    operations: z.array(z.object({
      type: z.enum(['insert', 'delete', 'update']),
      subject: z.string(),
      predicate: z.string(),
      object: z.any()
    })).min(1, 'At least one operation is required'),
    metadata: z.object({
      author: z.string().optional(),
      timestamp: z.string().datetime().optional(),
      description: z.string().optional()
    }).optional()
  }),

  // Query parameters schema
  queryParams: z.object({
    query: z.string().min(1, 'SPARQL query is required'),
    format: z.enum(['json', 'turtle', 'ntriples']).default('json'),
    timeout: z.number().int().positive().optional()
  }),

  // Pagination schema
  pagination: z.object({
    page: z.number().int().positive().default(1),
    limit: z.number().int().positive().max(100).default(20),
    sort: z.string().optional(),
    order: z.enum(['asc', 'desc']).default('asc')
  })
}

/**
 * Validate request against Zod schema
 * @param {Object} event - H3 event
 * @param {z.ZodSchema} schema - Zod schema to validate against
 * @param {string} source - Data source: 'body', 'query', or 'params'
 * @returns {Promise<any>} Validated data or throws validation error
 */
export async function validateRequest(event, schema, source = 'body') {
  const span = trace.getSpan(context.active())
  const startTime = Date.now()

  try {
    let data

    switch (source) {
      case 'body':
        data = await readBody(event)
        break
      case 'query':
        data = event.node.req.url ? new URL(event.node.req.url, `http://${event.node.req.headers.host}`).searchParams : {}
        break
      case 'params':
        data = event.context.params || {}
        break
      default:
        throw new Error(`Invalid validation source: ${source}`)
    }

    // Validate with Zod
    const validated = schema.parse(data)

    // Record successful validation
    if (span) {
      span.setAttribute('validation.success', true)
      span.setAttribute('validation.source', source)
      span.setAttribute('validation.duration_ms', Date.now() - startTime)
    }

    return validated
  } catch (error) {
    // Record validation failure
    if (span) {
      span.setAttribute('validation.success', false)
      span.setAttribute('validation.source', source)
      span.setAttribute('validation.error_count', error.errors?.length || 1)
      span.recordException(error)
    }

    // Return validation error response
    throw error
  }
}

/**
 * Request validation middleware
 * Note: Actual validation is done in handlers using validateRequest()
 * This middleware just sets up the infrastructure
 */
export default defineEventHandler(async (event) => {
  // Add validation helper to event context
  event.context.validate = (schema, source) => validateRequest(event, schema, source)

  // Continue to handler
  return
})

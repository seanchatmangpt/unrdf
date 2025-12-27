/**
 * @file SPARQL Query Endpoint
 * @description GET /api/query - Execute SPARQL query
 */

import { defineEventHandler, getQuery } from '#imports'
import { getManagers } from '../utils/managers.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../utils/response.mjs'
import { querySchema } from '../utils/validation.mjs'

/**
 * SPARQL query handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Query results
 */
export default defineEventHandler(asyncHandler(async (event) => {
  const queryParams = getQuery(event)

  // Validate request
  const validation = querySchema.safeParse(queryParams)
  if (!validation.success) {
    return sendValidationError(event, validation.error)
  }

  const { transactionManager } = getManagers()
  const { query, format } = validation.data

  // Execute SPARQL query
  const results = await transactionManager.executeQuery(query, {
    format: format || 'json'
  })

  return sendSuccess(event, {
    results,
    format: format || 'json',
    count: Array.isArray(results) ? results.length : undefined
  })
}))

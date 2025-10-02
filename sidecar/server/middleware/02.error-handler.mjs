/**
 * @file Global Error Handler Middleware
 * @description Catches and formats unhandled errors
 */

import { defineEventHandler } from '#imports'
import { sendError } from '../utils/response.mjs'
import { InternalError } from '../utils/errors.mjs'

/**
 * Global error handling middleware
 * @param {Object} event - H3 event
 */
export default defineEventHandler(async (event) => {
  try {
    // Let request proceed
    return
  } catch (error) {
    console.error('Unhandled error in middleware:', error)

    // Convert to InternalError if not already an ApiError
    const apiError = error.statusCode
      ? error
      : new InternalError('An unexpected error occurred', error)

    return sendError(event, apiError)
  }
})

/**
 * @file Delete Hook Endpoint
 * @description DELETE /api/hooks/:id - Delete a hook by ID
 */

import { defineEventHandler, getRouterParam } from '#imports'
import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, asyncHandler } from '../../utils/response.mjs'
import { NotFoundError } from '../../utils/errors.mjs'

/**
 * Delete hook handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Deletion confirmation
 */
export default defineEventHandler(asyncHandler(async (event) => {
  const hookId = getRouterParam(event, 'id')

  if (!hookId) {
    throw new NotFoundError('Hook ID is required')
  }

  const { hookManager } = getManagers()

  // Delete hook
  const deleted = await hookManager.removeHook(hookId)

  if (!deleted) {
    throw new NotFoundError(`Hook not found: ${hookId}`)
  }

  return sendSuccess(event, {
    hookId,
    deleted: true,
    timestamp: new Date().toISOString()
  })
}))

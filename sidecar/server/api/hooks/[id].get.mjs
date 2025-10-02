/**
 * @file Get Hook by ID Endpoint
 * @description GET /api/hooks/:id - Get a specific hook by ID
 */

import { defineEventHandler, getRouterParam } from '#imports'
import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, sendError, asyncHandler } from '../../utils/response.mjs'
import { NotFoundError } from '../../utils/errors.mjs'

/**
 * Get hook by ID handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Hook details
 */
export default defineEventHandler(asyncHandler(async (event) => {
  const hookId = getRouterParam(event, 'id')

  if (!hookId) {
    throw new NotFoundError('Hook ID is required')
  }

  const { hookManager } = getManagers()

  // Get hook by ID
  const hook = await hookManager.getHook(hookId)

  if (!hook) {
    throw new NotFoundError(`Hook not found: ${hookId}`)
  }

  // Format hook with full details including source code
  const formattedHook = {
    id: hook.id,
    meta: hook.meta || {},
    channel: hook.channel || {},
    when: hook.when || {},
    determinism: hook.determinism || { seed: 42 },
    receipt: hook.receipt || { anchor: 'none' },
    phase: hook.phase,
    predicates: hook.predicates || [],
    // Include source code representation
    source: hook._source || null,
    createdAt: hook.createdAt || new Date().toISOString(),
    updatedAt: hook.updatedAt || null
  }

  return sendSuccess(event, formattedHook)
}))

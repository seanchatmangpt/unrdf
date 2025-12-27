/**
 * @file List Hooks Endpoint
 * @description GET /api/hooks/list - List all registered knowledge hooks
 */

import { defineEventHandler } from '#imports'
import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, asyncHandler } from '../../utils/response.mjs'

/**
 * List hooks handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} List of hooks
 */
export default defineEventHandler(asyncHandler(async (event) => {
  const { hookManager } = getManagers()

  // Get all registered hooks
  const hooks = await hookManager.listHooks()

  // Format hooks for UI consumption
  const formattedHooks = hooks.map(hook => ({
    id: hook.id,
    name: hook.meta?.name || hook.id,
    description: hook.meta?.description || '',
    phase: hook.phase,
    predicateCount: hook.predicates?.length || 0,
    ontology: hook.meta?.ontology || [],
    channel: hook.channel || {},
    createdAt: hook.createdAt || new Date().toISOString()
  }))

  return sendSuccess(event, {
    hooks: formattedHooks,
    total: formattedHooks.length
  })
}))

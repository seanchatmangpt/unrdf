/**
 * @fileoverview Hook planning API endpoint
 */

import { planHook } from '../../../../lib/hooks.mjs'

/**
 * POST /api/hooks/[id]/plan - Plan hook execution
 */
export default defineEventHandler(async (event) => {
  const { requireAuth } = await import('../../../../lib/auth.mjs')
  requireAuth(event)
  const id = getRouterParam(event, 'id')
  
  const { hookRegistry } = await import('../../../../lib/hooks-state.mjs')
  
  if (!hookRegistry.has(id)) {
    throw createError({
      statusCode: 404,
      statusMessage: 'Hook not found'
    })
  }
  
  const hook = hookRegistry.get(id)
  const plan = planHook(hook)
  
  return {
    plan,
    timestamp: new Date().toISOString()
  }
})

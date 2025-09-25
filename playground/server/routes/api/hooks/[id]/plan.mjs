/**
 * @fileoverview Hook planning API endpoint
 */

import { planHook } from '../../../../src/hooks.mjs'

/**
 * POST /api/hooks/[id]/plan - Plan hook execution
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
  // Import the hook registry (this would be shared state in a real app)
  const { hookRegistry } = await import('../index.mjs')
  
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

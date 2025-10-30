/**
 * @fileoverview Individual hook API endpoint
 */

/**
 * GET /api/hooks/[id] - Get specific hook
 */
export default defineEventHandler(async (event) => {
  const { requireAuth } = await import('../_auth.mjs')
  requireAuth(event)
  const id = getRouterParam(event, 'id')
  
  const { hookRegistry, hookResults } = await import('./_shared.mjs')
  
  if (!hookRegistry.has(id)) {
    throw createError({
      statusCode: 404,
      statusMessage: 'Hook not found'
    })
  }
  
  const hook = hookRegistry.get(id)
  const results = hookResults.get(id) || []
  
  return {
    hook,
    recentResults: results.slice(-10), // Last 10 results
    totalEvaluations: results.length,
    timestamp: new Date().toISOString()
  }
})

// Also support DELETE /api/hooks/[id]
export const del = defineEventHandler(async (event) => {
  const { requireAuth } = await import('../_auth.mjs')
  requireAuth(event)
  const id = getRouterParam(event, 'id')
  const { hookRegistry, hookResults } = await import('./_shared.mjs')

  if (!hookRegistry.has(id)) {
    throw createError({
      statusCode: 404,
      statusMessage: 'Hook not found'
    })
  }

  hookRegistry.delete(id)
  hookResults.delete(id)

  return {
    success: true,
    message: `Hook ${id} deleted`,
    timestamp: new Date().toISOString()
  }
})

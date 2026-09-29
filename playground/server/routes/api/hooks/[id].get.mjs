/**
 * @fileoverview Individual hook API endpoint
 */

/**
 * GET /api/hooks/[id] - Get specific hook
 */
export default defineEventHandler(async (event) => {
  const { requireAuth } = await import('../../../lib/auth.mjs')
  requireAuth(event)
  const id = getRouterParam(event, 'id')
  
  const { hookRegistry, hookResults } = await import('../../../lib/hooks-state.mjs')
  
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

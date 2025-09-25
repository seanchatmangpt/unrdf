/**
 * @fileoverview Individual hook API endpoint
 */

/**
 * GET /api/hooks/[id] - Get specific hook
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
  // Import the hook registry (this would be shared state in a real app)
  const { hookRegistry, hookResults } = await import('./index.mjs')
  
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

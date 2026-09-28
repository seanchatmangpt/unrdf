/**
 * @fileoverview Individual data source API endpoint
 */

/**
 * GET /api/data/[id] - Get specific data source
 */
export default defineEventHandler(async (event) => {
  const { requireAuth } = await import('../../../lib/auth.mjs')
  requireAuth(event)
  const id = getRouterParam(event, 'id')
  
  const { dataStore } = await import('../../../lib/data-state.mjs')
  
  if (!dataStore.has(id)) {
    throw createError({
      statusCode: 404,
      statusMessage: 'Data source not found'
    })
  }
  
  const dataSource = dataStore.get(id)
  
  return {
    dataSource,
    timestamp: new Date().toISOString()
  }
})

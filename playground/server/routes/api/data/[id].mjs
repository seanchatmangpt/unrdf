/**
 * @fileoverview Individual data source API endpoint
 */

/**
 * GET /api/data/[id] - Get specific data source
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
  // Import the data store (this would be shared state in a real app)
  const { dataStore } = await import('./index.mjs')
  
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

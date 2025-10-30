/**
 * @fileoverview Individual data source API endpoint
 */

/**
 * GET /api/data/[id] - Get specific data source
 */
export default defineEventHandler(async (event) => {
  const { requireAuth } = await import('../_auth.mjs')
  requireAuth(event)
  const id = getRouterParam(event, 'id')
  
  const { dataStore } = await import('./_shared.mjs')
  
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

// Also support DELETE /api/data/[id]
export const del = defineEventHandler(async (event) => {
  const { requireAuth } = await import('../_auth.mjs')
  requireAuth(event)
  const id = getRouterParam(event, 'id')
  const { dataStore } = await import('./_shared.mjs')

  if (!dataStore.has(id)) {
    throw createError({
      statusCode: 404,
      statusMessage: 'Data source not found'
    })
  }

  dataStore.delete(id)

  return {
    success: true,
    message: `Data source ${id} deleted`,
    timestamp: new Date().toISOString()
  }
})

/**
 * @fileoverview Individual data source API endpoint
 */

/**
 * DELETE /api/data/[id] - Delete data source
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

  dataStore.delete(id)

  return {
    success: true,
    message: `Data source ${id} deleted`,
    timestamp: new Date().toISOString()
  }
})

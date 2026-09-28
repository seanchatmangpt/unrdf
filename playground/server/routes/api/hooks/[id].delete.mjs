/**
 * @fileoverview Individual hook API endpoint
 */

/**
 * DELETE /api/hooks/[id] - Delete hook
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

  hookRegistry.delete(id)
  hookResults.delete(id)

  return {
    success: true,
    message: `Hook ${id} deleted`,
    timestamp: new Date().toISOString()
  }
})

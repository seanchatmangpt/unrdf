/**
 * @fileoverview Data source query API endpoint
 */

import { createTurtleStore, termToJSON } from '../../../../lib/store.mjs'

/**
 * POST /api/data/[id]/query - Query data source
 */
export default defineEventHandler(async (event) => {
  const { requireAuth } = await import('../../../../lib/auth.mjs')
  requireAuth(event)
  const id = getRouterParam(event, 'id')
  
  const { dataStore } = await import('../../../../lib/data-state.mjs')
  
  if (!dataStore.has(id)) {
    throw createError({
      statusCode: 404,
      statusMessage: 'Data source not found'
    })
  }
  
  try {
    const body = await readBody(event)
    const dataSource = dataStore.get(id)
    
    if (!body.query) {
      throw createError({
        statusCode: 400,
        statusMessage: 'Query is required'
      })
    }
    
    const kind = body.query.trim().toUpperCase()
    if (!kind.startsWith('SELECT') && !kind.startsWith('ASK')) {
      throw new Error('Only SELECT and ASK queries are supported')
    }

    const store = createTurtleStore(dataSource.content)
    const raw = store.query(body.query)
    const result = Array.isArray(raw)
      ? raw.map(binding => {
          const row = {}
          for (const [name, term] of binding.entries()) row[name] = termToJSON(term)
          return row
        })
      : raw

    return {
      success: true,
      query: body.query,
      result,
      timestamp: new Date().toISOString()
    }
  } catch (error) {
    throw createError({
      statusCode: 500,
      statusMessage: error.message
    })
  }
})

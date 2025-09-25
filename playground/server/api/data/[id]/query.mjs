/**
 * @fileoverview Data source query API endpoint
 */

import { initStore } from '../../../../src/context/index.mjs'
import { useTurtle } from '../../../../src/composables/use-turtle.mjs'
import { useGraph } from '../../../../src/composables/use-graph.mjs'

/**
 * POST /api/data/[id]/query - Query data source
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
  // Import the data store (this would be shared state in a real app)
  const { dataStore } = await import('../index.mjs')
  
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
    
    // Initialize store with composable architecture
    const runApp = initStore()
    
    const result = await runApp(async () => {
      const turtle = await useTurtle()
      await turtle.parse(dataSource.content)
      
      const graph = await useGraph()
      
      if (body.query.trim().toUpperCase().startsWith('SELECT')) {
        return await graph.select(body.query)
      } else if (body.query.trim().toUpperCase().startsWith('ASK')) {
        return await graph.ask(body.query)
      } else {
        throw new Error('Only SELECT and ASK queries are supported')
      }
    })
    
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

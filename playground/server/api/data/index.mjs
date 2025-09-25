/**
 * @fileoverview Data management API for hooks runtime
 */

import { initStore } from '../../../../src/context/index.mjs'
import { useTurtle } from '../../../../src/composables/use-turtle.mjs'
import { useGraph } from '../../../../src/composables/use-graph.mjs'
import fs from 'node:fs/promises'
import { join } from 'node:path'

// In-memory data store
const dataStore = new Map()

/**
 * GET /api/data - List all data sources
 */
export default defineEventHandler(async (event) => {
  const method = getMethod(event)
  
  switch (method) {
    case 'GET':
      return {
        dataSources: Array.from(dataStore.entries()).map(([id, data]) => ({
          id,
          name: data.name,
          format: data.format,
          size: data.content.length,
          createdAt: data.createdAt
        })),
        total: dataStore.size,
        timestamp: new Date().toISOString()
      }
    
    case 'POST':
      return await createDataSource(event)
    
    default:
      throw createError({
        statusCode: 405,
        statusMessage: 'Method Not Allowed'
      })
  }
})

/**
 * POST /api/data - Create a new data source
 */
async function createDataSource(event) {
  try {
    const body = await readBody(event)
    
    if (!body.id || !body.content) {
      throw createError({
        statusCode: 400,
        statusMessage: 'ID and content are required'
      })
    }
    
    const dataSource = {
      id: body.id,
      name: body.name || body.id,
      content: body.content,
      format: body.format || 'Turtle',
      createdAt: new Date().toISOString()
    }
    
    dataStore.set(body.id, dataSource)
    
    return {
      success: true,
      dataSource: {
        id: dataSource.id,
        name: dataSource.name,
        format: dataSource.format,
        size: dataSource.content.length
      },
      timestamp: new Date().toISOString()
    }
  } catch (error) {
    throw createError({
      statusCode: 400,
      statusMessage: error.message
    })
  }
}

/**
 * GET /api/data/[id] - Get specific data source
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
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

/**
 * POST /api/data/[id]/query - Query data source
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
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

/**
 * DELETE /api/data/[id] - Delete data source
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
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

/**
 * @fileoverview Data management API for hooks runtime
 */

import { dataStore } from './_shared.mjs'
import { requireAuth } from '../../_auth.mjs'

/**
 * GET /api/data - List all data sources
 */
export default defineEventHandler(async (event) => {
  requireAuth(event)
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

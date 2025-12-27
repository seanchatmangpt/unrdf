/**
 * @fileoverview Hooks API endpoints for Nitro runtime
 */

import { defineHook } from '../../../../src/hooks.mjs'
import { requireAuth } from '../../_auth.mjs'
import { hookRegistry } from './_shared.mjs'

/**
 * GET /api/hooks - List all registered hooks
 */
export default defineEventHandler(async (event) => {
  requireAuth(event)
  const method = getMethod(event)
  
  switch (method) {
    case 'GET':
      return {
        hooks: Array.from(hookRegistry.values()),
        total: hookRegistry.size,
        timestamp: new Date().toISOString()
      }
    
    case 'POST':
      return await createHook(event)
    
    default:
      throw createError({
        statusCode: 405,
        statusMessage: 'Method Not Allowed'
      })
  }
})

/**
 * POST /api/hooks - Create a new hook
 */
async function createHook(event) {
  try {
    const body = await readBody(event)
    
    if (!body.id) {
      throw createError({
        statusCode: 400,
        statusMessage: 'Hook ID is required'
      })
    }
    
    const hook = defineHook(body)
    hookRegistry.set(hook.id, hook)
    
    return {
      success: true,
      hook: {
        id: hook.id,
        predicates: hook.predicates.length,
        combine: hook.combine
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


/**
 * @fileoverview Hooks API endpoints for Nitro runtime
 */

import { defineHook, evaluateHook, planHook, loadFrontmatterHook, registerPredicate } from '../../../../src/hooks.mjs'
import { initStore } from '../../../../src/context/index.mjs'
import { useTurtle } from '../../../../src/composables/use-turtle.mjs'
import { useGraph } from '../../../../src/composables/use-graph.mjs'
import fs from 'node:fs/promises'
import { join } from 'node:path'

// In-memory hook registry
const hookRegistry = new Map()
const hookResults = new Map()

/**
 * GET /api/hooks - List all registered hooks
 */
export default defineEventHandler(async (event) => {
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

/**
 * GET /api/hooks/[id] - Get specific hook
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
  if (!hookRegistry.has(id)) {
    throw createError({
      statusCode: 404,
      statusMessage: 'Hook not found'
    })
  }
  
  const hook = hookRegistry.get(id)
  const results = hookResults.get(id) || []
  
  return {
    hook,
    recentResults: results.slice(-10), // Last 10 results
    totalEvaluations: results.length,
    timestamp: new Date().toISOString()
  }
})

/**
 * POST /api/hooks/[id]/evaluate - Evaluate a hook
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
  if (!hookRegistry.has(id)) {
    throw createError({
      statusCode: 404,
      statusMessage: 'Hook not found'
    })
  }
  
  try {
    const body = await readBody(event)
    const hook = hookRegistry.get(id)
    
    // Initialize store with composable architecture
    const runApp = initStore()
    
    const result = await runApp(async () => {
      const turtle = await useTurtle()
      
      // Load data if provided
      if (body.data) {
        await turtle.parse(body.data)
      } else {
        // Use default sample data
        const sampleData = `
@prefix ex: <http://example.org/> .

ex:service1 a ex:Service ;
  ex:errorRate 0.05 ;
  ex:latency 1500 ;
  ex:requests 1000 .

ex:service2 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 300 ;
  ex:requests 2000 .
`
        await turtle.parse(sampleData)
      }
      
      // Evaluate hook
      const receipt = await evaluateHook(hook)
      
      // Store result
      const results = hookResults.get(id) || []
      results.push(receipt)
      hookResults.set(id, results)
      
      return receipt
    })
    
    return {
      success: true,
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
 * POST /api/hooks/[id]/plan - Plan hook execution
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
  if (!hookRegistry.has(id)) {
    throw createError({
      statusCode: 404,
      statusMessage: 'Hook not found'
    })
  }
  
  const hook = hookRegistry.get(id)
  const plan = planHook(hook)
  
  return {
    plan,
    timestamp: new Date().toISOString()
  }
})

/**
 * DELETE /api/hooks/[id] - Delete a hook
 */
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  
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

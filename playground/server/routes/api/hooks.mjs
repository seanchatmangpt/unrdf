/**
 * @fileoverview Hooks API for Nitro runtime
 */

import { defineHook, evaluateHook, planHook, registerPredicate } from '../../../src/hooks.mjs'
import { initStore } from '../../../src/context/index.mjs'
import { useTurtle } from '../../../src/composables/use-turtle.mjs'

// In-memory hook registry
const hookRegistry = new Map()
const hookResults = new Map()

/**
 * GET /api/hooks - List all hooks
 * POST /api/hooks - Create a hook
 */
export default defineEventHandler(async (event) => {
  const method = getMethod(event)
  
  if (method === 'GET') {
    return {
      hooks: Array.from(hookRegistry.values()),
      total: hookRegistry.size,
      timestamp: new Date().toISOString()
    }
  }
  
  if (method === 'POST') {
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
  
  throw createError({
    statusCode: 405,
    statusMessage: 'Method Not Allowed'
  })
})

/**
 * POST /api/hooks/evaluate - Evaluate a hook
 */
export default defineEventHandler(async (event) => {
  const method = getMethod(event)
  
  if (method === 'POST') {
    try {
      const body = await readBody(event)
      
      if (!body.hookId) {
        throw createError({
          statusCode: 400,
          statusMessage: 'Hook ID is required'
        })
      }
      
      if (!hookRegistry.has(body.hookId)) {
        throw createError({
          statusCode: 404,
          statusMessage: 'Hook not found'
        })
      }
      
      const hook = hookRegistry.get(body.hookId)
      
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
        const results = hookResults.get(body.hookId) || []
        results.push(receipt)
        hookResults.set(body.hookId, results)
        
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
  }
  
  throw createError({
    statusCode: 405,
    statusMessage: 'Method Not Allowed'
  })
})

/**
 * @fileoverview Hook evaluation API endpoint
 */

import { evaluateHook } from '../../../../packages/hooks/src/hooks/define-hook.mjs'
import { initStore } from '../../../../packages/composables/src/context/index.mjs'
import { useTurtle } from '../../../../packages/composables/src/composables/use-turtle.mjs'

/**
 * POST /api/hooks/[id]/evaluate - Evaluate a hook
 */
export default defineEventHandler(async (event) => {
  const { requireAuth } = await import('../../_auth.mjs')
  requireAuth(event)
  const id = getRouterParam(event, 'id')
  
  const { hookRegistry, hookResults } = await import('../_shared.mjs')
  
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

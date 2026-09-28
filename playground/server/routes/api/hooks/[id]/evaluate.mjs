/**
 * @fileoverview Hook evaluation API endpoint
 */

import { evaluateHook } from '../../../../lib/hooks.mjs'
import { createTurtleStore } from '../../../../lib/store.mjs'

/**
 * POST /api/hooks/[id]/evaluate - Evaluate a hook
 */
export default defineEventHandler(async (event) => {
  const { requireAuth } = await import('../../../../lib/auth.mjs')
  requireAuth(event)
  const id = getRouterParam(event, 'id')
  
  const { hookRegistry, hookResults } = await import('../../../../lib/hooks-state.mjs')
  
  if (!hookRegistry.has(id)) {
    throw createError({
      statusCode: 404,
      statusMessage: 'Hook not found'
    })
  }
  
  try {
    const body = await readBody(event)
    const hook = hookRegistry.get(id)
    
    const data = body?.data || `
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
    const store = createTurtleStore(data)

    // Evaluate hook
    const result = await evaluateHook(hook, store)

    // Store result
    const results = hookResults.get(id) || []
    results.push(result)
    // Bounded history: keep the most recent 100 evaluations per hook
    if (results.length > 100) results.shift()
    hookResults.set(id, results)

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

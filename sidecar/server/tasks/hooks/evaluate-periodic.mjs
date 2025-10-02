// @ts-check
/**
 * Periodic Knowledge Hook Evaluation Task
 *
 * Scheduled: Every 5 minutes (*/5 * * * *)
 *
 * Evaluates all registered knowledge hooks to:
 * - Detect policy violations
 * - Trigger effects based on conditions
 * - Update hook health metrics
 * - Store results in memory for analysis
 */

import { trace, context } from '@opentelemetry/api'
import { circuitBreakerRegistry } from '../../utils/circuit-breaker.mjs'

export default defineTask({
  meta: {
    name: 'hooks:evaluate-periodic',
    description: 'Periodic evaluation of all knowledge hooks with SAFLA neural feedback',
    version: '1.0.0'
  },

  async run({ payload, context: taskContext }) {
    const tracer = trace.getTracer('nitro-tasks')
    const span = tracer.startSpan('task.hooks.evaluate-periodic')

    try {
      console.log('[Task:hooks:evaluate-periodic] Starting periodic hook evaluation...')

      // Get hook manager from Nitro context
      const hookManager = taskContext?.hookManager
      if (!hookManager) {
        throw new Error('Hook manager not available in task context')
      }

      // Get all registered hooks
      const hooks = await hookManager.getAllHooks()
      console.log(`[Task:hooks:evaluate-periodic] Found ${hooks.length} registered hooks`)

      const results = {
        evaluated: 0,
        violated: 0,
        passed: 0,
        errors: 0,
        circuitOpen: 0,
        timestamp: new Date().toISOString()
      }

      // Evaluate each hook with circuit breaker protection
      for (const hook of hooks) {
        try {
          // Get or create circuit breaker for this hook
          const breaker = circuitBreakerRegistry.get(`hook:${hook.id}`, {
            failureThreshold: 3,
            timeout: 30000,
            errorThresholdPercentage: 40
          })

          // Execute hook evaluation with circuit breaker
          await breaker.execute(async () => {
            const result = await hookManager.evaluateHook(hook.id)

            results.evaluated++
            if (result.violated) {
              results.violated++
            } else {
              results.passed++
            }

            // Store result in memory for neural pattern learning
            await storeHookResult(hook.id, result)
          })

        } catch (error) {
          console.error(`[Task:hooks:evaluate-periodic] Error evaluating hook ${hook.id}:`, error)
          results.errors++

          // Check if circuit is open
          const breaker = circuitBreakerRegistry.get(`hook:${hook.id}`)
          if (breaker.getState() === 'OPEN') {
            results.circuitOpen++
          }

          span.recordException(error instanceof Error ? error : new Error(String(error)))
        }
      }

      // Record metrics
      span.setAttributes({
        'hooks.evaluated': results.evaluated,
        'hooks.violated': results.violated,
        'hooks.passed': results.passed,
        'hooks.errors': results.errors,
        'hooks.circuit_open': results.circuitOpen
      })

      console.log('[Task:hooks:evaluate-periodic] Evaluation complete:', results)

      // Store summary in memory for trending
      await storeEvaluationSummary(results)

      return { result: 'success', details: results }
    } catch (error) {
      span.recordException(error instanceof Error ? error : new Error(String(error)))
      span.setAttribute('task.error', true)
      throw error
    } finally {
      span.end()
    }
  }
})

/**
 * Store hook evaluation result for neural learning
 * @param {string} hookId - Hook ID
 * @param {Object} result - Evaluation result
 */
async function storeHookResult(hookId, result) {
  // In production, store in Redis or persistent storage
  // For now, use in-memory with OTEL metrics
  const tracer = trace.getTracer('nitro-tasks')
  const span = tracer.startSpan('store-hook-result')

  try {
    span.setAttributes({
      'hook.id': hookId,
      'hook.violated': result.violated,
      'hook.timestamp': new Date().toISOString()
    })

    // Could integrate with MCP memory here:
    // await mcp__claude-flow__memory_usage({
    //   action: 'store',
    //   namespace: 'safla-hooks',
    //   key: `hook:${hookId}:${Date.now()}`,
    //   value: JSON.stringify(result)
    // })
  } finally {
    span.end()
  }
}

/**
 * Store evaluation summary for trending
 * @param {Object} summary - Evaluation summary
 */
async function storeEvaluationSummary(summary) {
  const tracer = trace.getTracer('nitro-tasks')
  const span = tracer.startSpan('store-evaluation-summary')

  try {
    span.setAttributes({
      'summary.evaluated': summary.evaluated,
      'summary.violated': summary.violated,
      'summary.errors': summary.errors,
      'summary.timestamp': summary.timestamp
    })
  } finally {
    span.end()
  }
}

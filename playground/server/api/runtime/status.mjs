/**
 * @fileoverview Runtime status API for hooks engine
 */

/**
 * GET /api/runtime/status - Get runtime status
 */
export default defineEventHandler(async (event) => {
  const method = getMethod(event)
  
  switch (method) {
    case 'GET':
      return await getRuntimeStatus()
    
    case 'POST':
      return await executeRuntimeCommand(event)
    
    default:
      throw createError({
        statusCode: 405,
        statusMessage: 'Method Not Allowed'
      })
  }
})

/**
 * Get runtime status
 */
async function getRuntimeStatus() {
  try {
    // Import shared state (this would be shared state in a real app)
    const { hookRegistry, hookResults } = await import('../hooks/index.mjs')
    const { dataStore } = await import('../data/index.mjs')
    
    const totalHooks = hookRegistry.size
    const totalDataSources = dataStore.size
    const totalEvaluations = Array.from(hookResults.values()).reduce((sum, results) => sum + results.length, 0)
    
    // Get recent activity
    const recentActivity = []
    for (const [hookId, results] of hookResults.entries()) {
      if (results.length > 0) {
        const lastResult = results[results.length - 1]
        recentActivity.push({
          hookId,
          fired: lastResult.fired,
          timestamp: lastResult.at,
          duration: lastResult.durations.totalMs
        })
      }
    }
    
    // Sort by timestamp
    recentActivity.sort((a, b) => new Date(b.timestamp) - new Date(a.timestamp))
    
    return {
      status: 'running',
      uptime: process.uptime(),
      memory: process.memoryUsage(),
      hooks: {
        total: totalHooks,
        active: totalHooks,
        evaluations: totalEvaluations
      },
      data: {
        total: totalDataSources,
        active: totalDataSources
      },
      recentActivity: recentActivity.slice(0, 10),
      timestamp: new Date().toISOString()
    }
  } catch (error) {
    throw createError({
      statusCode: 500,
      statusMessage: error.message
    })
  }
}

/**
 * Execute runtime command
 */
async function executeRuntimeCommand(event) {
  try {
    const body = await readBody(event)
    const { command, params } = body
    
    switch (command) {
      case 'clear-results':
        const { hookResults } = await import('../hooks/index.mjs')
        hookResults.clear()
        return {
          success: true,
          message: 'All hook results cleared',
          timestamp: new Date().toISOString()
        }
      
      case 'reset-runtime':
        const { hookRegistry, hookResults: hookResults2 } = await import('../hooks/index.mjs')
        const { dataStore } = await import('../data/index.mjs')
        hookRegistry.clear()
        hookResults2.clear()
        dataStore.clear()
        return {
          success: true,
          message: 'Runtime reset complete',
          timestamp: new Date().toISOString()
        }
      
      case 'health-check':
        return {
          success: true,
          status: 'healthy',
          timestamp: new Date().toISOString()
        }
      
      default:
        throw createError({
          statusCode: 400,
          statusMessage: `Unknown command: ${command}`
        })
    }
  } catch (error) {
    throw createError({
      statusCode: 500,
      statusMessage: error.message
    })
  }
}

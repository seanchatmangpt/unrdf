/**
 * @fileoverview Default route for Nitro hooks runtime
 */

export default defineEventHandler(async (event) => {
  return {
    message: 'UNRDF Hooks Runtime',
    version: '1.0.0',
    endpoints: {
      hooks: '/api/hooks',
      data: '/api/data',
      runtime: '/api/runtime/status'
    },
    timestamp: new Date().toISOString()
  }
})

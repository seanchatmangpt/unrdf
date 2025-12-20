import { describe, it, expect } from 'vitest'
import { monitorClusterHealth } from './cluster-health-monitor.mjs'

describe('cluster-health-monitor - JSDoc Examples', () => {
  it('example 1', async () => {
    const report = await monitorClusterHealth(['my-app', 'db-container'], 2000, 30)
    console.log(report)
    // Output: { total: 2, healthy: 1, unhealthy: 1, downtime: 10 }
  })

})

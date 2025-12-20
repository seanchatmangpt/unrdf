import { describe, it, expect } from 'vitest'
import { checkSwarmHealth } from './docker-swarm-health.mjs'

describe('docker-swarm-health - JSDoc Examples', () => {
  it('example 1', async () => {
    const health = await checkSwarmHealth()
    console.log(health)
    // { total: 5, healthy: 4, unhealthy: 1, leaders: 2, workers: 3 }
  })

})

import { describe, it, expect } from 'vitest'
import { setupMicroSwarm } from './atomvm-micro-swarm-setup.mjs'

describe('atomvm-micro-swarm-setup - JSDoc Examples', () => {
  it('example 1', async () => {
    const swarmNodes = await setupMicroSwarm(['container1', 'container2'])
    console.log('Swarm nodes registered:', swarmNodes)
  })

})

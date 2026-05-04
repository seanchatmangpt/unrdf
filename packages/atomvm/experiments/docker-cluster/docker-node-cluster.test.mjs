import { describe, it, expect } from 'vitest'
import { startDockerCluster } from './docker-node-cluster.mjs'

describe('docker-node-cluster - JSDoc Examples', () => {
  it('example 1', async () => {
    const nodes = await startDockerCluster(3);
    console.log(nodes); // Array of StartedTestContainer instances
  })

})

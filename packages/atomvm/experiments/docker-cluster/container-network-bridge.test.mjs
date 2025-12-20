import { describe, it, expect } from 'vitest'
import { createClusterNetwork } from './container-network-bridge.mjs'

describe('container-network-bridge - JSDoc Examples', () => {
  it('example 1', async () => {
    const clusterInfo = await createClusterNetwork(['container1', 'container2', 'container3']);
    console.log(clusterInfo);
  })

})

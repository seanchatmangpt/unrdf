import { describe, it, expect } from 'vitest'
import { testErlangDistribution } from './real-erlang-distribution.mjs'

describe('real-erlang-distribution - JSDoc Examples', () => {
  it('example 1', async () => {
    const containers = {
      node1: { name: 'node1', exec: async (cmd) => 'ok' },
      node2: { name: 'node2', exec: async (cmd) => 'ok' }
    };
    const result = await testErlangDistribution(containers, 3, 1000, 5000);
    console.log(result);
  })

})

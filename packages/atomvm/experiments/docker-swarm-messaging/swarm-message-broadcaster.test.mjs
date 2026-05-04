import { describe, it, expect } from 'vitest'
import { broadcastMessage } from './swarm-message-broadcaster.mjs'

describe('swarm-message-broadcaster - JSDoc Examples', () => {
  it('example 1', async () => {
    const result = await broadcastMessage('node1', { data: 'hello' }, ['node2', 'node3', 'node4']);
    console.log(result); // { sent: 3, acked: 2, failed: 1 }
  })

})

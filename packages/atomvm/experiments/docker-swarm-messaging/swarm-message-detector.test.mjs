import { describe, it, expect } from 'vitest'
import { detectMessages } from './swarm-message-detector.mjs'

describe('swarm-message-detector - JSDoc Examples', () => {
  it('example 1', async () => {
    const result = await detectMessages(['my-container'], 10000);
    console.log(result.containerId); // 'my-container'
    console.log(result.messages); // [{from: 'user', content: 'Hello!', timestamp: '2023-04-05T12:34:56Z'}]
  })

})

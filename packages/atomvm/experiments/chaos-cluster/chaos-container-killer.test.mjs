import { describe, it, expect } from 'vitest'
import { killRandomContainer } from './chaos-container-killer.mjs'

describe('chaos-container-killer - JSDoc Examples', () => {
  it('example 1', async () => {
    const containers = ['my-container-1', 'my-container-2', 'my-container-3']
    const result = await killRandomContainer(containers)
    console.log(`Killed container: ${result.container} at ${result.timestamp}`)
  })

})

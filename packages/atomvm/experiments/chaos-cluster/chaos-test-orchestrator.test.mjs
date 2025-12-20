import { describe, it, expect } from 'vitest'
import { runChaosTest } from './chaos-test-orchestrator.mjs'

describe('chaos-test-orchestrator - JSDoc Examples', () => {
  it('example 1', async () => {
    const result = await runChaosTest(5, 3, 5000)
    console.log(`Total kills: ${result.totalKills}, Successful recoveries: ${result.successfulRecoveries}, Failures: ${result.failures}`)
  })

})

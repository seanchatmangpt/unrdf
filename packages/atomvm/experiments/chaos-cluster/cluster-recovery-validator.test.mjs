import { describe, it, expect } from 'vitest'
import { validateClusterRecovery } from './cluster-recovery-validator.mjs'

describe('cluster-recovery-validator - JSDoc Examples', () => {
  it('example 1', async () => {
    const beforeState = {
      containers: ['container1', 'container2', 'container3'],
      health: ['healthy', 'healthy', 'healthy']
    };
    const afterState = {
      containers: ['container1', 'container2', 'container3', 'container1'],
      health: ['healthy', 'healthy', 'healthy', 'healthy']
    };
    const result = validateClusterRecovery(beforeState, afterState, ['container1']);
    // result.recovered === true, result.failedNodes === [], result.cascadingFailures === false
  })

})

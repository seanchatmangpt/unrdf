import { describe, it, expect } from 'vitest'
import { cleanupDockerCluster, removeDockerNetwork } from './docker-cluster-cleanup.mjs'

describe('docker-cluster-cleanup - JSDoc Examples', () => {
  it('example 1', async () => {
    const containers = [
      { id: 'abc123', stop: jest.fn().mockResolvedValue(true) },
      { id: 'def456', stop: jest.fn().mockResolvedValue(true) }
    ];
    const result = await cleanupDockerCluster(containers, 'test-network');
    // result.success === true
  })

  it('example 2', async () => {
    await removeDockerNetwork('test-network');
  })

})

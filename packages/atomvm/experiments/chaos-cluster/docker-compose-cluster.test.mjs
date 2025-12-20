import { describe, it, expect } from 'vitest'
import { createDockerComposeYaml } from './docker-compose-cluster.mjs'

describe('docker-compose-cluster - JSDoc Examples', () => {
  it('example 1', async () => {
    const composeYaml = createDockerComposeYaml(3, 'my-bridge-network')
    console.log(composeYaml)
  })

})

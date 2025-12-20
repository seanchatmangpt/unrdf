import { describe, it, expect } from 'vitest'
import { setupErlangEPMD } from './erlang-epmd-setup.mjs'

describe('erlang-epmd-setup - JSDoc Examples', () => {
  it('example 1', async () => {
    const container = {
      exec: (cmd) => console.log(`Executing: ${cmd}`),
      run: (cmd) => console.log(`Running: ${cmd}`),
      wait: () => new Promise(resolve => setTimeout(resolve, 1000))
    };
    const epmdPort = await setupErlangEPMD(container);
    console.log(`EPMD is running on port ${epmdPort}`);
  })

})

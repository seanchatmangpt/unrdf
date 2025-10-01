import { test, expect } from 'vitest'
import path from 'path'
import { GenericContainer, BindMode } from 'testcontainers'

test('kgen CLI end-to-end in clean container', async () => {
  const projectDir = process.cwd()
  const container = await new GenericContainer('node:18')
    .withBindMount(projectDir, '/workspace', BindMode.READ_WRITE)
    .withWorkingDir('/workspace')
    .withCmd(['sleep', '300'])
    .start()
  try {
    let res = await container.exec(['npm', 'install', '-g', 'pnpm'])
    expect(res.exitCode).toBe(0)
    res = await container.exec(['pnpm', 'install'])
    expect(res.exitCode).toBe(0)
    res = await container.exec(['pnpm', 'start:demo'])
    expect(res.exitCode).toBe(0)
    const demoOutPath = path.posix.join('examples/demo/out.md')
    res = await container.exec(['cat', demoOutPath])
    expect(res.exitCode).toBe(0)
    expect(res.output).toContain('# Knowledge Graph Export')
  } finally {
    await container.stop()
  }
}, { timeout: 5 * 60 * 1000 })
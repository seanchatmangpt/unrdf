import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    include: ['test/**/*.spec.mjs', 'packages/kgen/test/**/*.spec.mjs'],
    exclude: ['**/*.integration.spec.mjs'],
    globals: true,
    setupFiles: 'setup.js'
  }
})
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    include: ['packages/**/*.spec.mjs'],
    globals: true,
    setupFiles: 'setup.js'
  }
})
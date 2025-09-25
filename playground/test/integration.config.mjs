import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    // Test environment
    environment: 'node',
    testTimeout: 30000,

    // Test patterns
    include: ['**/integration.test.mjs', '**/integration-*.test.mjs'],
    exclude: ['**/node_modules/**', '**/dist/**'],

    // Global setup and teardown
    globalSetup: './test/setup.mjs',
    setupFiles: ['./test/setup-each.mjs'],

    // Coverage configuration
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/**',
        'dist/**',
        'test/**',
        '**/*.config.mjs',
        'server.mjs'
      ],
      include: [
        'server/**/*.mjs',
        'src/**/*.mjs'
      ],
      thresholds: {
        global: {
          branches: 70,
          functions: 75,
          lines: 80,
          statements: 80
        }
      }
    },

    // Reporter configuration
    reporter: process.env.CI ? ['verbose', 'json'] : ['verbose', 'default'],

    // Retry configuration
    retry: 1,

    // Pool configuration for better performance
    pool: 'threads',
    poolOptions: {
      threads: {
        singleThread: false,
        isolate: true
      }
    }
  },

  // ES modules configuration
  esbuild: {
    target: 'node18'
  }
})

/**
 * @file Base Vitest Configuration
 * @description Shared vitest config factory for all UNRDF examples
 */

import { defineConfig } from 'vitest/config'

/**
 * Create vitest configuration for UNRDF packages
 * @param {Object} [options={}] - Configuration options
 * @returns {Object} - Vitest config object
 */
export function createVitestConfig(options = {}) {
  const {
    environment = 'node',
    include = ['test/**/*.test.mjs'],
    exclude = ['**/node_modules/**', '**/dist/**', '**/build/**'],
    coverage = {},
    testTimeout = 5000,
    globals = true,
    ...rest
  } = options

  return defineConfig({
    test: {
      // Test environment (node, jsdom, happy-dom)
      environment,

      // Test file patterns
      include,
      exclude,

      // Global test APIs (describe, it, expect)
      globals,

      // Test timeout
      testTimeout,

      // Coverage configuration
      coverage: {
        provider: 'v8',
        reporter: ['text', 'json', 'html', 'lcov'],
        reportsDirectory: './coverage',

        // Coverage thresholds
        lines: coverage.lines ?? 80,
        functions: coverage.functions ?? 80,
        branches: coverage.branches ?? 80,
        statements: coverage.statements ?? 80,

        // Files to include
        include: coverage.include ?? ['src/**/*.mjs'],

        // Files to exclude
        exclude: coverage.exclude ?? [
          'test/**',
          '**/*.test.mjs',
          '**/*.spec.mjs',
          '**/node_modules/**',
          '**/dist/**',
          '**/coverage/**'
        ],

        ...coverage
      },

      // Reporter configuration
      reporters: options.reporters ?? ['default'],

      // Setup files
      setupFiles: options.setupFiles ?? [],

      // Additional options
      ...rest
    }
  })
}

/**
 * Create config for Node.js environment tests
 * @param {Object} [options={}] - Additional options
 * @returns {Object} - Vitest config
 */
export function createNodeConfig(options = {}) {
  return createVitestConfig({
    environment: 'node',
    ...options
  })
}

/**
 * Create config for browser environment tests
 * @param {Object} [options={}] - Additional options
 * @returns {Object} - Vitest config
 */
export function createBrowserConfig(options = {}) {
  return createVitestConfig({
    environment: 'jsdom',
    ...options
  })
}

/**
 * Create config for happy-dom environment tests
 * @param {Object} [options={}] - Additional options
 * @returns {Object} - Vitest config
 */
export function createHappyDomConfig(options = {}) {
  return createVitestConfig({
    environment: 'happy-dom',
    ...options
  })
}

/**
 * Create config with custom coverage thresholds
 * @param {number} threshold - Coverage threshold percentage
 * @param {Object} [options={}] - Additional options
 * @returns {Object} - Vitest config
 */
export function createCoverageConfig(threshold, options = {}) {
  return createVitestConfig({
    coverage: {
      lines: threshold,
      functions: threshold,
      branches: threshold,
      statements: threshold
    },
    ...options
  })
}

/**
 * Create config for integration tests
 * @param {Object} [options={}] - Additional options
 * @returns {Object} - Vitest config
 */
export function createIntegrationConfig(options = {}) {
  return createVitestConfig({
    include: ['test/integration/**/*.test.mjs'],
    testTimeout: 10000, // Longer timeout for integration tests
    ...options
  })
}

/**
 * Create config for unit tests
 * @param {Object} [options={}] - Additional options
 * @returns {Object} - Vitest config
 */
export function createUnitConfig(options = {}) {
  return createVitestConfig({
    include: ['test/unit/**/*.test.mjs'],
    testTimeout: 5000,
    ...options
  })
}

export default createVitestConfig

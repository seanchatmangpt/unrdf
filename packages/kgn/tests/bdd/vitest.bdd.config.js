/**
 * Vitest BDD Configuration for KGEN Templates
 * Comprehensive testing setup with Cucumber integration
 */
import { defineConfig } from 'vitest/config';
import { getCucumberSteps } from '@amiceli/vitest-cucumber';
import path from 'path';

export default defineConfig({
  test: {
    // Test environment configuration
    environment: 'node',
    globals: true,

    // Include patterns for BDD tests
    include: [
      'tests/bdd/**/*.test.js',
      'tests/bdd/**/*.spec.js',
      'tests/bdd/features/**/*.feature'
    ],

    // Exclude patterns
    exclude: [
      'node_modules/**',
      'dist/**',
      'build/**',
      'coverage/**'
    ],

    // Test timeouts
    testTimeout: 30000,
    hookTimeout: 10000,

    // Coverage configuration
    coverage: {
      enabled: true,
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      reportsDirectory: './coverage/bdd',
      exclude: [
        'tests/**',
        'node_modules/**',
        '**/*.config.js',
        '**/*.config.ts'
      ],
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 75,
        statements: 80
      }
    },

    // Setup files
    setupFiles: [
      './tests/bdd/setup.js'
    ],

    // Reporter configuration
    reporters: [
      'default',
      'json',
      ['html', { outputFile: './coverage/bdd/test-report.html' }]
    ],

    // Parallel execution
    pool: 'forks',
    poolOptions: {
      forks: {
        singleFork: false,
        minForks: 1,
        maxForks: 4
      }
    },

    // Watch configuration
    watchIgnore: [
      'node_modules/**',
      'coverage/**',
      'dist/**',
      'build/**'
    ],

    // Environment variables
    env: {
      NODE_ENV: 'test',
      TEST_MODE: 'bdd',
      UPDATE_GOLDEN: process.env.UPDATE_GOLDEN || 'false',
      CROSS_PLATFORM: process.env.CROSS_PLATFORM || 'false'
    }
  },

  // Resolve configuration
  resolve: {
    alias: {
      '@': path.resolve(__dirname, '../../../src'),
      '@tests': path.resolve(__dirname, '.'),
      '@fixtures': path.resolve(__dirname, './fixtures'),
      '@golden': path.resolve(__dirname, './golden'),
      '@mocks': path.resolve(__dirname, './fixtures')
    }
  },

  // Define configuration for different test modes
  define: {
    __TEST_MODE__: JSON.stringify(process.env.TEST_MODE || 'bdd'),
    __PLATFORM__: JSON.stringify(process.platform),
    __NODE_VERSION__: JSON.stringify(process.version)
  },

  // Cucumber integration
  plugins: [
    {
      name: 'vitest-cucumber',
      config() {
        return {
          cucumber: {
            features: [
              './tests/bdd/features/**/*.feature'
            ],
            steps: [
              './tests/bdd/step-definitions/**/*.js'
            ],
            support: [
              './tests/bdd/support/**/*.js'
            ],
            // World configuration
            worldParameters: {
              timeout: 30000,
              retries: 2
            },
            // Format options
            format: [
              'progress',
              'json:./coverage/bdd/cucumber-report.json',
              'html:./coverage/bdd/cucumber-report.html'
            ],
            // Tag expressions for selective testing
            tags: process.env.CUCUMBER_TAGS || '@critical or @golden',
            // Parallel execution
            parallel: parseInt(process.env.CUCUMBER_PARALLEL || '2')
          }
        };
      }
    }
  ],

  // Global test configuration
  esbuild: {
    target: 'node18'
  }
});

// Export configuration for different test modes
export const configs = {
  // Standard BDD tests
  bdd: defineConfig({
    test: {
      include: ['tests/bdd/**/*.test.js'],
      exclude: ['tests/bdd/permutation/**', 'tests/bdd/cross-os/**'],
      env: { TEST_MODE: 'bdd' }
    }
  }),

  // Golden tests specifically
  golden: defineConfig({
    test: {
      include: ['tests/bdd/golden/**/*.test.js'],
      env: {
        TEST_MODE: 'golden',
        UPDATE_GOLDEN: process.env.UPDATE_GOLDEN || 'false'
      }
    }
  }),

  // Permutation tests
  permutation: defineConfig({
    test: {
      include: ['tests/bdd/permutation/**/*.test.js'],
      testTimeout: 60000,
      env: {
        TEST_MODE: 'permutation',
        PERMUTATION_ITERATIONS: process.env.PERMUTATION_ITERATIONS || '10'
      }
    }
  }),

  // Cross-platform tests
  crossPlatform: defineConfig({
    test: {
      include: ['tests/bdd/cross-os/**/*.test.js'],
      env: {
        TEST_MODE: 'cross-platform',
        CROSS_PLATFORM: 'true'
      }
    }
  }),

  // Performance tests
  performance: defineConfig({
    test: {
      include: ['tests/bdd/**/*.perf.test.js'],
      testTimeout: 120000,
      env: { TEST_MODE: 'performance' }
    }
  }),

  // CI configuration
  ci: defineConfig({
    test: {
      reporter: ['github-actions', 'json'],
      coverage: {
        reporter: ['lcov', 'text-summary']
      },
      pool: 'forks',
      poolOptions: {
        forks: {
          maxForks: 2
        }
      }
    }
  })
};
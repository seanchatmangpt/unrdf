import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Use node environment for compatibility with Node.js APIs
    environment: 'node',

    // Test file patterns
    include: ['test/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],
    exclude: ['node_modules', 'dist', '.idea', '.git', '.cache'],

    // Coverage configuration
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      lines: 80,        // Minimum 80% line coverage
      functions: 80,    // Minimum 80% function coverage
      branches: 80,     // Minimum 80% branch coverage
      statements: 80,   // Minimum 80% statement coverage
      all: true,        // Include all files in coverage
      exclude: [
        'node_modules/',
        'test/',
        '**/*.config.js',
        '**/*.config.mjs',
        '**/dist/**',
      ],
    },

    // Global test setup
    globals: true,

    // Timeout for tests - strict 5s max for quick iteration
    testTimeout: 5000,

    // Hook timeout - strict 5s max
    hookTimeout: 5000,

    // Reporter configuration
    reporters: ['default'],

    // Enable/disable specific test features
    isolate: true,
    threads: true,
    maxThreads: 4,
    minThreads: 1,

    // Bail out on first test failure (useful for CI)
    bail: 0,  // Set to 1 for fail-fast mode

    // Silent mode
    silent: false,
  },
});

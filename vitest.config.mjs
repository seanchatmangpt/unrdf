/**
 * @fileoverview Vitest configuration for unrdf
 * Maximized for concurrency and performance
 */
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Maximize concurrency - use all available CPU cores
    pool: 'threads',
    poolOptions: {
      threads: {
        // Use all available CPU cores
        maxThreads: undefined,
        minThreads: 1,
        // Enable single-thread mode for better performance on I/O bound tests
        useAtomics: true,
      },
    },
    
    // Run tests in parallel by default
    concurrent: true,
    
    // Maximum number of concurrent test files
    maxConcurrency: 10,
    
    // Test timeout - generous for RDF operations
    testTimeout: 30000,
    
    // Hook timeout
    hookTimeout: 30000,
    
    // Coverage configuration
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      // Exclude test files and config files from coverage
      exclude: [
        'node_modules/**',
        'test/**',
        '**/*.test.mjs',
        '**/*.config.mjs',
        'dist/**',
        'coverage/**',
      ],
      // Include source files
      include: ['src/**/*.mjs'],
      // Coverage thresholds
      thresholds: {
        global: {
          branches: 80,
          functions: 80,
          lines: 80,
          statements: 80,
        },
      },
    },
    
    // Environment configuration
    environment: 'node',
    
    // File patterns
    include: [
      'test/**/*.test.mjs', 
      'test/**/*.spec.mjs',
      'test/composables/**/*.test.mjs',
      'test/utils/**/*.test.mjs'
    ],
    exclude: ['node_modules/**', 'dist/**'],
    
    // Reporter configuration
    reporter: ['verbose', 'json', 'html'],
    outputFile: {
      json: './coverage/test-results.json',
      html: './coverage/test-results.html',
    },
    
    // Global setup
    globalSetup: [],
    
    // Setup files
    setupFiles: [],
    
    // Test file patterns
    globals: false,
    
    // Isolate test environment
    isolate: true,
    
    // Pass with no tests
    passWithNoTests: true,
    
    // Retry failed tests
    retry: 2,
    
    // Bail on first failure (useful for CI)
    bail: 0,
    
    // Watch mode configuration
    watch: false,
    
    // Force Rerun on file change
    forceRerunTriggers: ['**/package.json/**', '**/vitest.config.*/**'],
    
    // Type checking
    typecheck: {
      enabled: false, // We're using JSDoc, not TypeScript
    },
    
    // Benchmark configuration
    benchmark: {
      outputFile: './coverage/benchmark-results.json',
    },
    
    // UI mode configuration
    ui: false,
    
    // API mode
    api: false,
    
    // Inspect mode
    inspect: false,
    
    // Inspect brk mode
    inspectBrk: false,
    
    // Log level
    logLevel: 'info',
    
    // Silent mode
    silent: false,
    
    // Reporter options
    reporterOptions: {
      verbose: {
        showDiff: true,
        showErrorStack: true,
      },
    },
    
    // Test name pattern
    testNamePattern: undefined,
    
    // Update snapshots
    update: false,
    
    // Related files
    related: undefined,
    
    // Run tests
    run: true,
    
    // Mode
    mode: 'test',
    
    // Root directory
    root: process.cwd(),
    
    // Config file
    config: undefined,
    
    // Dependencies
    deps: {
      // External dependencies that should be treated as external
      external: [],
      // Inline dependencies that should be bundled
      inline: [],
    },
    
    // Server configuration
    server: {
      // Source map support
      sourcemap: true,
    },
    
    // Worker configuration
    worker: {
      // Worker pool configuration
      pool: 'threads',
      poolOptions: {
        threads: {
          maxThreads: undefined,
          minThreads: 1,
          useAtomics: true,
        },
      },
    },
    
    // Browser configuration (not used for Node.js tests)
    browser: {
      enabled: false,
    },
    
    // Experimental features
    experimentalFeatures: {
      // Enable experimental features if needed
    },
  },
  
  // Resolve configuration
  resolve: {
    alias: {
      // Add any path aliases if needed
    },
  },
  
  // Define global constants
  define: {
    // Define any global constants
  },
  
  // Optimize dependencies
  optimizeDeps: {
    // Include dependencies that should be pre-bundled
    include: ['n3', 'zod', '@comunica/query-sparql'],
    // Exclude dependencies that should not be pre-bundled
    exclude: [],
  },
  
  // Build configuration
  build: {
    // Source map support
    sourcemap: true,
    // Minification
    minify: false,
    // Target
    target: 'node18',
  },
  
  // CSS configuration (not used for Node.js tests)
  css: {
    // CSS configuration
  },
  
  // JSON configuration
  json: {
    // JSON configuration
  },
  
  // Assets configuration
  assetsInclude: [],
  
  // Public directory
  publicDir: false,
  
  // Cache directory
  cacheDir: 'node_modules/.vite',
  
  // Clear screen
  clearScreen: true,
  
  // Log level
  logLevel: 'info',
  
  // Custom logger
  customLogger: undefined,
  
  // Environment variables
  envPrefix: ['VITE_', 'VITEST_'],
  
  // Environment variables
  envDir: process.cwd(),
  
  // Mode
  mode: 'test',
  
  // Command
  command: 'test',
  
  // Is production
  isProduction: false,
  
  // Is preview
  isPreview: false,
  
  // Is test
  isTest: true,
  
  // Is build
  isBuild: false,
  
  // Is serve
  isServe: false,
  
  // Is optimize
  isOptimize: false,
  
  // Is watch
  isWatch: false,
  
  // Is dev
  isDev: false,
});
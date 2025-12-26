import jsdocPlugin from 'eslint-plugin-jsdoc';
import globals from 'globals';
import prettierConfig from 'eslint-config-prettier';

export default [
  {
    // Enable caching for faster incremental linting
    linterOptions: {
      reportUnusedDisableDirectives: true,
    },
  },
  {
    ignores: [
      'dist/**',
      'coverage/**',
      'node_modules/**',
      '.nyc_output/**',
      '.eslintcache',
      '*.config.mjs',
      'build.*.mjs'
    ]
  },
  // Base configuration for all files
  {
    files: ['**/*.mjs', '**/*.js'],
    languageOptions: {
      ecmaVersion: 2022,
      sourceType: 'module',
      parserOptions: {
        ecmaFeatures: {
          jsx: true
        }
      },
      globals: {
        // Node.js core globals
        ...globals.node,
        // Additional Node globals
        __dirname: 'readonly',
        __filename: 'readonly',
        // Vitest test globals
        describe: 'readonly',
        it: 'readonly',
        test: 'readonly',
        expect: 'readonly',
        beforeEach: 'readonly',
        afterEach: 'readonly',
        beforeAll: 'readonly',
        afterAll: 'readonly',
        vi: 'readonly'
      }
    },
    plugins: {
      jsdoc: jsdocPlugin
    },
    rules: {
      // Prettier config - disables ESLint rules that conflict with Prettier
      ...prettierConfig.rules,
      // JSDoc enforcement rules
      'jsdoc/require-jsdoc': ['warn', {
        publicOnly: true,
        require: {
          FunctionDeclaration: true,
          MethodDefinition: true,
          ClassDeclaration: true,
          ArrowFunctionExpression: false,
          FunctionExpression: false
        }
      }],
      'jsdoc/require-param-type': 'off',
      'jsdoc/require-returns-type': 'off',
      'jsdoc/check-types': 'off',
      'jsdoc/valid-types': 'off',

      // Basic code quality rules
      'no-unused-vars': ['warn', { 
        argsIgnorePattern: '^_', 
        varsIgnorePattern: '^_',
        caughtErrors: 'none' // Allow unused error parameters in catch blocks
      }],
      'no-console': 'off',
      'no-debugger': 'warn',
      'no-undef': 'error'
    }
  },
  // Browser-specific files
  {
    files: [
      'src/browser/**/*.mjs',
      'src/react-hooks/**/*.mjs',
      'src/knowledge-engine/browser-shims.mjs',
      'src/knowledge-engine/browser.mjs',
      'src/knowledge-engine/streaming/**/*.mjs',
      'src/security/sandbox/browser-executor.mjs',
      'test/browser/**/*.mjs'
    ],
    languageOptions: {
      parserOptions: {
        ecmaFeatures: {
          jsx: true
        }
      },
      globals: {
        ...globals.browser,
        ...globals.node,
        // Worker API
        Worker: 'readonly',
        // Worker threads (Node.js)
        isMainThread: 'readonly',
        // Vitest globals for browser tests
        describe: 'readonly',
        it: 'readonly',
        test: 'readonly',
        expect: 'readonly',
        beforeEach: 'readonly',
        afterEach: 'readonly',
        beforeAll: 'readonly',
        afterAll: 'readonly',
        vi: 'readonly'
      }
    }
  },
  // Test files that simulate browser environment
  {
    files: [
      'test/**/*.test.mjs',
      'test/**/*.spec.mjs',
      'test/**/*.mjs'
    ],
    languageOptions: {
      globals: {
        ...globals.node,
        ...globals.browser,
        // Vitest globals
        describe: 'readonly',
        it: 'readonly',
        test: 'readonly',
        expect: 'readonly',
        beforeEach: 'readonly',
        afterEach: 'readonly',
        beforeAll: 'readonly',
        afterAll: 'readonly',
        vi: 'readonly',
        // Browser globals for tests
        window: 'readonly',
        document: 'readonly',
        navigator: 'readonly'
      }
    },
    rules: {
      // Prettier config - disables ESLint rules that conflict with Prettier
      ...prettierConfig.rules,
      // Be more lenient with unused vars in tests (demo code, examples)
      'no-unused-vars': ['warn', {
        argsIgnorePattern: '^_',
        varsIgnorePattern: '^_',
        caughtErrors: 'none'
      }],
      // Disable no-undef for test files - helper functions defined in same file
      // Also handles false positives from ESLint parser issues
      'no-undef': 'off'
    }
  },
  // Streaming module index that imports/re-exports
  {
    files: ['src/knowledge-engine/streaming/index.mjs'],
    languageOptions: {
      globals: {
        ...globals.node,
        // Re-exported functions from submodules
        createSubscriptionManager: 'readonly',
        createChangeFeed: 'readonly',
        createStreamProcessor: 'readonly',
        createRealTimeValidator: 'readonly'
      }
    }
  },
  // Example files that may use browser APIs
  {
    files: ['examples/**/*.mjs'],
    languageOptions: {
      globals: {
        ...globals.node,
        ...globals.browser,
        // Additional globals used in examples
        engine: 'readonly',
        rdfCanonize: 'readonly',
        namedNode: 'readonly',
        manager: 'readonly',
        createSubscriptionManager: 'readonly',
        createChangeFeed: 'readonly',
        createStreamProcessor: 'readonly',
        createRealTimeValidator: 'readonly'
      }
    },
    rules: {
      // Prettier config - disables ESLint rules that conflict with Prettier
      ...prettierConfig.rules,
      // Be very lenient with examples - they're demo code
      'no-unused-vars': ['warn', {
        argsIgnorePattern: '^_',
        varsIgnorePattern: '^_',
        caughtErrors: 'none'
      }],
      'no-undef': 'warn' // Be more lenient with examples
    }
  }
];

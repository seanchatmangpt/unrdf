/**
 * ESLint Configuration for Deterministic Template Generation
 *
 * This configuration enforces deterministic behavior in all template-related code
 * to ensure identical outputs across different systems, times, and environments.
 */

module.exports = {
  extends: [
    'eslint:recommended'
  ],

  env: {
    es2022: true,
    node: true
  },

  parserOptions: {
    ecmaVersion: 2022,
    sourceType: 'module'
  },

  rules: {
    // ===== TEMPORAL NONDETERMINISM (CRITICAL) =====

    // Ban new Date() construction
    'no-new-date': 'error',

    // Ban Date.now() usage
    'no-date-now': 'error',

    // Ban Math.random()
    'no-math-random': 'error',

    // Ban crypto.randomBytes and similar
    'no-crypto-random': 'error',

    // Ban UUID v4 generation (nondeterministic)
    'no-uuid-v4': 'error',

    // ===== ENVIRONMENT DEPENDENCIES (CRITICAL) =====

    // Ban process.env in templates (allowed in config files only)
    'no-process-env-in-templates': 'error',

    // Ban system information access
    'no-system-info': 'error',

    // Ban Intl with system locale
    'no-intl-system-locale': 'error',

    // Ban network calls
    'no-network-calls': 'error',

    // ===== DATA STRUCTURE NONDETERMINISM (HIGH) =====

    // Require sorted object key iteration
    'require-object-key-sort': 'error',

    // Ban for...in loops without sorting
    'no-for-in-unsorted': 'error',

    // Require deterministic Set iteration
    'require-set-deterministic-iteration': 'error',

    // Require deterministic Map iteration
    'require-map-deterministic-iteration': 'error',

    // Require stable sorting
    'require-stable-sort': 'error',

    // ===== I/O NONDETERMINISM (HIGH) =====

    // Require sorted directory reading
    'require-readdir-sort': 'error',

    // Ban dynamic imports based on runtime conditions
    'no-dynamic-imports': 'error',

    // Require deterministic glob patterns
    'require-glob-deterministic': 'error',

    // Ban index.js files (implicit file resolution)
    'no-index-files': 'warn',

    // ===== PLATFORM DIFFERENCES (MEDIUM) =====

    // Warn about platform-specific math operations
    'no-platform-specific-math': 'warn',

    // Require explicit floating point precision
    'require-float-precision': 'warn',

    // ===== EXISTING ESLINT RULES =====

    // Enforce deterministic practices with existing rules
    'no-console': 'warn', // Console output can affect determinism
    'prefer-const': 'error',
    'no-var': 'error',
    'no-undef': 'error',
    'no-unused-vars': ['error', { 'argsIgnorePattern': '^_' }],
    'eqeqeq': 'error',
    'curly': 'error',
    'no-eval': 'error',
    'no-implied-eval': 'error',
    'no-new-func': 'error',
    'consistent-return': 'error'
  },

  overrides: [
    {
      // Stricter rules for template files
      files: [
        '**/templates/**/*.js',
        '**/filters/**/*.js',
        '**/generators/**/*.js',
        '**/*.template.js',
        '**/*.filter.js'
      ],
      rules: {
        // Extra strict for template code
        'no-process-env-in-templates': 'error',
        'no-console': 'error',
        'require-object-key-sort': 'error',
        'no-dynamic-imports': 'error'
      }
    },
    {
      // Relaxed rules for config files
      files: [
        '**/config/**/*.js',
        '**/*.config.js',
        '**/build/**/*.js'
      ],
      rules: {
        // Allow process.env in configuration files
        'no-process-env-in-templates': 'off',
        'no-system-info': 'warn'
      }
    },
    {
      // Test files can have some flexibility
      files: [
        '**/tests/**/*.js',
        '**/test/**/*.js',
        '**/*.test.js',
        '**/*.spec.js'
      ],
      rules: {
        'no-console': 'off',
        'no-new-date': 'warn', // Tests might need current time for assertions
        'no-math-random': 'warn' // Tests might need random data
      }
    }
  ],

  // Custom rule definitions (would need to be implemented as ESLint plugins)
  settings: {
    'determinism-rules': {
      // Configuration for custom rules

      'banned-temporal-functions': [
        'new Date',
        'Date.now',
        'Math.random',
        'crypto.randomBytes',
        'crypto.randomUUID',
        'uuidv4',
        'uuid.v4'
      ],

      'banned-environment-access': [
        'process.env',
        'os.hostname',
        'os.platform',
        'os.tmpdir',
        'os.userInfo',
        'process.platform',
        'process.version',
        'process.cwd'
      ],

      'banned-network-functions': [
        'fetch',
        'axios',
        'request',
        'http.get',
        'http.post',
        'https.get',
        'https.post',
        'XMLHttpRequest',
        'WebSocket'
      ],

      'required-deterministic-alternatives': {
        'new Date()': 'getDeterministicTimestamp()',
        'Date.now()': 'getDeterministicTimestamp()',
        'Math.random()': 'generateDeterministicRandom(seed)',
        'Object.keys(obj).forEach': 'Object.keys(obj).sort().forEach',
        'for (key in obj)': 'for (key of Object.keys(obj).sort())',
        'process.env.NODE_ENV': 'config.environment'
      },

      'allowed-exceptions': [
        // Functions that are allowed despite potential nondeterminism
        'console.log', // In development/debug mode only
        'Math.floor', // When not used with random values
        'Math.round', // For deterministic rounding
        'JSON.stringify' // When used with sorted keys
      ]
    }
  }
};

/**
 * Custom Rule Implementations (Pseudo-code)
 * These would need to be implemented as ESLint plugins
 */

/*
// no-new-date
function checkNoNewDate(context) {
  return {
    NewExpression(node) {
      if (node.callee.name === 'Date') {
        // Allow new Date() with literal string arguments only
        if (node.arguments.length === 0 ||
            node.arguments.some(arg => arg.type !== 'Literal')) {
          context.report({
            node,
            message: 'new Date() without literal arguments is nondeterministic. Use getDeterministicTimestamp() instead.'
          });
        }
      }
    }
  };
}

// require-object-key-sort
function checkObjectKeySort(context) {
  return {
    CallExpression(node) {
      if (node.callee.type === 'MemberExpression' &&
          node.callee.object.name === 'Object' &&
          node.callee.property.name === 'keys' &&
          node.parent.type === 'CallExpression' &&
          node.parent.callee.type === 'MemberExpression' &&
          node.parent.callee.property.name === 'forEach') {
        // Check if .sort() is called on Object.keys()
        if (!hasSortCall(node.parent)) {
          context.report({
            node,
            message: 'Object.keys() must be sorted before iteration. Use Object.keys(obj).sort().forEach().'
          });
        }
      }
    }
  };
}

// no-process-env-in-templates
function checkNoProcessEnvInTemplates(context) {
  const filename = context.getFilename();
  const isTemplateFile = /\/(templates|filters|generators)\//.test(filename);

  if (!isTemplateFile) return {};

  return {
    MemberExpression(node) {
      if (node.object.type === 'MemberExpression' &&
          node.object.object.name === 'process' &&
          node.object.property.name === 'env') {
        context.report({
          node,
          message: 'process.env is not allowed in template files. Use config parameters instead.'
        });
      }
    }
  };
}
*/
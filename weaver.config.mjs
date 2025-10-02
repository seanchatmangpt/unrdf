/**
 * @file weaver.config.mjs
 * @description Weaver code generation configuration for UNRDF dashboard
 *
 * This configuration drives the TypeScript → MJS/JSDoc/Zod code generation
 * process for the Nuxt UI dashboard template conversion.
 *
 * @see docs/architecture/weaver-codegen-strategy.md
 */

export default {
  // ============================================================================
  // INPUT SOURCES
  // ============================================================================

  /**
   * Source TypeScript files to convert
   */
  source: {
    // TypeScript type definitions (priority: P0)
    types: {
      path: './app/types/index.d.ts',
      priority: 'P0',
      output: './playground/schemas/index.mjs'
    },

    // Vue components with TypeScript scripts (priority: P1)
    components: {
      path: './app/components/**/*.vue',
      priority: 'P1',
      output: './playground/app/components/',
      exclude: [
        '**/customer/**',    // Skip customer components (low ROI)
        '**/inbox/**',       // Skip inbox components (low ROI)
        '**/settings/**',    // Skip settings components (low ROI)
        '**/HomeChart.*'     // Skip complex chart component (defer)
      ]
    },

    // API routes (priority: P0)
    api: {
      path: './server/api/**/*.ts',
      priority: 'P0',
      output: './playground/server/api/',
      include: [
        'customers.ts',
        'notifications.ts',
        'mails.ts',
        'members.ts'
      ]
    },

    // Composables (priority: P0)
    composables: {
      path: './app/composables/**/*.ts',
      priority: 'P0',
      output: './playground/composables/',
      include: [
        'useDashboard.ts'
      ]
    },

    // Layouts (priority: P0)
    layouts: {
      path: './app/layouts/**/*.vue',
      priority: 'P0',
      output: './playground/app/layouts/'
    },

    // Pages (priority: P1)
    pages: {
      path: './app/pages/**/*.vue',
      priority: 'P1',
      output: './playground/app/pages/',
      include: [
        'index.vue'    // Only home page (80/20)
      ],
      exclude: [
        'customers.vue',
        'inbox.vue',
        'settings.vue'
      ]
    },

    // Configuration files (priority: P0)
    config: {
      files: [
        {
          input: './app.config.ts',
          output: './playground/app.config.mjs',
          priority: 'P0'
        }
      ]
    }
  },

  // ============================================================================
  // OUTPUT CONFIGURATION
  // ============================================================================

  /**
   * Output directories for generated code
   */
  output: {
    // Root output directory
    root: './playground/',

    // Specific output paths
    schemas: './playground/schemas/',
    api: './playground/server/api/',
    composables: './playground/composables/',
    components: './playground/app/components/',
    layouts: './playground/app/layouts/',
    pages: './playground/app/pages/',
    tests: './playground/test/generated/'
  },

  // ============================================================================
  // GENERATION OPTIONS
  // ============================================================================

  /**
   * Code generation features
   */
  options: {
    // Add Zod validation to all generated code
    validation: true,

    // Add JSDoc type annotations
    jsdoc: true,

    // Add OpenTelemetry observability
    otel: true,

    // Generate tests alongside code
    tests: true,

    // Generate OTEL metrics
    metrics: true,

    // Format generated code with Prettier
    format: true,

    // Lint generated code with ESLint
    lint: true,

    // Dry run mode (preview without writing files)
    dryRun: false,

    // Verbose logging
    verbose: true,

    // Overwrite existing files
    overwrite: false,

    // Backup files before overwriting
    backup: true
  },

  // ============================================================================
  // 80/20 PRIORITY CONFIGURATION
  // ============================================================================

  /**
   * High-priority items for 80/20 principle
   * Focus on these items first for maximum value
   */
  priority: {
    // Critical schemas (80% of type safety value)
    schemas: [
      'User',           // Used in 60% of components
      'Notification',   // Used in notifications slideover
      'Stat',          // Used in dashboard stats
      'Sale',          // Used in sales table
      'UserStatus',    // Enum used in User
      'SaleStatus',    // Enum used in Sale
      'Period'         // Enum used in filters
    ],

    // Critical API routes (80% of backend value)
    api: [
      'customers',      // Most frequently used
      'notifications'   // Real-time updates
    ],

    // High-value components (80% of UI value)
    components: [
      'HomeStats',            // Dashboard stats cards
      'HomeSales',            // Sales data table
      'NotificationsSlideover', // Notifications panel
      'TeamsMenu',            // Team selector
      'UserMenu'              // User menu
    ],

    // Essential composables (100% of shared state)
    composables: [
      'useDashboard'     // Only composable, controls all navigation
    ],

    // Core layouts (100% of layout structure)
    layouts: [
      'default'          // Main dashboard layout
    ],

    // Essential pages (80% of usage)
    pages: [
      'index'            // Home page with stats/charts
    ]
  },

  // ============================================================================
  // TEMPLATE CONFIGURATION
  // ============================================================================

  /**
   * Handlebars template paths
   */
  templates: {
    // Schema generation templates
    schemas: {
      path: './templates/schemas/',
      files: {
        enum: 'enum.mjs.hbs',
        object: 'object.mjs.hbs',
        array: 'array.mjs.hbs',
        union: 'union.mjs.hbs',
        index: 'index.mjs.hbs'
      }
    },

    // API route generation templates
    api: {
      path: './templates/api/',
      files: {
        get: 'get.mjs.hbs',
        post: 'post.mjs.hbs',
        eventHandler: 'event-handler.mjs.hbs',
        otel: 'otel-wrapper.mjs.hbs'
      }
    },

    // Composable generation templates
    composables: {
      path: './templates/composables/',
      files: {
        use: 'use-composable.mjs.hbs',
        shared: 'shared-composable.mjs.hbs'
      }
    },

    // Vue component generation templates
    components: {
      path: './templates/components/',
      files: {
        scriptSetup: 'script-setup.hbs',
        jsdocHeader: 'jsdoc-header.hbs',
        typeImports: 'type-imports.hbs',
        refs: 'refs.hbs',
        computed: 'computed.hbs',
        methods: 'methods.hbs'
      }
    },

    // Configuration file templates
    config: {
      path: './templates/config/',
      files: {
        appConfig: 'app.config.mjs.hbs',
        nuxtConfig: 'nuxt.config.mjs.hbs'
      }
    },

    // Test generation templates
    tests: {
      path: './templates/tests/',
      files: {
        schema: 'schema.test.mjs.hbs',
        api: 'api.test.mjs.hbs',
        composable: 'composable.test.mjs.hbs',
        component: 'component.test.mjs.hbs'
      }
    }
  },

  // ============================================================================
  // TYPE MAPPING CONFIGURATION
  // ============================================================================

  /**
   * TypeScript → Zod type mappings
   */
  typeMapping: {
    // Primitive types
    string: 'z.string()',
    number: 'z.number()',
    boolean: 'z.boolean()',
    Date: 'z.date()',
    unknown: 'z.unknown()',
    any: 'z.any()',
    void: 'z.void()',
    null: 'z.null()',
    undefined: 'z.undefined()',

    // Array types
    'string[]': 'z.array(z.string())',
    'number[]': 'z.array(z.number())',
    'Array<T>': 'z.array({{T}})',

    // Object types
    object: 'z.object({})',
    Record: 'z.record({{K}}, {{V}})',

    // Utility types
    'Partial<T>': '{{T}}.partial()',
    'Required<T>': '{{T}}.required()',
    'Pick<T, K>': '{{T}}.pick({{K}})',
    'Omit<T, K>': '{{T}}.omit({{K}})',

    // Special patterns
    email: 'z.string().email()',
    url: 'z.string().url()',
    uuid: 'z.string().uuid()',
    datetime: 'z.string().datetime()',
    positiveInt: 'z.number().int().positive()',
    nonNegative: 'z.number().nonnegative()'
  },

  // ============================================================================
  // VALIDATION CONFIGURATION
  // ============================================================================

  /**
   * Code validation rules
   */
  validation: {
    // Syntax validation
    syntax: {
      enabled: true,
      parser: 'espree',
      ecmaVersion: 2022,
      sourceType: 'module'
    },

    // Zod validation
    zod: {
      enabled: true,
      strictMode: true,
      warnOnUnknown: true
    },

    // JSDoc validation
    jsdoc: {
      enabled: true,
      requireDescriptions: true,
      requireParamDescriptions: true,
      requireReturnType: true
    },

    // ESLint validation
    eslint: {
      enabled: true,
      config: '.eslintrc.json',
      fix: true
    },

    // Prettier validation
    prettier: {
      enabled: true,
      config: '.prettierrc.json'
    }
  },

  // ============================================================================
  // OTEL CONFIGURATION
  // ============================================================================

  /**
   * OpenTelemetry observability configuration
   */
  otel: {
    // Enable OTEL instrumentation
    enabled: true,

    // Service name for traces
    serviceName: 'unrdf-playground',

    // Instrument API routes
    api: {
      enabled: true,
      recordErrors: true,
      recordLatency: true,
      sampleRate: 1.0
    },

    // Instrument composables
    composables: {
      enabled: false  // Too noisy for client-side
    },

    // Custom attributes
    attributes: {
      'service.version': '2.1.1',
      'deployment.environment': 'development',
      'code.generator': 'weaver'
    }
  },

  // ============================================================================
  // TEST GENERATION CONFIGURATION
  // ============================================================================

  /**
   * Test generation options
   */
  testing: {
    // Enable test generation
    enabled: true,

    // Test framework
    framework: 'vitest',

    // Test file naming pattern
    pattern: '{{name}}.test.mjs',

    // Generate tests for schemas
    schemas: {
      enabled: true,
      testValid: true,
      testInvalid: true,
      coverage: 0.9
    },

    // Generate tests for API routes
    api: {
      enabled: true,
      testEndpoints: true,
      testValidation: true,
      coverage: 0.9
    },

    // Generate tests for composables
    composables: {
      enabled: true,
      testMethods: true,
      testState: true,
      coverage: 0.9
    },

    // Generate tests for components
    components: {
      enabled: true,
      testRendering: true,
      testEvents: true,
      coverage: 0.8
    }
  },

  // ============================================================================
  // PERFORMANCE CONFIGURATION
  // ============================================================================

  /**
   * Performance optimization settings
   */
  performance: {
    // Enable parallel processing
    parallel: true,

    // Max concurrent tasks
    concurrency: 4,

    // Cache template compilations
    cache: {
      enabled: true,
      ttl: 3600000  // 1 hour
    },

    // Incremental generation
    incremental: true,

    // Watch mode for development
    watch: false
  },

  // ============================================================================
  // CLI CONFIGURATION
  // ============================================================================

  /**
   * CLI command configuration
   */
  cli: {
    // Command prefix
    prefix: 'weaver',

    // Available commands
    commands: {
      'generate:schemas': {
        description: 'Generate Zod schemas from TypeScript types',
        priority: 'P0',
        phase: 1
      },
      'generate:api': {
        description: 'Generate API routes from TypeScript',
        priority: 'P0',
        phase: 1
      },
      'generate:composables': {
        description: 'Generate composables from TypeScript',
        priority: 'P0',
        phase: 1
      },
      'generate:components': {
        description: 'Generate Vue components from TypeScript',
        priority: 'P1',
        phase: 2
      },
      'generate:all': {
        description: 'Generate all code (80/20 priority)',
        priority: 'P0',
        phase: 'all'
      },
      'validate': {
        description: 'Validate generated code',
        priority: 'P0',
        phase: 'all'
      },
      'test': {
        description: 'Run tests on generated code',
        priority: 'P0',
        phase: 'all'
      },
      'clean': {
        description: 'Clean generated code',
        priority: 'P2',
        phase: 'all'
      }
    }
  },

  // ============================================================================
  // HOOKS CONFIGURATION
  // ============================================================================

  /**
   * Lifecycle hooks for generation process
   */
  hooks: {
    // Before generation starts
    beforeGenerate: async (context) => {
      console.log(`🚀 Starting code generation: ${context.target}`)
    },

    // After generation completes
    afterGenerate: async (context) => {
      console.log(`✅ Generation complete: ${context.files.length} files`)
    },

    // Before validation
    beforeValidate: async (context) => {
      console.log(`🔍 Validating: ${context.files.length} files`)
    },

    // After validation
    afterValidate: async (context) => {
      console.log(`✅ Validation complete: ${context.passed}/${context.total}`)
    },

    // On error
    onError: async (error, context) => {
      console.error(`❌ Error in ${context.phase}:`, error.message)
    }
  },

  // ============================================================================
  // METRICS CONFIGURATION
  // ============================================================================

  /**
   * Metrics tracking configuration
   */
  metrics: {
    // Enable metrics collection
    enabled: true,

    // Metrics to track
    track: [
      'generation_time',
      'files_generated',
      'lines_of_code',
      'template_compile_time',
      'validation_time',
      'test_coverage',
      'error_count'
    ],

    // Export metrics
    export: {
      enabled: true,
      format: 'json',
      path: './playground/weaver-metrics.json'
    }
  }
}

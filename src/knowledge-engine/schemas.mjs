/**
 * @file Zod schemas for knowledge hook validation
 * @module schemas
 *
 * @description
 * Comprehensive Zod schemas for validating all knowledge hook components
 * including hook definitions, conditions, events, and execution results.
 */

import { z } from 'zod';

/**
 * Schema for hook metadata
 */
export const HookMetaSchema = z.object({
  name: z
    .string()
    .min(1)
    .max(100)
    .regex(
      /^[a-zA-Z0-9:_-]+$/,
      'Name must contain only alphanumeric characters, colons, hyphens, and underscores'
    ),
  description: z.string().min(1).max(500).optional(),
  version: z
    .string()
    .regex(/^\d+\.\d+\.\d+$/, 'Version must be semantic version format')
    .optional(),
  author: z.string().min(1).max(100).optional(),
  tags: z.array(z.string().min(1).max(50)).max(10).optional(),
  ontology: z.array(z.string().min(1).max(50)).max(10).optional(),
  createdAt: z.coerce.date().optional(),
  updatedAt: z.coerce.date().optional(),
});

/**
 * Schema for content-addressed file references
 */
export const FileRefSchema = z.object({
  uri: z
    .string()
    .url({ message: 'Must be a valid URI' })
    .or(z.string().regex(/^file:\/\/.+/, 'Must be a valid file URI')),
  sha256: z
    .string()
    .length(64)
    .regex(/^[a-f0-9]+$/, 'Must be a valid SHA-256 hash')
    .optional(),
  mediaType: z.string().min(1).max(100).optional(),
  size: z.number().int().positive().optional(),
  lastModified: z.coerce.date().optional(),
});

/**
 * Schema for SPARQL ASK conditions
 * Supports EITHER file reference (ref) OR inline query (query) for convenience
 */
export const SparqlAskConditionSchema = z.object({
  kind: z.literal('sparql-ask'),
  ref: FileRefSchema.optional(),
  query: z.string().min(1).optional(),
  options: z
    .object({
      timeout: z.number().int().positive().max(30000).optional(),
      strict: z.boolean().optional(),
      variables: z.record(z.string()).optional(),
    })
    .optional(),
});

/**
 * Schema for SPARQL SELECT conditions
 * Supports EITHER file reference (ref) OR inline query (query) for convenience
 */
export const SparqlSelectConditionSchema = z.object({
  kind: z.literal('sparql-select'),
  ref: FileRefSchema.optional(),
  query: z.string().min(1).optional(),
  options: z
    .object({
      timeout: z.number().int().positive().max(30000).optional(),
      limit: z.number().int().positive().max(10000).optional(),
      offset: z.number().int().nonnegative().optional(),
      strict: z.boolean().optional(),
      variables: z.record(z.string()).optional(),
    })
    .optional(),
});

/**
 * Schema for SHACL validation conditions
 * Supports EITHER file reference (ref) OR inline shapes (shapes) for convenience
 */
export const ShaclConditionSchema = z.object({
  kind: z.literal('shacl'),
  ref: FileRefSchema.optional(),
  shapes: z.string().min(1).optional(), // Inline Turtle shapes for convenience
  options: z
    .object({
      strict: z.boolean().optional(),
      includeDetails: z.boolean().optional(),
      maxViolations: z.number().int().positive().max(1000).optional(),
      shapesGraph: z.string().url({ message: 'Must be a valid URL' }).optional(),
    })
    .optional(),
});

/**
 * Schema for DELTA predicate conditions
 */
export const DeltaConditionSchema = z.object({
  kind: z.literal('delta'),
  spec: z.object({
    change: z.enum(['any', 'increase', 'decrease', 'modify']),
    key: z.array(z.string()).min(1),
    threshold: z.number().min(0).max(1).optional(),
    baseline: z.string().optional(),
  }),
  options: z
    .object({
      timeout: z.number().int().positive().max(30000).optional(),
      strict: z.boolean().optional(),
    })
    .optional(),
});

/**
 * Schema for THRESHOLD predicate conditions
 */
export const ThresholdConditionSchema = z.object({
  kind: z.literal('threshold'),
  spec: z.object({
    var: z.string().min(1),
    op: z.enum(['>', '>=', '<', '<=', '==', '!=']),
    value: z.number(),
    aggregate: z.enum(['sum', 'avg', 'min', 'max', 'count']).optional(),
  }),
  options: z
    .object({
      timeout: z.number().int().positive().max(30000).optional(),
      strict: z.boolean().optional(),
    })
    .optional(),
});

/**
 * Schema for COUNT predicate conditions
 */
export const CountConditionSchema = z.object({
  kind: z.literal('count'),
  spec: z.object({
    op: z.enum(['>', '>=', '<', '<=', '==', '!=']),
    value: z.number().int().nonnegative(),
    query: z.string().optional(), // SPARQL query for counting
  }),
  options: z
    .object({
      timeout: z.number().int().positive().max(30000).optional(),
      strict: z.boolean().optional(),
    })
    .optional(),
});

/**
 * Schema for WINDOW predicate conditions
 */
export const WindowConditionSchema = z.object({
  kind: z.literal('window'),
  spec: z.object({
    size: z.number().int().positive(), // Window size in milliseconds
    slide: z.number().int().positive().optional(), // Slide interval
    aggregate: z.enum(['sum', 'avg', 'min', 'max', 'count']),
    query: z.string().optional(), // SPARQL query for windowing
  }),
  options: z
    .object({
      timeout: z.number().int().positive().max(30000).optional(),
      strict: z.boolean().optional(),
    })
    .optional(),
});

/**
 * Union schema for all condition types
 */
export const ConditionSchema = z.discriminatedUnion('kind', [
  SparqlAskConditionSchema,
  SparqlSelectConditionSchema,
  ShaclConditionSchema,
  DeltaConditionSchema,
  ThresholdConditionSchema,
  CountConditionSchema,
  WindowConditionSchema,
]);

/**
 * Schema for determinism configuration
 */
export const DeterminismSchema = z.object({
  seed: z.number().int().nonnegative().max(2147483647).default(42),
  algorithm: z.enum(['lcg', 'xorshift', 'mersenne']).default('lcg'),
  salt: z.string().min(1).max(100).optional(),
});

/**
 * Schema for receipt configuration
 */
export const ReceiptSchema = z.object({
  anchor: z
    .enum(['none', 'blockchain', 'merkle', 'timestamp', 'hash', 'git-notes'])
    .default('none'),
  format: z.enum(['json', 'jsonld', 'rdf', 'turtle']).default('json'),
  includeProof: z.boolean().default(false),
  ttl: z.number().int().positive().max(86400).optional(), // Time to live in seconds
});

/**
 * Schema for hook execution context
 */
export const HookContextSchema = z.object({
  graph: z.any(), // RDF Store instance - validated at runtime
  env: z.record(z.any()).optional(),
  metadata: z.record(z.any()).optional(),
  transactionId: z.string().uuid({ message: 'Must be a valid UUID' }).optional(),
  timestamp: z.coerce.date().optional(),
});

/**
 * Schema for hook events
 */
export const HookEventSchema = z.object({
  name: z.string().min(1).max(100),
  payload: z.any(), // Flexible payload structure
  context: HookContextSchema,
  id: z.string().uuid({ message: 'Must be a valid UUID' }).optional(),
  timestamp: z.coerce.date().optional(),
  source: z.string().min(1).max(100).optional(),
  correlationId: z.string().uuid({ message: 'Must be a valid UUID' }).optional(),
});

/**
 * Schema for hook execution results
 */
export const HookResultSchema = z.object({
  success: z.boolean(),
  result: z.any().optional(),
  error: z.string().optional(),
  duration: z.number().nonnegative().optional(),
  phase: z.enum(['before', 'run', 'after', 'completed', 'failed']).optional(),
  cancelled: z.boolean().default(false),
  metadata: z.record(z.any()).optional(),
  assertions: z
    .array(
      z.object({
        subject: z.string(),
        predicate: z.string(),
        object: z.string(),
        graph: z.string().optional(),
      })
    )
    .optional(),
});

/**
 * Schema for hook channel configuration
 */
export const HookChannelSchema = z.object({
  graphs: z.array(z.string()).optional(),
  view: z.enum(['before', 'after', 'delta']).optional(),
});

/**
 * Schema for complete knowledge hook definition
 */
export const KnowledgeHookSchema = z.object({
  meta: HookMetaSchema,
  channel: HookChannelSchema.optional(),
  when: ConditionSchema,
  run: z.function(),
  before: z.function().optional(),
  after: z.function().optional(),
  determinism: DeterminismSchema.optional(),
  receipt: ReceiptSchema.optional(),
  timeout: z.number().int().positive().max(300000).optional(), // 5 minutes max
  retries: z.number().int().nonnegative().max(5).optional(),
  priority: z.number().int().min(0).max(100).default(50).optional(),
});

/**
 * Schema for transaction delta
 */
export const TransactionDeltaSchema = z.object({
  additions: z.array(z.any()).default([]), // RDF Quad array
  removals: z.array(z.any()).default([]), // RDF Quad array
  metadata: z.record(z.any()).optional(),
  id: z.string().optional(),
  timestamp: z.coerce.date().optional(),
});

/**
 * Schema for transaction receipt
 */
export const TransactionReceiptSchema = z.object({
  committed: z.boolean(),
  delta: TransactionDeltaSchema,
  hookResults: z
    .array(
      z.object({
        hookId: z.string(),
        result: z.boolean(),
        error: z.string().optional(),
        duration: z.number().nonnegative().optional(),
      })
    )
    .default([]),
  beforeHash: z
    .object({
      sha3: z.string(),
      blake3: z.string(),
    })
    .optional(),
  afterHash: z
    .object({
      sha3: z.string(),
      blake3: z.string(),
    })
    .optional(),
  timestamp: z.coerce.date(),
  duration: z.number().nonnegative(),
  knowledgeHookResults: z.number().int().nonnegative().default(0),
});

/**
 * Schema for OpenTelemetry configuration
 */
export const ObservabilityConfigSchema = z.object({
  enableTracing: z.boolean().default(true),
  enableMetrics: z.boolean().default(true),
  enableLogging: z.boolean().default(true),
  serviceName: z
    .string()
    .min(1)
    .max(100)
    .regex(/^[a-zA-Z0-9._-]+$/)
    .default('unrdf-kgc'),
  serviceVersion: z.string().min(1).max(50).default('1.0.0'),
  endpoint: z.string().url().optional(),
  headers: z.record(z.string()).optional(),
  resourceAttributes: z.record(z.string()).optional(),
  samplingRatio: z.number().min(0).max(1).default(1.0),
  maxQueueSize: z.number().int().positive().default(2048),
  maxExportBatchSize: z.number().int().positive().default(512),
  exportTimeoutMillis: z.number().int().positive().default(30000),
  scheduledDelayMillis: z.number().int().positive().default(5000),
  // Optional: advertise the maximum cache size used by the runtime for metrics
  cacheMaxSize: z.number().int().nonnegative().optional(),
  // Smoothing to reduce false-positive alerts from low samples/spikes
  minSamples: z.number().int().positive().default(20),
  ewmaAlpha: z.number().min(0).max(1).default(0.3),
});

/**
 * Schema for performance metrics
 */
export const PerformanceMetricsSchema = z.object({
  transactionLatency: z.object({
    p50: z.number().nonnegative(),
    p95: z.number().nonnegative(),
    p99: z.number().nonnegative(),
    max: z.number().nonnegative(),
  }),
  hookExecutionRate: z.number().nonnegative(), // hooks per minute
  errorRate: z.number().min(0).max(1),
  memoryUsage: z.object({
    rss: z.number().nonnegative(),
    heapUsed: z.number().nonnegative(),
    heapTotal: z.number().nonnegative(),
    external: z.number().nonnegative(),
  }),
  cacheStats: z.object({
    hitRate: z.number().min(0).max(1),
    size: z.number().nonnegative(),
    maxSize: z.number().nonnegative(),
  }),
  backpressure: z.object({
    queueDepth: z.number().nonnegative(),
    watermarks: z.object({
      high: z.number().nonnegative(),
      low: z.number().nonnegative(),
    }),
  }),
});

/**
 * Schema for manager configuration
 */
export const ManagerConfigSchema = z.object({
  basePath: z.string().min(1).default(process.cwd()),
  strictMode: z.boolean().default(false),
  enableConditionEvaluation: z.boolean().default(true),
  maxHooks: z.number().int().positive().max(1000).default(100),
  timeout: z.number().int().positive().max(300000).default(30000),
  enableCache: z.boolean().default(true),
  cacheMaxAge: z.number().int().positive().max(3600000).default(300000), // 5 minutes
  enableMetrics: z.boolean().default(true),
  logLevel: z.enum(['error', 'warn', 'info', 'debug']).default('info'),
  observability: ObservabilityConfigSchema.optional(),
  performance: z
    .object({
      enableProfiling: z.boolean().default(false),
      maxConcurrency: z.number().int().positive().default(10),
      afterHashOnly: z.boolean().default(false), // KGC PRD fast path
      enableCache: z.boolean().default(true),
      timeoutMs: z.number().int().positive().default(2000), // KGC PRD: p99 ≤ 2ms
      maxHooks: z.number().int().positive().default(10000), // KGC PRD: 10k exec/min
    })
    .optional(),
});

/**
 * Schema for file resolver configuration
 */
export const FileResolverConfigSchema = z.object({
  basePath: z.string().min(1).default(process.cwd()),
  enableCache: z.boolean().default(true),
  cacheMaxAge: z.number().int().positive().max(3600000).default(300000),
  maxFileSize: z.number().int().positive().max(10485760).default(1048576), // 1MB default
  allowedMediaTypes: z
    .array(z.string())
    .default([
      'application/sparql-query',
      'text/turtle',
      'application/rdf+xml',
      'application/ld+json',
    ]),
  timeout: z.number().int().positive().max(30000).default(5000),
});

/**
 * Schema for condition evaluator configuration
 */
export const ConditionEvaluatorConfigSchema = z.object({
  enableCache: z.boolean().default(true),
  cacheMaxAge: z.number().int().positive().max(3600000).default(300000),
  timeout: z.number().int().positive().max(30000).default(10000),
  maxConcurrent: z.number().int().positive().max(100).default(10),
  retries: z.number().int().nonnegative().max(3).default(1),
  strict: z.boolean().default(false),
});

/**
 * Schema for hook executor configuration
 */
export const HookExecutorConfigSchema = z.object({
  timeout: z.number().int().positive().max(300000).default(30000),
  maxConcurrent: z.number().int().positive().max(100).default(10),
  retries: z.number().int().nonnegative().max(3).default(1),
  enableMetrics: z.boolean().default(true),
  strict: z.boolean().default(false),
  enableAssertions: z.boolean().default(true),
});

/**
 * Validation functions
 */

/**
 * Validate a knowledge hook definition
 * @param {any} hook - The hook definition to validate
 * @returns {Object} Validation result
 */
export function validateKnowledgeHook(hook) {
  try {
    const validated = KnowledgeHookSchema.parse(hook);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: (error.issues || error.errors || []).map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown',
          received: err.received,
          expected: err.expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a hook event
 * @param {any} event - The event to validate
 * @returns {Object} Validation result
 */
export function validateHookEvent(event) {
  try {
    const validated = HookEventSchema.parse(event);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: (error.issues || error.errors || []).map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown',
          received: err.received,
          expected: err.expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a condition
 * @param {any} condition - The condition to validate
 * @returns {Object} Validation result
 */
export function validateCondition(condition) {
  try {
    const validated = ConditionSchema.parse(condition);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: (error.issues || error.errors || []).map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown',
          received: err.received,
          expected: err.expected,
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate manager configuration
 * @param {any} config - The configuration to validate
 * @returns {Object} Validation result
 */
export function validateManagerConfig(config) {
  try {
    const validated = ManagerConfigSchema.parse(config);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors:
          error.errors?.map(err => ({
            path: err.path?.join('.') || 'unknown',
            message: err.message || 'Unknown error',
            code: err.code || 'unknown',
          })) || [],
      };
    }
    throw error;
  }
}

/**
 * Validate transaction delta
 * @param {any} delta - The delta to validate
 * @returns {Object} Validation result
 */
export function validateTransactionDelta(delta) {
  try {
    const validated = TransactionDeltaSchema.parse(delta);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors:
          error.errors?.map(err => ({
            path: err.path?.join('.') || 'unknown',
            message: err.message || 'Unknown error',
            code: err.code || 'unknown',
          })) || [],
      };
    }
    throw error;
  }
}

/**
 * Type-safe hook definition creator
 * @param {any} definition - The hook definition
 * @returns {Object} Validated and frozen hook definition
 */
export function createKnowledgeHook(definition) {
  try {
    const validation = validateKnowledgeHook(definition);

    if (!validation.success) {
      console.error('Validation errors:', validation.errors);
      console.error('Definition:', definition);
      const errorMessages =
        validation.errors
          ?.map(err => {
            let msg = `${err.path}: ${err.message}`;
            if (err.received !== undefined) {
              msg += ` (received: ${JSON.stringify(err.received)})`;
            }
            if (err.expected !== undefined) {
              msg += ` (expected: ${err.expected})`;
            }
            return msg;
          })
          .join(', ') || 'Unknown validation error';
      throw new TypeError(`Invalid knowledge hook definition: ${errorMessages}`);
    }

    // Apply defaults and freeze
    const normalized = {
      ...validation.data,
      determinism: { seed: 42, ...validation.data.determinism },
      receipt: { anchor: 'none', ...validation.data.receipt },
      priority: validation.data.priority ?? 50,
    };

    return Object.freeze(normalized);
  } catch (error) {
    if (error instanceof TypeError) {
      throw error;
    }
    console.error('Unexpected error in createKnowledgeHook:', error);
    throw new TypeError(`Invalid knowledge hook definition: ${error.message}`);
  }
}

/**
 * Type-safe event creator
 * @param {any} event - The event definition
 * @returns {Object} Validated event
 */
export function createHookEvent(event) {
  const validation = validateHookEvent(event);

  if (!validation.success) {
    const errorMessages =
      validation.errors
        ?.map(err => {
          let msg = `${err.path}: ${err.message}`;
          if (err.received !== undefined) {
            msg += ` (received: ${JSON.stringify(err.received)})`;
          }
          if (err.expected !== undefined) {
            msg += ` (expected: ${err.expected})`;
          }
          return msg;
        })
        .join(', ') || 'Unknown validation error';
    throw new TypeError(`Invalid hook event: ${errorMessages}`);
  }

  // Apply defaults
  const normalized = {
    ...validation.data,
    id: validation.data.id ?? crypto.randomUUID(),
    timestamp: validation.data.timestamp ?? new Date().toISOString(),
  };

  return normalized;
}

/**
 * Type-safe condition creator
 * @param {any} condition - The condition definition
 * @returns {Object} Validated condition
 */
export function createCondition(condition) {
  const validation = validateCondition(condition);

  if (!validation.success) {
    const errorMessages =
      validation.errors
        ?.map(err => {
          let msg = `${err.path}: ${err.message}`;
          if (err.received !== undefined) {
            msg += ` (received: ${JSON.stringify(err.received)})`;
          }
          if (err.expected !== undefined) {
            msg += ` (expected: ${err.expected})`;
          }
          return msg;
        })
        .join(', ') || 'Unknown validation error';
    throw new TypeError(`Invalid condition: ${errorMessages}`);
  }

  return validation.data;
}

// ========================================
// Consolidated Schemas from Other Files
// ========================================

/**
 * Schema for RDF quads (from transaction.mjs)
 */
export const QuadSchema = z
  .object({
    subject: z.any(), // RDF/JS Term
    predicate: z.any(), // RDF/JS Term
    object: z.any(), // RDF/JS Term
    graph: z.any().optional(), // RDF/JS Term
    equals: z.function().optional(),
  })
  .passthrough(); // Allow additional properties for RDF/JS Quad objects

/**
 * Schema for delta (from transaction.mjs)
 */
export const DeltaSchema = z.object({
  additions: z.array(QuadSchema),
  removals: z.array(QuadSchema),
});

/**
 * Schema for transaction hooks (from transaction.mjs)
 */
export const TransactionHookSchema = z.object({
  id: z.string().min(1),
  mode: z.enum(['pre', 'post']),
  condition: z.function(),
  effect: z.union([z.literal('veto'), z.function()]),
});

/**
 * Schema for hook results (from transaction.mjs)
 */
export const TransactionHookResultSchema = z.object({
  hookId: z.string(),
  mode: z.enum(['pre', 'post']),
  result: z.boolean(),
  error: z.string().optional(),
});

/**
 * Schema for hash values (from transaction.mjs)
 */
export const HashSchema = z.object({
  sha3: z.string(),
  blake3: z.string(),
});

/**
 * Schema for transaction receipt (from transaction.mjs)
 */
export const TransactionReceiptSchemaNew = z.object({
  id: z.string(),
  delta: DeltaSchema,
  committed: z.boolean(),
  hookResults: z.array(TransactionHookResultSchema),
  beforeHash: HashSchema,
  afterHash: HashSchema,
  timestamp: z.number(),
  durationMs: z.number(),
  actor: z.string().optional(),
  hookErrors: z.array(z.string()),
  error: z.string().optional(),
});

/**
 * Schema for transaction options (from transaction.mjs)
 */
export const TransactionOptionsSchema = z.object({
  skipHooks: z.boolean().default(false),
  timeoutMs: z.number().positive().default(30000),
  actor: z.string().optional(),
});

/**
 * Schema for manager options (from transaction.mjs)
 */
export const ManagerOptionsSchema = z.object({
  basePath: z.string().min(1).default(process.cwd()),
  strictMode: z.boolean().default(false),
  enableConditionEvaluation: z.boolean().default(true),
  maxHooks: z.number().int().positive().max(1000).default(100),
  timeout: z.number().int().positive().max(300000).default(30000),
  enableCache: z.boolean().default(true),
  cacheMaxAge: z.number().int().positive().max(3600000).default(300000), // 5 minutes
  enableMetrics: z.boolean().default(true),
  logLevel: z.enum(['error', 'warn', 'info', 'debug']).default('info'),
});

/**
 * Schema for query plans (from query-optimizer.mjs)
 */
export const QueryPlanSchema = z.object({
  id: z.string(),
  query: z.string(),
  type: z.enum(['sparql-ask', 'sparql-select', 'shacl']),
  hash: z.string(),
  plan: z.object({
    operations: z.array(
      z.object({
        type: z.string(),
        cost: z.number(),
        selectivity: z.number(),
        dependencies: z.array(z.string()).optional(),
      })
    ),
    estimatedCost: z.number(),
    estimatedRows: z.number(),
    indexes: z.array(z.string()).optional(),
  }),
  createdAt: z.number(),
  lastUsed: z.number(),
  hitCount: z.number().default(0),
});

/**
 * Schema for index definitions (from query-optimizer.mjs)
 */
export const IndexSchema = z.object({
  id: z.string(),
  name: z.string(),
  type: z.enum(['predicate', 'subject', 'object', 'graph', 'composite']),
  fields: z.array(z.string()),
  selectivity: z.number().min(0).max(1),
  size: z.number().nonnegative(),
  createdAt: z.number(),
  lastUpdated: z.number(),
});

/**
 * Schema for delta-aware evaluation context (from query-optimizer.mjs)
 */
export const DeltaAwareContextSchema = z.object({
  delta: z.object({
    additions: z.array(z.any()),
    removals: z.array(z.any()),
  }),
  affectedSubjects: z.set(z.string()).optional(),
  affectedPredicates: z.set(z.string()).optional(),
  affectedObjects: z.set(z.string()).optional(),
  affectedGraphs: z.set(z.string()).optional(),
});

/**
 * Schema for agent proposals (from resolution-layer.mjs)
 */
export const AgentProposalSchema = z.object({
  id: z.string().uuid(),
  agentId: z.string().min(1),
  delta: z.object({
    additions: z.array(z.any()),
    removals: z.array(z.any()),
    metadata: z.record(z.any()).optional(),
  }),
  confidence: z.number().min(0).max(1),
  priority: z.number().int().min(0).max(100).default(50),
  timestamp: z.number(),
  metadata: z.record(z.any()).optional(),
  dependencies: z.array(z.string()).optional(),
  conflicts: z.array(z.string()).optional(),
});

/**
 * Schema for resolution strategies (from resolution-layer.mjs)
 */
export const ResolutionStrategySchema = z.object({
  type: z.enum(['voting', 'merging', 'crdt', 'consensus', 'priority', 'random']),
  parameters: z.record(z.any()).optional(),
  timeout: z.number().int().positive().max(300000).default(30000),
  quorum: z.number().min(0).max(1).default(0.5),
  maxRetries: z.number().int().nonnegative().max(10).default(3),
});

/**
 * Schema for resolution results (from resolution-layer.mjs)
 */
export const ResolutionResultSchema = z.object({
  id: z.string().uuid(),
  strategy: z.string(),
  proposals: z.array(AgentProposalSchema),
  resolvedDelta: z.object({
    additions: z.array(z.any()),
    removals: z.array(z.any()),
    metadata: z.record(z.any()).optional(),
  }),
  confidence: z.number().min(0).max(1),
  consensus: z.boolean(),
  conflicts: z
    .array(
      z.object({
        type: z.enum(['addition', 'removal', 'metadata']),
        proposals: z.array(z.string()),
        resolution: z.string(),
      })
    )
    .optional(),
  timestamp: z.number(),
  duration: z.number().nonnegative(),
});

/**
 * Schema for sandbox configuration (from effect-sandbox.mjs)
 */
export const SandboxConfigSchema = z.object({
  type: z.enum(['vm2', 'worker', 'isolate']).default('worker'),
  timeout: z.number().int().positive().max(300000).default(30000),
  memoryLimit: z
    .number()
    .int()
    .positive()
    .max(1024 * 1024 * 1024)
    .default(64 * 1024 * 1024), // 64MB
  cpuLimit: z.number().int().positive().max(100).default(50), // 50% CPU
  allowedModules: z.array(z.string()).default([]),
  allowedGlobals: z.array(z.string()).default(['console', 'Date', 'Math', 'JSON']),
  enableNetwork: z.boolean().default(false),
  enableFileSystem: z.boolean().default(false),
  enableProcess: z.boolean().default(false),
  strictMode: z.boolean().default(true),
});

/**
 * Schema for sandbox execution context (from effect-sandbox.mjs)
 */
export const SandboxContextSchema = z.object({
  event: z.any(),
  store: z.any(),
  delta: z.any(),
  metadata: z.record(z.any()).optional(),
  allowedFunctions: z.array(z.string()).default(['emitEvent', 'log', 'assert']),
});

/**
 * Schema for sandbox execution result (from effect-sandbox.mjs)
 */
export const SandboxResultSchema = z.object({
  success: z.boolean(),
  result: z.any().optional(),
  error: z.string().optional(),
  duration: z.number().nonnegative(),
  memoryUsed: z.number().nonnegative().optional(),
  cpuUsed: z.number().nonnegative().optional(),
  assertions: z
    .array(
      z.object({
        subject: z.string(),
        predicate: z.string(),
        object: z.string(),
        graph: z.string().optional(),
      })
    )
    .optional(),
  events: z.array(z.any()).optional(),
});

/**
 * Schema for lockchain entries (from lockchain-writer.mjs)
 */
export const LockchainEntrySchema = z.object({
  id: z.string().uuid(),
  timestamp: z.number(),
  receipt: z.any(), // Transaction receipt
  signature: z.object({
    algorithm: z.string(),
    value: z.string(),
    publicKey: z.string().optional(),
  }),
  previousHash: z.string().optional().nullable(),
  merkleRoot: z.string().optional(),
  gitCommit: z.string().optional(),
  gitRef: z.string().optional(),
});

/**
 * Schema for lockchain configuration (from lockchain-writer.mjs)
 */
export const LockchainConfigSchema = z.object({
  gitRepo: z.string().default(process.cwd()),
  refName: z.string().default('refs/notes/lockchain'),
  signingKey: z.string().optional(),
  algorithm: z.enum(['ed25519', 'ecdsa', 'rsa']).default('ed25519'),
  batchSize: z.number().int().positive().default(10),
  enableMerkle: z.boolean().default(true),
  enableGitAnchoring: z.boolean().default(true),
  storagePath: z.string().optional(),
});

/**
 * Schema for policy pack metadata (from policy-pack.mjs)
 */
export const PolicyPackMetaSchema = z.object({
  name: z.string().min(1).max(100),
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  description: z.string().min(1).max(500).optional(),
  author: z.string().min(1).max(100).optional(),
  license: z.string().min(1).max(100).optional(),
  homepage: z.string().url().optional(),
  repository: z.string().url().optional(),
  keywords: z.array(z.string().min(1).max(50)).max(20).optional(),
  ontology: z.array(z.string().min(1).max(50)).max(10).optional(),
  dependencies: z.array(z.string()).optional(),
  createdAt: z.coerce.date().optional(),
  updatedAt: z.coerce.date().optional(),
});

/**
 * Schema for policy pack configuration (from policy-pack.mjs)
 */
export const PolicyPackConfigSchema = z.object({
  basePath: z.string().min(1).default(process.cwd()),
  manifestPath: z.string().default('./policy-pack.json'),
  hooksPath: z.string().default('./hooks'),
  activateOnLoad: z.boolean().default(false),
  enableValidation: z.boolean().default(true),
  maxHooks: z.number().int().positive().max(100).default(50),
  timeout: z.number().int().positive().max(300000).default(30000),
  strictMode: z.boolean().default(false),
});

/**
 * Schema for policy pack manifest (from policy-pack.mjs)
 */
export const PolicyPackManifestSchema = z.object({
  meta: PolicyPackMetaSchema,
  hooks: z.array(
    z.object({
      file: z.string().min(1),
      condition: z.object({
        kind: z.enum(['sparql-ask', 'sparql-select', 'shacl']),
        ref: FileRefSchema,
      }),
      priority: z.number().int().min(0).max(100).default(50).optional(),
      enabled: z.boolean().default(true).optional(),
    })
  ),
  rules: z
    .array(
      z.object({
        id: z.string().min(1),
        description: z.string().min(1).max(500),
        severity: z.enum(['error', 'warn', 'info']).default('info'),
        condition: z.string().min(1),
        appliesTo: z.array(z.string()).optional(),
        excludeFrom: z.array(z.string()).optional(),
      })
    )
    .optional(),
  dependencies: z.array(z.string()).optional(),
  peerDependencies: z.array(z.string()).optional(),
});

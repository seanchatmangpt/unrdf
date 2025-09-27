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
  name: z.string().min(1).max(100).regex(/^[a-zA-Z0-9:_-]+$/, 'Name must contain only alphanumeric characters, colons, hyphens, and underscores'),
  description: z.string().min(1).max(500).optional(),
  version: z.string().regex(/^\d+\.\d+\.\d+$/, 'Version must be semantic version format').optional(),
  author: z.string().min(1).max(100).optional(),
  tags: z.array(z.string().min(1).max(50)).max(10).optional(),
  ontology: z.array(z.string().min(1).max(50)).max(10).optional(),
  createdAt: z.coerce.date().optional(),
  updatedAt: z.coerce.date().optional()
});

/**
 * Schema for content-addressed file references
 */
export const FileRefSchema = z.object({
  uri: z.url({ message: 'Must be a valid URI' }).or(z.string().regex(/^file:\/\/.+/, 'Must be a valid file URI')),
  sha256: z.string().length(64).regex(/^[a-f0-9]+$/, 'Must be a valid SHA-256 hash'),
  mediaType: z.string().min(1).max(100).optional(),
  size: z.number().int().positive().optional(),
  lastModified: z.coerce.date().optional()
});

/**
 * Schema for SPARQL ASK conditions
 */
export const SparqlAskConditionSchema = z.object({
  kind: z.literal('sparql-ask'),
  ref: FileRefSchema,
  options: z.object({
    timeout: z.number().int().positive().max(30000).optional(),
    strict: z.boolean().optional(),
    variables: z.record(z.string()).optional()
  }).optional()
});

/**
 * Schema for SPARQL SELECT conditions
 */
export const SparqlSelectConditionSchema = z.object({
  kind: z.literal('sparql-select'),
  ref: FileRefSchema,
  options: z.object({
    timeout: z.number().int().positive().max(30000).optional(),
    limit: z.number().int().positive().max(10000).optional(),
    offset: z.number().int().nonnegative().optional(),
    strict: z.boolean().optional(),
    variables: z.record(z.string()).optional()
  }).optional()
});

/**
 * Schema for SHACL validation conditions
 */
export const ShaclConditionSchema = z.object({
  kind: z.literal('shacl'),
  ref: FileRefSchema,
  options: z.object({
    strict: z.boolean().optional(),
    includeDetails: z.boolean().optional(),
    maxViolations: z.number().int().positive().max(1000).optional(),
    shapesGraph: z.url({ message: 'Must be a valid URL' }).optional()
  }).optional()
});

/**
 * Union schema for all condition types
 */
export const ConditionSchema = z.discriminatedUnion('kind', [
  SparqlAskConditionSchema,
  SparqlSelectConditionSchema,
  ShaclConditionSchema
]);

/**
 * Schema for determinism configuration
 */
export const DeterminismSchema = z.object({
  seed: z.number().int().nonnegative().max(2147483647).default(42),
  algorithm: z.enum(['lcg', 'xorshift', 'mersenne']).default('lcg'),
  salt: z.string().min(1).max(100).optional()
});

/**
 * Schema for receipt configuration
 */
export const ReceiptSchema = z.object({
  anchor: z.enum(['none', 'blockchain', 'merkle', 'timestamp', 'hash', 'git-notes']).default('none'),
  format: z.enum(['json', 'jsonld', 'rdf', 'turtle']).default('json'),
  includeProof: z.boolean().default(false),
  ttl: z.number().int().positive().max(86400).optional() // Time to live in seconds
});

/**
 * Schema for hook execution context
 */
export const HookContextSchema = z.object({
  graph: z.any(), // RDF Store instance - validated at runtime
  env: z.record(z.any()).optional(),
  metadata: z.record(z.any()).optional(),
  transactionId: z.uuid({ message: 'Must be a valid UUID' }).optional(),
  timestamp: z.coerce.date().optional()
});

/**
 * Schema for hook events
 */
export const HookEventSchema = z.object({
  name: z.string().min(1).max(100),
  payload: z.any(), // Flexible payload structure
  context: HookContextSchema,
  id: z.uuid({ message: 'Must be a valid UUID' }).optional(),
  timestamp: z.coerce.date().optional(),
  source: z.string().min(1).max(100).optional(),
  correlationId: z.uuid({ message: 'Must be a valid UUID' }).optional()
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
  assertions: z.array(z.object({
    subject: z.string(),
    predicate: z.string(),
    object: z.string(),
    graph: z.string().optional()
  })).optional()
});

/**
 * Schema for hook channel configuration
 */
export const HookChannelSchema = z.object({
  graphs: z.array(z.string()).optional(),
  view: z.enum(['before', 'after', 'delta']).optional()
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
  priority: z.number().int().min(0).max(100).default(50).optional()
});

/**
 * Schema for transaction delta
 */
export const TransactionDeltaSchema = z.object({
  additions: z.array(z.any()).default([]), // RDF Quad array
  removals: z.array(z.any()).default([]), // RDF Quad array
  metadata: z.record(z.any()).optional(),
  id: z.uuid({ message: 'Must be a valid UUID' }).optional(),
  timestamp: z.coerce.date().optional()
});

/**
 * Schema for transaction receipt
 */
export const TransactionReceiptSchema = z.object({
  committed: z.boolean(),
  delta: TransactionDeltaSchema,
  hookResults: z.array(z.object({
    hookId: z.string(),
    result: z.boolean(),
    error: z.string().optional(),
    duration: z.number().nonnegative().optional()
  })).default([]),
  beforeHash: z.object({
    sha3: z.string(),
    blake3: z.string()
  }).optional(),
  afterHash: z.object({
    sha3: z.string(),
    blake3: z.string()
  }).optional(),
  timestamp: z.coerce.date(),
  duration: z.number().nonnegative(),
  knowledgeHookResults: z.number().int().nonnegative().default(0)
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
  logLevel: z.enum(['error', 'warn', 'info', 'debug']).default('info')
});

/**
 * Schema for file resolver configuration
 */
export const FileResolverConfigSchema = z.object({
  basePath: z.string().min(1).default(process.cwd()),
  enableCache: z.boolean().default(true),
  cacheMaxAge: z.number().int().positive().max(3600000).default(300000),
  maxFileSize: z.number().int().positive().max(10485760).default(1048576), // 1MB default
  allowedMediaTypes: z.array(z.string()).default([
    'application/sparql-query',
    'text/turtle',
    'application/rdf+xml',
    'application/ld+json'
  ]),
  timeout: z.number().int().positive().max(30000).default(5000)
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
  strict: z.boolean().default(false)
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
  enableAssertions: z.boolean().default(true)
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
        errors: error.errors?.map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown'
        })) || []
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
        errors: error.errors?.map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown'
        })) || []
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
        errors: error.errors?.map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown'
        })) || []
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
        errors: error.errors?.map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown'
        })) || []
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
        errors: error.errors?.map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown'
        })) || []
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
      const errorMessages = validation.errors?.map(err => `${err.path}: ${err.message}`).join(', ') || 'Unknown validation error';
      throw new TypeError(`Invalid knowledge hook definition: ${errorMessages}`);
    }
    
    // Apply defaults and freeze
    const normalized = {
      ...validation.data,
      determinism: { seed: 42, ...validation.data.determinism },
      receipt: { anchor: 'none', ...validation.data.receipt },
      priority: validation.data.priority ?? 50
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
    const errorMessages = validation.errors?.map(err => `${err.path}: ${err.message}`).join(', ') || 'Unknown validation error';
    throw new TypeError(`Invalid hook event: ${errorMessages}`);
  }
  
  // Apply defaults
  const normalized = {
    ...validation.data,
    id: validation.data.id ?? crypto.randomUUID(),
    timestamp: validation.data.timestamp ?? new Date().toISOString()
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
    const errorMessages = validation.errors?.map(err => `${err.path}: ${err.message}`).join(', ') || 'Unknown validation error';
    throw new TypeError(`Invalid condition: ${errorMessages}`);
  }
  
  return validation.data;
}

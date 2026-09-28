/**
 * @file Hook-related Zod schemas
 * @module hook-schemas
 *
 * @description
 * Zod schemas for knowledge hooks, conditions, events, and results.
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

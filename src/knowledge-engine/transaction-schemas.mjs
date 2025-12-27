/**
 * @file Transaction-related Zod schemas
 * @module transaction-schemas
 *
 * @description
 * Zod schemas for transactions, deltas, receipts, and hooks.
 */

import { z } from 'zod';

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
 * Schema for RDF quads
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
 * Schema for delta
 */
export const DeltaSchema = z.object({
  additions: z.array(QuadSchema),
  removals: z.array(QuadSchema),
});

/**
 * Schema for transaction hooks
 */
export const TransactionHookSchema = z.object({
  id: z.string().min(1),
  mode: z.enum(['pre', 'post']),
  condition: z.function(),
  effect: z.union([z.literal('veto'), z.function()]),
});

/**
 * Schema for hook results
 */
export const TransactionHookResultSchema = z.object({
  hookId: z.string(),
  mode: z.enum(['pre', 'post']),
  result: z.boolean(),
  error: z.string().optional(),
});

/**
 * Schema for hash values
 */
export const HashSchema = z.object({
  sha3: z.string(),
  blake3: z.string(),
});

/**
 * Schema for transaction receipt (new version)
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
 * Schema for transaction options
 */
export const TransactionOptionsSchema = z.object({
  skipHooks: z.boolean().default(false),
  timeoutMs: z.number().positive().default(30000),
  actor: z.string().optional(),
});

/**
 * Schema for manager options
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

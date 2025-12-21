/**
 * @file Consolidated Zod schemas for transaction system
 * @module utils/schemas
 */

import { z } from 'zod';

/**
 * Quad schema for RDF quads
 */
export const _QuadSchema = z.object({
  subject: z.object({ value: z.string() }),
  predicate: z.object({ value: z.string() }),
  object: z.object({ value: z.string() }),
  graph: z.object({ value: z.string() }).optional(),
});

/**
 * Delta schema for transaction changes
 */
export const DeltaSchema = z.object({
  additions: z.array(_QuadSchema).default([]),
  removals: z.array(_QuadSchema).default([]),
});

/**
 * Transaction hook schema
 */
export const TransactionHookSchema = z.object({
  id: z.string(),
  mode: z.enum(['pre', 'post']),
  condition: z.function().args(z.any(), z.any()).returns(z.promise(z.boolean())),
  effect: z.union([
    z.literal('veto'),
    z.function().args(z.any(), z.any()).returns(z.promise(z.void())),
  ]),
});

/**
 * Hook result schema
 */
export const _TransactionHookResultSchema = z.object({
  hookId: z.string(),
  mode: z.enum(['pre', 'post']),
  result: z.boolean(),
  error: z.string().optional(),
});

/**
 * Hash schema for store hashing
 */
export const _HashSchema = z.object({
  sha3: z.string(),
  blake3: z.string(),
});

/**
 * Transaction receipt schema
 */
export const _TransactionReceiptSchemaNew = z.object({
  id: z.string().optional(),
  delta: DeltaSchema,
  committed: z.boolean(),
  hookResults: z.array(_TransactionHookResultSchema),
  beforeHash: _HashSchema,
  afterHash: _HashSchema,
  timestamp: z.number().optional(),
  durationMs: z.number().optional(),
  actor: z.string().optional(),
  hookErrors: z.array(z.string()).optional(),
  error: z.string().optional(),
});

/**
 * Transaction options schema
 */
export const TransactionOptionsSchema = z
  .object({
    timeoutMs: z.number().default(30000),
    skipHooks: z.boolean().default(false),
    actor: z.string().optional(),
  })
  .default({});

/**
 * Manager options schema
 */
export const ManagerOptionsSchema = z
  .object({
    maxHooks: z.number().default(100),
    strictMode: z.boolean().default(false),
    afterHashOnly: z.boolean().default(false),
    enableLockchain: z.boolean().default(false),
    lockchainConfig: z
      .object({
        gitRepo: z.string().optional(),
        refName: z.string().optional(),
        signingKey: z.string().optional(),
        batchSize: z.number().optional(),
      })
      .optional(),
    enableResolution: z.boolean().default(false),
    resolutionConfig: z
      .object({
        defaultStrategy: z.enum(['voting', 'priority', 'consensus']).optional(),
        maxProposals: z.number().optional(),
        enableConflictDetection: z.boolean().optional(),
        enableConsensus: z.boolean().optional(),
        timeout: z.number().optional(),
      })
      .optional(),
  })
  .default({});

/**
 * Export aliases for backward compatibility
 */
export const QuadSchema = _QuadSchema;
export const HashSchema = _HashSchema;
export const TransactionHookResultSchema = _TransactionHookResultSchema;
export const TransactionReceiptSchemaNew = _TransactionReceiptSchemaNew;

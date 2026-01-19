/**
 * @file Federation Query Schemas
 * @module @unrdf/daemon/integrations/federation-query-schemas
 * @description Zod validation schemas for federated query execution
 */

import { z } from 'zod';

/**
 * Query execution statistics
 */
export const QueryStatsSchema = z.object({
  queryId: z.string(),
  sparql: z.string(),
  strategy: z.enum(['broadcast', 'selective', 'best-node']),
  nodeCount: z.number().int().nonnegative(),
  successCount: z.number().int().nonnegative(),
  failureCount: z.number().int().nonnegative(),
  totalDuration: z.number().nonnegative(),
  startTime: z.number(),
  endTime: z.number(),
  timestamp: z.date().default(() => new Date()),
});

/**
 * Federation executor configuration
 */
export const FederationExecutorConfigSchema = z.object({
  executorId: z.string().min(1).default(() => `executor-${Date.now()}`),
  strategy: z.enum(['broadcast', 'selective', 'best-node']).default('selective'),
  timeout: z.number().positive().default(30000),
  maxRetries: z.number().int().min(1).default(2),
  deduplicateResults: z.boolean().default(true),
  enableNodeSelection: z.boolean().default(true),
  healthCheckThreshold: z.number().min(0).max(1).default(0.7),
});

/**
 * Execute query options schema
 */
export const ExecuteQueryOptionsSchema = z.object({
  strategy: z.enum(['broadcast', 'selective', 'best-node']).optional(),
  timeout: z.number().positive().optional(),
  excludeNodes: z.array(z.string()).optional(),
}).optional();

/**
 * SPARQL query validation schema
 */
export const SparqlQuerySchema = z.string({
  required_error: 'Invalid SPARQL query: query is required',
  invalid_type_error: 'Invalid SPARQL query: must be a string'
}).refine(
  (query) => query && query.trim().length > 0,
  { message: 'Invalid SPARQL query: must be a non-empty string' }
);

/**
 * Query ID validation schema
 */
export const QueryIdSchema = z.string().min(1);

/**
 * Node ID validation schema
 */
export const NodeIdSchema = z.string().min(1);

/**
 * Daemon and coordinator validation schemas
 */
export const DaemonSchema = z.object({}).passthrough();
export const FederationCoordinatorSchema = z.object({}).passthrough();

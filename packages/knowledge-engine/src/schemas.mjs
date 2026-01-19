/**
 * @file Zod Validation Schemas for Knowledge Engine
 * @module knowledge-engine/schemas
 * @description Defines runtime validation schemas for observability, performance metrics, and configuration
 */

import { z } from 'zod';

/**
 * Observability configuration schema
 * @type {z.ZodObject}
 */
export const ObservabilityConfigSchema = z.object({
  serviceName: z.string().default('unrdf-knowledge-engine'),
  serviceVersion: z.string().default('1.0.0'),
  endpoint: z.string().url().optional(),
  headers: z.record(z.string(), z.string()).default({}),
  enableTracing: z.boolean().default(true),
  enableMetrics: z.boolean().default(true),
  scheduledDelayMillis: z.number().int().positive().default(5000),
  exportTimeoutMillis: z.number().int().positive().default(30000),
  samplingRate: z.number().min(0).max(1).default(0.1),
  logSamplingRate: z.number().min(0).max(1).default(0.01),
  minSamples: z.number().int().positive().default(10),
  ewmaAlpha: z.number().min(0).max(1).default(0.7),
  cacheMaxSize: z.number().int().positive().optional(),
  resourceAttributes: z.record(z.string(), z.any()).default({}),
});

/**
 * Performance metrics schema
 * @type {z.ZodObject}
 */
export const PerformanceMetricsSchema = z.object({
  transactionLatency: z.object({
    p50: z.number().nonnegative(),
    p95: z.number().nonnegative(),
    p99: z.number().nonnegative(),
    max: z.number().nonnegative(),
  }),
  hookExecutionRate: z.number().nonnegative(),
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
      high: z.number().positive(),
      low: z.number().nonnegative(),
    }),
  }),
});

/**
 * Hook execution context schema
 * @type {z.ZodObject}
 */
export const HookExecutionContextSchema = z.object({
  hookId: z.string(),
  transactionId: z.string(),
  timestamp: z.number(),
  duration: z.number().nonnegative(),
  success: z.boolean(),
  error: z.string().optional(),
});

/**
 * Query optimization config schema
 * @type {z.ZodObject}
 */
export const QueryOptimizationConfigSchema = z.object({
  enableOptimization: z.boolean().default(true),
  maxPlanCacheSize: z.number().int().positive().default(1000),
  estimationAccuracy: z.number().min(0).max(1).default(0.85),
  timeout: z.number().int().positive().default(5000),
});

/**
 * Transaction config schema
 * @type {z.ZodObject}
 */
export const TransactionConfigSchema = z.object({
  maxRetries: z.number().int().nonnegative().default(3),
  retryDelay: z.number().int().nonnegative().default(100),
  timeout: z.number().int().positive().default(30000),
  isolation: z.enum(['READ_UNCOMMITTED', 'READ_COMMITTED', 'REPEATABLE_READ', 'SERIALIZABLE']).default('READ_COMMITTED'),
});

export default {
  ObservabilityConfigSchema,
  PerformanceMetricsSchema,
  HookExecutionContextSchema,
  QueryOptimizationConfigSchema,
  TransactionConfigSchema,
};

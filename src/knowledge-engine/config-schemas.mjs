/**
 * @file Configuration-related Zod schemas
 * @module config-schemas
 *
 * @description
 * Zod schemas for various configuration objects used in the knowledge engine.
 */

import { z } from 'zod';

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

import { z } from 'zod';

const nonNegative = z.number().finite().nonnegative();

export const ObservabilityConfigSchema = z
  .object({
    serviceName: z.string().min(1).default('unrdf-knowledge-engine'),
    serviceVersion: z.string().min(1).default('0.0.0-agnostic'),
    endpoint: z.string().url().optional(),
    headers: z.record(z.string(), z.string()).default({}),
    resourceAttributes: z.record(z.string(), z.unknown()).default({}),
    enableTracing: z.boolean().default(true),
    enableMetrics: z.boolean().default(true),
    scheduledDelayMillis: z.number().int().positive().default(60000),
    exportTimeoutMillis: z.number().int().positive().default(30000),
    samplingRate: z.number().min(0).max(1).default(0.1),
    logSamplingRate: z.number().min(0).max(1).default(0.01),
    minSamples: z.number().int().nonnegative().default(10),
    ewmaAlpha: z.number().positive().max(1).default(0.3),
    cacheMaxSize: z.number().int().nonnegative().optional()
  })
  .passthrough();

export const PerformanceMetricsSchema = z.object({
  transactionLatency: z.object({
    p50: nonNegative,
    p95: nonNegative,
    p99: nonNegative,
    max: nonNegative
  }),
  hookExecutionRate: nonNegative,
  errorRate: z.number().min(0).max(1),
  memoryUsage: z
    .object({
      rss: nonNegative,
      heapTotal: nonNegative,
      heapUsed: nonNegative,
      external: nonNegative,
      arrayBuffers: nonNegative.optional()
    })
    .passthrough(),
  cacheStats: z.object({
    hitRate: z.number().min(0).max(1),
    size: nonNegative,
    maxSize: nonNegative
  }),
  backpressure: z
    .object({
      queueDepth: nonNegative,
      watermarks: z.object({ high: nonNegative, low: nonNegative })
    })
    .passthrough()
});

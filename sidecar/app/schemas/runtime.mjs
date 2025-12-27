/**
 * Runtime status and monitoring schemas
 * @module schemas/runtime
 */

import { z } from 'zod'
import { TimestampSchema, HookIdSchema } from './common.mjs'

/**
 * Memory usage schema
 * Tracks Node.js process memory consumption
 */
export const MemoryUsageSchema = z.object({
  /** Heap memory used in bytes */
  heapUsed: z.number().nonnegative(),

  /** Total heap memory allocated in bytes */
  heapTotal: z.number().nonnegative(),

  /** Resident set size in bytes */
  rss: z.number().nonnegative(),

  /** External memory usage in bytes */
  external: z.number().nonnegative().optional(),

  /** Array buffers memory in bytes */
  arrayBuffers: z.number().nonnegative().optional()
})

/**
 * Hook statistics schema
 * Tracks hook execution statistics
 */
export const HookStatisticsSchema = z.object({
  /** Total number of hooks registered */
  total: z.number().int().nonnegative(),

  /** Number of active (enabled) hooks */
  active: z.number().int().nonnegative(),

  /** Number of disabled hooks */
  disabled: z.number().int().nonnegative(),

  /** Breakdown by phase */
  byPhase: z.object({
    pre: z.number().int().nonnegative(),
    post: z.number().int().nonnegative(),
    invariant: z.number().int().nonnegative()
  }).optional()
})

/**
 * Data source statistics schema
 * Tracks connected data sources
 */
export const DataSourceStatisticsSchema = z.object({
  /** Total number of data sources */
  total: z.number().int().nonnegative(),

  /** Number of active connections */
  active: z.number().int().nonnegative().optional(),

  /** Breakdown by type */
  byType: z.record(z.number().int().nonnegative()).optional()
})

/**
 * Recent activity entry schema
 * Represents a single hook execution event
 */
export const RecentActivitySchema = z.object({
  /** Hook that was executed */
  hookId: HookIdSchema,

  /** Execution timestamp */
  timestamp: TimestampSchema,

  /** Whether execution succeeded */
  success: z.boolean(),

  /** Execution duration in milliseconds */
  duration: z.number().nonnegative().optional(),

  /** Error message if failed */
  error: z.string().optional()
})

/**
 * Runtime status schema
 * Comprehensive status of the Knowledge Hooks runtime
 */
export const RuntimeStatusSchema = z.object({
  /** Process uptime in seconds */
  uptime: z.number().nonnegative(),

  /** Memory usage statistics */
  memory: MemoryUsageSchema,

  /** Hook statistics */
  hooks: HookStatisticsSchema,

  /** Data source statistics */
  dataSources: DataSourceStatisticsSchema,

  /** Recent hook execution activity */
  recentActivity: z.array(RecentActivitySchema).max(100),

  /** Current Node.js version */
  nodeVersion: z.string().optional(),

  /** Application version */
  appVersion: z.string().optional(),

  /** Status timestamp */
  timestamp: TimestampSchema.optional()
})

/**
 * Performance metrics schema
 * Detailed performance metrics for monitoring
 */
export const PerformanceMetricsSchema = z.object({
  /** Average hook execution time in milliseconds */
  avgExecutionTime: z.number().nonnegative(),

  /** Median execution time in milliseconds */
  medianExecutionTime: z.number().nonnegative(),

  /** 95th percentile execution time */
  p95ExecutionTime: z.number().nonnegative(),

  /** 99th percentile execution time */
  p99ExecutionTime: z.number().nonnegative(),

  /** Total number of executions */
  totalExecutions: z.number().int().nonnegative(),

  /** Number of successful executions */
  successCount: z.number().int().nonnegative(),

  /** Number of failed executions */
  failureCount: z.number().int().nonnegative(),

  /** Success rate (0-1) */
  successRate: z.number().min(0).max(1),

  /** Time window for these metrics */
  timeWindow: z.object({
    start: TimestampSchema,
    end: TimestampSchema
  })
})

/**
 * Health check schema
 * Basic health check response
 */
export const HealthCheckSchema = z.object({
  /** Overall health status */
  status: z.enum(['healthy', 'degraded', 'unhealthy']),

  /** Detailed component checks */
  checks: z.object({
    /** Database connectivity */
    database: z.object({
      status: z.enum(['up', 'down']),
      latency: z.number().nonnegative().optional()
    }).optional(),

    /** SPARQL endpoint connectivity */
    sparqlEndpoint: z.object({
      status: z.enum(['up', 'down']),
      latency: z.number().nonnegative().optional()
    }).optional(),

    /** Memory health */
    memory: z.object({
      status: z.enum(['healthy', 'warning', 'critical']),
      usagePercent: z.number().min(0).max(100).optional()
    }).optional()
  }).optional(),

  /** Timestamp of health check */
  timestamp: TimestampSchema
})

/**
 * System configuration schema
 * Current runtime configuration
 */
export const SystemConfigSchema = z.object({
  /** Maximum concurrent hook executions */
  maxConcurrentExecutions: z.number().int().positive(),

  /** Default timeout for hook execution in milliseconds */
  defaultTimeout: z.number().int().positive(),

  /** Whether query result caching is enabled */
  cacheEnabled: z.boolean(),

  /** Cache TTL in seconds */
  cacheTtl: z.number().int().positive().optional(),

  /** Maximum cache size in entries */
  maxCacheSize: z.number().int().positive().optional(),

  /** Log level */
  logLevel: z.enum(['debug', 'info', 'warn', 'error']),

  /** Whether observability is enabled */
  observabilityEnabled: z.boolean()
})

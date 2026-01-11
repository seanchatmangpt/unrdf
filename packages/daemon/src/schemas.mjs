/**
 * @file Zod schemas for daemon package
 * @module schemas
 * @description Comprehensive schema definitions for daemon configuration, operations,
 * triggers, receipts, health status, and metrics. Provides runtime validation for
 * all daemon entities with composable, reusable schemas supporting refinements.
 */

import { z } from 'zod';

/**
 * Retry policy configuration for operations
 * Defines exponential backoff with jitter for failed operation retries.
 * - maxAttempts: Maximum number of retry attempts (default: 3)
 * - backoffMs: Initial backoff delay in milliseconds (default: 1000)
 * - backoffMultiplier: Exponential backoff multiplier (default: 2)
 * - maxBackoffMs: Maximum backoff delay cap (default: 30000)
 * - jitterFactor: Random jitter percentage [0-1] (default: 0.1)
 */
export const RetryPolicySchema = z.object({
  maxAttempts: z.number().int().min(1).default(3),
  backoffMs: z.number().int().min(0).default(1000),
  backoffMultiplier: z.number().min(1).default(2),
  maxBackoffMs: z.number().int().min(1000).default(30000),
  jitterFactor: z.number().min(0).max(1).default(0.1),
}).default({
  maxAttempts: 3,
  backoffMs: 1000,
  backoffMultiplier: 2,
  maxBackoffMs: 30000,
  jitterFactor: 0.1,
});

/**
 * Trigger configuration for scheduled operations
 * Supports five trigger mechanisms:
 * - cron: POSIX cron expressions with timezone support
 * - interval: Fixed interval triggering in milliseconds
 * - idle: Triggers when daemon has been idle beyond threshold
 * - reactive: Triggers on entity mutations (create/update/delete)
 * - event: Custom event-based triggering with optional filter predicates
 */
export const TriggerSchema = z.discriminatedUnion('type', [
  z.object({
    type: z.literal('cron'),
    expression: z.string().min(5),
    timezone: z.string().default('UTC'),
  }),
  z.object({
    type: z.literal('interval'),
    intervalMs: z.number().int().min(100),
  }),
  z.object({
    type: z.literal('idle'),
    idleThresholdMs: z.number().int().min(1000).default(60000),
  }),
  z.object({
    type: z.literal('reactive'),
    entityType: z.string().min(1),
    operation: z.enum(['create', 'update', 'delete']),
  }),
  z.object({
    type: z.literal('event'),
    eventName: z.string().min(1),
    filter: z.record(z.string(), z.any()).optional(),
  }),
]);

/**
 * Scheduled operation with triggers and retry policy
 * Represents a composable task unit that combines triggers, actions, and recovery policies.
 * - triggers: Array of trigger conditions (min 1 required)
 * - action: Operation to execute with type and payload
 * - retryPolicy: Optional retry configuration (uses default if omitted)
 * - enabled: Can be toggled on/off without deletion
 * - createdAt/updatedAt: Audit timestamps
 */
export const ScheduledOperationSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(255),
  description: z.string().max(500).optional(),
  triggers: z.array(TriggerSchema).min(1),
  action: z.object({
    type: z.string().min(1),
    payload: z.record(z.string(), z.any()),
  }),
  retryPolicy: RetryPolicySchema.optional(),
  enabled: z.boolean().default(true),
  metadata: z.record(z.string(), z.any()).default({}),
  createdAt: z.date().default(() => new Date()),
  updatedAt: z.date().default(() => new Date()),
});

/**
 * Daemon configuration with operational parameters
 * Top-level configuration for daemon instance including network, concurrency,
 * health monitoring, and registered scheduled operations.
 * - port: Server port (1024-65535, default: 8080)
 * - concurrency: Max parallel operation slots (1-100, default: 10)
 * - healthCheckIntervalMs: Health check frequency (default: 30000)
 * - metricsRetentionMs: Metrics window size (default: 3600000)
 */
export const DaemonConfigSchema = z.object({
  daemonId: z.string().uuid(),
  name: z.string().min(1).max(255),
  port: z.number().int().min(1024).max(65535).default(8080),
  logLevel: z.enum(['debug', 'info', 'warn', 'error']).default('info'),
  concurrency: z.number().int().min(1).default(10),
  healthCheckIntervalMs: z.number().int().min(1000).default(30000),
  metricsRetentionMs: z.number().int().min(60000).default(3600000),
  operations: z.array(ScheduledOperationSchema).default([]),
  globalRetryPolicy: RetryPolicySchema.optional(),
  environment: z.record(z.string(), z.string()).default({}),
}).refine(
  (config) => config.concurrency <= 100,
  { message: 'Concurrency must not exceed 100', path: ['concurrency'] }
);

/**
 * Operation receipt with execution proof
 * Records complete execution lifecycle with cryptographic proofs for auditability.
 * - status: Execution state (pending/running/success/failed/skipped)
 * - duration: Execution time in milliseconds
 * - attempts: Number of execution attempts (including retries)
 * - proof: Merkle hash and timestamp for integrity verification
 * - error: Detailed error information on failure (code/message/stack)
 */
export const OperationReceiptSchema = z.object({
  id: z.string().uuid(),
  operationId: z.string().uuid(),
  operationType: z.string().min(1),
  status: z.enum(['pending', 'running', 'success', 'failed', 'skipped']),
  startedAt: z.date(),
  completedAt: z.date().optional(),
  duration: z.number().int().min(0).optional(),
  result: z.record(z.string(), z.any()).optional(),
  error: z.object({
    code: z.string(),
    message: z.string(),
    stack: z.string().optional(),
  }).optional(),
  attempts: z.number().int().min(1),
  proof: z.object({
    hash: z.string().min(64),
    timestamp: z.date(),
    signature: z.string().min(1).optional(),
  }).optional(),
  metadata: z.record(z.string(), z.any()).default({}),
}).refine(
  (receipt) => !receipt.completedAt || receipt.completedAt >= receipt.startedAt,
  { message: 'Completed time must be after started time', path: ['completedAt'] }
);

/**
 * Daemon health status snapshot
 * Represents current daemon operational state with pass/fail checks.
 * - status: Overall health (healthy/degraded/unhealthy)
 * - uptime: Running duration in milliseconds
 * - activeOperations/totalOperations: Execution counters
 * - failureRate: Proportion of failed operations [0-1]
 * - checks: Key-value map of named health checks with results
 */
export const DaemonHealthSchema = z.object({
  status: z.enum(['healthy', 'degraded', 'unhealthy']),
  uptime: z.number().int().min(0),
  timestamp: z.date().default(() => new Date()),
  activeOperations: z.number().int().min(0),
  totalOperations: z.number().int().min(0),
  failureRate: z.number().min(0).max(1),
  lastError: z.object({
    code: z.string(),
    message: z.string(),
    timestamp: z.date(),
  }).optional(),
  checks: z.record(z.string(), z.object({
    passed: z.boolean(),
    message: z.string().optional(),
  })).default({}),
});

/**
 * Metrics snapshot for daemon performance monitoring
 * Captures point-in-time metrics for throughput, latency percentiles, and resources.
 * - operations: Aggregated operation counts and states
 * - latency: P50/P95/P99 millisecond percentiles
 * - memory: Heap usage metrics in bytes
 * - throughput: Operations per second rate
 */
export const DaemonMetricsSchema = z.object({
  timestamp: z.date().default(() => new Date()),
  uptime: z.number().int().min(0),
  operations: z.object({
    total: z.number().int().min(0),
    succeeded: z.number().int().min(0),
    failed: z.number().int().min(0),
    running: z.number().int().min(0),
  }),
  latency: z.object({
    p50: z.number().min(0),
    p95: z.number().min(0),
    p99: z.number().min(0),
  }),
  memory: z.object({
    used: z.number().min(0),
    heapMax: z.number().min(0),
  }),
  throughput: z.object({
    operationsPerSecond: z.number().min(0),
  }).default({ operationsPerSecond: 0 }),
}).refine(
  (metrics) => metrics.operations.succeeded + metrics.operations.failed >= metrics.operations.total,
  { message: 'Succeeded + failed must equal total', path: ['operations'] }
);

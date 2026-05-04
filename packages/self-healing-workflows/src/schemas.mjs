/**
 * @file Zod schemas for self-healing workflows
 * @module @unrdf/self-healing-workflows/schemas
 * @description Validation schemas for error recovery, retry strategies, and health monitoring
 */

import { z } from 'zod';

/**
 * Error severity levels
 */
export const ErrorSeveritySchema = z.enum([
  'critical',    // Unrecoverable, requires manual intervention
  'high',        // Severe but potentially recoverable
  'medium',      // Standard errors, retry likely to succeed
  'low'          // Minor errors, safe to retry
]);

/**
 * Error categories for classification
 */
export const ErrorCategorySchema = z.enum([
  'network',         // Network connectivity issues
  'timeout',         // Operation timeout
  'validation',      // Data validation failure
  'resource',        // Resource unavailable (memory, disk, etc.)
  'dependency',      // External dependency failure
  'business-logic',  // Business rule violation
  'unknown'          // Unclassified error
]);

/**
 * Recovery action types
 */
export const RecoveryActionTypeSchema = z.enum([
  'retry',           // Retry the operation
  'skip',            // Skip and continue
  'compensate',      // Execute compensating transaction
  'restart',         // Restart the workflow
  'fallback',        // Use fallback strategy
  'manual'           // Require manual intervention
]);

/**
 * Retry strategy configuration
 */
export const RetryStrategySchema = z.object({
  maxAttempts: z.number().int().positive().default(3),
  initialDelay: z.number().nonnegative().default(1000),
  maxDelay: z.number().positive().default(30000),
  backoffMultiplier: z.number().positive().default(2),
  jitter: z.boolean().default(true),
  retryableErrors: z.array(ErrorCategorySchema).default(['network', 'timeout', 'resource'])
});

/**
 * Circuit breaker configuration
 */
export const CircuitBreakerConfigSchema = z.object({
  failureThreshold: z.number().int().positive().default(5),
  successThreshold: z.number().int().positive().default(2),
  timeout: z.number().positive().default(60000),
  resetTimeout: z.number().positive().default(30000),
  monitoringPeriod: z.number().positive().default(10000)
});

/**
 * Circuit breaker states
 */
export const CircuitBreakerStateSchema = z.enum([
  'closed',      // Normal operation, requests allowed
  'open',        // Failure threshold exceeded, requests blocked
  'half-open'    // Testing if service recovered
]);

/**
 * Error pattern for classification
 */
export const ErrorPatternSchema = z.object({
  name: z.string().min(1),
  category: ErrorCategorySchema,
  severity: ErrorSeveritySchema,
  pattern: z.union([
    z.string(),
    z.instanceof(RegExp)
  ]),
  metadata: z.record(z.unknown()).optional()
});

/**
 * Classified error
 */
export const ClassifiedErrorSchema = z.object({
  originalError: z.instanceof(Error),
  category: ErrorCategorySchema,
  severity: ErrorSeveritySchema,
  matchedPattern: z.string().optional(),
  retryable: z.boolean(),
  timestamp: z.number(),
  metadata: z.record(z.unknown()).optional()
});

/**
 * Recovery action
 */
export const RecoveryActionSchema = z.object({
  type: RecoveryActionTypeSchema,
  name: z.string().min(1),
  execute: z.function(),
  condition: z.function().optional(),
  priority: z.number().int().min(0).max(100).default(50),
  metadata: z.record(z.unknown()).optional()
});

/**
 * Recovery result
 */
export const RecoveryResultSchema = z.object({
  success: z.boolean(),
  action: RecoveryActionTypeSchema,
  attempts: z.number().int().nonnegative(),
  duration: z.number().nonnegative(),
  error: z.instanceof(Error).optional(),
  metadata: z.record(z.unknown()).optional()
});

/**
 * Health check configuration
 */
export const HealthCheckConfigSchema = z.object({
  interval: z.number().positive().default(30000),
  timeout: z.number().positive().default(5000),
  unhealthyThreshold: z.number().int().positive().default(3),
  healthyThreshold: z.number().int().positive().default(2)
});

/**
 * Health status
 */
export const HealthStatusSchema = z.enum([
  'healthy',
  'degraded',
  'unhealthy'
]);

/**
 * Health check result
 */
export const HealthCheckResultSchema = z.object({
  status: HealthStatusSchema,
  timestamp: z.number(),
  checks: z.array(z.object({
    name: z.string(),
    status: HealthStatusSchema,
    message: z.string().optional(),
    duration: z.number().nonnegative()
  })),
  metadata: z.record(z.unknown()).optional()
});

/**
 * Self-healing engine configuration
 */
export const SelfHealingConfigSchema = z.object({
  retry: RetryStrategySchema.optional(),
  circuitBreaker: CircuitBreakerConfigSchema.optional(),
  healthCheck: HealthCheckConfigSchema.optional(),
  errorPatterns: z.array(ErrorPatternSchema).default([]),
  recoveryActions: z.array(RecoveryActionSchema).default([]),
  enableOtel: z.boolean().default(true),
  maxConcurrentRecoveries: z.number().int().positive().default(10)
});

/**
 * Recovery statistics
 */
export const RecoveryStatsSchema = z.object({
  totalAttempts: z.number().int().nonnegative(),
  successfulRecoveries: z.number().int().nonnegative(),
  failedRecoveries: z.number().int().nonnegative(),
  averageRecoveryTime: z.number().nonnegative(),
  successRate: z.number().min(0).max(1),
  errorsByCategory: z.record(ErrorCategorySchema, z.number().int().nonnegative()),
  actionsByType: z.record(RecoveryActionTypeSchema, z.number().int().nonnegative())
});

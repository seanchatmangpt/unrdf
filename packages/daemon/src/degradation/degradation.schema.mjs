/**
 * @file Degradation Schemas
 * @module @unrdf/daemon/degradation/degradation-schema
 * @description Zod validation schemas for graceful degradation patterns
 */

import { z } from 'zod';

// ============================================================================
// Circuit Breaker Schemas
// ============================================================================

/**
 * Circuit breaker states
 */
export const CircuitStateSchema = z.enum(['CLOSED', 'OPEN', 'HALF_OPEN']);

/**
 * Circuit breaker configuration schema
 */
export const CircuitBreakerConfigSchema = z.object({
  name: z.string().min(1).default('default'),
  failureThreshold: z.number().int().positive().default(5),
  successThreshold: z.number().int().positive().default(3),
  timeout: z.number().int().positive().default(30000),
  halfOpenMaxCalls: z.number().int().positive().default(3),
  volumeThreshold: z.number().int().nonnegative().default(10),
  errorPercentageThreshold: z.number().min(0).max(100).default(50),
  resetTimeout: z.number().int().positive().default(60000),
  monitorInterval: z.number().int().positive().default(10000),
  onStateChange: z.function().args(z.string(), z.string()).returns(z.void()).optional(),
  onFailure: z.function().args(z.any()).returns(z.void()).optional(),
  onSuccess: z.function().returns(z.void()).optional(),
});

/**
 * Circuit breaker metrics schema
 */
export const CircuitBreakerMetricsSchema = z.object({
  totalCalls: z.number().int().nonnegative(),
  successfulCalls: z.number().int().nonnegative(),
  failedCalls: z.number().int().nonnegative(),
  rejectedCalls: z.number().int().nonnegative(),
  lastFailureTime: z.number().nullable(),
  lastSuccessTime: z.number().nullable(),
  currentState: CircuitStateSchema,
  stateChanges: z.number().int().nonnegative(),
  consecutiveFailures: z.number().int().nonnegative(),
  consecutiveSuccesses: z.number().int().nonnegative(),
});

/**
 * Circuit breaker call result schema
 */
export const CircuitBreakerResultSchema = z.object({
  success: z.boolean(),
  result: z.any().optional(),
  error: z.any().optional(),
  duration: z.number().nonnegative(),
  state: CircuitStateSchema,
  rejected: z.boolean().default(false),
});

// ============================================================================
// Bulkhead Schemas
// ============================================================================

/**
 * Bulkhead rejection policies
 */
export const RejectionPolicySchema = z.enum(['reject', 'queue', 'drop']);

/**
 * Bulkhead configuration schema
 */
export const BulkheadConfigSchema = z.object({
  name: z.string().min(1).default('default'),
  maxConcurrent: z.number().int().positive().default(10),
  maxQueue: z.number().int().nonnegative().default(100),
  queueTimeout: z.number().int().positive().default(5000),
  rejectionPolicy: RejectionPolicySchema.default('reject'),
  onRejected: z.function().args(z.string()).returns(z.void()).optional(),
  onQueued: z.function().args(z.number()).returns(z.void()).optional(),
  onExecuted: z.function().args(z.number()).returns(z.void()).optional(),
});

/**
 * Bulkhead metrics schema
 */
export const BulkheadMetricsSchema = z.object({
  name: z.string(),
  activeCalls: z.number().int().nonnegative(),
  queueSize: z.number().int().nonnegative(),
  maxConcurrent: z.number().int().positive(),
  maxQueue: z.number().int().nonnegative(),
  totalExecuted: z.number().int().nonnegative(),
  totalRejected: z.number().int().nonnegative(),
  totalQueued: z.number().int().nonnegative(),
  totalDropped: z.number().int().nonnegative(),
  totalTimedOut: z.number().int().nonnegative(),
  averageWaitTime: z.number().nonnegative(),
});

/**
 * Bulkhead execution result schema
 */
export const BulkheadResultSchema = z.object({
  success: z.boolean(),
  result: z.any().optional(),
  error: z.any().optional(),
  waitTime: z.number().nonnegative(),
  executionTime: z.number().nonnegative(),
  wasQueued: z.boolean(),
  rejected: z.boolean(),
  rejectionReason: z.string().optional(),
});

/**
 * Thread pool configuration schema
 */
export const ThreadPoolConfigSchema = z.object({
  name: z.string().min(1).default('default'),
  coreSize: z.number().int().positive().default(10),
  maxSize: z.number().int().positive().default(20),
  keepAliveTime: z.number().int().positive().default(60000),
  queueCapacity: z.number().int().nonnegative().default(100),
});

// ============================================================================
// Fallback Handler Schemas
// ============================================================================

/**
 * Fallback type schema
 */
export const FallbackTypeSchema = z.enum(['primary', 'timeout', 'cache', 'default', 'custom']);

/**
 * Retry configuration schema
 */
export const RetryConfigSchema = z.object({
  maxRetries: z.number().int().nonnegative().default(3),
  initialDelay: z.number().int().positive().default(100),
  maxDelay: z.number().int().positive().default(10000),
  multiplier: z.number().positive().default(2),
  jitter: z.boolean().default(true),
  retryOn: z.array(z.string()).default(['Error']),
  onRetry: z.function().args(z.number(), z.any()).returns(z.void()).optional(),
});

/**
 * Fallback handler configuration schema
 */
export const FallbackConfigSchema = z.object({
  name: z.string().min(1).default('default'),
  timeout: z.number().int().positive().default(5000),
  cacheEnabled: z.boolean().default(false),
  cacheTtl: z.number().int().positive().default(60000),
  cacheMaxSize: z.number().int().positive().default(1000),
  defaultValue: z.any().optional(),
  retry: RetryConfigSchema.optional(),
  fallbacks: z.array(z.function()).default([]),
  onFallback: z.function().args(z.string(), z.any()).returns(z.void()).optional(),
  onSuccess: z.function().args(z.any()).returns(z.void()).optional(),
  onFailure: z.function().args(z.any()).returns(z.void()).optional(),
});

/**
 * Fallback execution result schema
 */
export const FallbackResultSchema = z.object({
  success: z.boolean(),
  result: z.any().optional(),
  error: z.any().optional(),
  source: FallbackTypeSchema,
  duration: z.number().nonnegative(),
  retries: z.number().int().nonnegative(),
  fromCache: z.boolean(),
});

/**
 * Cache entry schema
 */
export const CacheEntrySchema = z.object({
  value: z.any(),
  timestamp: z.number(),
  ttl: z.number().int().positive(),
  hits: z.number().int().nonnegative(),
});

/**
 * Fallback metrics schema
 */
export const FallbackMetricsSchema = z.object({
  totalCalls: z.number().int().nonnegative(),
  primarySuccesses: z.number().int().nonnegative(),
  timeoutFallbacks: z.number().int().nonnegative(),
  cacheFallbacks: z.number().int().nonnegative(),
  defaultFallbacks: z.number().int().nonnegative(),
  customFallbacks: z.number().int().nonnegative(),
  totalRetries: z.number().int().nonnegative(),
  totalFailures: z.number().int().nonnegative(),
  cacheHitRate: z.number().min(0).max(1),
  averageLatency: z.number().nonnegative(),
});

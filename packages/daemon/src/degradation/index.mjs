/**
 * @file Degradation Module Index
 * @module @unrdf/daemon/degradation
 * @description Graceful degradation patterns for fault tolerance and resilience.
 * Includes circuit breaker, bulkhead isolation, and fallback handling.
 */

// Circuit Breaker
export {
  CircuitBreaker,
  CircuitBreakerError,
  CircuitState,
  CircuitBreakerRegistry,
  createCircuitBreaker,
  globalRegistry as globalCircuitBreakerRegistry,
} from './circuit-breaker.mjs';

// Bulkhead
export {
  Bulkhead,
  BulkheadRejectionError,
  RejectionPolicy,
  ThreadPool,
  BulkheadRegistry,
  createBulkhead,
  createThreadPool,
  globalBulkheadRegistry,
} from './bulkhead.mjs';

// Fallback Handler
export {
  FallbackHandler,
  FallbackSource,
  FallbackHandlerRegistry,
  createFallbackHandler,
  withRetry,
  withTimeout,
  withFallback,
  globalFallbackRegistry,
} from './fallback-handler.mjs';

// Schemas
export {
  // Circuit Breaker Schemas
  CircuitStateSchema,
  CircuitBreakerConfigSchema,
  CircuitBreakerMetricsSchema,
  CircuitBreakerResultSchema,
  // Bulkhead Schemas
  RejectionPolicySchema,
  BulkheadConfigSchema,
  BulkheadMetricsSchema,
  BulkheadResultSchema,
  ThreadPoolConfigSchema,
  // Fallback Schemas
  FallbackTypeSchema,
  RetryConfigSchema,
  FallbackConfigSchema,
  FallbackResultSchema,
  FallbackMetricsSchema,
  CacheEntrySchema,
} from './degradation.schema.mjs';

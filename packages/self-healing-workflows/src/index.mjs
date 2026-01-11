/**
 * @file Self-Healing Workflows - Main export
 * @module @unrdf/self-healing-workflows
 * @description Automatic error recovery system with 85-95% success rate
 */

// Main engine
export {
  SelfHealingEngine,
  createSelfHealingEngine
} from './self-healing-engine.mjs';

// Error classification
export {
  ErrorClassifier,
  createErrorClassifier
} from './error-classifier.mjs';

// Retry strategies
export {
  RetryStrategy,
  createRetryStrategy,
  immediateRetry,
  exponentialRetry
} from './retry-strategy.mjs';

// Circuit breaker
export {
  CircuitBreaker,
  createCircuitBreaker
} from './circuit-breaker.mjs';

// Recovery actions
export {
  RecoveryActionExecutor,
  createRecoveryActionExecutor
} from './recovery-actions.mjs';

// Health monitoring
export {
  HealthMonitor,
  createHealthMonitor
} from './health-monitor.mjs';

// Schemas
export * from './schemas.mjs';

/**
 * @fileoverview Resilience Module - Production-Grade Error Recovery and Fault Tolerance
 *
 * **Purpose**: Comprehensive resilience mechanisms for KGC-SWARM system
 *
 * **Features**:
 * - Per-agent circuit breakers with fallback strategies
 * - Exponential backoff and intelligent retry
 * - Graceful degradation with QoS levels
 * - Checkpoint-based error recovery
 * - Receipt chain repair and verification
 * - Structured error logging and categorization
 *
 * **Usage**:
 * ```javascript
 * import {
 *   createAgentCircuitBreaker,
 *   RetryStrategy,
 *   GracefulDegradationManager,
 *   ErrorRecoveryManager,
 *   ReceiptChainRepairManager,
 *   logError,
 * } from './resilience/index.mjs';
 *
 * // Circuit breaker for agent
 * const breaker = createAgentCircuitBreaker('agent-001', 'coder');
 * const result = await breaker.execute(async () => agent.processTask(task));
 *
 * // Retry with exponential backoff
 * const retry = new RetryStrategy({ maxAttempts: 3 });
 * await retry.execute(async () => unreliableOperation());
 *
 * // Graceful degradation
 * const degradation = new GracefulDegradationManager();
 * const result = await degradation.executeWithDegradation(
 *   primaryOp,
 *   [cacheStrategy, simpleStrategy]
 * );
 *
 * // Checkpoint-based recovery
 * const recovery = new ErrorRecoveryManager();
 * const checkpoint = recovery.createCheckpoint('epoch_001', state);
 * try {
 *   await dangerousOp();
 * } catch (error) {
 *   await recovery.recoverFromCheckpoint(checkpoint.id);
 * }
 *
 * // Receipt chain repair
 * const repair = new ReceiptChainRepairManager();
 * const issues = await repair.detectIssues(chain);
 * if (issues.length > 0) {
 *   await repair.repairChain(chain);
 * }
 *
 * // Structured error logging
 * logError(error, {
 *   category: ErrorCategory.TRANSIENT,
 *   severity: ErrorSeverity.WARN,
 *   context: { agentId: 'agent-001' }
 * });
 * ```
 *
 * @module resilience
 */

// Circuit Breakers
export {
  AgentCircuitBreakerRegistry,
  createAgentCircuitBreaker,
  executeWithCircuitBreaker,
  defaultAgentRegistry,
  AgentCircuitConfigSchema,
} from './agent-circuit-breaker.mjs';

// Retry Strategy
export {
  RetryStrategy,
  RetryExhaustedError,
  withRetry,
  retryable,
  RetryConfigSchema,
  ErrorType,
} from './retry-strategy.mjs';

// Graceful Degradation
export {
  GracefulDegradationManager,
  DegradationExhaustedError,
  BatchFailureError,
  FallbackStrategies,
  QoSLevel,
  DegradationStrategySchema,
} from './graceful-degradation.mjs';

// Error Recovery
export {
  ErrorRecoveryManager,
  CheckpointSchema,
  RecoveryStrategy,
} from './error-recovery.mjs';

// Receipt Chain Repair
export {
  ReceiptChainRepairManager,
  RepairFailedError,
  IssueType,
  RepairStrategy,
} from './receipt-chain-repair.mjs';

// Structured Error Logging
export {
  StructuredErrorLogger,
  ErrorCategory,
  ErrorSeverity,
  StructuredErrorSchema,
  defaultErrorLogger,
  logError,
} from './structured-error-logger.mjs';

/**
 * Create a fully configured resilience stack
 * @param {Object} [config] - Configuration options
 * @returns {Object} Resilience stack
 *
 * @example
 * const resilience = createResilienceStack({
 *   circuit: { failureThreshold: 5 },
 *   retry: { maxAttempts: 3 },
 *   degradation: { defaultQoS: 'high' },
 * });
 *
 * await resilience.executeWithProtection(
 *   'agent-001',
 *   'coder',
 *   async () => operation()
 * );
 */
export async function createResilienceStack(config = {}) {
  const { AgentCircuitBreakerRegistry } = await import('./agent-circuit-breaker.mjs');
  const { RetryStrategy } = await import('./retry-strategy.mjs');
  const { GracefulDegradationManager } = await import('./graceful-degradation.mjs');
  const { ErrorRecoveryManager } = await import('./error-recovery.mjs');
  const { ReceiptChainRepairManager } = await import('./receipt-chain-repair.mjs');
  const { StructuredErrorLogger } = await import('./structured-error-logger.mjs');

  const agentRegistry = new AgentCircuitBreakerRegistry(config.circuit);
  const retryStrategy = new RetryStrategy(config.retry);
  const degradationManager = new GracefulDegradationManager(config.degradation);
  const recoveryManager = new ErrorRecoveryManager(config.recovery);
  const repairManager = new ReceiptChainRepairManager(config.repair);
  const errorLogger = new StructuredErrorLogger(config.logging);

  return {
    // Circuit breaker methods
    createCircuitBreaker: (agentId, agentType, customConfig) =>
      agentRegistry.getOrCreateForAgent(agentId, agentType, customConfig),

    executeWithProtection: (agentId, agentType, operation, options) =>
      agentRegistry.executeWithProtection(agentId, agentType, operation, options),

    getAgentHealth: () => agentRegistry.getAgentHealthSummary(),

    // Retry methods
    retry: async (operation, customConfig) => {
      if (customConfig) {
        const { RetryStrategy: RS } = await import('./retry-strategy.mjs');
        return new RS(customConfig).execute(operation);
      }
      return retryStrategy.execute(operation);
    },

    // Degradation methods
    degradeGracefully: (primary, fallbacks, context) =>
      degradationManager.executeWithDegradation(primary, fallbacks, context),

    executeBatch: (operations, options) =>
      degradationManager.executeBatchWithTolerance(operations, options),

    // Recovery methods
    createCheckpoint: (epoch, state, metadata) =>
      recoveryManager.createCheckpoint(epoch, state, metadata),

    recoverFromCheckpoint: (checkpointId, strategy) =>
      recoveryManager.recoverFromCheckpoint(checkpointId, strategy),

    executeWithRecovery: (epoch, state, operation, options) =>
      recoveryManager.executeWithRecovery(epoch, state, operation, options),

    // Repair methods
    detectChainIssues: (chain) => repairManager.detectIssues(chain),

    repairChain: (chain, strategy) => repairManager.repairChain(chain, strategy),

    // Logging methods
    logError: (error, options) => errorLogger.logError(error, options),

    getErrorsByCategory: (category, options) =>
      errorLogger.getErrorsByCategory(category, options),

    // Metrics
    getMetrics: () => ({
      circuit: agentRegistry.getAgentHealthSummary(),
      retry: retryStrategy.getMetrics(),
      degradation: degradationManager.getMetrics(),
      recovery: recoveryManager.getMetrics(),
      repair: repairManager.getMetrics(),
      logging: errorLogger.getMetrics(),
    }),

    // Reset
    reset: () => {
      agentRegistry.resetAllAgents();
      retryStrategy.resetMetrics();
      degradationManager.resetMetrics();
      recoveryManager.resetMetrics();
      repairManager.resetMetrics();
      errorLogger.resetMetrics();
    },
  };
}

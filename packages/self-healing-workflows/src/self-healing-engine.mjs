/**
 * @file Self-healing workflow engine
 * @module @unrdf/self-healing-workflows/engine
 * @description Main engine coordinating error recovery, circuit breaking, and health monitoring
 */

import { ErrorClassifier } from './error-classifier.mjs';
import { RetryStrategy } from './retry-strategy.mjs';
import { CircuitBreaker } from './circuit-breaker.mjs';
import { RecoveryActionExecutor } from './recovery-actions.mjs';
import { HealthMonitor } from './health-monitor.mjs';
import {
  SelfHealingConfigSchema
} from './schemas.mjs';

/**
 * Self-healing workflow engine
 */
export class SelfHealingEngine {
  /**
   * Creates a new self-healing engine
   * @param {Object} [config] - Engine configuration
   * @param {Object} [config.retry] - Retry strategy config
   * @param {Object} [config.circuitBreaker] - Circuit breaker config
   * @param {Object} [config.healthCheck] - Health check config
   * @param {Array<Object>} [config.errorPatterns] - Custom error patterns
   * @param {Array<Object>} [config.recoveryActions] - Custom recovery actions
   * @param {boolean} [config.enableOtel=true] - Enable OTEL instrumentation
   * @param {number} [config.maxConcurrentRecoveries=10] - Max concurrent recoveries
   */
  constructor(config = {}) {
    this.config = SelfHealingConfigSchema.parse(config);

    // Initialize components
    this.classifier = new ErrorClassifier({
      patterns: this.config.errorPatterns
    });

    this.retry = new RetryStrategy(this.config.retry);
    this.circuitBreaker = new CircuitBreaker(this.config.circuitBreaker);
    this.recoveryExecutor = new RecoveryActionExecutor();
    this.healthMonitor = new HealthMonitor(this.config.healthCheck);

    // Register custom recovery actions
    for (const action of this.config.recoveryActions) {
      this.recoveryExecutor.register(action);
    }

    // Statistics
    this.stats = {
      totalAttempts: 0,
      successfulRecoveries: 0,
      failedRecoveries: 0,
      totalRecoveryTime: 0,
      errorsByCategory: {},
      actionsByType: {}
    };

    // Active recoveries
    this.activeRecoveries = new Set();

    // Register health checks
    this.registerDefaultHealthChecks();
  }

  /**
   * Registers default health checks
   * @returns {void}
   */
  registerDefaultHealthChecks() {
    // Circuit breaker health
    this.healthMonitor.registerCheck('circuit-breaker', async () => {
      const state = this.circuitBreaker.getState();
      if (state === 'open') {
        throw new Error('Circuit breaker is open');
      }
    });

    // Recovery capacity
    this.healthMonitor.registerCheck('recovery-capacity', async () => {
      if (this.activeRecoveries.size >= this.config.maxConcurrentRecoveries) {
        throw new Error('Maximum concurrent recoveries reached');
      }
    });

    // Success rate
    this.healthMonitor.registerCheck('recovery-success-rate', async () => {
      const stats = this.getStats();
      if (stats.successRate < 0.5 && stats.totalAttempts > 10) {
        throw new Error(`Low success rate: ${(stats.successRate * 100).toFixed(1)}%`);
      }
    });
  }

  /**
   * Executes an operation with self-healing capabilities
   * @param {Function} operation - Async operation to execute
   * @param {Object} [options] - Execution options
   * @param {Function} [options.onRetry] - Retry callback
   * @param {Function} [options.fallback] - Fallback function
   * @param {Function} [options.compensationFn] - Compensation function
   * @param {Object} [options.workflow] - Workflow context
   * @returns {Promise<any>} Operation result
   * @throws {Error} If recovery fails
   * @example
   * const engine = new SelfHealingEngine();
   * const result = await engine.execute(async () => {
   *   return await fetch('https://api.example.com');
   * }, {
   *   fallback: () => cachedData
   * });
   */
  async execute(operation, options = {}) {
    // Check capacity
    if (this.activeRecoveries.size >= this.config.maxConcurrentRecoveries) {
      throw new Error('Maximum concurrent recoveries reached');
    }

    const recoveryId = this.generateRecoveryId();
    this.activeRecoveries.add(recoveryId);
    this.stats.totalAttempts++;

    const startTime = Date.now();
    let result;

    try {
      // Execute through circuit breaker
      result = await this.circuitBreaker.execute(async () => {
        // Execute with retry
        return await this.retry.execute(operation, {
          onRetry: options.onRetry
        });
      }, {
        fallback: options.fallback
      });

      this.stats.successfulRecoveries++;
      return result;
    } catch (error) {
      // Classify error
      const classified = this.classifier.classify(error);

      // Track error category
      const category = classified.category;
      this.stats.errorsByCategory[category] = (this.stats.errorsByCategory[category] || 0) + 1;

      // Attempt recovery
      try {
        result = await this.attemptRecovery(classified, {
          operation,
          ...options
        });

        this.stats.successfulRecoveries++;
        return result;
      } catch (recoveryError) {
        this.stats.failedRecoveries++;
        throw recoveryError;
      }
    } finally {
      const duration = Date.now() - startTime;
      this.stats.totalRecoveryTime += duration;
      this.activeRecoveries.delete(recoveryId);
    }
  }

  /**
   * Attempts recovery for a classified error
   * @param {Object} classifiedError - Classified error object
   * @param {Object} context - Recovery context
   * @returns {Promise<any>} Recovery result
   * @throws {Error} If recovery fails
   */
  async attemptRecovery(classifiedError, context) {
    const result = await this.recoveryExecutor.recover(classifiedError, context);

    // Track action type
    const actionType = classifiedError.retryable ? 'retry' : 'compensate';
    this.stats.actionsByType[actionType] = (this.stats.actionsByType[actionType] || 0) + 1;

    return result;
  }

  /**
   * Wraps a function with self-healing capabilities
   * @param {Function} fn - Function to wrap
   * @param {Object} [options] - Execution options
   * @returns {Function} Wrapped function
   */
  wrap(fn, options = {}) {
    return async (...args) => {
      return this.execute(() => fn(...args), options);
    };
  }

  /**
   * Starts health monitoring
   * @returns {void}
   */
  startHealthMonitoring() {
    this.healthMonitor.start();
  }

  /**
   * Stops health monitoring
   * @returns {void}
   */
  stopHealthMonitoring() {
    this.healthMonitor.stop();
  }

  /**
   * Gets recovery statistics
   * @returns {Object} Statistics object
   */
  getStats() {
    const { totalAttempts, successfulRecoveries, failedRecoveries, totalRecoveryTime } = this.stats;

    return {
      totalAttempts,
      successfulRecoveries,
      failedRecoveries,
      averageRecoveryTime: totalAttempts > 0 ? totalRecoveryTime / totalAttempts : 0,
      successRate: totalAttempts > 0 ? successfulRecoveries / totalAttempts : 0,
      errorsByCategory: { ...this.stats.errorsByCategory },
      actionsByType: { ...this.stats.actionsByType }
    };
  }

  /**
   * Resets statistics
   * @returns {void}
   */
  resetStats() {
    this.stats = {
      totalAttempts: 0,
      successfulRecoveries: 0,
      failedRecoveries: 0,
      totalRecoveryTime: 0,
      errorsByCategory: {},
      actionsByType: {}
    };
    this.recoveryExecutor.resetStats();
  }

  /**
   * Gets current health status
   * @returns {Promise<Object>} Health check result
   */
  async getHealth() {
    return this.healthMonitor.check();
  }

  /**
   * Checks if engine is healthy
   * @returns {boolean} True if healthy
   */
  isHealthy() {
    return this.healthMonitor.isHealthy();
  }

  /**
   * Registers a custom error pattern
   * @param {Object} pattern - Error pattern
   * @returns {void}
   */
  addErrorPattern(pattern) {
    this.classifier.addPattern(pattern);
  }

  /**
   * Registers a custom recovery action
   * @param {Object} action - Recovery action
   * @returns {void}
   */
  addRecoveryAction(action) {
    this.recoveryExecutor.register(action);
  }

  /**
   * Registers a custom health check
   * @param {string} name - Check name
   * @param {Function} checkFn - Check function
   * @param {Object} [options] - Check options
   * @returns {void}
   */
  addHealthCheck(name, checkFn, options) {
    this.healthMonitor.registerCheck(name, checkFn, options);
  }

  /**
   * Adds a health status change listener
   * @param {Function} listener - Listener function
   * @returns {Function} Unsubscribe function
   */
  onHealthChange(listener) {
    return this.healthMonitor.onStatusChange(listener);
  }

  /**
   * Gets circuit breaker state
   * @returns {string} Current state
   */
  getCircuitBreakerState() {
    return this.circuitBreaker.getState();
  }

  /**
   * Resets circuit breaker
   * @returns {void}
   */
  resetCircuitBreaker() {
    this.circuitBreaker.reset();
  }

  /**
   * Gets active recovery count
   * @returns {number} Number of active recoveries
   */
  getActiveRecoveryCount() {
    return this.activeRecoveries.size;
  }

  /**
   * Generates a unique recovery ID
   * @returns {string} Recovery ID
   */
  generateRecoveryId() {
    return `recovery-${Date.now()}-${Math.random().toString(36).slice(2, 11)}`;
  }

  /**
   * Gets comprehensive engine status
   * @returns {Object} Engine status
   */
  getStatus() {
    return {
      stats: this.getStats(),
      circuitBreaker: this.circuitBreaker.getStats(),
      health: this.healthMonitor.getStats(),
      activeRecoveries: this.activeRecoveries.size,
      maxConcurrentRecoveries: this.config.maxConcurrentRecoveries
    };
  }
}

/**
 * Creates a new self-healing engine instance
 * @param {Object} [config] - Engine configuration
 * @returns {SelfHealingEngine} Self-healing engine instance
 */
export function createSelfHealingEngine(config) {
  return new SelfHealingEngine(config);
}

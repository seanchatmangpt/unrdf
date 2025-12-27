/**
 * @fileoverview Circuit Breaker - Failure handling and recovery
 *
 * **Purpose**: Provide circuit breaker pattern for agent failures:
 * 1. Failure detection and tracking
 * 2. Circuit state management (closed, open, half-open)
 * 3. Automatic recovery attempts
 * 4. Failure rate monitoring
 *
 * **Properties**:
 * - Configurable thresholds
 * - Exponential backoff
 * - Health check probes
 * - Metrics and monitoring
 *
 * @module orchestration/circuit-breaker
 */

import { z } from 'zod';
import { EventEmitter } from 'events';

/**
 * Circuit state enum
 */
export const CircuitState = {
  CLOSED: 'closed',       // Normal operation
  OPEN: 'open',          // Failing, rejecting requests
  HALF_OPEN: 'half-open' // Testing if recovered
};

/**
 * Circuit breaker configuration schema
 */
export const CircuitBreakerConfigSchema = z.object({
  failureThreshold: z.number().min(1).default(5),
  successThreshold: z.number().min(1).default(2),
  timeout: z.number().default(60000),
  resetTimeout: z.number().default(30000),
  halfOpenRequests: z.number().default(3),
  monitoringWindow: z.number().default(60000)
});

/**
 * Circuit Breaker - Handles agent failures and recovery
 *
 * @class CircuitBreaker
 * @extends EventEmitter
 *
 * @example
 * const breaker = new CircuitBreaker({
 *   failureThreshold: 5,
 *   resetTimeout: 30000
 * });
 *
 * try {
 *   await breaker.execute(async () => {
 *     // Agent operation
 *     return await agent.process(task);
 *   });
 * } catch (error) {
 *   // Circuit open or operation failed
 * }
 */
export class CircuitBreaker extends EventEmitter {
  /**
   * Create a new circuit breaker
   *
   * @param {Object} config - Breaker configuration
   */
  constructor(config = {}) {
    super();

    /** @type {Object} Breaker configuration */
    this.config = CircuitBreakerConfigSchema.parse(config);

    /** @type {string} Current circuit state */
    this.state = CircuitState.CLOSED;

    /** @type {number} Consecutive failure count */
    this.failureCount = 0;

    /** @type {number} Consecutive success count (in half-open) */
    this.successCount = 0;

    /** @type {NodeJS.Timeout|null} Reset timer */
    this.resetTimer = null;

    /** @type {Array} Recent operations (for monitoring window) */
    this.recentOperations = [];

    /** @type {Object} Breaker statistics */
    this.stats = {
      totalRequests: 0,
      totalSuccess: 0,
      totalFailures: 0,
      totalRejected: 0,
      totalTimeouts: 0,
      stateTransitions: {
        closedToOpen: 0,
        openToHalfOpen: 0,
        halfOpenToClosed: 0,
        halfOpenToOpen: 0
      }
    };
  }

  /**
   * Execute operation through circuit breaker
   *
   * @param {Function} operation - Async operation to execute
   * @param {Object} [options] - Execution options
   * @param {number} [options.timeout] - Operation timeout override
   * @returns {Promise<*>} Operation result
   */
  async execute(operation, options = {}) {
    this.stats.totalRequests++;

    // Check circuit state
    if (this.state === CircuitState.OPEN) {
      this.stats.totalRejected++;
      throw new Error('Circuit breaker is OPEN');
    }

    // Execute operation with timeout
    const timeout = options.timeout || this.config.timeout;

    try {
      const result = await this._executeWithTimeout(operation, timeout);

      // Record success
      this._recordSuccess();

      return result;

    } catch (error) {
      // Record failure
      this._recordFailure(error);

      throw error;
    }
  }

  /**
   * Force circuit to open state
   *
   * @returns {void}
   */
  open() {
    if (this.state !== CircuitState.OPEN) {
      this._transitionTo(CircuitState.OPEN);
    }
  }

  /**
   * Force circuit to closed state
   *
   * @returns {void}
   */
  close() {
    if (this.state !== CircuitState.CLOSED) {
      this._transitionTo(CircuitState.CLOSED);
    }
  }

  /**
   * Reset circuit breaker
   *
   * @returns {void}
   */
  reset() {
    this.state = CircuitState.CLOSED;
    this.failureCount = 0;
    this.successCount = 0;
    this.recentOperations = [];

    if (this.resetTimer) {
      clearTimeout(this.resetTimer);
      this.resetTimer = null;
    }

    this.emit('breaker:reset');
  }

  /**
   * Get circuit state
   *
   * @returns {string} Current state
   */
  getState() {
    return this.state;
  }

  /**
   * Check if circuit is open
   *
   * @returns {boolean} Is open
   */
  isOpen() {
    return this.state === CircuitState.OPEN;
  }

  /**
   * Get circuit statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    const now = Date.now();
    const recentOps = this.recentOperations.filter(
      op => now - op.timestamp < this.config.monitoringWindow
    );

    const recentFailures = recentOps.filter(op => !op.success).length;
    const recentSuccesses = recentOps.filter(op => op.success).length;

    return {
      ...this.stats,
      state: this.state,
      failureCount: this.failureCount,
      successCount: this.successCount,
      recentFailureRate: recentOps.length > 0
        ? ((recentFailures / recentOps.length) * 100).toFixed(2) + '%'
        : 'N/A',
      successRate: this.stats.totalRequests > 0
        ? ((this.stats.totalSuccess / this.stats.totalRequests) * 100).toFixed(2) + '%'
        : 'N/A'
    };
  }

  /**
   * Execute operation with timeout
   *
   * @param {Function} operation - Operation
   * @param {number} timeout - Timeout ms
   * @returns {Promise<*>} Result
   * @private
   */
  async _executeWithTimeout(operation, timeout) {
    return Promise.race([
      operation(),
      new Promise((_, reject) =>
        setTimeout(() => {
          this.stats.totalTimeouts++;
          reject(new Error(`Operation timeout after ${timeout}ms`));
        }, timeout)
      )
    ]);
  }

  /**
   * Record successful operation
   *
   * @private
   */
  _recordSuccess() {
    this.stats.totalSuccess++;
    this.failureCount = 0;

    // Track recent operation
    this._addRecentOperation(true);

    if (this.state === CircuitState.HALF_OPEN) {
      this.successCount++;

      // Check if enough successes to close circuit
      if (this.successCount >= this.config.successThreshold) {
        this._transitionTo(CircuitState.CLOSED);
      }
    }
  }

  /**
   * Record failed operation
   *
   * @param {Error} error - Failure error
   * @private
   */
  _recordFailure(error) {
    this.stats.totalFailures++;
    this.failureCount++;
    this.successCount = 0;

    // Track recent operation
    this._addRecentOperation(false);

    // Check if should open circuit
    if (this.state === CircuitState.CLOSED) {
      if (this.failureCount >= this.config.failureThreshold) {
        this._transitionTo(CircuitState.OPEN);
      }
    } else if (this.state === CircuitState.HALF_OPEN) {
      // Any failure in half-open returns to open
      this._transitionTo(CircuitState.OPEN);
    }
  }

  /**
   * Add recent operation to monitoring window
   *
   * @param {boolean} success - Operation success
   * @private
   */
  _addRecentOperation(success) {
    const now = Date.now();

    this.recentOperations.push({
      success,
      timestamp: now
    });

    // Cleanup old operations
    this.recentOperations = this.recentOperations.filter(
      op => now - op.timestamp < this.config.monitoringWindow
    );
  }

  /**
   * Transition to new state
   *
   * @param {string} newState - New state
   * @private
   */
  _transitionTo(newState) {
    const oldState = this.state;

    if (oldState === newState) {
      return;
    }

    this.state = newState;

    // Track transition
    const transitionKey = `${oldState}To${newState.charAt(0).toUpperCase()}${newState.slice(1).replace('-', '')}`;
    if (this.stats.stateTransitions[transitionKey] !== undefined) {
      this.stats.stateTransitions[transitionKey]++;
    }

    this.emit('state:changed', {
      from: oldState,
      to: newState,
      failureCount: this.failureCount
    });

    // Handle state-specific logic
    if (newState === CircuitState.OPEN) {
      this._scheduleReset();
    } else if (newState === CircuitState.CLOSED) {
      this.failureCount = 0;
      this.successCount = 0;
      if (this.resetTimer) {
        clearTimeout(this.resetTimer);
        this.resetTimer = null;
      }
    } else if (newState === CircuitState.HALF_OPEN) {
      this.successCount = 0;
    }
  }

  /**
   * Schedule automatic reset attempt
   *
   * @private
   */
  _scheduleReset() {
    if (this.resetTimer) {
      clearTimeout(this.resetTimer);
    }

    this.resetTimer = setTimeout(() => {
      this._transitionTo(CircuitState.HALF_OPEN);
    }, this.config.resetTimeout);

    this.emit('reset:scheduled', {
      timeout: this.config.resetTimeout
    });
  }
}

/**
 * Create a circuit breaker
 *
 * @param {Object} [config] - Breaker configuration
 * @returns {CircuitBreaker}
 */
export function createCircuitBreaker(config = {}) {
  return new CircuitBreaker(config);
}

/**
 * Wrap function with circuit breaker
 *
 * @param {Function} fn - Function to wrap
 * @param {Object} [config] - Breaker configuration
 * @returns {Function} Wrapped function
 *
 * @example
 * const protectedFn = withCircuitBreaker(async (data) => {
 *   return await riskyOperation(data);
 * }, { failureThreshold: 3 });
 *
 * const result = await protectedFn(data);
 */
export function withCircuitBreaker(fn, config = {}) {
  const breaker = new CircuitBreaker(config);

  return async (...args) => {
    return breaker.execute(() => fn(...args));
  };
}

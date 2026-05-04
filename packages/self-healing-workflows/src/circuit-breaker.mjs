/**
 * @file Circuit breaker pattern implementation
 * @module @unrdf/self-healing-workflows/circuit-breaker
 * @description Implements circuit breaker pattern for fault tolerance
 */

import {
  CircuitBreakerConfigSchema
} from './schemas.mjs';

/**
 * Circuit breaker for fault tolerance
 */
export class CircuitBreaker {
  /**
   * Creates a new circuit breaker
   * @param {Object} [config] - Circuit breaker configuration
   * @param {number} [config.failureThreshold=5] - Failures before opening
   * @param {number} [config.successThreshold=2] - Successes before closing
   * @param {number} [config.timeout=60000] - Timeout in ms
   * @param {number} [config.resetTimeout=30000] - Reset timeout in ms
   * @param {number} [config.monitoringPeriod=10000] - Monitoring window in ms
   */
  constructor(config = {}) {
    this.config = CircuitBreakerConfigSchema.parse(config);
    this.state = 'closed';
    this.failures = 0;
    this.successes = 0;
    this.lastFailureTime = null;
    this.nextAttemptTime = null;
    this.stats = {
      totalRequests: 0,
      successfulRequests: 0,
      failedRequests: 0,
      rejectedRequests: 0
    };
  }

  /**
   * Executes an operation through the circuit breaker
   * @param {Function} operation - Async operation to execute
   * @param {Object} [options] - Execution options
   * @param {Function} [options.fallback] - Fallback function
   * @returns {Promise<any>} Operation result
   * @throws {Error} If circuit is open or operation fails
   * @example
   * const breaker = new CircuitBreaker({ failureThreshold: 3 });
   * const result = await breaker.execute(async () => {
   *   return await fetch('https://api.example.com');
   * });
   */
  async execute(operation, options = {}) {
    this.stats.totalRequests++;

    // Check if circuit is open
    if (this.state === 'open') {
      // Check if reset timeout has elapsed
      const now = Date.now();
      if (this.nextAttemptTime && now >= this.nextAttemptTime) {
        this.state = 'half-open';
        this.successes = 0;
      } else {
        this.stats.rejectedRequests++;

        // Use fallback if available
        if (options.fallback) {
          return options.fallback();
        }

        const error = new Error('Circuit breaker is OPEN');
        error.state = this.state;
        error.nextAttemptTime = this.nextAttemptTime;
        throw error;
      }
    }

    try {
      // Execute operation with timeout
      const result = await this.executeWithTimeout(operation);

      this.onSuccess();
      return result;
    } catch (error) {
      this.onFailure();

      // Use fallback if available
      if (options.fallback) {
        return options.fallback();
      }

      throw error;
    }
  }

  /**
   * Executes operation with timeout
   * @param {Function} operation - Operation to execute
   * @returns {Promise<any>} Operation result
   * @throws {Error} If operation times out
   */
  async executeWithTimeout(operation) {
    const { timeout } = this.config;

    return Promise.race([
      operation(),
      new Promise((_, reject) => {
        setTimeout(() => {
          reject(new Error(`Operation timed out after ${timeout}ms`));
        }, timeout);
      })
    ]);
  }

  /**
   * Handles successful operation
   * @returns {void}
   */
  onSuccess() {
    this.stats.successfulRequests++;
    this.failures = 0;

    if (this.state === 'half-open') {
      this.successes++;
      if (this.successes >= this.config.successThreshold) {
        this.close();
      }
    }
  }

  /**
   * Handles failed operation
   * @returns {void}
   */
  onFailure() {
    this.stats.failedRequests++;
    this.failures++;
    this.lastFailureTime = Date.now();

    if (this.state === 'half-open') {
      this.open();
    } else if (this.failures >= this.config.failureThreshold) {
      this.open();
    }
  }

  /**
   * Opens the circuit breaker
   * @returns {void}
   */
  open() {
    this.state = 'open';
    this.nextAttemptTime = Date.now() + this.config.resetTimeout;
  }

  /**
   * Closes the circuit breaker
   * @returns {void}
   */
  close() {
    this.state = 'closed';
    this.failures = 0;
    this.successes = 0;
    this.nextAttemptTime = null;
  }

  /**
   * Resets the circuit breaker to initial state
   * @returns {void}
   */
  reset() {
    this.state = 'closed';
    this.failures = 0;
    this.successes = 0;
    this.lastFailureTime = null;
    this.nextAttemptTime = null;
  }

  /**
   * Gets current circuit breaker state
   * @returns {string} Current state (closed, open, half-open)
   */
  getState() {
    return this.state;
  }

  /**
   * Gets circuit breaker statistics
   * @returns {Object} Statistics object
   */
  getStats() {
    const { totalRequests, successfulRequests, failedRequests, rejectedRequests } = this.stats;

    return {
      ...this.stats,
      successRate: totalRequests > 0 ? successfulRequests / totalRequests : 0,
      failureRate: totalRequests > 0 ? failedRequests / totalRequests : 0,
      rejectionRate: totalRequests > 0 ? rejectedRequests / totalRequests : 0,
      currentState: this.state,
      failures: this.failures,
      successes: this.successes
    };
  }

  /**
   * Checks if circuit is allowing requests
   * @returns {boolean} True if requests are allowed
   */
  isAllowingRequests() {
    if (this.state === 'closed') {
      return true;
    }

    if (this.state === 'half-open') {
      return true;
    }

    // Open state - check if reset timeout elapsed
    const now = Date.now();
    return this.nextAttemptTime && now >= this.nextAttemptTime;
  }

  /**
   * Wraps a function with circuit breaker
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
   * Updates circuit breaker configuration
   * @param {Object} updates - Configuration updates
   * @returns {void}
   */
  updateConfig(updates) {
    this.config = CircuitBreakerConfigSchema.parse({
      ...this.config,
      ...updates
    });
  }

  /**
   * Gets current configuration
   * @returns {Object} Current configuration
   */
  getConfig() {
    return { ...this.config };
  }
}

/**
 * Creates a new circuit breaker instance
 * @param {Object} [config] - Circuit breaker configuration
 * @returns {CircuitBreaker} Circuit breaker instance
 */
export function createCircuitBreaker(config) {
  return new CircuitBreaker(config);
}

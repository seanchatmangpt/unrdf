/**
 * @fileoverview Circuit Breaker pattern for fault tolerance
 * @module circuit-breaker
 * @description Prevents cascading failures by monitoring and stopping calls to failing services
 */

/**
 * Circuit states enum
 * @enum {string}
 */
export const CircuitState = {
  CLOSED: 'CLOSED',
  OPEN: 'OPEN',
  HALF_OPEN: 'HALF_OPEN',
};

/**
 * Error thrown when circuit is open
 */
export class CircuitOpenError extends Error {
  constructor(message = 'Circuit is open') {
    super(message);
    this.name = 'CircuitOpenError';
  }
}

/**
 * Circuit Breaker implementation
 * @class
 * @description Monitors operation execution and trips on repeated failures
 */
export class CircuitBreaker {
  /**
   * Creates a new CircuitBreaker instance
   * @param {Object} options - Configuration options
   * @param {number} [options.failureThreshold=5] - Failures before opening
   * @param {number} [options.resetTimeout=60000] - Timeout before half-open (ms)
   * @param {string} [options.name='circuit-breaker'] - Circuit name for logging
   */
  constructor(options = {}) {
    this.failureThreshold = options.failureThreshold || 5;
    this.resetTimeout = options.resetTimeout || 60000;
    this.name = options.name || 'circuit-breaker';

    this.state = CircuitState.CLOSED;
    this.failureCount = 0;
    this.successCount = 0;
    this.lastFailureTime = null;
    this.nextAttemptTime = null;
  }

  /**
   * Executes an async function with circuit breaker protection
   * @param {Function} fn - Async function to execute
   * @returns {Promise} Function result or throws CircuitOpenError
   * @throws {CircuitOpenError} If circuit is open
   */
  async execute(fn) {
    if (this.state === CircuitState.OPEN) {
      const now = Date.now();
      if (now < this.nextAttemptTime) {
        throw new CircuitOpenError(
          `Circuit is open for ${this.name}. Next attempt in ${this.nextAttemptTime - now}ms`
        );
      }
      // Try half-open state
      this.state = CircuitState.HALF_OPEN;
    }

    try {
      const result = await fn();

      // Success - reset state
      if (this.state === CircuitState.HALF_OPEN) {
        this.state = CircuitState.CLOSED;
        this.failureCount = 0;
        this.successCount = 0;
      } else if (this.state === CircuitState.CLOSED) {
        this.failureCount = 0;
      }

      return result;
    } catch (error) {
      this.failureCount++;
      this.lastFailureTime = Date.now();

      if (this.failureCount >= this.failureThreshold) {
        this.state = CircuitState.OPEN;
        this.nextAttemptTime = this.lastFailureTime + this.resetTimeout;
        throw new CircuitOpenError(
          `Circuit breaker ${this.name} is now open after ${this.failureCount} failures`
        );
      }

      if (this.state === CircuitState.HALF_OPEN) {
        // Failed in half-open, go back to open
        this.state = CircuitState.OPEN;
        this.nextAttemptTime = this.lastFailureTime + this.resetTimeout;
        throw new CircuitOpenError(
          `Circuit breaker ${this.name} reopened after failure in HALF_OPEN state`
        );
      }

      throw error;
    }
  }

  /**
   * Gets current circuit state
   * @returns {string} Current state
   */
  getState() {
    return this.state;
  }

  /**
   * Resets the circuit breaker
   */
  reset() {
    this.state = CircuitState.CLOSED;
    this.failureCount = 0;
    this.successCount = 0;
    this.lastFailureTime = null;
    this.nextAttemptTime = null;
  }
}

export default { CircuitBreaker, CircuitState, CircuitOpenError };

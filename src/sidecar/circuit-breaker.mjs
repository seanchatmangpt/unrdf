/**
 * @file Circuit breaker implementation for fault tolerance
 * @module sidecar/circuit-breaker
 *
 * @description
 * Implements circuit breaker pattern to prevent cascade failures.
 * States: CLOSED -> OPEN -> HALF_OPEN -> CLOSED
 */

import { z } from 'zod';
import { EventEmitter } from 'events';

/**
 * Circuit breaker states
 */
export const CircuitState = {
  CLOSED: 'CLOSED',
  OPEN: 'OPEN',
  HALF_OPEN: 'HALF_OPEN'
};

/**
 * Circuit breaker configuration schema
 */
const CircuitBreakerConfigSchema = z.object({
  threshold: z.number().int().positive().default(5),
  resetTimeout: z.number().int().positive().default(30000),
  halfOpenRequests: z.number().int().positive().default(3),
  rollingWindow: z.number().int().positive().default(60000),
  errorThresholdPercentage: z.number().min(0).max(100).default(50)
});

/**
 * Circuit breaker for preventing cascade failures
 */
export class CircuitBreaker extends EventEmitter {
  /**
   * Create a new circuit breaker
   * @param {Object} [config] - Circuit breaker configuration
   */
  constructor(config = {}) {
    super();
    this.config = CircuitBreakerConfigSchema.parse(config);
    this.state = CircuitState.CLOSED;
    this.failures = 0;
    this.successes = 0;
    this.halfOpenAttempts = 0;
    this.lastFailureTime = null;
    this.resetTimer = null;
    this.metrics = {
      requests: [],
      totalRequests: 0,
      totalFailures: 0,
      totalSuccesses: 0,
      stateTransitions: []
    };
  }

  /**
   * Execute function with circuit breaker protection
   * @param {Function} fn - Function to execute
   * @param {...any} args - Function arguments
   * @returns {Promise<any>} Function result
   * @throws {Error} If circuit is open or function fails
   */
  async execute(fn, ...args) {
    if (this.state === CircuitState.OPEN) {
      if (Date.now() - this.lastFailureTime >= this.config.resetTimeout) {
        this._transitionTo(CircuitState.HALF_OPEN);
      } else {
        const error = new Error('Circuit breaker is OPEN');
        error.code = 'CIRCUIT_OPEN';
        this._recordMetric(false, 0);
        throw error;
      }
    }

    const startTime = Date.now();

    try {
      const result = await fn(...args);
      const duration = Date.now() - startTime;
      this._onSuccess(duration);
      return result;
    } catch (error) {
      const duration = Date.now() - startTime;
      this._onFailure(duration);
      throw error;
    }
  }

  /**
   * Handle successful execution
   * @param {number} duration - Execution duration
   * @private
   */
  _onSuccess(duration) {
    this.successes++;
    this._recordMetric(true, duration);

    if (this.state === CircuitState.HALF_OPEN) {
      this.halfOpenAttempts++;

      if (this.halfOpenAttempts >= this.config.halfOpenRequests) {
        this._transitionTo(CircuitState.CLOSED);
      }
    } else if (this.state === CircuitState.CLOSED) {
      this.failures = 0;
    }

    this.emit('success', { state: this.state, duration });
  }

  /**
   * Handle failed execution
   * @param {number} duration - Execution duration
   * @private
   */
  _onFailure(duration) {
    this.failures++;
    this.lastFailureTime = Date.now();
    this._recordMetric(false, duration);

    if (this.state === CircuitState.HALF_OPEN) {
      this._transitionTo(CircuitState.OPEN);
    } else if (this.state === CircuitState.CLOSED) {
      if (this._shouldOpen()) {
        this._transitionTo(CircuitState.OPEN);
      }
    }

    this.emit('failure', { state: this.state, failures: this.failures, duration });
  }

  /**
   * Check if circuit should open
   * @returns {boolean} True if circuit should open
   * @private
   */
  _shouldOpen() {
    // Simple threshold-based check
    if (this.failures >= this.config.threshold) {
      return true;
    }

    // Percentage-based check within rolling window
    const recentRequests = this._getRecentRequests();
    if (recentRequests.length === 0) {
      return false;
    }

    const failedRequests = recentRequests.filter(r => !r.success).length;
    const errorPercentage = (failedRequests / recentRequests.length) * 100;

    return errorPercentage >= this.config.errorThresholdPercentage;
  }

  /**
   * Get recent requests within rolling window
   * @returns {Array} Recent requests
   * @private
   */
  _getRecentRequests() {
    const cutoff = Date.now() - this.config.rollingWindow;
    return this.metrics.requests.filter(r => r.timestamp >= cutoff);
  }

  /**
   * Transition to a new state
   * @param {string} newState - New circuit state
   * @private
   */
  _transitionTo(newState) {
    const oldState = this.state;
    this.state = newState;

    // Record state transition
    this.metrics.stateTransitions.push({
      from: oldState,
      to: newState,
      timestamp: Date.now()
    });

    // Reset counters based on new state
    if (newState === CircuitState.CLOSED) {
      this.failures = 0;
      this.successes = 0;
      this.halfOpenAttempts = 0;
      this._clearResetTimer();
    } else if (newState === CircuitState.HALF_OPEN) {
      this.halfOpenAttempts = 0;
    } else if (newState === CircuitState.OPEN) {
      this._scheduleReset();
    }

    this.emit('stateChange', { from: oldState, to: newState });
  }

  /**
   * Schedule circuit reset after timeout
   * @private
   */
  _scheduleReset() {
    this._clearResetTimer();

    this.resetTimer = setTimeout(() => {
      if (this.state === CircuitState.OPEN) {
        this._transitionTo(CircuitState.HALF_OPEN);
      }
    }, this.config.resetTimeout);
  }

  /**
   * Clear reset timer
   * @private
   */
  _clearResetTimer() {
    if (this.resetTimer) {
      clearTimeout(this.resetTimer);
      this.resetTimer = null;
    }
  }

  /**
   * Record metric for request
   * @param {boolean} success - Request success
   * @param {number} duration - Request duration
   * @private
   */
  _recordMetric(success, duration) {
    const metric = {
      success,
      duration,
      timestamp: Date.now(),
      state: this.state
    };

    this.metrics.requests.push(metric);
    this.metrics.totalRequests++;

    if (success) {
      this.metrics.totalSuccesses++;
    } else {
      this.metrics.totalFailures++;
    }

    // Cleanup old metrics
    this._cleanupOldMetrics();
  }

  /**
   * Cleanup old metrics outside rolling window
   * @private
   */
  _cleanupOldMetrics() {
    const cutoff = Date.now() - this.config.rollingWindow;
    this.metrics.requests = this.metrics.requests.filter(r => r.timestamp >= cutoff);
  }

  /**
   * Get current state
   * @returns {string} Current circuit state
   */
  getState() {
    return this.state;
  }

  /**
   * Check if circuit is open
   * @returns {boolean} True if circuit is open
   */
  isOpen() {
    return this.state === CircuitState.OPEN;
  }

  /**
   * Check if circuit is closed
   * @returns {boolean} True if circuit is closed
   */
  isClosed() {
    return this.state === CircuitState.CLOSED;
  }

  /**
   * Check if circuit is half-open
   * @returns {boolean} True if circuit is half-open
   */
  isHalfOpen() {
    return this.state === CircuitState.HALF_OPEN;
  }

  /**
   * Get circuit breaker metrics
   * @returns {Object} Circuit breaker metrics
   */
  getMetrics() {
    const recentRequests = this._getRecentRequests();
    const recentFailures = recentRequests.filter(r => !r.success).length;
    const errorRate = recentRequests.length > 0
      ? (recentFailures / recentRequests.length) * 100
      : 0;

    return {
      state: this.state,
      failures: this.failures,
      successes: this.successes,
      halfOpenAttempts: this.halfOpenAttempts,
      totalRequests: this.metrics.totalRequests,
      totalFailures: this.metrics.totalFailures,
      totalSuccesses: this.metrics.totalSuccesses,
      recentRequests: recentRequests.length,
      recentFailures,
      errorRate,
      stateTransitions: this.metrics.stateTransitions.length,
      lastFailureTime: this.lastFailureTime
    };
  }

  /**
   * Force circuit open
   */
  forceOpen() {
    this._transitionTo(CircuitState.OPEN);
  }

  /**
   * Force circuit closed
   */
  forceClosed() {
    this._transitionTo(CircuitState.CLOSED);
  }

  /**
   * Reset circuit breaker
   */
  reset() {
    this.failures = 0;
    this.successes = 0;
    this.halfOpenAttempts = 0;
    this.lastFailureTime = null;
    this._clearResetTimer();
    this._transitionTo(CircuitState.CLOSED);

    // Keep historical metrics but clear recent ones
    this.metrics.requests = [];
  }

  /**
   * Cleanup circuit breaker
   */
  cleanup() {
    this._clearResetTimer();
    this.removeAllListeners();
  }
}

/**
 * Create a circuit breaker
 * @param {Object} [config] - Circuit breaker configuration
 * @returns {CircuitBreaker} Circuit breaker instance
 */
export function createCircuitBreaker(config) {
  return new CircuitBreaker(config);
}

export default CircuitBreaker;

/**
 * @file Circuit Breaker Pattern Implementation
 * @module @unrdf/daemon/degradation/circuit-breaker
 * @description Production-ready circuit breaker with CLOSED, OPEN, HALF_OPEN states,
 * failure threshold configuration, timeout-based state transitions, and OTEL observability.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import {
  CircuitBreakerConfigSchema,
  CircuitBreakerMetricsSchema,
  CircuitBreakerResultSchema,
  CircuitStateSchema,
} from './degradation.schema.mjs';

const tracer = trace.getTracer('@unrdf/daemon/circuit-breaker');

/**
 * Circuit Breaker States
 * @readonly
 * @enum {string}
 */
export const CircuitState = {
  /** Circuit is closed, requests flow through normally */
  CLOSED: 'CLOSED',
  /** Circuit is open, requests are rejected immediately */
  OPEN: 'OPEN',
  /** Circuit is testing, limited requests allowed through */
  HALF_OPEN: 'HALF_OPEN',
};

/**
 * Circuit Breaker Error
 * Thrown when circuit is open and rejecting requests
 */
export class CircuitBreakerError extends Error {
  /**
   * @param {string} message - Error message
   * @param {string} state - Current circuit state
   * @param {string} circuitName - Name of the circuit
   */
  constructor(message, state, circuitName) {
    super(message);
    this.name = 'CircuitBreakerError';
    this.state = state;
    this.circuitName = circuitName;
    this.timestamp = Date.now();
  }
}

/**
 * Circuit Breaker Implementation
 * Prevents cascading failures by stopping calls to failing services
 * @example
 * const breaker = new CircuitBreaker({
 *   name: 'database',
 *   failureThreshold: 5,
 *   timeout: 30000
 * });
 *
 * const result = await breaker.execute(async () => {
 *   return await db.query('SELECT * FROM users');
 * });
 */
export class CircuitBreaker {
  /**
   * Create a circuit breaker
   * @param {Object} config - Circuit breaker configuration
   * @param {string} [config.name='default'] - Circuit name for identification
   * @param {number} [config.failureThreshold=5] - Failures before opening circuit
   * @param {number} [config.successThreshold=3] - Successes in HALF_OPEN to close
   * @param {number} [config.timeout=30000] - Call timeout in milliseconds
   * @param {number} [config.halfOpenMaxCalls=3] - Max concurrent calls in HALF_OPEN
   * @param {number} [config.volumeThreshold=10] - Minimum calls before evaluating
   * @param {number} [config.errorPercentageThreshold=50] - Error percentage to open
   * @param {number} [config.resetTimeout=60000] - Time in OPEN before HALF_OPEN
   * @param {Function} [config.onStateChange] - Callback on state changes
   */
  constructor(config = {}) {
    const validated = CircuitBreakerConfigSchema.parse(config);

    this.name = validated.name;
    this.failureThreshold = validated.failureThreshold;
    this.successThreshold = validated.successThreshold;
    this.timeout = validated.timeout;
    this.halfOpenMaxCalls = validated.halfOpenMaxCalls;
    this.volumeThreshold = validated.volumeThreshold;
    this.errorPercentageThreshold = validated.errorPercentageThreshold;
    this.resetTimeout = validated.resetTimeout;
    this.monitorInterval = validated.monitorInterval;

    this.onStateChange = validated.onStateChange;
    this.onFailure = validated.onFailure;
    this.onSuccess = validated.onSuccess;

    this._state = CircuitState.CLOSED;
    this._lastFailureTime = null;
    this._lastSuccessTime = null;
    this._consecutiveFailures = 0;
    this._consecutiveSuccesses = 0;
    this._halfOpenCalls = 0;
    this._stateChanges = 0;

    this._metrics = {
      totalCalls: 0,
      successfulCalls: 0,
      failedCalls: 0,
      rejectedCalls: 0,
    };

    this._windowStart = Date.now();
    this._windowCalls = [];

    this._eventListeners = new Map();
  }

  /**
   * Get current circuit state
   * @returns {string} Current state (CLOSED, OPEN, HALF_OPEN)
   */
  get state() {
    this._evaluateState();
    return this._state;
  }

  /**
   * Check if circuit is closed (healthy)
   * @returns {boolean}
   */
  get isClosed() {
    return this.state === CircuitState.CLOSED;
  }

  /**
   * Check if circuit is open (failing)
   * @returns {boolean}
   */
  get isOpen() {
    return this.state === CircuitState.OPEN;
  }

  /**
   * Check if circuit is half-open (testing)
   * @returns {boolean}
   */
  get isHalfOpen() {
    return this.state === CircuitState.HALF_OPEN;
  }

  /**
   * Evaluate and potentially transition state
   * @private
   */
  _evaluateState() {
    if (this._state === CircuitState.OPEN) {
      const timeSinceFailure = Date.now() - this._lastFailureTime;
      if (timeSinceFailure >= this.resetTimeout) {
        this._transitionTo(CircuitState.HALF_OPEN);
      }
    }
  }

  /**
   * Transition to a new state
   * @private
   * @param {string} newState - Target state
   */
  _transitionTo(newState) {
    const span = tracer.startSpan('circuit_breaker.state_transition', {
      attributes: {
        'circuit.name': this.name,
        'circuit.from_state': this._state,
        'circuit.to_state': newState,
      },
    });

    try {
      const oldState = this._state;
      this._state = newState;
      this._stateChanges++;

      if (newState === CircuitState.HALF_OPEN) {
        this._halfOpenCalls = 0;
        this._consecutiveSuccesses = 0;
      } else if (newState === CircuitState.CLOSED) {
        this._consecutiveFailures = 0;
        this._windowCalls = [];
        this._windowStart = Date.now();
      }

      span.setAttribute('circuit.state_changes', this._stateChanges);

      this._emit('stateChange', { from: oldState, to: newState });

      if (this.onStateChange) {
        this.onStateChange(oldState, newState);
      }

      span.setStatus({ code: SpanStatusCode.OK });
    } catch (error) {
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Record a successful call
   * @private
   */
  _recordSuccess() {
    this._metrics.totalCalls++;
    this._metrics.successfulCalls++;
    this._consecutiveSuccesses++;
    this._consecutiveFailures = 0;
    this._lastSuccessTime = Date.now();

    this._windowCalls.push({ success: true, timestamp: Date.now() });
    this._pruneWindow();

    if (this.onSuccess) {
      this.onSuccess();
    }

    if (this._state === CircuitState.HALF_OPEN) {
      if (this._consecutiveSuccesses >= this.successThreshold) {
        this._transitionTo(CircuitState.CLOSED);
      }
    }
  }

  /**
   * Record a failed call
   * @private
   * @param {Error} error - The error that occurred
   */
  _recordFailure(error) {
    this._metrics.totalCalls++;
    this._metrics.failedCalls++;
    this._consecutiveFailures++;
    this._consecutiveSuccesses = 0;
    this._lastFailureTime = Date.now();

    this._windowCalls.push({ success: false, timestamp: Date.now() });
    this._pruneWindow();

    if (this.onFailure) {
      this.onFailure(error);
    }

    this._emit('failure', { error, consecutiveFailures: this._consecutiveFailures });

    if (this._state === CircuitState.CLOSED) {
      if (this._shouldOpen()) {
        this._transitionTo(CircuitState.OPEN);
      }
    } else if (this._state === CircuitState.HALF_OPEN) {
      this._transitionTo(CircuitState.OPEN);
    }
  }

  /**
   * Prune old calls from sliding window
   * @private
   */
  _pruneWindow() {
    const cutoff = Date.now() - this.resetTimeout;
    this._windowCalls = this._windowCalls.filter(call => call.timestamp > cutoff);
  }

  /**
   * Check if circuit should open based on thresholds
   * @private
   * @returns {boolean}
   */
  _shouldOpen() {
    if (this._windowCalls.length < this.volumeThreshold) {
      return this._consecutiveFailures >= this.failureThreshold;
    }

    const failures = this._windowCalls.filter(c => !c.success).length;
    const errorPercentage = (failures / this._windowCalls.length) * 100;

    return errorPercentage >= this.errorPercentageThreshold ||
           this._consecutiveFailures >= this.failureThreshold;
  }

  /**
   * Execute a function through the circuit breaker
   * @param {Function} fn - Async function to execute
   * @param {Object} [context] - Optional execution context
   * @returns {Promise<Object>} Execution result
   * @throws {CircuitBreakerError} When circuit is open
   * @example
   * const result = await breaker.execute(async () => {
   *   return await fetch('https://api.example.com/data');
   * });
   */
  async execute(fn, context = {}) {
    const span = tracer.startSpan('circuit_breaker.execute', {
      attributes: {
        'circuit.name': this.name,
        'circuit.state': this._state,
      },
    });

    const startTime = Date.now();

    try {
      this._evaluateState();

      if (this._state === CircuitState.OPEN) {
        this._metrics.rejectedCalls++;
        span.setAttribute('circuit.rejected', true);
        span.setStatus({ code: SpanStatusCode.ERROR, message: 'Circuit is OPEN' });

        const error = new CircuitBreakerError(
          `Circuit breaker '${this.name}' is OPEN`,
          this._state,
          this.name
        );

        return CircuitBreakerResultSchema.parse({
          success: false,
          error,
          duration: Date.now() - startTime,
          state: this._state,
          rejected: true,
        });
      }

      if (this._state === CircuitState.HALF_OPEN) {
        if (this._halfOpenCalls >= this.halfOpenMaxCalls) {
          this._metrics.rejectedCalls++;
          span.setAttribute('circuit.rejected', true);

          const error = new CircuitBreakerError(
            `Circuit breaker '${this.name}' HALF_OPEN limit reached`,
            this._state,
            this.name
          );

          return CircuitBreakerResultSchema.parse({
            success: false,
            error,
            duration: Date.now() - startTime,
            state: this._state,
            rejected: true,
          });
        }
        this._halfOpenCalls++;
      }

      const result = await this._executeWithTimeout(fn, context);

      this._recordSuccess();
      span.setAttribute('circuit.success', true);
      span.setStatus({ code: SpanStatusCode.OK });

      return CircuitBreakerResultSchema.parse({
        success: true,
        result,
        duration: Date.now() - startTime,
        state: this._state,
        rejected: false,
      });
    } catch (error) {
      this._recordFailure(error);
      span.setAttribute('circuit.success', false);
      span.setAttribute('circuit.error', error.message);
      span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });

      return CircuitBreakerResultSchema.parse({
        success: false,
        error,
        duration: Date.now() - startTime,
        state: this._state,
        rejected: false,
      });
    } finally {
      span.end();
    }
  }

  /**
   * Execute function with timeout
   * @private
   * @param {Function} fn - Function to execute
   * @param {Object} context - Execution context
   * @returns {Promise<any>} Function result
   */
  async _executeWithTimeout(fn, context) {
    return new Promise((resolve, reject) => {
      const timeoutId = setTimeout(() => {
        reject(new Error(`Circuit breaker '${this.name}' execution timeout after ${this.timeout}ms`));
      }, this.timeout);

      Promise.resolve(fn(context))
        .then(result => {
          clearTimeout(timeoutId);
          resolve(result);
        })
        .catch(error => {
          clearTimeout(timeoutId);
          reject(error);
        });
    });
  }

  /**
   * Force circuit to open state
   * @example
   * breaker.forceOpen();
   */
  forceOpen() {
    this._transitionTo(CircuitState.OPEN);
    this._lastFailureTime = Date.now();
  }

  /**
   * Force circuit to closed state
   * @example
   * breaker.forceClosed();
   */
  forceClosed() {
    this._transitionTo(CircuitState.CLOSED);
  }

  /**
   * Force circuit to half-open state
   * @example
   * breaker.forceHalfOpen();
   */
  forceHalfOpen() {
    this._transitionTo(CircuitState.HALF_OPEN);
  }

  /**
   * Reset circuit to initial state
   * @example
   * breaker.reset();
   */
  reset() {
    this._state = CircuitState.CLOSED;
    this._lastFailureTime = null;
    this._lastSuccessTime = null;
    this._consecutiveFailures = 0;
    this._consecutiveSuccesses = 0;
    this._halfOpenCalls = 0;
    this._windowCalls = [];
    this._windowStart = Date.now();

    this._metrics = {
      totalCalls: 0,
      successfulCalls: 0,
      failedCalls: 0,
      rejectedCalls: 0,
    };

    this._emit('reset', {});
  }

  /**
   * Get circuit breaker metrics
   * @returns {Object} Metrics snapshot
   * @example
   * const metrics = breaker.getMetrics();
   * console.log(`Success rate: ${metrics.successfulCalls / metrics.totalCalls}`);
   */
  getMetrics() {
    return CircuitBreakerMetricsSchema.parse({
      totalCalls: this._metrics.totalCalls,
      successfulCalls: this._metrics.successfulCalls,
      failedCalls: this._metrics.failedCalls,
      rejectedCalls: this._metrics.rejectedCalls,
      lastFailureTime: this._lastFailureTime,
      lastSuccessTime: this._lastSuccessTime,
      currentState: this._state,
      stateChanges: this._stateChanges,
      consecutiveFailures: this._consecutiveFailures,
      consecutiveSuccesses: this._consecutiveSuccesses,
    });
  }

  /**
   * Add event listener
   * @param {string} event - Event name (stateChange, failure, reset)
   * @param {Function} callback - Event callback
   * @returns {Function} Unsubscribe function
   */
  on(event, callback) {
    if (!this._eventListeners.has(event)) {
      this._eventListeners.set(event, new Set());
    }
    this._eventListeners.get(event).add(callback);

    return () => {
      this._eventListeners.get(event)?.delete(callback);
    };
  }

  /**
   * Emit event to listeners
   * @private
   * @param {string} event - Event name
   * @param {Object} data - Event data
   */
  _emit(event, data) {
    const listeners = this._eventListeners.get(event);
    if (listeners) {
      for (const callback of listeners) {
        try {
          callback({ ...data, circuit: this.name, timestamp: Date.now() });
        } catch {
          // Ignore listener errors
        }
      }
    }
  }
}

/**
 * Create a circuit breaker with default configuration
 * @param {Object} config - Configuration overrides
 * @returns {CircuitBreaker} Configured circuit breaker
 * @example
 * const breaker = createCircuitBreaker({ name: 'api', failureThreshold: 3 });
 */
export function createCircuitBreaker(config = {}) {
  return new CircuitBreaker(config);
}

/**
 * Circuit breaker registry for managing multiple circuits
 */
export class CircuitBreakerRegistry {
  constructor() {
    this._breakers = new Map();
  }

  /**
   * Get or create a circuit breaker
   * @param {string} name - Circuit name
   * @param {Object} [config] - Configuration if creating new
   * @returns {CircuitBreaker}
   */
  get(name, config = {}) {
    if (!this._breakers.has(name)) {
      this._breakers.set(name, new CircuitBreaker({ ...config, name }));
    }
    return this._breakers.get(name);
  }

  /**
   * Remove a circuit breaker
   * @param {string} name - Circuit name
   */
  remove(name) {
    this._breakers.delete(name);
  }

  /**
   * Get all circuit breakers
   * @returns {Map<string, CircuitBreaker>}
   */
  getAll() {
    return new Map(this._breakers);
  }

  /**
   * Get metrics for all circuits
   * @returns {Object} Aggregated metrics
   */
  getAllMetrics() {
    const metrics = {};
    for (const [name, breaker] of this._breakers) {
      metrics[name] = breaker.getMetrics();
    }
    return metrics;
  }

  /**
   * Reset all circuit breakers
   */
  resetAll() {
    for (const breaker of this._breakers.values()) {
      breaker.reset();
    }
  }
}

/** Global circuit breaker registry */
export const globalRegistry = new CircuitBreakerRegistry();

/**
 * @file Circuit Breaker Pattern for Fault Tolerance
 * @module knowledge-engine/utils/circuit-breaker
 *
 * @description
 * Implements the circuit breaker pattern for fault tolerance in distributed
 * operations like federation, remote SPARQL endpoints, and external services.
 *
 * States:
 * - CLOSED: Normal operation, requests pass through
 * - OPEN: Circuit tripped, requests fail immediately
 * - HALF_OPEN: Testing if service recovered
 */

/**
 * Circuit breaker states
 * @readonly
 * @enum {string}
 */
export const CircuitState = Object.freeze({
  CLOSED: 'closed',
  OPEN: 'open',
  HALF_OPEN: 'half_open',
});

/**
 * Circuit breaker configuration schema
 * @typedef {Object} CircuitBreakerConfig
 * @property {number} [failureThreshold=5] - Failures before opening circuit
 * @property {number} [resetTimeout=30000] - Time in ms before trying half-open
 * @property {number} [halfOpenMaxCalls=3] - Max calls in half-open state
 * @property {number} [successThreshold=2] - Successes in half-open to close
 * @property {string} [name='circuit-breaker'] - Name for tracing
 * @property {Function} [onStateChange] - Callback on state transitions
 * @property {Function} [isFailure] - Custom failure detection function
 */

/**
 * Circuit breaker for fault tolerance
 *
 * @example
 * const breaker = new CircuitBreaker({
 *   failureThreshold: 5,
 *   resetTimeout: 30000,
 *   name: 'sparql-endpoint'
 * });
 *
 * try {
 *   const result = await breaker.execute(() => fetch(sparqlEndpoint));
 * } catch (error) {
 *   if (error.name === 'CircuitOpenError') {
 *     // Circuit is open, use cached data
 *   }
 * }
 */
export class CircuitBreaker {
  /**
   * Create a new circuit breaker
   * @param {CircuitBreakerConfig} [config={}] - Configuration options
   */
  constructor(config = {}) {
    this.state = CircuitState.CLOSED;
    this.failureCount = 0;
    this.successCount = 0;
    this.halfOpenCalls = 0;
    this.lastFailureTime = null;
    this.lastStateChange = Date.now();

    // Configuration with defaults
    this.failureThreshold = config.failureThreshold ?? 5;
    this.resetTimeout = config.resetTimeout ?? 30000;
    this.halfOpenMaxCalls = config.halfOpenMaxCalls ?? 3;
    this.successThreshold = config.successThreshold ?? 2;
    this.name = config.name ?? 'circuit-breaker';
    this.onStateChange = config.onStateChange ?? null;
    this.isFailure = config.isFailure ?? (_error => true);

    // Metrics
    this.metrics = {
      totalCalls: 0,
      successfulCalls: 0,
      failedCalls: 0,
      rejectedCalls: 0,
      stateChanges: 0,
    };
  }

  /**
   * Execute a function with circuit breaker protection
   * @param {Function} fn - Async function to execute
   * @param {Object} [context={}] - Execution context (reserved for observability wrapper)
   * @returns {Promise<any>} Result of the function
   * @throws {CircuitOpenError} If circuit is open
   * @throws {Error} If function execution fails
   */
  async execute(fn, _context = {}) {
    this.metrics.totalCalls++;

    try {
      // Check if circuit should transition
      this._checkStateTransition();

      // Handle based on current state
      if (this.state === CircuitState.OPEN) {
        this.metrics.rejectedCalls++;
        throw new CircuitOpenError(
          `Circuit breaker '${this.name}' is open`,
          this.name,
          this.lastFailureTime
        );
      }

      if (this.state === CircuitState.HALF_OPEN) {
        if (this.halfOpenCalls >= this.halfOpenMaxCalls) {
          this.metrics.rejectedCalls++;
          throw new CircuitOpenError(
            `Circuit breaker '${this.name}' half-open limit reached`,
            this.name,
            this.lastFailureTime
          );
        }
        this.halfOpenCalls++;
      }

      // Execute the function
      const result = await fn();

      // Record success
      this.recordSuccess();
      this.metrics.successfulCalls++;

      return result;
    } catch (error) {
      // Check if this is a circuit error (pass through)
      if (error instanceof CircuitOpenError) {
        throw error;
      }

      // Determine if this error counts as a failure
      if (this.isFailure(error)) {
        this.recordFailure();
        this.metrics.failedCalls++;
      }

      throw error;
    }
  }

  /**
   * Record a successful operation
   */
  recordSuccess() {
    if (this.state === CircuitState.HALF_OPEN) {
      this.successCount++;
      if (this.successCount >= this.successThreshold) {
        this.reset();
      }
    } else if (this.state === CircuitState.CLOSED) {
      // Reset failure count on success in closed state
      this.failureCount = 0;
    }
  }

  /**
   * Record a failed operation
   */
  recordFailure() {
    this.failureCount++;
    this.lastFailureTime = Date.now();

    if (this.state === CircuitState.HALF_OPEN) {
      // Any failure in half-open trips the circuit again
      this.trip();
    } else if (this.state === CircuitState.CLOSED) {
      if (this.failureCount >= this.failureThreshold) {
        this.trip();
      }
    }
  }

  /**
   * Trip the circuit breaker (open the circuit)
   */
  trip() {
    if (this.state !== CircuitState.OPEN) {
      const previousState = this.state;
      this.state = CircuitState.OPEN;
      this.lastStateChange = Date.now();
      this.metrics.stateChanges++;

      this._notifyStateChange(previousState, CircuitState.OPEN);
    }
  }

  /**
   * Reset the circuit breaker (close the circuit)
   */
  reset() {
    const previousState = this.state;
    this.state = CircuitState.CLOSED;
    this.failureCount = 0;
    this.successCount = 0;
    this.halfOpenCalls = 0;
    this.lastStateChange = Date.now();
    this.metrics.stateChanges++;

    this._notifyStateChange(previousState, CircuitState.CLOSED);
  }

  /**
   * Force the circuit into half-open state
   */
  halfOpen() {
    if (this.state !== CircuitState.HALF_OPEN) {
      const previousState = this.state;
      this.state = CircuitState.HALF_OPEN;
      this.successCount = 0;
      this.halfOpenCalls = 0;
      this.lastStateChange = Date.now();
      this.metrics.stateChanges++;

      this._notifyStateChange(previousState, CircuitState.HALF_OPEN);
    }
  }

  /**
   * Check if state should transition based on timeout
   * @private
   */
  _checkStateTransition() {
    if (this.state === CircuitState.OPEN) {
      const timeSinceOpen = Date.now() - this.lastStateChange;
      if (timeSinceOpen >= this.resetTimeout) {
        this.halfOpen();
      }
    }
  }

  /**
   * Notify state change callback
   * @param {string} from - Previous state
   * @param {string} to - New state
   * @private
   */
  _notifyStateChange(from, to) {
    if (this.onStateChange) {
      try {
        this.onStateChange({
          name: this.name,
          from,
          to,
          timestamp: Date.now(),
          failureCount: this.failureCount,
          metrics: { ...this.metrics },
        });
      } catch (error) {
        // Ignore callback errors
      }
    }
  }

  /**
   * Get current circuit breaker status
   * @returns {Object} Status object
   */
  getStatus() {
    return {
      name: this.name,
      state: this.state,
      failureCount: this.failureCount,
      successCount: this.successCount,
      halfOpenCalls: this.halfOpenCalls,
      lastFailureTime: this.lastFailureTime,
      lastStateChange: this.lastStateChange,
      config: {
        failureThreshold: this.failureThreshold,
        resetTimeout: this.resetTimeout,
        halfOpenMaxCalls: this.halfOpenMaxCalls,
        successThreshold: this.successThreshold,
      },
      metrics: { ...this.metrics },
    };
  }

  /**
   * Check if circuit is healthy (closed)
   * @returns {boolean} True if circuit is closed
   */
  isHealthy() {
    return this.state === CircuitState.CLOSED;
  }

  /**
   * Check if circuit is open
   * @returns {boolean} True if circuit is open
   */
  isOpen() {
    this._checkStateTransition();
    return this.state === CircuitState.OPEN;
  }

  /**
   * Check if circuit is half-open
   * @returns {boolean} True if circuit is half-open
   */
  isHalfOpen() {
    this._checkStateTransition();
    return this.state === CircuitState.HALF_OPEN;
  }

  /**
   * Reset metrics counters
   */
  resetMetrics() {
    this.metrics = {
      totalCalls: 0,
      successfulCalls: 0,
      failedCalls: 0,
      rejectedCalls: 0,
      stateChanges: 0,
    };
  }
}

/**
 * Error thrown when circuit is open
 */
export class CircuitOpenError extends Error {
  /**
   * Create a circuit open error
   * @param {string} message - Error message
   * @param {string} circuitName - Name of the circuit
   * @param {number} lastFailureTime - Timestamp of last failure
   */
  constructor(message, circuitName, lastFailureTime) {
    super(message);
    this.name = 'CircuitOpenError';
    this.circuitName = circuitName;
    this.lastFailureTime = lastFailureTime;
  }
}

/**
 * Circuit breaker registry for managing multiple breakers
 */
export class CircuitBreakerRegistry {
  /**
   *
   */
  constructor() {
    this.breakers = new Map();
  }

  /**
   * Get or create a circuit breaker
   * @param {string} name - Breaker name
   * @param {CircuitBreakerConfig} [config] - Config for new breaker
   * @returns {CircuitBreaker} Circuit breaker instance
   */
  getOrCreate(name, config = {}) {
    if (!this.breakers.has(name)) {
      this.breakers.set(name, new CircuitBreaker({ ...config, name }));
    }
    return this.breakers.get(name);
  }

  /**
   * Get an existing circuit breaker
   * @param {string} name - Breaker name
   * @returns {CircuitBreaker|undefined} Circuit breaker or undefined
   */
  get(name) {
    return this.breakers.get(name);
  }

  /**
   * Check if a breaker exists
   * @param {string} name - Breaker name
   * @returns {boolean} True if exists
   */
  has(name) {
    return this.breakers.has(name);
  }

  /**
   * Remove a circuit breaker
   * @param {string} name - Breaker name
   * @returns {boolean} True if removed
   */
  remove(name) {
    return this.breakers.delete(name);
  }

  /**
   * Get all circuit breaker statuses
   * @returns {Object} Map of name to status
   */
  getAllStatuses() {
    const statuses = {};
    for (const [name, breaker] of this.breakers) {
      statuses[name] = breaker.getStatus();
    }
    return statuses;
  }

  /**
   * Reset all circuit breakers
   */
  resetAll() {
    for (const breaker of this.breakers.values()) {
      breaker.reset();
    }
  }

  /**
   * Get health summary of all breakers
   * @returns {Object} Health summary
   */
  getHealthSummary() {
    let healthy = 0;
    let open = 0;
    let halfOpen = 0;

    for (const breaker of this.breakers.values()) {
      if (breaker.isHealthy()) healthy++;
      else if (breaker.isOpen()) open++;
      else if (breaker.isHalfOpen()) halfOpen++;
    }

    return {
      total: this.breakers.size,
      healthy,
      open,
      halfOpen,
      healthPercent: this.breakers.size > 0 ? (healthy / this.breakers.size) * 100 : 100,
    };
  }
}

/**
 * Default global registry
 */
export const defaultRegistry = new CircuitBreakerRegistry();

/**
 * Create a circuit breaker with default configuration
 * @param {string} name - Breaker name
 * @param {CircuitBreakerConfig} [config] - Configuration
 * @returns {CircuitBreaker} Circuit breaker instance
 */
export function createCircuitBreaker(name, config = {}) {
  return new CircuitBreaker({ ...config, name });
}

/**
 * Decorator/wrapper function for adding circuit breaker to any async function
 * @param {Function} fn - Async function to wrap
 * @param {CircuitBreaker} breaker - Circuit breaker instance
 * @returns {Function} Wrapped function
 */
export function withCircuitBreaker(fn, breaker) {
  return async function (...args) {
    return await breaker.execute(() => fn.apply(this, args));
  };
}

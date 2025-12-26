/**
 * @file Error recovery patterns and resilience utilities
 * @module @unrdf/core/recovery
 */

import { TimeoutError, NetworkError, createError } from './errors.mjs';
import { createDebugger } from './debug.mjs';

const debug = createDebugger('recovery');

/**
 * Retry configuration options
 * @typedef {Object} RetryOptions
 * @property {number} [maxAttempts=3] - Maximum retry attempts
 * @property {number} [initialDelay=100] - Initial delay in milliseconds
 * @property {number} [maxDelay=5000] - Maximum delay in milliseconds
 * @property {number} [backoffMultiplier=2] - Exponential backoff multiplier
 * @property {Function} [shouldRetry] - Custom function to determine if retry should occur
 * @property {Function} [onRetry] - Callback on retry attempt
 */

/**
 * Default retry predicate - retries on network and timeout errors
 * @param {Error} error - Error to check
 * @returns {boolean} True if should retry
 */
function defaultShouldRetry(error) {
  return (
    error instanceof NetworkError ||
    error instanceof TimeoutError ||
    error.code === 'ECONNRESET' ||
    error.code === 'ETIMEDOUT' ||
    error.code === 'ENOTFOUND'
  );
}

/**
 * Retry operation with exponential backoff
 * @param {Function} operation - Async operation to retry
 * @param {RetryOptions} [options] - Retry configuration
 * @returns {Promise<*>} Operation result
 *
 * @example
 * const result = await retry(
 *   async () => fetch('https://api.example.com/data'),
 *   { maxAttempts: 5, initialDelay: 200 }
 * );
 */
export async function retry(operation, options = {}) {
  const {
    maxAttempts = 3,
    initialDelay = 100,
    maxDelay = 5000,
    backoffMultiplier = 2,
    shouldRetry = defaultShouldRetry,
    onRetry = null,
  } = options;

  let lastError;
  let delay = initialDelay;

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      debug.log(`Attempt ${attempt}/${maxAttempts}`);
      return await operation();
    } catch (error) {
      lastError = error;

      // Check if we should retry
      if (attempt === maxAttempts || !shouldRetry(error)) {
        debug.error(`Failed after ${attempt} attempts`, error);
        throw error;
      }

      // Calculate delay with exponential backoff
      const currentDelay = Math.min(delay, maxDelay);
      debug.log(`Retrying in ${currentDelay}ms`, { attempt, error: error.message });

      // Call retry callback if provided
      if (onRetry) {
        await onRetry(error, attempt, currentDelay);
      }

      // Wait before retry
      await sleep(currentDelay);

      // Increase delay for next attempt
      delay *= backoffMultiplier;
    }
  }

  throw lastError;
}

/**
 * Circuit breaker state
 * @typedef {Object} CircuitBreakerState
 * @property {'closed'|'open'|'half-open'} state - Current state
 * @property {number} failures - Failure count
 * @property {number} successes - Success count
 * @property {number|null} openedAt - Timestamp when opened
 */

/**
 * Circuit breaker for preventing cascading failures
 */
export class CircuitBreaker {
  /**
   * @param {Object} options - Configuration
   * @param {number} [options.failureThreshold=5] - Failures before opening
   * @param {number} [options.successThreshold=2] - Successes to close from half-open
   * @param {number} [options.timeout=60000] - Time in ms to wait before half-open
   * @param {Function} [options.onStateChange] - Callback on state change
   */
  constructor(options = {}) {
    this.failureThreshold = options.failureThreshold || 5;
    this.successThreshold = options.successThreshold || 2;
    this.timeout = options.timeout || 60000;
    this.onStateChange = options.onStateChange || null;

    this.state = 'closed';
    this.failures = 0;
    this.successes = 0;
    this.openedAt = null;

    this.debug = createDebugger('recovery:circuit-breaker');
  }

  /**
   * Execute operation with circuit breaker
   * @param {Function} operation - Async operation
   * @returns {Promise<*>} Operation result
   *
   * @example
   * const breaker = new CircuitBreaker({ failureThreshold: 3 });
   * const result = await breaker.execute(async () => {
   *   return await fetchRemoteData();
   * });
   */
  async execute(operation) {
    // Check if circuit is open
    if (this.state === 'open') {
      // Check if timeout has passed to transition to half-open
      const now = Date.now();
      if (now - this.openedAt >= this.timeout) {
        this.debug.log('Transitioning to half-open');
        this._setState('half-open');
      } else {
        const remainingMs = this.timeout - (now - this.openedAt);
        throw createError('ERR_CIRCUIT_OPEN', {
          state: this.state,
          failures: this.failures,
          remainingMs,
        });
      }
    }

    try {
      const result = await operation();
      this._onSuccess();
      return result;
    } catch (error) {
      this._onFailure();
      throw error;
    }
  }

  /**
   * Handle successful execution
   * @private
   */
  _onSuccess() {
    this.failures = 0;

    if (this.state === 'half-open') {
      this.successes++;
      this.debug.log(`Half-open success: ${this.successes}/${this.successThreshold}`);

      if (this.successes >= this.successThreshold) {
        this._setState('closed');
        this.successes = 0;
      }
    }
  }

  /**
   * Handle failed execution
   * @private
   */
  _onFailure() {
    this.successes = 0;
    this.failures++;

    this.debug.log(`Failure: ${this.failures}/${this.failureThreshold}`);

    if (this.failures >= this.failureThreshold) {
      this._setState('open');
      this.openedAt = Date.now();
    }
  }

  /**
   * Change circuit state
   * @param {string} newState - New state
   * @private
   */
  _setState(newState) {
    const oldState = this.state;
    this.state = newState;

    this.debug.log(`State change: ${oldState} -> ${newState}`);

    if (this.onStateChange) {
      this.onStateChange(oldState, newState, this.getState());
    }
  }

  /**
   * Get current circuit state
   * @returns {CircuitBreakerState} Current state
   */
  getState() {
    return {
      state: this.state,
      failures: this.failures,
      successes: this.successes,
      openedAt: this.openedAt,
    };
  }

  /**
   * Reset circuit breaker
   */
  reset() {
    this.debug.log('Resetting circuit breaker');
    this.state = 'closed';
    this.failures = 0;
    this.successes = 0;
    this.openedAt = null;
  }
}

/**
 * Fallback strategy for graceful degradation
 * @param {Function} primary - Primary operation
 * @param {Function|*} fallback - Fallback operation or value
 * @returns {Promise<*>} Result
 *
 * @example
 * const data = await fallback(
 *   async () => fetchFromRemote(),
 *   async () => fetchFromCache()
 * );
 *
 * // Or with static fallback
 * const data = await fallback(
 *   async () => fetchData(),
 *   []  // Empty array as fallback
 * );
 */
export async function fallback(primary, fallback) {
  try {
    return await primary();
  } catch (error) {
    debug.warn('Primary operation failed, using fallback', { error: error.message });

    // If fallback is a function, execute it
    if (typeof fallback === 'function') {
      return await fallback();
    }

    // Otherwise, return fallback value directly
    return fallback;
  }
}

/**
 * Timeout wrapper for operations
 * @param {Function} operation - Async operation
 * @param {number} timeoutMs - Timeout in milliseconds
 * @param {string} [message] - Custom timeout message
 * @returns {Promise<*>} Operation result
 *
 * @example
 * const result = await withTimeout(
 *   async () => fetchData(),
 *   5000,
 *   'Data fetch timeout'
 * );
 */
export async function withTimeout(operation, timeoutMs, message = 'Operation timeout') {
  return Promise.race([
    operation(),
    sleep(timeoutMs).then(() => {
      throw new TimeoutError(message, {
        timeoutMs,
        timestamp: new Date().toISOString(),
      });
    }),
  ]);
}

/**
 * Bulk operation with individual error handling
 * @param {Array} items - Items to process
 * @param {Function} operation - Operation to perform on each item
 * @param {Object} [options] - Options
 * @param {boolean} [options.continueOnError=true] - Continue on individual failures
 * @param {number} [options.concurrency=5] - Concurrent operations
 * @returns {Promise<Object>} Results with successes and failures
 *
 * @example
 * const { successes, failures } = await bulkOperation(
 *   quads,
 *   async (quad) => addQuad(store, quad),
 *   { continueOnError: true, concurrency: 10 }
 * );
 */
export async function bulkOperation(items, operation, options = {}) {
  const { continueOnError = true, concurrency = 5 } = options;

  const successes = [];
  const failures = [];

  // Process in batches for concurrency control
  for (let i = 0; i < items.length; i += concurrency) {
    const batch = items.slice(i, i + concurrency);

    const results = await Promise.allSettled(batch.map((item) => operation(item)));

    results.forEach((result, index) => {
      if (result.status === 'fulfilled') {
        successes.push({ item: batch[index], result: result.value });
      } else {
        failures.push({ item: batch[index], error: result.reason });

        if (!continueOnError) {
          throw result.reason;
        }
      }
    });
  }

  debug.log('Bulk operation complete', {
    total: items.length,
    successes: successes.length,
    failures: failures.length,
  });

  return { successes, failures };
}

/**
 * Rate limiter for controlling operation frequency
 */
export class RateLimiter {
  /**
   * @param {Object} options - Configuration
   * @param {number} [options.maxOperations=10] - Max operations per window
   * @param {number} [options.windowMs=1000] - Time window in milliseconds
   */
  constructor(options = {}) {
    this.maxOperations = options.maxOperations || 10;
    this.windowMs = options.windowMs || 1000;
    this.operations = [];
    this.debug = createDebugger('recovery:rate-limiter');
  }

  /**
   * Execute operation with rate limiting
   * @param {Function} operation - Operation to execute
   * @returns {Promise<*>} Operation result
   *
   * @example
   * const limiter = new RateLimiter({ maxOperations: 5, windowMs: 1000 });
   * for (const item of items) {
   *   await limiter.execute(async () => processItem(item));
   * }
   */
  async execute(operation) {
    await this._waitIfNeeded();
    this.operations.push(Date.now());
    return await operation();
  }

  /**
   * Wait if rate limit is exceeded
   * @private
   */
  async _waitIfNeeded() {
    const now = Date.now();
    const windowStart = now - this.windowMs;

    // Remove operations outside current window
    this.operations = this.operations.filter((time) => time > windowStart);

    // Check if we've hit the limit
    if (this.operations.length >= this.maxOperations) {
      const oldestOperation = this.operations[0];
      const waitTime = oldestOperation + this.windowMs - now;

      if (waitTime > 0) {
        this.debug.log(`Rate limit reached, waiting ${waitTime}ms`);
        await sleep(waitTime);
        return this._waitIfNeeded();
      }
    }
  }

  /**
   * Get current rate limit status
   * @returns {Object} Status
   */
  getStatus() {
    const now = Date.now();
    const windowStart = now - this.windowMs;
    const currentCount = this.operations.filter((time) => time > windowStart).length;

    return {
      current: currentCount,
      max: this.maxOperations,
      windowMs: this.windowMs,
      available: this.maxOperations - currentCount,
    };
  }

  /**
   * Reset rate limiter
   */
  reset() {
    this.operations = [];
    this.debug.log('Rate limiter reset');
  }
}

/**
 * Sleep utility
 * @param {number} ms - Milliseconds to sleep
 * @returns {Promise<void>}
 */
function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Create recovery wrapper with multiple strategies
 * @param {Function} operation - Operation to wrap
 * @param {Object} [options] - Recovery options
 * @param {RetryOptions} [options.retry] - Retry configuration
 * @param {CircuitBreaker} [options.circuitBreaker] - Circuit breaker instance
 * @param {number} [options.timeout] - Timeout in milliseconds
 * @param {Function|*} [options.fallback] - Fallback strategy
 * @returns {Function} Wrapped operation
 *
 * @example
 * const robustFetch = withRecovery(
 *   async (url) => fetch(url),
 *   {
 *     retry: { maxAttempts: 3 },
 *     timeout: 5000,
 *     fallback: async () => cachedData
 *   }
 * );
 *
 * const data = await robustFetch('https://api.example.com/data');
 */
export function withRecovery(operation, options = {}) {
  const { retry: retryOpts, circuitBreaker, timeout: timeoutMs, fallback: fallbackValue } = options;

  return async function (...args) {
    let wrappedOp = () => operation(...args);

    // Apply timeout
    if (timeoutMs) {
      const originalOp = wrappedOp;
      wrappedOp = () => withTimeout(originalOp, timeoutMs);
    }

    // Apply circuit breaker
    if (circuitBreaker) {
      const originalOp = wrappedOp;
      wrappedOp = () => circuitBreaker.execute(originalOp);
    }

    // Apply retry
    if (retryOpts) {
      const originalOp = wrappedOp;
      wrappedOp = () => retry(originalOp, retryOpts);
    }

    // Apply fallback
    if (fallbackValue !== undefined) {
      return fallback(wrappedOp, fallbackValue);
    }

    return wrappedOp();
  };
}

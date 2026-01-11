/**
 * @file Retry strategy with exponential backoff
 * @module @unrdf/self-healing-workflows/retry
 * @description Implements retry logic with exponential backoff and jitter
 */

import { RetryStrategySchema } from './schemas.mjs';

/**
 * Retry strategy with exponential backoff
 */
export class RetryStrategy {
  /**
   * Creates a new retry strategy
   * @param {Object} [config] - Retry configuration
   * @param {number} [config.maxAttempts=3] - Maximum retry attempts
   * @param {number} [config.initialDelay=1000] - Initial delay in ms
   * @param {number} [config.maxDelay=30000] - Maximum delay in ms
   * @param {number} [config.backoffMultiplier=2] - Backoff multiplier
   * @param {boolean} [config.jitter=true] - Add random jitter
   * @param {Array<string>} [config.retryableErrors] - Error categories to retry
   */
  constructor(config = {}) {
    this.config = RetryStrategySchema.parse(config);
  }

  /**
   * Executes an operation with retry logic
   * @param {Function} operation - Async operation to execute
   * @param {Object} [options] - Execution options
   * @param {Function} [options.onRetry] - Callback on retry
   * @param {Function} [options.shouldRetry] - Custom retry predicate
   * @returns {Promise<any>} Operation result
   * @throws {Error} If all retries exhausted
   * @example
   * const retry = new RetryStrategy({ maxAttempts: 3 });
   * const result = await retry.execute(async () => {
   *   return await fetch('https://api.example.com');
   * });
   */
  async execute(operation, options = {}) {
    const { onRetry, shouldRetry } = options;
    let lastError;
    let attempt = 0;

    while (attempt < this.config.maxAttempts) {
      try {
        const result = await operation();
        return result;
      } catch (error) {
        lastError = error;
        attempt++;

        // Check if we should retry
        const canRetry = shouldRetry
          ? shouldRetry(error, attempt)
          : attempt < this.config.maxAttempts;

        if (!canRetry) {
          break;
        }

        // Calculate delay
        const delay = this.calculateDelay(attempt);

        // Call retry callback
        if (onRetry) {
          onRetry(error, attempt, delay);
        }

        // Wait before retry
        await this.sleep(delay);
      }
    }

    // All retries exhausted
    const error = new Error(
      `Operation failed after ${attempt} attempts: ${lastError.message}`
    );
    error.cause = lastError;
    error.attempts = attempt;
    throw error;
  }

  /**
   * Calculates delay for a given attempt with exponential backoff
   * @param {number} attempt - Current attempt number (1-based)
   * @returns {number} Delay in milliseconds
   */
  calculateDelay(attempt) {
    const { initialDelay, maxDelay, backoffMultiplier, jitter } = this.config;

    // Exponential backoff: delay = initial * (multiplier ^ (attempt - 1))
    let delay = initialDelay * Math.pow(backoffMultiplier, attempt - 1);

    // Cap at max delay
    delay = Math.min(delay, maxDelay);

    // Add jitter to prevent thundering herd
    if (jitter) {
      const jitterAmount = delay * 0.2; // ±20% jitter
      delay = delay + (Math.random() * 2 - 1) * jitterAmount;
    }

    return Math.max(0, Math.floor(delay));
  }

  /**
   * Sleeps for specified duration
   * @param {number} ms - Milliseconds to sleep
   * @returns {Promise<void>}
   */
  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  /**
   * Executes operation with retry and returns detailed result
   * @param {Function} operation - Async operation to execute
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Result with metadata
   */
  async executeWithMetadata(operation, options = {}) {
    const startTime = Date.now();
    const attempts = [];
    let result;
    let success = false;

    try {
      result = await this.execute(operation, {
        ...options,
        onRetry: (error, attempt, delay) => {
          attempts.push({
            attempt,
            error: error.message,
            delay,
            timestamp: Date.now()
          });
          if (options.onRetry) {
            options.onRetry(error, attempt, delay);
          }
        }
      });
      success = true;
    } catch (error) {
      result = error;
      success = false;
    }

    return {
      success,
      result: success ? result : undefined,
      error: success ? undefined : result,
      attempts: attempts.length + 1,
      duration: Date.now() - startTime,
      retryHistory: attempts
    };
  }

  /**
   * Creates a retryable version of an async function
   * @param {Function} fn - Function to wrap
   * @param {Object} [options] - Retry options
   * @returns {Function} Wrapped function
   */
  wrap(fn, options = {}) {
    return async (...args) => {
      return this.execute(() => fn(...args), options);
    };
  }

  /**
   * Checks if an error is retryable based on configuration
   * @param {Object} classifiedError - Classified error object
   * @returns {boolean} True if error is retryable
   */
  isRetryable(classifiedError) {
    if (!classifiedError.retryable) {
      return false;
    }
    return this.config.retryableErrors.includes(classifiedError.category);
  }

  /**
   * Updates retry configuration
   * @param {Object} updates - Configuration updates
   * @returns {void}
   */
  updateConfig(updates) {
    this.config = RetryStrategySchema.parse({
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
 * Creates a new retry strategy instance
 * @param {Object} [config] - Retry configuration
 * @returns {RetryStrategy} Retry strategy instance
 */
export function createRetryStrategy(config) {
  return new RetryStrategy(config);
}

/**
 * Immediate retry helper (3 attempts, no backoff)
 * @param {Function} operation - Operation to execute
 * @returns {Promise<any>} Operation result
 */
export async function immediateRetry(operation) {
  const strategy = new RetryStrategy({
    maxAttempts: 3,
    initialDelay: 0,
    backoffMultiplier: 1
  });
  return strategy.execute(operation);
}

/**
 * Exponential backoff retry helper (default config)
 * @param {Function} operation - Operation to execute
 * @returns {Promise<any>} Operation result
 */
export async function exponentialRetry(operation) {
  const strategy = new RetryStrategy({
    maxAttempts: 5,
    initialDelay: 1000,
    maxDelay: 30000,
    backoffMultiplier: 2
  });
  return strategy.execute(operation);
}

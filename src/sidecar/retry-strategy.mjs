/**
 * @file Retry strategy with exponential backoff
 * @module sidecar/retry-strategy
 *
 * @description
 * Implements retry logic with exponential backoff, jitter, and configurable policies.
 */

import { z } from 'zod';

/**
 * Retry configuration schema
 */
const RetryConfigSchema = z.object({
  maxRetries: z.number().int().min(0).max(10).default(3),
  initialDelay: z.number().int().positive().default(100),
  maxDelay: z.number().int().positive().default(10000),
  backoffMultiplier: z.number().positive().default(2),
  jitter: z.boolean().default(true),
  retryableErrors: z.array(z.string()).default([
    'UNAVAILABLE',
    'DEADLINE_EXCEEDED',
    'RESOURCE_EXHAUSTED',
    'ABORTED',
    'INTERNAL',
    'UNKNOWN'
  ])
});

/**
 * Retry strategy with exponential backoff
 */
export class RetryStrategy {
  /**
   * Create a new retry strategy
   * @param {Object} [config] - Retry configuration
   */
  constructor(config = {}) {
    this.config = RetryConfigSchema.parse(config);
    this.metrics = {
      totalAttempts: 0,
      totalRetries: 0,
      successAfterRetry: 0,
      permanentFailures: 0,
      retriesByError: new Map()
    };
  }

  /**
   * Execute function with retry logic
   * @param {Function} fn - Function to execute
   * @param {...any} args - Function arguments
   * @returns {Promise<any>} Function result
   * @throws {Error} If all retries exhausted
   */
  async execute(fn, ...args) {
    let lastError;
    let attempt = 0;

    while (attempt <= this.config.maxRetries) {
      this.metrics.totalAttempts++;

      try {
        const result = await fn(...args);

        if (attempt > 0) {
          this.metrics.successAfterRetry++;
        }

        return result;
      } catch (error) {
        lastError = error;

        // Check if error is retryable
        if (!this._isRetryable(error)) {
          this.metrics.permanentFailures++;
          throw error;
        }

        // Update retry metrics
        this._recordRetry(error);

        // Check if we should retry
        if (attempt >= this.config.maxRetries) {
          this.metrics.permanentFailures++;
          break;
        }

        // Calculate backoff delay
        const delay = this._calculateDelay(attempt);

        // Wait before retry
        await this._sleep(delay);

        attempt++;
        this.metrics.totalRetries++;
      }
    }

    // All retries exhausted
    throw lastError;
  }

  /**
   * Check if error is retryable
   * @param {Error} error - Error to check
   * @returns {boolean} True if retryable
   * @private
   */
  _isRetryable(error) {
    // Check error code
    if (error.code && this.config.retryableErrors.includes(error.code)) {
      return true;
    }

    // Check error message for retryable patterns
    const message = error.message?.toLowerCase() || '';
    const retryablePatterns = [
      'timeout',
      'unavailable',
      'connection refused',
      'econnrefused',
      'enotfound',
      'etimedout',
      'network error',
      'socket hang up'
    ];

    return retryablePatterns.some(pattern => message.includes(pattern));
  }

  /**
   * Calculate backoff delay
   * @param {number} attempt - Attempt number (0-indexed)
   * @returns {number} Delay in milliseconds
   * @private
   */
  _calculateDelay(attempt) {
    // Exponential backoff: initialDelay * (multiplier ^ attempt)
    let delay = this.config.initialDelay * Math.pow(this.config.backoffMultiplier, attempt);

    // Cap at max delay
    delay = Math.min(delay, this.config.maxDelay);

    // Add jitter if enabled
    if (this.config.jitter) {
      delay = this._addJitter(delay);
    }

    return delay;
  }

  /**
   * Add jitter to delay
   * @param {number} delay - Base delay
   * @returns {number} Delay with jitter
   * @private
   */
  _addJitter(delay) {
    // Add random jitter between 0% and 25% of delay
    const jitter = Math.random() * delay * 0.25;
    return delay + jitter;
  }

  /**
   * Sleep for specified duration
   * @param {number} ms - Milliseconds to sleep
   * @returns {Promise<void>}
   * @private
   */
  _sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  /**
   * Record retry attempt
   * @param {Error} error - Error that caused retry
   * @private
   */
  _recordRetry(error) {
    const errorCode = error.code || 'UNKNOWN';
    const count = this.metrics.retriesByError.get(errorCode) || 0;
    this.metrics.retriesByError.set(errorCode, count + 1);
  }

  /**
   * Get retry metrics
   * @returns {Object} Retry metrics
   */
  getMetrics() {
    const retriesByError = Object.fromEntries(this.metrics.retriesByError);

    return {
      totalAttempts: this.metrics.totalAttempts,
      totalRetries: this.metrics.totalRetries,
      successAfterRetry: this.metrics.successAfterRetry,
      permanentFailures: this.metrics.permanentFailures,
      retriesByError,
      retryRate: this.metrics.totalAttempts > 0
        ? (this.metrics.totalRetries / this.metrics.totalAttempts) * 100
        : 0,
      successRate: this.metrics.totalAttempts > 0
        ? ((this.metrics.totalAttempts - this.metrics.permanentFailures) / this.metrics.totalAttempts) * 100
        : 0
    };
  }

  /**
   * Reset retry metrics
   */
  resetMetrics() {
    this.metrics = {
      totalAttempts: 0,
      totalRetries: 0,
      successAfterRetry: 0,
      permanentFailures: 0,
      retriesByError: new Map()
    };
  }
}

/**
 * Create a retry strategy
 * @param {Object} [config] - Retry configuration
 * @returns {RetryStrategy} Retry strategy instance
 */
export function createRetryStrategy(config) {
  return new RetryStrategy(config);
}

/**
 * Retry with exponential backoff (simple helper)
 * @param {Function} fn - Function to execute
 * @param {Object} [options] - Retry options
 * @returns {Promise<any>} Function result
 */
export async function retryWithBackoff(fn, options = {}) {
  const strategy = new RetryStrategy(options);
  return strategy.execute(fn);
}

export default RetryStrategy;

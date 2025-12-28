/**
 * @fileoverview Exponential Backoff and Retry Strategy
 *
 * **Purpose**: Intelligent retry with exponential backoff for transient failures
 * - Configurable retry attempts and backoff multiplier
 * - Jitter to avoid thundering herd
 * - Transient vs permanent error detection
 * - OTEL tracing for retry attempts
 *
 * @module resilience/retry-strategy
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

/**
 * Retry configuration schema
 */
export const RetryConfigSchema = z.object({
  maxAttempts: z.number().min(1).max(10).default(3),
  initialDelayMs: z.number().min(100).max(10000).default(1000),
  maxDelayMs: z.number().min(1000).max(60000).default(30000),
  backoffMultiplier: z.number().min(1).max(10).default(2),
  jitterMs: z.number().min(0).max(5000).default(100),
  retryableErrors: z.array(z.string()).default(['ETIMEDOUT', 'ECONNRESET', 'ENOTFOUND']),
});

/**
 * Error classification
 */
export const ErrorType = {
  TRANSIENT: 'transient', // Retry makes sense
  PERMANENT: 'permanent', // Retry won't help
  UNKNOWN: 'unknown', // Unknown, default to retry
};

/**
 * Exponential backoff retry strategy
 *
 * @example
 * const retry = new RetryStrategy({ maxAttempts: 3 });
 * const result = await retry.execute(async () => {
 *   return await unreliableOperation();
 * });
 */
export class RetryStrategy {
  /**
   * Create a new retry strategy
   * @param {Object} [config] - Retry configuration
   */
  constructor(config = {}) {
    this.config = RetryConfigSchema.parse(config);
    this.tracer = trace.getTracer('unrdf-retry-strategy');
    this.metrics = {
      totalAttempts: 0,
      successfulRetries: 0,
      failedRetries: 0,
      permanentFailures: 0,
    };
  }

  /**
   * Execute operation with retry logic
   * @param {Function} operation - Async operation to execute
   * @param {Object} [context] - Execution context for tracing
   * @returns {Promise<any>} Operation result
   * @throws {Error} If all retries exhausted
   */
  async execute(operation, context = {}) {
    return await this.tracer.startActiveSpan('retry.execute', async (span) => {
      span.setAttributes({
        'retry.max_attempts': this.config.maxAttempts,
        'retry.initial_delay_ms': this.config.initialDelayMs,
        ...context,
      });

      let lastError = null;
      let attempt = 0;

      while (attempt < this.config.maxAttempts) {
        attempt++;
        this.metrics.totalAttempts++;

        try {
          span.setAttributes({ 'retry.attempt': attempt });

          const result = await operation();

          if (attempt > 1) {
            this.metrics.successfulRetries++;
            span.setAttributes({
              'retry.succeeded_on_attempt': attempt,
              'retry.total_attempts': attempt,
            });
          }

          span.setStatus({ code: SpanStatusCode.OK });
          span.end();

          return result;
        } catch (error) {
          lastError = error;

          // Classify error
          const errorType = this._classifyError(error);

          span.recordException(error);
          span.setAttributes({
            'retry.error_type': errorType,
            'retry.error_message': error.message,
          });

          // If permanent error, don't retry
          if (errorType === ErrorType.PERMANENT) {
            this.metrics.permanentFailures++;
            span.setAttributes({ 'retry.permanent_failure': true });
            span.setStatus({
              code: SpanStatusCode.ERROR,
              message: 'Permanent failure - no retry',
            });
            span.end();
            throw error;
          }

          // If last attempt, throw
          if (attempt >= this.config.maxAttempts) {
            this.metrics.failedRetries++;
            span.setAttributes({
              'retry.exhausted': true,
              'retry.total_attempts': attempt,
            });
            span.setStatus({
              code: SpanStatusCode.ERROR,
              message: 'Retry exhausted',
            });
            span.end();
            throw new RetryExhaustedError(
              `Operation failed after ${attempt} attempts: ${error.message}`,
              attempt,
              lastError
            );
          }

          // Calculate backoff delay
          const delay = this._calculateBackoff(attempt);

          span.setAttributes({
            'retry.backoff_ms': delay,
            'retry.will_retry': true,
          });

          console.warn(
            `[Retry] Attempt ${attempt}/${this.config.maxAttempts} failed: ${error.message}. Retrying in ${delay}ms...`
          );

          // Wait before retry
          await this._sleep(delay);
        }
      }

      // Should never reach here, but TypeScript likes it
      span.setStatus({ code: SpanStatusCode.ERROR, message: 'Unexpected retry exit' });
      span.end();
      throw lastError;
    });
  }

  /**
   * Calculate exponential backoff delay with jitter
   * @param {number} attempt - Current attempt number (1-indexed)
   * @returns {number} Delay in milliseconds
   * @private
   */
  _calculateBackoff(attempt) {
    // Exponential: delay = initialDelay * (multiplier ^ (attempt - 1))
    const exponentialDelay =
      this.config.initialDelayMs * Math.pow(this.config.backoffMultiplier, attempt - 1);

    // Cap at max delay
    const cappedDelay = Math.min(exponentialDelay, this.config.maxDelayMs);

    // Add jitter to avoid thundering herd
    const jitter = Math.random() * this.config.jitterMs;

    return Math.floor(cappedDelay + jitter);
  }

  /**
   * Classify error as transient or permanent
   * @param {Error} error - Error to classify
   * @returns {string} Error type
   * @private
   */
  _classifyError(error) {
    // Check error code
    if (error.code && this.config.retryableErrors.includes(error.code)) {
      return ErrorType.TRANSIENT;
    }

    // Check error message patterns
    const message = error.message?.toLowerCase() || '';

    // Transient patterns
    const transientPatterns = [
      'timeout',
      'connection reset',
      'econnreset',
      'etimedout',
      'enotfound',
      'socket hang up',
      'network',
      'unavailable',
      'rate limit',
      'too many requests',
      '503',
      '429',
    ];

    if (transientPatterns.some((pattern) => message.includes(pattern))) {
      return ErrorType.TRANSIENT;
    }

    // Permanent patterns
    const permanentPatterns = [
      'not found',
      '404',
      'unauthorized',
      '401',
      'forbidden',
      '403',
      'bad request',
      '400',
      'invalid',
      'validation failed',
      'schema',
    ];

    if (permanentPatterns.some((pattern) => message.includes(pattern))) {
      return ErrorType.PERMANENT;
    }

    // Default to transient for unknown errors
    return ErrorType.UNKNOWN;
  }

  /**
   * Sleep for specified milliseconds
   * @param {number} ms - Milliseconds to sleep
   * @returns {Promise<void>}
   * @private
   */
  _sleep(ms) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  /**
   * Get retry metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      successRate:
        this.metrics.totalAttempts > 0
          ? (
              ((this.metrics.totalAttempts - this.metrics.failedRetries) /
                this.metrics.totalAttempts) *
              100
            ).toFixed(2) + '%'
          : 'N/A',
    };
  }

  /**
   * Reset metrics
   */
  resetMetrics() {
    this.metrics = {
      totalAttempts: 0,
      successfulRetries: 0,
      failedRetries: 0,
      permanentFailures: 0,
    };
  }
}

/**
 * Error thrown when retry attempts are exhausted
 */
export class RetryExhaustedError extends Error {
  /**
   * Create a retry exhausted error
   * @param {string} message - Error message
   * @param {number} attempts - Number of attempts made
   * @param {Error} lastError - Last error encountered
   */
  constructor(message, attempts, lastError) {
    super(message);
    this.name = 'RetryExhaustedError';
    this.attempts = attempts;
    this.lastError = lastError;
  }
}

/**
 * Convenience function to retry an operation
 * @param {Function} operation - Async operation
 * @param {Object} [config] - Retry configuration
 * @returns {Promise<any>} Operation result
 */
export async function withRetry(operation, config = {}) {
  const retry = new RetryStrategy(config);
  return await retry.execute(operation);
}

/**
 * Decorator for adding retry to any function
 * @param {Function} fn - Function to wrap
 * @param {Object} [config] - Retry configuration
 * @returns {Function} Wrapped function
 */
export function retryable(fn, config = {}) {
  const retry = new RetryStrategy(config);
  return async function (...args) {
    return await retry.execute(() => fn.apply(this, args));
  };
}

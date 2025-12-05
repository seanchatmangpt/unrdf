/**
 * @file Sidecar retry logic with circuit breaker pattern
 * @module cli/utils/retry-logic
 *
 * Implements exponential backoff retry strategy for network operations
 * FM-CLI-009: Prevent false alarms from transient network failures
 */

/**
 * Exponential backoff retry configuration
 */
export const DEFAULT_RETRY_CONFIG = {
  maxRetries: 4,
  initialDelay: 500,    // 500ms
  maxDelay: 10000,      // 10s
  backoffFactor: 2,     // exponential factor
  jitter: true          // add randomness
};

/**
 * Retry a function with exponential backoff
 *
 * @param {Function} fn - Function to retry
 * @param {Object} config - Retry configuration
 * @returns {Promise<any>} Result of successful function call
 *
 * @example
 * const response = await retryWithBackoff(
 *   () => fetchSidecarHealth(),
 *   { maxRetries: 3, initialDelay: 200 }
 * );
 */
export async function retryWithBackoff(fn, config = {}) {
  const {
    maxRetries = DEFAULT_RETRY_CONFIG.maxRetries,
    initialDelay = DEFAULT_RETRY_CONFIG.initialDelay,
    maxDelay = DEFAULT_RETRY_CONFIG.maxDelay,
    backoffFactor = DEFAULT_RETRY_CONFIG.backoffFactor,
    jitter = DEFAULT_RETRY_CONFIG.jitter
  } = config;

  let lastError;
  let delay = initialDelay;

  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error;

      if (attempt === maxRetries) {
        // All retries exhausted
        break;
      }

      // Calculate next delay with jitter
      let nextDelay = delay * backoffFactor;
      if (jitter) {
        nextDelay = nextDelay * (0.5 + Math.random());
      }
      nextDelay = Math.min(nextDelay, maxDelay);

      console.warn(
        `⚠️  Attempt ${attempt + 1}/${maxRetries} failed: ${error.message}. ` +
        `Retrying in ${Math.round(nextDelay)}ms...`
      );

      await new Promise(resolve => setTimeout(resolve, nextDelay));
      delay = nextDelay;
    }
  }

  throw lastError;
}

/**
 * Circuit breaker pattern implementation
 *
 * Prevents cascading failures by stopping attempts after threshold is reached
 */
export class CircuitBreaker {
  constructor(name, config = {}) {
    this.name = name;
    this.failureThreshold = config.failureThreshold || 5;
    this.resetTimeout = config.resetTimeout || 60000; // 1 minute
    this.state = 'CLOSED'; // CLOSED, OPEN, HALF_OPEN
    this.failureCount = 0;
    this.lastFailureTime = null;
    this.lastResetTime = null;
  }

  /**
   * Execute function through circuit breaker
   */
  async execute(fn) {
    if (this.state === 'OPEN') {
      if (Date.now() - this.lastFailureTime > this.resetTimeout) {
        this.state = 'HALF_OPEN';
        console.log(`🔄 Circuit breaker "${this.name}" attempting reset...`);
      } else {
        throw new Error(
          `Circuit breaker "${this.name}" is OPEN. ` +
          `Service unavailable. Retry in ${Math.round((this.resetTimeout - (Date.now() - this.lastFailureTime)) / 1000)}s`
        );
      }
    }

    try {
      const result = await fn();

      // Success - reset breaker
      if (this.state === 'HALF_OPEN') {
        this.state = 'CLOSED';
        this.failureCount = 0;
        console.log(`✅ Circuit breaker "${this.name}" closed`);
      }

      return result;
    } catch (error) {
      this.failureCount++;
      this.lastFailureTime = Date.now();

      if (this.failureCount >= this.failureThreshold) {
        this.state = 'OPEN';
        console.error(
          `🚨 Circuit breaker "${this.name}" opened after ${this.failureCount} failures`
        );
      }

      throw error;
    }
  }

  /**
   * Manual reset
   */
  reset() {
    this.state = 'CLOSED';
    this.failureCount = 0;
    this.lastFailureTime = null;
    console.log(`🔄 Circuit breaker "${this.name}" manually reset`);
  }
}

export default {
  retryWithBackoff,
  CircuitBreaker,
  DEFAULT_RETRY_CONFIG
};

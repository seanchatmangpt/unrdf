/**
 * @file Durable Activity - Unit of work with retries and timeouts
 * @module @unrdf/yawl-durable/activity
 *
 * Activities are the building blocks of workflows. Each activity:
 * - Has configurable timeout
 * - Supports automatic retries with exponential backoff
 * - Can have compensation logic for sagas
 * - Generates receipts for deterministic replay
 */

import { z } from 'zod';

// =============================================================================
// Retry Logic
// =============================================================================

/**
 * Calculate backoff delay for retry attempt
 *
 * @param {number} attempt - Current attempt number (1-based)
 * @param {Object} policy - Retry policy
 * @returns {number} Delay in milliseconds
 */
function calculateBackoff(attempt, policy) {
  const delay = policy.initialInterval * Math.pow(policy.backoffCoefficient, attempt - 1);
  return Math.min(delay, policy.maximumInterval);
}

/**
 * Sleep for specified milliseconds
 * @param {number} ms - Milliseconds to sleep
 * @returns {Promise<void>}
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

// =============================================================================
// DurableActivity
// =============================================================================

/**
 * Durable Activity with retry and timeout support
 *
 * @example
 * const activity = new DurableActivity({
 *   id: 'bookFlight',
 *   name: 'Book Flight',
 *   handler: async (input) => {
 *     // Call external API
 *     const booking = await flightAPI.book(input.flightId);
 *     return { bookingId: booking.id, status: 'CONFIRMED' };
 *   },
 *   timeout: 30000,
 *   retryPolicy: {
 *     maxAttempts: 3,
 *     initialInterval: 1000,
 *     backoffCoefficient: 2,
 *   },
 *   compensate: async (output) => {
 *     // Rollback the booking
 *     await flightAPI.cancel(output.bookingId);
 *   },
 * });
 */
export class DurableActivity {
  /**
   * @param {Object} config - Activity configuration
   */
  constructor(config) {
    this.config = config;
    this.executionCount = 0;
    this.lastResult = null;
    this.lastError = null;
  }

  /**
   * Execute activity with retry and timeout logic
   *
   * This implements the core durable execution semantics:
   * 1. Execute with timeout
   * 2. Retry on failure with exponential backoff
   * 3. Track execution history
   * 4. Return deterministic result
   *
   * @param {Object} input - Activity input
   * @param {Object} context - Execution context
   * @returns {Promise<Object>} Activity result
   */
  async execute(input, context) {
    const retryPolicy = this.config.retryPolicy || {
      maxAttempts: 1,
      initialInterval: 1000,
      backoffCoefficient: 1,
      maximumInterval: 60000,
    };

    let lastError = null;

    for (let attempt = 1; attempt <= retryPolicy.maxAttempts; attempt++) {
      try {
        this.executionCount++;

        // Execute with timeout
        const result = await this._executeWithTimeout(input, context, attempt);

        this.lastResult = result;
        this.lastError = null;

        return result;
      } catch (error) {
        lastError = error;
        this.lastError = error;

        // Don't retry if this was the last attempt
        if (attempt >= retryPolicy.maxAttempts) {
          break;
        }

        // Calculate backoff and wait
        const backoff = calculateBackoff(attempt, retryPolicy);
        console.log(`Activity ${this.config.id} attempt ${attempt} failed, retrying in ${backoff}ms...`);
        await sleep(backoff);
      }
    }

    // All retries exhausted
    throw new Error(
      `Activity ${this.config.id} failed after ${retryPolicy.maxAttempts} attempts: ${lastError.message}`
    );
  }

  /**
   * Execute activity with timeout enforcement
   * @private
   */
  async _executeWithTimeout(input, context, attempt) {
    const timeout = this.config.timeout || 30000;

    return Promise.race([
      // Actual execution
      this.config.handler(input, {
        ...context,
        attempt,
        activityId: this.config.id,
      }),

      // Timeout
      new Promise((_, reject) => {
        setTimeout(() => {
          reject(new Error(`Activity ${this.config.id} timed out after ${timeout}ms`));
        }, timeout);
      }),
    ]);
  }

  /**
   * Execute compensation logic (for saga rollback)
   *
   * @param {Object} output - Original activity output to compensate
   * @param {Object} context - Execution context
   * @returns {Promise<void>}
   */
  async compensate(output, context) {
    if (!this.config.compensate) {
      console.warn(`Activity ${this.config.id} has no compensation handler`);
      return;
    }

    try {
      await this.config.compensate(output, context);
      console.log(`Activity ${this.config.id} compensated successfully`);
    } catch (error) {
      console.error(`Compensation failed for ${this.config.id}:`, error);
      throw error;
    }
  }

  /**
   * Get activity execution statistics
   * @returns {Object} Execution stats
   */
  getStats() {
    return {
      activityId: this.config.id,
      executionCount: this.executionCount,
      hasLastResult: this.lastResult !== null,
      hasLastError: this.lastError !== null,
      lastError: this.lastError ? this.lastError.message : null,
    };
  }
}

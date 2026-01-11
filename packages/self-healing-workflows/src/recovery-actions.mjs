/**
 * @file Recovery action catalog
 * @module @unrdf/self-healing-workflows/recovery
 * @description Library of recovery actions for error handling
 */

import { RecoveryActionSchema } from './schemas.mjs';

/**
 * Recovery action executor
 */
export class RecoveryActionExecutor {
  /**
   * Creates a new recovery action executor
   */
  constructor() {
    this.actions = new Map();
    this.stats = new Map();
    this.registerDefaultActions();
  }

  /**
   * Registers default recovery actions
   * @returns {void}
   */
  registerDefaultActions() {
    // Retry action
    this.register({
      type: 'retry',
      name: 'retry-operation',
      execute: async (context) => {
        const { operation, maxAttempts = 3 } = context;
        let attempt = 0;
        let lastError;

        while (attempt < maxAttempts) {
          try {
            return await operation();
          } catch (error) {
            lastError = error;
            attempt++;
          }
        }

        throw lastError;
      },
      condition: (error) => error.retryable,
      priority: 80
    });

    // Skip action
    this.register({
      type: 'skip',
      name: 'skip-and-continue',
      execute: async (context) => {
        return { skipped: true, reason: context.error?.message };
      },
      priority: 20
    });

    // Compensate action
    this.register({
      type: 'compensate',
      name: 'compensating-transaction',
      execute: async (context) => {
        const { compensationFn } = context;
        if (compensationFn) {
          await compensationFn();
        }
        return { compensated: true };
      },
      priority: 60
    });

    // Restart action
    this.register({
      type: 'restart',
      name: 'restart-workflow',
      execute: async (context) => {
        const { workflow } = context;
        if (workflow && workflow.restart) {
          await workflow.restart();
        }
        return { restarted: true };
      },
      priority: 40
    });

    // Fallback action
    this.register({
      type: 'fallback',
      name: 'use-fallback',
      execute: async (context) => {
        const { fallbackFn } = context;
        if (fallbackFn) {
          return await fallbackFn();
        }
        throw new Error('No fallback function provided');
      },
      priority: 50
    });

    // Manual intervention action
    this.register({
      type: 'manual',
      name: 'require-manual-intervention',
      execute: async (context) => {
        const { notificationFn, error } = context;
        if (notificationFn) {
          await notificationFn({
            type: 'manual-intervention-required',
            error: error?.originalError || error,
            timestamp: Date.now()
          });
        }
        return { requiresManualIntervention: true };
      },
      priority: 10
    });
  }

  /**
   * Registers a recovery action
   * @param {Object} action - Recovery action definition
   * @returns {void}
   */
  register(action) {
    const validated = RecoveryActionSchema.parse(action);
    const key = `${validated.type}:${validated.name}`;
    this.actions.set(key, validated);
    this.stats.set(key, {
      attempts: 0,
      successes: 0,
      failures: 0
    });
  }

  /**
   * Executes a recovery action
   * @param {string} type - Action type
   * @param {string} name - Action name
   * @param {Object} context - Execution context
   * @returns {Promise<Object>} Recovery result
   * @throws {Error} If action not found or execution fails
   */
  async execute(type, name, context) {
    const key = `${type}:${name}`;
    const action = this.actions.get(key);

    if (!action) {
      throw new Error(`Recovery action not found: ${key}`);
    }

    const stats = this.stats.get(key);
    stats.attempts++;

    try {
      const result = await action.execute(context);
      stats.successes++;
      return result;
    } catch (err) {
      stats.failures++;
      throw err;
    }
  }

  /**
   * Selects best recovery action for an error
   * @param {Object} classifiedError - Classified error object
   * @param {Object} [_context] - Additional context
   * @returns {Object|null} Selected action or null
   */
  selectAction(classifiedError, _context = {}) {
    const candidates = [];

    for (const [key, action] of this.actions) {
      // Check if action condition matches
      if (action.condition && !action.condition(classifiedError)) {
        continue;
      }

      candidates.push({ key, action });
    }

    if (candidates.length === 0) {
      return null;
    }

    // Sort by priority (highest first)
    candidates.sort((a, b) => b.action.priority - a.action.priority);

    return candidates[0];
  }

  /**
   * Executes best recovery action for an error
   * @param {Object} classifiedError - Classified error object
   * @param {Object} context - Execution context
   * @returns {Promise<Object>} Recovery result
   * @throws {Error} If no suitable action found or execution fails
   */
  async recover(classifiedError, context) {
    const selected = this.selectAction(classifiedError, context);

    if (!selected) {
      throw new Error('No suitable recovery action found');
    }

    const { action } = selected;
    return this.execute(action.type, action.name, {
      ...context,
      error: classifiedError
    });
  }

  /**
   * Gets all registered actions
   * @returns {Array<Object>} Registered actions
   */
  getActions() {
    return Array.from(this.actions.values());
  }

  /**
   * Gets action statistics
   * @returns {Object} Statistics by action
   */
  getStats() {
    const stats = {};
    for (const [key, stat] of this.stats) {
      stats[key] = {
        ...stat,
        successRate: stat.attempts > 0 ? stat.successes / stat.attempts : 0
      };
    }
    return stats;
  }

  /**
   * Resets action statistics
   * @returns {void}
   */
  resetStats() {
    for (const [key] of this.stats) {
      this.stats.set(key, {
        attempts: 0,
        successes: 0,
        failures: 0
      });
    }
  }

  /**
   * Removes a recovery action
   * @param {string} type - Action type
   * @param {string} name - Action name
   * @returns {boolean} True if action was removed
   */
  unregister(type, name) {
    const key = `${type}:${name}`;
    this.stats.delete(key);
    return this.actions.delete(key);
  }
}

/**
 * Creates a new recovery action executor
 * @returns {RecoveryActionExecutor} Recovery action executor
 */
export function createRecoveryActionExecutor() {
  return new RecoveryActionExecutor();
}

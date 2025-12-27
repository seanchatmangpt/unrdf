/**
 * @file Hook Composition - Advanced Hook Chaining Patterns
 * @module kgc-claude/capabilities/hook-composition
 *
 * @description
 * Hyper-advanced hook composition patterns for Claude Code.
 * Implements before/after/around hooks, conditional execution,
 * and priority ordering for complex governance workflows.
 *
 * **Patterns:**
 * - Before/After/Around hooks (AOP-style)
 * - Conditional hook execution
 * - Hook priority ordering
 * - Async hook chains
 * - Error boundary hooks
 * - Circuit breaker pattern
 */

import { z } from 'zod';

/* ========================================================================= */
/* Schemas                                                                   */
/* ========================================================================= */

/**
 * Hook execution phase
 * @enum {string}
 */
export const HookPhase = {
  BEFORE: 'before',
  AFTER: 'after',
  AROUND: 'around',
  ERROR: 'error',
  FINALLY: 'finally',
};

/**
 * Schema for composed hook
 */
export const ComposedHookSchema = z.object({
  name: z.string().min(1),
  phase: z.enum(['before', 'after', 'around', 'error', 'finally']),
  execute: z.function(),
  condition: z.function().optional(), // (context) => boolean
  priority: z.number().int().min(0).max(100).default(50),
  timeout: z.number().int().positive().optional(),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * Schema for hook composition config
 */
export const CompositionConfigSchema = z.object({
  name: z.string().min(1),
  hooks: z.array(ComposedHookSchema),
  errorStrategy: z.enum(['stop', 'continue', 'skip']).default('stop'),
  timeout: z.number().int().positive().default(5000),
  circuitBreaker: z
    .object({
      enabled: z.boolean().default(false),
      threshold: z.number().int().positive().default(3),
      resetTimeout: z.number().int().positive().default(60000),
    })
    .optional(),
});

/* ========================================================================= */
/* Hook Composer                                                             */
/* ========================================================================= */

/**
 * Hook Composer for advanced hook chaining
 */
export class HookComposer {
  /**
   * Create a new hook composer
   * @param {Object} config - Composition configuration
   */
  constructor(config) {
    this.config = CompositionConfigSchema.parse(config);
    this.hooks = {
      before: [],
      after: [],
      around: [],
      error: [],
      finally: [],
    };

    // Circuit breaker state
    this.circuitState = {
      failures: 0,
      lastFailure: null,
      open: false,
    };

    // Organize hooks by phase
    for (const hook of this.config.hooks) {
      this.hooks[hook.phase].push(hook);
    }

    // Sort hooks by priority (higher first)
    for (const phase of Object.keys(this.hooks)) {
      this.hooks[phase].sort((a, b) => (b.priority || 50) - (a.priority || 50));
    }
  }

  /**
   * Execute composed hook chain
   * @param {Function} targetFn - Target function to execute
   * @param {Object} context - Execution context
   * @returns {Promise<Object>} Execution result
   */
  async execute(targetFn, context = {}) {
    // Check circuit breaker
    if (this._isCircuitOpen()) {
      return {
        success: false,
        error: 'Circuit breaker is open',
        circuitOpen: true,
      };
    }

    const result = {
      success: true,
      value: null,
      beforeResults: [],
      afterResults: [],
      errors: [],
      timing: {
        start: Date.now(),
        before: 0,
        target: 0,
        after: 0,
        total: 0,
      },
    };

    try {
      // Execute BEFORE hooks
      const beforeStart = Date.now();
      for (const hook of this.hooks.before) {
        if (this._shouldExecute(hook, context)) {
          const hookResult = await this._executeHook(hook, context, 'before');
          result.beforeResults.push(hookResult);

          if (!hookResult.success && this.config.errorStrategy === 'stop') {
            throw new Error(`Before hook failed: ${hook.name}`);
          }

          // Merge context changes
          Object.assign(context, hookResult.context || {});
        }
      }
      result.timing.before = Date.now() - beforeStart;

      // Execute AROUND hooks or target function
      const targetStart = Date.now();
      if (this.hooks.around.length > 0) {
        // Execute around hooks (they control target execution)
        for (const hook of this.hooks.around) {
          if (this._shouldExecute(hook, context)) {
            const hookResult = await this._executeHook(hook, context, 'around', targetFn);
            result.value = hookResult.value;

            if (!hookResult.success && this.config.errorStrategy === 'stop') {
              throw new Error(`Around hook failed: ${hook.name}`);
            }
          }
        }
      } else {
        // Execute target function directly
        result.value = await targetFn(context);
      }
      result.timing.target = Date.now() - targetStart;

      // Execute AFTER hooks
      const afterStart = Date.now();
      for (const hook of this.hooks.after) {
        if (this._shouldExecute(hook, context)) {
          const hookResult = await this._executeHook(
            hook,
            { ...context, result: result.value },
            'after'
          );
          result.afterResults.push(hookResult);

          if (!hookResult.success && this.config.errorStrategy === 'stop') {
            throw new Error(`After hook failed: ${hook.name}`);
          }

          // After hooks can modify result
          if (hookResult.value !== undefined) {
            result.value = hookResult.value;
          }
        }
      }
      result.timing.after = Date.now() - afterStart;

      // Reset circuit breaker on success
      this._resetCircuitBreaker();
    } catch (error) {
      result.success = false;
      result.error = error.message;
      result.stack = error.stack;

      // Execute ERROR hooks
      for (const hook of this.hooks.error) {
        if (this._shouldExecute(hook, context)) {
          const hookResult = await this._executeHook(
            hook,
            { ...context, error },
            'error'
          );

          // Error hooks can recover from errors
          if (hookResult.recovered) {
            result.success = true;
            result.value = hookResult.value;
            result.recovered = true;
          }
        }
      }

      // Trip circuit breaker on failure
      if (!result.success) {
        this._tripCircuitBreaker();
      }
    } finally {
      // Execute FINALLY hooks
      for (const hook of this.hooks.finally) {
        if (this._shouldExecute(hook, context)) {
          await this._executeHook(hook, { ...context, result }, 'finally');
        }
      }

      result.timing.total = Date.now() - result.timing.start;
    }

    return result;
  }

  /**
   * Add a hook to the composition
   * @param {Object} hook - Hook to add
   */
  addHook(hook) {
    const validHook = ComposedHookSchema.parse(hook);
    this.hooks[validHook.phase].push(validHook);

    // Re-sort by priority
    this.hooks[validHook.phase].sort((a, b) => (b.priority || 50) - (a.priority || 50));
  }

  /**
   * Remove a hook from the composition
   * @param {string} name - Hook name
   * @param {string} [phase] - Hook phase (optional)
   */
  removeHook(name, phase = null) {
    if (phase) {
      this.hooks[phase] = this.hooks[phase].filter(h => h.name !== name);
    } else {
      // Remove from all phases
      for (const p of Object.keys(this.hooks)) {
        this.hooks[p] = this.hooks[p].filter(h => h.name !== name);
      }
    }
  }

  /**
   * Get composition statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      name: this.config.name,
      hooks: {
        before: this.hooks.before.length,
        after: this.hooks.after.length,
        around: this.hooks.around.length,
        error: this.hooks.error.length,
        finally: this.hooks.finally.length,
        total:
          this.hooks.before.length +
          this.hooks.after.length +
          this.hooks.around.length +
          this.hooks.error.length +
          this.hooks.finally.length,
      },
      circuitBreaker: {
        enabled: this.config.circuitBreaker?.enabled || false,
        failures: this.circuitState.failures,
        open: this.circuitState.open,
        lastFailure: this.circuitState.lastFailure,
      },
    };
  }

  /* ======================================================================= */
  /* Private Methods                                                         */
  /* ======================================================================= */

  /**
   * Check if hook should execute
   * @private
   */
  _shouldExecute(hook, context) {
    if (!hook.condition) return true;
    try {
      return hook.condition(context);
    } catch (error) {
      console.warn(`Hook condition failed: ${hook.name}`, error);
      return false;
    }
  }

  /**
   * Execute a single hook
   * @private
   */
  async _executeHook(hook, context, phase, targetFn = null) {
    const result = {
      name: hook.name,
      phase,
      success: true,
      value: null,
      context: {},
      timing: 0,
    };

    const start = Date.now();

    try {
      // Execute with timeout
      const timeout = hook.timeout || this.config.timeout;
      const executePromise =
        phase === 'around'
          ? hook.execute(context, targetFn)
          : hook.execute(context);

      const timeoutPromise = new Promise((_, reject) =>
        setTimeout(() => reject(new Error(`Hook timeout: ${hook.name}`)), timeout)
      );

      result.value = await Promise.race([executePromise, timeoutPromise]);
    } catch (error) {
      result.success = false;
      result.error = error.message;
      result.stack = error.stack;
    } finally {
      result.timing = Date.now() - start;
    }

    return result;
  }

  /**
   * Check if circuit breaker is open
   * @private
   */
  _isCircuitOpen() {
    if (!this.config.circuitBreaker?.enabled) return false;

    if (this.circuitState.open) {
      const resetTimeout = this.config.circuitBreaker.resetTimeout;
      const elapsed = Date.now() - this.circuitState.lastFailure;

      if (elapsed > resetTimeout) {
        // Try to close circuit
        this.circuitState.open = false;
        this.circuitState.failures = 0;
        return false;
      }

      return true;
    }

    return false;
  }

  /**
   * Trip circuit breaker
   * @private
   */
  _tripCircuitBreaker() {
    if (!this.config.circuitBreaker?.enabled) return;

    this.circuitState.failures++;
    this.circuitState.lastFailure = Date.now();

    if (this.circuitState.failures >= this.config.circuitBreaker.threshold) {
      this.circuitState.open = true;
      console.warn(
        `Circuit breaker opened for ${this.config.name} after ${this.circuitState.failures} failures`
      );
    }
  }

  /**
   * Reset circuit breaker
   * @private
   */
  _resetCircuitBreaker() {
    if (!this.config.circuitBreaker?.enabled) return;
    this.circuitState.failures = 0;
    this.circuitState.open = false;
  }
}

/* ========================================================================= */
/* Factory Functions                                                         */
/* ========================================================================= */

/**
 * Create a hook composer
 * @param {string} name - Composer name
 * @param {Array} [hooks] - Initial hooks
 * @param {Object} [options] - Composer options
 * @returns {HookComposer} Hook composer
 */
export function createHookComposer(name, hooks = [], options = {}) {
  return new HookComposer({
    name,
    hooks,
    errorStrategy: options.errorStrategy || 'stop',
    timeout: options.timeout || 5000,
    circuitBreaker: options.circuitBreaker,
  });
}

/**
 * Create a before hook
 * @param {string} name - Hook name
 * @param {Function} execute - Execution function
 * @param {Object} [options] - Hook options
 * @returns {Object} Hook definition
 */
export function createBeforeHook(name, execute, options = {}) {
  return {
    name,
    phase: 'before',
    execute,
    condition: options.condition,
    priority: options.priority || 50,
    timeout: options.timeout,
    metadata: options.metadata,
  };
}

/**
 * Create an after hook
 * @param {string} name - Hook name
 * @param {Function} execute - Execution function
 * @param {Object} [options] - Hook options
 * @returns {Object} Hook definition
 */
export function createAfterHook(name, execute, options = {}) {
  return {
    name,
    phase: 'after',
    execute,
    condition: options.condition,
    priority: options.priority || 50,
    timeout: options.timeout,
    metadata: options.metadata,
  };
}

/**
 * Create an around hook
 * @param {string} name - Hook name
 * @param {Function} execute - Execution function (context, targetFn) => result
 * @param {Object} [options] - Hook options
 * @returns {Object} Hook definition
 */
export function createAroundHook(name, execute, options = {}) {
  return {
    name,
    phase: 'around',
    execute,
    condition: options.condition,
    priority: options.priority || 50,
    timeout: options.timeout,
    metadata: options.metadata,
  };
}

/**
 * Create an error hook
 * @param {string} name - Hook name
 * @param {Function} execute - Execution function
 * @param {Object} [options] - Hook options
 * @returns {Object} Hook definition
 */
export function createErrorHook(name, execute, options = {}) {
  return {
    name,
    phase: 'error',
    execute,
    condition: options.condition,
    priority: options.priority || 50,
    timeout: options.timeout,
    metadata: options.metadata,
  };
}

/**
 * Create a finally hook
 * @param {string} name - Hook name
 * @param {Function} execute - Execution function
 * @param {Object} [options] - Hook options
 * @returns {Object} Hook definition
 */
export function createFinallyHook(name, execute, options = {}) {
  return {
    name,
    phase: 'finally',
    execute,
    condition: options.condition,
    priority: options.priority || 50,
    timeout: options.timeout,
    metadata: options.metadata,
  };
}

/* ========================================================================= */
/* Predefined Compositions                                                   */
/* ========================================================================= */

/**
 * Create a logging composition
 * @param {string} name - Composition name
 * @param {Function} logger - Logger function
 * @returns {HookComposer} Hook composer
 */
export function createLoggingComposition(name, logger = console.log) {
  const hooks = [
    createBeforeHook('log-before', async context => {
      logger(`[${name}] Before:`, context);
    }),
    createAfterHook('log-after', async context => {
      logger(`[${name}] After:`, context.result);
    }),
    createErrorHook('log-error', async context => {
      logger(`[${name}] Error:`, context.error);
    }),
  ];

  return createHookComposer(name, hooks);
}

/**
 * Create a validation composition
 * @param {string} name - Composition name
 * @param {Function} validator - Validator function (context) => boolean
 * @returns {HookComposer} Hook composer
 */
export function createValidationComposition(name, validator) {
  const hooks = [
    createBeforeHook(
      'validate-input',
      async context => {
        if (!validator(context)) {
          throw new Error('Validation failed');
        }
      },
      { priority: 90 }
    ),
  ];

  return createHookComposer(name, hooks);
}

/**
 * Create a retry composition
 * @param {string} name - Composition name
 * @param {number} maxRetries - Maximum retry attempts
 * @returns {HookComposer} Hook composer
 */
export function createRetryComposition(name, maxRetries = 3) {
  const hooks = [
    createAroundHook('retry-on-failure', async (context, targetFn) => {
      let lastError;
      for (let attempt = 0; attempt < maxRetries; attempt++) {
        try {
          return await targetFn(context);
        } catch (error) {
          lastError = error;
          if (attempt < maxRetries - 1) {
            await new Promise(resolve => setTimeout(resolve, Math.pow(2, attempt) * 100));
          }
        }
      }
      throw lastError;
    }),
  ];

  return createHookComposer(name, hooks);
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
  HookComposer,
  HookPhase,
  createHookComposer,
  createBeforeHook,
  createAfterHook,
  createAroundHook,
  createErrorHook,
  createFinallyHook,
  createLoggingComposition,
  createValidationComposition,
  createRetryComposition,
};

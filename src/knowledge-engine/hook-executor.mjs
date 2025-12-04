/**
 * @file Production hook execution engine.
 * @module hook-executor
 *
 * @description
 * Production-ready hook execution engine that evaluates conditions,
 * executes hook lifecycles, and integrates with the knowledge engine.
 */

import { createConditionEvaluator } from './condition-evaluator.mjs';
import { createEffectSandbox } from './effect-sandbox.mjs';
import { createErrorSanitizer } from './security/error-sanitizer.mjs';
import { createSandboxRestrictions } from './security/sandbox-restrictions.mjs';
import { createStore } from '@unrdf/oxigraph';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf');

/**
 * Execute a knowledge hook with full lifecycle management.
 * @param {Object} hook - The hook definition
 * @param {Object} event - The hook event
 * @param {Object} [options] - Execution options
 * @returns {Promise<Object>} Hook execution result
 *
 * @throws {Error} If hook execution fails
 */
export async function executeHook(hook, event, options = {}) {
  if (!hook || typeof hook !== 'object') {
    throw new TypeError('executeHook: hook must be an object');
  }
  if (!event || typeof event !== 'object') {
    throw new TypeError('executeHook: event must be an object');
  }

  const {
    basePath = process.cwd(),
    strictMode = false,
    timeoutMs = 30000,
    enableConditionEvaluation = true,
    enableSandboxing = true,
    sandboxConfig = {},
  } = options;

  const startTime = Date.now();
  const executionId = `hook-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

  try {
    // Set up timeout
    const timeoutPromise = new Promise((_, reject) => {
      setTimeout(() => reject(new Error(`Hook execution timeout after ${timeoutMs}ms`)), timeoutMs);
    });

    const executionPromise = _executeHookLifecycle(hook, event, {
      basePath,
      strictMode,
      enableConditionEvaluation,
      enableSandboxing,
      sandboxConfig,
      executionId,
    });

    const result = await Promise.race([executionPromise, timeoutPromise]);

    return {
      ...result,
      executionId,
      durationMs: Date.now() - startTime,
      success: true,
    };
  } catch (error) {
    // Sanitize error message to prevent information disclosure
    const errorSanitizer = createErrorSanitizer();
    const sanitizedError = errorSanitizer.sanitize(error);

    return {
      executionId,
      durationMs: Date.now() - startTime,
      success: false,
      error: sanitizedError,
      cancelled: false,
    };
  }
}

/**
 * Execute the complete hook lifecycle.
 * @param {Object} hook - The hook definition
 * @param {Object} event - The hook event
 * @param {Object} options - Execution options
 * @returns {Promise<Object>} Lifecycle execution result
 */
async function _executeHookLifecycle(hook, event, options) {
  const {
    _basePath,
    strictMode,
    _enableConditionEvaluation,
    enableSandboxing,
    _sandboxConfig,
    executionId,
  } = options;

  return tracer.startActiveSpan('hook.evaluate', async span => {
    try {
      span.setAttributes({
        'hook.execution_id': executionId,
        'hook.has_before': !!hook.before,
        'hook.has_when': !!hook.when,
        'hook.has_run': !!hook.run,
        'hook.has_after': !!hook.after,
        'hook.strict_mode': strictMode,
        'hook.sandboxing_enabled': enableSandboxing,
      });

      let currentEvent = { ...event };
      let beforeResult = null;
      let runResult = null;
      let afterResult = null;
      let conditionResult = null;
      let cancelled = false;
      let cancelReason = null;

      const result = await _executeHookPhases(
        hook,
        currentEvent,
        beforeResult,
        runResult,
        afterResult,
        conditionResult,
        cancelled,
        cancelReason,
        options
      );

      span.setAttributes({
        'hook.cancelled': result.cancelled || false,
        'hook.success': result.success || false,
        'hook.phase': result.phase || 'unknown',
      });

      span.setStatus({ code: SpanStatusCode.OK });
      return result;
    } catch (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      throw error;
    } finally {
      span.end();
    }
  });
}

async function _executeHookPhases(
  hook,
  currentEvent,
  beforeResult,
  runResult,
  afterResult,
  conditionResult,
  cancelled,
  cancelReason,
  options
) {
  const {
    basePath,
    strictMode,
    enableConditionEvaluation,
    enableSandboxing,
    sandboxConfig,
    _executionId,
  } = options;

  // Phase 1: Before
  if (hook.before) {
    try {
      if (enableSandboxing) {
        const sandbox = createEffectSandbox(sandboxConfig);
        beforeResult = await sandbox.executeEffect(hook.before, {
          event: currentEvent,
          store: currentEvent.context?.graph,
          delta: currentEvent.payload?.delta,
          metadata: currentEvent.context?.metadata || {},
        });
      } else {
        beforeResult = await hook.before(currentEvent);
      }

      if (beforeResult && beforeResult.cancel) {
        cancelled = true;
        cancelReason = beforeResult.reason || 'Cancelled in before phase';
        return {
          beforeResult,
          cancelled: true,
          cancelReason,
          phase: 'before',
          success: false,
        };
      }

      // Merge before result into event payload
      if (beforeResult && typeof beforeResult === 'object' && !beforeResult.cancel) {
        currentEvent = {
          ...currentEvent,
          payload: { ...currentEvent.payload, ...beforeResult },
        };
      }
    } catch (error) {
      // Sanitize error message
      const errorSanitizer = createErrorSanitizer();
      const sanitizedError = errorSanitizer.sanitize(error);

      if (strictMode) {
        throw new Error(`Before phase failed: ${sanitizedError}`);
      }
      return {
        beforeResult: { error: sanitizedError },
        cancelled: true,
        cancelReason: `Before phase error: ${sanitizedError}`,
        phase: 'before',
      };
    }
  }

  // Phase 2: Condition Evaluation
  if (enableConditionEvaluation && hook.when) {
    try {
      const evaluator = createConditionEvaluator({ basePath, strictMode });
      conditionResult = await evaluator.evaluate(
        hook.when,
        currentEvent.context?.graph || createStore(),
        currentEvent.context?.env || {}
      );

      // Check if condition is satisfied
      const isSatisfied = await evaluator.isSatisfied(
        hook.when,
        currentEvent.context?.graph || createStore(),
        currentEvent.context?.env || {}
      );

      if (!isSatisfied) {
        return {
          beforeResult,
          conditionResult,
          cancelled: true,
          cancelReason: 'Condition not satisfied',
          phase: 'condition',
        };
      }
    } catch (error) {
      // Sanitize error message
      const errorSanitizer = createErrorSanitizer();
      const sanitizedError = errorSanitizer.sanitize(error);

      if (strictMode) {
        throw new Error(`Condition evaluation failed: ${sanitizedError}`);
      }
      return {
        beforeResult,
        conditionResult: { error: sanitizedError },
        cancelled: true,
        cancelReason: `Condition evaluation error: ${sanitizedError}`,
        phase: 'condition',
        success: false,
      };
    }
  }

  // Phase 3: Run
  if (hook.run) {
    try {
      if (enableSandboxing) {
        // Use security sandbox with restrictions
        const sandboxRestrictions = createSandboxRestrictions(sandboxConfig);
        runResult = await sandboxRestrictions.executeRestricted(hook.run, currentEvent);
      } else {
        runResult = await hook.run(currentEvent);
      }
    } catch (error) {
      // Sanitize error message
      const errorSanitizer = createErrorSanitizer();
      const sanitizedError = errorSanitizer.sanitize(error);

      if (strictMode) {
        throw new Error(`Run phase failed: ${sanitizedError}`);
      }
      return {
        beforeResult,
        conditionResult,
        runResult: { error: sanitizedError },
        cancelled: true,
        cancelReason: `Run phase error: ${sanitizedError}`,
        phase: 'run',
        success: false,
      };
    }
  }

  // Phase 4: After
  if (hook.after) {
    try {
      if (enableSandboxing) {
        const sandbox = createEffectSandbox(sandboxConfig);
        afterResult = await sandbox.executeEffect(hook.after, {
          event: {
            ...currentEvent,
            result: runResult,
            cancelled: false,
          },
          store: currentEvent.context?.graph,
          delta: currentEvent.payload?.delta,
          metadata: currentEvent.context?.metadata || {},
        });
      } else {
        afterResult = await hook.after({
          ...currentEvent,
          result: runResult,
          cancelled: false,
        });
      }
    } catch (error) {
      // Sanitize error message
      const errorSanitizer = createErrorSanitizer();
      const sanitizedError = errorSanitizer.sanitize(error);

      if (strictMode) {
        throw new Error(`After phase failed: ${sanitizedError}`);
      }
      // After phase errors don't cancel the hook, just log them
      afterResult = { error: sanitizedError };
    }
  }

  const finalResult = {
    beforeResult,
    conditionResult,
    runResult,
    afterResult,
    cancelled: false,
    phase: 'completed',
    success: !cancelled && (!runResult || !runResult.error),
  };

  // Record hook result span
  return tracer.startActiveSpan('hook.result', resultSpan => {
    try {
      resultSpan.setAttributes({
        'hook.result.success': finalResult.success,
        'hook.result.cancelled': finalResult.cancelled,
        'hook.result.phase': finalResult.phase,
        'hook.result.has_before': !!beforeResult,
        'hook.result.has_condition': !!conditionResult,
        'hook.result.has_run': !!runResult,
        'hook.result.has_after': !!afterResult,
      });

      resultSpan.setStatus({ code: SpanStatusCode.OK });
      resultSpan.end();
      return finalResult;
    } catch (error) {
      resultSpan.recordException(error);
      resultSpan.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      resultSpan.end();
      throw error;
    }
  });
}

/**
 * Circuit breaker states
 * @enum {string}
 */
const CircuitBreakerState = {
  CLOSED: 'closed',
  OPEN: 'open',
  HALF_OPEN: 'half-open',
};

/**
 * Create a hook executor with advanced features.
 * @param {Object} [options] - Executor options
 * @param {string} [options.basePath] - Base path for file resolution
 * @param {boolean} [options.strictMode] - Enable strict error handling
 * @param {number} [options.timeoutMs] - Default timeout in milliseconds
 * @param {number} [options.totalBudgetMs] - Total timeout budget for batch execution (prevents N hooks * 30s cascade)
 * @param {boolean} [options.enableConditionEvaluation] - Enable condition evaluation
 * @param {boolean} [options.enableMetrics] - Enable execution metrics
 * @param {number} [options.circuitBreakerThreshold] - Number of failures before circuit opens
 * @param {number} [options.circuitBreakerResetMs] - Time before circuit half-opens
 * @returns {Object} Hook executor instance
 */
export function createHookExecutor(options = {}) {
  const {
    basePath = process.cwd(),
    strictMode = false,
    timeoutMs = 30000,
    totalBudgetMs = 60000, // Total budget for batch execution (prevents cascade)
    enableConditionEvaluation = true,
    enableMetrics = true,
    enableSandboxing = true,
    sandboxConfig = {},
    missingDependencyPolicy = 'warn', // 'error' | 'warn' | 'ignore'
    circuitBreakerThreshold = 5, // Open circuit after 5 consecutive failures
    circuitBreakerResetMs = 30000, // Try again after 30s
  } = options;

  const metrics = {
    totalExecutions: 0,
    successfulExecutions: 0,
    failedExecutions: 0,
    cancelledExecutions: 0,
    totalDuration: 0,
    averageDuration: 0,
    executionsByPhase: {
      before: 0,
      condition: 0,
      run: 0,
      after: 0,
      completed: 0,
    },
  };

  // Circuit breaker state for timeout cascade prevention
  const circuitBreaker = {
    state: CircuitBreakerState.CLOSED,
    failureCount: 0,
    lastFailureTime: 0,
    successCount: 0,
  };

  return {
    /**
     * Execute a hook.
     * @param {Object} hook - The hook definition
     * @param {Object} event - The hook event
     * @param {Object} [executionOptions] - Execution-specific options
     * @returns {Promise<Object>} Execution result
     */
    async execute(hook, event, executionOptions = {}) {
      const mergedOptions = {
        basePath,
        strictMode,
        timeoutMs,
        enableConditionEvaluation,
        enableSandboxing,
        sandboxConfig,
        ...executionOptions,
      };

      const result = await executeHook(hook, event, mergedOptions);

      if (enableMetrics) {
        this._updateMetrics(result);
      }

      return result;
    },

    /**
     * Execute multiple hooks in parallel with budget tracking.
     * Prevents timeout cascade where N hooks * 30s = N*30s total wait.
     * @param {Array} hooks - Array of hook definitions
     * @param {Object} event - The hook event
     * @param {Object} [executionOptions] - Execution-specific options
     * @returns {Promise<Object>} Results with succeeded/failed arrays
     */
    async executeAll(hooks, event, executionOptions = {}) {
      if (!Array.isArray(hooks)) {
        throw new TypeError('executeAll: hooks must be an array');
      }

      // Check circuit breaker before execution
      if (this._isCircuitOpen()) {
        return {
          succeeded: [],
          failed: hooks.map((hook, idx) => ({
            hookIndex: idx,
            hookName: hook?.meta?.name || `hook-${idx}`,
            error: 'Circuit breaker open - too many recent failures',
            circuitBreakerTripped: true,
          })),
          circuitBreakerOpen: true,
        };
      }

      const budgetMs = executionOptions.totalBudgetMs || totalBudgetMs;
      const perHookTimeout = Math.min(
        executionOptions.timeoutMs || timeoutMs,
        Math.floor(budgetMs / Math.max(hooks.length, 1))
      );

      const batchStartTime = Date.now();
      const succeeded = [];
      const failed = [];

      // Use Promise.allSettled to handle partial failures
      const promises = hooks.map((hook, idx) => {
        // Calculate remaining budget for this hook
        const elapsed = Date.now() - batchStartTime;
        const remainingBudget = budgetMs - elapsed;

        if (remainingBudget <= 0) {
          return Promise.resolve({
            status: 'rejected',
            reason: new Error('Budget exhausted - hook skipped to prevent cascade'),
          });
        }

        const hookTimeout = Math.min(perHookTimeout, remainingBudget);

        return this.execute(hook, event, {
          ...executionOptions,
          timeoutMs: hookTimeout,
        })
          .then(result => ({
            status: 'fulfilled',
            value: result,
            hookIndex: idx,
          }))
          .catch(error => ({
            status: 'rejected',
            reason: error,
            hookIndex: idx,
          }));
      });

      const results = await Promise.allSettled(promises);

      for (let i = 0; i < results.length; i++) {
        const result = results[i];
        const hook = hooks[i];
        const hookName = hook?.meta?.name || `hook-${i}`;

        if (result.status === 'fulfilled' && result.value?.status === 'fulfilled') {
          succeeded.push({
            hookIndex: i,
            hookName,
            result: result.value.value,
          });
          this._recordCircuitSuccess();
        } else {
          const error =
            result.status === 'rejected' ? result.reason : result.value?.reason || 'Unknown error';
          failed.push({
            hookIndex: i,
            hookName,
            error: error?.message || String(error),
          });
          this._recordCircuitFailure();
        }
      }

      return {
        succeeded,
        failed,
        totalBudgetMs: budgetMs,
        actualDurationMs: Date.now() - batchStartTime,
        circuitBreakerState: circuitBreaker.state,
      };
    },

    /**
     * Check if circuit breaker is open
     * @returns {boolean}
     * @private
     */
    _isCircuitOpen() {
      if (circuitBreaker.state === CircuitBreakerState.CLOSED) {
        return false;
      }

      if (circuitBreaker.state === CircuitBreakerState.OPEN) {
        // Check if enough time has passed to try again
        const timeSinceFailure = Date.now() - circuitBreaker.lastFailureTime;
        if (timeSinceFailure >= circuitBreakerResetMs) {
          circuitBreaker.state = CircuitBreakerState.HALF_OPEN;
          circuitBreaker.successCount = 0;
          return false;
        }
        return true;
      }

      // HALF_OPEN - allow through to test
      return false;
    },

    /**
     * Record a circuit breaker success
     * @private
     */
    _recordCircuitSuccess() {
      if (circuitBreaker.state === CircuitBreakerState.HALF_OPEN) {
        circuitBreaker.successCount++;
        // After 3 successes in half-open, close the circuit
        if (circuitBreaker.successCount >= 3) {
          circuitBreaker.state = CircuitBreakerState.CLOSED;
          circuitBreaker.failureCount = 0;
        }
      } else {
        circuitBreaker.failureCount = 0;
      }
    },

    /**
     * Record a circuit breaker failure
     * @private
     */
    _recordCircuitFailure() {
      circuitBreaker.failureCount++;
      circuitBreaker.lastFailureTime = Date.now();

      if (circuitBreaker.failureCount >= circuitBreakerThreshold) {
        circuitBreaker.state = CircuitBreakerState.OPEN;
      }
    },

    /**
     * Get circuit breaker status
     * @returns {Object}
     */
    getCircuitBreakerStatus() {
      return {
        state: circuitBreaker.state,
        failureCount: circuitBreaker.failureCount,
        lastFailureTime: circuitBreaker.lastFailureTime,
        threshold: circuitBreakerThreshold,
        resetMs: circuitBreakerResetMs,
      };
    },

    /**
     * Reset circuit breaker
     */
    resetCircuitBreaker() {
      circuitBreaker.state = CircuitBreakerState.CLOSED;
      circuitBreaker.failureCount = 0;
      circuitBreaker.lastFailureTime = 0;
      circuitBreaker.successCount = 0;
    },

    /**
     * Execute hooks sequentially.
     * @param {Array} hooks - Array of hook definitions
     * @param {Object} event - The hook event
     * @param {Object} [executionOptions] - Execution-specific options
     * @returns {Promise<Array>} Array of execution results
     */
    async executeSequential(hooks, event, executionOptions = {}) {
      if (!Array.isArray(hooks)) {
        throw new TypeError('executeSequential: hooks must be an array');
      }

      const results = [];
      for (const hook of hooks) {
        const result = await this.execute(hook, event, executionOptions);
        results.push(result);

        // Stop on first failure in strict mode
        if (executionOptions.strictMode && !result.success) {
          break;
        }
      }

      return results;
    },

    /**
     * Execute hooks with dependency resolution.
     * @param {Array} hooks - Array of hook definitions with dependencies
     * @param {Object} event - The hook event
     * @param {Object} [executionOptions] - Execution-specific options
     * @returns {Promise<Array>} Array of execution results
     */
    async executeWithDependencies(hooks, event, executionOptions = {}) {
      if (!Array.isArray(hooks)) {
        throw new TypeError('executeWithDependencies: hooks must be an array');
      }

      // Build dependency graph from hook.meta.dependencies
      const nameOf = (hook, idx) => hook?.meta?.name || `hook-${idx}`;
      const graph = new Map(); // name -> Set(dependencies)
      const byName = new Map(); // name -> hook

      for (let i = 0; i < hooks.length; i++) {
        const hook = hooks[i];
        const name = nameOf(hook, i);
        byName.set(name, hook);
        const deps = Array.isArray(hook?.meta?.dependencies) ? hook.meta.dependencies : [];
        graph.set(name, new Set(deps));
      }

      // Kahn's algorithm for topological sort with cycle tolerance
      const inDegree = new Map();
      // Initialize in-degrees
      for (const [name, deps] of graph.entries()) {
        if (!inDegree.has(name)) inDegree.set(name, 0);
        for (const dep of deps) {
          if (!graph.has(dep)) {
            const policy = executionOptions.missingDependencyPolicy || missingDependencyPolicy;
            if (policy === 'error') {
              throw new Error(
                `executeWithDependencies: missing dependency '${dep}' for hook '${name}'`
              );
            }
            if (policy === 'warn') {
              console.warn(
                `executeWithDependencies: missing dependency '${dep}' for hook '${name}' (continuing)`
              );
            }
            continue; // ignore
          }
          inDegree.set(name, (inDegree.get(name) || 0) + 1);
        }
      }

      const queue = [];
      for (const [name, deg] of inDegree.entries()) {
        if (deg === 0) queue.push(name);
      }

      const orderedNames = [];
      while (queue.length > 0) {
        const current = queue.shift();
        orderedNames.push(current);
        // Reduce in-degree of nodes that depend on current
        for (const [name, deps] of graph.entries()) {
          if (deps.has(current)) {
            const newDeg = (inDegree.get(name) || 0) - 1;
            inDegree.set(name, newDeg);
            if (newDeg === 0) queue.push(name);
          }
        }
      }

      // Detect cycles or unresolved nodes
      if (orderedNames.length < hooks.length) {
        const unresolved = hooks.map((h, i) => nameOf(h, i)).filter(n => !orderedNames.includes(n));
        const policy = executionOptions.missingDependencyPolicy || missingDependencyPolicy;
        if (policy === 'error') {
          throw new Error(
            `executeWithDependencies: cyclic or unresolved dependencies among: ${unresolved.join(', ')}`
          );
        }
        // Append unresolved in original order as a last resort
        orderedNames.push(...unresolved);
      }

      const orderedHooks = orderedNames.map(n => byName.get(n));
      return this.executeSequential(orderedHooks, event, executionOptions);
    },

    /**
     * Get execution metrics.
     * @returns {Object} Current metrics
     */
    getMetrics() {
      if (!enableMetrics) {
        return { metricsDisabled: true };
      }

      return {
        ...metrics,
        successRate:
          metrics.totalExecutions > 0 ? metrics.successfulExecutions / metrics.totalExecutions : 0,
        failureRate:
          metrics.totalExecutions > 0 ? metrics.failedExecutions / metrics.totalExecutions : 0,
        cancellationRate:
          metrics.totalExecutions > 0 ? metrics.cancelledExecutions / metrics.totalExecutions : 0,
      };
    },

    /**
     * Reset metrics.
     */
    resetMetrics() {
      if (enableMetrics) {
        Object.assign(metrics, {
          totalExecutions: 0,
          successfulExecutions: 0,
          failedExecutions: 0,
          cancelledExecutions: 0,
          totalDuration: 0,
          averageDuration: 0,
          executionsByPhase: {
            before: 0,
            condition: 0,
            run: 0,
            after: 0,
            completed: 0,
          },
        });
      }
    },

    /**
     * Update metrics with execution result.
     * @param {Object} result - Execution result
     * @private
     */
    _updateMetrics(result) {
      metrics.totalExecutions++;
      metrics.totalDuration += result.durationMs || 0;
      metrics.averageDuration = metrics.totalDuration / metrics.totalExecutions;

      if (result.success) {
        metrics.successfulExecutions++;
      } else {
        metrics.failedExecutions++;
      }

      if (result.cancelled) {
        metrics.cancelledExecutions++;
      }

      if (result.phase && metrics.executionsByPhase[result.phase] !== undefined) {
        metrics.executionsByPhase[result.phase]++;
      }
    },
  };
}

/**
 * Validate a hook definition for execution.
 * @param {Object} hook - The hook definition
 * @returns {Object} Validation result
 */
export function validateHookForExecution(hook) {
  if (!hook || typeof hook !== 'object') {
    return { valid: false, error: 'Hook must be an object' };
  }

  if (!hook.meta || !hook.meta.name) {
    return { valid: false, error: 'Hook must have meta.name' };
  }

  if (!hook.run || typeof hook.run !== 'function') {
    return { valid: false, error: 'Hook must have a run function' };
  }

  if (!hook.when) {
    return { valid: false, error: 'Hook must have a when condition' };
  }

  if (!hook.when.kind) {
    return { valid: false, error: 'Hook when condition must have a kind' };
  }

  if (!hook.when.ref) {
    return { valid: false, error: 'Hook when condition must have a ref' };
  }

  if (!hook.when.ref.uri) {
    return { valid: false, error: 'Hook when condition ref must have a uri' };
  }

  if (!hook.when.ref.sha256) {
    return {
      valid: false,
      error: 'Hook when condition ref must have a sha256 hash',
    };
  }

  return { valid: true };
}

/**
 * @file Production hook execution engine.
 * @module hook-executor
 * 
 * @description
 * Production-ready hook execution engine that evaluates conditions,
 * executes hook lifecycles, and integrates with the knowledge engine.
 */

import { createConditionEvaluator } from './condition-evaluator.mjs';
import { Store } from 'n3';

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
    enableConditionEvaluation = true
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
      executionId
    });
    
    const result = await Promise.race([executionPromise, timeoutPromise]);
    
    return {
      ...result,
      executionId,
      durationMs: Date.now() - startTime,
      success: true
    };
  } catch (error) {
    return {
      executionId,
      durationMs: Date.now() - startTime,
      success: false,
      error: error.message,
      cancelled: false
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
  const { basePath, strictMode, enableConditionEvaluation, executionId } = options;
  
  let currentEvent = { ...event };
  let beforeResult = null;
  let runResult = null;
  let afterResult = null;
  let conditionResult = null;
  let cancelled = false;
  let cancelReason = null;
  
  // Phase 1: Before
  if (hook.before) {
    try {
      beforeResult = await hook.before(currentEvent);
      
      if (beforeResult && beforeResult.cancel) {
        cancelled = true;
        cancelReason = beforeResult.reason || 'Cancelled in before phase';
        return {
          beforeResult,
          cancelled: true,
          cancelReason,
          phase: 'before'
        };
      }
      
      // Merge before result into event payload
      if (beforeResult && typeof beforeResult === 'object' && !beforeResult.cancel) {
        currentEvent = {
          ...currentEvent,
          payload: { ...currentEvent.payload, ...beforeResult }
        };
      }
    } catch (error) {
      if (strictMode) {
        throw new Error(`Before phase failed: ${error.message}`);
      }
      return {
        beforeResult: { error: error.message },
        cancelled: true,
        cancelReason: `Before phase error: ${error.message}`,
        phase: 'before'
      };
    }
  }
  
  // Phase 2: Condition Evaluation
  if (enableConditionEvaluation && hook.when) {
    try {
      const evaluator = createConditionEvaluator({ basePath, strictMode });
      conditionResult = await evaluator.evaluate(hook.when, currentEvent.context?.graph || new Store(), currentEvent.context?.env || {});
      
      // Check if condition is satisfied
      const isSatisfied = await evaluator.isSatisfied(hook.when, currentEvent.context?.graph || new Store(), currentEvent.context?.env || {});
      
      if (!isSatisfied) {
        return {
          beforeResult,
          conditionResult,
          cancelled: true,
          cancelReason: 'Condition not satisfied',
          phase: 'condition'
        };
      }
    } catch (error) {
      if (strictMode) {
        throw new Error(`Condition evaluation failed: ${error.message}`);
      }
      return {
        beforeResult,
        conditionResult: { error: error.message },
        cancelled: true,
        cancelReason: `Condition evaluation error: ${error.message}`,
        phase: 'condition'
      };
    }
  }
  
  // Phase 3: Run
  if (hook.run) {
    try {
      runResult = await hook.run(currentEvent);
    } catch (error) {
      if (strictMode) {
        throw new Error(`Run phase failed: ${error.message}`);
      }
      return {
        beforeResult,
        conditionResult,
        runResult: { error: error.message },
        cancelled: true,
        cancelReason: `Run phase error: ${error.message}`,
        phase: 'run'
      };
    }
  }
  
  // Phase 4: After
  if (hook.after) {
    try {
      afterResult = await hook.after({
        ...currentEvent,
        result: runResult,
        cancelled: false
      });
    } catch (error) {
      if (strictMode) {
        throw new Error(`After phase failed: ${error.message}`);
      }
      // After phase errors don't cancel the hook, just log them
      afterResult = { error: error.message };
    }
  }
  
  return {
    beforeResult,
    conditionResult,
    runResult,
    afterResult,
    cancelled: false,
    phase: 'completed'
  };
}

/**
 * Create a hook executor with advanced features.
 * @param {Object} [options] - Executor options
 * @param {string} [options.basePath] - Base path for file resolution
 * @param {boolean} [options.strictMode] - Enable strict error handling
 * @param {number} [options.timeoutMs] - Default timeout in milliseconds
 * @param {boolean} [options.enableConditionEvaluation] - Enable condition evaluation
 * @param {boolean} [options.enableMetrics] - Enable execution metrics
 * @returns {Object} Hook executor instance
 */
export function createHookExecutor(options = {}) {
  const {
    basePath = process.cwd(),
    strictMode = false,
    timeoutMs = 30000,
    enableConditionEvaluation = true,
    enableMetrics = true
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
      completed: 0
    }
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
        ...executionOptions
      };
      
      const result = await executeHook(hook, event, mergedOptions);
      
      if (enableMetrics) {
        this._updateMetrics(result);
      }
      
      return result;
    },
    
    /**
     * Execute multiple hooks in parallel.
     * @param {Array} hooks - Array of hook definitions
     * @param {Object} event - The hook event
     * @param {Object} [executionOptions] - Execution-specific options
     * @returns {Promise<Array>} Array of execution results
     */
    async executeAll(hooks, event, executionOptions = {}) {
      if (!Array.isArray(hooks)) {
        throw new TypeError('executeAll: hooks must be an array');
      }
      
      const promises = hooks.map(hook => this.execute(hook, event, executionOptions));
      return Promise.all(promises);
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
      
      // Simple dependency resolution - execute in order for now
      // TODO: Implement proper dependency graph resolution
      return this.executeSequential(hooks, event, executionOptions);
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
        successRate: metrics.totalExecutions > 0 ? metrics.successfulExecutions / metrics.totalExecutions : 0,
        failureRate: metrics.totalExecutions > 0 ? metrics.failedExecutions / metrics.totalExecutions : 0,
        cancellationRate: metrics.totalExecutions > 0 ? metrics.cancelledExecutions / metrics.totalExecutions : 0
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
            completed: 0
          }
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
    }
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
    return { valid: false, error: 'Hook when condition ref must have a sha256 hash' };
  }
  
  return { valid: true };
}

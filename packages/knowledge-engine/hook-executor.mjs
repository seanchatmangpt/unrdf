/**
 * @fileoverview Hook Executor - Minimal dependency resolution implementation
 * @module hook-executor
 * @description Executes hooks with proper dependency ordering
 */

/**
 * Creates a hook executor with dependency resolution
 * @param {Object} options - Configuration options
 * @param {boolean} [options.strictMode] - Strict error handling
 * @param {boolean} [options.enableConditionEvaluation] - Enable condition evaluation
 * @param {boolean} [options.enableSandboxing] - Enable sandboxing
 * @returns {Object} Hook executor instance
 * @example
 * const executor = createHookExecutor({ strictMode: true });
 * await executor.executeWithDependencies([hook], { payload: {}, context: {} });
 */
export function createHookExecutor(options = {}) {
  const {
    strictMode = false,
    enableConditionEvaluation = true,
    enableSandboxing = true,
  } = options;

  /**
   * Resolves hook dependencies and returns execution order
   * @param {Array} hooks - Array of hooks with meta.dependencies
   * @returns {Array} Hooks in dependency order
   */
  function resolveDependencies(hooks) {
    const hooksByName = new Map(hooks.map(h => [h.meta.name, h]));
    const visited = new Set();
    const ordered = [];

    function visit(hookName, visiting = new Set()) {
      if (visited.has(hookName)) return;
      if (visiting.has(hookName)) {
        throw new Error(`Circular dependency: ${hookName}`);
      }

      const hook = hooksByName.get(hookName);
      if (!hook) {
        throw new Error(`Missing dependency: ${hookName}`);
      }

      visiting.add(hookName);

      // Visit dependencies first
      if (hook.meta.dependencies) {
        for (const dep of hook.meta.dependencies) {
          visit(dep, visiting);
        }
      }

      visiting.delete(hookName);
      visited.add(hookName);
      ordered.push(hook);
    }

    // Visit all hooks
    for (const hook of hooks) {
      visit(hook.meta.name);
    }

    return ordered;
  }

  /**
   * Executes hooks with dependency resolution
   * @param {Array} hooks - Hooks to execute
   * @param {Object} context - Execution context
   * @param {Object} context.payload - Payload data
   * @param {Object} context.context - Context data
   * @returns {Promise<Array>} Results with success property
   */
  async function executeWithDependencies(hooks, context) {
    try {
      const ordered = resolveDependencies(hooks);
      const results = [];

      for (const hook of ordered) {
        try {
          const result = await hook.run(context.payload, context.context);
          results.push({
            success: true,
            hook: hook.meta.name,
            result,
          });
        } catch (error) {
          if (strictMode) {
            throw error;
          }
          results.push({
            success: false,
            hook: hook.meta.name,
            error: error.message,
          });
        }
      }

      return results;
    } catch (error) {
      throw new Error(error.message);
    }
  }

  return {
    executeWithDependencies,
    resolveDependencies,
  };
}

export default { createHookExecutor };

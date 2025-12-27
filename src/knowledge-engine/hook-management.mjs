/**
 * @file Standalone Hook Management Functions
 * @module hook-management
 *
 * @description
 * Provides standalone functions for hook management as expected by the README API.
 * These functions wrap the class methods for easier usage.
 */

import { defineHook } from "./define-hook.mjs";
import { KnowledgeHookManager } from "./knowledge-hook-manager.mjs";

// Global hook manager instance for standalone functions
let globalHookManager = null;

/**
 * Initialize the global hook manager if not already initialized
 * @param {Object} options - Options for the hook manager
 * @returns {KnowledgeHookManager} The global hook manager
 */
function getGlobalHookManager(options = {}) {
  if (!globalHookManager) {
    globalHookManager = new KnowledgeHookManager(options);
  }
  return globalHookManager;
}

/**
 * Register a knowledge hook globally
 * @param {Object} hook - The knowledge hook to register
 * @param {Object} options - Manager options (if manager needs initialization)
 * @throws {Error} If hook is invalid or already exists
 *
 * @example
 * const hook = defineHook({
 *   meta: { name: 'test-hook', description: 'Test hook' },
 *   when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
 *   run: async (event) => console.log('Hook triggered')
 * });
 *
 * await registerHook(hook);
 */
export async function registerHook(hook, options = {}) {
  const manager = getGlobalHookManager(options);

  // Validate hook if it's not already validated
  const validatedHook =
    typeof hook === "object" && !hook._validated ? defineHook(hook) : hook;

  return manager.addKnowledgeHook(validatedHook);
}

/**
 * Deregister (remove) a knowledge hook globally
 * @param {string} hookName - The name of the hook to remove
 * @returns {boolean} True if hook was removed, false if not found
 *
 * @example
 * const removed = await deregisterHook('test-hook');
 * console.log('Hook removed:', removed);
 */
export async function deregisterHook(hookName) {
  if (!globalHookManager) {
    return false;
  }

  return globalHookManager.removeKnowledgeHook(hookName);
}

/**
 * Manually evaluate a hook against given data
 * @param {Object} hook - The hook to evaluate
 * @param {Store} store - The RDF store to evaluate against
 * @param {Object} context - Additional context for evaluation
 * @returns {Object} Evaluation result
 *
 * @example
 * const result = await evaluateHook(hook, store, { timestamp: Date.now() });
 * console.log('Hook evaluation result:', result);
 */
export async function evaluateHook(hook, store, context = {}) {
  // Validate hook if needed
  const validatedHook =
    typeof hook === "object" && !hook._validated ? defineHook(hook) : hook;

  // Create a temporary manager for evaluation
  const tempManager = new KnowledgeHookManager({ enableKnowledgeHooks: true });

  try {
    // Add hook temporarily
    tempManager.addKnowledgeHook(validatedHook);

    // Evaluate the hook
    const event = {
      store,
      context,
      timestamp: Date.now(),
      hookName: validatedHook.meta.name,
    };

    // Run the hook evaluation logic
    const conditionResult = await tempManager.conditionEvaluator.isSatisfied(
      validatedHook.when,
      store,
      context,
    );

    event.result = conditionResult;
    event.satisfied = conditionResult === true;

    // Execute hook if condition is satisfied
    if (event.satisfied && validatedHook.run) {
      event.output = await validatedHook.run(event);
    }

    return event;
  } catch (error) {
    return {
      error: error.message,
      hookName: validatedHook.meta.name,
      timestamp: Date.now(),
    };
  }
}

/**
 * Get all registered hooks
 * @returns {Array} Array of registered hook names
 */
export function getRegisteredHooks() {
  if (!globalHookManager) {
    return [];
  }

  return Array.from(globalHookManager.knowledgeHooks.keys());
}

/**
 * Reset the global hook manager
 * Useful for testing or cleanup
 */
export function resetGlobalHookManager() {
  globalHookManager = null;
}

export default {
  registerHook,
  deregisterHook,
  evaluateHook,
  getRegisteredHooks,
  resetGlobalHookManager,
};


/**
 * @fileoverview KnowledgeHookManager - Class-based wrapper for hook management
 * @module @unrdf/hooks/knowledge-hook-manager
 */

import {
  createHookRegistry,
  registerHook,
  unregisterHook,
  getHook,
  listHooks,
  getHooksByTrigger,
  hasHook,
  clearHooks,
  getRegistryStats,
} from './hook-management.mjs';
import {
  executeHook,
  executeHookChain,
  executeHooksByTrigger,
  wouldPassHooks,
} from './hook-executor.mjs';
import { builtinHooks } from './builtin-hooks.mjs';
import { defineHook } from './define-hook.mjs';

/**
 * KnowledgeHookManager - Class-based interface for managing hooks
 *
 * @class
 * @example
 * const manager = new KnowledgeHookManager();
 * manager.registerHook(myHook);
 * const result = await manager.executeByTrigger('before:insert', quad, store);
 */
export class KnowledgeHookManager {
  /**
   * @private
   * @type {import('./hook-management.mjs').HookRegistry}
   */
  #registry;

  /**
   * Create a new KnowledgeHookManager
   *
   * @param {Object} [options] - Configuration options
   * @param {boolean} [options.includeBuiltins=false] - Include built-in hooks
   */
  constructor(options = {}) {
    this.#registry = createHookRegistry();

    // Register built-in hooks if requested
    if (options.includeBuiltins) {
      for (const hook of Object.values(builtinHooks)) {
        registerHook(this.#registry, hook);
      }
    }
  }

  /**
   * Define and register a hook
   *
   * @param {import('./define-hook.mjs').HookConfig} hookDef - Hook definition
   * @returns {import('./define-hook.mjs').Hook} The defined hook
   */
  define(hookDef) {
    const hook = defineHook(hookDef);
    registerHook(this.#registry, hook);
    return hook;
  }

  /**
   * Register a hook
   *
   * @param {import('./define-hook.mjs').Hook} hook - Hook to register
   * @returns {void}
   */
  registerHook(hook) {
    registerHook(this.#registry, hook);
  }

  /**
   * Unregister a hook
   *
   * @param {string} hookId - ID of hook to unregister
   * @returns {boolean} True if hook was removed
   */
  unregisterHook(hookId) {
    return unregisterHook(this.#registry, hookId);
  }

  /**
   * Get a hook by ID
   *
   * @param {string} hookId - Hook ID
   * @returns {import('./define-hook.mjs').Hook | undefined}
   */
  getHook(hookId) {
    return getHook(this.#registry, hookId);
  }

  /**
   * List all hooks
   *
   * @param {Object} [options] - Filter options
   * @param {string} [options.trigger] - Filter by trigger
   * @param {boolean} [options.enabled] - Filter by enabled status
   * @returns {import('./define-hook.mjs').Hook[]}
   */
  listHooks(options) {
    return listHooks(this.#registry, options);
  }

  /**
   * Get hooks by trigger
   *
   * @param {string} trigger - Trigger to filter by
   * @returns {import('./define-hook.mjs').Hook[]}
   */
  getHooksByTrigger(trigger) {
    return getHooksByTrigger(this.#registry, trigger);
  }

  /**
   * Check if hook exists
   *
   * @param {string} hookId - Hook ID
   * @returns {boolean}
   */
  hasHook(hookId) {
    return hasHook(this.#registry, hookId);
  }

  /**
   * Clear all hooks
   *
   * @returns {void}
   */
  clearHooks() {
    clearHooks(this.#registry);
  }

  /**
   * Get registry statistics
   *
   * @returns {{ total: number, enabled: number, disabled: number, byTrigger: Record<string, number> }}
   */
  getStats() {
    return getRegistryStats(this.#registry);
  }

  /**
   * Execute a specific hook
   *
   * @param {string} hookId - Hook ID
   * @param {*} data - Data to process
   * @param {*} context - Execution context
   * @returns {Promise<import('./hook-executor.mjs').HookResult>}
   */
  async executeHook(hookId, data, context) {
    const hook = this.getHook(hookId);
    if (!hook) {
      throw new Error(`Hook not found: ${hookId}`);
    }
    return executeHook(hook, data, context);
  }

  /**
   * Execute hooks in chain
   *
   * @param {import('./define-hook.mjs').Hook[]} hooks - Hooks to execute
   * @param {*} data - Initial data
   * @param {*} context - Execution context
   * @returns {Promise<import('./hook-executor.mjs').ChainResult>}
   */
  async executeChain(hooks, data, context) {
    return executeHookChain(hooks, data, context);
  }

  /**
   * Execute hooks by trigger
   *
   * @param {string} trigger - Trigger to execute
   * @param {*} data - Data to process
   * @param {*} context - Execution context
   * @returns {Promise<import('./hook-executor.mjs').ChainResult>}
   */
  async executeByTrigger(trigger, data, context) {
    const hooks = this.listHooks();
    return executeHooksByTrigger(hooks, trigger, data, context);
  }

  /**
   * Check if data would pass hooks
   *
   * @param {string} trigger - Trigger to check
   * @param {*} data - Data to validate
   * @param {*} context - Execution context
   * @returns {Promise<boolean>}
   */
  async wouldPass(trigger, data, context) {
    const hooks = this.getHooksByTrigger(trigger);
    return wouldPassHooks(hooks, data, context);
  }

  /**
   * Get built-in hooks
   *
   * @returns {import('./define-hook.mjs').Hook[]}
   */
  static getBuiltinHooks() {
    return Object.values(builtinHooks);
  }
}

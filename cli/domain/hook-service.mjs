/**
 * @file Hook Domain Service - Business logic layer for knowledge hooks operations
 * @module cli/domain/hook-service
 *
 * ARCHITECTURE:
 * - CLI Commands (presentation) → Domain Services (business logic) → Packages (data access)
 * - This service encapsulates ALL business logic for hook operations
 * - Commands should be thin wrappers that validate args and call this service
 * - Packages (@unrdf/hooks) should only do data access
 */

import { KnowledgeHookManager } from '@unrdf/hooks';
import { z } from 'zod';

/* ========================================================================= */
/* Schemas - Domain-level validation                                        */
/* ========================================================================= */

const ListHooksOptionsSchema = z.object({
  trigger: z.string().optional(),
  enabled: z.boolean().optional(),
  policy: z.string().optional(),
});

const RegisterHookOptionsSchema = z.object({
  id: z.string().min(1, 'Hook ID is required'),
  name: z.string().min(1, 'Hook name is required'),
  description: z.string().optional(),
  version: z.string().optional().default('1.0.0'),
  trigger: z.string().min(1, 'Trigger is required'),
  enabled: z.boolean().optional().default(true),
  validate: z.function().optional(),
});

const ExecuteHookOptionsSchema = z.object({
  hookId: z.string().min(1, 'Hook ID is required'),
  data: z.any(),
  context: z.any().optional(),
});

const ExecuteByTriggerOptionsSchema = z.object({
  trigger: z.string().min(1, 'Trigger is required'),
  data: z.any(),
  context: z.any().optional(),
});

/* ========================================================================= */
/* Domain Service Class                                                     */
/* ========================================================================= */

/**
 * HookService - Domain logic for knowledge hooks operations
 *
 * Responsibilities:
 * - Validate business rules for hooks
 * - Orchestrate hook manager operations
 * - Transform hook data for presentation layer
 * - Handle errors with domain-specific messages
 *
 * Does NOT:
 * - Parse CLI arguments (that's the command's job)
 * - Format output for display (that's the command's job)
 * - Directly manipulate hook registry (delegates to package)
 */
export class HookService {
  #manager;

  constructor() {
    // Initialize hook manager (can include builtins if needed)
    this.#manager = new KnowledgeHookManager({
      includeBuiltins: false,
      maxExecutionDepth: 3
    });
  }

  /**
   * List all registered hooks
   *
   * @param {Object} [options] - Filter options
   * @param {string} [options.trigger] - Filter by trigger type
   * @param {boolean} [options.enabled] - Filter by enabled status
   * @param {string} [options.policy] - Filter by policy pack
   * @returns {Object} Hook list with metadata
   *
   * @example
   * const service = new HookService();
   * const result = await service.listHooks({ trigger: 'before-add' });
   * // result = { hooks: [...], metadata: { totalCount, enabledCount } }
   */
  async listHooks(options = {}) {
    const validated = ListHooksOptionsSchema.parse(options);

    try {
      let hooks = this.#manager.listHooks();

      // Apply filters at domain layer
      if (validated.trigger) {
        hooks = hooks.filter(h => h.trigger === validated.trigger);
      }

      if (validated.enabled !== undefined) {
        hooks = hooks.filter(h => h.enabled === validated.enabled);
      }

      if (validated.policy) {
        hooks = hooks.filter(h => h.meta?.policy === validated.policy);
      }

      // Transform for presentation
      const transformed = hooks.map(hook => ({
        id: hook.id || hook.name,
        name: hook.name,
        description: hook.description || 'No description',
        version: hook.version || '1.0.0',
        trigger: hook.trigger,
        enabled: hook.enabled ?? true,
        policy: hook.meta?.policy || 'default'
      }));

      return {
        hooks: transformed,
        metadata: {
          totalCount: transformed.length,
          enabledCount: transformed.filter(h => h.enabled).length,
          disabledCount: transformed.filter(h => !h.enabled).length,
          filters: validated
        }
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Failed to list hooks');
    }
  }

  /**
   * Get single hook by ID
   *
   * @param {string} hookId - Hook ID
   * @returns {Object|null} Hook details or null if not found
   *
   * @example
   * const service = new HookService();
   * const hook = await service.getHook('validate-iri');
   * // hook = { id, name, description, trigger, ... }
   */
  async getHook(hookId) {
    try {
      const hook = this.#manager.getHook(hookId);

      if (!hook) {
        return null;
      }

      return {
        id: hook.id || hook.name,
        name: hook.name,
        description: hook.description || 'No description',
        version: hook.version || '1.0.0',
        trigger: hook.trigger,
        enabled: hook.enabled ?? true,
        policy: hook.meta?.policy || 'default'
      };
    } catch (error) {
      throw this.#enhanceError(error, `Failed to get hook: ${hookId}`);
    }
  }

  /**
   * Register a new hook
   *
   * @param {Object} hookDef - Hook definition
   * @returns {Object} Registration result
   *
   * @example
   * const service = new HookService();
   * const result = await service.registerHook({
   *   id: 'my-hook',
   *   name: 'my-hook',
   *   trigger: 'before-add',
   *   validate: (quad) => ({ valid: true })
   * });
   * // result = { success: true, hookId: 'my-hook' }
   */
  async registerHook(hookDef) {
    const validated = RegisterHookOptionsSchema.parse(hookDef);

    try {
      // Check if hook already exists
      if (this.#manager.hasHook(validated.id)) {
        throw new Error(`Hook already registered: ${validated.id}`);
      }

      // Register via manager
      this.#manager.registerHook(validated);

      return {
        success: true,
        hookId: validated.id,
        name: validated.name,
        trigger: validated.trigger
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Failed to register hook');
    }
  }

  /**
   * Unregister a hook
   *
   * @param {string} hookId - Hook ID to remove
   * @returns {Object} Unregister result
   *
   * @example
   * const service = new HookService();
   * const result = await service.unregisterHook('my-hook');
   * // result = { success: true, removed: true }
   */
  async unregisterHook(hookId) {
    try {
      const removed = this.#manager.unregisterHook(hookId);

      return {
        success: true,
        removed,
        hookId
      };
    } catch (error) {
      throw this.#enhanceError(error, `Failed to unregister hook: ${hookId}`);
    }
  }

  /**
   * Execute a specific hook
   *
   * @param {Object} options - Execution options
   * @param {string} options.hookId - Hook ID to execute
   * @param {*} options.data - Data to process
   * @param {*} [options.context] - Execution context
   * @returns {Object} Execution result
   *
   * @example
   * const service = new HookService();
   * const result = await service.executeHook({
   *   hookId: 'validate-iri',
   *   data: myQuad,
   *   context: { store }
   * });
   * // result = { valid: true, data: transformedQuad, errors: [] }
   */
  async executeHook(options) {
    const validated = ExecuteHookOptionsSchema.parse(options);

    try {
      const result = await this.#manager.executeHook(
        validated.hookId,
        validated.data,
        validated.context
      );

      return {
        ...result,
        hookId: validated.hookId,
        executedAt: new Date().toISOString()
      };
    } catch (error) {
      throw this.#enhanceError(error, `Failed to execute hook: ${validated.hookId}`);
    }
  }

  /**
   * Execute all hooks for a trigger
   *
   * @param {Object} options - Execution options
   * @param {string} options.trigger - Trigger type
   * @param {*} options.data - Data to process
   * @param {*} [options.context] - Execution context
   * @returns {Object} Execution result
   *
   * @example
   * const service = new HookService();
   * const result = await service.executeByTrigger({
   *   trigger: 'before-add',
   *   data: myQuad,
   *   context: { store }
   * });
   * // result = { valid: true, data: transformedQuad, hooksExecuted: 3 }
   */
  async executeByTrigger(options) {
    const validated = ExecuteByTriggerOptionsSchema.parse(options);

    try {
      const result = await this.#manager.executeByTrigger(
        validated.trigger,
        validated.data,
        validated.context
      );

      // Get hooks that were executed
      const hooks = this.#manager.getHooksByTrigger(validated.trigger);

      return {
        ...result,
        trigger: validated.trigger,
        hooksExecuted: hooks.length,
        executedAt: new Date().toISOString()
      };
    } catch (error) {
      throw this.#enhanceError(error, `Failed to execute hooks for trigger: ${validated.trigger}`);
    }
  }

  /**
   * Get hook statistics
   *
   * @returns {Object} Hook statistics
   *
   * @example
   * const service = new HookService();
   * const stats = await service.getStats();
   * // stats = { totalHooks: 10, byTrigger: {...}, executionDepth: 0 }
   */
  async getStats() {
    try {
      const stats = this.#manager.getStats();

      return {
        totalHooks: stats.totalHooks || 0,
        byTrigger: stats.byTrigger || {},
        executionDepth: this.#manager.getExecutionDepth(),
        isExecuting: this.#manager.isExecuting()
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Failed to get hook statistics');
    }
  }

  /**
   * Clear all hooks
   *
   * @returns {Object} Clear result
   *
   * @example
   * const service = new HookService();
   * const result = await service.clearAll();
   * // result = { success: true, cleared: 10 }
   */
  async clearAll() {
    try {
      const before = this.#manager.listHooks().length;
      this.#manager.clearHooks();

      return {
        success: true,
        cleared: before
      };
    } catch (error) {
      throw this.#enhanceError(error, 'Failed to clear hooks');
    }
  }

  /**
   * Check if data would pass hooks (dry-run)
   *
   * @param {string} trigger - Trigger to check
   * @param {*} data - Data to validate
   * @param {*} [context] - Execution context
   * @returns {Object} Validation result
   *
   * @example
   * const service = new HookService();
   * const result = await service.wouldPass('before-add', myQuad);
   * // result = { wouldPass: true, trigger: 'before-add' }
   */
  async wouldPass(trigger, data, context) {
    try {
      const wouldPass = await this.#manager.wouldPass(trigger, data, context);

      return {
        wouldPass,
        trigger
      };
    } catch (error) {
      throw this.#enhanceError(error, `Failed to check if data would pass hooks for trigger: ${trigger}`);
    }
  }

  /* ========================================================================= */
  /* Private Helper Methods                                                    */
  /* ========================================================================= */

  /**
   * Enhance error with domain context
   * @private
   */
  #enhanceError(error, context) {
    const enhanced = new Error(`${context}: ${error.message}`);
    enhanced.cause = error;
    enhanced.domain = 'HookService';
    return enhanced;
  }
}

/* ========================================================================= */
/* Singleton Instance                                                        */
/* ========================================================================= */

let hookServiceInstance = null;

/**
 * Get singleton HookService instance
 *
 * @returns {HookService}
 *
 * @example
 * import { getHookService } from './domain/hook-service.mjs';
 * const service = getHookService();
 * const result = await service.listHooks({ trigger: 'before-add' });
 */
export function getHookService() {
  if (!hookServiceInstance) {
    hookServiceInstance = new HookService();
  }
  return hookServiceInstance;
}

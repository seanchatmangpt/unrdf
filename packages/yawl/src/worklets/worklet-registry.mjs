/**
 * @file Worklet Registry - Handler storage with RDR selection
 * @module @unrdf/yawl/worklets/registry
 *
 * @description
 * Implements Ripple Down Rules (RDR) for worklet handler selection:
 * - Priority-based handler registration
 * - Condition evaluation for context-aware selection
 * - Most specific handler wins (highest priority matching)
 * - Default handler fallback
 *
 * Based on: Java YAWL WorkletRepository + RDR algorithm
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Worklet handler condition schema
 * Conditions can be:
 * - String expression: "amount > 10000"
 * - Function: (context) => context.amount > 10000
 * - Boolean: true (always match)
 */
export const HandlerConditionSchema = z.union([
  z.string().min(1),
  z.function().args(z.record(z.string(), z.any())).returns(z.boolean()),
  z.boolean(),
]);

/**
 * Worklet handler specification
 */
export const WorkletHandlerSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  exceptionType: z.string().min(1),
  priority: z.number().int().nonnegative(),
  condition: HandlerConditionSchema,
  workflow: z.union([
    z.function(),                         // Function handler
    z.object({                            // Workflow specification
      id: z.string().min(1),
      tasks: z.array(z.any()),
    }),
  ]),
  description: z.string().optional(),
  metadata: z.record(z.string(), z.any()).optional(),
});

/**
 * @typedef {z.infer<typeof WorkletHandlerSchema>} WorkletHandler
 */

// ============================================================================
// WORKLET REGISTRY
// ============================================================================

/**
 * Worklet Registry - Stores and selects exception handlers
 *
 * Implements Ripple Down Rules (RDR):
 * - Handlers registered with priority (lower = higher priority)
 * - Conditions evaluated in priority order
 * - First matching handler selected (most specific wins)
 * - Default handler (priority: Infinity) as fallback
 *
 * @example
 * const registry = new WorkletRegistry();
 *
 * // Register handlers (priority 0 = highest)
 * registry.registerHandler({
 *   id: 'vp-approval',
 *   exceptionType: 'timeout',
 *   priority: 10,
 *   condition: 'amount > 10000',
 *   workflow: escalateToVP
 * });
 *
 * registry.registerHandler({
 *   id: 'supervisor-approval',
 *   exceptionType: 'timeout',
 *   priority: 20,
 *   condition: 'amount > 1000',
 *   workflow: escalateToSupervisor
 * });
 *
 * // Select handler
 * const handler = registry.selectHandler('timeout', { amount: 5000 });
 * // Returns: supervisor-approval (priority 20, amount 1000-10000)
 */
export class WorkletRegistry {
  /**
   * Create worklet registry
   */
  constructor() {
    /** @type {Map<string, WorkletHandler>} */
    this.handlers = new Map();

    /** @type {Map<string, WorkletHandler[]>} */
    this.handlersByException = new Map();
  }

  /**
   * Register worklet handler
   *
   * @param {Object} spec - Handler specification
   * @param {string} spec.id - Unique handler ID
   * @param {string} spec.name - Human-readable name
   * @param {string} spec.exceptionType - Exception type to handle
   * @param {number} spec.priority - Priority (lower = higher priority, 0 = highest)
   * @param {string|Function|boolean} spec.condition - Match condition
   * @param {Function|Object} spec.workflow - Handler workflow or function
   * @param {string} [spec.description] - Handler description
   * @param {Object} [spec.metadata] - Additional metadata
   * @returns {WorkletHandler} Registered handler
   * @throws {Error} If validation fails
   *
   * @example
   * registry.registerHandler({
   *   id: 'timeout-high-value',
   *   name: 'High Value Timeout Escalation',
   *   exceptionType: 'timeout',
   *   priority: 10,
   *   condition: (ctx) => ctx.amount > 10000,
   *   workflow: async (context) => ({
   *     success: true,
   *     action: 'resume',
   *     outputData: { approver: 'vp' }
   *   })
   * });
   */
  registerHandler(spec) {
    const handler = WorkletHandlerSchema.parse(spec);

    // Check for duplicate ID
    if (this.handlers.has(handler.id)) {
      throw new Error(`Handler with ID ${handler.id} already registered`);
    }

    // Store handler
    this.handlers.set(handler.id, handler);

    // Index by exception type
    if (!this.handlersByException.has(handler.exceptionType)) {
      this.handlersByException.set(handler.exceptionType, []);
    }
    this.handlersByException.get(handler.exceptionType).push(handler);

    // Sort by priority (lower = higher priority)
    this.handlersByException.get(handler.exceptionType)
      .sort((a, b) => a.priority - b.priority);

    return handler;
  }

  /**
   * Unregister worklet handler
   * @param {string} handlerId - Handler ID to remove
   * @returns {boolean} True if removed, false if not found
   */
  unregisterHandler(handlerId) {
    const handler = this.handlers.get(handlerId);

    if (!handler) {
      return false;
    }

    this.handlers.delete(handlerId);

    // Remove from exception index
    const exceptionHandlers = this.handlersByException.get(handler.exceptionType);
    if (exceptionHandlers) {
      const index = exceptionHandlers.findIndex(h => h.id === handlerId);
      if (index !== -1) {
        exceptionHandlers.splice(index, 1);
      }
    }

    return true;
  }

  /**
   * Select worklet handler via RDR (Ripple Down Rules)
   *
   * Selection algorithm:
   * 1. Get all handlers for exception type
   * 2. Sort by priority (already sorted)
   * 3. Evaluate conditions in priority order
   * 4. Return first matching handler (most specific)
   * 5. Return null if no match
   *
   * @param {string} exceptionType - Exception type
   * @param {Object} context - Exception context for condition evaluation
   * @returns {WorkletHandler|null} Selected handler or null
   *
   * @example
   * // Amount > 10K → vp-approval (priority 10)
   * registry.selectHandler('timeout', { amount: 15000 });
   *
   * // Amount 1K-10K → supervisor-approval (priority 20)
   * registry.selectHandler('timeout', { amount: 5000 });
   *
   * // Amount < 1K → auto-approve (priority 30)
   * registry.selectHandler('timeout', { amount: 500 });
   */
  selectHandler(exceptionType, context) {
    const handlers = this.handlersByException.get(exceptionType);

    if (!handlers || handlers.length === 0) {
      return null;
    }

    // Evaluate handlers in priority order
    for (const handler of handlers) {
      if (this._evaluateCondition(handler.condition, context)) {
        return handler;
      }
    }

    return null;
  }

  /**
   * Evaluate handler condition
   * @private
   */
  _evaluateCondition(condition, context) {
    // Boolean condition
    if (typeof condition === 'boolean') {
      return condition;
    }

    // Function condition
    if (typeof condition === 'function') {
      try {
        return condition(context);
      } catch {
        return false;
      }
    }

    // String expression condition
    if (typeof condition === 'string') {
      try {
        // Create safe evaluation function
        // Whitelist: comparison operators, logical operators, numbers, identifiers
        const safeExpression = condition
          .replace(/[^a-zA-Z0-9_.<>=!&|() ]/g, '') // Remove unsafe chars
          .trim();

        if (!safeExpression) {
          return false;
        }

        // Build context accessor
        const contextKeys = Object.keys(context);
        const contextValues = contextKeys.map(k => context[k]);

        // Evaluate expression with context
        const evaluator = new Function(...contextKeys, `return ${safeExpression};`);
        return Boolean(evaluator(...contextValues));
      } catch {
        return false;
      }
    }

    return false;
  }

  /**
   * Get handler by ID
   * @param {string} handlerId - Handler ID
   * @returns {WorkletHandler|null} Handler or null if not found
   */
  getHandler(handlerId) {
    return this.handlers.get(handlerId) || null;
  }

  /**
   * Get all handlers for exception type
   * @param {string} exceptionType - Exception type
   * @returns {WorkletHandler[]} Array of handlers (sorted by priority)
   */
  getHandlersForException(exceptionType) {
    return this.handlersByException.get(exceptionType) || [];
  }

  /**
   * Get all registered handlers
   * @returns {WorkletHandler[]} Array of all handlers
   */
  getAllHandlers() {
    return Array.from(this.handlers.values());
  }

  /**
   * Clear all handlers
   */
  clear() {
    this.handlers.clear();
    this.handlersByException.clear();
  }

  /**
   * Get registry statistics
   * @returns {Object} Registry statistics
   */
  getStats() {
    const exceptionTypes = Array.from(this.handlersByException.keys());
    const handlerCounts = exceptionTypes.map(type => ({
      exceptionType: type,
      count: this.handlersByException.get(type).length,
    }));

    return {
      totalHandlers: this.handlers.size,
      exceptionTypes: exceptionTypes.length,
      handlersByException: handlerCounts,
    };
  }
}

/**
 * Create worklet registry
 * @returns {WorkletRegistry} Worklet registry instance
 */
export function createWorkletRegistry() {
  return new WorkletRegistry();
}

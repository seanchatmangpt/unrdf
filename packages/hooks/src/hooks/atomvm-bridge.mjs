/**
 * @file AtomVM/Erlang Bridge for Knowledge Hooks
 * @module hooks/atomvm-bridge
 *
 * @description
 * Bridge for executing knowledge hooks from Erlang/BEAM processes via AtomVM.
 * Enables hooks to be registered, evaluated, and executed from distributed systems.
 *
 * Usage from Erlang:
 * ```erlang
 * % Via HTTP gateway:
 * {ok, HookId} = atomvm_bridge:register_hook(#{
 *   <<"name">> => <<"compliance">>,
 *   <<"condition">> => #{<<"kind">> => <<"sparql-ask">>}
 * }),
 * {ok, Result} = atomvm_bridge:evaluate_condition(Condition).
 * ```
 */

import { evaluateCondition, KnowledgeHookEngine } from './index.mjs';
import { validateKnowledgeHook } from './schemas.mjs';

/**
 * HooksBridge - AtomVM/Erlang integration for Knowledge Hooks
 *
 * Provides a stateful API for managing hooks across distributed systems.
 * Each bridge instance maintains its own registry and execution context.
 *
 * @class HooksBridge
 */
export class HooksBridge {
  /**
   * Create a new HooksBridge instance.
   *
   * @param {Object} store - RDF store (oxigraph)
   * @param {Object} [options] - Bridge options
   * @param {boolean} [options.persistReceipts=true] - Persist receipt chain
   * @param {number} [options.maxHooks=1000] - Max hooks in registry
   * @param {number} [options.maxReceiptHistory=10000] - Max receipts to keep
   */
  constructor(store, options = {}) {
    this.store = store;
    this.options = {
      persistReceipts: options.persistReceipts ?? true,
      maxHooks: options.maxHooks ?? 1000,
      maxReceiptHistory: options.maxReceiptHistory ?? 10000,
    };

    this.hookRegistry = new Map(); // hookId -> hook definition
    this.receiptChain = []; // Array of receipts in order
    this.engine = new KnowledgeHookEngine(store);
    this.nextHookId = 1;
  }

  /**
   * Register a hook definition (typically from Erlang).
   *
   * @param {Object} hookDef - Hook definition
   * @param {string} hookDef.name - Hook name
   * @param {Object} hookDef.condition - Condition (9 kinds)
   * @param {Array} [hookDef.effects] - Effects
   * @param {Object} [hookDef.metadata] - Custom metadata
   * @returns {Promise<string>} Hook ID
   *
   * @throws {Error} If hook definition invalid or registry full
   *
   * @example
   * const hookId = await bridge.registerHook({
   *   name: 'erlang-check',
   *   condition: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
   *   effects: [{ kind: 'sparql-construct', query: '...' }]
   * });
   */
  async registerHook(hookDef) {
    // Validate hook definition
    const validation = validateKnowledgeHook(hookDef);
    if (!validation.success) {
      throw new Error(`Invalid hook definition: ${validation.error.message}`);
    }

    // Check registry size
    if (this.hookRegistry.size >= this.options.maxHooks) {
      throw new Error(`Hook registry full (max ${this.options.maxHooks})`);
    }

    // Assign ID and store
    const hookId = String(this.nextHookId++);
    this.hookRegistry.set(hookId, hookDef);

    return hookId;
  }

  /**
   * Get a registered hook by ID.
   *
   * @param {string} hookId - Hook ID from registerHook()
   * @returns {Object|null} Hook definition or null if not found
   */
  getHook(hookId) {
    return this.hookRegistry.get(hookId) ?? null;
  }

  /**
   * Unregister a hook by ID.
   *
   * @param {string} hookId - Hook ID to remove
   * @returns {boolean} True if hook was removed, false if not found
   */
  unregisterHook(hookId) {
    return this.hookRegistry.delete(hookId);
  }

  /**
   * List all registered hooks.
   *
   * @returns {Array<{id: string, name: string}>} Array of hook metadata
   */
  listHooks() {
    return Array.from(this.hookRegistry.entries()).map(([id, def]) => ({
      id,
      name: def.name,
    }));
  }

  /**
   * Evaluate a condition against the store (typically from Erlang).
   *
   * @param {Object} condition - Condition definition (9 kinds)
   * @param {string} [condition.kind] - Condition kind (sparql-ask, shacl, etc.)
   * @returns {Promise<boolean>} Condition evaluation result
   *
   * @throws {Error} If condition invalid or evaluation fails
   *
   * @example
   * // SPARQL ASK condition
   * const result = await bridge.evaluateCondition({
   *   kind: 'sparql-ask',
   *   query: 'ASK { ?s a ex:Person }'
   * });
   *
   * // Datalog condition
   * const result = await bridge.evaluateCondition({
   *   kind: 'datalog',
   *   facts: ['user(alice)', 'admin(alice)'],
   *   rules: ['allowed(X) :- admin(X)'],
   *   goal: 'allowed(alice)'
   * });
   */
  async evaluateCondition(condition) {
    if (!condition || typeof condition !== 'object') {
      throw new Error('Condition must be a valid object');
    }

    if (!condition.kind) {
      throw new Error('Condition must have a "kind" property (9 kinds supported)');
    }

    return evaluateCondition(condition, this.store);
  }

  /**
   * Execute hooks with receipt chaining (typically from Erlang).
   *
   * @param {Object} context - Execution context
   * @param {string} context.nodeId - Application identifier
   * @param {bigint} context.t_ns - Timestamp in nanoseconds
   * @param {string} [context.previousReceiptHash] - Link to prior operation
   * @param {Array<string>} hookIds - Hook IDs to execute (from registerHook)
   * @returns {Promise<Object>} Execution result with receipt
   *
   * @throws {Error} If hooks not found or execution fails
   *
   * @example
   * const result = await bridge.executeHooks(
   *   {
   *     nodeId: 'erlang-app',
   *     t_ns: BigInt(Date.now() * 1000000),
   *     previousReceiptHash: 'prior-hash'
   *   },
   *   ['hook1', 'hook2']
   * );
   *
   * console.log('Receipt:', result.receipt.receiptHash);
   * console.log('Delta:', result.receipt.delta.adds.length, 'additions');
   */
  async executeHooks(context, hookIds) {
    if (!context || !context.nodeId) {
      throw new Error('Context must have nodeId property');
    }

    if (!Array.isArray(hookIds)) {
      throw new Error('hookIds must be an array');
    }

    // Resolve hook definitions
    const hooks = [];
    for (const hookId of hookIds) {
      const hook = this.getHook(hookId);
      if (!hook) {
        throw new Error(`Hook not found: ${hookId}`);
      }
      hooks.push(hook);
    }

    // Execute via engine (provides receipt chaining)
    const result = await this.engine.execute(context, hooks);

    // Persist receipt if enabled
    if (this.options.persistReceipts && result.receipt) {
      this.receiptChain.push(result.receipt);

      // Prune old receipts if necessary
      if (this.receiptChain.length > this.options.maxReceiptHistory) {
        this.receiptChain = this.receiptChain.slice(-this.options.maxReceiptHistory);
      }
    }

    return result;
  }

  /**
   * Get entire receipt chain (for audit trail).
   *
   * @returns {Array<Object>} Receipts in execution order
   *
   * @example
   * const chain = bridge.getReceiptChain();
   * chain.forEach((r, i) => {
   *   console.log(`Step ${i}: ${r.receiptHash}`);
   *   console.log(`  Previous: ${r.previousReceiptHash}`);
   *   console.log(`  Delta: +${r.delta.adds.length} -${r.delta.deletes.length}`);
   * });
   */
  getReceiptChain() {
    return [...this.receiptChain];
  }

  /**
   * Verify receipt chain integrity (all previousReceiptHash links are valid).
   *
   * @returns {Object} Verification result
   *
   * @example
   * const result = bridge.verifyReceiptChain();
   * if (result.valid) {
   *   console.log('✓ Chain verified: 42 receipts, unbroken links');
   * } else {
   *   console.log('✗ Chain broken at step:', result.brokenAt);
   * }
   */
  verifyReceiptChain() {
    if (this.receiptChain.length === 0) {
      return { valid: true, length: 0 };
    }

    for (let i = 1; i < this.receiptChain.length; i++) {
      const current = this.receiptChain[i];
      const previous = this.receiptChain[i - 1];

      if (current.previousReceiptHash !== previous.receiptHash) {
        return {
          valid: false,
          brokenAt: i,
          expected: previous.receiptHash,
          got: current.previousReceiptHash,
        };
      }
    }

    return {
      valid: true,
      length: this.receiptChain.length,
    };
  }

  /**
   * Clear receipt history (does not affect registered hooks).
   *
   * @returns {number} Number of receipts cleared
   */
  clearReceiptChain() {
    const count = this.receiptChain.length;
    this.receiptChain = [];
    return count;
  }

  /**
   * Get bridge statistics.
   *
   * @returns {Object} Stats object
   *
   * @example
   * const stats = bridge.getStats();
   * console.log('Hooks registered:', stats.hookCount);
   * console.log('Receipt chain length:', stats.receiptCount);
   * console.log('Memory usage:', stats.memoryBytes);
   */
  getStats() {
    return {
      hookCount: this.hookRegistry.size,
      nextHookId: this.nextHookId,
      receiptCount: this.receiptChain.length,
      maxHooks: this.options.maxHooks,
      maxReceiptHistory: this.options.maxReceiptHistory,
      memoryBytes: JSON.stringify({
        hooks: Array.from(this.hookRegistry.values()),
        receipts: this.receiptChain,
      }).length,
    };
  }
}

/**
 * Create a HooksBridge instance.
 *
 * @param {Object} store - RDF store
 * @param {Object} [options] - Bridge options
 * @returns {HooksBridge} New bridge instance
 *
 * @example
 * import { createHooksBridge } from '@unrdf/hooks/atomvm';
 * import { createStore } from '@unrdf/oxigraph';
 *
 * const store = createStore();
 * const bridge = createHooksBridge(store);
 *
 * const hookId = await bridge.registerHook({...});
 * const result = await bridge.executeHooks({...}, [hookId]);
 */
export function createHooksBridge(store, options) {
  return new HooksBridge(store, options);
}

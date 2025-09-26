/**
 * @file Transaction manager with hooks and receipts.
 * @module transaction
 */

import { Store } from 'n3';
import { sha3_256 } from '@noble/hashes/sha3.js';
import { blake3 } from '@noble/hashes/blake3.js';
import { utf8ToBytes, bytesToHex } from '@noble/hashes/utils.js';
import { canonicalize } from './canonicalize.mjs';

/**
 * @typedef {object} Delta
 * @property {import('n3').Quad[]} additions - Quads to add
 * @property {import('n3').Quad[]} removals - Quads to remove
 */

/**
 * @typedef {object} Hook
 * @property {string} id - Unique hook identifier
 * @property {'pre'|'post'} mode - Hook execution mode
 * @property {(store: Store, delta: Delta) => Promise<boolean>} condition - Condition function
 * @property {'veto'|((store: Store, delta: Delta) => Promise<void>)} effect - Effect function or 'veto'
 */

/**
 * @typedef {object} HookResult
 * @property {string} hookId - Hook identifier
 * @property {'pre'|'post'} mode - Hook mode
 * @property {boolean} result - Condition result
 * @property {string} [error] - Error message if hook failed
 */

/**
 * @typedef {object} Receipt
 * @property {Delta} delta - Applied delta
 * @property {boolean} committed - Whether transaction was committed
 * @property {HookResult[]} hookResults - Results from all hooks
 * @property {{ sha3: string, blake3: string }} beforeHash - Store hash before transaction
 * @property {{ sha3: string, blake3: string }} afterHash - Store hash after transaction
 * @property {number} timestamp - Transaction timestamp
 * @property {string} [error] - Error message if transaction failed
 */

/**
 * Hash a store canonically with SHA-3 and BLAKE3.
 * @param {Store} store - The store to hash
 * @returns {Promise<{ sha3: string, blake3: string }>} Promise resolving to hash object
 * 
 * @throws {Error} If hashing fails
 */
async function hashStore(store) {
  try {
    const c14n = await canonicalize(store);
    const bytes = utf8ToBytes(c14n);
    return {
      sha3: bytesToHex(sha3_256(bytes)),
      blake3: bytesToHex(blake3(bytes)),
    };
  } catch (error) {
    throw new Error(`Store hashing failed: ${error.message}`);
  }
}

/**
 * Transaction manager with hooks and receipts.
 * Provides atomic transactions with pre/post hooks and comprehensive receipts.
 */
export class TransactionManager {
  /**
   * Create a new transaction manager.
   * @param {Object} [options] - Manager options
   * @param {boolean} [options.strictMode=false] - Enable strict validation mode
   * @param {number} [options.maxHooks=100] - Maximum number of hooks allowed
   */
  constructor(options = {}) {
    /** @type {Hook[]} */
    this.hooks = [];
    this.options = {
      strictMode: false,
      maxHooks: 100,
      ...options
    };
  }

  /**
   * Register a hook.
   * @param {Hook} hook - Hook to register
   * @throws {Error} If hook is invalid or limit exceeded
   * 
   * @example
   * tx.addHook({
   *   id: "no-eve",
   *   mode: "pre",
   *   condition: async (store, delta) => !delta.additions.some(q => q.object.value.endsWith("eve")),
   *   effect: "veto"
   * });
   */
  addHook(hook) {
    if (!hook || typeof hook !== 'object') {
      throw new TypeError('addHook: hook must be an object');
    }
    if (!hook.id || typeof hook.id !== 'string') {
      throw new TypeError('addHook: hook must have a string id');
    }
    if (!['pre', 'post'].includes(hook.mode)) {
      throw new TypeError('addHook: hook mode must be "pre" or "post"');
    }
    if (typeof hook.condition !== 'function') {
      throw new TypeError('addHook: hook condition must be a function');
    }
    if (hook.effect !== 'veto' && typeof hook.effect !== 'function') {
      throw new TypeError('addHook: hook effect must be "veto" or a function');
    }

    // Check for duplicate IDs
    if (this.hooks.some(h => h.id === hook.id)) {
      throw new Error(`Hook with id "${hook.id}" already exists`);
    }

    // Check hook limit
    if (this.hooks.length >= this.options.maxHooks) {
      throw new Error(`Maximum number of hooks (${this.options.maxHooks}) exceeded`);
    }

    this.hooks.push(hook);
  }

  /**
   * Remove a hook by ID.
   * @param {string} hookId - Hook identifier to remove
   * @returns {boolean} True if hook was removed, false if not found
   * 
   * @example
   * const removed = tx.removeHook("no-eve");
   * console.log('Hook removed:', removed);
   */
  removeHook(hookId) {
    if (typeof hookId !== 'string') {
      throw new TypeError('removeHook: hookId must be a string');
    }

    const index = this.hooks.findIndex(h => h.id === hookId);
    if (index === -1) {
      return false;
    }

    this.hooks.splice(index, 1);
    return true;
  }

  /**
   * Get all registered hooks.
   * @returns {Hook[]} Array of registered hooks
   */
  getHooks() {
    return [...this.hooks];
  }

  /**
   * Clear all hooks.
   */
  clearHooks() {
    this.hooks = [];
  }

  /**
   * Apply a transaction.
   * @param {Store} store - The store to apply the transaction to
   * @param {Delta} delta - The delta to apply
   * @param {Object} [options] - Transaction options
   * @param {boolean} [options.skipHooks=false] - Skip hook execution
   * @param {number} [options.timeoutMs=30000] - Transaction timeout
   * @returns {Promise<{store: Store, receipt: Receipt}>} Promise resolving to transaction result
   * 
   * @throws {Error} If transaction fails
   * 
   * @example
   * const delta = {
   *   additions: [quad(namedNode("ex:alice"), namedNode("ex:knows"), namedNode("ex:bob"))],
   *   removals: []
   * };
   * 
   * const result = await tx.apply(store, delta);
   * console.log('Committed:', result.receipt.committed);
   * console.log('New store size:', result.store.size);
   */
  async apply(store, delta, options = {}) {
    if (!store || typeof store.getQuads !== 'function') {
      throw new TypeError('apply: store must be a valid Store instance');
    }
    if (!delta || typeof delta !== 'object') {
      throw new TypeError('apply: delta must be an object');
    }
    if (!Array.isArray(delta.additions)) {
      throw new TypeError('apply: delta.additions must be an array');
    }
    if (!Array.isArray(delta.removals)) {
      throw new TypeError('apply: delta.removals must be an array');
    }

    const { skipHooks = false, timeoutMs = 30000 } = options;
    const startTime = Date.now();

    /** @type {HookResult[]} */
    const hookResults = [];

    try {
      // Set up timeout
      const timeoutPromise = new Promise((_, reject) => {
        setTimeout(() => reject(new Error('Transaction timeout')), timeoutMs);
      });

      const transactionPromise = this._executeTransaction(store, delta, skipHooks, hookResults);
      const result = await Promise.race([transactionPromise, timeoutPromise]);

      return {
        store: result.store,
        receipt: {
          ...result.receipt,
          timestamp: startTime
        }
      };
    } catch (error) {
      const beforeHash = await hashStore(store).catch(() => ({ sha3: '', blake3: '' }));
      
      return {
        store,
        receipt: {
          delta,
          committed: false,
          hookResults,
          beforeHash,
          afterHash: beforeHash,
          timestamp: startTime,
          error: error.message
        }
      };
    }
  }

  /**
   * Execute the transaction with hooks.
   * @private
   */
  async _executeTransaction(store, delta, skipHooks, hookResults) {
    const beforeHash = await hashStore(store);

    // Pre-hooks
    if (!skipHooks) {
      for (const hook of this.hooks.filter(h => h.mode === 'pre')) {
        try {
          const ok = await hook.condition(store, delta);
          hookResults.push({ hookId: hook.id, mode: hook.mode, result: ok });
          
          if (!ok && hook.effect === 'veto') {
            return {
              store,
              receipt: { 
                delta, 
                committed: false, 
                hookResults, 
                beforeHash, 
                afterHash: beforeHash 
              }
            };
          }
        } catch (error) {
          hookResults.push({ 
            hookId: hook.id, 
            mode: hook.mode, 
            result: false, 
            error: error.message 
          });
          
          if (this.options.strictMode) {
            throw new Error(`Pre-hook "${hook.id}" failed: ${error.message}`);
          }
        }
      }
    }

    // Commit transaction
    const newStore = new Store([
      ...store.getQuads().filter(q => !delta.removals.some(r => r.equals(q))),
      ...delta.additions,
    ]);

    // Post-hooks
    if (!skipHooks) {
      for (const hook of this.hooks.filter(h => h.mode === 'post')) {
        try {
          const ok = await hook.condition(newStore, delta);
          hookResults.push({ hookId: hook.id, mode: hook.mode, result: ok });
          
          if (ok && typeof hook.effect === 'function') {
            await hook.effect(newStore, delta);
          }
        } catch (error) {
          hookResults.push({ 
            hookId: hook.id, 
            mode: hook.mode, 
            result: false, 
            error: error.message 
          });
          
          if (this.options.strictMode) {
            throw new Error(`Post-hook "${hook.id}" failed: ${error.message}`);
          }
        }
      }
    }

    const afterHash = await hashStore(newStore);

    return {
      store: newStore,
      receipt: { 
        delta, 
        committed: true, 
        hookResults, 
        beforeHash, 
        afterHash 
      }
    };
  }

  /**
   * Create a transaction session for batch operations.
   * @param {Store} initialStore - Initial store state
   * @param {Object} [sessionOptions] - Session options
   * @returns {Object} Transaction session
   * 
   * @example
   * const session = tx.createSession(store);
   * 
   * // Add multiple deltas
   * session.addDelta(delta1);
   * session.addDelta(delta2);
   * 
   * // Apply all deltas
   * const results = await session.applyAll();
   * 
   * // Get final state
   * const finalStore = session.getCurrentStore();
   */
  createSession(initialStore, sessionOptions = {}) {
    if (!initialStore || typeof initialStore.getQuads !== 'function') {
      throw new TypeError('createSession: initialStore must be a valid Store instance');
    }

    let currentStore = new Store(initialStore.getQuads());
    const deltas = [];
    const receipts = [];

    return {
      /**
       * Add a delta to the session.
       * @param {Delta} delta - Delta to add
       */
      addDelta(delta) {
        if (!delta || typeof delta !== 'object') {
          throw new TypeError('addDelta: delta must be an object');
        }
        deltas.push(delta);
      },

      /**
       * Apply all deltas in the session.
       * @param {Object} [options] - Apply options
       * @returns {Promise<Array<Receipt>>} Promise resolving to array of receipts
       */
      async applyAll(options = {}) {
        const results = [];
        
        for (const delta of deltas) {
          const result = await this.apply(currentStore, delta, options);
          currentStore = result.store;
          receipts.push(result.receipt);
          results.push(result.receipt);
        }
        
        return results;
      },

      /**
       * Get current store state.
       * @returns {Store} Current store
       */
      getCurrentStore() {
        return new Store(currentStore.getQuads());
      },

      /**
       * Get all receipts.
       * @returns {Receipt[]} Array of receipts
       */
      getReceipts() {
        return [...receipts];
      },

      /**
       * Reset session to initial state.
       */
      reset() {
        currentStore = new Store(initialStore.getQuads());
        deltas.length = 0;
        receipts.length = 0;
      },

      /**
       * Get session statistics.
       * @returns {Object} Session statistics
       */
      getStats() {
        const committedCount = receipts.filter(r => r.committed).length;
        const failedCount = receipts.length - committedCount;
        
        return {
          deltaCount: deltas.length,
          receiptCount: receipts.length,
          committedCount,
          failedCount,
          successRate: receipts.length > 0 ? committedCount / receipts.length : 0
        };
      }
    };
  }

  /**
   * Get transaction manager statistics.
   * @returns {Object} Manager statistics
   */
  getStats() {
    const preHooks = this.hooks.filter(h => h.mode === 'pre').length;
    const postHooks = this.hooks.filter(h => h.mode === 'post').length;
    
    return {
      totalHooks: this.hooks.length,
      preHooks,
      postHooks,
      maxHooks: this.options.maxHooks,
      strictMode: this.options.strictMode
    };
  }
}

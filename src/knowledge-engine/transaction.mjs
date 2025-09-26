/**
 * @file Transaction manager with hooks and receipts.
 * @module transaction
 */

import { Store } from 'n3';
import { sha3_256 } from '@noble/hashes/sha3.js';
import { blake3 } from '@noble/hashes/blake3.js';
import { utf8ToBytes, bytesToHex } from '@noble/hashes/utils.js';
import { canonicalize } from './canonicalize.mjs';
import { randomUUID } from 'crypto';
import { z } from 'zod';

// Zod schemas for validation
const QuadSchema = z.object({
  subject: z.any(), // RDF/JS Term
  predicate: z.any(), // RDF/JS Term
  object: z.any(), // RDF/JS Term
  graph: z.any().optional(), // RDF/JS Term
  equals: z.function().optional()
}).passthrough(); // Allow additional properties for RDF/JS Quad objects

const DeltaSchema = z.object({
  additions: z.array(QuadSchema),
  removals: z.array(QuadSchema)
});

const HookSchema = z.object({
  id: z.string().min(1),
  mode: z.enum(['pre', 'post']),
  condition: z.function(),
  effect: z.union([z.literal('veto'), z.function()])
});

const HookResultSchema = z.object({
  hookId: z.string(),
  mode: z.enum(['pre', 'post']),
  result: z.boolean(),
  error: z.string().optional()
});

const HashSchema = z.object({
  sha3: z.string(),
  blake3: z.string()
});

const ReceiptSchema = z.object({
  id: z.string(),
  delta: DeltaSchema,
  committed: z.boolean(),
  hookResults: z.array(HookResultSchema),
  beforeHash: HashSchema,
  afterHash: HashSchema,
  timestamp: z.number(),
  durationMs: z.number(),
  actor: z.string().optional(),
  hookErrors: z.array(z.string()),
  error: z.string().optional()
});

const TransactionOptionsSchema = z.object({
  skipHooks: z.boolean().default(false),
  timeoutMs: z.number().positive().default(30000),
  actor: z.string().optional()
});

const ManagerOptionsSchema = z.object({
  strictMode: z.boolean().default(false),
  maxHooks: z.number().positive().default(100),
  afterHashOnly: z.boolean().default(false)
});

/**
 * @typedef {z.infer<typeof DeltaSchema>} Delta
 * @typedef {z.infer<typeof HookSchema>} Hook
 * @typedef {z.infer<typeof HookResultSchema>} HookResult
 * @typedef {z.infer<typeof ReceiptSchema>} Receipt
 * @typedef {z.infer<typeof TransactionOptionsSchema>} TransactionOptions
 * @typedef {z.infer<typeof ManagerOptionsSchema>} ManagerOptions
 */

/**
 * Hash a store canonically with SHA-3 and BLAKE3.
 * @param {Store} store - The store to hash
 * @param {Object} [options] - Hashing options
 * @param {boolean} [options.afterHashOnly=false] - Skip canonicalization for performance
 * @returns {Promise<{ sha3: string, blake3: string }>} Promise resolving to hash object
 * 
 * @throws {Error} If hashing fails
 */
async function hashStore(store, options = {}) {
  try {
    if (options.afterHashOnly) {
      // Fast hash without canonicalization for performance
      const quads = store.getQuads();
      const content = quads.map(q => `${q.subject.value} ${q.predicate.value} ${q.object.value} ${q.graph.value}`).join('\n');
      const bytes = utf8ToBytes(content);
      return {
        sha3: bytesToHex(sha3_256(bytes)),
        blake3: bytesToHex(blake3(bytes)),
      };
    }
    
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
   * @param {ManagerOptions} [options] - Manager options
   */
  constructor(options = {}) {
    /** @type {Hook[]} */
    this.hooks = [];
    this.options = ManagerOptionsSchema.parse(options);
    
    // Simple mutex for concurrency control
    this._applyMutex = Promise.resolve();
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
    // Validate hook with Zod
    const validatedHook = HookSchema.parse(hook);

    // Check for duplicate IDs
    if (this.hooks.some(h => h.id === validatedHook.id)) {
      throw new Error(`Hook with id "${validatedHook.id}" already exists`);
    }

    // Check hook limit
    if (this.hooks.length >= this.options.maxHooks) {
      throw new Error(`Maximum number of hooks (${this.options.maxHooks}) exceeded`);
    }

    this.hooks.push(validatedHook);
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
    // Validate hookId with Zod
    const validatedHookId = z.string().parse(hookId);

    const index = this.hooks.findIndex(h => h.id === validatedHookId);
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
   * @param {TransactionOptions} [options] - Transaction options
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
    // Validate inputs with Zod
    if (!store || typeof store.getQuads !== 'function') {
      throw new TypeError('apply: store must be a valid Store instance');
    }
    
    const validatedDelta = DeltaSchema.parse(delta);
    const validatedOptions = TransactionOptionsSchema.parse(options);
    const startTime = Date.now();
    const transactionId = randomUUID();

    /** @type {HookResult[]} */
    const hookResults = [];
    /** @type {string[]} */
    const hookErrors = [];

    // Use mutex for concurrency control
    return new Promise((resolve, reject) => {
      this._applyMutex = this._applyMutex.then(async () => {
        try {
          // Set up timeout with proper cleanup
          let timeoutHandle;
          const timeoutPromise = new Promise((_, timeoutReject) => {
            timeoutHandle = setTimeout(() => timeoutReject(new Error('Transaction timeout')), validatedOptions.timeoutMs);
          });

          const transactionPromise = this._executeTransaction(store, validatedDelta, validatedOptions.skipHooks, hookResults, hookErrors, transactionId, validatedOptions.actor);
          
          const result = await Promise.race([transactionPromise, timeoutPromise]);
          clearTimeout(timeoutHandle);
          
          resolve({
            store: result.store,
            receipt: {
              ...result.receipt,
              id: transactionId,
              timestamp: startTime,
              durationMs: Date.now() - startTime,
              actor: validatedOptions.actor,
              hookErrors
            }
          });
        } catch (error) {
          const beforeHash = await hashStore(store, this.options).catch(() => ({ sha3: '', blake3: '' }));
          
          resolve({
            store,
            receipt: {
              id: transactionId,
              delta: validatedDelta,
              committed: false,
              hookResults,
              beforeHash,
              afterHash: beforeHash,
              timestamp: startTime,
              durationMs: Date.now() - startTime,
              actor: validatedOptions.actor,
              hookErrors,
              error: error.message
            }
          });
        }
      }).catch(reject);
    });
  }

  /**
   * Execute the transaction with hooks.
   * @private
   */
  async _executeTransaction(store, delta, skipHooks, hookResults, hookErrors, transactionId, actor) {
    const beforeHash = await hashStore(store, this.options);

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
          const errorMsg = `Pre-hook "${hook.id}" failed: ${error.message}`;
          hookErrors.push(errorMsg);
          hookResults.push({ 
            hookId: hook.id, 
            mode: hook.mode, 
            result: false, 
            error: error.message 
          });
          
          if (this.options.strictMode) {
            throw new Error(errorMsg);
          }
        }
      }
    }

    // Commit transaction - MUTATE IN PLACE for state accumulation
    // Remove quads first
    for (const quad of delta.removals) {
      store.removeQuad(quad);
    }
    
    // Add new quads
    for (const quad of delta.additions) {
      store.addQuad(quad);
    }

    // Post-hooks
    if (!skipHooks) {
      for (const hook of this.hooks.filter(h => h.mode === 'post')) {
        try {
          const ok = await hook.condition(store, delta);
          hookResults.push({ hookId: hook.id, mode: hook.mode, result: ok });
          
          // Post-hooks ignore veto effects - only execute function effects
          if (ok && typeof hook.effect === 'function') {
            await hook.effect(store, delta);
          }
        } catch (error) {
          const errorMsg = `Post-hook "${hook.id}" failed: ${error.message}`;
          hookErrors.push(errorMsg);
          hookResults.push({ 
            hookId: hook.id, 
            mode: hook.mode, 
            result: false, 
            error: error.message 
          });
          
          if (this.options.strictMode) {
            throw new Error(errorMsg);
          }
        }
      }
    }

    const afterHash = await hashStore(store, this.options);

    return {
      store,
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
        const validatedDelta = DeltaSchema.parse(delta);
        deltas.push(validatedDelta);
      },

      /**
       * Apply all deltas in the session.
       * @param {TransactionOptions} [options] - Apply options
       * @returns {Promise<Array<Receipt>>} Promise resolving to array of receipts
       */
      async applyAll(options = {}) {
        const validatedOptions = TransactionOptionsSchema.parse(options);
        const results = [];
        
        for (const delta of deltas) {
          // Capture manager.apply to preserve this context
          const result = await manager.apply(currentStore, delta, validatedOptions);
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
      strictMode: this.options.strictMode,
      afterHashOnly: this.options.afterHashOnly
    };
  }
}

/**
 * Print a receipt in a consistent format.
 * @param {Receipt} receipt - The receipt to print
 * @param {Object} [options] - Print options
 * @param {boolean} [options.verbose=false] - Include detailed information
 */
export function printReceipt(receipt, options = {}) {
  const { verbose = false } = options;
  
  console.log(`📋 Transaction Receipt ${receipt.id}`);
  console.log(`   Status: ${receipt.committed ? '✅ Committed' : '❌ Failed'}`);
  console.log(`   Duration: ${receipt.durationMs}ms`);
  
  if (receipt.actor) {
    console.log(`   Actor: ${receipt.actor}`);
  }
  
  if (receipt.error) {
    console.log(`   Error: ${receipt.error}`);
  }
  
  console.log(`   Hooks: ${receipt.hookResults.length} executed`);
  receipt.hookResults.forEach(result => {
    const status = result.result ? '✅' : '❌';
    console.log(`     ${status} ${result.hookId} (${result.mode})`);
    if (result.error) {
      console.log(`       Error: ${result.error}`);
    }
  });
  
  if (receipt.hookErrors.length > 0) {
    console.log(`   Hook Errors: ${receipt.hookErrors.length}`);
    receipt.hookErrors.forEach(error => {
      console.log(`     • ${error}`);
    });
  }
  
  if (verbose) {
    console.log(`   Delta: ${receipt.delta.additions.length} additions, ${receipt.delta.removals.length} removals`);
    console.log(`   Before Hash: ${receipt.beforeHash.sha3.substring(0, 16)}...`);
    console.log(`   After Hash:  ${receipt.afterHash.sha3.substring(0, 16)}...`);
  }
}


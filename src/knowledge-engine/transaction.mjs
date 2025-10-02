/**
 * @file Transaction manager with hooks and receipts.
 * @module transaction
 */

import { Store } from 'n3';
import { sha3_256 } from '@noble/hashes/sha3.js';
import { blake3 } from '@noble/hashes/blake3.js';
import { utf8ToBytes, bytesToHex } from '@noble/hashes/utils.js';
import { canonicalize } from './canonicalize.mjs';
import { createLockchainWriter } from './lockchain-writer.mjs';
import { createResolutionLayer } from './resolution-layer.mjs';
import { createObservabilityManager } from './observability.mjs';
import { randomUUID } from 'crypto';
import { z } from 'zod';

// Import consolidated schemas
import { 
  QuadSchema, 
  DeltaSchema, 
  TransactionHookSchema, 
  TransactionHookResultSchema, 
  HashSchema, 
  TransactionReceiptSchemaNew, 
  TransactionOptionsSchema, 
  ManagerOptionsSchema 
} from './schemas.mjs';

// Use alias for backward compatibility
const HookSchema = TransactionHookSchema;
const HookResultSchema = TransactionHookResultSchema;
const ReceiptSchema = TransactionReceiptSchemaNew;

// Zod schemas for validation
// QuadSchema now imported from schemas.mjs

// All schemas now imported from schemas.mjs

/**
 * @typedef {z.infer<typeof DeltaSchema>} Delta
 * @typedef {z.infer<typeof TransactionHookSchema>} Hook
 * @typedef {z.infer<typeof TransactionHookResultSchema>} HookResult
 * @typedef {z.infer<typeof TransactionReceiptSchemaNew>} Receipt
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

    // Simple mutex for concurrency control - no circular ref accumulation
    this._applyMutex = null;
    this._resetMutex();

    // Initialize observability manager
    this.observability = createObservabilityManager(this.options.observability || {});

    // Performance tracking with bounded arrays
    this.performanceMetrics = {
      transactionLatency: [],
      hookExecutionRate: 0,
      errorCount: 0,
      totalTransactions: 0,
      _maxLatencyEntries: 1000
    };
    
    // Initialize lockchain writer if enabled
    if (this.options.enableLockchain) {
      const lockchainConfig = {
        gitRepo: this.options.lockchainConfig?.gitRepo || process.cwd(),
        refName: this.options.lockchainConfig?.refName || 'refs/notes/lockchain',
        signingKey: this.options.lockchainConfig?.signingKey,
        batchSize: this.options.lockchainConfig?.batchSize || 10
      };
      this.lockchainWriter = createLockchainWriter(lockchainConfig);
    } else {
      this.lockchainWriter = null;
    }
    
    // Initialize resolution layer if enabled
    if (this.options.enableResolution) {
      const resolutionConfig = {
        defaultStrategy: this.options.resolutionConfig?.defaultStrategy || 'voting',
        maxProposals: this.options.resolutionConfig?.maxProposals || 100,
        enableConflictDetection: this.options.resolutionConfig?.enableConflictDetection !== false,
        enableConsensus: this.options.resolutionConfig?.enableConsensus !== false,
        timeout: this.options.resolutionConfig?.timeout || 30000
      };
      this.resolutionLayer = createResolutionLayer(resolutionConfig);
    } else {
      this.resolutionLayer = null;
    }
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
    const validatedHook = TransactionHookSchema.parse(hook);

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

    // Start observability span
    const spanContext = this.observability.startTransactionSpan(transactionId, {
      'kgc.delta.additions': validatedDelta.additions.length,
      'kgc.delta.removals': validatedDelta.removals.length,
      'kgc.actor': validatedOptions.actor || 'system',
      'kgc.skipHooks': validatedOptions.skipHooks || false
    });

    // Use mutex for concurrency control - reset to prevent chain buildup
    const currentMutex = this._applyMutex;

    return new Promise((resolve, reject) => {
      this._applyMutex = currentMutex.then(async () => {
        try {
          // Set up timeout with proper cleanup
          let timeoutHandle;
          const timeoutPromise = new Promise((_, timeoutReject) => {
            timeoutHandle = setTimeout(() => timeoutReject(new Error('Transaction timeout')), validatedOptions.timeoutMs);
          });

          const transactionPromise = this._executeTransaction(store, validatedDelta, validatedOptions.skipHooks, hookResults, hookErrors, transactionId, validatedOptions.actor);

          const result = await Promise.race([transactionPromise, timeoutPromise]);
          clearTimeout(timeoutHandle);

          // Reset mutex chain to prevent circular reference buildup
          this._resetMutex();
          
          const finalReceipt = {
            ...result.receipt,
            id: transactionId,
            timestamp: startTime,
            durationMs: Date.now() - startTime,
            actor: validatedOptions.actor,
            hookErrors
          };
          
          // Write to lockchain if enabled
          if (this.lockchainWriter && result.receipt.committed) {
            try {
              await this.lockchainWriter.writeReceipt(finalReceipt);
            } catch (lockchainError) {
              console.warn('Failed to write receipt to lockchain:', lockchainError.message);
            }
          }
          
          // Update performance metrics
          const duration = Date.now() - startTime;
          this._updatePerformanceMetrics(duration, true);
          
          // End observability span
          this.observability.endTransactionSpan(transactionId, {
            'kgc.transaction.committed': finalReceipt.committed,
            'kgc.hook.results': hookResults.length,
            'kgc.hook.errors': hookErrors.length
          });
          
          resolve({
            store: result.store,
            receipt: finalReceipt
          });
        } catch (error) {
          const beforeHash = await hashStore(store, this.options).catch(() => ({ sha3: '', blake3: '' }));
          
          // Update performance metrics
          const duration = Date.now() - startTime;
          this._updatePerformanceMetrics(duration, false);
          
          // Record error
          this.observability.recordError(error, {
            'kgc.transaction.id': transactionId,
            'kgc.actor': validatedOptions.actor || 'system'
          });
          
          // End observability span with error
          this.observability.endTransactionSpan(transactionId, {}, error);
          
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
    
    const stats = {
      totalHooks: this.hooks.length,
      preHooks,
      postHooks,
      maxHooks: this.options.maxHooks,
      strictMode: this.options.strictMode,
      afterHashOnly: this.options.afterHashOnly,
      lockchainEnabled: this.options.enableLockchain
    };
    
    // Add lockchain stats if enabled
    if (this.lockchainWriter) {
      stats.lockchain = this.lockchainWriter.getStats();
    }
    
    return stats;
  }
  
  /**
   * Commit pending lockchain entries
   * @returns {Promise<Object>} Commit result
   */
  async commitLockchain() {
    if (!this.lockchainWriter) {
      throw new Error('Lockchain is not enabled');
    }
    
    return this.lockchainWriter.commitBatch();
  }

  /**
   * Submit a proposal to the resolution layer
   * @param {string} agentId - Agent identifier
   * @param {Object} delta - Proposed delta
   * @param {Object} [options] - Proposal options
   * @returns {Promise<string>} Proposal ID
   */
  async submitProposal(agentId, delta, options = {}) {
    if (!this.resolutionLayer) {
      throw new Error('Resolution layer is not enabled');
    }
    
    return this.resolutionLayer.submitProposal(agentId, delta, options);
  }

  /**
   * Resolve proposals using the resolution layer
   * @param {Array<string>} proposalIds - Proposal IDs to resolve
   * @param {Object} [strategy] - Resolution strategy
   * @returns {Promise<Object>} Resolution result
   */
  async resolveProposals(proposalIds, strategy = {}) {
    if (!this.resolutionLayer) {
      throw new Error('Resolution layer is not enabled');
    }
    
    return this.resolutionLayer.resolveProposals(proposalIds, strategy);
  }

  /**
   * Get resolution layer statistics
   * @returns {Object} Resolution statistics
   */
  getResolutionStats() {
    if (!this.resolutionLayer) {
      return { enabled: false };
    }
    
    return {
      enabled: true,
      ...this.resolutionLayer.getStats()
    };
  }

  /**
   * Get manager statistics.
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      hooks: this.hooks.length,
      lockchain: this.lockchainWriter ? this.lockchainWriter.getStats() : null,
      resolution: this.resolutionLayer ? this.resolutionLayer.getStats() : null,
      performance: this.performanceMetrics,
      observability: this.observability.getPerformanceMetrics()
    };
  }

  /**
   * Update performance metrics
   * @param {number} duration - Transaction duration
   * @param {boolean} success - Whether transaction succeeded
   * @private
   */
  _updatePerformanceMetrics(duration, success) {
    const maxEntries = this.performanceMetrics._maxLatencyEntries;

    // Prevent unbounded array growth - remove oldest before adding
    if (this.performanceMetrics.transactionLatency.length >= maxEntries) {
      this.performanceMetrics.transactionLatency.shift();
    }

    this.performanceMetrics.transactionLatency.push({
      timestamp: Date.now(),
      duration,
      success
    });

    this.performanceMetrics.totalTransactions++;

    if (!success) {
      this.performanceMetrics.errorCount++;
    }
  }

  /**
   * Reset mutex chain to prevent circular references
   * @private
   */
  _resetMutex() {
    this._applyMutex = Promise.resolve();
  }

  /**
   * Cleanup transaction manager resources
   */
  async cleanup() {
    // Clear hooks
    this.hooks.length = 0;

    // Clear performance metrics
    this.performanceMetrics.transactionLatency.length = 0;
    this.performanceMetrics.errorCount = 0;
    this.performanceMetrics.totalTransactions = 0;

    // Cleanup lockchain writer
    if (this.lockchainWriter && typeof this.lockchainWriter.cleanup === 'function') {
      await this.lockchainWriter.cleanup();
    }

    // Cleanup resolution layer
    if (this.resolutionLayer && typeof this.resolutionLayer.cleanup === 'function') {
      await this.resolutionLayer.cleanup();
    }

    // Reset mutex
    this._resetMutex();
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


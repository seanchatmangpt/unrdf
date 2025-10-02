/**
 * @file Global test cleanup hooks
 * @module test-setup/cleanup-hooks
 *
 * @description
 * Provides automatic cleanup after each test to prevent memory leaks.
 * Clears caches, removes event listeners, closes connections, and triggers GC.
 */

import { afterEach, beforeEach } from 'vitest';

/**
 * Global cleanup registry
 */
const cleanupRegistry = {
  stores: new Set(),
  managers: new Set(),
  emitters: new Set(),
  timers: new Set(),
  connections: new Set(),
  caches: new Map()
};

/**
 * Register a store for cleanup
 * @param {Store} store - N3 Store instance
 */
export function registerStore(store) {
  cleanupRegistry.stores.add(store);
}

/**
 * Register a manager for cleanup
 * @param {Object} manager - Manager instance (TransactionManager, etc.)
 */
export function registerManager(manager) {
  cleanupRegistry.managers.add(manager);
}

/**
 * Register an event emitter for cleanup
 * @param {EventEmitter} emitter - Event emitter
 */
export function registerEmitter(emitter) {
  cleanupRegistry.emitters.add(emitter);
}

/**
 * Register a timer for cleanup
 * @param {number} timerId - Timer ID
 */
export function registerTimer(timerId) {
  cleanupRegistry.timers.add(timerId);
}

/**
 * Register a connection for cleanup
 * @param {Object} connection - Connection object
 */
export function registerConnection(connection) {
  cleanupRegistry.connections.add(connection);
}

/**
 * Register a cache for cleanup
 * @param {string} name - Cache name
 * @param {Map|Set} cache - Cache object
 */
export function registerCache(name, cache) {
  cleanupRegistry.caches.set(name, cache);
}

/**
 * Cleanup all registered resources
 */
async function cleanupAll() {
  // Clear all stores
  for (const store of cleanupRegistry.stores) {
    try {
      if (typeof store.removeQuads === 'function') {
        store.removeQuads(store.getQuads());
      }
    } catch (error) {
      console.warn('[Cleanup] Failed to clear store:', error.message);
    }
  }
  cleanupRegistry.stores.clear();

  // Cleanup all managers
  for (const manager of cleanupRegistry.managers) {
    try {
      // Clear hooks
      if (typeof manager.clearHooks === 'function') {
        manager.clearHooks();
      }

      // Cleanup method
      if (typeof manager.cleanup === 'function') {
        await manager.cleanup();
      }

      // Clear performance metrics
      if (manager.performanceMetrics && typeof manager.performanceMetrics === 'object') {
        if (Array.isArray(manager.performanceMetrics.transactionLatency)) {
          manager.performanceMetrics.transactionLatency.length = 0;
        }
      }

      // Clear component maps (for DarkMatterCore)
      if (manager.components && typeof manager.components.clear === 'function') {
        manager.components.clear();
      }

      // Clear query caches (for QueryOptimizer)
      if (manager.queryPlans && typeof manager.queryPlans.clear === 'function') {
        manager.queryPlans.clear();
      }
      if (manager.indexes && typeof manager.indexes.clear === 'function') {
        manager.indexes.clear();
      }
      if (manager.cache && typeof manager.cache.clear === 'function') {
        manager.cache.clear();
      }
    } catch (error) {
      console.warn('[Cleanup] Failed to cleanup manager:', error.message);
    }
  }
  cleanupRegistry.managers.clear();

  // Remove all event listeners
  for (const emitter of cleanupRegistry.emitters) {
    try {
      if (typeof emitter.removeAllListeners === 'function') {
        emitter.removeAllListeners();
      }
    } catch (error) {
      console.warn('[Cleanup] Failed to remove listeners:', error.message);
    }
  }
  cleanupRegistry.emitters.clear();

  // Clear all timers
  for (const timerId of cleanupRegistry.timers) {
    try {
      clearTimeout(timerId);
      clearInterval(timerId);
    } catch (error) {
      console.warn('[Cleanup] Failed to clear timer:', error.message);
    }
  }
  cleanupRegistry.timers.clear();

  // Close all connections
  for (const connection of cleanupRegistry.connections) {
    try {
      if (typeof connection.close === 'function') {
        await connection.close();
      } else if (typeof connection.end === 'function') {
        await connection.end();
      } else if (typeof connection.destroy === 'function') {
        connection.destroy();
      }
    } catch (error) {
      console.warn('[Cleanup] Failed to close connection:', error.message);
    }
  }
  cleanupRegistry.connections.clear();

  // Clear all caches
  for (const [name, cache] of cleanupRegistry.caches) {
    try {
      if (typeof cache.clear === 'function') {
        cache.clear();
      }
    } catch (error) {
      console.warn(`[Cleanup] Failed to clear cache ${name}:`, error.message);
    }
  }
  cleanupRegistry.caches.clear();

  // Force garbage collection if available
  if (global.gc) {
    global.gc();
  }
}

/**
 * Memory usage before test
 */
let memoryBefore = null;

/**
 * Before each test - record memory baseline
 */
beforeEach(() => {
  memoryBefore = process.memoryUsage();
});

/**
 * After each test - cleanup and check for leaks
 */
afterEach(async () => {
  // Run cleanup
  await cleanupAll();

  // Check memory growth
  const memoryAfter = process.memoryUsage();
  const heapGrowth = memoryAfter.heapUsed - memoryBefore.heapUsed;
  const heapGrowthMB = heapGrowth / 1024 / 1024;

  // Warn if significant memory growth
  if (heapGrowthMB > 50) {
    console.warn(`[MemoryLeak] Test leaked ${heapGrowthMB.toFixed(2)}MB of heap memory`);
  }

  // Reset baseline
  memoryBefore = null;
});

/**
 * Helper to create auto-cleanup store
 * @param {Array} [quads=[]] - Initial quads
 * @returns {Promise<Store>} Store with auto-cleanup
 */
export async function createTestStore(quads = []) {
  const { Store } = await import('n3');
  const store = new Store(quads);
  registerStore(store);
  return store;
}

/**
 * Helper to create auto-cleanup manager
 * @param {Object} [config={}] - Manager config
 * @returns {Promise<TransactionManager>} Manager with auto-cleanup
 */
export async function createTestManager(config = {}) {
  const { TransactionManager } = await import('../../src/knowledge-engine/transaction.mjs');
  const manager = new TransactionManager(config);
  registerManager(manager);
  return manager;
}

/**
 * Helper to create auto-cleanup optimizer
 * @param {Object} [config={}] - Optimizer config
 * @returns {Promise<QueryOptimizer>} Optimizer with auto-cleanup
 */
export async function createTestOptimizer(config = {}) {
  const { QueryOptimizer } = await import('../../src/knowledge-engine/query-optimizer.mjs');
  const optimizer = new QueryOptimizer(config);
  registerManager(optimizer);
  return optimizer;
}

/**
 * Helper to create auto-cleanup dark matter core
 * @param {Object} [config={}] - Core config
 * @returns {Promise<DarkMatterCore>} Core with auto-cleanup
 */
export async function createTestDarkMatterCore(config = {}) {
  const { DarkMatterCore } = await import('../../src/knowledge-engine/dark-matter-core.mjs');
  const core = new DarkMatterCore(config);
  registerManager(core);
  return core;
}

/**
 * Manual cleanup trigger (for explicit cleanup in tests)
 */
export async function cleanup() {
  await cleanupAll();
}

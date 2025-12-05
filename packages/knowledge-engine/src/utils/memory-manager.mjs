/**
 * @file Memory Manager for preventing heap OOM
 * @module memory-manager
 *
 * @description
 * Provides WeakMap/WeakSet-based memory management, connection pooling,
 * and automatic cleanup to prevent circular references and memory leaks.
 */

import { EventEmitter } from 'node:events';

/**
 * Memory Manager - Prevents circular references and unbounded growth
 */
export class MemoryManager {
  /**
   *
   */
  constructor(config = {}) {
    this.config = {
      maxArraySize: config.maxArraySize || 1000,
      maxMapSize: config.maxMapSize || 10000,
      enableGCHints: config.enableGCHints !== false,
      gcInterval: config.gcInterval || 60000, // 1 minute
      memoryThreshold: config.memoryThreshold || 0.8, // 80% of heap
      ...config,
    };

    // Use WeakMap for circular reference prevention
    this.weakRefs = new WeakMap();
    this.weakSets = new WeakSet();

    // Track managed resources
    this.managedArrays = new Set();
    this.managedMaps = new Set();
    this.managedEmitters = new Set();

    // Cleanup registry for finalization
    this.cleanupRegistry = new FinalizationRegistry(this._finalize.bind(this));

    // Memory monitoring
    this.memoryStats = {
      arraysManaged: 0,
      mapsManaged: 0,
      emittersManaged: 0,
      gcRuns: 0,
      lastGC: null,
    };

    // Start memory monitoring
    if (this.config.enableGCHints) {
      this._startMonitoring();
    }
  }

  /**
   * Create a managed array with automatic size limiting
   * @param {Array} [initial=[]] - Initial array
   * @returns {Proxy} Managed array proxy
   */
  createManagedArray(initial = []) {
    const manager = this;
    const arr = [...initial];
    this.managedArrays.add(arr);
    this.memoryStats.arraysManaged++;

    return new Proxy(arr, {
      set(target, prop, value) {
        // Limit array size
        if (prop === 'length' || !isNaN(prop)) {
          if (target.length >= manager.config.maxArraySize) {
            // Remove oldest entries (FIFO)
            target.splice(0, Math.floor(manager.config.maxArraySize * 0.2));
          }
        }
        target[prop] = value;
        return true;
      },
    });
  }

  /**
   * Create a managed Map with automatic size limiting
   * @param {Map} [initial=new Map()] - Initial map
   * @returns {Proxy} Managed map proxy
   */
  createManagedMap(initial = new Map()) {
    const manager = this;
    const map = new Map(initial);
    this.managedMaps.add(map);
    this.memoryStats.mapsManaged++;

    return new Proxy(map, {
      get(target, prop) {
        if (prop === 'set') {
          return function (key, value) {
            // Check size limit
            if (target.size >= manager.config.maxMapSize && !target.has(key)) {
              // Remove oldest entry (first entry)
              const firstKey = target.keys().next().value;
              target.delete(firstKey);
            }
            return target.set(key, value);
          };
        }
        return target[prop];
      },
    });
  }

  /**
   * Register an EventEmitter for cleanup tracking
   * @param {EventEmitter} emitter - Event emitter
   * @returns {EventEmitter} Tracked emitter
   */
  trackEmitter(emitter) {
    if (!(emitter instanceof EventEmitter)) {
      throw new TypeError('Expected EventEmitter instance');
    }

    this.managedEmitters.add(emitter);
    this.memoryStats.emittersManaged++;
    this.cleanupRegistry.register(emitter, {
      type: 'emitter',
      id: Symbol('emitter'),
    });

    return emitter;
  }

  /**
   * Create a WeakRef wrapper for preventing circular references
   * @param {Object} obj - Object to wrap
   * @returns {WeakRef} Weak reference
   */
  createWeakRef(obj) {
    if (typeof obj !== 'object' || obj === null) {
      throw new TypeError('WeakRef requires an object');
    }

    const ref = new WeakRef(obj);
    this.cleanupRegistry.register(obj, {
      type: 'weakref',
      id: Symbol('weakref'),
    });

    return ref;
  }

  /**
   * Store data with weak reference (prevents circular refs)
   * @param {Object} key - Key object
   * @param {*} value - Value to store
   */
  setWeak(key, value) {
    this.weakRefs.set(key, value);
  }

  /**
   * Get data from weak reference
   * @param {Object} key - Key object
   * @returns {*} Stored value or undefined
   */
  getWeak(key) {
    return this.weakRefs.get(key);
  }

  /**
   * Add to weak set
   * @param {Object} obj - Object to add
   */
  addWeak(obj) {
    this.weakSets.add(obj);
  }

  /**
   * Check if weak set has object
   * @param {Object} obj - Object to check
   * @returns {boolean} Has object
   */
  hasWeak(obj) {
    return this.weakSets.has(obj);
  }

  /**
   * Manual cleanup trigger
   */
  cleanup() {
    // Clear managed arrays
    for (const arr of this.managedArrays) {
      arr.length = 0;
    }
    this.managedArrays.clear();

    // Clear managed maps
    for (const map of this.managedMaps) {
      map.clear();
    }
    this.managedMaps.clear();

    // Remove all listeners from emitters
    for (const emitter of this.managedEmitters) {
      emitter.removeAllListeners();
    }
    this.managedEmitters.clear();

    // Reset stats
    this.memoryStats.arraysManaged = 0;
    this.memoryStats.mapsManaged = 0;
    this.memoryStats.emittersManaged = 0;

    // Hint GC
    if (this.config.enableGCHints && global.gc) {
      global.gc();
      this.memoryStats.gcRuns++;
      this.memoryStats.lastGC = Date.now();
    }
  }

  /**
   * Get memory stats
   * @returns {Object} Memory statistics
   */
  getStats() {
    const heapUsed = process.memoryUsage().heapUsed;
    const heapTotal = process.memoryUsage().heapTotal;

    return {
      ...this.memoryStats,
      heapUsed,
      heapTotal,
      heapUsedMB: Math.round(heapUsed / 1024 / 1024),
      heapTotalMB: Math.round(heapTotal / 1024 / 1024),
      heapUtilization: heapUsed / heapTotal,
      timestamp: Date.now(),
    };
  }

  /**
   * Start memory monitoring
   * @private
   */
  _startMonitoring() {
    this._monitoringInterval = setInterval(() => {
      const stats = this.getStats();

      // Trigger GC if heap usage is above threshold
      if (stats.heapUtilization > this.config.memoryThreshold) {
        console.warn(
          `[MemoryManager] Heap usage ${(stats.heapUtilization * 100).toFixed(1)}% exceeds threshold ${(this.config.memoryThreshold * 100).toFixed(1)}%`
        );

        if (global.gc) {
          global.gc();
          this.memoryStats.gcRuns++;
          this.memoryStats.lastGC = Date.now();
        }
      }
    }, this.config.gcInterval);

    // Don't block process exit
    this._monitoringInterval.unref();
  }

  /**
   * Stop memory monitoring
   */
  stopMonitoring() {
    if (this._monitoringInterval) {
      clearInterval(this._monitoringInterval);
      this._monitoringInterval = null;
    }
  }

  /**
   * Finalization callback
   * @private
   */
  _finalize(_heldValue) {
    // Object was garbage collected
    // Nothing to do - cleanup already happened
  }

  /**
   * Destroy memory manager
   */
  destroy() {
    this.stopMonitoring();
    this.cleanup();
  }
}

/**
 * Connection Pool Manager
 * Prevents connection leaks and manages resource limits
 */
export class ConnectionPool {
  /**
   *
   */
  constructor(config = {}) {
    this.config = {
      maxConnections: config.maxConnections || 10,
      minConnections: config.minConnections || 2,
      acquireTimeout: config.acquireTimeout || 5000,
      idleTimeout: config.idleTimeout || 30000,
      createConnection: config.createConnection || (() => Promise.resolve({})),
      destroyConnection: config.destroyConnection || (() => Promise.resolve()),
      validateConnection: config.validateConnection || (() => true),
      ...config,
    };

    this.available = [];
    this.inUse = new Set();
    this.total = 0;
    this.pendingAcquires = [];

    // Stats
    this.stats = {
      acquired: 0,
      released: 0,
      created: 0,
      destroyed: 0,
      timeouts: 0,
      errors: 0,
    };
  }

  /**
   * Initialize pool
   */
  async initialize() {
    for (let i = 0; i < this.config.minConnections; i++) {
      const conn = await this._createConnection();
      this.available.push(conn);
    }
  }

  /**
   * Acquire a connection from the pool
   * @returns {Promise<Object>} Connection
   */
  async acquire() {
    // Try to get from available
    if (this.available.length > 0) {
      const conn = this.available.pop();

      // Validate connection
      if (this.config.validateConnection(conn)) {
        this.inUse.add(conn);
        this.stats.acquired++;
        return conn;
      } else {
        // Connection is stale, destroy and create new
        await this._destroyConnection(conn);
        return this.acquire();
      }
    }

    // Create new if under limit
    if (this.total < this.config.maxConnections) {
      const conn = await this._createConnection();
      this.inUse.add(conn);
      this.stats.acquired++;
      return conn;
    }

    // Wait for connection to be released
    return new Promise((resolve, reject) => {
      const timeoutId = setTimeout(() => {
        const idx = this.pendingAcquires.findIndex(p => p.resolve === resolve);
        if (idx !== -1) {
          this.pendingAcquires.splice(idx, 1);
        }
        this.stats.timeouts++;
        reject(new Error('Connection acquire timeout'));
      }, this.config.acquireTimeout);

      this.pendingAcquires.push({ resolve, reject, timeoutId });
    });
  }

  /**
   * Release a connection back to the pool
   * @param {Object} conn - Connection to release
   */
  release(conn) {
    if (!this.inUse.has(conn)) {
      return;
    }

    this.inUse.delete(conn);
    this.stats.released++;

    // Give to pending acquire if any
    if (this.pendingAcquires.length > 0) {
      const { resolve, timeoutId } = this.pendingAcquires.shift();
      clearTimeout(timeoutId);
      this.inUse.add(conn);
      this.stats.acquired++;
      resolve(conn);
      return;
    }

    // Return to pool
    this.available.push(conn);
  }

  /**
   * Drain the pool (release all connections)
   */
  async drain() {
    // Wait for all in-use connections to be released
    while (this.inUse.size > 0) {
      await new Promise(resolve => setTimeout(resolve, 100));
    }

    // Destroy all available connections
    for (const conn of this.available) {
      await this._destroyConnection(conn);
    }

    this.available = [];
    this.total = 0;
  }

  /**
   * Create a new connection
   * @private
   */
  async _createConnection() {
    try {
      const conn = await this.config.createConnection();
      this.total++;
      this.stats.created++;
      return conn;
    } catch (error) {
      this.stats.errors++;
      throw error;
    }
  }

  /**
   * Destroy a connection
   * @private
   */
  async _destroyConnection(conn) {
    try {
      await this.config.destroyConnection(conn);
      this.total--;
      this.stats.destroyed++;
    } catch (error) {
      this.stats.errors++;
    }
  }

  /**
   * Get pool stats
   * @returns {Object} Pool statistics
   */
  getStats() {
    return {
      ...this.stats,
      available: this.available.length,
      inUse: this.inUse.size,
      total: this.total,
      pending: this.pendingAcquires.length,
    };
  }
}

/**
 * Global memory manager instance
 */
export const globalMemoryManager = new MemoryManager({
  maxArraySize: 1000,
  maxMapSize: 10000,
  enableGCHints: true,
  gcInterval: 60000,
});

/**
 * Create a memory manager instance
 * @param {Object} [config] - Configuration
 * @returns {MemoryManager} Memory manager
 */
export function createMemoryManager(config = {}) {
  return new MemoryManager(config);
}

/**
 * Create a connection pool instance
 * @param {Object} [config] - Configuration
 * @returns {ConnectionPool} Connection pool
 */
export function createConnectionPool(config = {}) {
  return new ConnectionPool(config);
}

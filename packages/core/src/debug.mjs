/**
 * @file Debug utilities and performance monitoring
 * @module @unrdf/core/debug
 */

/**
 * Debug namespace configuration
 * Enable with: DEBUG=unrdf:* or DEBUG=unrdf:store,unrdf:query
 */
const DEBUG_NAMESPACES = {
  store: 'unrdf:store',
  query: 'unrdf:query',
  hooks: 'unrdf:hooks',
  workflow: 'unrdf:workflow',
  federation: 'unrdf:federation',
  cache: 'unrdf:cache',
  performance: 'unrdf:perf',
};

/**
 * Check if debug mode is enabled
 * @param {string} namespace - Debug namespace
 * @returns {boolean} True if enabled
 */
function isDebugEnabled(namespace) {
  const debugEnv = process.env.DEBUG || '';

  // Check for wildcard
  if (debugEnv.includes('unrdf:*')) return true;

  // Check for specific namespace
  return debugEnv.split(',').some((ns) => ns.trim() === namespace);
}

/**
 * Format debug message with timestamp and namespace
 * @param {string} namespace - Debug namespace
 * @param {string} message - Message
 * @param {Object} [data] - Additional data
 * @returns {string} Formatted message
 */
function formatDebugMessage(namespace, message, data = null) {
  const timestamp = new Date().toISOString();
  const parts = [`[${timestamp}]`, `[${namespace}]`, message];

  if (data) {
    parts.push(JSON.stringify(data, null, 2));
  }

  return parts.join(' ');
}

/**
 * Debug logger for specific namespace
 */
export class DebugLogger {
  /**
   * @param {string} namespace - Debug namespace (e.g., 'store', 'query')
   */
  constructor(namespace) {
    this.namespace = DEBUG_NAMESPACES[namespace] || `unrdf:${namespace}`;
    this.enabled = isDebugEnabled(this.namespace);
    this.timers = new Map();
  }

  /**
   * Log debug message
   * @param {string} message - Message
   * @param {Object} [data] - Additional data
   */
  log(message, data = null) {
    if (!this.enabled) return;

    const formatted = formatDebugMessage(this.namespace, message, data);
    console.debug(formatted);
  }

  /**
   * Log error message
   * @param {string} message - Message
   * @param {Error} [error] - Error object
   */
  error(message, error = null) {
    if (!this.enabled) return;

    const data = error
      ? {
          name: error.name,
          message: error.message,
          code: error.code,
          stack: error.stack,
        }
      : null;

    const formatted = formatDebugMessage(this.namespace, `ERROR: ${message}`, data);
    console.error(formatted);
  }

  /**
   * Log warning message
   * @param {string} message - Message
   * @param {Object} [data] - Additional data
   */
  warn(message, data = null) {
    if (!this.enabled) return;

    const formatted = formatDebugMessage(this.namespace, `WARN: ${message}`, data);
    console.warn(formatted);
  }

  /**
   * Start performance timer
   * @param {string} label - Timer label
   */
  time(label) {
    if (!this.enabled) return;

    this.timers.set(label, performance.now());
    this.log(`Timer started: ${label}`);
  }

  /**
   * End performance timer and log duration
   * @param {string} label - Timer label
   * @returns {number|null} Duration in milliseconds
   */
  timeEnd(label) {
    if (!this.enabled) return null;

    const start = this.timers.get(label);
    if (!start) {
      this.warn(`Timer not found: ${label}`);
      return null;
    }

    const duration = performance.now() - start;
    this.timers.delete(label);

    this.log(`Timer ended: ${label}`, { durationMs: duration.toFixed(2) });
    return duration;
  }

  /**
   * Log memory usage
   * @param {string} [label] - Optional label
   */
  memory(label = 'Memory Usage') {
    if (!this.enabled) return;

    const mem = process.memoryUsage();
    const data = {
      rss: `${(mem.rss / 1024 / 1024).toFixed(2)} MB`,
      heapTotal: `${(mem.heapTotal / 1024 / 1024).toFixed(2)} MB`,
      heapUsed: `${(mem.heapUsed / 1024 / 1024).toFixed(2)} MB`,
      external: `${(mem.external / 1024 / 1024).toFixed(2)} MB`,
    };

    this.log(label, data);
  }

  /**
   * Create child logger with sub-namespace
   * @param {string} subNamespace - Sub-namespace
   * @returns {DebugLogger} Child logger
   */
  child(subNamespace) {
    const childNamespace = `${this.namespace}:${subNamespace}`;
    const logger = new DebugLogger(subNamespace);
    logger.namespace = childNamespace;
    logger.enabled = isDebugEnabled(childNamespace) || this.enabled;
    return logger;
  }
}

/**
 * Create debug logger for namespace
 * @param {string} namespace - Namespace (store, query, hooks, etc.)
 * @returns {DebugLogger} Debug logger
 *
 * @example
 * const debug = createDebugger('store');
 * debug.log('Adding quad', { quad });
 * debug.time('operation');
 * // ... do work
 * debug.timeEnd('operation');
 */
export function createDebugger(namespace) {
  return new DebugLogger(namespace);
}

/**
 * Performance tracker for operations
 */
export class PerformanceTracker {
  /**
   * Create performance tracker
   */
  constructor() {
    this.metrics = new Map();
    this.debug = createDebugger('performance');
  }

  /**
   * Track operation start
   * @param {string} operation - Operation name
   * @returns {string} Unique operation ID
   */
  start(operation) {
    const id = `${operation}_${Date.now()}_${Math.random().toString(36).slice(2)}`;
    const startTime = performance.now();

    this.metrics.set(id, {
      operation,
      startTime,
      startMemory: process.memoryUsage().heapUsed,
    });

    this.debug.log(`Started: ${operation}`, { id });
    return id;
  }

  /**
   * Track operation end
   * @param {string} id - Operation ID from start()
   * @returns {Object|null} Performance metrics
   */
  end(id) {
    const metric = this.metrics.get(id);
    if (!metric) {
      this.debug.warn(`Metric not found: ${id}`);
      return null;
    }

    const endTime = performance.now();
    const endMemory = process.memoryUsage().heapUsed;

    const result = {
      operation: metric.operation,
      durationMs: (endTime - metric.startTime).toFixed(2),
      memoryDeltaMB: ((endMemory - metric.startMemory) / 1024 / 1024).toFixed(2),
      timestamp: new Date().toISOString(),
    };

    this.metrics.delete(id);
    this.debug.log(`Completed: ${metric.operation}`, result);

    return result;
  }

  /**
   * Get all active metrics
   * @returns {Array<Object>} Active metrics
   */
  getActive() {
    return Array.from(this.metrics.entries()).map(([id, metric]) => ({
      id,
      ...metric,
      elapsedMs: (performance.now() - metric.startTime).toFixed(2),
    }));
  }

  /**
   * Clear all metrics
   */
  clear() {
    this.metrics.clear();
    this.debug.log('Cleared all metrics');
  }
}

/**
 * Global performance tracker instance
 */
export const perfTracker = new PerformanceTracker();

/**
 * Trace function execution with automatic timing
 * @param {Function} fn - Function to trace
 * @param {string} [name] - Function name for logging
 * @returns {Function} Wrapped function
 *
 * @example
 * const addQuadTraced = trace(addQuad, 'addQuad');
 * addQuadTraced(store, quad); // Automatically logs timing
 */
export function trace(fn, name = fn.name || 'anonymous') {
  return function (...args) {
    const id = perfTracker.start(name);

    try {
      const result = fn.apply(this, args);

      // Handle promises
      if (result && typeof result.then === 'function') {
        return result.finally(() => perfTracker.end(id));
      }

      perfTracker.end(id);
      return result;
    } catch (error) {
      perfTracker.end(id);
      throw error;
    }
  };
}

/**
 * Decorator for tracing class methods
 * @param {string} [name] - Method name override
 * @returns {Function} Decorator function
 *
 * @example
 * class MyStore {
 *   @traceMethod('addQuad')
 *   addQuad(quad) {
 *     // ... implementation
 *   }
 * }
 */
export function traceMethod(name = null) {
  return function (target, propertyKey, descriptor) {
    const originalMethod = descriptor.value;
    const methodName = name || `${target.constructor.name}.${propertyKey}`;

    descriptor.value = trace(originalMethod, methodName);

    return descriptor;
  };
}

/**
 * Format bytes to human-readable string
 * @param {number} bytes - Bytes
 * @returns {string} Formatted string
 */
export function formatBytes(bytes) {
  const sizes = ['Bytes', 'KB', 'MB', 'GB'];
  if (bytes === 0) return '0 Bytes';

  const i = Math.floor(Math.log(bytes) / Math.log(1024));
  return `${(bytes / Math.pow(1024, i)).toFixed(2)} ${sizes[i]}`;
}

/**
 * Get system information for debugging
 * @returns {Object} System information
 */
export function getSystemInfo() {
  const mem = process.memoryUsage();

  return {
    node: process.version,
    platform: process.platform,
    arch: process.arch,
    memory: {
      rss: formatBytes(mem.rss),
      heapTotal: formatBytes(mem.heapTotal),
      heapUsed: formatBytes(mem.heapUsed),
      external: formatBytes(mem.external),
    },
    uptime: `${(process.uptime() / 60).toFixed(2)} minutes`,
    env: {
      DEBUG: process.env.DEBUG || 'not set',
      NODE_ENV: process.env.NODE_ENV || 'development',
    },
  };
}

/**
 * Dump debug snapshot to console
 */
export function dumpDebugSnapshot() {
  console.log('\n=== UNRDF Debug Snapshot ===\n');
  console.log('System Info:', JSON.stringify(getSystemInfo(), null, 2));
  console.log('\nActive Performance Metrics:', JSON.stringify(perfTracker.getActive(), null, 2));
  console.log('\n=== End Snapshot ===\n');
}

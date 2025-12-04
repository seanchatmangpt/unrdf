/**
 * @file Module preloader for lazy-loaded knowledge engine modules
 * @module react-hooks/core/use-module-preloader
 *
 * @description
 * TRIZ #24 - Intermediary pattern: Provides intelligent preloading of knowledge engine
 * modules to reduce latency when features are needed. Uses a caching layer to ensure
 * modules are only loaded once and are immediately available when requested.
 *
 * Key features:
 * - Parallel module preloading for performance
 * - Module caching to prevent duplicate loads
 * - Priority-based preloading strategies
 * - React hook integration with loading states
 * - Memory-efficient with cache clearing capability
 *
 * @example
 * ```jsx
 * import { useModulePreloader, PRELOAD_MODULES } from 'unrdf/react-hooks/core';
 *
 * function App() {
 *   const { loaded, loading, progress, errors } = useModulePreloader([
 *     PRELOAD_MODULES.federation,
 *     PRELOAD_MODULES.optimizer
 *   ]);
 *
 *   if (loading) return <div>Loading {Math.round(progress * 100)}%...</div>;
 *   return <KnowledgeEngineApp />;
 * }
 * ```
 */

import { useState, useEffect, useCallback, useRef } from 'react';

/**
 * Module cache for storing loaded modules
 * @type {Map<string, Object>}
 */
const moduleCache = new Map();

/**
 * Promise cache for in-flight module loads
 * @type {Map<string, Promise<Object>>}
 */
const preloadPromises = new Map();

/**
 * Load timestamps for cache invalidation
 * @type {Map<string, number>}
 */
const loadTimestamps = new Map();

/**
 * Predefined module paths for common knowledge engine components
 * @constant {Object.<string, string>}
 */
export const PRELOAD_MODULES = {
  /** Federation coordinator for distributed knowledge graphs */
  federation: '../../knowledge-engine/federation/federation-coordinator.mjs',

  /** Dark matter optimizer for query performance */
  optimizer: '../../knowledge-engine/dark-matter/optimizer.mjs',

  /** Streaming subscription manager for real-time updates */
  streaming: '../../knowledge-engine/streaming/subscription-manager.mjs',

  /** Consensus manager for distributed agreement */
  consensus: '../../knowledge-engine/federation/consensus-manager.mjs',

  /** Data replication for distributed stores */
  replication: '../../knowledge-engine/federation/data-replication.mjs',

  /** Knowledge hook manager for hook execution */
  hookManager: '../../knowledge-engine/knowledge-hook-manager.mjs',

  /** SHACL validation engine */
  shacl: '../../knowledge-engine/validators/shacl-validator.mjs',

  /** SPARQL query engine */
  sparql: '../../knowledge-engine/sparql/sparql-engine.mjs',

  /** Edge case handler for error scenarios */
  edgeCases: '../../knowledge-engine/utils/edge-case-handler.mjs',

  /** Memory manager for resource optimization */
  memoryManager: '../../knowledge-engine/utils/memory-manager.mjs',
};

/**
 * Preload priority levels
 * @constant {Object.<string, number>}
 */
export const PRELOAD_PRIORITY = {
  /** Critical modules needed immediately */
  CRITICAL: 1,
  /** High priority modules */
  HIGH: 2,
  /** Normal priority modules */
  NORMAL: 3,
  /** Low priority background loading */
  LOW: 4,
};

/**
 * Default preload groups by use case
 * @constant {Object.<string, string[]>}
 */
export const PRELOAD_GROUPS = {
  /** Essential modules for basic operation */
  essential: [PRELOAD_MODULES.hookManager, PRELOAD_MODULES.edgeCases],

  /** Modules for distributed operations */
  distributed: [PRELOAD_MODULES.federation, PRELOAD_MODULES.consensus, PRELOAD_MODULES.replication],

  /** Modules for query operations */
  query: [PRELOAD_MODULES.sparql, PRELOAD_MODULES.optimizer],

  /** Modules for real-time features */
  realtime: [PRELOAD_MODULES.streaming],

  /** All validation modules */
  validation: [PRELOAD_MODULES.shacl],

  /** All available modules */
  all: Object.values(PRELOAD_MODULES),
};

/**
 * Preload a single module and cache it
 * @param {string} modulePath - Path to the module
 * @param {Object} [options={}] - Preload options
 * @param {number} [options.timeout=10000] - Load timeout in ms
 * @param {boolean} [options.forceReload=false] - Force reload even if cached
 * @returns {Promise<Object>} Loaded module
 */
export async function preloadModule(modulePath, options = {}) {
  const { timeout = 10000, forceReload = false } = options;

  // Return cached module if available and not forcing reload
  if (!forceReload && moduleCache.has(modulePath)) {
    return moduleCache.get(modulePath);
  }

  // Return existing promise if load is in progress
  if (preloadPromises.has(modulePath)) {
    return preloadPromises.get(modulePath);
  }

  // Create load promise with timeout
  const loadPromise = new Promise((resolve, reject) => {
    const timeoutId = setTimeout(() => {
      preloadPromises.delete(modulePath);
      reject(new Error(`Module load timeout: ${modulePath}`));
    }, timeout);

    import(modulePath)
      .then(mod => {
        clearTimeout(timeoutId);
        moduleCache.set(modulePath, mod);
        loadTimestamps.set(modulePath, Date.now());
        preloadPromises.delete(modulePath);
        resolve(mod);
      })
      .catch(err => {
        clearTimeout(timeoutId);
        preloadPromises.delete(modulePath);
        reject(err);
      });
  });

  preloadPromises.set(modulePath, loadPromise);
  return loadPromise;
}

/**
 * Preload multiple modules in parallel
 * @param {string[]} moduleList - Array of module paths to preload
 * @param {Object} [options={}] - Preload options
 * @param {number} [options.concurrency=5] - Max concurrent loads
 * @param {Function} [options.onProgress] - Progress callback (loaded, total)
 * @param {boolean} [options.stopOnError=false] - Stop on first error
 * @returns {Promise<Object>} Results object with modules and errors
 */
export async function preloadModules(moduleList, options = {}) {
  const { concurrency = 5, onProgress, stopOnError = false } = options;

  const results = {
    modules: new Map(),
    errors: new Map(),
    loaded: 0,
    failed: 0,
  };

  if (!moduleList || moduleList.length === 0) {
    return results;
  }

  // Process in batches for controlled concurrency
  const batches = [];
  for (let i = 0; i < moduleList.length; i += concurrency) {
    batches.push(moduleList.slice(i, i + concurrency));
  }

  let loadedCount = 0;

  for (const batch of batches) {
    const batchPromises = batch.map(async path => {
      try {
        const mod = await preloadModule(path);
        results.modules.set(path, mod);
        results.loaded++;
        loadedCount++;

        if (onProgress) {
          onProgress(loadedCount, moduleList.length);
        }

        return { path, success: true, module: mod };
      } catch (error) {
        results.errors.set(path, error);
        results.failed++;
        loadedCount++;

        if (onProgress) {
          onProgress(loadedCount, moduleList.length);
        }

        if (stopOnError) {
          throw error;
        }

        return { path, success: false, error };
      }
    });

    await Promise.all(batchPromises);
  }

  return results;
}

/**
 * Get a cached module synchronously
 * @param {string} modulePath - Module path
 * @returns {Object|undefined} Cached module or undefined
 */
export function getModule(modulePath) {
  return moduleCache.get(modulePath);
}

/**
 * Check if a module is loaded
 * @param {string} modulePath - Module path
 * @returns {boolean} True if module is cached
 */
export function isModuleLoaded(modulePath) {
  return moduleCache.has(modulePath);
}

/**
 * Check if a module load is in progress
 * @param {string} modulePath - Module path
 * @returns {boolean} True if loading
 */
export function isModuleLoading(modulePath) {
  return preloadPromises.has(modulePath);
}

/**
 * Get the load timestamp for a module
 * @param {string} modulePath - Module path
 * @returns {number|undefined} Unix timestamp or undefined
 */
export function getModuleLoadTime(modulePath) {
  return loadTimestamps.get(modulePath);
}

/**
 * Clear a specific module from cache
 * @param {string} modulePath - Module path to clear
 */
export function clearModule(modulePath) {
  moduleCache.delete(modulePath);
  loadTimestamps.delete(modulePath);
  preloadPromises.delete(modulePath);
}

/**
 * Clear all modules from cache
 */
export function clearModuleCache() {
  moduleCache.clear();
  preloadPromises.clear();
  loadTimestamps.clear();
}

/**
 * Get cache statistics
 * @returns {Object} Cache statistics
 */
export function getCacheStats() {
  return {
    cachedModules: moduleCache.size,
    loadingModules: preloadPromises.size,
    moduleList: Array.from(moduleCache.keys()),
    loadingList: Array.from(preloadPromises.keys()),
    timestamps: Object.fromEntries(loadTimestamps),
  };
}

/**
 * Hook return value type
 * @typedef {Object} UseModulePreloaderResult
 * @property {boolean} loaded - True when all modules are loaded
 * @property {boolean} loading - True while loading
 * @property {number} progress - Progress value 0-1
 * @property {number} loadedCount - Number of loaded modules
 * @property {number} totalCount - Total modules to load
 * @property {Map<string, Error>} errors - Map of load errors
 * @property {Function} getModule - Get a cached module
 * @property {Function} reload - Force reload all modules
 * @property {Function} clear - Clear cache
 */

/**
 * React hook for preloading knowledge engine modules
 *
 * @param {string[]} [modules=Object.values(PRELOAD_MODULES)] - Modules to preload
 * @param {Object} [options={}] - Hook options
 * @param {boolean} [options.autoLoad=true] - Start loading on mount
 * @param {number} [options.concurrency=5] - Max concurrent loads
 * @param {Function} [options.onComplete] - Callback when loading completes
 * @param {Function} [options.onError] - Callback on load error
 * @returns {UseModulePreloaderResult} Hook result
 *
 * @example
 * ```jsx
 * const { loaded, progress, errors } = useModulePreloader(
 *   [PRELOAD_MODULES.federation, PRELOAD_MODULES.streaming],
 *   { onComplete: () => console.log('Modules ready') }
 * );
 * ```
 */
export function useModulePreloader(modules = Object.values(PRELOAD_MODULES), options = {}) {
  const { autoLoad = true, concurrency = 5, onComplete, onError } = options;

  // State
  const [loaded, setLoaded] = useState(false);
  const [loading, setLoading] = useState(false);
  const [progress, setProgress] = useState(0);
  const [loadedCount, setLoadedCount] = useState(0);
  const [errors, setErrors] = useState(new Map());

  // Refs for stable references
  const mountedRef = useRef(true);
  const modulesRef = useRef(modules);
  const totalCount = modules.length;

  /**
   * Load all modules
   */
  const load = useCallback(async () => {
    if (!mountedRef.current) return;

    setLoading(true);
    setProgress(0);
    setLoadedCount(0);
    setErrors(new Map());

    try {
      const results = await preloadModules(modulesRef.current, {
        concurrency,
        onProgress: (loaded, total) => {
          if (mountedRef.current) {
            setProgress(loaded / total);
            setLoadedCount(loaded);
          }
        },
      });

      if (mountedRef.current) {
        setErrors(results.errors);
        setLoaded(results.failed === 0);
        setLoading(false);

        if (results.failed === 0 && onComplete) {
          onComplete(results);
        } else if (results.failed > 0 && onError) {
          onError(results.errors);
        }
      }
    } catch (err) {
      if (mountedRef.current) {
        setLoading(false);
        if (onError) {
          onError(err);
        }
      }
    }
  }, [concurrency, onComplete, onError]);

  /**
   * Reload all modules
   */
  const reload = useCallback(async () => {
    clearModuleCache();
    setLoaded(false);
    await load();
  }, [load]);

  /**
   * Clear the cache
   */
  const clear = useCallback(() => {
    clearModuleCache();
    setLoaded(false);
    setProgress(0);
    setLoadedCount(0);
    setErrors(new Map());
  }, []);

  // Update modules ref when prop changes
  useEffect(() => {
    modulesRef.current = modules;
  }, [modules]);

  // Auto-load on mount
  useEffect(() => {
    mountedRef.current = true;

    if (autoLoad) {
      load();
    }

    return () => {
      mountedRef.current = false;
    };
  }, [autoLoad, load]);

  return {
    loaded,
    loading,
    progress,
    loadedCount,
    totalCount,
    errors,
    getModule,
    reload,
    clear,
  };
}

/**
 * Utility: Check if essential modules are loaded
 * @returns {boolean} True if essential modules are loaded
 */
export function areEssentialModulesLoaded() {
  return PRELOAD_GROUPS.essential.every(isModuleLoaded);
}

/**
 * Utility: Preload essential modules (blocking)
 * @returns {Promise<Object>} Preload results
 */
export async function preloadEssentialModules() {
  return preloadModules(PRELOAD_GROUPS.essential);
}

/**
 * Utility: Get all loaded modules as an object
 * @returns {Object} Object with module paths as keys
 */
export function getAllLoadedModules() {
  const result = {};
  for (const [path, mod] of moduleCache.entries()) {
    result[path] = mod;
  }
  return result;
}

export default useModulePreloader;

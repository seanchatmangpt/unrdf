/**
 * @fileoverview useKnowledgeEngine - Main React hook for UNRDF Knowledge Engine
 * @module react-hooks/core/useKnowledgeEngine
 *
 * @description
 * Primary hook for initializing and managing the UNRDF Knowledge Engine in React applications.
 * Provides full access to RDF operations, SPARQL queries, knowledge hooks, and transactions.
 *
 * @example
 * ```jsx
 * import { useKnowledgeEngine } from 'unrdf/react-hooks';
 *
 * async function MyComponent() {
 *   const { engine, store, query, loading, error } = useKnowledgeEngine({
 *     enableKnowledgeHooks: true,
 *     enableObservability: true
 *   });
 *
 *   useEffect(() => {
 *     if (!loading && engine) {
 *       query('SELECT * WHERE { ?s ?p ?o } LIMIT 10')
 *         .then(results => console.log(results));
 *     }
 *   }, [loading, engine]);
 *
 *   return <div>Engine loaded: {!loading}</div>;
 * }
 * ```
 */

import { useState, useEffect, useCallback, useRef } from 'react';
import { z } from 'zod';
import { KnowledgeHookManager } from '../../knowledge-engine/knowledge-hook-manager.mjs';
import { createStore } from '@unrdf/oxigraph';

/**
 * Zod schema for URL validation (endpoints)
 * Validates URL format to prevent injection attacks
 */
const UrlSchema = z
  .string()
  .url('Invalid URL format')
  .max(2048, 'URL exceeds maximum length')
  .optional();

/**
 * Zod schema for base path validation
 * Validates file path to prevent path traversal attacks
 */
const BasePathSchema = z
  .string()
  .max(4096, 'Base path exceeds maximum length')
  .refine(path => !path.includes('..'), 'Base path cannot contain path traversal sequences')
  .optional();

/**
 * Zod schema for lockchain options validation
 */
const LockchainOptionsSchema = z
  .object({
    enabled: z.boolean().optional(),
    threshold: z.number().int().positive().max(1000).optional(),
    timeout: z.number().positive().max(300000).optional(),
  })
  .passthrough()
  .optional();

/**
 * Zod schema for useKnowledgeEngine options validation
 * Prevents XSS/injection by validating all configuration inputs
 */
const UseKnowledgeEngineOptionsSchema = z
  .object({
    /** Base path for file resolution - validated for path traversal */
    basePath: BasePathSchema,
    /** Enable knowledge hook execution */
    enableKnowledgeHooks: z.boolean().optional().default(true),
    /** Enable OpenTelemetry observability */
    enableObservability: z.boolean().optional().default(true),
    /** Enable strict error handling */
    strictMode: z.boolean().optional().default(false),
    /** Initial RDF store (n3 Store instance) */
    initialStore: z.instanceof(Store).nullable().optional(),
    /** Lockchain configuration options */
    lockchainOptions: LockchainOptionsSchema,
    /** Automatically initialize engine on mount */
    autoInit: z.boolean().optional().default(true),
    /** Optional endpoint URL for remote knowledge engine */
    endpoint: UrlSchema,
  })
  .strict();

/**
 * Configuration options for useKnowledgeEngine hook
 * @typedef {Object} UseKnowledgeEngineOptions
 * @property {string} [basePath] - Base path for file resolution
 * @property {boolean} [enableKnowledgeHooks=true] - Enable knowledge hook execution
 * @property {boolean} [enableObservability=true] - Enable OpenTelemetry observability
 * @property {boolean} [strictMode=false] - Enable strict error handling
 * @property {Store} [initialStore] - Initial RDF store
 * @property {Object} [lockchainOptions] - Lockchain configuration
 * @property {boolean} [autoInit=true] - Automatically initialize engine on mount
 */

/**
 * Hook return value
 * @typedef {Object} UseKnowledgeEngineResult
 * @property {KnowledgeHookManager} engine - Knowledge engine instance
 * @property {Store} store - RDF store
 * @property {Function} query - Execute SPARQL query
 * @property {Function} addKnowledgeHook - Register a knowledge hook
 * @property {Function} removeKnowledgeHook - Remove a knowledge hook
 * @property {Function} applyTransaction - Apply transaction to store
 * @property {boolean} loading - Loading state
 * @property {Error|null} error - Error state
 * @property {Function} reinitialize - Reinitialize engine
 * @property {Object} stats - Engine statistics
 */

/**
 * Main hook for UNRDF Knowledge Engine integration
 *
 * @param {UseKnowledgeEngineOptions} [options={}] - Configuration options
 * @returns {UseKnowledgeEngineResult} Hook result with engine instance and operations
 */
export async function useKnowledgeEngine(options = {}) {
  // Validate options with Zod schema to prevent XSS/injection
  const validatedOptions = UseKnowledgeEngineOptionsSchema.parse(options);

  const {
    basePath = process.cwd(),
    enableKnowledgeHooks = true,
    enableObservability = true,
    strictMode = false,
    initialStore = null,
    lockchainOptions = {},
    autoInit = true,
  } = validatedOptions;

  // State
  const [engine, setEngine] = useState(null);
  const [store, setStore] = useState(initialStore || await createStore());
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [stats, setStats] = useState({});

  // Refs for stable references
  const engineRef = useRef(null);
  const mountedRef = useRef(true);

  /**
   * Initialize the knowledge engine
   */
  const initialize = useCallback(async () => {
    if (!mountedRef.current) return;

    try {
      setLoading(true);
      setError(null);

      // Create knowledge hook manager
      const manager = new KnowledgeHookManager({
        basePath,
        enableKnowledgeHooks,
        enableObservability,
        strictMode,
        lockchainOptions,
      });

      engineRef.current = manager;

      if (mountedRef.current) {
        setEngine(manager);
        setLoading(false);
      }
    } catch (err) {
      if (mountedRef.current) {
        setError(err);
        setLoading(false);
      }
      console.error('[useKnowledgeEngine] Initialization failed:', err);
    }
  }, [basePath, enableKnowledgeHooks, enableObservability, strictMode]);

  /**
   * Execute SPARQL query
   */
  const query = useCallback(
    async (sparql, queryOptions = {}) => {
      if (!engineRef.current || !store) {
        throw new Error('[useKnowledgeEngine] Engine not initialized');
      }

      try {
        return await engineRef.current.query(store, sparql, queryOptions);
      } catch (err) {
        console.error('[useKnowledgeEngine] Query failed:', err);
        throw err;
      }
    },
    [store]
  );

  /**
   * Add a knowledge hook
   */
  const addKnowledgeHook = useCallback(hook => {
    if (!engineRef.current) {
      throw new Error('[useKnowledgeEngine] Engine not initialized');
    }
    return engineRef.current.addKnowledgeHook(hook);
  }, []);

  /**
   * Remove a knowledge hook
   */
  const removeKnowledgeHook = useCallback(hookName => {
    if (!engineRef.current) {
      throw new Error('[useKnowledgeEngine] Engine not initialized');
    }
    return engineRef.current.removeKnowledgeHook(hookName);
  }, []);

  /**
   * Apply transaction to store
   */
  const applyTransaction = useCallback(
    async (delta, transactionOptions = {}) => {
      if (!engineRef.current || !store) {
        throw new Error('[useKnowledgeEngine] Engine not initialized');
      }

      try {
        const result = await engineRef.current.apply(store, delta, transactionOptions);

        // Update store state if transaction modified it
        if (result.store && result.store !== store) {
          setStore(result.store);
        }

        return result;
      } catch (err) {
        console.error('[useKnowledgeEngine] Transaction failed:', err);
        throw err;
      }
    },
    [store]
  );

  /**
   * Reinitialize the engine
   */
  const reinitialize = useCallback(async () => {
    if (engineRef.current) {
      engineRef.current.clearCaches?.();
      engineRef.current = null;
    }
    setEngine(null);
    setStore(initialStore || await createStore());
    await initialize();
  }, [initialize, initialStore]);

  /**
   * Update stats periodically
   */
  const updateStats = useCallback(() => {
    if (engineRef.current) {
      const currentStats = engineRef.current.getStats();
      setStats(currentStats);
    }
  }, []);

  // Initialize on mount
  useEffect(() => {
    mountedRef.current = true;

    if (autoInit) {
      initialize();
    }

    return () => {
      mountedRef.current = false;
      if (engineRef.current) {
        engineRef.current.clearCaches?.();
      }
    };
  }, [autoInit, initialize]);

  // Update stats periodically (every 5 seconds)
  useEffect(() => {
    if (!engine || loading) return;

    updateStats();
    const interval = setInterval(updateStats, 5000);

    return () => clearInterval(interval);
  }, [engine, loading, updateStats]);

  return {
    engine,
    store,
    query,
    addKnowledgeHook,
    removeKnowledgeHook,
    applyTransaction,
    loading,
    error,
    reinitialize,
    stats,
  };
}

/**
 * useQuery Composable - Reactive SPARQL Query Execution
 *
 * Execute SPARQL queries on RDF stores with reactive results.
 * Supports auto-execution on store changes and result memoization.
 *
 * @module composables/use-query
 */

import { ref, watch, onUnmounted } from 'vue';
import { executeQuery, executeSelect, executeConstruct, executeAsk } from '@unrdf/core';
import { z } from 'zod';

/**
 * Options schema for useQuery
 */
const UseQueryOptionsSchema = z
  .object({
    autoExecute: z.boolean().optional().default(false),
    watchStore: z.boolean().optional().default(false),
    memoize: z.boolean().optional().default(true),
  })
  .strict();

/**
 * Execute SPARQL queries with reactive results
 *
 * @param {import('vue').Ref<object> | object} storeRef - RDF store (ref or plain object)
 * @param {string} sparql - SPARQL query string
 * @param {object} [options={}] - Configuration options
 * @param {boolean} [options.autoExecute=false] - Execute query immediately
 * @param {boolean} [options.watchStore=false] - Re-execute on store changes
 * @param {boolean} [options.memoize=true] - Cache query results
 * @returns {{
 *   results: import('vue').Ref<any>,
 *   loading: import('vue').Ref<boolean>,
 *   error: import('vue').Ref<Error | null>,
 *   execute: () => Promise<void>,
 *   clear: () => void
 * }} Reactive query state and methods
 * @example
 * const { results, loading, execute } = useQuery(store, 'SELECT * WHERE { ?s ?p ?o }', { autoExecute: true })
 * watch(results, (data) => console.log('Query results:', data))
 */
export function useQuery(storeRef, sparql, options = {}) {
  const opts = UseQueryOptionsSchema.parse(options);

  // Reactive state
  const results = ref(null);
  const loading = ref(false);
  const error = ref(null);
  const memoCache = new Map();

  // Unwrap store ref if needed
  const getStore = () => {
    return storeRef?.value ?? storeRef;
  };

  /**
   * Execute the SPARQL query
   */
  async function execute() {
    const store = getStore();
    if (!store) {
      error.value = new Error('No store provided');
      return;
    }

    loading.value = true;
    error.value = null;

    try {
      // Check memo cache
      if (opts.memoize && memoCache.has(sparql)) {
        results.value = memoCache.get(sparql);
        loading.value = false;
        return;
      }

      // Execute query based on type
      let queryResults;
      const trimmed = sparql.trim().toUpperCase();

      if (trimmed.startsWith('SELECT')) {
        queryResults = await executeSelect(store, sparql);
      } else if (trimmed.startsWith('CONSTRUCT')) {
        queryResults = await executeConstruct(store, sparql);
      } else if (trimmed.startsWith('ASK')) {
        queryResults = await executeAsk(store, sparql);
      } else {
        queryResults = await executeQuery(store, sparql);
      }

      results.value = queryResults;

      // Cache results
      if (opts.memoize) {
        memoCache.set(sparql, queryResults);
      }
    } catch (err) {
      error.value = err;
      results.value = null;
    } finally {
      loading.value = false;
    }
  }

  /**
   * Clear results and cache
   */
  function clear() {
    results.value = null;
    error.value = null;
    memoCache.clear();
  }

  // Auto-execute on mount
  if (opts.autoExecute) {
    execute();
  }

  // Watch store for changes
  let unwatchStore = null;
  if (opts.watchStore && storeRef?.value !== undefined) {
    unwatchStore = watch(
      storeRef,
      () => {
        // Clear cache and re-execute
        memoCache.clear();
        execute();
      },
      { deep: true }
    );
  }

  // Cleanup on unmount
  onUnmounted(() => {
    if (unwatchStore) {
      unwatchStore();
    }
    memoCache.clear();
  });

  return {
    results,
    loading,
    error,
    execute,
    clear,
  };
}

/**
 * useGraph Composable - Reactive RDF Graph Store
 *
 * Creates a reactive RDF graph store for Vue 3 applications.
 * Automatically fetches and manages RDF data with reactive state.
 *
 * @module composables/use-graph
 */

import { ref, computed, onUnmounted } from 'vue';
import { createStore } from '@unrdf/core';
import { createBrowserRDFStore } from '@unrdf/browser';
import { z } from 'zod';

/**
 * Options schema for useGraph
 */
const UseGraphOptionsSchema = z
  .object({
    autoRefresh: z.boolean().optional().default(false),
    fetchInterval: z.number().optional().default(60000),
    headers: z.record(z.string()).optional(),
    enableIndexedDB: z.boolean().optional().default(false),
  })
  .strict();

/**
 * Create a reactive RDF graph store
 *
 * @param {string | null} graphUrl - URL to fetch RDF graph from (null for local-only)
 * @param {object} [options={}] - Configuration options
 * @param {boolean} [options.autoRefresh=false] - Auto-refresh graph at interval
 * @param {number} [options.fetchInterval=60000] - Refresh interval in ms
 * @param {Record<string, string>} [options.headers] - Custom fetch headers
 * @param {boolean} [options.enableIndexedDB=false] - Use IndexedDB for persistence
 * @returns {{
 *   store: import('vue').Ref<object>,
 *   loading: import('vue').Ref<boolean>,
 *   error: import('vue').Ref<Error | null>,
 *   quads: import('vue').ComputedRef<Array>,
 *   refresh: () => Promise<void>
 * }} Reactive graph state and methods
 * @example
 * const { store, loading, error, quads, refresh } = useGraph('https://example.org/data.ttl')
 * watch(quads, (newQuads) => console.log('Graph updated:', newQuads.length))
 */
export function useGraph(graphUrl, options = {}) {
  const opts = UseGraphOptionsSchema.parse(options);

  // Reactive state
  const store = ref(opts.enableIndexedDB ? null : createStore());
  const loading = ref(false);
  const error = ref(null);
  let intervalId = null;

  // Computed quads array
  const quads = computed(() => {
    if (!store.value) return [];
    try {
      const allQuads = [];
      for (const quad of store.value.match()) {
        allQuads.push(quad);
      }
      return allQuads;
    } catch (err) {
      error.value = err;
      return [];
    }
  });

  /**
   * Fetch and load graph data
   */
  async function refresh() {
    if (!graphUrl) {
      error.value = new Error('No graph URL provided');
      return;
    }

    loading.value = true;
    error.value = null;

    try {
      // Initialize IndexedDB store if needed
      if (opts.enableIndexedDB && !store.value) {
        store.value = await createBrowserRDFStore({ dbName: 'unrdf-graph' });
      }

      // Fetch RDF data
      const response = await fetch(graphUrl, {
        headers: {
          Accept: 'text/turtle, application/rdf+xml, application/ld+json',
          ...opts.headers,
        },
      });

      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      // Parse RDF (simplified - would use a real parser in production)
      // For now, this is a placeholder that triggers reactivity
      // In production, integrate with rdf-parse or N3.js
      // TODO: Parse response based on content-type
      await response.text();
      if (!store.value) {
        store.value = createStore();
      }

      // Trigger reactivity by reassigning
      store.value = { ...store.value };
    } catch (err) {
      error.value = err;
    } finally {
      loading.value = false;
    }
  }

  // Auto-refresh setup
  if (opts.autoRefresh && graphUrl) {
    intervalId = setInterval(refresh, opts.fetchInterval);
    // Initial fetch
    refresh();
  }

  // Cleanup on unmount
  onUnmounted(() => {
    if (intervalId) {
      clearInterval(intervalId);
    }
  });

  return {
    store,
    loading,
    error,
    quads,
    refresh,
  };
}

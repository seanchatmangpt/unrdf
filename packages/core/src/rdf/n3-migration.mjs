/**
 * @file N3 Migration Module - Backward compatibility for N3.Store
 * @module @unrdf/core/rdf/n3-migration
 *
 * This module provides backward compatibility for code using N3.Store
 * by detecting N3 stores and converting them to Oxigraph stores.
 *
 * **Why this exists:**
 * - Some external code may pass N3.Store instances to our APIs
 * - This module ensures 100% backward compatibility
 * - Internal code should NEVER import N3 directly
 *
 * **Usage:**
 * ```javascript
 * import { ensureOxigraphStore } from '@unrdf/core/rdf/n3-migration';
 *
 * export function myFunction(store) {
 *   // Convert N3.Store to Oxigraph if needed
 *   const oxiStore = ensureOxigraphStore(store);
 *   // ... use oxiStore
 * }
 * ```
 */

import { N3Store as Store, N3DataFactory } from './n3-justified-only.mjs';
import { createStore } from '@unrdf/oxigraph';

// Extract DataFactory methods from N3DataFactory
const { namedNode, literal, blankNode, variable, defaultGraph, quad } = N3DataFactory;

/**
 * Check if a store is an N3.Store instance
 * @param {any} store - Store to check
 * @returns {boolean} True if store is N3.Store
 *
 * @example
 * const n3Store = new N3.Store();
 * const isN3 = isN3Store(n3Store); // true
 *
 * const oxiStore = createStore();
 * const isN3Again = isN3Store(oxiStore); // false
 */
export function isN3Store(store) {
  if (!store) return false;

  // N3.Store has specific properties/methods
  return (
    store instanceof Store ||
    (typeof store.getQuads === 'function' &&
      typeof store.addQuad === 'function' &&
      typeof store.removeQuad === 'function' &&
      !store.query) // Oxigraph has query(), N3 doesn't
  );
}

/**
 * Convert N3.Store to Oxigraph Store
 * @param {Store} n3Store - N3.Store to convert
 * @returns {import('@unrdf/oxigraph').Store} Oxigraph store
 *
 * @throws {TypeError} If n3Store is not valid
 *
 * @example
 * const n3Store = new N3.Store();
 * n3Store.addQuad(quad(...));
 *
 * const oxiStore = convertN3ToOxigraph(n3Store);
 * // oxiStore now contains all quads from n3Store
 */
export function convertN3ToOxigraph(n3Store) {
  if (!n3Store) {
    throw new TypeError('n3Store is required');
  }

  if (!isN3Store(n3Store)) {
    throw new TypeError('n3Store must be an N3.Store instance');
  }

  const oxiStore = createStore();
  const quads = n3Store.getQuads();

  for (const quad of quads) {
    oxiStore.add(quad);
  }

  return oxiStore;
}

/**
 * Ensure store is Oxigraph (convert if N3)
 * @param {any} store - Store to check/convert
 * @returns {import('@unrdf/oxigraph').Store} Oxigraph store
 *
 * @throws {TypeError} If store is not valid
 *
 * @example
 * // Handles both N3 and Oxigraph stores
 * export function processStore(store) {
 *   const oxiStore = ensureOxigraphStore(store);
 *   return oxiStore.query('SELECT * WHERE { ?s ?p ?o }');
 * }
 */
export function ensureOxigraphStore(store) {
  if (!store) {
    throw new TypeError('store is required');
  }

  // Already Oxigraph - return as-is
  if (!isN3Store(store)) {
    return store;
  }

  // Convert N3 to Oxigraph
  return convertN3ToOxigraph(store);
}

/**
 * Re-export N3 DataFactory for backward compatibility ONLY
 *
 * **DEPRECATED:** New code should use @unrdf/oxigraph dataFactory
 *
 * @deprecated Use @unrdf/oxigraph dataFactory instead
 */
export { namedNode, literal, blankNode, variable, defaultGraph, quad };

/**
 * Create N3.Store for backward compatibility testing
 *
 * **INTERNAL USE ONLY:** Only for testing backward compatibility
 * Production code should never create N3 stores
 *
 * @returns {Store} N3.Store instance
 *
 * @example
 * // Only for testing N3 backward compat
 * const n3Store = createN3Store();
 * n3Store.addQuad(quad(...));
 * const oxiStore = ensureOxigraphStore(n3Store);
 */
export function createN3Store() {
  return new Store();
}

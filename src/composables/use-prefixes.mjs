/**
 * @fileoverview usePrefixes composable - opinionated prefix registry helpers
 */

import { useStoreContext } from '../context/index.mjs';

function normaliseEntries(entries) {
  if (!entries) return [];

  if (entries instanceof Map) {
    return [...entries.entries()];
  }

  if (Array.isArray(entries)) {
    return entries.map(([prefix, iri]) => [prefix, iri]);
  }

  if (typeof entries === 'object') {
    return Object.entries(entries);
  }

  throw new TypeError('[usePrefixes] Expected Map, array of tuples, or plain object');
}

/**
 * Create a shared prefix registry attached to the active store context.
 *
 * @param {Map|Array|Object} [initial] - Initial prefix entries to register.
 * @returns {Object} Prefix helper API.
 */
export function usePrefixes(initial) {
  const storeContext = useStoreContext();
  if (!storeContext.prefixRegistry) {
    storeContext.prefixRegistry = new Map();
  }

  const registry = storeContext.prefixRegistry;

  if (initial) {
    for (const [prefix, iri] of normaliseEntries(initial)) {
      registry.set(prefix, iri);
    }
  }

  return {
    /**
     * Register one or more prefixes.
     * @param {Map|Array|Object|string} prefix - Prefix entry or record.
     * @param {string} [iri] - IRI when prefix is a string.
     */
    register(prefix, iri) {
      if (typeof prefix === 'string') {
        if (!iri) {
          throw new TypeError('[usePrefixes] IRI is required when prefix is a string');
        }
        registry.set(prefix, iri);
        return this;
      }

      for (const [key, value] of normaliseEntries(prefix)) {
        registry.set(key, value);
      }
      return this;
    },

    /**
     * Expand a CURIE into a full IRI.
     * @param {string} curie - Compact IRI (e.g., `ex:Person`).
     * @returns {string|null}
     */
    expand(curie) {
      if (typeof curie !== 'string') {
        throw new TypeError('[usePrefixes] CURIE must be a string');
      }
      const [prefix, remainder] = curie.split(':');
      if (!prefix || remainder === undefined) {
        return null;
      }
      const base = registry.get(prefix);
      return base ? `${base}${remainder}` : null;
    },

    /**
     * Shrink an IRI using the registered prefixes.
     * @param {string} iri - Absolute IRI.
     * @returns {string|null}
     */
    shrink(iri) {
      if (typeof iri !== 'string') {
        throw new TypeError('[usePrefixes] IRI must be a string');
      }
      for (const [prefix, base] of registry.entries()) {
        if (iri.startsWith(base)) {
          return `${prefix}:${iri.slice(base.length)}`;
        }
      }
      return null;
    },

    /**
     * List the current prefix registry as a plain object.
     * @returns {Object}
     */
    list() {
      return Object.fromEntries(registry.entries());
    },

    /**
     * Clear the registry.
     * @returns {void}
     */
    clear() {
      registry.clear();
    },
  };
}

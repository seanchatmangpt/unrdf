/**
 * @fileoverview useCanon composable - canonicalization and isomorphism operations
 *
 * This composable provides canonicalization and isomorphism checking capabilities.
 * It enforces the "One Canonicalization Rule" - URDNA2015 is the only method.
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import * as rdfCanonizeModule from 'rdf-canonize';
import { useStoreContext } from '../context/index.mjs';
import { createStore } from '@unrdf/core';

const rdfCanonize = rdfCanonizeModule.default || rdfCanonizeModule;

/**
 * Create a canonicalization composable
 *
 * @param {Object} [options] - Canonicalization options
 * @param {number} [options.timeoutMs=30000] - Canonicalization timeout
 * @param {Function} [options.onMetric] - Metrics callback
 * @returns {Object} Canonicalization interface
 *
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 *
 * runApp(() => {
 *   const canon = useCanon();
 *
 *   // Canonicalize the context store
 *   const canonical = canon.canonicalize(store);
 *
 *   // Check if two stores are isomorphic
 *   const isIsomorphic = canon.isIsomorphic(store1, store2);
 * });
 *
 * @throws {Error} If store context is not initialized
 */
export function useCanon(options = {}) {
  const { _timeoutMs = 30_000, _onMetric } = options;

  // Get the store context - uses canonicalization methods
  const storeContext = useStoreContext();

  return {
    /**
     * Canonicalize the context store using URDNA2015 (READER operation)
     * @param {Object} [options] - Canonicalization options
     * @param {string} [options.algorithm='URDNA2015'] - Canonicalization algorithm
     * @param {number} [options.timeoutMs=30000] - Canonicalization timeout
     * @param {Function} [options.onMetric] - Metrics callback
     * @returns {Promise<string>} Canonical N-Quads string
     *
     * @example
     * const canon = useCanon();
     * const canonical = canon.canonicalize();
     * console.log("Canonical form:", canonical);
     *
     * @note This is a READER operation - use sparingly
     */
    canonicalize(options = {}) {
      return storeContext.canonicalize(options);
    },

    /**
     * Check if two stores are isomorphic (READER operation)
     * @param {Store|Object} store1 - First store
     * @param {Store|Object} store2 - Second store
     * @param {Object} [options] - Isomorphism options
     * @returns {Promise<boolean>} True if stores are isomorphic
     *
     * @example
     * const canon = useCanon();
     * const isIsomorphic = canon.isIsomorphic(store1, store2);
     * if (isIsomorphic) {
     *   console.log("Stores are logically equivalent");
     * }
     *
     * @note This is a READER operation - use sparingly
     */
    isIsomorphic(store1, store2, options = {}) {
      return storeContext.isIsomorphic(store1, store2, options);
    },

    /**
     * Get a canonical hash of the context store using SHA-256 (READER operation)
     * @param {Object} [options] - Hash options
     * @param {string} [options.algorithm='SHA-256'] - Hash algorithm
     * @returns {Promise<string>} Canonical hash string
     *
     * @example
     * const canon = useCanon();
     * const hash = canon.hash();
     * console.log("Store hash:", hash);
     *
     * @note This is a READER operation - use sparingly
     */
    hash(options = {}) {
      return storeContext.hash(options);
    },

    /**
     * Check if multiple stores are all isomorphic
     * @param {Array<Store|Object>} stores - Array of stores to check
     * @returns {boolean} True if all stores are isomorphic
     *
     * @example
     * const allIsomorphic = canon.allIsomorphic([store1, store2, store3]);
     * if (allIsomorphic) {
     *   console.log("All stores are equivalent");
     * }
     */
    allIsomorphic(stores) {
      if (stores.length < 2) return true;

      const first = stores[0].store || stores[0];
      for (let i = 1; i < stores.length; i++) {
        const current = stores[i].store || stores[i];
        // Use canonicalization to check isomorphism
        const canon1 = this.canonicalize(first);
        const canon2 = this.canonicalize(current);
        if (canon1 !== canon2) {
          return false;
        }
      }
      return true;
    },

    /**
     * Find stores that are isomorphic to a reference store
     * @param {Store|Object} referenceStore - Reference store
     * @param {Array<Store|Object>} stores - Array of stores to check
     * @returns {Array<{store: Store, index: number}>} Array of isomorphic stores
     *
     * @example
     * const isomorphic = canon.findIsomorphic(referenceStore, [store1, store2, store3]);
     * console.log(`Found ${isomorphic.length} isomorphic stores`);
     */
    findIsomorphic(referenceStore, stores) {
      const reference = referenceStore.store || referenceStore;
      const refCanon = this.canonicalize(reference);
      const results = [];

      for (const [i, store] of stores.entries()) {
        const current = store.store || store;
        const currentCanon = this.canonicalize(current);
        if (refCanon === currentCanon) {
          results.push({ store: store, index: i });
        }
      }

      return results;
    },

    /**
     * Group stores by isomorphism
     * @param {Array<Store|Object>} stores - Array of stores to group
     * @returns {Array<Array<{store: Store, index: number}>>} Groups of isomorphic stores
     *
     * @example
     * const groups = canon.groupByIsomorphism([store1, store2, store3, store4]);
     * console.log(`Found ${groups.length} distinct groups`);
     */
    groupByIsomorphism(stores) {
      const groups = [];
      const processed = new Set();

      for (let i = 0; i < stores.length; i++) {
        if (processed.has(i)) continue;

        const current = stores[i].store || stores[i];
        const currentCanon = this.canonicalize(current);
        const group = [{ store: stores[i], index: i }];
        processed.add(i);

        for (let j = i + 1; j < stores.length; j++) {
          if (processed.has(j)) continue;

          const other = stores[j].store || stores[j];
          const otherCanon = this.canonicalize(other);
          if (currentCanon === otherCanon) {
            group.push({ store: stores[j], index: j });
            processed.add(j);
          }
        }

        groups.push(group);
      }

      return groups;
    },

    /**
     * Get canonical statistics for a store
     * @param {Store|Object} store - Store to analyze
     * @returns {Object} Canonical statistics
     *
     * @example
     * const stats = canon.getStats(store);
     * console.log(`Canonical size: ${stats.canonicalSize}, Hash: ${stats.hash}`);
     */
    getStats(store) {
      const storeInstance = store.store || store;
      const canonical = this.canonicalize(store);
      const hash = this.hash(store);

      return {
        originalSize: storeInstance.size,
        canonicalSize: canonical.length,
        hash,
        canonical,
      };
    },

    /**
     * Compare two stores and return detailed comparison
     * @param {Store|Object} store1 - First store
     * @param {Store|Object} store2 - Second store
     * @returns {Object} Detailed comparison result
     *
     * @example
     * const comparison = canon.compare(store1, store2);
     * console.log(`Isomorphic: ${comparison.isomorphic}, Hash match: ${comparison.hashMatch}`);
     */
    compare(store1, store2) {
      const s1 = store1.store || store1;
      const s2 = store2.store || store2;

      const canonical1 = this.canonicalize(store1);
      const canonical2 = this.canonicalize(store2);
      const hash1 = this.hash(store1);
      const hash2 = this.hash(store2);

      const isomorphic = canonical1 === canonical2;
      const hashMatch = hash1 === hash2;

      return {
        isomorphic,
        hashMatch,
        canonical1,
        canonical2,
        hash1,
        hash2,
        size1: s1.size,
        size2: s2.size,
        sizeDifference: s2.size - s1.size,
      };
    },

    /**
     * Create a canonical store from multiple stores (union + canonicalize)
     * @param {Array<Store|Object>} stores - Array of stores to merge
     * @param {Object} [options] - Canonicalization options
     * @returns {Object} New useGraph instance with canonical store
     *
     * @example
     * const canonical = canon.createCanonicalStore([store1, store2, store3]);
     * console.log(`Canonical store has ${canonical.size} triples`);
     */
    async createCanonicalStore(stores, options = {}) {
      const storeInstances = stores.map(s => s.store || s);
      // Create union by merging all quads into a new store
      const unionStore = createStore();
      for (const store of storeInstances) {
        for (const quad of store.match()) {
          unionStore.add(quad);
        }
      }
      const canonical = await this.canonicalize(unionStore, options);
      // Parse canonical N-Quads back into a Store
      const { Parser } = await import('n3');
      const parser = new Parser({ format: 'N-Quads' });
      const canonicalStore = createStore();
      const quads = parser.parse(canonical);
      for (const quad of quads) {
        canonicalStore.add(quad);
      }

      return {
        store: canonicalStore,
        canonical,
        hash: await this.hash(canonicalStore),
        size: canonicalStore.size,
      };
    },

    /**
     * Canonicalize synchronously (for small datasets)
     * @param {Store|Object} store - Store to canonicalize
     * @param {Object} [options] - Canonicalization options
     * @returns {string} Canonical N-Quads string
     *
     * @example
     * const canonical = canon.canonicalizeSync(store);
     */
    canonicalizeSync(store, options = {}) {
      const storeInstance = store.store || store;
      const { algorithm = 'URDNA2015' } = options;

      try {
        return rdfCanonize._canonizeSync(storeInstance, { algorithm });
      } catch (error) {
        // Fallback to async canonicalization (no sync alternative available)
        console.warn(`Synchronous canonicalization not supported: ${error.message}`);
        throw new Error(
          'Synchronous canonicalization is not available. Use canonicalize() instead.'
        );
      }
    },

    /**
     * Get canonicalization algorithms
     * @returns {Array<string>} Available algorithms
     *
     * @example
     * const algorithms = canon.getAlgorithms();
     * console.log("Available algorithms:", algorithms);
     */
    getAlgorithms() {
      return ['URDNA2015'];
    },

    /**
     * Check if canonicalization is supported
     * @returns {boolean} True if canonicalization is supported
     *
     * @example
     * if (canon.isSupported()) {
     *   const canonical = await canon.canonicalize(store);
     * }
     */
    isSupported() {
      try {
        return typeof rdfCanonize.canonize === 'function';
      } catch (error) {
        return false;
      }
    },

    /**
     * Get canonicalization statistics
     * @param {Store|Object} store - Store to analyze
     * @returns {Object} Canonicalization statistics
     *
     * @example
     * const stats = canon.getCanonicalizationStats(store);
     * console.log(`Original size: ${stats.originalSize}, Canonical size: ${stats.canonicalSize}`);
     */
    async getCanonicalizationStats(store) {
      const storeInstance = store.store || store;
      const startTime = performance.now();

      try {
        const canonical = await this.canonicalize(storeInstance);
        const endTime = performance.now();

        return {
          originalSize: storeInstance.size,
          canonicalSize: canonical.length,
          canonicalQuads: canonical.split('\n').filter(line => line.trim().endsWith('.')).length,
          duration: endTime - startTime,
          compressionRatio: canonical.length / (storeInstance.size * 100), // Rough estimate
          supported: this.isSupported(),
        };
      } catch (error) {
        return {
          originalSize: storeInstance.size,
          canonicalSize: 0,
          canonicalQuads: 0,
          duration: 0,
          compressionRatio: 0,
          supported: false,
          error: error.message,
        };
      }
    },
  };
}

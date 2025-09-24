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

import rdfCanonize from "rdf-canonize";
import { useStoreContext } from "../context/index.mjs";

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
 *   const canonical = await canon.canonicalize(store);
 *   
 *   // Check if two stores are isomorphic
 *   const isIsomorphic = await canon.isIsomorphic(store1, store2);
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useCanon(options = {}) {
  const {
    timeoutMs = 30_000,
    onMetric
  } = options;

  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;

  return {
    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return engine;
    },

    /**
     * Canonicalize a store using URDNA2015
     * @param {Store|Object} store - Store to canonicalize
     * @param {Object} [options] - Canonicalization options
     * @param {string} [options.algorithm='URDNA2015'] - Canonicalization algorithm
     * @param {boolean} [options.synchronous=false] - Use synchronous canonicalization
     * @returns {Promise<string>} Canonical N-Quads string
     * 
     * @example
     * const canonical = await canon.canonicalize(store);
     * console.log("Canonical form:", canonical);
     * 
     * // With options
     * const canonicalSync = await canon.canonicalize(store, { synchronous: true });
     */
    async canonicalize(store, options = {}) {
      const storeInstance = store.store || store;
      const { algorithm = 'URDNA2015', synchronous = false } = options;
      
      try {
        if (synchronous) {
          return rdfCanonize._canonizeSync(storeInstance, { algorithm });
        } else {
          return await rdfCanonize.canonize(storeInstance, { algorithm });
        }
      } catch (error) {
        // Fallback to engine canonicalization
        console.warn(`Advanced canonicalization failed, using fallback: ${error.message}`);
        return engine.canonicalize(storeInstance);
      }
    },

    /**
     * Check if two stores are isomorphic
     * @param {Store|Object} store1 - First store
     * @param {Store|Object} store2 - Second store
     * @returns {Promise<boolean>} True if stores are isomorphic
     * 
     * @example
     * const isIsomorphic = await canon.isIsomorphic(store1, store2);
     * if (isIsomorphic) {
     *   console.log("Stores are logically equivalent");
     * }
     */
    async isIsomorphic(store1, store2) {
      const s1 = store1.store || store1;
      const s2 = store2.store || store2;
      return engine.isIsomorphic(s1, s2);
    },

    /**
     * Get a canonical hash of a store using SHA-256
     * @param {Store|Object} store - Store to hash
     * @returns {Promise<string>} Canonical hash string
     * 
     * @example
     * const hash = await canon.hash(store);
     * console.log("Store hash:", hash);
     */
    async hash(store) {
      const canonical = await this.canonicalize(store);
      
      // Use Node.js crypto module for cryptographic hashing
      const { createHash } = await import('node:crypto');
      const hash = createHash('sha256');
      hash.update(canonical, 'utf8');
      return hash.digest('hex');
    },

    /**
     * Check if multiple stores are all isomorphic
     * @param {Array<Store|Object>} stores - Array of stores to check
     * @returns {Promise<boolean>} True if all stores are isomorphic
     * 
     * @example
     * const allIsomorphic = await canon.allIsomorphic([store1, store2, store3]);
     * if (allIsomorphic) {
     *   console.log("All stores are equivalent");
     * }
     */
    async allIsomorphic(stores) {
      if (stores.length < 2) return true;
      
      const first = stores[0].store || stores[0];
      for (let i = 1; i < stores.length; i++) {
        const current = stores[i].store || stores[i];
        if (!(await engine.isIsomorphic(first, current))) {
          return false;
        }
      }
      return true;
    },

    /**
     * Find stores that are isomorphic to a reference store
     * @param {Store|Object} referenceStore - Reference store
     * @param {Array<Store|Object>} stores - Array of stores to check
     * @returns {Promise<Array<{store: Store, index: number}>>} Array of isomorphic stores
     * 
     * @example
     * const isomorphic = await canon.findIsomorphic(referenceStore, [store1, store2, store3]);
     * console.log(`Found ${isomorphic.length} isomorphic stores`);
     */
    async findIsomorphic(referenceStore, stores) {
      const reference = referenceStore.store || referenceStore;
      const results = [];
      
      for (const [i, store] of stores.entries()) {
        const current = store.store || store;
        if (await engine.isIsomorphic(reference, current)) {
          results.push({ store: store, index: i });
        }
      }
      
      return results;
    },

    /**
     * Group stores by isomorphism
     * @param {Array<Store|Object>} stores - Array of stores to group
     * @returns {Promise<Array<Array<{store: Store, index: number}>>>} Groups of isomorphic stores
     * 
     * @example
     * const groups = await canon.groupByIsomorphism([store1, store2, store3, store4]);
     * console.log(`Found ${groups.length} distinct groups`);
     */
    async groupByIsomorphism(stores) {
      const groups = [];
      const processed = new Set();
      
      for (let i = 0; i < stores.length; i++) {
        if (processed.has(i)) continue;
        
        const current = stores[i].store || stores[i];
        const group = [{ store: stores[i], index: i }];
        processed.add(i);
        
        for (let j = i + 1; j < stores.length; j++) {
          if (processed.has(j)) continue;
          
          const other = stores[j].store || stores[j];
          if (await engine.isIsomorphic(current, other)) {
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
     * @returns {Promise<Object>} Canonical statistics
     * 
     * @example
     * const stats = await canon.getStats(store);
     * console.log(`Canonical size: ${stats.canonicalSize}, Hash: ${stats.hash}`);
     */
    async getStats(store) {
      const storeInstance = store.store || store;
      const canonical = await this.canonicalize(store);
      const hash = await this.hash(store);
      
      return {
        originalSize: storeInstance.size,
        canonicalSize: canonical.length,
        hash,
        canonical
      };
    },

    /**
     * Compare two stores and return detailed comparison
     * @param {Store|Object} store1 - First store
     * @param {Store|Object} store2 - Second store
     * @returns {Promise<Object>} Detailed comparison result
     * 
     * @example
     * const comparison = await canon.compare(store1, store2);
     * console.log(`Isomorphic: ${comparison.isomorphic}, Hash match: ${comparison.hashMatch}`);
     */
    async compare(store1, store2) {
      const s1 = store1.store || store1;
      const s2 = store2.store || store2;
      
      const [canonical1, canonical2, hash1, hash2] = await Promise.all([
        this.canonicalize(store1),
        this.canonicalize(store2),
        this.hash(store1),
        this.hash(store2)
      ]);
      
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
        sizeDifference: s2.size - s1.size
      };
    },

    /**
     * Create a canonical store from multiple stores (union + canonicalize)
     * @param {Array<Store|Object>} stores - Array of stores to merge
     * @param {Object} [options] - Canonicalization options
     * @returns {Promise<Object>} New useGraph instance with canonical store
     * 
     * @example
     * const canonical = await canon.createCanonicalStore([store1, store2, store3]);
     * console.log(`Canonical store has ${canonical.size} triples`);
     */
    async createCanonicalStore(stores, options = {}) {
      const storeInstances = stores.map(s => s.store || s);
      const unionStore = engine.union(...storeInstances);
      const canonical = await this.canonicalize(unionStore, options);
      const canonicalStore = engine.parseNQuads(canonical);
      
      return {
        store: canonicalStore,
        canonical,
        hash: await this.hash(canonicalStore),
        size: canonicalStore.size
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
        // Fallback to engine canonicalization
        console.warn(`Synchronous canonicalization failed, using fallback: ${error.message}`);
        return engine.canonicalize(storeInstance);
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
     * @returns {Promise<Object>} Canonicalization statistics
     * 
     * @example
     * const stats = await canon.getCanonicalizationStats(store);
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
          supported: this.isSupported()
        };
      } catch (error) {
        return {
          originalSize: storeInstance.size,
          canonicalSize: 0,
          canonicalQuads: 0,
          duration: 0,
          compressionRatio: 0,
          supported: false,
          error: error.message
        };
      }
    }
  };
}

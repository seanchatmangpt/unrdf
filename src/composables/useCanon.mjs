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

import { RdfEngine } from "../engines/RdfEngine.mjs";

// Create a single, shared instance of the engine for efficiency
const rdfEngine = new RdfEngine();

/**
 * Create a canonicalization composable
 * 
 * @param {Object} [options] - Canonicalization options
 * @param {number} [options.timeoutMs=30000] - Canonicalization timeout
 * @param {Function} [options.onMetric] - Metrics callback
 * @returns {Object} Canonicalization interface
 * 
 * @example
 * const canon = useCanon();
 * 
 * // Canonicalize a store
 * const canonical = await canon.canonicalize(store);
 * 
 * // Check if two stores are isomorphic
 * const isIsomorphic = await canon.isIsomorphic(store1, store2);
 */
export function useCanon(options = {}) {
  const {
    timeoutMs = 30000,
    onMetric
  } = options;

  const engine = new RdfEngine({ 
    timeoutMs,
    onMetric 
  });

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
     * @returns {Promise<string>} Canonical N-Quads string
     * 
     * @example
     * const canonical = await canon.canonicalize(store);
     * console.log("Canonical form:", canonical);
     */
    async canonicalize(store) {
      const storeInstance = store.store || store;
      return engine.canonicalize(storeInstance);
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
     * Get a canonical hash of a store
     * @param {Store|Object} store - Store to hash
     * @returns {Promise<string>} Canonical hash string
     * 
     * @example
     * const hash = await canon.hash(store);
     * console.log("Store hash:", hash);
     */
    async hash(store) {
      const canonical = await this.canonicalize(store);
      // Simple hash function - in production you might want crypto.createHash
      let hash = 0;
      for (let i = 0; i < canonical.length; i++) {
        const char = canonical.charCodeAt(i);
        hash = ((hash << 5) - hash) + char;
        hash = hash & hash; // Convert to 32-bit integer
      }
      return hash.toString(16);
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
      
      for (let i = 0; i < stores.length; i++) {
        const current = stores[i].store || stores[i];
        if (await engine.isIsomorphic(reference, current)) {
          results.push({ store: stores[i], index: i });
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
     * @returns {Promise<Object>} New useGraph instance with canonical store
     * 
     * @example
     * const canonical = await canon.createCanonicalStore([store1, store2, store3]);
     * console.log(`Canonical store has ${canonical.size} triples`);
     */
    async createCanonicalStore(stores) {
      const storeInstances = stores.map(s => s.store || s);
      const unionStore = engine.union(...storeInstances);
      const canonical = await engine.canonicalize(unionStore);
      const canonicalStore = engine.parseNQuads(canonical);
      
      return {
        store: canonicalStore,
        canonical,
        hash: await this.hash(canonicalStore),
        size: canonicalStore.size
      };
    }
  };
}

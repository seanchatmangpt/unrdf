/**
 * @file Canonicalization and isomorphism checks for RDF graphs.
 * @module canonicalize
 */

import { Writer } from 'n3';
import rdfCanonize from 'rdf-canonize';

/**
 * Canonicalize a store into URDNA2015 canonical N-Quads.
 * @param {import('n3').Store} store - The store to canonicalize
 * @param {Object} [options] - Canonicalization options
 * @param {string} [options.algorithm='URDNA2015'] - Canonicalization algorithm
 * @param {boolean} [options.produceGeneralizedRdf=false] - Produce generalized RDF
 * @param {number} [options.timeoutMs=30000] - Timeout in milliseconds
 * @returns {Promise<string>} Promise resolving to canonical N-Quads string
 * 
 * @throws {Error} If canonicalization fails
 * 
 * @example
 * const store = new Store();
 * // ... add quads to store
 * 
 * const canonical = await canonicalize(store);
 * console.log('Canonical N-Quads:', canonical);
 */
export async function canonicalize(store, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('canonicalize: store must be a valid Store instance');
  }

  const {
    algorithm = 'URDNA2015',
    produceGeneralizedRdf = false,
    timeoutMs = 30000
  } = options;

  try {
    // Convert store to N-Quads format
    const writer = new Writer({ format: 'N-Quads' });
    writer.addQuads(store.getQuads());
    const nquads = await new Promise((resolve, reject) => {
      writer.end((error, result) => {
        if (error) {
          reject(new Error(`Failed to serialize to N-Quads: ${error.message}`));
        } else {
          resolve(result);
        }
      });
    });

    // Set up timeout
    const timeoutPromise = new Promise((_, reject) => {
      setTimeout(() => reject(new Error('Canonicalization timeout')), timeoutMs);
    });

    // Perform canonicalization
    const canonicalPromise = rdfCanonize.canonize(nquads, {
      algorithm,
      inputFormat: 'application/n-quads',
      produceGeneralizedRdf
    });

    return await Promise.race([canonicalPromise, timeoutPromise]);
  } catch (error) {
    throw new Error(`Canonicalization failed: ${error.message}`);
  }
}

/**
 * Check if two stores are isomorphic (logically equivalent).
 * @param {import('n3').Store} storeA - First store to compare
 * @param {import('n3').Store} storeB - Second store to compare
 * @param {Object} [options] - Comparison options
 * @param {string} [options.algorithm='URDNA2015'] - Canonicalization algorithm
 * @param {number} [options.timeoutMs=30000] - Timeout in milliseconds
 * @returns {Promise<boolean>} Promise resolving to true if stores are isomorphic
 * 
 * @throws {Error} If comparison fails
 * 
 * @example
 * const store1 = new Store();
 * const store2 = new Store();
 * // ... add quads to both stores
 * 
 * const isomorphic = await isIsomorphic(store1, store2);
 * console.log('Stores are isomorphic:', isomorphic);
 */
export async function isIsomorphic(storeA, storeB, options = {}) {
  if (!storeA || typeof storeA.getQuads !== 'function') {
    throw new TypeError('isIsomorphic: storeA must be a valid Store instance');
  }
  if (!storeB || typeof storeB.getQuads !== 'function') {
    throw new TypeError('isIsomorphic: storeB must be a valid Store instance');
  }

  try {
    // Quick size check first
    if (storeA.size !== storeB.size) {
      return false;
    }

    // If both stores are empty, they are isomorphic
    if (storeA.size === 0) {
      return true;
    }

    // Canonicalize both stores and compare
    const [canonicalA, canonicalB] = await Promise.all([
      canonicalize(storeA, options),
      canonicalize(storeB, options)
    ]);

    return canonicalA === canonicalB;
  } catch (error) {
    throw new Error(`Isomorphism check failed: ${error.message}`);
  }
}

/**
 * Get canonical hash of a store.
 * @param {import('n3').Store} store - The store to hash
 * @param {Object} [options] - Hashing options
 * @param {string} [options.algorithm='SHA-256'] - Hash algorithm
 * @param {string} [options.canonicalAlgorithm='URDNA2015'] - Canonicalization algorithm
 * @returns {Promise<string>} Promise resolving to hexadecimal hash string
 * 
 * @throws {Error} If hashing fails
 * 
 * @example
 * const store = new Store();
 * // ... add quads to store
 * 
 * const hash = await getCanonicalHash(store);
 * console.log('Canonical hash:', hash);
 */
export async function getCanonicalHash(store, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('getCanonicalHash: store must be a valid Store instance');
  }

  const { algorithm = 'SHA-256', canonicalAlgorithm = 'URDNA2015' } = options;

  try {
    const canonical = await canonicalize(store, { algorithm: canonicalAlgorithm });
    
    // Use Web Crypto API if available, otherwise fall back to Node.js crypto
    if (typeof crypto !== 'undefined' && crypto.subtle) {
      const encoder = new TextEncoder();
      const data = encoder.encode(canonical);
      const hashBuffer = await crypto.subtle.digest(algorithm, data);
      const hashArray = Array.from(new Uint8Array(hashBuffer));
      return hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
    } else {
      // Node.js fallback
      const crypto = await import('node:crypto');
      const hash = crypto.createHash(algorithm.toLowerCase());
      hash.update(canonical);
      return hash.digest('hex');
    }
  } catch (error) {
    throw new Error(`Canonical hashing failed: ${error.message}`);
  }
}

/**
 * Compare multiple stores and group them by isomorphism.
 * @param {Array<import('n3').Store>} stores - Array of stores to compare
 * @param {Object} [options] - Comparison options
 * @returns {Promise<Array<Array<number>>>} Promise resolving to array of groups (indices of isomorphic stores)
 * 
 * @throws {Error} If comparison fails
 * 
 * @example
 * const stores = [store1, store2, store3, store4];
 * const groups = await groupByIsomorphism(stores);
 * console.log('Isomorphic groups:', groups);
 * // Output: [[0, 2], [1], [3]] - stores 0 and 2 are isomorphic, 1 and 3 are unique
 */
export async function groupByIsomorphism(stores, options = {}) {
  if (!Array.isArray(stores) || stores.length === 0) {
    throw new TypeError('groupByIsomorphism: stores must be a non-empty array');
  }

  try {
    const groups = [];
    const processed = new Set();

    for (let i = 0; i < stores.length; i++) {
      if (processed.has(i)) {
        continue;
      }

      const currentGroup = [i];
      processed.add(i);

      for (let j = i + 1; j < stores.length; j++) {
        if (processed.has(j)) {
          continue;
        }

        try {
          const isomorphic = await isIsomorphic(stores[i], stores[j], options);
          if (isomorphic) {
            currentGroup.push(j);
            processed.add(j);
          }
        } catch (error) {
          // If comparison fails, treat as non-isomorphic
          console.warn(`Failed to compare stores ${i} and ${j}: ${error.message}`);
        }
      }

      groups.push(currentGroup);
    }

    return groups;
  } catch (error) {
    throw new Error(`Isomorphism grouping failed: ${error.message}`);
  }
}

/**
 * Find duplicate stores in an array.
 * @param {Array<import('n3').Store>} stores - Array of stores to check
 * @param {Object} [options] - Comparison options
 * @returns {Promise<Array<Array<number>>>} Promise resolving to array of duplicate groups
 * 
 * @throws {Error} If comparison fails
 * 
 * @example
 * const stores = [store1, store2, store3];
 * const duplicates = await findDuplicates(stores);
 * if (duplicates.length > 0) {
 *   console.log('Found duplicate stores:', duplicates);
 * }
 */
export async function findDuplicates(stores, options = {}) {
  const groups = await groupByIsomorphism(stores, options);
  return groups.filter(group => group.length > 1);
}

/**
 * Get canonicalization statistics.
 * @param {import('n3').Store} store - The store to analyze
 * @param {Object} [options] - Analysis options
 * @returns {Promise<Object>} Promise resolving to canonicalization statistics
 * 
 * @example
 * const stats = await getCanonicalizationStats(store);
 * console.log('Store size:', stats.quadCount);
 * console.log('Canonical size:', stats.canonicalSize);
 * console.log('Compression ratio:', stats.compressionRatio);
 */
export async function getCanonicalizationStats(store, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('getCanonicalizationStats: store must be a valid Store instance');
  }

  try {
    const startTime = Date.now();
    const canonical = await canonicalize(store, options);
    const endTime = Date.now();

    const quadCount = store.size;
    const canonicalSize = canonical.length;
    const compressionRatio = quadCount > 0 ? canonicalSize / (quadCount * 100) : 0; // Rough estimate

    return {
      quadCount,
      canonicalSize,
      compressionRatio,
      canonicalizationTime: endTime - startTime,
      averageQuadSize: quadCount > 0 ? canonicalSize / quadCount : 0
    };
  } catch (error) {
    throw new Error(`Canonicalization statistics failed: ${error.message}`);
  }
}

/**
 * Create a canonicalization session for batch operations.
 * @param {Object} [options] - Session options
 * @returns {Object} Canonicalization session
 * 
 * @example
 * const session = createCanonicalizationSession();
 * 
 * // Add stores to session
 * session.addStore('store1', store1);
 * session.addStore('store2', store2);
 * 
 * // Canonicalize all stores
 * const results = await session.canonicalizeAll();
 * 
 * // Check for duplicates
 * const duplicates = await session.findDuplicates();
 */
export function createCanonicalizationSession(options = {}) {
  const stores = new Map();
  const sessionOptions = options;

  return {
    /**
     * Add a store to the session.
     * @param {string} id - Store identifier
     * @param {import('n3').Store} store - Store to add
     */
    addStore(id, store) {
      if (!store || typeof store.getQuads !== 'function') {
        throw new TypeError('addStore: store must be a valid Store instance');
      }
      stores.set(id, store);
    },

    /**
     * Remove a store from the session.
     * @param {string} id - Store identifier
     */
    removeStore(id) {
      stores.delete(id);
    },

    /**
     * Get all store IDs.
     * @returns {Array<string>} Array of store IDs
     */
    getStoreIds() {
      return Array.from(stores.keys());
    },

    /**
     * Canonicalize all stores in the session.
     * @returns {Promise<Map<string, string>>} Promise resolving to map of ID -> canonical N-Quads
     */
    async canonicalizeAll() {
      const results = new Map();
      const storeEntries = Array.from(stores.entries());

      for (const [id, store] of storeEntries) {
        try {
          const canonical = await canonicalize(store, sessionOptions);
          results.set(id, canonical);
        } catch (error) {
          console.error(`Failed to canonicalize store ${id}: ${error.message}`);
        }
      }

      return results;
    },

    /**
     * Find duplicate stores in the session.
     * @returns {Promise<Array<Array<string>>>} Promise resolving to array of duplicate groups
     */
    async findDuplicates() {
      const storeArray = Array.from(stores.values());
      const duplicateGroups = await findDuplicates(storeArray, sessionOptions);
      
      // Convert indices back to IDs
      const storeIds = Array.from(stores.keys());
      return duplicateGroups.map(group => group.map(index => storeIds[index]));
    },

    /**
     * Get session statistics.
     * @returns {Promise<Object>} Promise resolving to session statistics
     */
    async getStats() {
      const storeArray = Array.from(stores.values());
      const totalQuads = storeArray.reduce((sum, store) => sum + store.size, 0);
      
      const canonicalResults = await this.canonicalizeAll();
      const totalCanonicalSize = Array.from(canonicalResults.values())
        .reduce((sum, canonical) => sum + canonical.length, 0);

      return {
        storeCount: stores.size,
        totalQuads,
        totalCanonicalSize,
        averageQuadsPerStore: stores.size > 0 ? totalQuads / stores.size : 0,
        averageCanonicalSize: stores.size > 0 ? totalCanonicalSize / stores.size : 0
      };
    }
  };
}

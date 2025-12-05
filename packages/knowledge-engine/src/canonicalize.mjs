/**
 * @file Canonicalization and isomorphism checks for RDF graphs.
 * @module canonicalize
 */

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

  const { algorithm = 'URDNA2015', produceGeneralizedRdf = false, timeoutMs = 30000 } = options;

  try {
    // Get quads from store and validate
    const quads = store.getQuads();
    if (!Array.isArray(quads)) {
      throw new TypeError('store.getQuads() must return an array');
    }

    // If store is empty, return empty canonical form
    if (quads.length === 0) {
      return '';
    }

    // Convert store to N-Quads format using Oxigraph dump
    const nquads = store.dump({ format: 'application/n-quads' });

    // Validate nquads output
    if (typeof nquads !== 'string' || nquads.trim().length === 0) {
      throw new TypeError('Serialization produced empty or invalid N-Quads');
    }

    // Parse N-Quads string to RDF.js dataset format
    // rdf-canonize expects an array of quad objects, not a string
    const parsedDataset = rdfCanonize.NQuads.parse(nquads);

    // Set up timeout
    const timeoutPromise = new Promise((_, reject) => {
      setTimeout(() => reject(new Error('Canonicalization timeout')), timeoutMs);
    });

    // Perform canonicalization with parsed dataset
    const canonicalPromise = rdfCanonize.canonize(parsedDataset, {
      algorithm,
      produceGeneralizedRdf,
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
      canonicalize(storeB, options),
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
 * @param {string} [options.hashAlgorithm='SHA-256'] - Hash algorithm
 * @param {string} [options.algorithm='URDNA2015'] - Canonicalization algorithm
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

  const { hashAlgorithm = 'SHA-256', algorithm = 'URDNA2015' } = options;

  // Normalize hash algorithm name for Web Crypto API
  const normalizeHashAlgorithm = alg => {
    const normalized = alg.toLowerCase().replace(/[^a-z0-9]/g, '');
    const map = {
      sha1: 'SHA-1',
      sha256: 'SHA-256',
      sha384: 'SHA-384',
      sha512: 'SHA-512',
    };
    return map[normalized] || alg;
  };

  try {
    const canonical = await canonicalize(store, { algorithm });

    // Use Web Crypto API if available, otherwise fall back to Node.js crypto
    if (typeof crypto !== 'undefined' && crypto.subtle) {
      const encoder = new TextEncoder();
      const data = encoder.encode(canonical);
      const webCryptoAlg = normalizeHashAlgorithm(hashAlgorithm);
      const hashBuffer = await crypto.subtle.digest(webCryptoAlg, data);
      const hashArray = Array.from(new Uint8Array(hashBuffer));
      return hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
    } else {
      // Node.js fallback
      const crypto = await import('node:crypto');
      const nodeCryptoAlg = hashAlgorithm.toLowerCase().replace('-', '');
      const hash = crypto.createHash(nodeCryptoAlg);
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
 * @returns {Promise<Array<Object>>} Promise resolving to array of group objects with stores property
 *
 * @throws {Error} If comparison fails
 *
 * @example
 * const stores = [store1, store2, store3, store4];
 * const groups = await groupByIsomorphism(stores);
 * console.log('Isomorphic groups:', groups);
 * // Output: [{stores: [store1, store3]}, {stores: [store2]}, {stores: [store4]}]
 */
export async function groupByIsomorphism(stores, options = {}) {
  if (!Array.isArray(stores)) {
    return [];
  }

  if (stores.length === 0) {
    return [];
  }

  // Validate all stores are valid before processing
  for (let i = 0; i < stores.length; i++) {
    if (!stores[i] || typeof stores[i].getQuads !== 'function') {
      throw new TypeError(`groupByIsomorphism: store at index ${i} must be a valid Store instance`);
    }
  }

  try {
    const groups = [];
    const processed = new Set();

    for (let i = 0; i < stores.length; i++) {
      if (processed.has(i)) {
        continue;
      }

      const currentGroupStores = [stores[i]];
      processed.add(i);

      for (let j = i + 1; j < stores.length; j++) {
        if (processed.has(j)) {
          continue;
        }

        try {
          const isomorphic = await isIsomorphic(stores[i], stores[j], options);
          if (isomorphic) {
            currentGroupStores.push(stores[j]);
            processed.add(j);
          }
        } catch (error) {
          // If comparison fails, treat as non-isomorphic
          console.warn(`Failed to compare stores ${i} and ${j}: ${error.message}`);
        }
      }

      groups.push({ stores: currentGroupStores });
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
 * @returns {Promise<Array<Object>>} Promise resolving to array of duplicate objects with stores and canonicalHash
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
  const duplicates = [];

  for (const group of groups) {
    if (group.stores.length > 1) {
      // Get canonical hash for the group
      const hash = await getCanonicalHash(group.stores[0], options);
      duplicates.push({
        stores: group.stores,
        canonicalHash: hash,
      });
    }
  }

  return duplicates;
}

/**
 * Get canonicalization statistics.
 * @param {import('n3').Store} store - The store to analyze
 * @param {Object} [options] - Analysis options
 * @returns {Promise<Object>} Promise resolving to canonicalization statistics
 *
 * @example
 * const stats = await getCanonicalizationStats(store);
 * console.log('Store size:', stats.quads);
 * console.log('Canonical size:', stats.canonicalLength);
 * console.log('Time:', stats.canonicalizationTime);
 */
export async function getCanonicalizationStats(store, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('getCanonicalizationStats: store must be a valid Store instance');
  }

  const { algorithm = 'URDNA2015' } = options;

  try {
    const startTime = performance.now();
    const canonical = await canonicalize(store, { algorithm });
    const endTime = performance.now();

    return {
      quads: store.size,
      canonicalLength: canonical.length,
      canonicalizationTime: Math.max(endTime - startTime, 0.01), // Ensure non-zero time
      algorithm,
    };
  } catch (error) {
    throw new Error(`Canonicalization statistics failed: ${error.message}`);
  }
}

/**
 * Create a canonicalization session for batch operations.
 * @param {Object} [options] - Session options
 * @returns {Promise<Object>} Canonicalization session
 *
 * @example
 * const session = await createCanonicalizationSession();
 *
 * // Direct canonicalization
 * const canonical = await session.canonicalize(store);
 *
 * // Isomorphism check
 * const isomorphic = await session.isIsomorphic(store1, store2);
 *
 * // Get canonical hash
 * const hash = await session.getCanonicalHash(store);
 *
 * // Get session statistics
 * const stats = session.getStats();
 */
export async function createCanonicalizationSession(options = {}) {
  const sessionOptions = options;
  let canonicalizationCount = 0;
  let isomorphismCheckCount = 0;
  let totalTime = 0;

  return {
    /**
     * Canonicalize a store.
     * @param {import('n3').Store} store - Store to canonicalize
     * @returns {Promise<string>} Canonical N-Quads
     */
    async canonicalize(store) {
      const startTime = Date.now();
      try {
        const result = await canonicalize(store, sessionOptions);
        canonicalizationCount++;
        totalTime += Date.now() - startTime;
        return result;
      } catch (error) {
        totalTime += Date.now() - startTime;
        throw error;
      }
    },

    /**
     * Check if two stores are isomorphic.
     * @param {import('n3').Store} storeA - First store
     * @param {import('n3').Store} storeB - Second store
     * @returns {Promise<boolean>} True if isomorphic
     */
    async isIsomorphic(storeA, storeB) {
      const startTime = Date.now();
      try {
        const result = await isIsomorphic(storeA, storeB, sessionOptions);
        isomorphismCheckCount++;
        totalTime += Date.now() - startTime;
        return result;
      } catch (error) {
        totalTime += Date.now() - startTime;
        throw error;
      }
    },

    /**
     * Get canonical hash of a store.
     * @param {import('n3').Store} store - Store to hash
     * @returns {Promise<string>} Canonical hash
     */
    async getCanonicalHash(store) {
      const startTime = Date.now();
      try {
        const result = await getCanonicalHash(store, sessionOptions);
        canonicalizationCount++;
        totalTime += Date.now() - startTime;
        return result;
      } catch (error) {
        totalTime += Date.now() - startTime;
        throw error;
      }
    },

    /**
     * Get session statistics.
     * @returns {Object} Session statistics
     */
    getStats() {
      return {
        canonicalizations: canonicalizationCount,
        isomorphismChecks: isomorphismCheckCount,
        totalTime,
      };
    },
  };
}

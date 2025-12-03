/**
 * @file RDF Canonicalization - URDNA2015 normalization
 * @module @unrdf/core/rdf/canonicalize
 */

import { Writer } from 'n3';
import rdfCanonize from 'rdf-canonize';

/**
 * @typedef {import('n3').Store} Store
 * @typedef {import('n3').Quad} Quad
 */

/**
 * Canonicalize a store into URDNA2015 canonical N-Quads
 * @param {Store} store - The store to canonicalize
 * @param {Object} [options] - Canonicalization options
 * @param {string} [options.algorithm='URDNA2015'] - Canonicalization algorithm
 * @param {boolean} [options.produceGeneralizedRdf=false] - Produce generalized RDF
 * @param {number} [options.timeoutMs=30000] - Timeout in milliseconds
 * @returns {Promise<string>} Promise resolving to canonical N-Quads string
 *
 * @throws {TypeError} If store is not valid
 * @throws {Error} If canonicalization fails or times out
 *
 * @example
 * const store = createStore();
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
    const quads = store.getQuads();

    if (!Array.isArray(quads)) {
      throw new TypeError('store.getQuads() must return an array');
    }

    if (quads.length === 0) {
      return '';
    }

    // Convert store to N-Quads format
    const writer = new Writer({ format: 'N-Quads' });
    writer.addQuads(quads);
    const nquads = await new Promise((resolve, reject) => {
      writer.end((error, result) => {
        if (error) {
          reject(new Error(`Failed to serialize to N-Quads: ${error.message}`));
        } else {
          resolve(result);
        }
      });
    });

    if (typeof nquads !== 'string' || nquads.trim().length === 0) {
      throw new TypeError('Serialization produced empty or invalid N-Quads');
    }

    // Parse N-Quads string to RDF dataset format
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
 * Convert quads to N-Triples format
 * @param {Quad[]} quads - Array of quads to convert
 * @returns {Promise<string>} Promise resolving to N-Triples string
 *
 * @throws {TypeError} If quads is not an array
 * @throws {Error} If serialization fails
 *
 * @example
 * const quads = getQuads(store);
 * const ntriples = await toNTriples(quads);
 * console.log('N-Triples:', ntriples);
 */
export async function toNTriples(quads) {
  if (!Array.isArray(quads)) {
    throw new TypeError('toNTriples: quads must be an array');
  }

  if (quads.length === 0) {
    return '';
  }

  try {
    const writer = new Writer({ format: 'N-Triples' });
    writer.addQuads(quads);

    return await new Promise((resolve, reject) => {
      writer.end((error, result) => {
        if (error) {
          reject(new Error(`Failed to serialize to N-Triples: ${error.message}`));
        } else {
          resolve(result);
        }
      });
    });
  } catch (error) {
    throw new Error(`N-Triples conversion failed: ${error.message}`);
  }
}

/**
 * Sort quads for deterministic output
 * @param {Quad[]} quads - Array of quads to sort
 * @returns {Quad[]} Sorted array of quads
 *
 * @throws {TypeError} If quads is not an array
 *
 * @example
 * const quads = getQuads(store);
 * const sorted = sortQuads(quads);
 */
export function sortQuads(quads) {
  if (!Array.isArray(quads)) {
    throw new TypeError('sortQuads: quads must be an array');
  }

  return [...quads].sort((a, b) => {
    // Sort by subject, predicate, object, graph
    const aSubj = a.subject.value;
    const bSubj = b.subject.value;
    if (aSubj !== bSubj) return aSubj.localeCompare(bSubj);

    const aPred = a.predicate.value;
    const bPred = b.predicate.value;
    if (aPred !== bPred) return aPred.localeCompare(bPred);

    const aObj = a.object.value;
    const bObj = b.object.value;
    if (aObj !== bObj) return aObj.localeCompare(bObj);

    const aGraph = a.graph?.value || '';
    const bGraph = b.graph?.value || '';
    return aGraph.localeCompare(bGraph);
  });
}

/**
 * Check if two stores are isomorphic
 * @param {Store} store1 - First store
 * @param {Store} store2 - Second store
 * @param {Object} [options] - Canonicalization options
 * @returns {Promise<boolean>} True if stores are isomorphic
 *
 * @throws {TypeError} If stores are not valid
 * @throws {Error} If canonicalization fails
 *
 * @example
 * const store1 = createStore();
 * const store2 = createStore();
 * // ... add quads to stores
 *
 * const isIsomorphic = await isIsomorphic(store1, store2);
 * if (isIsomorphic) {
 *   console.log('Stores are logically equivalent');
 * }
 */
export async function isIsomorphic(store1, store2, options = {}) {
  if (!store1 || typeof store1.getQuads !== 'function') {
    throw new TypeError('isIsomorphic: store1 must be a valid Store instance');
  }
  if (!store2 || typeof store2.getQuads !== 'function') {
    throw new TypeError('isIsomorphic: store2 must be a valid Store instance');
  }

  try {
    const canonical1 = await canonicalize(store1, options);
    const canonical2 = await canonicalize(store2, options);
    return canonical1 === canonical2;
  } catch (error) {
    throw new Error(`Isomorphism check failed: ${error.message}`);
  }
}

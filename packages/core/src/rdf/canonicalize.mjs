/**
 * @file RDF Canonicalization - URDNA2015 normalization
 * @module @unrdf/core/rdf/canonicalize
 */

import rdfCanonize from 'rdf-canonize';
import { createStore } from '@unrdf/oxigraph';

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
  if (!store) {
    throw new TypeError('store is required');
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

    // Convert store to N-Quads format using Oxigraph
    const tempStore = createStore();
    for (const quad of quads) {
      tempStore.add(quad);
    }
    const nquads = tempStore.dump({ format: 'application/n-quads' });

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
    // Oxigraph requires N-Quads format for datasets
    // N-Triples is a subset of N-Quads (quads without graph component)
    const tempStore = createStore();
    for (const quad of quads) {
      tempStore.add(quad);
    }
    const nquads = tempStore.dump({ format: 'application/n-quads' });

    // Convert N-Quads to N-Triples by removing graph components
    // N-Quads: <s> <p> <o> <g> .
    // N-Triples: <s> <p> <o> .
    return nquads
      .split('\n')
      .map(line => {
        if (!line.trim()) return line;
        // Match pattern: <s> <p> <o> [<g>] .
        // Remove optional 4th component (graph)
        const match = line.match(/^(.+?)\s+(.+?)\s+(.+?)(?:\s+<[^>]+>)?\s+\.$/);
        if (match) {
          return `${match[1]} ${match[2]} ${match[3]} .`;
        }
        return line;
      })
      .join('\n');
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
  if (!store1) {
    throw new TypeError('store1 is required');
  }
  if (!store2) {
    throw new TypeError('store2 is required');
  }

  try {
    const canonical1 = await canonicalize(store1, options);
    const canonical2 = await canonicalize(store2, options);
    return canonical1 === canonical2;
  } catch (error) {
    throw new Error(`Isomorphism check failed: ${error.message}`);
  }
}

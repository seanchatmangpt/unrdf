/**
 * @file RDF Store operations - N3.Store wrapper
 * @module @unrdf/core/rdf/store
 */

import { Store, DataFactory } from 'n3';
import { z } from 'zod';

const { namedNode, literal, blankNode, variable, defaultGraph, quad } = DataFactory;

/**
 * @typedef {import('n3').Quad} Quad
 * @typedef {import('n3').Term} Term
 * @typedef {import('n3').Store} Store
 */

/**
 * Quad schema for validation
 */
const QuadSchema = z.object({
  subject: z.object({ value: z.string() }),
  predicate: z.object({ value: z.string() }),
  object: z.object({ value: z.string() }),
  graph: z.object({ value: z.string() }).optional(),
});

/**
 * Create a new RDF store
 * @returns {Store} New N3.Store instance
 *
 * @example
 * const store = createStore();
 * console.log('Created RDF store');
 */
export function createStore() {
  return new Store();
}

/**
 * Add a quad to the store
 * @param {Store} store - The store to add to
 * @param {Quad|Object} quadData - Quad or quad-like object to add
 * @returns {void}
 *
 * @throws {TypeError} If store is not valid
 * @throws {TypeError} If quadData is missing required fields
 *
 * @example
 * const store = createStore();
 * addQuad(store, {
 *   subject: namedNode('http://example.org/alice'),
 *   predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
 *   object: literal('Alice'),
 *   graph: defaultGraph()
 * });
 */
export function addQuad(store, quadData) {
  if (!store || typeof store.addQuad !== 'function') {
    throw new TypeError('addQuad: store must be a valid Store instance');
  }

  if (!quadData) {
    throw new TypeError('addQuad: quadData is required');
  }

  // Validate quad structure
  QuadSchema.parse(quadData);

  store.addQuad(quadData);
}

/**
 * Remove a quad from the store
 * @param {Store} store - The store to remove from
 * @param {Quad|Object} quadData - Quad or quad-like object to remove
 * @returns {void}
 *
 * @throws {TypeError} If store is not valid
 * @throws {TypeError} If quadData is missing required fields
 *
 * @example
 * const store = createStore();
 * removeQuad(store, {
 *   subject: namedNode('http://example.org/alice'),
 *   predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
 *   object: literal('Alice'),
 *   graph: defaultGraph()
 * });
 */
export function removeQuad(store, quadData) {
  if (!store || typeof store.removeQuad !== 'function') {
    throw new TypeError('removeQuad: store must be a valid Store instance');
  }

  if (!quadData) {
    throw new TypeError('removeQuad: quadData is required');
  }

  // Validate quad structure
  QuadSchema.parse(quadData);

  store.removeQuad(quadData);
}

/**
 * Get quads from the store
 * @param {Store} store - The store to query
 * @param {Term|null} [subject] - Subject filter (null for any)
 * @param {Term|null} [predicate] - Predicate filter (null for any)
 * @param {Term|null} [object] - Object filter (null for any)
 * @param {Term|null} [graph] - Graph filter (null for any)
 * @returns {Quad[]} Array of matching quads
 *
 * @throws {TypeError} If store is not valid
 *
 * @example
 * const store = createStore();
 * // Get all quads
 * const allQuads = getQuads(store);
 *
 * // Get quads by subject
 * const aliceQuads = getQuads(store, namedNode('http://example.org/alice'));
 *
 * // Get quads by predicate
 * const nameQuads = getQuads(store, null, namedNode('http://xmlns.com/foaf/0.1/name'));
 */
export function getQuads(store, subject = null, predicate = null, object = null, graph = null) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('getQuads: store must be a valid Store instance');
  }

  return store.getQuads(subject, predicate, object, graph);
}

/**
 * Iterate over all quads in the store
 * @param {Store} store - The store to iterate
 * @returns {IterableIterator<Quad>} Iterator over quads
 *
 * @throws {TypeError} If store is not valid
 *
 * @example
 * const store = createStore();
 * for (const quad of iterateQuads(store)) {
 *   console.log(`${quad.subject.value} ${quad.predicate.value} ${quad.object.value}`);
 * }
 */
export function* iterateQuads(store) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('iterateQuads: store must be a valid Store instance');
  }

  const quads = store.getQuads();
  for (const quad of quads) {
    yield quad;
  }
}

/**
 * Count quads in the store
 * @param {Store} store - The store to count
 * @param {Term|null} [subject] - Subject filter (null for any)
 * @param {Term|null} [predicate] - Predicate filter (null for any)
 * @param {Term|null} [object] - Object filter (null for any)
 * @param {Term|null} [graph] - Graph filter (null for any)
 * @returns {number} Number of matching quads
 *
 * @throws {TypeError} If store is not valid
 *
 * @example
 * const store = createStore();
 * const count = countQuads(store);
 * console.log(`Store has ${count} quads`);
 */
export function countQuads(store, subject = null, predicate = null, object = null, graph = null) {
  if (!store || typeof store.countQuads !== 'function') {
    throw new TypeError('countQuads: store must be a valid Store instance');
  }

  return store.countQuads(subject, predicate, object, graph);
}

// Re-export N3 DataFactory for convenience
export { namedNode, literal, blankNode, variable, defaultGraph, quad };

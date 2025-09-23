/**
 * @fileoverview useStore composable - canonical N3.Store operations
 * 
 * This composable provides the foundation for all unrdf operations.
 * It enforces the "One Store Rule" - N3.Store is the only memory model.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { Store, DataFactory } from "n3";
import { RdfEngine } from "../engines/RdfEngine.mjs";

const { namedNode, literal, blankNode, quad, defaultGraph } = DataFactory;

/**
 * Create a new store composable
 * 
 * @param {Array} [initialQuads=[]] - Initial quads to add to the store
 * @param {Object} [options] - Store options
 * @param {string} [options.baseIRI] - Base IRI for the store
 * @param {boolean} [options.deterministic=true] - Enable deterministic operations
 * @returns {Object} Store composable interface
 * 
 * @example
 * const store = useStore();
 * store.add(quad(namedNode('ex:subject'), namedNode('ex:predicate'), literal('value')));
 * console.log(store.stats());
 */
export function useStore(initialQuads = [], options = {}) {
  const engine = new RdfEngine(options);
  const store = new Store(initialQuads);

  return {
    /**
     * The raw N3.Store instance
     * @type {Store}
     */
    get store() {
      return store;
    },

    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return engine;
    },

    /**
     * Add quads to the store
     * @param {...Quad} quads - Quads to add
     * @returns {void}
     */
    add(...quads) {
      for (const q of quads) {
        store.add(q);
      }
    },

    /**
     * Remove quads from the store
     * @param {...Quad} quads - Quads to remove
     * @returns {void}
     */
    remove(...quads) {
      for (const q of quads) {
        store.delete(q);
      }
    },

    /**
     * Clear all quads from the store
     * @returns {void}
     */
    clear() {
      store.removeQuads([...store]);
    },

    /**
     * Get statistics about the store
     * @returns {Object} Store statistics
     */
    stats() {
      return engine.getStats(store);
    },

    /**
     * Serialize the store to a string
     * @param {Object} [options] - Serialization options
     * @param {string} [options.format='Turtle'] - Output format
     * @param {Object} [options.prefixes] - Prefix mappings
     * @returns {Promise<string>} Serialized string
     */
    async serialize(options = {}) {
      const { format = "Turtle", prefixes } = options;
      
      if (format === "Turtle") {
        return await engine.serializeTurtle(store, { prefixes });
      }
      if (format === "N-Quads") {
        return await engine.serializeNQuads(store);
      }
      
      throw new Error(`Unsupported serialization format: ${format}`);
    },

    /**
     * Create a named node
     * @param {string} value - IRI value
     * @returns {NamedNode} Named node
     */
    namedNode(value) {
      return namedNode(value);
    },

    /**
     * Create a literal
     * @param {string} value - Literal value
     * @param {string} [datatype] - Datatype IRI
     * @returns {Literal} Literal
     */
    literal(value, datatype) {
      return literal(value, datatype);
    },

    /**
     * Create a blank node
     * @param {string} [value] - Blank node identifier
     * @returns {BlankNode} Blank node
     */
    blankNode(value) {
      return blankNode(value);
    },

    /**
     * Create a quad
     * @param {Term} s - Subject
     * @param {Term} p - Predicate
     * @param {Term} o - Object
     * @param {Term} [g] - Graph
     * @returns {Quad} Quad
     */
    quad(s, p, o, g) {
      return quad(s, p, o, g || defaultGraph());
    },

    /**
     * Check if the store contains a specific quad
     * @param {Quad} q - Quad to check
     * @returns {boolean} True if quad exists
     */
    has(q) {
      return store.has(q);
    },

    /**
     * Get all quads matching a pattern
     * @param {Term} [subject] - Subject pattern
     * @param {Term} [predicate] - Predicate pattern
     * @param {Term} [object] - Object pattern
     * @param {Term} [graph] - Graph pattern
     * @returns {Array<Quad>} Matching quads
     */
    getQuads(subject, predicate, object, graph) {
      return store.getQuads(subject, predicate, object, graph);
    },

    /**
     * Get the size of the store
     * @returns {number} Number of quads
     */
    get size() {
      return store.size;
    },

    /**
     * Create an iterator over all quads
     * @returns {Iterator<Quad>} Quad iterator
     */
    [Symbol.iterator]() {
      return store[Symbol.iterator]();
    }
  };
}

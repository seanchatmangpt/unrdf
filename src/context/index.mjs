/**
 * @fileoverview Root context for unrdf using unctx
 * 
 * This module creates the root context that should be used at the application
 * level to provide store access to all composables.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { createContext } from "unctx";
import { AsyncLocalStorage } from "node:async_hooks";
import { Store, DataFactory } from "n3";
import { RdfEngine } from "../engines/rdf-engine.mjs";

const { namedNode, literal, blankNode, quad, defaultGraph } = DataFactory;

/**
 * Store context interface
 * @typedef {Object} StoreContext
 * @property {Store} store - The N3.Store instance
 * @property {RdfEngine} engine - The RDF engine
 * @property {Function} add - Add quads to store
 * @property {Function} remove - Remove quads from store
 * @property {Function} clear - Clear all quads
 * @property {Function} stats - Get store statistics
 * @property {Function} serialize - Serialize store
 * @property {Function} namedNode - Create named node
 * @property {Function} literal - Create literal
 * @property {Function} blankNode - Create blank node
 * @property {Function} quad - Create quad
 * @property {Function} has - Check if quad exists
 * @property {Function} getQuads - Get quads matching pattern
 * @property {number} size - Store size
 */

/**
 * Create the root store context using unctx with native async context support
 * This enables context preservation across async operations
 */
export const storeContext = createContext({
  asyncContext: true,
  AsyncLocalStorage,
});

/**
 * Hook to access the store context
 * @returns {StoreContext} Current store context
 * 
 * @throws {Error} If store context is not initialized
 */
export const useStoreContext = storeContext.use;

/**
 * Create a store context instance
 * @param {Array<Quad>} [initialQuads=[]] - Initial quads
 * @param {Object} [options] - Store options
 * @returns {StoreContext} Store context
 */
export function createStoreContext(initialQuads = [], options = {}) {
  // Input validation
  if (!Array.isArray(initialQuads)) {
    throw new TypeError("[createStoreContext] initialQuads must be an array");
  }
  
  if (options && typeof options !== 'object') {
    throw new TypeError("[createStoreContext] options must be an object");
  }

  const engine = new RdfEngine(options);
  const store = new Store(initialQuads);

  const context = {
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
     * @returns {StoreContext} This context for chaining
     * 
     * @throws {TypeError} If any quad is invalid
     */
    add(...quads) {
      for (const q of quads) {
        if (q === null || q === undefined) {
          throw new TypeError("[StoreContext] Cannot add null or undefined quad");
        }
        if (typeof q !== 'object' || !q.termType) {
          throw new TypeError("[StoreContext] Invalid quad: must have termType property");
        }
        store.add(q);
      }
      return this;
    },

    /**
     * Remove quads from the store
     * @param {...Quad} quads - Quads to remove
     * @returns {StoreContext} This context for chaining
     * 
     * @throws {TypeError} If any quad is invalid
     */
    remove(...quads) {
      for (const q of quads) {
        if (q === null || q === undefined) {
          throw new TypeError("[StoreContext] Cannot remove null or undefined quad");
        }
        if (typeof q !== 'object' || !q.termType) {
          throw new TypeError("[StoreContext] Invalid quad: must have termType property");
        }
        store.delete(q);
      }
      return this;
    },

    /**
     * Clear all quads from the store
     * @returns {StoreContext} This context for chaining
     */
    clear() {
      store.removeQuads([...store]);
      return this;
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
     * 
     * @throws {TypeError} If options is not an object
     * @throws {Error} If format is unsupported
     */
    async serialize(options = {}) {
      if (options && typeof options !== 'object') {
        throw new TypeError("[StoreContext] serialize options must be an object");
      }
      
      const { format = "Turtle", prefixes } = options;
      
      if (format === "Turtle") {
        return await engine.serializeTurtle(store, { prefixes });
      }
      if (format === "N-Quads") {
        return await engine.serializeNQuads(store);
      }
      
      throw new Error(`[StoreContext] Unsupported serialization format: ${format}`);
    },

    /**
     * Create a named node
     * @param {string} value - IRI value
     * @returns {NamedNode} Named node
     * 
     * @throws {TypeError} If value is not a string
     */
    namedNode(value) {
      if (typeof value !== 'string') {
        throw new TypeError("[StoreContext] namedNode value must be a string");
      }
      return namedNode(value);
    },

    /**
     * Create a literal
     * @param {string} value - Literal value
     * @param {string} [datatype] - Datatype IRI
     * @returns {Literal} Literal
     * 
     * @throws {TypeError} If value is not a string
     */
    literal(value, datatype) {
      if (typeof value !== 'string') {
        throw new TypeError("[StoreContext] literal value must be a string");
      }
      return literal(value, datatype);
    },

    /**
     * Create a blank node
     * @param {string} [value] - Blank node identifier
     * @returns {BlankNode} Blank node
     * 
     * @throws {TypeError} If value is provided but not a string
     */
    blankNode(value) {
      if (value !== undefined && typeof value !== 'string') {
        throw new TypeError("[StoreContext] blankNode value must be a string");
      }
      return blankNode(value);
    },

    /**
     * Create a quad
     * @param {Term} s - Subject
     * @param {Term} p - Predicate
     * @param {Term} o - Object
     * @param {Term} [g] - Graph
     * @returns {Quad} Quad
     * 
     * @throws {TypeError} If any required parameter is missing or invalid
     */
    quad(s, p, o, g) {
      if (!s || !p || !o) {
        throw new TypeError("[StoreContext] quad requires subject, predicate, and object");
      }
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

  return context;
}

/**
 * Initialize the root store context
 * This should be called at the root of your application
 * 
 * @param {Array<Quad>} [initialQuads=[]] - Initial quads
 * @param {Object} [options] - Store options
 * @returns {Function} Function to call with your application logic
 * 
 * @example
 * // At the root of your application
 * const runApp = initStore([], { baseIRI: 'http://example.org/' });
 * 
 * await runApp(async () => {
 *   // Your application code here
 *   const store = useStore();
 *   // All composables will use the same store
 * });
 */
export function initStore(initialQuads = [], options = {}) {
  const context = createStoreContext(initialQuads, options);
  
  return (fn) => {
    return storeContext.callAsync(context, fn);
  };
}

/**
 * Set the store context for the current execution context
 * This is useful when you need to set the context outside of initStore
 * 
 * @param {Array<Quad>} [initialQuads=[]] - Initial quads
 * @param {Object} [options] - Store options
 * @returns {StoreContext} The created context
 */
export function setStoreContext(initialQuads = [], options = {}) {
  const context = createStoreContext(initialQuads, options);
  storeContext.set(context);
  return context;
}

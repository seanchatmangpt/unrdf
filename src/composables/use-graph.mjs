/**
 * @fileoverview useGraph composable - high-level RDF graph operations with context
 * 
 * This composable provides the main interface for RDF operations.
 * It wraps a store with common graph operations like SPARQL queries,
 * set operations, and traversal utilities. Now uses unctx for store access.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { useStoreContext } from "../context/index.mjs";

/**
 * Create a graph composable for operating on the global RDF store
 * 
 * @returns {Object} Graph operations interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const graph = useGraph();
 *   
 *   // SPARQL SELECT query
 *   const results = await graph.select(`
 *     PREFIX ex: <http://example.org/>
 *     SELECT ?s ?p ?o WHERE { ?s ?p ?o }
 *   `);
 *   
 *   // SPARQL ASK query
 *   const exists = await graph.ask(`
 *     PREFIX ex: <http://example.org/>
 *     ASK { ex:subject ex:predicate ?o }
 *   `);
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useGraph() {
  // Get the store and engine from context
  const storeContext = useStoreContext();
  const storeInstance = storeContext.store;
  const rdfEngine = storeContext.engine;

  return {
    /**
     * The raw N3.Store instance being operated on
     * @type {Store}
     */
    get store() {
      return storeInstance;
    },

    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return rdfEngine;
    },

    /**
     * Execute any valid SPARQL 1.1 query
     * @param {string} sparql - SPARQL query string
     * @param {Object} [options] - Query options
     * @param {number} [options.limit] - Result limit
     * @param {AbortSignal} [options.signal] - Abort signal
     * @returns {Promise<Object>} Query result object
     * 
     * @throws {TypeError} If sparql is not a string
     */
    async query(sparql, options) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      return rdfEngine.query(sparql, options);
    },

    /**
     * Execute a SPARQL SELECT query
     * @param {string} sparql - SPARQL SELECT query string
     * @returns {Promise<Array<Object>>} Array of result bindings
     * 
     * @throws {TypeError} If sparql is not a string
     * @throws {Error} If query is not a SELECT query
     */
    async select(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = await rdfEngine.query(sparql);
      if (res.type !== "select") {
        throw new Error("[useGraph] Query is not a SELECT query");
      }
      return res.results;
    },

    /**
     * Execute a SPARQL ASK query
     * @param {string} sparql - SPARQL ASK query string
     * @returns {Promise<boolean>} Boolean result
     * 
     * @throws {TypeError} If sparql is not a string
     * @throws {Error} If query is not an ASK query
     */
    async ask(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = await rdfEngine.query(sparql);
      if (res.type !== "ask") {
        throw new Error("[useGraph] Query is not an ASK query");
      }
      return res.boolean;
    },

    /**
     * Execute a SPARQL CONSTRUCT query
     * @param {string} sparql - SPARQL CONSTRUCT query string
     * @returns {Promise<Store>} New store with constructed triples
     * 
     * @throws {TypeError} If sparql is not a string
     * @throws {Error} If query is not a CONSTRUCT query
     */
    async construct(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = await rdfEngine.query(sparql);
      if (res.type !== "construct") {
        throw new Error("[useGraph] Query is not a CONSTRUCT query");
      }
      return res.store;
    },

    /**
     * Execute a SPARQL UPDATE query
     * @param {string} sparql - SPARQL UPDATE query string
     * @returns {Promise<Object>} Update result
     * 
     * @throws {TypeError} If sparql is not a string
     * @throws {Error} If query is not an UPDATE query
     */
    async update(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = await rdfEngine.query(sparql);
      if (res.type !== "update") {
        throw new Error("[useGraph] Query is not an UPDATE query");
      }
      return res;
    },

    /**
     * Validate the graph against SHACL shapes
     * @param {string|Store} shapesInput - SHACL shapes as Turtle string or Store
     * @returns {Promise<Object>} Validation report
     */
    async validate(shapesInput) {
      return rdfEngine.validateShacl(storeInstance, shapesInput);
    },

    /**
     * Validate the graph against SHACL shapes, throw on failure
     * @param {string|Store} shapesInput - SHACL shapes
     * @returns {Promise<Object>} Validation report
     * @throws {Error} If validation fails
     */
    async validateOrThrow(shapesInput) {
      return rdfEngine.validateShaclOrThrow(storeInstance, shapesInput);
    },

    /**
     * Serialize the graph to a string
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
        throw new TypeError("[useGraph] serialize options must be an object");
      }
      
      const { format = "Turtle", prefixes } = options;
      
      if (format === "Turtle") {
        return await rdfEngine.serializeTurtle(storeInstance, { prefixes });
      }
      if (format === "N-Quads") {
        return await rdfEngine.serializeNQuads(storeInstance);
      }
      
      throw new Error(`[useGraph] Unsupported serialization format: ${format}`);
    },

    /**
     * Get a Clownface pointer for fluent graph traversal
     * @returns {Clownface} Clownface pointer
     */
    pointer() {
      return rdfEngine.getClownface(storeInstance);
    },

    /**
     * Get basic statistics about the graph
     * @returns {Object} Graph statistics
     */
    stats() {
      return rdfEngine.getStats(storeInstance);
    },

    /**
     * Check if this graph is isomorphic to another
     * @param {Object|Store} otherGraph - Another useGraph instance or Store
     * @returns {Promise<boolean>} True if isomorphic
     */
    async isIsomorphic(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      return rdfEngine.isIsomorphic(storeInstance, otherStore);
    },

    /**
     * Create a new graph containing the union of this graph and others
     * @param {...Object|Store} otherGraphs - Other useGraph instances or Stores
     * @returns {Object} New useGraph instance with union
     */
    union(...otherGraphs) {
      const otherStores = otherGraphs.map(g => g.store || g);
      const resultStore = rdfEngine.union(storeInstance, ...otherStores);
      // Create a temporary context for the result
      return createTemporaryGraph(resultStore, rdfEngine);
    },

    /**
     * Create a new graph containing quads in this graph but not in the other
     * @param {Object|Store} otherGraph - Another useGraph instance or Store
     * @returns {Object} New useGraph instance with difference
     */
    difference(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      const resultStore = rdfEngine.difference(storeInstance, otherStore);
      return createTemporaryGraph(resultStore, rdfEngine);
    },

    /**
     * Create a new graph containing only quads that exist in both graphs
     * @param {Object|Store} otherGraph - Another useGraph instance or Store
     * @returns {Object} New useGraph instance with intersection
     */
    intersection(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      const resultStore = rdfEngine.intersection(storeInstance, otherStore);
      return createTemporaryGraph(resultStore, rdfEngine);
    },

    /**
     * Skolemize blank nodes in the graph
     * @param {string} [baseIRI] - Base IRI for skolemization
     * @returns {Object} New useGraph instance with skolemized nodes
     */
    skolemize(baseIRI) {
      const resultStore = rdfEngine.skolemize(storeInstance, baseIRI);
      return createTemporaryGraph(resultStore, rdfEngine);
    },

    /**
     * Convert the graph to JSON-LD
     * @param {Object} [options] - Conversion options
     * @param {Object} [options.context] - JSON-LD context
     * @param {Object} [options.frame] - JSON-LD frame
     * @returns {Promise<Object>} JSON-LD document
     */
    async toJSONLD(options = {}) {
      return rdfEngine.toJSONLD(storeInstance, options);
    },

    /**
     * Get the size of the graph
     * @returns {number} Number of quads
     */
    get size() {
      return storeInstance.size;
    }
  };
}

/**
 * Create a temporary graph interface for a specific store
 * Used for operations that return new stores (union, difference, etc.)
 * @param {Store} store - The store to wrap
 * @param {RdfEngine} engine - The RDF engine to use
 * @returns {Object} Graph interface
 * @private
 */
function createTemporaryGraph(store, engine) {
  return {
    get store() {
      return store;
    },
    
    get engine() {
      return engine;
    },
    
    async query(sparql, options) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      try {
        return await engine.query(store, sparql, options);
      } catch (error) {
        throw new Error(`[useGraph] Query failed: ${error.message}`);
      }
    },
    
    async select(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = await engine.query(store, sparql);
      if (res.type !== "select") {
        throw new Error("[useGraph] Query is not a SELECT query");
      }
      return res.results;
    },
    
    async ask(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = await engine.query(store, sparql);
      if (res.type !== "ask") {
        throw new Error("[useGraph] Query is not an ASK query");
      }
      return res.boolean;
    },
    
    async construct(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = await engine.query(store, sparql);
      if (res.type !== "construct") {
        throw new Error("[useGraph] Query is not a CONSTRUCT query");
      }
      return res.store;
    },
    
    async update(sparql) {
      if (typeof sparql !== 'string') {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = await engine.query(store, sparql);
      if (res.type !== "update") {
        throw new Error("[useGraph] Query is not an UPDATE query");
      }
      return res;
    },
    
    async validate(shapesInput) {
      return engine.validateShacl(store, shapesInput);
    },
    
    async validateOrThrow(shapesInput) {
      return engine.validateShaclOrThrow(store, shapesInput);
    },
    
    async serialize(options = {}) {
      if (options && typeof options !== 'object') {
        throw new TypeError("[useGraph] serialize options must be an object");
      }
      
      const { format = "Turtle", prefixes } = options;
      
      if (format === "Turtle") {
        return await engine.serializeTurtle(store, { prefixes });
      }
      if (format === "N-Quads") {
        return await engine.serializeNQuads(store);
      }
      
      throw new Error(`[useGraph] Unsupported serialization format: ${format}`);
    },
    
    pointer() {
      return engine.getClownface(store);
    },
    
    stats() {
      return engine.getStats(store);
    },
    
    async isIsomorphic(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      return engine.isIsomorphic(store, otherStore);
    },
    
    union(...otherGraphs) {
      const otherStores = otherGraphs.map(g => g.store || g);
      const resultStore = engine.union(store, ...otherStores);
      return createTemporaryGraph(resultStore, engine);
    },
    
    difference(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      const resultStore = engine.difference(store, otherStore);
      return createTemporaryGraph(resultStore, engine);
    },
    
    intersection(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      const resultStore = engine.intersection(store, otherStore);
      return createTemporaryGraph(resultStore, engine);
    },
    
    skolemize(baseIRI) {
      const resultStore = engine.skolemize(store, baseIRI);
      return createTemporaryGraph(resultStore, engine);
    },
    
    async toJSONLD(options = {}) {
      return engine.toJSONLD(store, options);
    },
    
    get size() {
      return store.size;
    }
  };
}

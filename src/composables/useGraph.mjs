/**
 * @fileoverview useGraph composable - high-level RDF graph operations
 * 
 * This composable provides the main interface for RDF operations.
 * It wraps a store with common graph operations like SPARQL queries,
 * set operations, and traversal utilities.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { RdfEngine } from "../engines/RdfEngine.mjs";

// Create a single, shared instance of the engine for efficiency
const rdfEngine = new RdfEngine();

/**
 * Create a graph composable for operating on an RDF store
 * 
 * @param {Store} store - N3.Store instance to operate on
 * @returns {Object} Graph operations interface
 * 
 * @example
 * const store = useStore();
 * const graph = useGraph(store);
 * 
 * // SPARQL SELECT query
 * const results = await graph.select(`
 *   PREFIX ex: <http://example.org/>
 *   SELECT ?s ?p ?o WHERE { ?s ?p ?o }
 * `);
 * 
 * // SPARQL ASK query
 * const exists = await graph.ask(`
 *   PREFIX ex: <http://example.org/>
 *   ASK { ex:subject ex:predicate ?o }
 * `);
 * 
 * // Set operations
 * const otherGraph = useGraph(otherStore);
 * const union = graph.union(otherGraph);
 * const diff = graph.difference(otherGraph);
 */
export function useGraph(store) {
  if (!store || typeof store.getQuads !== "function") {
    throw new Error("[useGraph] An N3.Store instance must be provided.");
  }

  return {
    /**
     * The raw N3.Store instance being operated on
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
      return rdfEngine;
    },

    /**
     * Execute any valid SPARQL 1.1 query
     * @param {string} sparql - SPARQL query string
     * @param {Object} [options] - Query options
     * @param {number} [options.limit] - Result limit
     * @param {AbortSignal} [options.signal] - Abort signal
     * @returns {Promise<Object>} Query result object
     */
    async query(sparql, options) {
      return rdfEngine.query(store, sparql, options);
    },

    /**
     * Execute a SPARQL SELECT query
     * @param {string} sparql - SPARQL SELECT query string
     * @returns {Promise<Array<Object>>} Array of result bindings
     */
    async select(sparql) {
      const res = await rdfEngine.query(store, sparql);
      if (res.type !== "select") {
        throw new Error("Query is not a SELECT query.");
      }
      return res.results;
    },

    /**
     * Execute a SPARQL ASK query
     * @param {string} sparql - SPARQL ASK query string
     * @returns {Promise<boolean>} Boolean result
     */
    async ask(sparql) {
      const res = await rdfEngine.query(store, sparql);
      if (res.type !== "ask") {
        throw new Error("Query is not an ASK query.");
      }
      return res.boolean;
    },

    /**
     * Execute a SPARQL CONSTRUCT query
     * @param {string} sparql - SPARQL CONSTRUCT query string
     * @returns {Promise<Store>} New store with constructed triples
     */
    async construct(sparql) {
      const res = await rdfEngine.query(store, sparql);
      if (res.type !== "construct") {
        throw new Error("Query is not a CONSTRUCT query.");
      }
      return res.store;
    },

    /**
     * Execute a SPARQL UPDATE query
     * @param {string} sparql - SPARQL UPDATE query string
     * @returns {Promise<Object>} Update result
     */
    async update(sparql) {
      const res = await rdfEngine.query(store, sparql);
      if (res.type !== "update") {
        throw new Error("Query is not an UPDATE query.");
      }
      return res;
    },

    /**
     * Validate the graph against SHACL shapes
     * @param {string|Store} shapesInput - SHACL shapes as Turtle string or Store
     * @returns {Promise<Object>} Validation report
     */
    async validate(shapesInput) {
      return rdfEngine.validateShacl(store, shapesInput);
    },

    /**
     * Validate the graph against SHACL shapes, throw on failure
     * @param {string|Store} shapesInput - SHACL shapes
     * @returns {Promise<Object>} Validation report
     * @throws {Error} If validation fails
     */
    async validateOrThrow(shapesInput) {
      return rdfEngine.validateShaclOrThrow(store, shapesInput);
    },

    /**
     * Serialize the graph to a string
     * @param {Object} [options] - Serialization options
     * @param {string} [options.format='Turtle'] - Output format
     * @param {Object} [options.prefixes] - Prefix mappings
     * @returns {Promise<string>} Serialized string
     */
    async serialize(options = {}) {
      const { format = "Turtle", prefixes } = options;
      
      if (format === "Turtle") {
        return await rdfEngine.serializeTurtle(store, { prefixes });
      }
      if (format === "N-Quads") {
        return await rdfEngine.serializeNQuads(store);
      }
      
      throw new Error(`Unsupported serialization format: ${format}`);
    },

    /**
     * Get a Clownface pointer for fluent graph traversal
     * @returns {Clownface} Clownface pointer
     */
    pointer() {
      return rdfEngine.getClownface(store);
    },

    /**
     * Get basic statistics about the graph
     * @type {Object}
     */
    get stats() {
      return rdfEngine.getStats(store);
    },

    /**
     * Check if this graph is isomorphic to another
     * @param {Object|Store} otherGraph - Another useGraph instance or Store
     * @returns {Promise<boolean>} True if isomorphic
     */
    async isIsomorphic(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      return rdfEngine.isIsomorphic(store, otherStore);
    },

    /**
     * Create a new graph containing the union of this graph and others
     * @param {...Object|Store} otherGraphs - Other useGraph instances or Stores
     * @returns {Object} New useGraph instance with union
     */
    union(...otherGraphs) {
      const otherStores = otherGraphs.map(g => g.store || g);
      const resultStore = rdfEngine.union(store, ...otherStores);
      return useGraph(resultStore);
    },

    /**
     * Create a new graph containing quads in this graph but not in the other
     * @param {Object|Store} otherGraph - Another useGraph instance or Store
     * @returns {Object} New useGraph instance with difference
     */
    difference(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      const resultStore = rdfEngine.difference(store, otherStore);
      return useGraph(resultStore);
    },

    /**
     * Create a new graph containing only quads that exist in both graphs
     * @param {Object|Store} otherGraph - Another useGraph instance or Store
     * @returns {Object} New useGraph instance with intersection
     */
    intersection(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      const resultStore = rdfEngine.intersection(store, otherStore);
      return useGraph(resultStore);
    },

    /**
     * Skolemize blank nodes in the graph
     * @param {string} [baseIRI] - Base IRI for skolemization
     * @returns {Object} New useGraph instance with skolemized nodes
     */
    skolemize(baseIRI) {
      const resultStore = rdfEngine.skolemize(store, baseIRI);
      return useGraph(resultStore);
    },

    /**
     * Convert the graph to JSON-LD
     * @param {Object} [options] - Conversion options
     * @param {Object} [options.context] - JSON-LD context
     * @param {Object} [options.frame] - JSON-LD frame
     * @returns {Promise<Object>} JSON-LD document
     */
    async toJSONLD(options = {}) {
      return rdfEngine.toJSONLD(store, options);
    },

    /**
     * Get the size of the graph
     * @returns {number} Number of quads
     */
    get size() {
      return store.size;
    }
  };
}

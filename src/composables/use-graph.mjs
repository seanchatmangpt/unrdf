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
 *   const results = graph.select(`
 *     PREFIX ex: <http://example.org/>
 *     SELECT ?s ?p ?o WHERE { ?s ?p ?o }
 *   `);
 *
 *   // SPARQL ASK query
 *   const exists = graph.ask(`
 *     PREFIX ex: <http://example.org/>
 *     ASK { ex:subject ex:predicate ?o }
 *   `);
 * });
 *
 * @throws {Error} If store context is not initialized
 */
export function useGraph() {
  // Get the store context - now sender-only operations
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  const store = storeContext.store;

  return {
    /**
     * Execute any valid SPARQL 1.1 query
     * @param {string} sparql - SPARQL query string
     * @param {Object} [options] - Query options
     * @param {number} [options.limit] - Result limit
     * @param {AbortSignal} [options.signal] - Abort signal
     * @returns {Object} Query result object
     *
     * @throws {TypeError} If sparql is not a string
     * @note This is a READER operation - use sparingly
     */
    query(sparql, options) {
      if (typeof sparql !== "string") {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      return storeContext.query(sparql, options);
    },

    /**
     * Execute a SPARQL SELECT query
     * @param {string} sparql - SPARQL SELECT query string
     * @returns {Array<Object>} Array of result bindings
     *
     * @throws {TypeError} If sparql is not a string
     * @throws {Error} If query is not a SELECT query
     * @note This is a READER operation - use sparingly
     */
    select(sparql) {
      if (typeof sparql !== "string") {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = storeContext.query(sparql);
      if (res.type !== "select") {
        throw new Error("[useGraph] Query is not a SELECT query");
      }
      // In sender-only mode, queries return empty results
      // This is expected behavior for reader operations
      return res.results || [];
    },

    /**
     * Execute a SPARQL ASK query
     * @param {string} sparql - SPARQL ASK query string
     * @returns {boolean} Boolean result
     *
     * @throws {TypeError} If sparql is not a string
     * @throws {Error} If query is not an ASK query
     * @note This is a READER operation - use sparingly
     */
    ask(sparql) {
      if (typeof sparql !== "string") {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      console.log(
        "DEBUG: useGraph.ask called with:",
        sparql.substring(0, 50) + "...",
      );
      console.log("DEBUG: storeContext type:", typeof storeContext);
      console.log("DEBUG: storeContext has query:", typeof storeContext.query);
      const res = storeContext.query(sparql);
      if (res.type !== "ask") {
        throw new Error("[useGraph] Query is not an ASK query");
      }
      // In sender-only mode, ASK queries return false
      // This is expected behavior for reader operations
      return res.boolean || false;
    },

    /**
     * Execute a SPARQL CONSTRUCT query
     * @param {string} sparql - SPARQL CONSTRUCT query string
     * @returns {Store} New store with constructed triples
     *
     * @throws {TypeError} If sparql is not a string
     * @throws {Error} If query is not a CONSTRUCT query
     * @note This is a READER operation - use sparingly
     */
    construct(sparql) {
      if (typeof sparql !== "string") {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = storeContext.query(sparql);
      if (res.type !== "construct") {
        throw new Error("[useGraph] Query is not a CONSTRUCT query");
      }
      // In sender-only mode, CONSTRUCT queries return empty store
      // This is expected behavior for reader operations
      return res.store || new Store();
    },

    /**
     * Execute a SPARQL UPDATE query
     * @param {string} sparql - SPARQL UPDATE query string
     * @returns {Object} Update result
     *
     * @throws {TypeError} If sparql is not a string
     * @throws {Error} If query is not an UPDATE query
     */
    async update(sparql) {
      if (typeof sparql !== "string") {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = await storeContext.query(sparql);
      if (res.type !== "update") {
        throw new Error("[useGraph] Query is not an UPDATE query");
      }
      return res;
    },

    /**
     * Validate the graph against SHACL shapes
     * @param {string|Store} shapesInput - SHACL shapes as Turtle string or Store
     * @returns {Object} Validation report
     */
    validate(shapesInput) {
      return engine.validateShacl(store, shapesInput);
    },

    /**
     * Validate the graph against SHACL shapes, throw on failure
     * @param {string|Store} shapesInput - SHACL shapes
     * @returns {Object} Validation report
     * @throws {Error} If validation fails
     */
    validateOrThrow(shapesInput) {
      const result = engine.validateShacl(store, shapesInput);
      if (!result.conforms) {
        throw new Error(
          `SHACL validation failed: ${result.results.map((r) => r.message).join(", ")}`,
        );
      }
      return result;
    },

    /**
     * Serialize the graph to a string
     * @param {Object} [options] - Serialization options
     * @param {string} [options.format='Turtle'] - Output format
     * @param {Object} [options.prefixes] - Prefix mappings
     * @returns {string} Serialized string
     *
     * @throws {TypeError} If options is not an object
     * @throws {Error} If format is unsupported
     */
    serialize(options = {}) {
      if (options && typeof options !== "object") {
        throw new TypeError("[useGraph] serialize options must be an object");
      }

      const { format = "Turtle", prefixes } = options;

      if (format === "Turtle") {
        return engine.serializeTurtle(store, { prefixes });
      }
      if (format === "N-Quads") {
        return engine.serializeNQuads(store);
      }

      throw new Error(`[useGraph] Unsupported serialization format: ${format}`);
    },

    /**
     * Get a Clownface pointer for fluent graph traversal
     * @returns {Clownface} Clownface pointer
     */
    pointer() {
      return engine.getClownface(store);
    },

    /**
     * Get basic statistics about the graph
     * @returns {Object} Graph statistics
     */
    stats() {
      return engine.getStats(store);
    },

    /**
     * Check if this graph is isomorphic to another
     * @param {Object|Store} otherGraph - Another useGraph instance or Store
     * @returns {boolean} True if isomorphic
     */
    isIsomorphic(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      return engine.isIsomorphic(store, otherStore);
    },

    /**
     * Create a new graph containing the union of this graph and others
     * @param {...Object|Store} otherGraphs - Other useGraph instances or Stores
     * @returns {Object} New useGraph instance with union
     */
    union(...otherGraphs) {
      const otherStores = otherGraphs.map((g) => g.store || g);
      const resultStore = engine.union(store, ...otherStores);
      // Create a temporary context for the result
      return createTemporaryGraph(resultStore, engine);
    },

    /**
     * Create a new graph containing quads in this graph but not in the other
     * @param {Object|Store} otherGraph - Another useGraph instance or Store
     * @returns {Object} New useGraph instance with difference
     */
    difference(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      const resultStore = engine.difference(store, otherStore);
      return createTemporaryGraph(resultStore, engine);
    },

    /**
     * Create a new graph containing only quads that exist in both graphs
     * @param {Object|Store} otherGraph - Another useGraph instance or Store
     * @returns {Object} New useGraph instance with intersection
     */
    intersection(otherGraph) {
      const otherStore = otherGraph.store || otherGraph;
      const resultStore = engine.intersection(store, otherStore);
      return createTemporaryGraph(resultStore, engine);
    },

    /**
     * Skolemize blank nodes in the graph
     * @param {string} [baseIRI] - Base IRI for skolemization
     * @returns {Object} New useGraph instance with skolemized nodes
     */
    skolemize(baseIRI) {
      const resultStore = engine.skolemize(store, baseIRI);
      return createTemporaryGraph(resultStore, engine);
    },

    /**
     * Convert the graph to JSON-LD
     * @param {Object} [options] - Conversion options
     * @param {Object} [options.context] - JSON-LD context
     * @param {Object} [options.frame] - JSON-LD frame
     * @returns {Object} JSON-LD document
     */
    toJSONLD(options = {}) {
      return engine.toJSONLD(store, options);
    },

    /**
     * Get the size of the graph
     * @returns {number} Number of quads
     */
    get size() {
      return store.size;
    },
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

    query(sparql, options) {
      if (typeof sparql !== "string") {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      try {
        return engine.query(store, sparql, options);
      } catch (error) {
        throw new Error(`[useGraph] Query failed: ${error.message}`);
      }
    },

    select(sparql) {
      if (typeof sparql !== "string") {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = engine.query(store, sparql);
      if (res.type !== "select") {
        throw new Error("[useGraph] Query is not a SELECT query");
      }
      return res.results;
    },

    ask(sparql) {
      if (typeof sparql !== "string") {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = engine.query(store, sparql);
      if (res.type !== "ask") {
        throw new Error("[useGraph] Query is not an ASK query");
      }
      return res.boolean;
    },

    construct(sparql) {
      if (typeof sparql !== "string") {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = engine.query(store, sparql);
      if (res.type !== "construct") {
        throw new Error("[useGraph] Query is not a CONSTRUCT query");
      }
      return res.store;
    },

    update(sparql) {
      if (typeof sparql !== "string") {
        throw new TypeError("[useGraph] SPARQL query must be a string");
      }
      const res = engine.query(store, sparql);
      if (res.type !== "update") {
        throw new Error("[useGraph] Query is not an UPDATE query");
      }
      return res;
    },

    validate(shapesInput) {
      return engine.validateShacl(store, shapesInput);
    },

    validateOrThrow(shapesInput) {
      return engine.validateShaclOrThrow(store, shapesInput);
    },

    serialize(options = {}) {
      if (options && typeof options !== "object") {
        throw new TypeError("[useGraph] serialize options must be an object");
      }

      const { format = "Turtle", prefixes } = options;

      if (format === "Turtle") {
        return engine.serializeTurtle(store, { prefixes });
      }
      if (format === "N-Quads") {
        return engine.serializeNQuads(store);
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
      const otherStores = otherGraphs.map((g) => g.store || g);
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

    toJSONLD(options = {}) {
      return engine.toJSONLD(store, options);
    },

    get size() {
      return store.size;
    },
  };
}

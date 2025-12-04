/**
 * @file UnrdfStore - Persistent RDF Store with Oxigraph
 * @module @unrdf/core/rdf/unrdf-store
 */

import { OxigraphStore } from '@unrdf/oxigraph';
import { z } from 'zod';

/**
 * @typedef {Object} QueryOptions
 * @property {string} [baseIri] - Base IRI for relative IRI resolution
 * @property {string} [defaultGraph] - Default graph IRI
 * @property {string[]} [namedGraphs] - Named graph IRIs
 * @property {string} [resultsFormat] - Results format ('json', 'bindings', 'quads')
 * @property {number} [timeout] - Query timeout in milliseconds
 */

/**
 * Query options schema
 */
const QueryOptionsSchema = z
  .object({
    baseIri: z.string().optional(),
    defaultGraph: z.string().optional(),
    namedGraphs: z.array(z.string()).optional(),
    resultsFormat: z.enum(['json', 'bindings', 'quads']).optional(),
    timeout: z.number().positive().optional(),
  })
  .optional();

/**
 * SPARQL JSON Results format schema (W3C standard)
 * @see https://www.w3.org/TR/sparql11-results-json/
 * @deprecated Reserved for future use
 */
const _SparqlJsonResultsSchema = z.object({
  head: z.object({
    vars: z.array(z.string()),
  }),
  results: z.object({
    bindings: z.array(z.record(z.any())),
  }),
});

/**
 * UnrdfStore - Persistent RDF store wrapping Oxigraph
 *
 * Provides synchronous query execution for reactivity (computed()),
 * bulk operations, transactions, and comprehensive SPARQL support.
 *
 * @class
 * @example
 * const store = new UnrdfStore([quad1, quad2]);
 *
 * // Synchronous query (enables computed())
 * const results = store.query(`
 *   SELECT ?name WHERE { ?s foaf:name ?name }
 * `);
 *
 * // Async query
 * const results = await store.queryAsync(`
 *   SELECT ?name WHERE { ?s foaf:name ?name }
 * `);
 *
 * // Bulk operations
 * store.bulkAdd([quad1, quad2, quad3]);
 * store.bulkRemove([quad4, quad5]);
 *
 * // Transactions
 * store.transaction((txStore) => {
 *   txStore.add(quad1);
 *   txStore.add(quad2);
 * });
 */
export class UnrdfStore {
  /**
   * Create a new UnrdfStore
   * @param {Array} [quads] - Initial quads to populate the store
   * @param {Object} [options] - Store options
   */
  constructor(quads, options = {}) {
    this._store = new OxigraphStore(quads || []);
    this._version = 0; // For reactivity tracking
    this._options = options;
  }

  /**
   * Execute a SPARQL query synchronously
   *
   * Enables reactive computed() usage by executing queries synchronously.
   * Returns formatted results based on query type (SELECT, ASK, CONSTRUCT, DESCRIBE).
   *
   * @param {string} sparql - SPARQL query string
   * @param {QueryOptions} [options] - Query options
   * @returns {Array|boolean|Object} Query results
   *
   * @throws {TypeError} If sparql is not a string
   * @throws {Error} If query execution fails
   *
   * @example
   * // SELECT query
   * const results = store.query(`
   *   SELECT ?name WHERE { ?s foaf:name ?name }
   * `);
   * // Returns: [{ name: { type: 'Literal', value: 'Alice' } }, ...]
   *
   * // ASK query
   * const exists = store.query(`ASK { ?s foaf:name "Alice" }`);
   * // Returns: true or false
   *
   * // CONSTRUCT query
   * const quads = store.query(`
   *   CONSTRUCT { ?s foaf:name ?name }
   *   WHERE { ?s foaf:name ?name }
   * `);
   * // Returns: [quad1, quad2, ...]
   */
  query(sparql, options = {}) {
    if (typeof sparql !== 'string') {
      throw new TypeError('query: sparql must be a string');
    }

    // Validate options
    const validOptions = QueryOptionsSchema.parse(options);

    // Build Oxigraph query options
    const oxigraphOptions = {};
    if (validOptions?.baseIri) {
      oxigraphOptions.base_iri = validOptions.baseIri;
    }
    if (validOptions?.defaultGraph) {
      oxigraphOptions.default_graph = validOptions.defaultGraph;
    }
    if (validOptions?.namedGraphs) {
      oxigraphOptions.named_graphs = validOptions.namedGraphs;
    }

    // Execute query synchronously
    const queryResult = this._store.query(sparql, oxigraphOptions);

    // Determine query type
    const queryType = this._detectQueryType(sparql);

    // Format result based on query type
    return this._formatResult(queryResult, queryType, validOptions);
  }

  /**
   * Execute a SPARQL query asynchronously
   *
   * Async wrapper around synchronous query() for compatibility with async workflows.
   *
   * @param {string} sparql - SPARQL query string
   * @param {QueryOptions} [options] - Query options
   * @returns {Promise<Array|boolean|Object>} Query results
   *
   * @throws {TypeError} If sparql is not a string
   * @throws {Error} If query execution fails
   *
   * @example
   * const results = await store.queryAsync(`
   *   SELECT ?name WHERE { ?s foaf:name ?name }
   * `);
   */
  async queryAsync(sparql, options = {}) {
    return this.query(sparql, options);
  }

  /**
   * Add a quad to the store
   * @param {Object} quad - RDF quad to add
   * @returns {void}
   *
   * @throws {TypeError} If quad is invalid
   *
   * @example
   * store.add({
   *   subject: namedNode('http://example.org/alice'),
   *   predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
   *   object: literal('Alice')
   * });
   */
  add(quad) {
    if (!quad) {
      throw new TypeError('add: quad is required');
    }

    this._store.add(quad);
    this._version++;
  }

  /**
   * Remove a quad from the store
   * @param {Object} quad - RDF quad to remove
   * @returns {void}
   *
   * @throws {TypeError} If quad is invalid
   *
   * @example
   * store.delete(quad);
   */
  delete(quad) {
    if (!quad) {
      throw new TypeError('delete: quad is required');
    }

    this._store.delete(quad);
    this._version++;
  }

  /**
   * Add multiple quads in a single operation
   *
   * Bulk insert for better performance with large datasets.
   *
   * @param {Array<Object>} quads - Array of RDF quads to add
   * @returns {void}
   *
   * @throws {TypeError} If quads is not an array
   *
   * @example
   * store.bulkAdd([quad1, quad2, quad3]);
   */
  bulkAdd(quads) {
    if (!Array.isArray(quads)) {
      throw new TypeError('bulkAdd: quads must be an array');
    }

    for (const quad of quads) {
      this._store.add(quad);
    }
    this._version++;
  }

  /**
   * Remove multiple quads in a single operation
   *
   * Bulk delete for better performance with large datasets.
   *
   * @param {Array<Object>} quads - Array of RDF quads to remove
   * @returns {void}
   *
   * @throws {TypeError} If quads is not an array
   *
   * @example
   * store.bulkRemove([quad1, quad2, quad3]);
   */
  bulkRemove(quads) {
    if (!Array.isArray(quads)) {
      throw new TypeError('bulkRemove: quads must be an array');
    }

    for (const quad of quads) {
      this._store.delete(quad);
    }
    this._version++;
  }

  /**
   * Execute operations within a transaction
   *
   * Provides atomic operations - all succeed or all fail.
   * If the transaction function throws, all changes are rolled back.
   *
   * @param {Function} fn - Transaction function receiving store instance
   * @returns {void}
   *
   * @throws {TypeError} If fn is not a function
   * @throws {Error} If transaction fails
   *
   * @example
   * store.transaction((txStore) => {
   *   txStore.add(quad1);
   *   txStore.add(quad2);
   *   // All or nothing - if this throws, both adds are rolled back
   * });
   */
  transaction(fn) {
    if (typeof fn !== 'function') {
      throw new TypeError('transaction: fn must be a function');
    }

    // Snapshot current state
    const snapshot = this.match();

    try {
      // Execute transaction
      fn(this);
    } catch (error) {
      // Rollback on error
      this.clear();
      for (const quad of snapshot) {
        this._store.add(quad);
      }
      throw new Error(`Transaction failed: ${error.message}`);
    }
  }

  /**
   * Check if a quad exists in the store
   * @param {Object} quad - RDF quad to check
   * @returns {boolean} True if quad exists
   *
   * @example
   * if (store.has(quad)) {
   *   console.log('Quad exists');
   * }
   */
  has(quad) {
    return this._store.has(quad);
  }

  /**
   * Match quads by pattern
   * @param {Object} [subject] - Subject to match (null for any)
   * @param {Object} [predicate] - Predicate to match (null for any)
   * @param {Object} [object] - Object to match (null for any)
   * @param {Object} [graph] - Graph to match (null for any)
   * @returns {Array<Object>} Matching quads
   *
   * @example
   * // Get all quads
   * const all = store.match();
   *
   * // Get quads by subject
   * const aliceQuads = store.match(namedNode('http://example.org/alice'));
   */
  match(subject, predicate, object, graph) {
    return this._store.match(subject, predicate, object, graph);
  }

  /**
   * Get the size of the store (number of quads)
   * @returns {number} Number of quads
   *
   * @example
   * console.log(`Store has ${store.size()} quads`);
   */
  size() {
    return this._store.size();
  }

  /**
   * Clear all quads from the store
   * @returns {void}
   *
   * @example
   * store.clear();
   */
  clear() {
    this._store.clear();
    this._version++;
  }

  /**
   * Execute a SPARQL UPDATE query
   * @param {string} sparql - SPARQL UPDATE query string
   * @param {QueryOptions} [options] - Query options
   * @returns {void}
   *
   * @throws {TypeError} If sparql is not a string
   * @throws {Error} If update fails
   *
   * @example
   * store.update(`
   *   PREFIX foaf: <http://xmlns.com/foaf/0.1/>
   *   INSERT DATA { <http://example.org/alice> foaf:name "Alice" }
   * `);
   */
  update(sparql, options = {}) {
    if (typeof sparql !== 'string') {
      throw new TypeError('update: sparql must be a string');
    }

    // Validate options
    const validOptions = QueryOptionsSchema.parse(options);

    // Build Oxigraph update options
    const oxigraphOptions = {};
    if (validOptions?.baseIri) {
      oxigraphOptions.base_iri = validOptions.baseIri;
    }

    this._store.update(sparql, oxigraphOptions);
    this._version++;
  }

  /**
   * Load RDF data into the store
   * @param {string} data - Serialized RDF data
   * @param {Object} options - Load options
   * @param {string} options.format - RDF format (turtle, ntriples, etc)
   * @returns {void}
   *
   * @throws {TypeError} If data or format is invalid
   * @throws {Error} If load fails
   *
   * @example
   * store.load(turtleData, { format: 'turtle' });
   */
  load(data, options) {
    this._store.load(data, options);
    this._version++;
  }

  /**
   * Dump the store to serialized RDF format
   * @param {Object} options - Dump options
   * @param {string} options.format - RDF format (turtle, ntriples, etc)
   * @returns {string} Serialized RDF data
   *
   * @throws {TypeError} If format is invalid
   * @throws {Error} If dump fails
   *
   * @example
   * const turtle = store.dump({ format: 'turtle' });
   */
  dump(options) {
    return this._store.dump(options);
  }

  /**
   * Get the current version (for reactivity tracking)
   * @returns {number} Version number
   *
   * @example
   * const version = store.version;
   */
  get version() {
    return this._version;
  }

  /**
   * Detect SPARQL query type from query string
   * @private
   * @param {string} sparql - SPARQL query string
   * @returns {string} Query type (SELECT, ASK, CONSTRUCT, DESCRIBE)
   */
  _detectQueryType(sparql) {
    // Remove PREFIX declarations
    const queryWithoutPrefixes = sparql.replace(/PREFIX\s+[^\s]+\s+<[^>]+>/gim, '').trim();

    // Find first uppercase keyword
    const queryType =
      queryWithoutPrefixes
        .split(/\s+/)
        .find(word => word && /^[A-Z]/.test(word))
        ?.toUpperCase() || 'SELECT';

    return queryType;
  }

  /**
   * Format query results based on query type
   * @private
   * @param {*} queryResult - Raw query result from Oxigraph
   * @param {string} queryType - Query type (SELECT, ASK, CONSTRUCT, DESCRIBE)
   * @param {QueryOptions} [options] - Query options
   * @returns {Array|boolean|Object} Formatted results
   */
  _formatResult(queryResult, queryType, options = {}) {
    switch (queryType) {
      case 'SELECT': {
        return this._formatSelectResult(queryResult, options);
      }

      case 'CONSTRUCT':
      case 'DESCRIBE': {
        return this._formatConstructResult(queryResult, queryType);
      }

      case 'ASK': {
        return this._formatAskResult(queryResult);
      }

      default: {
        throw new Error(`Unsupported query type: ${queryType}`);
      }
    }
  }

  /**
   * Format SELECT query results
   * @private
   * @param {*} queryResult - Raw query result
   * @param {QueryOptions} [options] - Query options
   * @returns {Array|Object} Formatted SELECT results
   */
  _formatSelectResult(queryResult, options = {}) {
    // Handle array of Maps from Oxigraph
    const bindings = Array.isArray(queryResult)
      ? queryResult.map(item => {
          const row = {};

          // Handle Map objects from Oxigraph
          if (item instanceof Map) {
            for (const [key, val] of item.entries()) {
              row[key] = this._formatTerm(val);
            }
          } else if (item && typeof item === 'object') {
            // Fallback for plain objects
            for (const [key, val] of Object.entries(item)) {
              row[key] = this._formatTerm(val);
            }
          }

          return row;
        })
      : [];

    // Return SPARQL JSON Results format if requested
    if (options.resultsFormat === 'json') {
      // Extract variable names from first binding
      const vars = bindings.length > 0 ? Object.keys(bindings[0]) : [];

      return {
        head: { vars },
        results: { bindings },
      };
    }

    // Return bindings array by default
    return bindings;
  }

  /**
   * Format CONSTRUCT/DESCRIBE query results
   * @private
   * @param {*} queryResult - Raw query result
   * @param {string} _queryType - Query type (unused, reserved for future use)
   * @returns {Array} Array of quads
   */
  _formatConstructResult(queryResult, _queryType) {
    const quads = Array.isArray(queryResult) ? queryResult : [];
    return quads;
  }

  /**
   * Format ASK query results
   * @private
   * @param {*} queryResult - Raw query result
   * @returns {boolean} Boolean result
   */
  _formatAskResult(queryResult) {
    return typeof queryResult === 'boolean' ? queryResult : false;
  }

  /**
   * Format an RDF term to standard representation
   * @private
   * @param {*} term - RDF term from Oxigraph
   * @returns {Object} Formatted term
   */
  _formatTerm(term) {
    // Handle Oxigraph Term objects
    if (term && typeof term === 'object') {
      const type = term.termType || 'Literal';
      const value = term.value || term.toString();

      const formatted = { type, value };

      // Add language tag for literals
      if (term.language) {
        formatted.language = term.language;
      }

      // Add datatype for typed literals
      if (term.datatype) {
        formatted.datatype = term.datatype.value || term.datatype.toString();
      }

      return formatted;
    }

    // Fallback for primitives
    return {
      type: 'Literal',
      value: String(term),
    };
  }
}

/**
 * Create a new UnrdfStore instance
 *
 * Factory function for creating stores.
 *
 * @param {Array} [quads] - Initial quads to populate the store
 * @param {Object} [options] - Store options
 * @returns {UnrdfStore} New store instance
 *
 * @example
 * const store = createStore([quad1, quad2]);
 *
 * const results = store.query(`
 *   SELECT ?name WHERE { ?s foaf:name ?name }
 * `);
 */
export function createStore(quads, options = {}) {
  return new UnrdfStore(quads, options);
}

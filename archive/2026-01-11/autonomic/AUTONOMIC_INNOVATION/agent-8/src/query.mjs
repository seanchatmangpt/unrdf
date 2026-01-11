/**
 * Query Operations - SPARQL query wrappers for AtomicStore
 * @module agent-8/query
 */

import { UNIVERSE_GRAPH } from './store.mjs';

/**
 * Query store with SPARQL
 *
 * Executes SPARQL query against the store with optional configuration.
 * Returns results in the format appropriate for the query type:
 * - SELECT: Array of bindings
 * - ASK: Boolean
 * - CONSTRUCT/DESCRIBE: Array of quads
 *
 * @param {AtomicStore} store - Store to query
 * @param {string} sparql - SPARQL query string
 * @param {Object} [options] - Query options
 * @param {string} [options.baseIri] - Base IRI for resolution
 * @param {string} [options.defaultGraph] - Default graph IRI
 * @param {number} [options.timeout] - Query timeout in ms
 * @returns {Promise<Array|boolean|Object>} Query results
 * @throws {Error} If query execution fails
 *
 * @example
 * import { queryStore } from './query.mjs';
 *
 * const results = await queryStore(store, `
 *   SELECT ?name WHERE {
 *     ?person foaf:name ?name .
 *   }
 * `);
 *
 * console.log(results); // [{ name: { termType: 'Literal', value: 'Alice' } }, ...]
 */
export async function queryStore(store, sparql, options = {}) {
  if (!store || typeof store.query !== 'function') {
    throw new TypeError('Store must have a query method');
  }

  if (typeof sparql !== 'string') {
    throw new TypeError('SPARQL query must be a string');
  }

  try {
    // Execute query synchronously (UnrdfStore supports this)
    return store.query(sparql, options);
  } catch (error) {
    throw new Error(`Query execution failed: ${error.message}`);
  }
}

/**
 * Query with automatic graph context (Universe graph only)
 *
 * Automatically injects FROM clause to query only the Universe graph.
 * Useful for isolating application data from metadata graphs.
 *
 * @param {AtomicStore} store - Store to query
 * @param {string} sparql - SPARQL query (FROM clause auto-injected)
 * @returns {Promise<Array>} Results from Universe graph only
 * @throws {Error} If query execution fails
 *
 * @example
 * const results = await queryUniverse(store, `
 *   SELECT ?s ?p ?o WHERE {
 *     ?s ?p ?o .
 *   }
 * `);
 */
export async function queryUniverse(store, sparql) {
  if (typeof sparql !== 'string') {
    throw new TypeError('SPARQL query must be a string');
  }

  // Check if query already has FROM clause
  const hasFrom = /FROM\s+</i.test(sparql);

  let modifiedSparql = sparql;
  if (!hasFrom) {
    // Inject FROM clause after SELECT/CONSTRUCT/DESCRIBE/ASK
    modifiedSparql = sparql.replace(
      /^(\s*(?:SELECT|CONSTRUCT|DESCRIBE|ASK).*?WHERE)/is,
      `$1 FROM <${UNIVERSE_GRAPH.value}>`
    );
  }

  return queryStore(store, modifiedSparql);
}

/**
 * Count quads in store
 *
 * Returns the total number of quads in the store, optionally filtered by graph.
 *
 * @param {AtomicStore} store - Store to query
 * @param {string} [graph] - Optional graph filter (IRI string)
 * @returns {number} Quad count
 *
 * @example
 * const totalQuads = countQuads(store);
 * const universeQuads = countQuads(store, 'urn:autonomic:universe');
 */
export function countQuads(store, graph) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('Store must have a match method');
  }

  let count = 0;

  if (graph) {
    // Count quads in specific graph
    const graphNode = store._store ?
      { termType: 'NamedNode', value: graph } :
      null;

    for (const _quad of store.match(null, null, null, graphNode)) {
      count++;
    }
  } else {
    // Count all quads
    for (const _quad of store.match()) {
      count++;
    }
  }

  return count;
}

/**
 * Execute ASK query (returns boolean)
 *
 * Convenience method for ASK queries that returns a boolean result.
 *
 * @param {AtomicStore} store - Store to query
 * @param {string} sparql - ASK query
 * @returns {Promise<boolean>} True if pattern matches
 *
 * @example
 * const exists = await askQuery(store, `
 *   ASK WHERE { <http://ex.org/alice> ?p ?o }
 * `);
 * console.log(exists); // true or false
 */
export async function askQuery(store, sparql) {
  if (typeof sparql !== 'string') {
    throw new TypeError('SPARQL query must be a string');
  }

  if (!sparql.trim().toUpperCase().startsWith('ASK')) {
    throw new TypeError('Query must be an ASK query');
  }

  const result = await queryStore(store, sparql);
  return typeof result === 'boolean' ? result : false;
}

/**
 * Execute SELECT query (returns bindings)
 *
 * Convenience method for SELECT queries that always returns an array.
 *
 * @param {AtomicStore} store - Store to query
 * @param {string} sparql - SELECT query
 * @returns {Promise<Array>} Array of binding objects
 *
 * @example
 * const bindings = await selectQuery(store, `
 *   SELECT ?name WHERE { ?person foaf:name ?name }
 * `);
 */
export async function selectQuery(store, sparql) {
  if (typeof sparql !== 'string') {
    throw new TypeError('SPARQL query must be a string');
  }

  const result = await queryStore(store, sparql);
  return Array.isArray(result) ? result : [];
}

/**
 * Execute CONSTRUCT query (returns quads)
 *
 * Convenience method for CONSTRUCT queries that always returns an array of quads.
 *
 * @param {AtomicStore} store - Store to query
 * @param {string} sparql - CONSTRUCT query
 * @returns {Promise<Array>} Array of quads
 *
 * @example
 * const quads = await constructQuery(store, `
 *   CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }
 * `);
 */
export async function constructQuery(store, sparql) {
  if (typeof sparql !== 'string') {
    throw new TypeError('SPARQL query must be a string');
  }

  const result = await queryStore(store, sparql);
  return Array.isArray(result) ? result : [];
}

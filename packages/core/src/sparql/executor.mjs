/**
 * @file SPARQL Query Execution (Async API)
 * @module @unrdf/core/sparql/executor
 *
 * This module provides async wrappers around synchronous SPARQL execution.
 * For new code, consider using executor-sync.mjs directly for better performance.
 *
 * @deprecated Prefer synchronous executor-sync.mjs for better performance
 */

import {
  executeQuerySync,
  executeSelectSync,
  executeConstructSync,
  executeAskSync,
  prepareQuerySync,
} from './executor-sync.mjs';

/**
 * @typedef {import('n3').Store} Store
 */

/**
 * Execute a SPARQL query on a store
 * @param {Store} store - The store to query
 * @param {string} sparql - SPARQL query string
 * @param {Object} [options] - Query options
 * @param {number} [options.limit] - Result limit
 * @param {number} [options.offset] - Result offset
 * @param {AbortSignal} [options.signal] - Abort signal
 * @returns {Promise<Object>} Query results
 *
 * @throws {TypeError} If store is not valid
 * @throws {TypeError} If sparql is not a string
 * @throws {Error} If query execution fails
 *
 * @example
 * const store = createStore();
 * // ... add quads to store
 *
 * const results = await executeQuery(store, `
 *   PREFIX foaf: <http://xmlns.com/foaf/0.1/>
 *   SELECT ?name WHERE { ?s foaf:name ?name }
 * `);
 *
 * console.log('Results:', results.rows);
 */
export async function executeQuery(store, sparql, options = {}) {
  return executeQuerySync(store, sparql, options);
}

/**
 * Prepare a SPARQL query (parse and validate without executing)
 * @param {string} sparql - SPARQL query string
 * @returns {Promise<Object>} Query metadata
 *
 * @throws {TypeError} If sparql is not a string
 * @throws {Error} If query parsing fails
 *
 * @example
 * const metadata = await prepareQuery(`
 *   SELECT ?s ?p ?o WHERE { ?s ?p ?o }
 * `);
 *
 * console.log('Query type:', metadata.type);
 * console.log('Variables:', metadata.variables);
 */
export async function prepareQuery(sparql) {
  return prepareQuerySync(sparql);
}

/**
 * Execute a SPARQL SELECT query
 * @param {Store} store - The store to query
 * @param {string} sparql - SPARQL SELECT query string
 * @param {Object} [options] - Query options
 * @returns {Promise<Array<Object>>} Array of result bindings
 *
 * @throws {TypeError} If store or sparql is invalid
 * @throws {Error} If query is not a SELECT query
 *
 * @example
 * const results = await executeSelect(store, `
 *   SELECT ?name WHERE { ?s foaf:name ?name }
 * `);
 *
 * results.forEach(row => {
 *   console.log('Name:', row.name.value);
 * });
 */
export async function executeSelect(store, sparql, options = {}) {
  return executeSelectSync(store, sparql, options);
}

/**
 * Execute a SPARQL CONSTRUCT query
 * @param {Store} store - The store to query
 * @param {string} sparql - SPARQL CONSTRUCT query string
 * @param {Object} [options] - Query options
 * @returns {Promise<Array>} Array of constructed quads
 *
 * @throws {TypeError} If store or sparql is invalid
 * @throws {Error} If query is not a CONSTRUCT query
 *
 * @example
 * const quads = await executeConstruct(store, `
 *   CONSTRUCT { ?s foaf:name ?name }
 *   WHERE { ?s foaf:name ?name }
 * `);
 *
 * console.log('Constructed quads:', quads.length);
 */
export async function executeConstruct(store, sparql, options = {}) {
  return executeConstructSync(store, sparql, options);
}

/**
 * Execute a SPARQL ASK query
 * @param {Store} store - The store to query
 * @param {string} sparql - SPARQL ASK query string
 * @param {Object} [options] - Query options
 * @returns {Promise<boolean>} Boolean result
 *
 * @throws {TypeError} If store or sparql is invalid
 * @throws {Error} If query is not an ASK query
 *
 * @example
 * const exists = await executeAsk(store, `
 *   ASK { ?s foaf:name "Alice" }
 * `);
 *
 * if (exists) {
 *   console.log('Alice exists in the store');
 * }
 */
export async function executeAsk(store, sparql, options = {}) {
  return executeAskSync(store, sparql, options);
}

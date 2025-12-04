/**
 * @file SPARQL Query Execution
 * @module @unrdf/core/sparql/executor
 */

import { createStore } from '@unrdf/oxigraph';
import { z } from 'zod';

/**
 * @typedef {import('n3').Store} Store
 */

/**
 * Query options schema
 */
const QueryOptionsSchema = z
  .object({
    limit: z.number().optional(),
    offset: z.number().optional(),
    signal: z.instanceof(AbortSignal).optional(),
  })
  .optional();

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
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('executeQuery: store must be a valid Store instance');
  }

  if (typeof sparql !== 'string') {
    throw new TypeError('executeQuery: sparql must be a string');
  }

  // Validate options
  QueryOptionsSchema.parse(options);

  try {
    // Convert n3.Store to substrate store
    const quads = store.getQuads();
    const rdfStore = createStore(Array.from(quads));

    // Determine query type (skip PREFIX declarations)
    const queryWithoutPrefixes = sparql.replace(/PREFIX\s+[^\s]+\s+<[^>]+>/gm, '').trim();
    const queryType =
      queryWithoutPrefixes
        .split(/\s+/)
        .find(word => word && /^[A-Z]/.test(word))
        ?.toUpperCase() || 'SELECT';

    // Execute query synchronously
    const queryResult = rdfStore.query(sparql);

    switch (queryType) {
      case 'SELECT': {
        // SELECT query - return array of bindings
        const rows = Array.isArray(queryResult)
          ? queryResult.map(item => {
              const row = {};

              // Handle Map objects from Oxigraph
              if (item instanceof Map) {
                for (const [key, val] of item.entries()) {
                  // Oxigraph returns Term objects (Literal, NamedNode, etc)
                  if (val && typeof val === 'object') {
                    row[key] = {
                      type: val.termType || 'Literal',
                      value: val.value || val.toString(),
                    };
                  } else {
                    row[key] = {
                      type: 'Literal',
                      value: val,
                    };
                  }
                }
              } else if (item && typeof item === 'object') {
                // Fallback for plain objects
                for (const [key, val] of Object.entries(item)) {
                  row[key] = {
                    type: 'Literal',
                    value: val && val.value ? val.value : val,
                  };
                }
              } else {
                return item;
              }

              return row;
            })
          : [];

        // Return as array-like object with type property for compatibility
        rows.type = 'select';
        rows.rows = rows;
        return rows;
      }

      case 'CONSTRUCT':
      case 'DESCRIBE': {
        // CONSTRUCT/DESCRIBE query - return array of quads
        const quads = Array.isArray(queryResult) ? queryResult : [];
        quads.type = queryType.toLowerCase();
        quads.quads = quads;
        return quads;
      }

      case 'ASK': {
        // ASK query - return boolean
        const result = typeof queryResult === 'boolean' ? queryResult : false;
        return result;
      }

      default: {
        throw new Error(`Unsupported query type: ${queryType}`);
      }
    }
  } catch (error) {
    throw new Error(`SPARQL query execution failed: ${error.message}`);
  }
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
  if (typeof sparql !== 'string') {
    throw new TypeError('prepareQuery: sparql must be a string');
  }

  try {
    // Basic query type detection
    const trimmed = sparql.trim().toUpperCase();
    let type = 'unknown';

    if (trimmed.includes('SELECT')) {
      type = 'select';
    } else if (trimmed.includes('CONSTRUCT')) {
      type = 'construct';
    } else if (trimmed.includes('ASK')) {
      type = 'ask';
    } else if (trimmed.includes('DESCRIBE')) {
      type = 'describe';
    }

    // Extract variables
    const variables = [];
    const varMatches = sparql.match(/\?(\w+)/g);
    if (varMatches) {
      const uniqueVars = new Set(varMatches.map(v => v.slice(1)));
      variables.push(...uniqueVars);
    }

    // Extract prefixes
    const prefixes = {};
    const prefixMatches = sparql.matchAll(/PREFIX\s+(\w+):\s*<([^>]+)>/gi);
    for (const match of prefixMatches) {
      prefixes[match[1]] = match[2];
    }

    return {
      type,
      variables,
      prefixes,
      query: sparql,
    };
  } catch (error) {
    throw new Error(`Query preparation failed: ${error.message}`);
  }
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
  const result = await executeQuery(store, sparql, options);

  if (result.type !== 'select') {
    throw new Error('Query is not a SELECT query');
  }

  return result.rows;
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
  const result = await executeQuery(store, sparql, options);

  if (result.type !== 'construct') {
    throw new Error('Query is not a CONSTRUCT query');
  }

  return result.quads;
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
  const result = await executeQuery(store, sparql, options);

  if (typeof result !== 'boolean') {
    throw new Error('Query is not an ASK query');
  }

  return result;
}

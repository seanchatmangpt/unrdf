/**
 * @file SPARQL querying utilities.
 * @module query
 */

import { Store } from 'n3';
import { createStore } from '@unrdf/oxigraph';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf');

/**
 * Run a SPARQL query against a store.
 * @param {Store} store - The n3.Store to query against
 * @param {string} sparql - The SPARQL query string
 * @param {Object} [options] - Query options (for compatibility)
 * @param {number} [options.limit] - Maximum number of results
 * @returns {Promise<any>} Promise resolving to the query result
 *
 * @throws {Error} If query execution fails
 *
 * @example
 * const store = new Store();
 * // ... add quads to store
 *
 * const results = await query(store, `
 *   SELECT ?s ?o WHERE {
 *     ?s <http://example.org/knows> ?o
 *   }
 * `);
 * console.log(results); // Array of binding objects
 */
export async function query(store, sparql, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('query: store must be a valid Store instance');
  }
  if (typeof sparql !== 'string' || !sparql.trim()) {
    throw new TypeError('query: sparql must be a non-empty string');
  }

  return tracer.startActiveSpan('query.sparql', async span => {
    const startTime = performance.now();
    try {
      const queryType = sparql.trim().split(/\s+/)[0].toUpperCase();
      span.setAttributes({
        'query.type': queryType,
        'query.length': sparql.length,
        'query.store_size': store.size,
      });

      // Convert n3.Store quads to substrate store (synchronous)
      const quads = store.getQuads();
      const rdfStore = createStore(Array.from(quads));

      // Execute query synchronously
      let result;
      try {
        const queryResult = rdfStore.query(sparql);

        // Determine result type and format accordingly
        if (Array.isArray(queryResult)) {
          // SELECT query result (array of bindings)
          if (queryResult.length > 0 && typeof queryResult[0] === 'object' && !queryResult[0].subject) {
            // Already formatted bindings
            result = queryResult;
          } else {
            // Check query type to determine formatting
            if (queryType === 'CONSTRUCT' || queryType === 'DESCRIBE') {
              // Return as new Store
              result = new Store(queryResult);
            } else {
              // SELECT - format as bindings array
              result = queryResult.map(item => {
                if (item instanceof Map) {
                  // Handle Map objects from Oxigraph
                  const bindings = {};
                  for (const [key, val] of item.entries()) {
                    bindings[key] = val && val.value ? val.value : (val && val.toString ? val.toString() : val);
                  }
                  return bindings;
                } else if (item && typeof item === 'object') {
                  return Object.fromEntries(
                    Object.entries(item).map(([k, v]) => [
                      k,
                      v && v.value ? v.value : v,
                    ])
                  );
                }
                return item;
              });
            }
          }
          span.setAttribute('query.result_count', result instanceof Store ? result.size : result.length);
        } else if (typeof queryResult === 'boolean') {
          // ASK query
          result = queryResult;
          span.setAttribute('query.result_type', 'boolean');
          span.setAttribute('query.result', result);
        } else {
          // CONSTRUCT/DESCRIBE - should be array of quads
          result = new Store(Array.isArray(queryResult) ? queryResult : []);
          span.setAttribute('query.result_count', result.size);
        }
      } catch (engineError) {
        // If query execution fails, provide helpful error
        throw new Error(`Query execution failed: ${engineError.message}`);
      }

      span.setStatus({ code: SpanStatusCode.OK });
      const duration = performance.now() - startTime;
      span.setAttribute('query.duration_ms', duration);
      return result;
    } catch (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      throw new Error(`SPARQL query failed: ${error.message}`);
    } finally {
      span.end();
    }
  });
}

/**
 * Execute a SELECT query and return bindings.
 * @param {Store} store - The store to query against
 * @param {string} sparql - The SPARQL SELECT query
 * @param {Object} [options] - Query options
 * @returns {Promise<Array<Object>>} Promise resolving to array of binding objects
 *
 * @throws {Error} If query execution fails
 *
 * @example
 * const bindings = await select(store, `
 *   SELECT ?name ?age WHERE {
 *     ?person <http://example.org/name> ?name ;
 *              <http://example.org/age> ?age .
 *   }
 * `);
 */
export async function select(store, sparql, options = {}) {
  const result = await query(store, sparql, options);
  if (Array.isArray(result)) {
    return result;
  }
  throw new Error('SELECT query did not return bindings');
}

/**
 * Execute an ASK query and return boolean result.
 * @param {Store} store - The store to query against
 * @param {string} sparql - The SPARQL ASK query
 * @param {Object} [options] - Query options
 * @returns {Promise<boolean>} Promise resolving to boolean result
 *
 * @throws {Error} If query execution fails
 *
 * @example
 * const hasData = await ask(store, `
 *   ASK WHERE {
 *     ?s ?p ?o .
 *   }
 * `);
 */
export async function ask(store, sparql, options = {}) {
  const result = await query(store, sparql, options);
  if (typeof result === 'boolean') {
    return result;
  }
  throw new Error('ASK query did not return boolean result');
}

/**
 * Execute a CONSTRUCT query and return a new store.
 * @param {Store} store - The store to query against
 * @param {string} sparql - The SPARQL CONSTRUCT query
 * @param {Object} [options] - Query options
 * @returns {Promise<Store>} Promise resolving to a new store with constructed quads
 *
 * @throws {Error} If query execution fails
 *
 * @example
 * const constructed = await construct(store, `
 *   CONSTRUCT {
 *     ?person <http://example.org/type> <http://example.org/Person> .
 *   } WHERE {
 *     ?person <http://example.org/name> ?name .
 *   }
 * `);
 */
export async function construct(store, sparql, options = {}) {
  const result = await query(store, sparql, options);
  if (result && typeof result.getQuads === 'function') {
    return result;
  }
  throw new Error('CONSTRUCT query did not return a store');
}

/**
 * Execute a DESCRIBE query and return a new store.
 * @param {Store} store - The store to query against
 * @param {string} sparql - The SPARQL DESCRIBE query
 * @param {Object} [options] - Query options
 * @returns {Promise<Store>} Promise resolving to a new store with described quads
 *
 * @throws {Error} If query execution fails
 *
 * @example
 * const described = await describe(store, `
 *   DESCRIBE <http://example.org/alice>
 * `);
 */
export async function describe(store, sparql, options = {}) {
  const result = await query(store, sparql, options);
  if (result && typeof result.getQuads === 'function') {
    return result;
  }
  throw new Error('DESCRIBE query did not return a store');
}

/**
 * Execute a SPARQL UPDATE operation (INSERT, DELETE, etc.).
 * @param {Store} store - The store to update
 * @param {string} sparql - The SPARQL UPDATE query
 * @param {Object} [options] - Update options
 * @returns {Promise<Store>} Promise resolving to the updated store
 *
 * @throws {Error} If update execution fails
 *
 * @example
 * const updated = await update(store, `
 *   INSERT DATA {
 *     <http://example.org/alice> <http://example.org/age> "30" .
 *   }
 * `);
 */
export async function update(store, sparql, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('update: store must be a valid Store instance');
  }
  if (typeof sparql !== 'string' || !sparql.trim()) {
    throw new TypeError('update: sparql must be a non-empty string');
  }

  try {
    // Convert n3.Store to substrate store for update
    const quads = store.getQuads();
    const rdfStore = createStore(Array.from(quads));

    // Execute update synchronously
    rdfStore.query(sparql);

    // Return the updated store
    return store;
  } catch (error) {
    throw new Error(`SPARQL update failed: ${error.message}`);
  }
}

/**
 * Get query execution statistics.
 * @param {Store} store - The store to query against
 * @param {string} sparql - The SPARQL query
 * @param {Object} [options] - Query options
 * @returns {Promise<Object>} Promise resolving to execution statistics
 *
 * @example
 * const stats = await getQueryStats(store, sparql);
 * console.log(`Execution time: ${stats.duration}ms`);
 * console.log(`Result count: ${stats.resultCount}`);
 */
export async function getQueryStats(store, sparql, options = {}) {
  const startTime = Date.now();

  try {
    const result = await query(store, sparql, options);
    const endTime = Date.now();

    let resultCount = 0;
    if (Array.isArray(result)) {
      resultCount = result.length;
    } else if (result && typeof result.getQuads === 'function') {
      resultCount = result.size;
    } else if (typeof result === 'boolean') {
      resultCount = 1;
    }

    return {
      duration: endTime - startTime,
      resultCount,
      success: true,
    };
  } catch (error) {
    const endTime = Date.now();
    return {
      duration: endTime - startTime,
      resultCount: 0,
      success: false,
      error: error.message,
    };
  }
}

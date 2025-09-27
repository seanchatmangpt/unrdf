/**
 * @file SPARQL querying utilities.
 * @module query
 */

import { QueryEngine } from '@comunica/query-sparql';
import { Store } from 'n3';

// Create a new QueryEngine instance for each query to prevent hanging
function createQueryEngine() {
  return new QueryEngine();
}

/**
 * Run a SPARQL query against a store.
 * @param {Store} store - The store to query against
 * @param {string} sparql - The SPARQL query string
 * @param {Object} [options] - Query options
 * @param {number} [options.limit] - Maximum number of results
 * @param {AbortSignal} [options.signal] - Abort signal for cancellation
 * @param {boolean} [options.deterministic] - Enable deterministic results
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

  try {
    const queryOptions = {
      sources: [store],
      ...options
    };

    const comunica = createQueryEngine();
    const res = await comunica.query(sparql, queryOptions);
    
    switch (res.resultType) {
      case 'bindings': {
        const executed = await res.execute();
        const rows = [];
        for await (const binding of executed) {
          rows.push(Object.fromEntries([...binding].map(([k, v]) => [k.value, v.value])));
        }
        return rows;
      }
      case 'boolean':
        return res.booleanResult ?? (await res.execute());
      case 'quads': {
        const executed = await res.execute();
        const quads = [];
        for await (const quad of executed) {
          quads.push(quad);
        }
        return new Store(quads);
      }
      default:
        throw new Error(`Unsupported query type: ${res.resultType}`);
    }
  } catch (error) {
    throw new Error(`SPARQL query failed: ${error.message}`);
  }
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
    const source = { type: 'rdfjsSource', value: store };
    const updateOptions = {
      sources: [source],
      ...options
    };

    const comunica = createQueryEngine();
    await comunica.query(sparql, updateOptions);
    
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
      success: true
    };
  } catch (error) {
    const endTime = Date.now();
    return {
      duration: endTime - startTime,
      resultCount: 0,
      success: false,
      error: error.message
    };
  }
}

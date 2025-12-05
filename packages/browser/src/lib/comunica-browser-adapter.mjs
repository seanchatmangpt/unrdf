/**
 * @fileoverview Browser adapter for SPARQL queries
 *
 * Adapts SPARQL engine for browser environment:
 * - Uses IndexedDB quad store as data source
 * - Supports SELECT, ASK, CONSTRUCT queries
 * - Synchronous query execution
 *
 * @module browser/browser-adapter
 */

import { createStore } from '@unrdf/oxigraph';
import { IndexedDBQuadStore } from './indexeddb-store.mjs';

/**
 * Browser-compatible SPARQL query executor
 *
 * @example
 * const executor = new BrowserQueryExecutor();
 * await executor.init();
 *
 * const results = await executor.query(`
 *   SELECT ?subject ?name WHERE {
 *     ?subject <http://xmlns.com/foaf/0.1/name> ?name .
 *   }
 * `);
 */
export class BrowserQueryExecutor {
  /**
   *
   */
  constructor(store = null) {
    this.store = store || new IndexedDBQuadStore();
    this.initialized = false;
  }

  /**
   * Initialize query executor
   * @returns {Promise<void>}
   */
  async init() {
    if (this.initialized) return;

    await this.store.init();
    this.initialized = true;
  }

  /**
   * Execute SPARQL query
   * @param {string} queryString - SPARQL query string
   * @param {Object} [_options] - Query options
   * @returns {Promise<Object>} Query results
   */
  async query(queryString, _options = {}) {
    await this.init();

    try {
      // Get quads from IndexedDB store
      const quads = await this.store.match({});

      // Create substrate store from quads
      const rdfStore = createStore(quads);

      // Execute query synchronously
      const queryResult = rdfStore.query(queryString);

      // Determine query type and format result
      const queryType = queryString.trim().split(/\s+/)[0].toUpperCase();

      if (queryType === 'SELECT') {
        return this.handleSelectResult(queryResult);
      } else if (queryType === 'ASK') {
        return this.handleAskResult(queryResult);
      } else if (queryType === 'CONSTRUCT' || queryType === 'DESCRIBE') {
        return this.handleConstructResult(queryResult);
      } else {
        throw new Error(`Unsupported query type: ${queryType}`);
      }
    } catch (error) {
      throw new Error(`Query execution failed: ${error.message}`);
    }
  }

  /**
   * Handle SELECT query results (bindings)
   * @private
   * @param {Array} result - Query result
   * @returns {Object} Bindings array
   */
  handleSelectResult(result) {
    const bindings = Array.isArray(result)
      ? result.map(item => {
          if (item instanceof Map) {
            // Handle Map objects from Oxigraph
            const binding = {};
            for (const [key, val] of item.entries()) {
              binding[key] =
                val && val.value ? val.value : val && val.toString ? val.toString() : val;
            }
            return binding;
          } else if (item && typeof item === 'object') {
            return Object.fromEntries(
              Object.entries(item).map(([k, v]) => [k, v && v.value ? v.value : v])
            );
          }
          return item;
        })
      : [];

    return {
      type: 'bindings',
      bindings,
      variables: bindings.length > 0 ? Object.keys(bindings[0]) : [],
    };
  }

  /**
   * Handle ASK query results (boolean)
   * @private
   * @param {boolean} result - Query result
   * @returns {Object} Boolean result
   */
  handleAskResult(result) {
    return {
      type: 'boolean',
      value: typeof result === 'boolean' ? result : false,
    };
  }

  /**
   * Handle CONSTRUCT/DESCRIBE query results (quads)
   * @private
   * @param {Array} result - Query result
   * @returns {Object} Quads result
   */
  handleConstructResult(result) {
    const quads = Array.isArray(result) ? result : [];

    return {
      type: 'quads',
      quads,
      size: quads.length,
    };
  }

  /**
   * Execute SELECT query
   * @param {string} queryString - SPARQL SELECT query
   * @returns {Promise<Array<Object>>} Query results
   */
  async select(queryString) {
    const result = await this.query(queryString);

    if (result.type !== 'bindings') {
      throw new Error('Expected SELECT query result');
    }

    return result.bindings;
  }

  /**
   * Execute ASK query
   * @param {string} queryString - SPARQL ASK query
   * @returns {Promise<boolean>} Query result
   */
  async ask(queryString) {
    const result = await this.query(queryString);

    if (result.type !== 'boolean') {
      throw new Error('Expected ASK query result');
    }

    return result.value;
  }

  /**
   * Execute CONSTRUCT query
   * @param {string} queryString - SPARQL CONSTRUCT query
   * @returns {Promise<Array<Object>>} Constructed quads
   */
  async construct(queryString) {
    const result = await this.query(queryString);

    if (result.type !== 'quads') {
      throw new Error('Expected CONSTRUCT query result');
    }

    return result.quads;
  }

  /**
   * Get quad store
   * @returns {IndexedDBQuadStore} Quad store
   */
  getStore() {
    return this.store;
  }

  /**
   * Close executor and cleanup
   */
  async close() {
    if (this.store) {
      this.store.close();
    }
    this.initialized = false;
  }
}

/**
 * Create browser query executor
 * @param {IndexedDBQuadStore} [store] - Optional quad store
 * @returns {Promise<BrowserQueryExecutor>} Query executor
 */
export async function createBrowserQueryExecutor(store) {
  const executor = new BrowserQueryExecutor(store);
  await executor.init();
  return executor;
}

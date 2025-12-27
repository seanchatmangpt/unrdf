/**
 * @fileoverview Comunica browser adapter for SPARQL queries
 *
 * Adapts Comunica query engine for browser environment:
 * - Uses IndexedDB quad store as data source
 * - Supports SELECT, ASK, CONSTRUCT queries
 * - Handles async iteration in browser
 *
 * @module browser/comunica-browser-adapter
 */

import { QueryEngine } from '@comunica/query-sparql';
import { IndexedDBQuadStore } from './indexeddb-store.mjs';

/**
 * Browser-compatible Comunica query executor
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
    this.engine = new QueryEngine();
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
   * Create source descriptor for Comunica
   * @private
   * @returns {Object} Source descriptor
   */
  createSource() {
    return {
      type: 'rdfjsSource',
      value: {
        match: async (s, p, o, g) => {
          const pattern = {};
          if (s) pattern.subject = s;
          if (p) pattern.predicate = p;
          if (o) pattern.object = o;
          if (g) pattern.graph = g;

          const quads = await this.store.match(pattern);

          // Return async iterable
          return {
            [Symbol.asyncIterator]: async function* () {
              for (const quad of quads) {
                yield quad;
              }
            },
          };
        },
      },
    };
  }

  /**
   * Execute SPARQL query
   * @param {string} queryString - SPARQL query string
   * @param {Object} [options] - Query options
   * @returns {Promise<Object>} Query results
   */
  async query(queryString, options = {}) {
    await this.init();

    const sources = [this.createSource()];

    try {
      const result = await this.engine.query(queryString, {
        sources,
        ...options,
      });

      // Handle different query types
      if (result.type === 'bindings') {
        return this.handleBindingsResult(result);
      } else if (result.type === 'boolean') {
        return this.handleBooleanResult(result);
      } else if (result.type === 'quads') {
        return this.handleQuadsResult(result);
      } else {
        throw new Error(`Unsupported query result type: ${result.type}`);
      }
    } catch (error) {
      throw new Error(`Query execution failed: ${error.message}`);
    }
  }

  /**
   * Handle SELECT query results (bindings)
   * @private
   * @param {Object} result - Query result
   * @returns {Promise<Array<Object>>} Bindings array
   */
  async handleBindingsResult(result) {
    const bindings = [];

    for await (const binding of result.bindingsStream) {
      const row = {};
      for (const [variable, term] of binding) {
        row[variable.value] = this.termToValue(term);
      }
      bindings.push(row);
    }

    return {
      type: 'bindings',
      bindings,
      variables: [...(result.metadata?.variables || [])].map(v => v.value),
    };
  }

  /**
   * Handle ASK query results (boolean)
   * @private
   * @param {Object} result - Query result
   * @returns {Promise<Object>} Boolean result
   */
  async handleBooleanResult(result) {
    const boolean = await result.booleanResult;

    return {
      type: 'boolean',
      value: boolean,
    };
  }

  /**
   * Handle CONSTRUCT/DESCRIBE query results (quads)
   * @private
   * @param {Object} result - Query result
   * @returns {Promise<Array<Object>>} Quads array
   */
  async handleQuadsResult(result) {
    const quads = [];

    for await (const quad of result.quadStream) {
      quads.push(quad);
    }

    return {
      type: 'quads',
      quads,
    };
  }

  /**
   * Convert RDF term to JavaScript value
   * @private
   * @param {Object} term - RDF term
   * @returns {*} JavaScript value
   */
  termToValue(term) {
    if (!term) return null;

    if (term.termType === 'NamedNode') {
      return term.value;
    } else if (term.termType === 'Literal') {
      // Try to parse typed literals
      if (term.datatype) {
        const datatype = term.datatype.value;

        if (datatype === 'http://www.w3.org/2001/XMLSchema#integer') {
          return parseInt(term.value, 10);
        } else if (
          datatype === 'http://www.w3.org/2001/XMLSchema#decimal' ||
          datatype === 'http://www.w3.org/2001/XMLSchema#double'
        ) {
          return parseFloat(term.value);
        } else if (datatype === 'http://www.w3.org/2001/XMLSchema#boolean') {
          return term.value === 'true';
        } else if (datatype === 'http://www.w3.org/2001/XMLSchema#dateTime') {
          return new Date(term.value);
        }
      }

      return term.value;
    } else if (term.termType === 'BlankNode') {
      return `_:${term.value}`;
    }

    return term.value;
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

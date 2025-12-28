/**
 * @fileoverview SPARQL query execution for knowledge hooks
 * @module hooks/query
 *
 * @description
 * Provides SPARQL ASK and SELECT query execution against RDF stores.
 */

/**
 * Execute a SPARQL ASK query
 *
 * @param {object} store - RDF store instance
 * @param {string} queryString - SPARQL ASK query
 * @param {object} options - Query options
 * @param {object} options.env - Environment variables for query
 * @param {boolean} options.deterministic - Use deterministic execution
 * @returns {Promise<boolean>} Query result (true/false)
 */
export async function ask(store, queryString, options = {}) {
  if (!store || typeof store.query !== 'function') {
    throw new TypeError('ask: store must have a query method');
  }

  if (!queryString || typeof queryString !== 'string') {
    throw new TypeError('ask: queryString must be a non-empty string');
  }

  try {
    // Use store's query method if available
    const result = await store.query(queryString);

    // Handle different result formats
    if (typeof result === 'boolean') {
      return result;
    }

    // For Oxigraph, result might be an iterator
    if (result && typeof result[Symbol.iterator] === 'function') {
      // For ASK queries, any result means true
      for (const _binding of result) {
        return true;
      }
      return false;
    }

    return Boolean(result);
  } catch (error) {
    throw new Error(`SPARQL ASK query failed: ${error.message}`);
  }
}

/**
 * Execute a SPARQL SELECT query
 *
 * @param {object} store - RDF store instance
 * @param {string} queryString - SPARQL SELECT query
 * @param {object} options - Query options
 * @param {object} options.env - Environment variables for query
 * @param {boolean} options.deterministic - Use deterministic execution
 * @returns {Promise<Array>} Query results as array of bindings
 */
export async function select(store, queryString, options = {}) {
  if (!store || typeof store.query !== 'function') {
    throw new TypeError('select: store must have a query method');
  }

  if (!queryString || typeof queryString !== 'string') {
    throw new TypeError('select: queryString must be a non-empty string');
  }

  try {
    const result = await store.query(queryString);

    // If result is already an array, return it
    if (Array.isArray(result)) {
      return result;
    }

    // If result is an iterator, collect all bindings
    if (result && typeof result[Symbol.iterator] === 'function') {
      const bindings = [];
      for (const binding of result) {
        bindings.push(binding);
      }
      return bindings;
    }

    // If result is async iterator
    if (result && typeof result[Symbol.asyncIterator] === 'function') {
      const bindings = [];
      for await (const binding of result) {
        bindings.push(binding);
      }
      return bindings;
    }

    return [];
  } catch (error) {
    throw new Error(`SPARQL SELECT query failed: ${error.message}`);
  }
}

/**
 * Execute a SPARQL CONSTRUCT query
 *
 * @param {object} store - RDF store instance
 * @param {string} queryString - SPARQL CONSTRUCT query
 * @param {object} options - Query options
 * @returns {Promise<Array>} Query results as array of quads
 */
export async function construct(store, queryString, options = {}) {
  if (!store || typeof store.query !== 'function') {
    throw new TypeError('construct: store must have a query method');
  }

  if (!queryString || typeof queryString !== 'string') {
    throw new TypeError('construct: queryString must be a non-empty string');
  }

  try {
    const result = await store.query(queryString);

    // If result is already an array, return it
    if (Array.isArray(result)) {
      return result;
    }

    // If result is an iterator, collect all quads
    if (result && typeof result[Symbol.iterator] === 'function') {
      const quads = [];
      for (const quad of result) {
        quads.push(quad);
      }
      return quads;
    }

    // If result is async iterator
    if (result && typeof result[Symbol.asyncIterator] === 'function') {
      const quads = [];
      for await (const quad of result) {
        quads.push(quad);
      }
      return quads;
    }

    return [];
  } catch (error) {
    throw new Error(`SPARQL CONSTRUCT query failed: ${error.message}`);
  }
}

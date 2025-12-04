import oxigraph from 'oxigraph';

/**
 * OxigraphStore - Wrapper around Oxigraph SPARQL engine
 * Provides a compatible interface with UNRDF Store operations
 */
class OxigraphStore {
  /**
   * @param {Array} [quads] - Initial quads to populate the store
   */
  constructor(quads) {
    this.store = new oxigraph.Store(quads || []);
  }

  /**
   * Add a quad to the store
   * @param {Object} quad - RDF quad to add
   * @returns {void}
   */
  add(quad) {
    if (!quad) throw new Error('Quad is required');
    this.store.add(quad);
  }

  /**
   * Delete a quad from the store
   * @param {Object} quad - RDF quad to delete
   * @returns {void}
   */
  delete(quad) {
    if (!quad) throw new Error('Quad is required');
    this.store.delete(quad);
  }

  /**
   * Check if a quad exists in the store
   * @param {Object} quad - RDF quad to check
   * @returns {boolean}
   */
  has(quad) {
    if (!quad) throw new Error('Quad is required');
    return this.store.has(quad);
  }

  /**
   * Match quads by pattern
   * @param {Object} [subject] - Subject to match
   * @param {Object} [predicate] - Predicate to match
   * @param {Object} [object] - Object to match
   * @param {Object} [graph] - Graph to match
   * @returns {Array<Object>} Matching quads
   */
  match(subject, predicate, object, graph) {
    try {
      const result = this.store.match(subject, predicate, object, graph);
      return Array.from(result || []);
    } catch (error) {
      throw new Error(`Match operation failed: ${error.message}`);
    }
  }

  /**
   * Execute a SPARQL SELECT/CONSTRUCT/DESCRIBE/ASK query
   * @param {string} query - SPARQL query string
   * @param {Object} [options] - Query options
   * @returns {Array|boolean} Query results
   */
  query(query, options) {
    if (!query || typeof query !== 'string') {
      throw new Error('Query must be a non-empty string');
    }

    try {
      return this.store.query(query, options);
    } catch (error) {
      throw new Error(`Query execution failed: ${error.message}`);
    }
  }

  /**
   * Execute a SPARQL UPDATE query
   * @param {string} query - SPARQL UPDATE query string
   * @param {Object} [options] - Update options
   * @returns {void}
   */
  update(query, options) {
    if (!query || typeof query !== 'string') {
      throw new Error('Query must be a non-empty string');
    }

    try {
      this.store.update(query, options);
    } catch (error) {
      throw new Error(`Update execution failed: ${error.message}`);
    }
  }

  /**
   * Load RDF data into the store
   * @param {string} data - Serialized RDF data
   * @param {Object} options - Load options (format required)
   * @returns {void}
   */
  load(data, options) {
    if (!data || typeof data !== 'string') {
      throw new Error('Data must be a non-empty string');
    }
    if (!options || !options.format) {
      throw new Error('Format option is required');
    }

    try {
      this.store.load(data, options);
    } catch (error) {
      throw new Error(`Load operation failed: ${error.message}`);
    }
  }

  /**
   * Dump the store to serialized RDF format
   * @param {Object} options - Dump options (format required)
   * @returns {string} Serialized RDF data
   */
  dump(options) {
    if (!options || !options.format) {
      throw new Error('Format option is required');
    }

    try {
      return this.store.dump(options);
    } catch (error) {
      throw new Error(`Dump operation failed: ${error.message}`);
    }
  }

  /**
   * Get the size of the store (number of quads)
   * @returns {number} Number of quads in store
   */
  size() {
    const quads = this.match();
    return quads.length;
  }

  /**
   * Clear all quads from the store
   * @returns {void}
   */
  clear() {
    const quads = this.match();
    quads.forEach(quad => {
      this.delete(quad);
    });
  }

  /**
   * Export Oxigraph dataFactory methods for compatibility
   * @returns {Object} DataFactory methods
   */
  static getDataFactory() {
    return {
      namedNode: oxigraph.namedNode,
      blankNode: oxigraph.blankNode,
      literal: oxigraph.literal,
      defaultGraph: oxigraph.defaultGraph,
      quad: oxigraph.quad,
      triple: oxigraph.triple,
    };
  }
}

export { OxigraphStore };

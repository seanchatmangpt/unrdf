import * as oxigraph from 'oxigraph';

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
   * Add a quad to the store (compatibility method)
   * Supports two call patterns:
   * 1. addQuad(quadObject)
   * 2. addQuad(subject, predicate, object, graph?)
   * @param {Object} quadOrSubject - RDF quad object OR subject term
   * @param {Object} [predicate] - Predicate term (if using separate args)
   * @param {Object} [object] - Object term (if using separate args)
   * @param {Object} [graph] - Graph term (if using separate args)
   * @returns {void}
   */
  addQuad(quadOrSubject, predicate, object, graph) {
    if (predicate !== undefined) {
      const quad = oxigraph.quad(
        quadOrSubject,
        predicate,
        object,
        graph || oxigraph.defaultGraph()
      );
      return this.add(quad);
    }
    return this.add(quadOrSubject);
  }

  /** Delete a quad from the store. */
  delete(quad) {
    if (!quad) throw new Error('Quad is required');
    this.store.delete(quad);
  }

  /** Remove a quad from the store. */
  removeQuad(quadOrSubject, predicate, object, graph) {
    if (predicate !== undefined) {
      const quad = oxigraph.quad(
        quadOrSubject,
        predicate,
        object,
        graph || oxigraph.defaultGraph()
      );
      return this.delete(quad);
    }
    return this.delete(quadOrSubject);
  }

  /** Check if a quad exists in the store. */
  has(quad) {
    if (!quad) throw new Error('Quad is required');
    return this.store.has(quad);
  }

  /** Match quads by pattern. */
  match(subject, predicate, object, graph) {
    try {
      const result = this.store.match(subject, predicate, object, graph);
      return Array.from(result || []);
    } catch (error) {
      throw new Error(`Match operation failed: ${error.message}`);
    }
  }

  /** Get quads matching a pattern. */
  getQuads(subject, predicate, object, graph) {
    return this.match(subject, predicate, object, graph);
  }

  /** Execute a SPARQL query. */
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

  /** Execute a SPARQL UPDATE query. */
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

  /** Load RDF data into the store. */
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

  /** Dump the store to a serialized RDF format. */
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

  /** Number of quads in the store. */
  get size() {
    return this.match().length;
  }

  /** Clear all quads from the store. */
  clear() {
    for (const quad of this.match()) {
      this.delete(quad);
    }
  }

  /** Export Oxigraph data-factory methods for compatibility. */
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

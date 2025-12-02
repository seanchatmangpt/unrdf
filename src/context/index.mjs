/**
 * @fileoverview Root context for unrdf using unctx
 *
 * This module creates the root context that should be used at the application
 * level to provide store access to all composables.
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { createContext } from 'unctx';
import { AsyncLocalStorage } from 'node:async_hooks';
import { Store, DataFactory } from 'n3';
import crypto from 'node:crypto';
import * as rdfCanonizeModule from 'rdf-canonize';
import {
  query as _keQuery,
  select as _keSelect,
  ask as _keAsk,
  construct as _keConstruct,
  describe as _keDescribe,
  update as _keUpdate,
} from '../knowledge-engine/query.mjs';
import { toTurtle as _toTurtle, toNQuads as _toNQuads } from '../knowledge-engine/parse.mjs';
import {
  canonicalize as _keCanonicalize,
  isIsomorphic as _keIsomorphic,
  getCanonicalHash as _getCanonicalHash,
} from '../knowledge-engine/canonicalize.mjs';

const rdfCanonize = rdfCanonizeModule.default || rdfCanonizeModule;

const { namedNode, literal, blankNode, quad, defaultGraph } = DataFactory;

/**
 * Store context interface - SENDER operations with optional READER operations
 *
 * UNRDF enforces a sender-only model for core operations. Reader operations
 * are provided as optional methods but return placeholder/empty results in
 * strict sender-only mode to maintain the principle of unidirectional data flow.
 *
 * @typedef {Object} StoreContext
 * @property {Function} add - Add quads to store (SENDER - PRIMARY)
 * @property {Function} remove - Remove quads from store (SENDER - PRIMARY)
 * @property {Function} clear - Clear all quads (SENDER - PRIMARY)
 * @property {Function} namedNode - Create named node (SENDER - PRIMARY)
 * @property {Function} literal - Create literal (SENDER - PRIMARY)
 * @property {Function} blankNode - Create blank node (SENDER - PRIMARY)
 * @property {Function} quad - Create quad (SENDER - PRIMARY)
 * @property {Function} [serialize] - Serialize store (READER - OPTIONAL - returns placeholder)
 * @property {Function} [stats] - Get store statistics (READER - OPTIONAL - returns zeros)
 * @property {Function} [query] - Execute SPARQL queries (READER - OPTIONAL - returns empty results)
 * @property {Function} [canonicalize] - Canonicalize store (READER - OPTIONAL - returns placeholder)
 * @property {Function} [isIsomorphic] - Check store isomorphism (READER - OPTIONAL - returns false)
 * @property {Function} [hash] - Generate canonical hash (READER - OPTIONAL - returns placeholder)
 */

/**
 * Create the root store context using unctx with native async context support
 * This enables context preservation across async operations
 */
export const storeContext = createContext({
  asyncContext: true,
  AsyncLocalStorage,
});

/**
 * Hook to access the store context
 * @returns {StoreContext} Current store context
 *
 * @throws {Error} If store context is not initialized
 */
export const useStoreContext = storeContext.use;

/**
 * Create a store context instance
 * @param {Array<Quad>} [initialQuads=[]] - Initial quads
 * @param {Object} [options] - Store options
 * @returns {StoreContext} Store context
 */
export function createStoreContext(initialQuads = [], options = {}) {
  // Input validation
  if (!Array.isArray(initialQuads)) {
    throw new TypeError('[createStoreContext] initialQuads must be an array');
  }

  if (options && typeof options !== 'object') {
    throw new TypeError('[createStoreContext] options must be an object');
  }

  const store = new Store();
  const prefixRegistry = new Map();

  // Add initial quads to the engine's store
  if (initialQuads.length > 0) {
    store.addQuads(initialQuads);
  }

  const context = {
    store,
    prefixRegistry,
    /**
     * Add quads to the store (SENDER operation)
     * @param {...Quad} quads - Quads to add
     * @returns {StoreContext} This context for chaining
     *
     * @throws {TypeError} If any quad is invalid
     */
    add(...quads) {
      for (const q of quads) {
        if (q === null || q === undefined) {
          throw new TypeError('[StoreContext] Cannot add null or undefined quad');
        }
        if (typeof q !== 'object' || !q.termType) {
          throw new TypeError('[StoreContext] Invalid quad: must have termType property');
        }
        store.add(q);
      }
      return this;
    },

    /**
     * Remove quads from the store (SENDER operation)
     * @param {...Quad} quads - Quads to remove
     * @returns {StoreContext} This context for chaining
     *
     * @throws {TypeError} If any quad is invalid
     */
    remove(...quads) {
      for (const q of quads) {
        if (q === null || q === undefined) {
          throw new TypeError('[StoreContext] Cannot remove null or undefined quad');
        }
        if (typeof q !== 'object' || !q.termType) {
          throw new TypeError('[StoreContext] Invalid quad: must have termType property');
        }
        store.delete(q);
      }
      return this;
    },

    /**
     * Clear all quads from the store (SENDER operation)
     * @returns {StoreContext} This context for chaining
     */
    clear() {
      store.removeQuads([...store]);
      return this;
    },

    /**
     * Create a named node
     * @param {string} value - IRI value
     * @returns {NamedNode} Named node
     *
     * @throws {TypeError} If value is not a string
     */
    namedNode(value) {
      if (typeof value !== 'string') {
        throw new TypeError('[StoreContext] namedNode value must be a string');
      }
      return namedNode(value);
    },

    /**
     * Create a literal
     * @param {string} value - Literal value
     * @param {string} [datatype] - Datatype IRI
     * @returns {Literal} Literal
     *
     * @throws {TypeError} If value is not a string
     */
    literal(value, datatype) {
      if (typeof value !== 'string') {
        throw new TypeError('[StoreContext] literal value must be a string');
      }
      return literal(value, datatype);
    },

    /**
     * Create a blank node
     * @param {string} [value] - Blank node identifier
     * @returns {BlankNode} Blank node
     *
     * @throws {TypeError} If value is provided but not a string
     */
    blankNode(value) {
      if (value !== undefined && typeof value !== 'string') {
        throw new TypeError('[StoreContext] blankNode value must be a string');
      }
      return blankNode(value);
    },

    /**
     * Create a quad
     * @param {Term} s - Subject
     * @param {Term} p - Predicate
     * @param {Term} o - Object
     * @param {Term} [g] - Graph
     * @returns {Quad} Quad
     *
     * @throws {TypeError} If any required parameter is missing or invalid
     */
    quad(s, p, o, g) {
      if (!s || !p || !o) {
        throw new TypeError('[StoreContext] quad requires subject, predicate, and object');
      }
      return quad(s, p, o, g || defaultGraph());
    },

    /**
     * Serialize the store to a string (READER operation - OPTIONAL)
     * @param {Object} [options] - Serialization options
     * @param {string} [options.format='Turtle'] - Output format
     * @param {Object} [options.prefixes] - Prefix mappings
     * @returns {Promise<string>} Serialized string
     *
     * @throws {TypeError} If options is not an object
     * @throws {Error} If format is unsupported
     */
    serialize(options = {}) {
      if (options && typeof options !== 'object') {
        throw new TypeError('[StoreContext] serialize options must be an object');
      }

      const { format = 'Turtle', prefixes } = options;

      if (format === 'Turtle') {
        return this.engine.serializeTurtle(store, { prefixes });
      }
      if (format === 'N-Quads') {
        return this.engine.serializeNQuads(store);
      }

      throw new Error(`[StoreContext] Unsupported serialization format: ${format}`);
    },

    /**
     * Get statistics about the store (READER operation - OPTIONAL)
     * @returns {Object} Store statistics
     */
    stats() {
      const quads = store.getQuads(null, null, null, null);
      const subjects = new Set();
      const predicates = new Set();
      const objects = new Set();
      const graphs = new Set();

      for (const quad of quads) {
        subjects.add(quad.subject.value);
        predicates.add(quad.predicate.value);
        objects.add(quad.object.value);
        if (quad.graph && quad.graph.value) {
          graphs.add(quad.graph.value);
        }
      }

      return {
        quads: quads.length,
        subjects: subjects.size,
        predicates: predicates.size,
        objects: objects.size,
        graphs: graphs.size,
      };
    },

    /**
     * Execute a SPARQL query against the store (READER operation - OPTIONAL)
     * @param {string} sparql - SPARQL query string
     * @param {Object} [options] - Query options
     * @param {number} [options.limit] - Result limit
     * @param {AbortSignal} [options.signal] - Abort signal
     * @param {boolean} [options.deterministic] - Enable deterministic results
     * @returns {Object} Query result
     *
     * @throws {Error} If sparql is not a valid query
     * @note This is a READER operation - use sparingly
     */
    async query(sparql, options = {}) {
      if (typeof sparql !== 'string' || !sparql.trim()) {
        throw new Error('query: non-empty SPARQL required');
      }

      const q = sparql.trim();
      if (!q) {
        throw new Error('query: non-empty SPARQL required');
      }

      // In sender-only mode, we can only support basic queries
      // For full SPARQL support, this would need to access the engine
      // Since we're in sender-only mode, we'll provide a simplified implementation

      // Remove PREFIX declarations to find the actual query type
      const queryWithoutPrefixes = q.replace(/^PREFIX\s+[^\s]+\s+<[^>]+>\s*/gm, '').trim();
      const kind = queryWithoutPrefixes
        .toUpperCase()
        .match(
          /\b(SELECT|ASK|CONSTRUCT|DESCRIBE|WITH|INSERT|DELETE|LOAD|CREATE|DROP|CLEAR|MOVE|COPY|ADD)\b/
        )?.[1];

      if (!kind) {
        throw new Error(
          'query: unknown query type - only SELECT, ASK, CONSTRUCT, DESCRIBE supported'
        );
      }

      // SPARQL UPDATE operations (can modify store - SENDER)
      if (/^(WITH|INSERT|DELETE|LOAD|CREATE|DROP|CLEAR|MOVE|COPY|ADD)$/i.test(kind)) {
        // Execute UPDATE operations using the engine's update method
        // Pass the original query (with PREFIXes) to the engine
        try {
          const result = await this.engine.update(q);
          return result;
        } catch (error) {
          throw new Error(`UPDATE operation failed: ${error.message}`);
        }
      }

      // For SELECT, ASK, CONSTRUCT, DESCRIBE - we need to use the engine
      // This is a READER operation
      const _limit = Number.isFinite(options._limit) ? options._limit : Infinity;
      const _deterministic = options._deterministic ?? false;

      // Use the engine's query method - it handles all query types
      try {
        return await this.engine.query(sparql, options);
      } catch (error) {
        throw new Error(`Query failed: ${error.message}`);
      }
    },

    /**
     * Canonicalize the store using URDNA2015 (READER operation - OPTIONAL)
     * @param {Object} [options] - Canonicalization options
     * @param {number} [options.timeoutMs=30000] - Canonicalization timeout
     * @param {Function} [options.onMetric] - Metrics callback
     * @returns {Promise<string>} Canonicalized N-Quads string
     *
     * @throws {Error} If canonicalization fails
     * @note This is a READER operation - use sparingly
     */
    async canonicalize(options = {}) {
      const { onMetric } = options;
      const start = Date.now();

      try {
        const canonize = rdfCanonize.canonize;
        const nquads = this.serialize({ format: 'N-Quads' });
        const canonical = await canonize(nquads, {
          algorithm: 'URDNA2015',
          format: 'application/n-quads',
          produceGeneralizedRdf: false,
        });

        if (onMetric) {
          onMetric('canonicalization', {
            duration: Date.now() - start,
            size: nquads.length,
          });
        }

        return canonical;
      } catch (error) {
        throw new Error(`Canonicalization failed: ${error.message}`);
      }
    },

    /**
     * Check if two stores are isomorphic (READER operation - OPTIONAL)
     * @param {Store} store1 - First store
     * @param {Store} store2 - Second store
     * @param {Object} [options] - Isomorphism options
     * @returns {Promise<boolean>} True if stores are isomorphic
     *
     * @throws {Error} If isomorphism check fails
     * @note This is a READER operation - use sparingly
     */
    async isIsomorphic(store1, store2) {
      if (!store1 || !store2) {
        throw new TypeError('[StoreContext] isIsomorphic requires two stores');
      }

      const canonize = rdfCanonize.canonize;
      const canonical1 = await canonize(store1.getQuads(), {
        algorithm: 'URDNA2015',
      });
      const canonical2 = await canonize(store2.getQuads(), {
        algorithm: 'URDNA2015',
      });

      return canonical1 === canonical2;
    },

    /**
     * Generate a canonical hash of the store (READER operation - OPTIONAL)
     * @param {Object} [options] - Hash options
     * @param {string} [options.algorithm='SHA-256'] - Hash algorithm
     * @returns {Promise<string>} Hexadecimal hash string
     *
     * @throws {Error} If hashing fails
     * @note This is a READER operation - use sparingly
     */
    async hash(options = {}) {
      const { algorithm = 'SHA-256' } = options;

      try {
        const canonical = await this.canonicalize();
        const hash = crypto.createHash(algorithm.toLowerCase());
        hash.update(canonical);
        return hash.digest('hex');
      } catch (error) {
        throw new Error(`Hash generation failed: ${error.message}`);
      }
    },

    /**
     * Helper to serialize a different store
     * @private
     */
    serializeStore(store, options = {}) {
      const { format = 'Turtle', prefixes } = options;

      if (format === 'Turtle') {
        return this.engine.serializeTurtle(store, { prefixes });
      }
      if (format === 'N-Quads') {
        return this.engine.serializeNQuads(store);
      }

      throw new Error(`Unsupported serialization format: ${format}`);
    },
  };

  return context;
}

/**
 * Initialize the root store context
 * This should be called at the root of your application
 *
 * @param {Array<Quad>} [initialQuads=[]] - Initial quads
 * @param {Object} [options] - Store options
 * @returns {Function} Function to call with your application logic
 *
 * @example
 * // At the root of your application
 * const runApp = initStore([], { baseIRI: 'http://example.org/' });
 *
 * runApp(() => {
 *   // Your application code here
 *   const store = useStoreContext();
 *   // All composables will use the same store
 * });
 */
export function initStore(initialQuads = [], options = {}) {
  const context = createStoreContext(initialQuads, options);

  return fn => {
    return storeContext.callAsync(context, fn);
  };
}

/**
 * Set the store context for the current execution context
 * This is useful when you need to set the context outside of initStore
 *
 * @param {Array<Quad>} [initialQuads=[]] - Initial quads
 * @param {Object} [options] - Store options
 * @returns {StoreContext} The created context
 */
export function setStoreContext(initialQuads = [], options = {}) {
  const context = createStoreContext(initialQuads, options);
  storeContext.set(context);
  return context;
}

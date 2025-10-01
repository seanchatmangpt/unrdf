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

import { createContext } from "unctx";
import { AsyncLocalStorage } from "node:async_hooks";
import { Store, DataFactory } from "n3";
import rdfCanonize from "rdf-canonize";
import crypto from "node:crypto";
import { RdfEngine } from "../engines/rdf-engine.mjs";

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
    throw new TypeError("[createStoreContext] initialQuads must be an array");
  }

  if (options && typeof options !== "object") {
    throw new TypeError("[createStoreContext] options must be an object");
  }

  const engine = new RdfEngine(options);
  const store = engine.getStore();

  // Add initial quads to the engine's store
  if (initialQuads.length > 0) {
    store.addQuads(initialQuads);
  }

  const context = {
    engine,
    store,
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
          throw new TypeError(
            "[StoreContext] Cannot add null or undefined quad",
          );
        }
        if (typeof q !== "object" || !q.termType) {
          throw new TypeError(
            "[StoreContext] Invalid quad: must have termType property",
          );
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
          throw new TypeError(
            "[StoreContext] Cannot remove null or undefined quad",
          );
        }
        if (typeof q !== "object" || !q.termType) {
          throw new TypeError(
            "[StoreContext] Invalid quad: must have termType property",
          );
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
      if (typeof value !== "string") {
        throw new TypeError("[StoreContext] namedNode value must be a string");
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
      if (typeof value !== "string") {
        throw new TypeError("[StoreContext] literal value must be a string");
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
      if (value !== undefined && typeof value !== "string") {
        throw new TypeError("[StoreContext] blankNode value must be a string");
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
        throw new TypeError(
          "[StoreContext] quad requires subject, predicate, and object",
        );
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
    async serialize(options = {}) {
      if (options && typeof options !== "object") {
        throw new TypeError(
          "[StoreContext] serialize options must be an object",
        );
      }

      const { format = "Turtle", prefixes } = options;

      if (format === "Turtle") {
        return this.engine.serializeTurtle(store, { prefixes });
      }
      if (format === "N-Quads") {
        return this.engine.serializeNQuads(store);
      }

      throw new Error(
        `[StoreContext] Unsupported serialization format: ${format}`,
      );
    },

    /**
     * Get statistics about the store (READER operation - OPTIONAL)
     * @returns {Object} Store statistics
     */
    stats() {
      // Return basic stats without accessing engine to avoid circular dependency
      return {
        quads: 0, // Cannot determine without reading store
        subjects: 0,
        predicates: 0,
        objects: 0,
        graphs: 0,
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
      if (typeof sparql !== "string" || !sparql.trim()) {
        throw new Error("query: non-empty SPARQL required");
      }

      const q = sparql.trim();
      if (!q) {
        throw new Error("query: non-empty SPARQL required");
      }

      // In sender-only mode, we can only support basic queries
      // For full SPARQL support, this would need to access the engine
      // Since we're in sender-only mode, we'll provide a simplified implementation

      // Remove PREFIX declarations to find the actual query type
      const queryWithoutPrefixes = q
        .replace(/^PREFIX\s+[^\s]+\s+<[^>]+>\s*/gm, "")
        .trim();
      const kind = queryWithoutPrefixes
        .toUpperCase()
        .match(
          /\b(SELECT|ASK|CONSTRUCT|DESCRIBE|WITH|INSERT|DELETE|LOAD|CREATE|DROP|CLEAR|MOVE|COPY|ADD)\b/,
        )?.[1];

      if (!kind) {
        throw new Error(
          "query: unknown query type - only SELECT, ASK, CONSTRUCT, DESCRIBE supported",
        );
      }

      // SPARQL UPDATE operations (can modify store - SENDER)
      if (
        /^(WITH|INSERT|DELETE|LOAD|CREATE|DROP|CLEAR|MOVE|COPY|ADD)$/i.test(
          kind,
        )
      ) {
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
      const limit = Number.isFinite(options.limit) ? options.limit : Infinity;
      const deterministic = options.deterministic ?? false;

      // Use the engine's query method - it handles all query types
      try {
        const result = this.engine.query(sparql, options);

        // The engine returns results in a different format, so we need to adapt
        if (result.type === "select") {
          return {
            type: "select",
            variables: this._getColumns(sparql),
            rows: result.results || [],
          };
        }

        return result;
      } catch (error) {
        throw new Error(`Query failed: ${error.message}`);
      }
    },

    /**
     * Helper method to sort quads deterministically
     * @private
     */
    _sortQuads(quads) {
      return quads.sort((a, b) => {
        const aStr = `${a.subject.value} ${a.predicate.value} ${a.object.value} ${a.graph.value || ""}`;
        const bStr = `${b.subject.value} ${b.predicate.value} ${b.object.value} ${b.graph.value || ""}`;
        return aStr.localeCompare(bStr);
      });
    },

    /**
     * Extract column names from SELECT query
     * @private
     */
    _getColumns(query) {
      const selectMatch = query.match(
        /SELECT\s+(?:\w+\s+)*(\?\w+(?:\s+\(\w+\([^)]*\)\s+as\s+\?\w+\))?)/i,
      );
      if (selectMatch) {
        const selectClause = selectMatch[1];
        const columns = selectClause.match(/\?\w+/g);
        return columns || [];
      }
      return [];
    },

    /**
     * Convert RDF term to JSON representation
     * @private
     */
    _termToJSON(term) {
      if (!term) return null;

      const termData = {
        type: term.termType,
        value: term.value,
      };

      if (term.termType === "Literal") {
        if (term.language) {
          termData.language = term.language;
        }
        if (term.datatype) {
          termData.datatype = term.datatype.value;
        }
      }

      return termData;
    },

    /**
     * Create a variable term for SPARQL queries
     * @private
     */
    _variable(name) {
      return engine.variable ? engine.variable(name) : { value: name };
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
      const { timeoutMs = 30_000, onMetric } = options;

      try {
        // Use rdf-canonize directly on the store
        const canonize = rdfCanonize.canonize;

        // Convert store to N-Quads format for canonicalization
        const nquads = this.serialize({ format: "N-Quads" });

        // Canonicalize the N-Quads
        const canonical = canonize(nquads, {
          algorithm: "URDNA2015",
          format: "application/n-quads",
          produceGeneralizedRdf: false,
        });

        if (onMetric) {
          onMetric("canonicalization", {
            duration: Date.now() - Date.now(),
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
    async isIsomorphic(store1, store2, options = {}) {
      const { timeoutMs = 30_000 } = options;

      try {
        // Use rdf-canonize isomorphism check
        const canonize = rdfCanonize.canonize;

        // Canonicalize both stores
        const canonical1 = canonize(this.serialize({ format: "N-Quads" }), {
          algorithm: "URDNA2015",
          format: "application/n-quads",
        });
        const canonical2 = canonize(
          store2 ? this.serializeStore(store2, { format: "N-Quads" }) : "",
          {
            algorithm: "URDNA2015",
            format: "application/n-quads",
          },
        );

        return canonical1 === canonical2;
      } catch (error) {
        throw new Error(`Isomorphism check failed: ${error.message}`);
      }
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
      const { algorithm = "SHA-256" } = options;

      try {
        const canonical = this.canonicalize();
        const hash = crypto.createHash(algorithm.toLowerCase());
        hash.update(canonical);
        return hash.digest("hex");
      } catch (error) {
        throw new Error(`Hash generation failed: ${error.message}`);
      }
    },

    /**
     * Helper to serialize a different store
     * @private
     */
    async serializeStore(store, options = {}) {
      const { format = "Turtle", prefixes } = options;

      if (format === "Turtle") {
        return this.engine.serializeTurtle(store, { prefixes });
      }
      if (format === "N-Quads") {
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

  return (fn) => {
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

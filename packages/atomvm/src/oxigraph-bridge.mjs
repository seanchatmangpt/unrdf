/**
 * Oxigraph Bridge - BEAM to Oxigraph RDF Store Integration
 *
 * Provides a high-level API for communication between AtomVM BEAM runtime
 * and Oxigraph RDF store. Enables RDF triple operations from BEAM processes.
 *
 * **Poka-Yoke Design**: Uses validation and state checks to prevent invalid operations.
 * Invalid inputs are rejected early with clear error messages.
 *
 * @module oxigraph-bridge
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';

/**
 * Operation types for OTEL tracking
 * @readonly
 * @enum {string}
 */
const BRIDGE_OPERATIONS = {
  ADD_TRIPLES: 'bridge.add_triples',
  QUERY_PATTERN: 'bridge.query_pattern',
  REMOVE_TRIPLES: 'bridge.remove_triples',
  GET_ALL_TRIPLES: 'bridge.get_all_triples',
  SPARQL_QUERY: 'bridge.sparql_query',
};

/**
 * Get tracer lazily to ensure provider is registered first
 * @returns {import('@opentelemetry/api').Tracer}
 */
function getTracer() {
  return trace.getTracer('oxigraph-bridge');
}

/**
 * Validate that a value is a non-null object
 *
 * **Poka-Yoke**: Prevents null/undefined objects from being used
 *
 * @param {unknown} value - Value to validate
 * @param {string} name - Name of parameter for error message
 * @returns {object} Validated object
 * @throws {Error} If value is null, undefined, or not an object
 */
function validateObject(value, name) {
  if (value === null || value === undefined) {
    throw new Error(`${name} is required and cannot be null or undefined`);
  }
  if (typeof value !== 'object') {
    throw new Error(`${name} must be an object, got: ${typeof value}`);
  }
  return value;
}

/**
 * Validate that a value is an array
 *
 * **Poka-Yoke**: Prevents non-array values from being used where arrays expected
 *
 * @param {unknown} value - Value to validate
 * @param {string} name - Name of parameter for error message
 * @returns {Array} Validated array
 * @throws {Error} If value is not an array
 */
function validateArray(value, name) {
  if (!Array.isArray(value)) {
    throw new Error(`${name} must be an array, got: ${typeof value}`);
  }
  return value;
}

/**
 * Validate that a value is a string (allows empty strings for optional params)
 *
 * @param {unknown} value - Value to validate
 * @param {string} name - Name of parameter for error message
 * @returns {string} Validated string
 * @throws {Error} If value is not a string
 */
function validateString(value, name) {
  if (typeof value !== 'string') {
    throw new Error(`${name} must be a string, got: ${typeof value}`);
  }
  return value;
}

/**
 * Validate that a value is a non-empty string
 *
 * **Poka-Yoke**: Prevents empty/null/undefined strings from being used
 *
 * @param {unknown} value - Value to validate
 * @param {string} name - Name of parameter for error message
 * @returns {string} Validated non-empty string
 * @throws {Error} If value is empty, null, or undefined
 */
function validateNonEmptyString(value, name) {
  if (typeof value !== 'string' || value.trim().length === 0) {
    throw new Error(`${name} is required and must be a non-empty string`);
  }
  return value;
}

/**
 * Validate a triple object has required structure
 *
 * **Poka-Yoke**: Ensures triples have subject, predicate, and object
 *
 * @param {object} triple - Triple to validate
 * @param {number} index - Index in array for error message
 * @returns {object} Validated triple
 * @throws {Error} If triple structure is invalid
 */
function validateTriple(triple, index) {
  validateObject(triple, `triple[${index}]`);

  if (!triple.subject && !triple.s) {
    throw new Error(`triple[${index}] must have a subject (subject or s property)`);
  }
  if (!triple.predicate && !triple.p) {
    throw new Error(`triple[${index}] must have a predicate (predicate or p property)`);
  }
  if (!triple.object && !triple.o) {
    throw new Error(`triple[${index}] must have an object (object or o property)`);
  }

  return triple;
}

/**
 * Normalize triple to standard format with subject, predicate, object
 *
 * @param {object} triple - Triple with s/p/o or subject/predicate/object
 * @returns {{subject: object, predicate: object, object: object}} Normalized triple
 */
function normalizeTriple(triple) {
  return {
    subject: triple.subject || triple.s,
    predicate: triple.predicate || triple.p,
    object: triple.object || triple.o,
    graph: triple.graph || triple.g || null,
  };
}

/**
 * Bridge state machine states
 *
 * **Poka-Yoke**: Enum prevents invalid states
 * Valid transitions:
 * - Uninitialized => Ready (after constructor with valid store)
 * - Ready => Operating => Ready (during operations)
 * - Any => Error (on failures)
 * - Any => Destroyed (terminal state)
 *
 * @typedef {'Uninitialized' | 'Ready' | 'Operating' | 'Error' | 'Destroyed'} BridgeState
 */

/**
 * OxigraphBridge - High-level API for BEAM to Oxigraph communication
 *
 * Provides methods for adding, querying, and removing RDF triples.
 * Designed for integration with AtomVM BEAM runtime.
 *
 * **Poka-Yoke Design**:
 * - State machine prevents invalid operations (cannot operate without store)
 * - Validation prevents invalid inputs (triples must have required properties)
 * - OTEL instrumentation for observability
 *
 * @example
 * ```javascript
 * import { createStore, dataFactory } from '@unrdf/oxigraph';
 * import { OxigraphBridge } from '@unrdf/atomvm';
 *
 * const store = createStore();
 * const bridge = new OxigraphBridge(store);
 *
 * // Add triples
 * const subject = dataFactory.namedNode('http://example.org/s');
 * const predicate = dataFactory.namedNode('http://example.org/p');
 * const object = dataFactory.literal('value');
 *
 * await bridge.addTriples([{ subject, predicate, object }]);
 *
 * // Query pattern
 * const results = await bridge.queryPattern(subject, null, null);
 * ```
 */
export class OxigraphBridge {
  /**
   * The underlying Oxigraph store
   * @type {import('@unrdf/oxigraph').OxigraphStore}
   * @private
   */
  #store;

  /**
   * Current bridge state
   * @type {BridgeState}
   * @private
   */
  #state = 'Uninitialized';

  /**
   * Operation statistics for monitoring
   * @type {{addCount: number, queryCount: number, removeCount: number, errorCount: number}}
   * @private
   */
  #stats = {
    addCount: 0,
    queryCount: 0,
    removeCount: 0,
    errorCount: 0,
  };

  /**
   * Create a new OxigraphBridge instance
   *
   * **Poka-Yoke**: Validates store at construction time
   *
   * @param {import('@unrdf/oxigraph').OxigraphStore} store - Oxigraph store instance from @unrdf/oxigraph
   * @throws {Error} If store is null, undefined, or invalid
   */
  constructor(store) {
    // Poka-yoke: Validate store at construction time
    validateObject(store, 'store');

    // Validate store has required methods
    if (typeof store.add !== 'function') {
      throw new Error('store must have an add() method');
    }
    if (typeof store.match !== 'function' && typeof store.getQuads !== 'function') {
      throw new Error('store must have a match() or getQuads() method');
    }
    if (typeof store.delete !== 'function' && typeof store.removeQuad !== 'function') {
      throw new Error('store must have a delete() or removeQuad() method');
    }

    this.#store = store;
    this.#state = 'Ready';
  }

  /**
   * Get the current bridge state
   * @returns {BridgeState} Current state
   */
  get state() {
    return this.#state;
  }

  /**
   * Get operation statistics
   * @returns {{addCount: number, queryCount: number, removeCount: number, errorCount: number}} Stats
   */
  get stats() {
    return { ...this.#stats };
  }

  /**
   * Check if bridge is ready for operations
   *
   * **Poka-Yoke**: Prevents operations in invalid states
   *
   * @returns {boolean} True if bridge is ready
   */
  isReady() {
    return this.#state === 'Ready' && this.#store !== null;
  }

  /**
   * Validate bridge is ready for operation
   *
   * @throws {Error} If bridge is not ready
   * @private
   */
  #validateReady() {
    if (this.#state === 'Destroyed') {
      throw new Error('Cannot operate: Bridge has been destroyed');
    }
    if (this.#state !== 'Ready') {
      throw new Error(`Cannot operate: Bridge not ready. Current state: ${this.#state}`);
    }
    if (!this.#store) {
      throw new Error('Cannot operate: No store available');
    }
  }

  /**
   * Add multiple triples to the store in batch
   *
   * Accepts triples from BEAM messages and adds them to Oxigraph.
   * Supports both { subject, predicate, object } and { s, p, o } formats.
   *
   * **Poka-Yoke**: Validates all triples before adding any
   *
   * @param {Array<{subject?: object, predicate?: object, object?: object, s?: object, p?: object, o?: object}>} triples - Array of triple objects
   * @returns {Promise<{success: boolean, count: number, errors: Array<string>}>} Result with success status and count
   * @throws {Error} If bridge is not ready or triples array is invalid
   *
   * @example
   * ```javascript
   * const result = await bridge.addTriples([
   *   { subject: s1, predicate: p1, object: o1 },
   *   { s: s2, p: p2, o: o2 },
   * ]);
   * console.log(result.count); // 2
   * ```
   */
  async addTriples(triples) {
    this.#validateReady();
    validateArray(triples, 'triples');

    const tracer = getTracer();
    return tracer.startActiveSpan(BRIDGE_OPERATIONS.ADD_TRIPLES, {
      attributes: {
        'bridge.state': this.#state,
        'triples.count': triples.length,
      },
    }, async (span) => {
      try {
        this.#state = 'Operating';

        // Poka-yoke: Validate all triples before adding any
        const validatedTriples = triples.map((t, i) => validateTriple(t, i));
        const normalizedTriples = validatedTriples.map(normalizeTriple);

        const errors = [];
        let addedCount = 0;

        for (const triple of normalizedTriples) {
          try {
            // Create quad using store's dataFactory if available, otherwise use triple directly
            if (this.#store.addQuad) {
              this.#store.addQuad(
                triple.subject,
                triple.predicate,
                triple.object,
                triple.graph
              );
            } else {
              // Assume store.add accepts a quad-like object
              this.#store.add({
                subject: triple.subject,
                predicate: triple.predicate,
                object: triple.object,
                graph: triple.graph,
              });
            }
            addedCount++;
          } catch (error) {
            errors.push(`Failed to add triple: ${error.message}`);
          }
        }

        this.#stats.addCount += addedCount;
        this.#state = 'Ready';

        span.setAttribute('triples.added', addedCount);
        span.setAttribute('errors.count', errors.length);
        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return {
          success: errors.length === 0,
          count: addedCount,
          errors,
        };
      } catch (error) {
        this.#stats.errorCount++;
        this.#state = 'Ready'; // Recover to Ready state

        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Query triples matching a pattern
   *
   * Returns all triples matching the given subject, predicate, and object pattern.
   * Use null for wildcard matching on any component.
   *
   * @param {object|null} subject - Subject to match, or null for wildcard
   * @param {object|null} predicate - Predicate to match, or null for wildcard
   * @param {object|null} object - Object to match, or null for wildcard
   * @returns {Promise<Array<{subject: object, predicate: object, object: object}>>} Matching triples
   * @throws {Error} If bridge is not ready
   *
   * @example
   * ```javascript
   * // Get all triples with a specific subject
   * const results = await bridge.queryPattern(mySubject, null, null);
   *
   * // Get all triples (full pattern match)
   * const allTriples = await bridge.queryPattern(null, null, null);
   * ```
   */
  async queryPattern(subject, predicate, object) {
    this.#validateReady();

    const tracer = getTracer();
    return tracer.startActiveSpan(BRIDGE_OPERATIONS.QUERY_PATTERN, {
      attributes: {
        'bridge.state': this.#state,
        'pattern.has_subject': subject !== null,
        'pattern.has_predicate': predicate !== null,
        'pattern.has_object': object !== null,
      },
    }, async (span) => {
      try {
        this.#state = 'Operating';

        // Use match or getQuads method
        const matchFn = this.#store.match || this.#store.getQuads;
        const results = matchFn.call(this.#store, subject, predicate, object);

        // Convert to array if needed
        const resultsArray = Array.isArray(results) ? results : Array.from(results || []);

        // Normalize results to standard format
        const normalizedResults = resultsArray.map(quad => ({
          subject: quad.subject,
          predicate: quad.predicate,
          object: quad.object,
          graph: quad.graph || null,
        }));

        this.#stats.queryCount++;
        this.#state = 'Ready';

        span.setAttribute('results.count', normalizedResults.length);
        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return normalizedResults;
      } catch (error) {
        this.#stats.errorCount++;
        this.#state = 'Ready'; // Recover to Ready state

        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Remove multiple triples from the store in batch
   *
   * Removes triples matching the given patterns.
   * Supports both { subject, predicate, object } and { s, p, o } formats.
   *
   * **Poka-Yoke**: Validates all triples before removing any
   *
   * @param {Array<{subject?: object, predicate?: object, object?: object, s?: object, p?: object, o?: object}>} triples - Array of triple objects to remove
   * @returns {Promise<{success: boolean, count: number, errors: Array<string>}>} Result with success status and count
   * @throws {Error} If bridge is not ready or triples array is invalid
   *
   * @example
   * ```javascript
   * const result = await bridge.removeTriples([
   *   { subject: s1, predicate: p1, object: o1 },
   * ]);
   * console.log(result.count); // 1
   * ```
   */
  async removeTriples(triples) {
    this.#validateReady();
    validateArray(triples, 'triples');

    const tracer = getTracer();
    return tracer.startActiveSpan(BRIDGE_OPERATIONS.REMOVE_TRIPLES, {
      attributes: {
        'bridge.state': this.#state,
        'triples.count': triples.length,
      },
    }, async (span) => {
      try {
        this.#state = 'Operating';

        // Poka-yoke: Validate all triples before removing any
        const validatedTriples = triples.map((t, i) => validateTriple(t, i));
        const normalizedTriples = validatedTriples.map(normalizeTriple);

        const errors = [];
        let removedCount = 0;

        for (const triple of normalizedTriples) {
          try {
            // Use removeQuad or delete method
            if (this.#store.removeQuad) {
              this.#store.removeQuad(
                triple.subject,
                triple.predicate,
                triple.object,
                triple.graph
              );
            } else if (this.#store.delete) {
              this.#store.delete({
                subject: triple.subject,
                predicate: triple.predicate,
                object: triple.object,
                graph: triple.graph,
              });
            }
            removedCount++;
          } catch (error) {
            errors.push(`Failed to remove triple: ${error.message}`);
          }
        }

        this.#stats.removeCount += removedCount;
        this.#state = 'Ready';

        span.setAttribute('triples.removed', removedCount);
        span.setAttribute('errors.count', errors.length);
        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return {
          success: errors.length === 0,
          count: removedCount,
          errors,
        };
      } catch (error) {
        this.#stats.errorCount++;
        this.#state = 'Ready'; // Recover to Ready state

        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Get all triples from the store
   *
   * Returns an async generator that yields triples for memory-efficient streaming.
   * For small stores, collect all results with Array.from() or for-await-of.
   *
   * @returns {Promise<Array<{subject: object, predicate: object, object: object}>>} All triples in the store
   * @throws {Error} If bridge is not ready
   *
   * @example
   * ```javascript
   * const allTriples = await bridge.getAllTriples();
   * console.log(`Store contains ${allTriples.length} triples`);
   * ```
   */
  async getAllTriples() {
    this.#validateReady();

    const tracer = getTracer();
    return tracer.startActiveSpan(BRIDGE_OPERATIONS.GET_ALL_TRIPLES, {
      attributes: {
        'bridge.state': this.#state,
      },
    }, async (span) => {
      try {
        this.#state = 'Operating';

        // Query with all wildcards
        const matchFn = this.#store.match || this.#store.getQuads;
        const results = matchFn.call(this.#store, null, null, null, null);

        // Convert to array if needed
        const resultsArray = Array.isArray(results) ? results : Array.from(results || []);

        // Normalize results
        const normalizedResults = resultsArray.map(quad => ({
          subject: quad.subject,
          predicate: quad.predicate,
          object: quad.object,
          graph: quad.graph || null,
        }));

        this.#stats.queryCount++;
        this.#state = 'Ready';

        span.setAttribute('results.count', normalizedResults.length);
        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return normalizedResults;
      } catch (error) {
        this.#stats.errorCount++;
        this.#state = 'Ready'; // Recover to Ready state

        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Execute a SPARQL query on the store
   *
   * Supports SELECT, ASK, CONSTRUCT, and DESCRIBE queries.
   * This is a placeholder that delegates to the underlying store's query method.
   * Full SPARQL implementation is handled by Agent 2.
   *
   * @param {string} query - SPARQL query string
   * @returns {Promise<Array<object>|boolean>} Query results (array for SELECT/CONSTRUCT, boolean for ASK)
   * @throws {Error} If bridge is not ready or query is invalid
   *
   * @example
   * ```javascript
   * const results = await bridge.sparqlQuery('SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10');
   * console.log(`Found ${results.length} results`);
   *
   * const exists = await bridge.sparqlQuery('ASK { <http://example.org/s> ?p ?o }');
   * console.log(`Triple exists: ${exists}`);
   * ```
   */
  async sparqlQuery(query) {
    this.#validateReady();
    validateNonEmptyString(query, 'query');

    const tracer = getTracer();
    return tracer.startActiveSpan(BRIDGE_OPERATIONS.SPARQL_QUERY, {
      attributes: {
        'bridge.state': this.#state,
        'query.length': query.length,
        'query.type': this.#detectQueryType(query),
      },
    }, async (span) => {
      try {
        this.#state = 'Operating';

        // Validate store has query method
        if (typeof this.#store.query !== 'function') {
          throw new Error('Store does not support SPARQL queries');
        }

        const results = this.#store.query(query);

        // Convert to array if needed (for SELECT/CONSTRUCT)
        const normalizedResults = Array.isArray(results)
          ? results
          : (typeof results === 'boolean' ? results : Array.from(results || []));

        this.#stats.queryCount++;
        this.#state = 'Ready';

        span.setAttribute('results.type', typeof normalizedResults);
        if (Array.isArray(normalizedResults)) {
          span.setAttribute('results.count', normalizedResults.length);
        }
        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return normalizedResults;
      } catch (error) {
        this.#stats.errorCount++;
        this.#state = 'Ready'; // Recover to Ready state

        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Detect SPARQL query type from query string
   *
   * @param {string} query - SPARQL query string
   * @returns {string} Query type (SELECT, ASK, CONSTRUCT, DESCRIBE, or UNKNOWN)
   * @private
   */
  #detectQueryType(query) {
    const normalized = query.trim().toUpperCase();
    if (normalized.startsWith('SELECT')) return 'SELECT';
    if (normalized.startsWith('ASK')) return 'ASK';
    if (normalized.startsWith('CONSTRUCT')) return 'CONSTRUCT';
    if (normalized.startsWith('DESCRIBE')) return 'DESCRIBE';
    return 'UNKNOWN';
  }

  /**
   * Clean up resources and destroy the bridge
   *
   * **Poka-Yoke**: Terminal state prevents further operations
   */
  destroy() {
    this.#state = 'Destroyed';
    this.#store = null;
  }
}

/**
 * Export bridge operations for use in other modules
 */
export { BRIDGE_OPERATIONS };

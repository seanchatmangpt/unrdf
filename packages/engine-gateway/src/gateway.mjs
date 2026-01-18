/**
 * @fileoverview Engine Gateway - Route operations to correct engine
 * @module @unrdf/engine-gateway/gateway
 */

import { createStore as createOxigraphStore } from '@unrdf/oxigraph';
import { detectOperation, OperationType } from './operation-detector.mjs';
import { validateEngine, validateQuery } from './validators.mjs';

/**
 * Engine Gateway - Routes RDF operations to the correct engine
 *
 * Enforces the μ(O) principle:
 * - Oxigraph: SPARQL queries, fast batch operations
 * - N3: Streaming only (via @unrdf/core/rdf/n3-justified-only)
 *
 * @class EngineGateway
 */
export class EngineGateway {
  /**
   * Create an Engine Gateway
   * @param {Object} [options] - Configuration options
   * @param {boolean} [options.strict=true] - Enforce strict Oxigraph-only mode
   * @param {Function} [options.onViolation] - Callback for policy violations
   */
  constructor(options = {}) {
    this.strict = options.strict ?? true;
    this.onViolation = options.onViolation || (() => {});
    this.store = createOxigraphStore();
    this.violations = [];
  }

  /**
   * Execute a SPARQL query
   * @param {string} query - SPARQL query string
   * @param {Object} [options] - Query options
   * @returns {Promise<Object>} Query results
   * @throws {Error} If query is invalid
   */
  async query(query, options = {}) {
    validateQuery(query);

    const operation = detectOperation(query);

    if (this.strict && operation === OperationType.N3_ONLY) {
      const violation = {
        type: 'N3_USAGE',
        query,
        timestamp: Date.now(),
      };
      this.violations.push(violation);
      this.onViolation(violation);
      throw new Error('N3 Store usage detected - use Oxigraph for SPARQL queries');
    }

    // Route to Oxigraph
    return this.store.query(query);
  }

  /**
   * Add quads to the store
   * @param {...import('@unrdf/core').Quad} quads - Quads to add
   * @returns {EngineGateway} This gateway for chaining
   */
  add(...quads) {
    this.store.addAll(quads);
    return this;
  }

  /**
   * Get violation statistics
   * @returns {Object} Violation statistics
   */
  getViolations() {
    return {
      count: this.violations.length,
      violations: this.violations,
    };
  }

  /**
   * Clear all data
   */
  clear() {
    this.violations = [];
  }
}

/**
 * Create an Engine Gateway instance
 * @param {Object} [options] - Configuration options
 * @returns {EngineGateway} Gateway instance
 *
 * @example
 * const gateway = createGateway({ strict: true });
 * await gateway.query('SELECT * WHERE { ?s ?p ?o }');
 */
export function createGateway(options) {
  return new EngineGateway(options);
}

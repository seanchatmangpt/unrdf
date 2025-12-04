/**
 * @file EngineGateway - Enforce μ(O) Routing Rules
 * @module @unrdf/engine-gateway/gateway
 *
 * Routes RDF operations to appropriate engine (Oxigraph or N3) per μ(O) rules
 */

import { detectOperationType, N3_ONLY_OPS, OXIGRAPH_OPS } from './operation-detector.mjs'
import { validateN3Usage, validateOxigraphUsage } from './validators.mjs'

/**
 * EngineGateway: Routes operations to correct engine per μ(O)
 *
 * @example
 * const gateway = new EngineGateway({ store });
 *
 * // Routes to Oxigraph (authoritative)
 * const results = gateway.route('query', sparql);
 *
 * // Routes to N3 (justified case) and re-enters Oxigraph
 * const parsed = gateway.route('stream-parse', stream);
 */
export class EngineGateway {
  /**
   * Create EngineGateway instance
   *
   * @param {Object} config - Configuration
   * @param {OxigraphStore} config.store - Oxigraph store instance
   * @param {boolean} [config.enforce=true] - Enforce μ(O) rules strictly
   * @param {boolean} [config.verbose=false] - Log routing decisions
   */
  constructor(config = {}) {
    this.store = config.store
    this.enforce = config.enforce !== false
    this.verbose = config.verbose || false
  }

  /**
   * Route operation to appropriate engine
   *
   * @param {string} operation - Operation name
   * @param {...*} args - Arguments to pass to operation
   * @returns {*} Result from routed operation
   * @throws {Error} If operation violates μ(O) rules and enforcement enabled
   */
  async route(operation, ...args) {
    if (this.enforce) {
      validateN3Usage(operation)
    }

    const engine = detectOperationType(operation)

    if (this.verbose) {
      console.log(`[μ(O)] Routing "${operation}" to ${engine}`)
    }

    if (engine === 'n3') {
      return this.routeToN3(operation, ...args)
    }

    if (engine === 'oxigraph') {
      return this.routeToOxigraph(operation, ...args)
    }

    throw new Error(`Unknown engine for operation: ${operation}`)
  }

  /**
   * Route to Oxigraph (authoritative engine)
   *
   * @private
   * @param {string} operation - Operation name
   * @param {...*} args - Arguments
   * @returns {*} Result from Oxigraph operation
   */
  routeToOxigraph(operation, ...args) {
    const op = operation.toLowerCase()

    if (!this.store) {
      throw new Error('EngineGateway: Oxigraph store not initialized')
    }

    // Map operation names to store methods
    const operationMap = {
      query: 'query',
      update: 'update',
      add: 'addQuad',
      delete: 'removeQuad',
      match: 'match',
      has: 'has',
      load: 'load',
      dump: 'dump',
      size: 'size',
      clear: 'clear',
      parse: 'load', // parse is load in Oxigraph
      serialize: 'dump' // serialize is dump in Oxigraph
    }

    const method = operationMap[op] || op

    if (typeof this.store[method] !== 'function') {
      throw new Error(`Oxigraph does not support operation: ${operation}`)
    }

    return this.store[method](...args)
  }

  /**
   * Route to N3 (justified boundary cases only)
   * Note: Implementation depends on @unrdf/core/rdf/minimal-n3-integration module
   *
   * @private
   * @param {string} operation - Operation name
   * @param {...*} args - Arguments
   * @returns {*} Result (must re-enter Oxigraph)
   */
  async routeToN3(operation, ...args) {
    const op = operation.toLowerCase()

    // Dynamic import to avoid hard dependency on minimal-n3-integration
    const minimalN3 = await import('@unrdf/core/rdf/minimal-n3-integration')

    const operationMap = {
      'stream-parse': 'streamParse',
      'stream-serialize': 'streamSerialize',
      'n3-reason': 'applyN3Rules',
      'permissive-parse': 'parsePermissive',
      'rdf-transform': 'transformRdfStructure'
    }

    const method = operationMap[op]
    if (!method || typeof minimalN3[method] !== 'function') {
      throw new Error(`N3 does not support justified operation: ${operation}`)
    }

    // Call N3 operation (must re-enter Oxigraph per μ(O))
    return minimalN3[method](this.store, ...args)
  }

  /**
   * Check if operation can be routed
   *
   * @param {string} operation - Operation name
   * @returns {boolean} True if operation can be routed
   */
  canRoute(operation) {
    const engine = detectOperationType(operation)
    return engine === 'n3' || engine === 'oxigraph'
  }

  /**
   * Get routing metadata for operation
   *
   * @param {string} operation - Operation name
   * @returns {Object} Routing metadata
   */
  getRoutingMetadata(operation) {
    const engine = detectOperationType(operation)
    const validation = engine === 'n3' ? validateN3Usage(operation) : validateOxigraphUsage(operation)

    return {
      operation,
      engine,
      ...validation
    }
  }
}

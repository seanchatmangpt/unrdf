/**
 * @file μ(O) Validators - Enforce minimal-N3 rules
 * @module @unrdf/engine-gateway/validators
 *
 * Validators ensure that N3 is ONLY used in 5 justified cases
 * All other operations MUST use Oxigraph
 */

import { detectOperationType } from './operation-detector.mjs'

/**
 * Validate that N3 is not used for storage operations
 *
 * @param {string} operation - Operation name
 * @throws {Error} If N3 is used for storage
 * @returns {boolean} True if valid (or operation is not storage)
 */
export function validateN3NotForStorage(operation) {
  const storageOps = ['add', 'delete', 'clear']
  if (storageOps.includes(operation.toLowerCase())) {
    throw new Error(
      `μ(O) Violation: N3 cannot be used for storage operation "${operation}". ` +
      `Use Oxigraph instead.`
    )
  }
  return true
}

/**
 * Validate that N3 is not used for SPARQL operations
 *
 * @param {string} operation - Operation name
 * @throws {Error} If N3 is used for SPARQL
 * @returns {boolean} True if valid (or operation is not SPARQL)
 */
export function validateN3NotForSparql(operation) {
  const sparqlOps = ['query', 'update']
  if (sparqlOps.includes(operation.toLowerCase())) {
    throw new Error(
      `μ(O) Violation: N3 cannot be used for SPARQL operation "${operation}". ` +
      `Use Oxigraph instead.`
    )
  }
  return true
}

/**
 * Validate that N3 is not used for basic parsing
 *
 * @param {string} operation - Operation name
 * @throws {Error} If N3 is used for basic parsing
 * @returns {boolean} True if valid (or operation is not basic parsing)
 */
export function validateN3NotForBasicParsing(operation) {
  if (operation.toLowerCase() === 'parse' && !operation.includes('permissive')) {
    throw new Error(
      `μ(O) Violation: N3 cannot be used for basic parsing. ` +
      `Use Oxigraph.load() instead. N3 parsing is only justified for: ` +
      `stream-parse (backpressure) or permissive-parse (malformed RDF).`
    )
  }
  return true
}

/**
 * Validate that N3 is not used for basic serialization
 *
 * @param {string} operation - Operation name
 * @throws {Error} If N3 is used for basic serialization
 * @returns {boolean} True if valid (or operation is not basic serialization)
 */
export function validateN3NotForBasicSerialization(operation) {
  if (operation.toLowerCase() === 'serialize' && !operation.includes('stream')) {
    throw new Error(
      `μ(O) Violation: N3 cannot be used for basic serialization. ` +
      `Use Oxigraph.dump() instead. N3 serialization is only justified for ` +
      `stream-serialize (output backpressure).`
    )
  }
  return true
}

/**
 * Comprehensive N3 usage validation
 * Throws error if N3 would be used outside the 5 justified cases
 *
 * @param {string} operation - Operation name
 * @throws {Error} If N3 usage violates μ(O) rules
 * @returns {Object} Validation result
 */
export function validateN3Usage(operation) {
  const engine = detectOperationType(operation)

  if (engine === 'oxigraph') {
    return {
      valid: true,
      engine: 'oxigraph',
      message: `Operation "${operation}" correctly routes to Oxigraph`
    }
  }

  // N3 operation - validate it's one of the 5 justified cases
  const justifiedCases = {
    'stream-parse': 'Streaming parsing with backpressure handling',
    'stream-serialize': 'Streaming serialization to sink',
    'n3-reason': 'N3 rule-based forward-chaining reasoning',
    'permissive-parse': 'Malformed RDF recovery with permissive parsing',
    'rdf-transform': 'Structural transforms not expressible in SPARQL'
  }

  if (justifiedCases[operation.toLowerCase()]) {
    return {
      valid: true,
      engine: 'n3',
      justification: justifiedCases[operation.toLowerCase()],
      reenterOxigraph: true,
      message: `Operation "${operation}" is a justified N3 use case`
    }
  }

  throw new Error(
    `μ(O) Violation: Operation "${operation}" is not a justified N3 use case. ` +
    `N3 is only allowed for: ${Object.keys(justifiedCases).join(', ')}. ` +
    `For all other operations, use Oxigraph.`
  )
}

/**
 * Validate Oxigraph usage is appropriate
 *
 * @param {string} operation - Operation name
 * @returns {Object} Validation result
 */
export function validateOxigraphUsage(operation) {
  const engine = detectOperationType(operation)

  if (engine === 'oxigraph') {
    return {
      valid: true,
      engine: 'oxigraph',
      message: `Operation "${operation}" correctly uses Oxigraph as authoritative engine`
    }
  }

  return {
    valid: false,
    engine: 'oxigraph',
    message: `Operation "${operation}" should use Oxigraph but was routed to N3`
  }
}

/**
 * Validate that a store operation re-enters Oxigraph
 * (applies to all N3 operations - they must convert result back to Oxigraph)
 *
 * @param {string} operation - Operation name
 * @returns {Object} Validation result
 */
export function validateReenterOxigraph(operation) {
  const engine = detectOperationType(operation)

  if (engine === 'oxigraph') {
    return {
      valid: true,
      mustReenter: false,
      message: `Operation "${operation}" stays in Oxigraph`
    }
  }

  // N3 operations must re-enter Oxigraph
  return {
    valid: true,
    mustReenter: true,
    message: `Operation "${operation}" must convert N3 result back to Oxigraph store`
  }
}

/**
 * @file Operation Type Detection
 * @module @unrdf/engine-gateway/operation-detector
 *
 * Detects operation type and routes to correct engine per μ(O) rules
 */

/**
 * N3-justified operations (5 only)
 * These are operations where Oxigraph cannot perform the task
 */
export const N3_ONLY_OPS = new Set([
  'stream-parse',
  'stream-serialize',
  'n3-reason',
  'permissive-parse',
  'rdf-transform'
])

/**
 * Oxigraph-primary operations
 * These are all other operations that Oxigraph should handle
 */
export const OXIGRAPH_OPS = new Set([
  'query',
  'update',
  'add',
  'delete',
  'match',
  'has',
  'load',
  'dump',
  'size',
  'clear',
  'parse',
  'serialize'
])

/**
 * Detect if an operation requires N3 or should use Oxigraph
 *
 * @param {string} operation - Operation name
 * @returns {string} Engine type ('n3' | 'oxigraph')
 *
 * @example
 * detectOperationType('query') // 'oxigraph'
 * detectOperationType('stream-parse') // 'n3'
 */
export function detectOperationType(operation) {
  if (!operation || typeof operation !== 'string') {
    return 'oxigraph' // Default to Oxigraph for unknown operations
  }

  const normalized = operation.toLowerCase().trim()

  // Check N3-only operations
  if (N3_ONLY_OPS.has(normalized)) {
    return 'n3'
  }

  // Check Oxigraph operations
  if (OXIGRAPH_OPS.has(normalized)) {
    return 'oxigraph'
  }

  // Default: Oxigraph for unlisted operations
  return 'oxigraph'
}

/**
 * Check if operation should use N3
 *
 * @param {string} operation - Operation name
 * @returns {boolean} True if N3 is justified
 */
export function isN3Operation(operation) {
  return detectOperationType(operation) === 'n3'
}

/**
 * Check if operation should use Oxigraph
 *
 * @param {string} operation - Operation name
 * @returns {boolean} True if Oxigraph is appropriate
 */
export function isOxigraphOperation(operation) {
  return detectOperationType(operation) === 'oxigraph'
}

/**
 * Get metadata about an operation
 *
 * @param {string} operation - Operation name
 * @returns {Object} Operation metadata
 */
export function getOperationMetadata(operation) {
  const engine = detectOperationType(operation)
  const isJustifiedN3 = N3_ONLY_OPS.has(operation.toLowerCase())

  const metadata = {
    operation,
    engine,
    isN3: engine === 'n3',
    isOxigraph: engine === 'oxigraph',
    justifiedN3: isJustifiedN3
  }

  // Add specific metadata for known operations
  if (isJustifiedN3) {
    const reasons = {
      'stream-parse': 'Input streaming requires backpressure handling',
      'stream-serialize': 'Output streaming requires backpressure handling',
      'n3-reason': 'N3 rule-based forward-chaining logic',
      'permissive-parse': 'Malformed RDF recovery with permissive parsing',
      'rdf-transform': 'Structural transforms not expressible in SPARQL'
    }
    metadata.justification = reasons[operation.toLowerCase()] || 'N3-specific capability'
    metadata.reenterOxigraph = true
  } else {
    metadata.justification = 'Oxigraph is the authoritative engine'
    metadata.reenterOxigraph = false
  }

  return metadata
}

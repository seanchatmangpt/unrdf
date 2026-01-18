/**
 * @fileoverview Operation detector - Classify RDF operations
 * @module @unrdf/engine-gateway/operation-detector
 */

/**
 * Operation types
 * @enum {string}
 */
export const OperationType = {
  SPARQL_QUERY: 'SPARQL_QUERY',
  SPARQL_UPDATE: 'SPARQL_UPDATE',
  N3_STREAMING: 'N3_STREAMING',
  N3_ONLY: 'N3_ONLY',
  BATCH_INSERT: 'BATCH_INSERT',
};

/**
 * Detect the operation type from a query or code
 * @param {string} input - Query string or code
 * @returns {OperationType} Detected operation type
 *
 * @example
 * detectOperation('SELECT * WHERE { ?s ?p ?o }'); // SPARQL_QUERY
 * detectOperation('INSERT DATA { <s> <p> <o> }'); // SPARQL_UPDATE
 */
export function detectOperation(input) {
  if (typeof input !== 'string') {
    throw new TypeError('Input must be a string');
  }

  const trimmed = input.trim();

  // SPARQL queries
  if (/^\s*(SELECT|ASK|CONSTRUCT|DESCRIBE)/i.test(trimmed)) {
    return OperationType.SPARQL_QUERY;
  }

  // SPARQL updates
  if (/^\s*(INSERT|DELETE|LOAD|CLEAR|CREATE|DROP)/i.test(trimmed)) {
    return OperationType.SPARQL_UPDATE;
  }

  // N3 streaming patterns (acceptable)
  if (/Parser|StreamParser/i.test(trimmed)) {
    return OperationType.N3_STREAMING;
  }

  // N3 Store usage (violation)
  if (/new\s+Store|from\s+['"]n3['"]/i.test(trimmed)) {
    return OperationType.N3_ONLY;
  }

  // Batch insert
  if (/addAll|bulkInsert/i.test(trimmed)) {
    return OperationType.BATCH_INSERT;
  }

  // Default to SPARQL query
  return OperationType.SPARQL_QUERY;
}

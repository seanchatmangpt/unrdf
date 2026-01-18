/**
 * @fileoverview Validators for engine operations
 * @module @unrdf/engine-gateway/validators
 */

/**
 * Validate that the correct engine is being used
 * @param {string} engineName - Engine name ('oxigraph' or 'n3')
 * @param {string} context - Usage context
 * @throws {Error} If N3 is used for non-streaming operations
 *
 * @example
 * validateEngine('oxigraph', 'SPARQL query'); // OK
 * validateEngine('n3', 'SPARQL query'); // Throws
 */
export function validateEngine(engineName, context) {
  if (typeof engineName !== 'string') {
    throw new TypeError('engineName must be a string');
  }

  if (typeof context !== 'string') {
    throw new TypeError('context must be a string');
  }

  if (engineName === 'n3' && !context.includes('streaming')) {
    throw new Error(
      `N3 usage detected in non-streaming context: ${context}. ` +
      'Use Oxigraph for SPARQL queries and batch operations.'
    );
  }
}

/**
 * Validate a SPARQL query string
 * @param {string} query - SPARQL query
 * @returns {boolean} True if valid
 * @throws {Error} If query is invalid
 *
 * @example
 * validateQuery('SELECT * WHERE { ?s ?p ?o }'); // true
 * validateQuery(''); // Throws
 */
export function validateQuery(query) {
  if (typeof query !== 'string') {
    throw new TypeError('Query must be a string');
  }

  if (!query.trim()) {
    throw new Error('Query cannot be empty');
  }

  // Check for basic SPARQL structure
  const hasKeyword = /\b(SELECT|ASK|CONSTRUCT|DESCRIBE|INSERT|DELETE)\b/i.test(query);

  if (!hasKeyword) {
    throw new Error('Invalid SPARQL query: missing query type keyword');
  }

  return true;
}

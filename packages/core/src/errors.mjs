/**
 * @file Custom error classes with enhanced debugging capabilities
 * @module @unrdf/core/errors
 */

/**
 * Base error class for UNRDF errors
 * Provides error codes, context, and documentation links
 */
export class UnrdfError extends Error {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Additional context
   * @param {string} [code] - Error code
   * @param {string} [docsUrl] - Documentation URL
   */
  constructor(message, context = {}, code = 'ERR_UNRDF', docsUrl = null) {
    super(message);
    this.name = this.constructor.name;
    this.code = code;
    this.context = context;
    this.docsUrl = docsUrl || `https://unrdf.dev/errors/${code}`;
    this.timestamp = new Date().toISOString();

    // Capture stack trace
    Error.captureStackTrace(this, this.constructor);

    // Filter internal frames from stack trace
    this._filterStackTrace();
  }

  /**
   * Filter internal implementation frames from stack trace
   * @private
   */
  _filterStackTrace() {
    if (!this.stack) return;

    const lines = this.stack.split('\n');
    const filtered = lines.filter((line) => {
      // Keep error message line
      if (line.includes(this.name)) return true;

      // Filter out node_modules and internal frames
      if (line.includes('node_modules')) return false;
      if (line.includes('node:internal')) return false;

      return true;
    });

    this.stack = filtered.join('\n');
  }

  /**
   * Get formatted error message with context
   * @returns {string} Formatted error message
   */
  toString() {
    const parts = [
      `${this.name} [${this.code}]: ${this.message}`,
      '',
      'Context:',
      JSON.stringify(this.context, null, 2),
      '',
      `Documentation: ${this.docsUrl}`,
      `Timestamp: ${this.timestamp}`,
    ];

    return parts.join('\n');
  }

  /**
   * Convert error to JSON
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      name: this.name,
      code: this.code,
      message: this.message,
      context: this.context,
      docsUrl: this.docsUrl,
      timestamp: this.timestamp,
      stack: this.stack,
    };
  }
}

/**
 * Validation error for invalid RDF data
 */
export class ValidationError extends UnrdfError {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Validation context
   * @param {string} [code] - Error code
   */
  constructor(message, context = {}, code = 'ERR_VALIDATION') {
    super(message, context, code);
  }
}

/**
 * Configuration error for invalid settings
 */
export class ConfigError extends UnrdfError {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Configuration context
   * @param {string} [code] - Error code
   */
  constructor(message, context = {}, code = 'ERR_CONFIG') {
    super(message, context, code);
  }
}

/**
 * Query error for SPARQL execution failures
 */
export class QueryError extends UnrdfError {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Query context
   * @param {string} [code] - Error code
   */
  constructor(message, context = {}, code = 'ERR_QUERY') {
    super(message, context, code);
  }
}

/**
 * Store error for RDF store operations
 */
export class StoreError extends UnrdfError {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Store context
   * @param {string} [code] - Error code
   */
  constructor(message, context = {}, code = 'ERR_STORE') {
    super(message, context, code);
  }
}

/**
 * Network error for federation and remote operations
 */
export class NetworkError extends UnrdfError {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Network context
   * @param {string} [code] - Error code
   */
  constructor(message, context = {}, code = 'ERR_NETWORK') {
    super(message, context, code);
  }
}

/**
 * Timeout error for operations that exceed time limits
 */
export class TimeoutError extends UnrdfError {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Timeout context
   * @param {string} [code] - Error code
   */
  constructor(message, context = {}, code = 'ERR_TIMEOUT') {
    super(message, context, code);
  }
}

/**
 * Parser error for RDF parsing failures
 */
export class ParserError extends UnrdfError {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Parser context
   * @param {string} [code] - Error code
   */
  constructor(message, context = {}, code = 'ERR_PARSER') {
    super(message, context, code);
  }
}

/**
 * Error codes with default messages and solutions
 */
export const ERROR_CODES = {
  // Validation errors
  ERR_INVALID_QUAD_SUBJECT: {
    message: 'Invalid quad subject: must be NamedNode or BlankNode',
    solution: 'Ensure the subject is created with namedNode() or blankNode()',
    example: "const subject = namedNode('http://example.org/alice')",
  },
  ERR_INVALID_QUAD_PREDICATE: {
    message: 'Invalid quad predicate: must be NamedNode',
    solution: 'Ensure the predicate is created with namedNode()',
    example: "const predicate = namedNode('http://xmlns.com/foaf/0.1/name')",
  },
  ERR_INVALID_QUAD_OBJECT: {
    message: 'Invalid quad object: must be NamedNode, BlankNode, or Literal',
    solution: 'Ensure the object is created with namedNode(), blankNode(), or literal()',
    example: "const object = literal('Alice')",
  },
  ERR_INVALID_TERM_TYPE: {
    message: 'Invalid term type',
    solution: 'Use namedNode, literal, blankNode, variable, or defaultGraph',
    example: "const term = namedNode('http://example.org/resource')",
  },

  // Query errors
  ERR_INVALID_SPARQL: {
    message: 'Invalid SPARQL query syntax',
    solution: 'Check SPARQL syntax and ensure all variables are properly bound',
    example: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10',
  },
  ERR_QUERY_EXECUTION: {
    message: 'Query execution failed',
    solution: 'Check query syntax, store state, and data validity',
    example: 'Enable debug mode with DEBUG=unrdf:query for details',
  },
  ERR_QUERY_TIMEOUT: {
    message: 'Query execution timeout',
    solution: 'Simplify query, add LIMIT clause, or increase timeout',
    example: 'SELECT ?s WHERE { ?s ?p ?o } LIMIT 100',
  },

  // Store errors
  ERR_STORE_NOT_FOUND: {
    message: 'Store not found or not initialized',
    solution: 'Create store with createStore() before operations',
    example: 'const store = createStore()',
  },
  ERR_STORE_READ_ONLY: {
    message: 'Cannot modify read-only store',
    solution: 'Use a writable store for add/remove operations',
    example: 'const store = createStore({ readOnly: false })',
  },
  ERR_QUAD_NOT_FOUND: {
    message: 'Quad not found in store',
    solution: 'Verify quad exists before removal',
    example: 'const quads = getQuads(store, subject, predicate, object)',
  },

  // Network errors
  ERR_NETWORK_REQUEST: {
    message: 'Network request failed',
    solution: 'Check network connection and endpoint availability',
    example: 'Verify endpoint URL is accessible',
  },
  ERR_FEDERATION_TIMEOUT: {
    message: 'Federation query timeout',
    solution: 'Check remote endpoint performance or increase timeout',
    example: 'await federatedQuery(query, { timeout: 30000 })',
  },

  // Configuration errors
  ERR_INVALID_CONFIG: {
    message: 'Invalid configuration',
    solution: 'Verify configuration matches schema requirements',
    example: 'const config = { baseIRI: "http://example.org/" }',
  },
  ERR_MISSING_REQUIRED_PARAM: {
    message: 'Missing required parameter',
    solution: 'Provide all required parameters',
    example: 'Check function signature and provide missing arguments',
  },

  // Parser errors
  ERR_PARSE_TURTLE: {
    message: 'Failed to parse Turtle syntax',
    solution: 'Verify Turtle syntax is valid',
    example: 'Use online validator at https://www.w3.org/2015/03/ShExValidata/',
  },
  ERR_PARSE_JSONLD: {
    message: 'Failed to parse JSON-LD',
    solution: 'Verify JSON-LD structure and context',
    example: 'Check @context and ensure valid JSON structure',
  },
};

/**
 * Create error with predefined code
 * @param {string} code - Error code
 * @param {Object} [context] - Additional context
 * @returns {UnrdfError} Error instance
 *
 * @example
 * throw createError('ERR_INVALID_QUAD_SUBJECT', {
 *   received: quad.subject.termType,
 *   expected: ['NamedNode', 'BlankNode']
 * });
 */
export function createError(code, context = {}) {
  const errorDef = ERROR_CODES[code];

  if (!errorDef) {
    return new UnrdfError(`Unknown error code: ${code}`, context, code);
  }

  const message = errorDef.message;
  const enrichedContext = {
    ...context,
    solution: errorDef.solution,
    example: errorDef.example,
  };

  // Determine error class based on code (check specific patterns first)
  // Check for timeout first (most specific)
  if (code.includes('TIMEOUT')) {
    return new TimeoutError(message, enrichedContext, code);
  }
  // Check for query errors (includes ERR_INVALID_SPARQL)
  if (code.startsWith('ERR_QUERY_') || code === 'ERR_INVALID_SPARQL') {
    return new QueryError(message, enrichedContext, code);
  }
  // Check for validation errors (but not ERR_INVALID_SPARQL)
  if (code.startsWith('ERR_INVALID_') || code.startsWith('ERR_VALIDATION')) {
    return new ValidationError(message, enrichedContext, code);
  }
  if (code.startsWith('ERR_STORE_')) {
    return new StoreError(message, enrichedContext, code);
  }
  if (code.startsWith('ERR_NETWORK_') || code.startsWith('ERR_FEDERATION_')) {
    return new NetworkError(message, enrichedContext, code);
  }
  if (code.startsWith('ERR_CONFIG_') || code.startsWith('ERR_MISSING_')) {
    return new ConfigError(message, enrichedContext, code);
  }
  if (code.startsWith('ERR_PARSE_')) {
    return new ParserError(message, enrichedContext, code);
  }

  return new UnrdfError(message, enrichedContext, code);
}

/**
 * Wrap error with additional context
 * @param {Error} error - Original error
 * @param {Object} context - Additional context
 * @returns {UnrdfError} Wrapped error
 *
 * @example
 * try {
 *   // ... some operation
 * } catch (err) {
 *   throw wrapError(err, { operation: 'addQuad', quad });
 * }
 */
export function wrapError(error, context = {}) {
  if (error instanceof UnrdfError) {
    error.context = { ...error.context, ...context };
    return error;
  }

  return new UnrdfError(error.message, { ...context, originalError: error.name }, 'ERR_WRAPPED');
}

/**
 * Assert condition or throw error
 * @param {boolean} condition - Condition to check
 * @param {string} code - Error code
 * @param {Object} [context] - Error context
 *
 * @example
 * assertError(quad.subject.termType === 'NamedNode', 'ERR_INVALID_QUAD_SUBJECT', {
 *   received: quad.subject.termType
 * });
 */
export function assertError(condition, code, context = {}) {
  if (!condition) {
    throw createError(code, context);
  }
}

/**
 * @file SPARQL Query Executor
 * @module cli/commands/sync/sparql-executor
 * @description Executes SPARQL queries against RDF ontology store
 */

/**
 * Common RDF prefixes for SPARQL queries
 */
const COMMON_PREFIXES = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  owl: 'http://www.w3.org/2002/07/owl#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  skos: 'http://www.w3.org/2004/02/skos/core#',
  dc: 'http://purl.org/dc/elements/1.1/',
  dcterms: 'http://purl.org/dc/terms/',
  foaf: 'http://xmlns.com/foaf/0.1/',
  schema: 'http://schema.org/',
};

/**
 * Custom error class for SPARQL execution failures
 * @extends Error
 */
export class SparqlExecutionError extends Error {
  /**
   * Create a SPARQL execution error
   * @param {string} message - Error message
   * @param {Object} [options] - Error options
   * @param {Error} [options.cause] - Original error that caused this error
   * @param {string} [options.query] - The SPARQL query that failed
   * @param {string} [options.phase] - Execution phase where error occurred
   */
  constructor(message, options = {}) {
    super(message);
    this.name = 'SparqlExecutionError';
    this.cause = options.cause ?? null;
    this.query = options.query ?? null;
    this.phase = options.phase ?? null;
  }
}

/**
 * Execute SPARQL query and return results
 * @param {Object} store - RDF store
 * @param {string} query - SPARQL query string
 * @param {Object} prefixes - Prefix mappings
 * @param {Object} options - Query options
 * @param {number} [options.timeout=5000] - Query timeout in milliseconds
 * @returns {Promise<Array>} Query results as array of objects
 * @throws {SparqlExecutionError} If query execution fails
 */
export async function executeSparqlQuery(store, query, prefixes = {}, options = {}) {
  const { timeout = 5000 } = options;

  // Validate store
  if (!store) {
    throw new SparqlExecutionError('Invalid store: store is null or undefined', {
      phase: 'validate',
    });
  }

  // Validate query
  if (!query || typeof query !== 'string' || query.trim() === '') {
    throw new SparqlExecutionError('Invalid query: query is empty or not a string', {
      phase: 'validate',
    });
  }

  const prefixDeclarations = buildPrefixDeclarations(prefixes);
  const prefixLineCount = prefixDeclarations.split('\n').length;
  const fullQuery = prefixDeclarations + '\n' + query.trim();

  try {
    const results = await Promise.race([
      executeQueryInternal(store, fullQuery),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error(`Query execution timed out after ${timeout}ms`)), timeout)
      ),
    ]);
    return transformResults(results);
  } catch (err) {
    if (err instanceof SparqlExecutionError) {
      throw err;
    }

    // Handle timeout errors
    if (err.message.includes('timed out')) {
      throw new SparqlExecutionError(
        `Query execution timed out after ${timeout}ms\n` +
        `  Fix: Simplify query, add LIMIT clause, or increase timeout\n` +
        `  Query preview: ${query.trim().split('\n')[0].substring(0, 80)}...`,
        {
          cause: err,
          query: fullQuery,
          phase: 'execute',
        }
      );
    }

    // Extract line/column information from error message
    const lineInfo = extractLineInfo(err.message, prefixLineCount);
    const syntaxSuggestions = getSyntaxSuggestions(err.message);

    let errorMsg = `SPARQL query failed: ${err.message}`;
    if (lineInfo.line !== null) {
      errorMsg += `\n  Line ${lineInfo.userLine} (${lineInfo.queryLine} in full query): ${lineInfo.content || '(error location)'}`;
    }
    if (syntaxSuggestions.length > 0) {
      errorMsg += '\n\nPossible fixes:\n  ' + syntaxSuggestions.join('\n  ');
    }

    throw new SparqlExecutionError(errorMsg, {
      cause: err,
      query: fullQuery,
      phase: 'execute',
      line: lineInfo.line,
      column: lineInfo.column,
    });
  }
}

/**
 * Internal query execution that tries multiple store methods
 * @param {Object} store - RDF store
 * @param {string} query - SPARQL query string
 * @returns {Promise<*>} Query results
 * @throws {Error} If store does not support SPARQL queries
 */
async function executeQueryInternal(store, query) {
  if (typeof store.query === 'function') {
    return store.query(query);
  }
  if (typeof store.execute === 'function') {
    return store.execute(query);
  }
  throw new Error('Store does not support SPARQL queries');
}

/**
 * Build PREFIX declarations string from prefix map
 * @param {Object} prefixes - Custom prefix mappings
 * @returns {string} PREFIX declarations as string
 */
export function buildPrefixDeclarations(prefixes = {}) {
  const allPrefixes = { ...COMMON_PREFIXES, ...prefixes };
  return Object.entries(allPrefixes)
    .map(([prefix, uri]) => `PREFIX ${prefix}: <${uri}>`)
    .join('\n');
}

/**
 * Transform SPARQL results to template-friendly format
 * @param {*} results - Raw SPARQL results (can be array, boolean, or iterable)
 * @returns {Array<Object>} Transformed results as array of objects
 */
export function transformResults(results) {
  // Handle null/undefined
  if (results === null || results === undefined) {
    return [];
  }

  // Handle boolean ASK results
  if (typeof results === 'boolean') {
    return [{ result: results, _isBoolean: true }];
  }

  // Handle empty array
  if (Array.isArray(results) && results.length === 0) {
    return [];
  }

  // Convert to array if iterable
  let bindings = [];
  if (Array.isArray(results)) {
    bindings = results;
  } else if (results?.results?.bindings) {
    bindings = results.results.bindings;
  } else if (results?.bindings) {
    bindings = results.bindings;
  } else if (typeof results?.[Symbol.iterator] === 'function') {
    bindings = [...results];
  }

  if (bindings.length === 0) {
    return [];
  }

  // Check if these are quad results (CONSTRUCT/DESCRIBE)
  const firstBinding = bindings[0];
  if (isQuad(firstBinding)) {
    return transformQuadResults(bindings);
  }

  // Transform SELECT results (Map or object bindings)
  return bindings.map((binding, index) => {
    const row = { _index: index, _meta: {} };

    // Handle Map bindings (raw Oxigraph format)
    const entries = binding instanceof Map ? [...binding.entries()] : Object.entries(binding);

    for (const [key, value] of entries) {
      const cleanKey = key.startsWith('?') ? key.slice(1) : key;
      const varName = '?' + cleanKey;

      if (value === null || value === undefined) {
        row[cleanKey] = null;
        row[varName] = null;
        continue;
      }

      // Extract value and metadata
      const termInfo = extractTermInfo(value);
      // Store both with and without ? prefix for compatibility
      row[cleanKey] = termInfo.value;
      row[varName] = termInfo.value;
      row._meta[cleanKey] = termInfo.meta;

      // Add _localName convenience property for NamedNodes
      if (termInfo.meta.localName !== undefined) {
        row[cleanKey + '_localName'] = termInfo.meta.localName;
        row[varName + '_localName'] = termInfo.meta.localName;
      }
    }

    return row;
  });
}

/**
 * Check if an object is a quad
 * @param {Object} obj - Object to check
 * @returns {boolean} True if object is a quad
 */
function isQuad(obj) {
  return obj && obj.subject && obj.predicate && obj.object;
}

/**
 * Transform quad results from CONSTRUCT/DESCRIBE queries
 * @param {Array} quads - Array of quads
 * @returns {Array<Object>} Transformed quad results
 */
function transformQuadResults(quads) {
  return quads.map((quad, index) => ({
    _index: index,
    _isQuad: true,
    subject: extractValue(quad.subject),
    predicate: extractValue(quad.predicate),
    object: extractValue(quad.object),
    graph: quad.graph ? extractValue(quad.graph) : null,
  }));
}

/**
 * Extract the value from an RDF term
 * @param {Object|string} term - RDF term
 * @returns {string|null} The value
 */
function extractValue(term) {
  if (!term) return null;
  if (typeof term === 'string') return term;
  return term.value ?? term.id ?? String(term);
}

/**
 * Extract term information including value and metadata
 * @param {Object|string} term - RDF term
 * @returns {Object} Object with value and meta properties
 */
function extractTermInfo(term) {
  if (typeof term === 'string') {
    return { value: term, meta: { termType: 'string' } };
  }

  const value = term.value ?? term.id ?? String(term);
  const meta = {
    termType: term.termType ?? 'unknown',
  };

  // Handle NamedNode - extract local name and namespace
  if (term.termType === 'NamedNode' || term.type === 'uri') {
    const hashIndex = value.lastIndexOf('#');
    const slashIndex = value.lastIndexOf('/');
    const splitIndex = Math.max(hashIndex, slashIndex);

    if (splitIndex > -1) {
      meta.localName = value.slice(splitIndex + 1);
      meta.namespace = value.slice(0, splitIndex + 1);
    } else {
      meta.localName = value;
      meta.namespace = '';
    }
  }

  // Handle Literal - extract language and datatype
  if (term.termType === 'Literal' || term.type === 'literal') {
    if (term.language) {
      meta.language = term.language;
    }
    if (term.datatype?.value) {
      meta.datatype = term.datatype.value;
    } else if (term.datatype) {
      meta.datatype = term.datatype;
    }
  }

  return { value, meta };
}

/**
 * Substitute parameters in a SPARQL query template
 * @param {string} template - SPARQL query template with $param placeholders
 * @param {Object} params - Parameter values to substitute
 * @returns {string} Query with parameters substituted
 */
export function substituteParameters(template, params = {}) {
  let result = template;

  for (const [key, value] of Object.entries(params)) {
    const placeholder = `$${key}`;
    const replacement = formatParameterValue(value);
    result = result.replaceAll(placeholder, replacement);
  }

  return result;
}

/**
 * Format a parameter value for SPARQL insertion
 * @param {*} value - Parameter value
 * @returns {string} Formatted SPARQL value
 */
function formatParameterValue(value) {
  // Handle typed parameter objects
  if (value && typeof value === 'object' && 'type' in value) {
    switch (value.type) {
      case 'uri':
        return `<${value.value}>`;

      case 'literal':
        if (value.language) {
          return `"${escapeStringLiteral(value.value)}"@${value.language}`;
        }
        return `"${escapeStringLiteral(value.value)}"`;

      case 'typed-literal':
        return `"${escapeStringLiteral(value.value)}"^^<${value.datatype}>`;

      default:
        return String(value.value);
    }
  }

  // Handle primitive types
  if (typeof value === 'number') {
    return String(value);
  }

  if (typeof value === 'boolean') {
    return String(value);
  }

  if (typeof value === 'string') {
    // Check if it's a prefixed name (e.g., foaf:Person)
    if (isPrefixedName(value)) {
      return value;
    }

    // Check if it's a full URI
    if (value.startsWith('http://') || value.startsWith('https://')) {
      return `<${value}>`;
    }

    // Treat as string literal
    return `"${escapeStringLiteral(value)}"`;
  }

  return String(value);
}

/**
 * Check if a string is a prefixed name
 * @param {string} str - String to check
 * @returns {boolean} True if string is a prefixed name
 */
function isPrefixedName(str) {
  // Prefixed names have format prefix:localName
  // Must start with a letter or underscore, contain a colon, and have no spaces
  return /^[a-zA-Z_][a-zA-Z0-9_-]*:[a-zA-Z_][a-zA-Z0-9_-]*$/.test(str);
}

/**
 * Escape special characters in a SPARQL string literal
 * @param {string} str - String to escape
 * @returns {string} Escaped string
 */
function escapeStringLiteral(str) {
  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');
}

/**
 * Validate a SPARQL query for common issues
 * @param {string} query - SPARQL query to validate
 * @returns {Object} Validation result with valid flag, type, variables, and issues
 */
export function validateSparqlQuery(query) {
  const issues = [];

  // Handle null/empty query
  if (!query || typeof query !== 'string' || query.trim() === '') {
    return {
      valid: false,
      type: null,
      variables: [],
      issues: [{ type: 'error', message: 'Query is empty or not a string' }],
    };
  }

  const trimmedQuery = query.trim();

  // Detect query type
  const typeMatch = trimmedQuery.match(/^\s*(?:PREFIX[^]*?)?(?:BASE[^]*?)?\s*(SELECT|CONSTRUCT|ASK|DESCRIBE)\b/i);
  const type = typeMatch ? typeMatch[1].toUpperCase() : null;

  if (!type) {
    issues.push({ type: 'error', message: 'No valid query type found (SELECT, CONSTRUCT, ASK, DESCRIBE)' });
  }

  // Extract variables from SELECT clause
  const variables = [];
  if (type === 'SELECT') {
    const selectMatch = trimmedQuery.match(/SELECT\s+(?:DISTINCT\s+)?(.+?)\s+WHERE/i);
    if (selectMatch) {
      const varPart = selectMatch[1];
      const varMatches = varPart.matchAll(/\?(\w+)/g);
      for (const match of varMatches) {
        variables.push(match[1]);
      }
    }
  }

  // Check for unbalanced braces
  const openBraces = (trimmedQuery.match(/\{/g) || []).length;
  const closeBraces = (trimmedQuery.match(/\}/g) || []).length;
  if (openBraces !== closeBraces) {
    issues.push({ type: 'error', message: `Unbalanced braces: ${openBraces} open, ${closeBraces} close` });
  }

  // Check for undefined prefixes (prefixes used but not declared)
  const declaredPrefixes = new Set();
  const prefixDeclarations = trimmedQuery.matchAll(/PREFIX\s+(\w+):/gi);
  for (const match of prefixDeclarations) {
    declaredPrefixes.add(match[1].toLowerCase());
  }

  // Add common prefixes that are always available
  const commonPrefixNames = Object.keys(COMMON_PREFIXES);
  for (const p of commonPrefixNames) {
    declaredPrefixes.add(p.toLowerCase());
  }

  // Find used prefixes
  const usedPrefixes = trimmedQuery.matchAll(/(?<![<\w])(\w+):/g);
  for (const match of usedPrefixes) {
    const prefix = match[1].toLowerCase();
    // Skip if it's a PREFIX declaration itself
    if (match.input.slice(Math.max(0, match.index - 7), match.index).includes('PREFIX')) {
      continue;
    }
    if (!declaredPrefixes.has(prefix)) {
      issues.push({ type: 'warning', message: `Prefix '${match[1]}' is used but not declared` });
    }
  }

  const hasErrors = issues.some(i => i.type === 'error');

  return {
    valid: !hasErrors,
    type,
    variables,
    issues,
  };
}

/**
 * Execute a parameterized SPARQL query
 * @param {Object} store - RDF store
 * @param {string} template - SPARQL query template with $param placeholders
 * @param {Object} params - Parameter values to substitute
 * @param {Object} prefixes - Additional prefix mappings
 * @param {Object} options - Query options
 * @returns {Promise<Array>} Query results as array of objects
 * @throws {SparqlExecutionError} If query execution fails
 */
export async function executeParameterizedQuery(store, template, params = {}, prefixes = {}, options = {}) {
  const query = substituteParameters(template, params);
  return executeSparqlQuery(store, query, prefixes, options);
}

/**
 * Extract line and column information from error message
 * @param {string} errorMsg - Error message
 * @param {number} prefixLineCount - Number of prefix declaration lines
 * @returns {Object} Object with line, column, userLine, queryLine, content
 */
function extractLineInfo(errorMsg, prefixLineCount = 0) {
  // Common patterns for line/column in SPARQL errors
  const patterns = [
    /line[:\s]+(\d+)(?:[,:\s]+column[:\s]+(\d+))?/i,
    /at line (\d+), column (\d+)/i,
    /\[line (\d+), col (\d+)\]/i,
    /position[:\s]+(\d+):(\d+)/i,
    /(\d+):(\d+):/,
  ];

  for (const pattern of patterns) {
    const match = errorMsg.match(pattern);
    if (match) {
      const queryLine = parseInt(match[1], 10);
      const column = match[2] ? parseInt(match[2], 10) : null;
      // Adjust line number to account for prefix declarations
      const userLine = Math.max(1, queryLine - prefixLineCount);

      return {
        line: queryLine,
        column,
        userLine,
        queryLine,
        content: null,
      };
    }
  }

  return { line: null, column: null, userLine: null, queryLine: null, content: null };
}

/**
 * Get syntax fix suggestions based on error message
 * @param {string} errorMsg - Error message
 * @returns {Array<string>} Array of suggestion strings
 */
function getSyntaxSuggestions(errorMsg) {
  const suggestions = [];
  const msg = errorMsg.toLowerCase();

  if (msg.includes('prefix') || msg.includes('namespace')) {
    suggestions.push('Check that all prefixes are declared (e.g., PREFIX foaf: <http://xmlns.com/foaf/0.1/>)');
  }

  if (msg.includes('unexpected') || msg.includes('expected')) {
    suggestions.push('Check for missing or extra punctuation (., ;, ,)');
    suggestions.push('Verify all brackets/braces are balanced');
  }

  if (msg.includes('variable') || msg.includes('?')) {
    suggestions.push('Ensure all variables start with ? or $ (e.g., ?subject)');
  }

  if (msg.includes('uri') || msg.includes('iri')) {
    suggestions.push('Check URI syntax - should be <http://...> or use prefixed names');
  }

  if (msg.includes('literal')) {
    suggestions.push('Check string literals are properly quoted and escaped');
  }

  if (msg.includes('token')) {
    suggestions.push('Look for typos in SPARQL keywords (SELECT, WHERE, FILTER, etc.)');
  }

  // Generic suggestions if no specific ones matched
  if (suggestions.length === 0) {
    suggestions.push('Validate query syntax using a SPARQL validator');
    suggestions.push('Check for typos in keywords and variable names');
  }

  return suggestions;
}

export default {
  executeSparqlQuery,
  executeParameterizedQuery,
  buildPrefixDeclarations,
  transformResults,
  substituteParameters,
  validateSparqlQuery,
  SparqlExecutionError,
};

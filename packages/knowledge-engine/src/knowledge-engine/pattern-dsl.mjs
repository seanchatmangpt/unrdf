/**
 * @file Pattern DSL - Simple pattern parsing
 * @module @unrdf/knowledge-engine/pattern-dsl
 */

/**
 * Parse a simple pattern string
 * Format: "?subject predicate ?object"
 *
 * @param {string} patternString - Pattern string to parse
 * @returns {Object} Parsed pattern object
 *
 * @example
 * const pattern = parsePattern("?person rdf:type foaf:Person");
 * // Returns: { subject: '?person', predicate: 'rdf:type', object: 'foaf:Person' }
 */
export function parsePattern(patternString) {
  const trimmed = patternString.trim();
  const parts = trimmed.split(/\s+/);

  if (parts.length < 3) {
    throw new Error(
      `Invalid pattern: "${patternString}". Expected format: "subject predicate object"`
    );
  }

  return {
    subject: parts[0],
    predicate: parts[1],
    object: parts.slice(2).join(' '),
  };
}

/**
 * Convert a pattern to SPARQL WHERE clause
 *
 * @param {Object} pattern - Pattern object
 * @returns {string} SPARQL pattern string
 *
 * @example
 * const sparql = patternToSparql({ subject: '?x', predicate: 'rdf:type', object: 'foaf:Person' });
 * // Returns: "?x rdf:type foaf:Person ."
 */
export function patternToSparql(pattern) {
  const subject = formatTerm(pattern.subject);
  const predicate = formatTerm(pattern.predicate);
  const object = formatTerm(pattern.object);

  return `${subject} ${predicate} ${object} .`;
}

/**
 * Format a term for SPARQL
 *
 * @param {string|Object} term - Term to format
 * @returns {string} Formatted term
 */
function formatTerm(term) {
  if (!term) return '?_';

  if (typeof term === 'object' && term.value) {
    return formatTerm(term.value);
  }

  const str = String(term);

  if (str.startsWith('?')) {
    return str;
  }

  if (str.startsWith('http://') || str.startsWith('https://')) {
    return `<${str}>`;
  }

  if (str.includes(':')) {
    return str;
  }

  return `"${str}"`;
}

/**
 * Parse multiple patterns from a multi-line string
 *
 * @param {string} patternsString - Multi-line pattern string
 * @returns {Object[]} Array of parsed patterns
 *
 * @example
 * const patterns = parsePatterns(`
 *   ?x rdf:type foaf:Person
 *   ?x foaf:name ?name
 * `);
 */
export function parsePatterns(patternsString) {
  const lines = patternsString
    .split('\n')
    .map(line => line.trim())
    .filter(line => line && !line.startsWith('#'));

  return lines.map(parsePattern);
}

/**
 * Build a pattern object from components
 *
 * @param {string} subject - Subject term
 * @param {string} predicate - Predicate term
 * @param {string} object - Object term
 * @param {string} [graph] - Optional graph term
 * @returns {Object} Pattern object
 *
 * @example
 * const pattern = buildPattern('?x', 'rdf:type', 'foaf:Person');
 */
export function buildPattern(subject, predicate, object, graph = null) {
  const pattern = { subject, predicate, object };

  if (graph) {
    pattern.graph = graph;
  }

  return pattern;
}

/**
 * Validate a pattern object
 *
 * @param {Object} pattern - Pattern to validate
 * @returns {boolean} True if valid
 *
 * @example
 * if (isValidPattern(pattern)) {
 *   console.log('Pattern is valid');
 * }
 */
export function isValidPattern(pattern) {
  if (!pattern || typeof pattern !== 'object') {
    return false;
  }

  return Boolean(pattern.subject && pattern.predicate && pattern.object);
}

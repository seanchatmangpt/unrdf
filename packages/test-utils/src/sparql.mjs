/**
 * @file Common SPARQL query fixtures and execution helpers.
 *
 * Eliminates repeated query string literals across 60+ test files.
 *
 * @module @unrdf/test-utils/sparql
 */

// ============================================================================
// Common SPARQL query strings
// ============================================================================

/** Pre-built SPARQL queries for common test scenarios */
export const SPARQL = {
  selectAll: 'SELECT * WHERE { ?s ?p ?o }',
  selectAllLimit10: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10',
  selectAllLimit1: 'SELECT * WHERE { ?s ?p ?o } LIMIT 1',
  countAll: 'SELECT (COUNT(*) AS ?count) WHERE { ?s ?p ?o }',
  askAny: 'ASK { ?s ?p ?o }',
  askEmpty: 'ASK { <http://example.org/nonexistent> ?p ?o }',
  constructAll: 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }',
  deleteAll: 'DELETE { ?s ?p ?o } WHERE { ?s ?p ?o }',

  /** Select all triples with a specific subject */
  selectBySubject: (subjectIRI) =>
    `SELECT * WHERE { <${subjectIRI}> ?p ?o }`,

  /** Select all instances of a class */
  selectByType: (classIRI) =>
    `SELECT * WHERE { ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <${classIRI}> }`,

  /** Ask if a subject has a specific type */
  askType: (subjectIRI, classIRI) =>
    `ASK { <${subjectIRI}> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <${classIRI}> }`,

  /** Select with a specific predicate */
  selectByPredicate: (predicateIRI) =>
    `SELECT * WHERE { ?s <${predicateIRI}> ?o }`,

  /** Invalid SPARQL — useful for error handling tests */
  invalid: 'THIS IS NOT VALID SPARQL !!!',
};

// ============================================================================
// Result helpers
// ============================================================================

/**
 * Extract binding values from a SPARQL SELECT result array.
 *
 * @param {object[]} results - Array of binding objects from store.query()
 * @param {string} variable - Variable name (without '?')
 * @returns {string[]} Array of values for the variable
 *
 * @example
 * const results = store.query(SPARQL.selectAll);
 * const subjects = getBindingValues(results, 's');
 */
export function getBindingValues(results, variable) {
  return results.map(row => row[variable]?.value ?? row.get?.(variable)?.value).filter(Boolean);
}

/**
 * Count results from a SPARQL query.
 *
 * @param {object[]|boolean} results - Query results (SELECT returns array, ASK returns boolean)
 * @returns {number}
 */
export function countResults(results) {
  if (typeof results === 'boolean') return results ? 1 : 0;
  if (Array.isArray(results)) return results.length;
  // Iterator/iterable
  let count = 0;
  for (const _ of results) count++;
  return count;
}

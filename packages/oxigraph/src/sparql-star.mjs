/**
 * @file SPARQL-star Query Utilities
 * @module @unrdf/oxigraph/sparql-star
 * @description SPARQL-star query support for RDF-star annotations
 */

/**
 * SPARQL-star Query Builder
 *
 * @example
 * const builder = new SPARQLStarQueryBuilder()
 *   .select(['?s', '?p', '?o', '?confidence'])
 *   .whereQuoted('?s', '?p', '?o')
 *   .whereAnnotation('?confidence', 'confidence')
 *   .filter('?confidence > 0.9')
 *   .build();
 */
export class SPARQLStarQueryBuilder {
  /**
   * Create a new SPARQL-star query builder
   */
  constructor() {
    this.prefixes = new Map();
    this.selectVars = [];
    this.wherePatterns = [];
    this.filterConditions = [];
    this.orderByVars = [];
    this.limitValue = null;
    this.offsetValue = null;

    this.addPrefix('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    this.addPrefix('rdfs', 'http://www.w3.org/2000/01/rdf-schema#');
    this.addPrefix('xsd', 'http://www.w3.org/2001/XMLSchema#');
    this.addPrefix('rdfstar', 'http://www.w3.org/ns/rdf-star#');
  }

  /**
   * Add a prefix
   * @param {string} prefix - Prefix name
   * @param {string} iri - IRI for the prefix
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  addPrefix(prefix, iri) {
    this.prefixes.set(prefix, iri);
    return this;
  }

  /**
   * Set SELECT variables
   * @param {Array<string>} vars - Variable names (with or without '?')
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  select(vars) {
    this.selectVars = vars.map((v) => (v.startsWith('?') ? v : `?${v}`));
    return this;
  }

  /**
   * Add a quoted triple pattern (RDF-star)
   * @param {string} subject - Subject variable or IRI
   * @param {string} predicate - Predicate variable or IRI
   * @param {string} object - Object variable or IRI
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  whereQuoted(subject, predicate, object) {
    this.wherePatterns.push(`<<${subject} ${predicate} ${object}>>`);
    return this;
  }

  /**
   * Add a regular triple pattern
   * @param {string} subject - Subject
   * @param {string} predicate - Predicate
   * @param {string} object - Object
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  where(subject, predicate, object) {
    this.wherePatterns.push(`${subject} ${predicate} ${object} .`);
    return this;
  }

  /**
   * Add an annotation pattern for a quoted triple
   * @param {string} variable - Variable to bind annotation value
   * @param {string} annotationType - Type of annotation (confidence, source, etc.)
   * @param {string} [quotedPattern] - Optional quoted triple pattern to annotate
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  whereAnnotation(variable, annotationType, quotedPattern) {
    const varName = variable.startsWith('?') ? variable : `?${variable}`;
    const pattern = quotedPattern || this.wherePatterns[this.wherePatterns.length - 1];

    if (!pattern) {
      throw new Error('No quoted triple pattern found. Call whereQuoted() first.');
    }

    this.wherePatterns.push(`${pattern} rdfstar:${annotationType} ${varName} .`);
    return this;
  }

  /**
   * Add a FILTER condition
   * @param {string} condition - Filter condition
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  filter(condition) {
    this.filterConditions.push(condition);
    return this;
  }

  /**
   * Filter by confidence threshold
   * @param {number} threshold - Minimum confidence (0-1)
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  confidenceThreshold(threshold) {
    this.wherePatterns.push(`?_qt rdfstar:confidence ?_confidence .`);
    this.filter(`?_confidence >= ${threshold}`);
    return this;
  }

  /**
   * Filter by temporal validity
   * @param {string} timestamp - ISO 8601 timestamp
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  temporalFilter(timestamp) {
    this.wherePatterns.push(
      `OPTIONAL { ?_qt rdfstar:validFrom ?_validFrom }`,
      `OPTIONAL { ?_qt rdfstar:validTo ?_validTo }`
    );
    this.filter(
      `(!BOUND(?_validFrom) || ?_validFrom <= "${timestamp}"^^xsd:dateTime) && (!BOUND(?_validTo) || ?_validTo >= "${timestamp}"^^xsd:dateTime)`
    );
    return this;
  }

  /**
   * Add ORDER BY clause
   * @param {string} variable - Variable to order by
   * @param {string} [direction='ASC'] - Direction (ASC or DESC)
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  orderBy(variable, direction = 'ASC') {
    const varName = variable.startsWith('?') ? variable : `?${variable}`;
    this.orderByVars.push(`${direction}(${varName})`);
    return this;
  }

  /**
   * Set LIMIT clause
   * @param {number} limit - Maximum number of results
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  limit(limit) {
    this.limitValue = limit;
    return this;
  }

  /**
   * Set OFFSET clause
   * @param {number} offset - Number of results to skip
   * @returns {SPARQLStarQueryBuilder} This instance for chaining
   */
  offset(offset) {
    this.offsetValue = offset;
    return this;
  }

  /**
   * Build the complete SPARQL query
   * @returns {string} Complete SPARQL-star query
   */
  build() {
    const parts = [];

    for (const [prefix, iri] of this.prefixes) {
      parts.push(`PREFIX ${prefix}: <${iri}>`);
    }

    parts.push('');

    parts.push(`SELECT ${this.selectVars.join(' ')}`);
    parts.push('WHERE {');

    for (const pattern of this.wherePatterns) {
      parts.push(`  ${pattern}`);
    }

    for (const condition of this.filterConditions) {
      parts.push(`  FILTER(${condition})`);
    }

    parts.push('}');

    if (this.orderByVars.length > 0) {
      parts.push(`ORDER BY ${this.orderByVars.join(' ')}`);
    }

    if (this.limitValue !== null) {
      parts.push(`LIMIT ${this.limitValue}`);
    }

    if (this.offsetValue !== null) {
      parts.push(`OFFSET ${this.offsetValue}`);
    }

    return parts.join('\n');
  }
}

/**
 * Execute a SPARQL-star query
 * @param {Object} store - Oxigraph store
 * @param {string} query - SPARQL-star query
 * @param {Object} [options] - Query options
 * @returns {Array|boolean} Query results
 */
export function executeSPARQLStar(store, query, options = {}) {
  if (!store || typeof store.query !== 'function') {
    throw new Error('Invalid store: must have a query method');
  }

  if (!query || typeof query !== 'string') {
    throw new Error('Invalid query: must be a non-empty string');
  }

  try {
    return store.query(query, options);
  } catch (error) {
    throw new Error(`SPARQL-star query failed: ${error.message}`);
  }
}

/**
 * Query for annotated triples with confidence above threshold
 * @param {Object} store - Oxigraph store
 * @param {number} threshold - Minimum confidence (0-1)
 * @param {Object} [options] - Additional options
 * @returns {Array} Query results
 */
export function queryByConfidence(store, threshold, options = {}) {
  const query = new SPARQLStarQueryBuilder()
    .select(['?s', '?p', '?o', '?confidence'])
    .where('?s', '?p', '?o')
    .whereQuoted('?s', '?p', '?o')
    .whereAnnotation('?confidence', 'confidence')
    .filter(`?confidence >= ${threshold}`)
    .orderBy('?confidence', 'DESC')
    .build();

  return executeSPARQLStar(store, query, options);
}

/**
 * Query for triples from a specific source
 * @param {Object} store - Oxigraph store
 * @param {string} sourceIRI - Source IRI
 * @param {Object} [options] - Additional options
 * @returns {Array} Query results
 */
export function queryBySource(store, sourceIRI, options = {}) {
  const query = new SPARQLStarQueryBuilder()
    .select(['?s', '?p', '?o'])
    .where('?s', '?p', '?o')
    .whereQuoted('?s', '?p', '?o')
    .whereAnnotation('?source', 'source')
    .filter(`?source = <${sourceIRI}>`)
    .build();

  return executeSPARQLStar(store, query, options);
}

/**
 * Query for temporally valid triples at a specific time
 * @param {Object} store - Oxigraph store
 * @param {string} timestamp - ISO 8601 timestamp
 * @param {Object} [options] - Additional options
 * @returns {Array} Query results
 */
export function queryByTemporal(store, timestamp, options = {}) {
  const query = new SPARQLStarQueryBuilder()
    .select(['?s', '?p', '?o'])
    .where('?s', '?p', '?o')
    .temporalFilter(timestamp)
    .build();

  return executeSPARQLStar(store, query, options);
}

/**
 * Create a SPARQL-star query builder
 * @returns {SPARQLStarQueryBuilder} New query builder instance
 */
export function createSPARQLStarBuilder() {
  return new SPARQLStarQueryBuilder();
}

export default {
  SPARQLStarQueryBuilder,
  createSPARQLStarBuilder,
  executeSPARQLStar,
  queryByConfidence,
  queryBySource,
  queryByTemporal,
};

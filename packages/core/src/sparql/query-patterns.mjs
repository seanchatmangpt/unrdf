/**
 * @file SPARQL Query Patterns and Templates
 * @module @unrdf/core/sparql/query-patterns
 * @description
 * Common SPARQL query patterns, graph pattern templates, and property path helpers.
 */

import { z } from 'zod';
import { sparql } from './query-builder.mjs';

/**
 * Zod schema for property path options (reserved for future validation)
 */
const _PropertyPathOptionsSchema = z.object({
  minLength: z.number().int().nonnegative().optional(),
  maxLength: z.number().int().positive().optional(),
  inverse: z.boolean().optional(),
}).strict();

/**
 * Common RDF/RDFS/OWL prefixes
 */
export const COMMON_PREFIXES = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  owl: 'http://www.w3.org/2002/07/owl#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  foaf: 'http://xmlns.com/foaf/0.1/',
  dc: 'http://purl.org/dc/elements/1.1/',
  dcterms: 'http://purl.org/dc/terms/',
  skos: 'http://www.w3.org/2004/02/skos/core#',
  schema: 'http://schema.org/',
};

/**
 * Property path builder for complex property expressions
 */
export class PropertyPath {
  /**
   * @param {string} predicate - Starting predicate
   */
  constructor(predicate) {
    z.string().min(1).parse(predicate);
    this._path = predicate;
  }

  /**
   * Add a sequence path (/)
   * @param {string} predicate - Next predicate in sequence
   * @returns {PropertyPath} This path for chaining
   * @example
   * path('foaf:knows').sequence('foaf:name')
   * // Result: foaf:knows/foaf:name
   */
  sequence(predicate) {
    z.string().min(1).parse(predicate);
    this._path = `${this._path}/${predicate}`;
    return this;
  }

  /**
   * Add an alternative path (|)
   * @param {string} predicate - Alternative predicate
   * @returns {PropertyPath} This path for chaining
   * @example
   * path('foaf:name').alternative('rdfs:label')
   * // Result: foaf:name|rdfs:label
   */
  alternative(predicate) {
    z.string().min(1).parse(predicate);
    this._path = `(${this._path}|${predicate})`;
    return this;
  }

  /**
   * Add zero or more (*) quantifier
   * @returns {PropertyPath} This path for chaining
   * @example
   * path('foaf:knows').zeroOrMore()
   * // Result: foaf:knows*
   */
  zeroOrMore() {
    this._path = `${this._path}*`;
    return this;
  }

  /**
   * Add one or more (+) quantifier
   * @returns {PropertyPath} This path for chaining
   * @example
   * path('foaf:knows').oneOrMore()
   * // Result: foaf:knows+
   */
  oneOrMore() {
    this._path = `${this._path}+`;
    return this;
  }

  /**
   * Add zero or one (?) quantifier
   * @returns {PropertyPath} This path for chaining
   * @example
   * path('foaf:knows').zeroOrOne()
   * // Result: foaf:knows?
   */
  zeroOrOne() {
    this._path = `${this._path}?`;
    return this;
  }

  /**
   * Add inverse path (^)
   * @returns {PropertyPath} This path for chaining
   * @example
   * path('foaf:knows').inverse()
   * // Result: ^foaf:knows
   */
  inverse() {
    this._path = `^${this._path}`;
    return this;
  }

  /**
   * Get the path string
   * @returns {string} The property path
   */
  toString() {
    return this._path;
  }
}

/**
 * Create a property path
 * @param {string} predicate - Starting predicate
 * @returns {PropertyPath} New property path builder
 * @example
 * const p = path('foaf:knows').oneOrMore().sequence('foaf:name');
 * console.log(p.toString()); // "foaf:knows+/foaf:name"
 */
export function path(predicate) {
  return new PropertyPath(predicate);
}

/**
 * Create a query to find all instances of a type
 * @param {string} type - RDF type URI or prefixed name
 * @param {Object} [options] - Query options
 * @param {Array<string>} [options.properties=[]] - Properties to select
 * @param {number} [options.limit] - Result limit
 * @returns {string} SPARQL SELECT query
 * @example
 * findInstancesOfType('foaf:Person', { properties: ['foaf:name', 'foaf:email'], limit: 10 });
 */
export function findInstancesOfType(type, options = {}) {
  z.string().min(1).parse(type);

  const builder = sparql()
    .select('?instance');

  if (options.properties && options.properties.length > 0) {
    options.properties.forEach((prop, i) => {
      const varName = `?prop${i}`;
      builder._variables.push(varName);
    });
  }

  builder.where(`?instance rdf:type ${type}`);

  if (options.properties) {
    options.properties.forEach((prop, i) => {
      const varName = `?prop${i}`;
      builder.optional(`?instance ${prop} ${varName}`);
    });
  }

  if (options.limit) {
    builder.limit(options.limit);
  }

  return builder.build();
}

/**
 * Create a query to find all properties of a resource
 * @param {string} resource - Resource URI or variable
 * @param {Object} [options] - Query options
 * @param {boolean} [options.includeInverse=false] - Include inverse properties
 * @returns {string} SPARQL SELECT query
 * @example
 * findPropertiesOf('<http://example.org/person/alice>');
 */
export function findPropertiesOf(resource, options = {}) {
  z.string().min(1).parse(resource);

  const builder = sparql()
    .select('?property', '?value')
    .where(`${resource} ?property ?value`);

  if (options.includeInverse) {
    builder.union(
      b => b.where(`${resource} ?property ?value`),
      b => b.where(`?value ?property ${resource}`)
    );
  }

  return builder.build();
}

/**
 * Create a query to find resources by property value
 * @param {string} property - Property URI or prefixed name
 * @param {string} value - Property value (use quotes for literals)
 * @param {Object} [options] - Query options
 * @param {number} [options.limit] - Result limit
 * @returns {string} SPARQL SELECT query
 * @example
 * findByProperty('foaf:name', '"Alice"');
 * findByProperty('foaf:age', '25', { limit: 10 });
 */
export function findByProperty(property, value, options = {}) {
  z.string().min(1).parse(property);
  z.string().min(1).parse(value);

  const builder = sparql()
    .select('?resource')
    .where(`?resource ${property} ${value}`);

  if (options.limit) {
    builder.limit(options.limit);
  }

  return builder.build();
}

/**
 * Create a query with full-text search using REGEX
 * @param {string} property - Property to search in
 * @param {string} searchTerm - Search term (will be escaped)
 * @param {Object} [options] - Query options
 * @param {boolean} [options.caseInsensitive=true] - Case insensitive search
 * @param {number} [options.limit] - Result limit
 * @returns {string} SPARQL SELECT query
 * @example
 * textSearch('foaf:name', 'alice', { caseInsensitive: true, limit: 10 });
 */
export function textSearch(property, searchTerm, options = {}) {
  z.string().min(1).parse(property);
  z.string().min(1).parse(searchTerm);

  const builder = sparql()
    .select('?resource', '?value')
    .where(`?resource ${property} ?value`);

  const flags = options.caseInsensitive !== false ? ', "i"' : '';
  builder.filter(`REGEX(?value, "${searchTerm}"${flags})`);

  if (options.limit) {
    builder.limit(options.limit);
  }

  return builder.build();
}

/**
 * Create a query to get subclass hierarchy
 * @param {string} baseClass - Base class URI or prefixed name
 * @param {Object} [options] - Query options
 * @param {number} [options.maxDepth] - Maximum depth (uses property path)
 * @returns {string} SPARQL SELECT query
 * @example
 * getSubclasses('owl:Thing');
 * getSubclasses('schema:Person', { maxDepth: 3 });
 */
export function getSubclasses(baseClass, options = {}) {
  z.string().min(1).parse(baseClass);

  let pathExpr = 'rdfs:subClassOf';
  if (options.maxDepth) {
    pathExpr = `rdfs:subClassOf{1,${options.maxDepth}}`;
  } else {
    pathExpr = 'rdfs:subClassOf+';
  }

  return sparql()
    .select('?subclass')
    .where(`?subclass ${pathExpr} ${baseClass}`)
    .build();
}

/**
 * Create a query to count instances by type
 * @param {string} type - RDF type URI or prefixed name
 * @returns {string} SPARQL SELECT query with COUNT
 * @example
 * countInstancesByType('foaf:Person');
 */
export function countInstancesByType(type) {
  z.string().min(1).parse(type);

  return sparql()
    .select('(COUNT(?instance) AS ?count)')
    .where(`?instance rdf:type ${type}`)
    .build();
}

/**
 * Create a DESCRIBE query for a resource
 * @param {string} resource - Resource URI or variable
 * @returns {string} SPARQL DESCRIBE query
 * @example
 * describeResource('<http://example.org/person/alice>');
 */
export function describeResource(resource) {
  z.string().min(1).parse(resource);

  return `DESCRIBE ${resource}`;
}

/**
 * Create a query to find connected resources (graph traversal)
 * @param {string} startResource - Starting resource URI
 * @param {Object} options - Traversal options
 * @param {string} options.via - Property path for traversal
 * @param {number} [options.maxDepth=3] - Maximum traversal depth
 * @returns {string} SPARQL SELECT query
 * @example
 * findConnectedResources('<http://example.org/alice>', {
 *   via: 'foaf:knows',
 *   maxDepth: 2
 * });
 */
export function findConnectedResources(startResource, options) {
  z.string().min(1).parse(startResource);
  z.object({
    via: z.string().min(1),
    maxDepth: z.number().int().positive().optional(),
  }).parse(options);

  const maxDepth = options.maxDepth || 3;
  const pathExpr = `${options.via}{1,${maxDepth}}`;

  return sparql()
    .select('?connected')
    .where(`${startResource} ${pathExpr} ?connected`)
    .build();
}

/**
 * Create a query with common prefixes already included
 * @param {Function} builderFn - Function that receives a builder with prefixes
 * @returns {string} SPARQL query string
 * @example
 * withCommonPrefixes(b =>
 *   b.select('?name')
 *    .where('?s foaf:name ?name')
 * );
 */
export function withCommonPrefixes(builderFn) {
  const builder = sparql();

  for (const [prefix, uri] of Object.entries(COMMON_PREFIXES)) {
    builder.prefix(prefix, uri);
  }

  builderFn(builder);

  return builder.build();
}

/**
 * Create a federated query across multiple endpoints
 * @param {Array<Object>} services - Array of service configurations
 * @param {Function} builderFn - Function that receives builder
 * @returns {string} SPARQL federated query
 * @example
 * federatedQuery([
 *   { endpoint: 'http://dbpedia.org/sparql', patterns: ['?s rdfs:label ?label'] }
 * ], b => b.select('?s', '?label'));
 */
export function federatedQuery(services, builderFn) {
  z.array(z.object({
    endpoint: z.string().url(),
    patterns: z.array(z.string()),
  })).parse(services);

  const builder = sparql();
  builderFn(builder);

  services.forEach(({ endpoint, patterns }) => {
    builder.graph(`SERVICE <${endpoint}>`, b => {
      patterns.forEach(p => b.where(p));
    });
  });

  return builder.build();
}

/**
 * Create an aggregation query
 * @param {Object} options - Aggregation options
 * @param {string} options.groupBy - Variable to group by
 * @param {Array<Object>} options.aggregations - Aggregation expressions
 * @param {string} options.wherePattern - WHERE clause pattern
 * @returns {string} SPARQL SELECT query with aggregation
 * @example
 * aggregateQuery({
 *   groupBy: '?person',
 *   aggregations: [
 *     { fn: 'COUNT', var: '?friend', as: '?friendCount' },
 *     { fn: 'AVG', var: '?age', as: '?avgAge' }
 *   ],
 *   wherePattern: '?person foaf:knows ?friend'
 * });
 */
export function aggregateQuery(options) {
  const schema = z.object({
    groupBy: z.string().min(1),
    aggregations: z.array(z.object({
      fn: z.string().min(1),
      var: z.string().min(1),
      as: z.string().min(1),
    })),
    wherePattern: z.string().min(1),
  });

  const validated = schema.parse(options);

  const aggVars = validated.aggregations.map(
    agg => `(${agg.fn}(${agg.var}) AS ${agg.as})`
  );

  const builder = sparql()
    .select(validated.groupBy, ...aggVars)
    .where(validated.wherePattern)
    .groupBy(validated.groupBy);

  return builder.build();
}

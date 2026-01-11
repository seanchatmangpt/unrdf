/**
 * @file Fluent SPARQL Query Builder
 * @module @unrdf/core/sparql/query-builder
 * @description
 * Provides a type-safe fluent API for constructing SPARQL queries.
 * Supports SELECT, INSERT, DELETE, CONSTRUCT operations with validation.
 */

import { z } from 'zod';
import {
  PrefixSchema,
  TriplePatternSchema,
  FilterSchema,
  QueryBuilderOptionsSchema,
} from './query-builder.schema.mjs';

export { QueryBuilderOptionsSchema };

/**
 * SPARQL Query Builder - Fluent API for constructing SPARQL queries
 *
 * @class
 * @example
 * const query = sparql()
 *   .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
 *   .select('?name', '?email')
 *   .where('?person foaf:name ?name')
 *   .where('?person foaf:mbox ?email')
 *   .filter('REGEX(?email, "@example.com")')
 *   .limit(10)
 *   .build();
 */
class QueryBuilder {
  /**
   * @param {Object} [options={}] - Query builder options
   * @param {Object} [options.prefixes={}] - Initial prefixes
   * @param {string} [options.baseIRI] - Base IRI for relative URIs
   */
  constructor(options = {}) {
    const validated = QueryBuilderOptionsSchema.parse(options);

    this._queryType = null;
    this._prefixes = new Map(Object.entries(validated.prefixes || {}));
    this._baseIRI = validated.baseIRI || null;
    this._variables = [];
    this._wherePatterns = [];
    this._optionalPatterns = [];
    this._unionPatterns = [];
    this._filters = [];
    this._graphPatterns = new Map();
    this._limit = null;
    this._offset = null;
    this._orderBy = [];
    this._groupBy = [];
    this._having = [];
    this._distinct = false;
    this._reduced = false;
    this._insertPatterns = [];
    this._deletePatterns = [];
    this._constructPatterns = [];
  }

  /**
   * Add a prefix definition
   * @param {string} prefix - Prefix name (without colon)
   * @param {string} uri - Full URI
   * @returns {QueryBuilder} This builder for chaining
   * @throws {z.ZodError} If prefix or uri is invalid
   * @example
   * builder.prefix('foaf', 'http://xmlns.com/foaf/0.1/');
   */
  prefix(prefix, uri) {
    PrefixSchema.parse({ prefix, uri });
    this._prefixes.set(prefix, uri);
    return this;
  }

  /**
   * Set base IRI
   * @param {string} iri - Base IRI
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.base('http://example.org/');
   */
  base(iri) {
    z.string().url().parse(iri);
    this._baseIRI = iri;
    return this;
  }

  /**
   * Start a SELECT query
   * @param {...string} variables - Variable names (with or without ?)
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.select('?name', '?email');
   * builder.select('*'); // Select all variables
   */
  select(...variables) {
    this._queryType = 'SELECT';
    this._variables = variables.map(v => v.startsWith('?') ? v : `?${v}`);
    return this;
  }

  /**
   * Mark query as DISTINCT
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.select('?name').distinct();
   */
  distinct() {
    this._distinct = true;
    return this;
  }

  /**
   * Mark query as REDUCED
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.select('?name').reduced();
   */
  reduced() {
    this._reduced = true;
    return this;
  }

  /**
   * Start a CONSTRUCT query
   * @param {...string} patterns - Triple patterns for construction
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.construct('?s foaf:name ?name');
   */
  construct(...patterns) {
    this._queryType = 'CONSTRUCT';
    this._constructPatterns.push(...patterns);
    return this;
  }

  /**
   * Start an ASK query
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.ask().where('?s foaf:name "Alice"');
   */
  ask() {
    this._queryType = 'ASK';
    return this;
  }

  /**
   * Start an INSERT query
   * @param {...string} patterns - Triple patterns to insert
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.insert('?s foaf:name "Alice"');
   */
  insert(...patterns) {
    this._queryType = 'INSERT';
    this._insertPatterns.push(...patterns);
    return this;
  }

  /**
   * Start a DELETE query
   * @param {...string} patterns - Triple patterns to delete
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.delete('?s foaf:name ?oldName');
   */
  delete(...patterns) {
    this._queryType = 'DELETE';
    this._deletePatterns.push(...patterns);
    return this;
  }

  /**
   * Add a WHERE clause pattern
   * @param {string|Object} pattern - Triple pattern or object
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.where('?person foaf:name ?name');
   * builder.where({ subject: '?s', predicate: 'rdf:type', object: 'foaf:Person' });
   */
  where(pattern) {
    if (typeof pattern === 'string') {
      this._wherePatterns.push(pattern);
    } else {
      const validated = TriplePatternSchema.parse(pattern);
      this._wherePatterns.push(`${validated.subject} ${validated.predicate} ${validated.object}`);
    }
    return this;
  }

  /**
   * Add an OPTIONAL clause pattern
   * @param {string|Function} pattern - Pattern or function that takes builder
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.optional('?person foaf:mbox ?email');
   * builder.optional(b => b.where('?person foaf:phone ?phone'));
   */
  optional(pattern) {
    if (typeof pattern === 'function') {
      const subBuilder = new QueryBuilder();
      pattern(subBuilder);
      this._optionalPatterns.push(subBuilder._wherePatterns);
    } else {
      this._optionalPatterns.push([pattern]);
    }
    return this;
  }

  /**
   * Add a UNION clause
   * @param {...Function} patterns - Functions that take builder for each union branch
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.union(
   *   b => b.where('?s foaf:name ?name'),
   *   b => b.where('?s rdfs:label ?name')
   * );
   */
  union(...patterns) {
    const branches = patterns.map(fn => {
      const subBuilder = new QueryBuilder();
      fn(subBuilder);
      return subBuilder._wherePatterns;
    });
    this._unionPatterns.push(branches);
    return this;
  }

  /**
   * Add a FILTER clause
   * @param {string} expression - Filter expression
   * @returns {QueryBuilder} This builder for chaining
   * @throws {z.ZodError} If expression is invalid
   * @example
   * builder.filter('?age > 18');
   * builder.filter('REGEX(?email, "@example.com")');
   */
  filter(expression) {
    FilterSchema.parse(expression);
    this._filters.push(expression);
    return this;
  }

  /**
   * Add a named graph pattern
   * @param {string} graph - Graph URI or variable
   * @param {Function} pattern - Function that takes builder
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.graph('http://example.org/graph', b => {
   *   b.where('?s foaf:name ?name');
   * });
   */
  graph(graph, pattern) {
    z.string().min(1).parse(graph);
    const subBuilder = new QueryBuilder();
    pattern(subBuilder);
    this._graphPatterns.set(graph, subBuilder._wherePatterns);
    return this;
  }

  /**
   * Set LIMIT
   * @param {number} limit - Maximum number of results
   * @returns {QueryBuilder} This builder for chaining
   * @throws {z.ZodError} If limit is not a positive integer
   * @example
   * builder.limit(10);
   */
  limit(limit) {
    z.number().int().positive().parse(limit);
    this._limit = limit;
    return this;
  }

  /**
   * Set OFFSET
   * @param {number} offset - Number of results to skip
   * @returns {QueryBuilder} This builder for chaining
   * @throws {z.ZodError} If offset is not a non-negative integer
   * @example
   * builder.offset(20);
   */
  offset(offset) {
    z.number().int().nonnegative().parse(offset);
    this._offset = offset;
    return this;
  }

  /**
   * Add ORDER BY clause
   * @param {...string} variables - Variables to order by (prefix with - for DESC)
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.orderBy('?name');        // ASC
   * builder.orderBy('-?age');        // DESC
   * builder.orderBy('?name', '-?age');
   */
  orderBy(...variables) {
    this._orderBy.push(...variables);
    return this;
  }

  /**
   * Add GROUP BY clause
   * @param {...string} variables - Variables to group by
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.groupBy('?person');
   */
  groupBy(...variables) {
    this._groupBy.push(...variables);
    return this;
  }

  /**
   * Add HAVING clause
   * @param {string} condition - Having condition
   * @returns {QueryBuilder} This builder for chaining
   * @example
   * builder.having('COUNT(?item) > 5');
   */
  having(condition) {
    z.string().min(1).parse(condition);
    this._having.push(condition);
    return this;
  }

  /**
   * Build the complete SPARQL query string
   * @returns {string} Complete SPARQL query
   * @throws {Error} If query type is not set
   * @example
   * const queryString = builder.build();
   */
  build() {
    if (!this._queryType) {
      throw new Error('Query type must be set (select, construct, ask, insert, or delete)');
    }

    const parts = [];

    if (this._baseIRI) {
      parts.push(`BASE <${this._baseIRI}>`);
    }

    for (const [prefix, uri] of this._prefixes.entries()) {
      parts.push(`PREFIX ${prefix}: <${uri}>`);
    }

    switch (this._queryType) {
      case 'SELECT': {
        let selectClause = 'SELECT';
        if (this._distinct) selectClause += ' DISTINCT';
        if (this._reduced) selectClause += ' REDUCED';
        selectClause += ` ${this._variables.join(' ')}`;
        parts.push(selectClause);
        break;
      }
      case 'CONSTRUCT': {
        parts.push('CONSTRUCT {');
        this._constructPatterns.forEach(p => parts.push(`  ${p} .`));
        parts.push('}');
        break;
      }
      case 'ASK': {
        parts.push('ASK');
        break;
      }
      case 'INSERT': {
        parts.push('INSERT DATA {');
        this._insertPatterns.forEach(p => parts.push(`  ${p} .`));
        parts.push('}');
        return parts.join('\n');
      }
      case 'DELETE': {
        parts.push('DELETE DATA {');
        this._deletePatterns.forEach(p => parts.push(`  ${p} .`));
        parts.push('}');
        return parts.join('\n');
      }
    }

    if (this._wherePatterns.length > 0 ||
        this._optionalPatterns.length > 0 ||
        this._unionPatterns.length > 0 ||
        this._graphPatterns.size > 0) {
      parts.push('WHERE {');

      this._wherePatterns.forEach(p => {
        parts.push(`  ${p} .`);
      });

      for (const [graph, patterns] of this._graphPatterns.entries()) {
        parts.push(`  GRAPH ${graph} {`);
        patterns.forEach(p => parts.push(`    ${p} .`));
        parts.push('  }');
      }

      this._optionalPatterns.forEach(patterns => {
        parts.push('  OPTIONAL {');
        patterns.forEach(p => parts.push(`    ${p} .`));
        parts.push('  }');
      });

      this._unionPatterns.forEach(branches => {
        branches.forEach((patterns, index) => {
          if (index > 0) parts.push('  UNION');
          parts.push('  {');
          patterns.forEach(p => parts.push(`    ${p} .`));
          parts.push('  }');
        });
      });

      this._filters.forEach(f => {
        parts.push(`  FILTER(${f})`);
      });

      parts.push('}');
    }

    if (this._groupBy.length > 0) {
      parts.push(`GROUP BY ${this._groupBy.join(' ')}`);
    }

    if (this._having.length > 0) {
      parts.push(`HAVING (${this._having.join(' && ')})`);
    }

    if (this._orderBy.length > 0) {
      const orderClauses = this._orderBy.map(v => {
        if (v.startsWith('-')) {
          return `DESC(${v.substring(1)})`;
        }
        return v;
      });
      parts.push(`ORDER BY ${orderClauses.join(' ')}`);
    }

    if (this._limit !== null) {
      parts.push(`LIMIT ${this._limit}`);
    }

    if (this._offset !== null) {
      parts.push(`OFFSET ${this._offset}`);
    }

    return parts.join('\n');
  }

  /**
   * Build and return both the query string and metadata
   * @returns {Object} Object with query string and metadata
   * @returns {string} returns.query - The SPARQL query string
   * @returns {Object} returns.metadata - Query metadata
   * @example
   * const { query, metadata } = builder.buildWithMetadata();
   * console.log(metadata.type, metadata.variables);
   */
  buildWithMetadata() {
    return {
      query: this.build(),
      metadata: {
        type: this._queryType,
        variables: this._variables,
        prefixes: Object.fromEntries(this._prefixes),
        hasFilters: this._filters.length > 0,
        hasOptionals: this._optionalPatterns.length > 0,
        hasUnions: this._unionPatterns.length > 0,
        hasGraphs: this._graphPatterns.size > 0,
      },
    };
  }
}

/**
 * Create a new SPARQL query builder
 * @param {Object} [options] - Query builder options
 * @returns {QueryBuilder} New query builder instance
 * @example
 * const query = sparql()
 *   .prefix('foaf', 'http://xmlns.com/foaf/0.1/')
 *   .select('?name')
 *   .where('?s foaf:name ?name')
 *   .build();
 */
export function sparql(options) {
  return new QueryBuilder(options);
}

export { QueryBuilder };

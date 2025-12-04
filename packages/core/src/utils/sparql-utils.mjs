/**
 * @fileoverview SPARQL utilities - Query building and SPARQL operations
 *
 * These utilities provide SPARQL query building, query analysis,
 * and common SPARQL patterns for RDF operations.
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { generateSPARQLPrefixes } from './namespace-utils.mjs';

/**
 * SPARQL query builder class
 */
export class SPARQLBuilder {
  /**
   *
   */
  constructor() {
    this.prefixes = new Map();
    this.selectVars = [];
    this.whereClauses = [];
    this.filters = [];
    this.optionalClauses = [];
    this.groupByVars = [];
    this.orderByVars = [];
    this.limit = null;
    this.offset = null;
    this.distinct = false;
    this.queryType = 'SELECT';
  }

  /**
   * Add a prefix declaration
   * @param {string} prefix - The prefix
   * @param {string} namespace - The namespace URI
   * @returns {SPARQLBuilder} This builder instance
   */
  addPrefix(prefix, namespace) {
    this.prefixes.set(prefix, namespace);
    return this;
  }

  /**
   * Add multiple prefixes
   * @param {Object} prefixes - Object with prefix mappings
   * @returns {SPARQLBuilder} This builder instance
   */
  addPrefixes(prefixes) {
    for (const [prefix, namespace] of Object.entries(prefixes)) {
      this.addPrefix(prefix, namespace);
    }
    return this;
  }

  /**
   * Set query type
   * @param {string} type - Query type (SELECT, CONSTRUCT, ASK, DESCRIBE)
   * @returns {SPARQLBuilder} This builder instance
   */
  setType(type) {
    this.queryType = type.toUpperCase();
    return this;
  }

  /**
   * Add SELECT variables
   * @param {...string} vars - Variable names
   * @returns {SPARQLBuilder} This builder instance
   */
  select(...vars) {
    this.selectVars.push(...vars);
    return this;
  }

  /**
   * Add a WHERE clause (triple pattern)
   * @param {string} subject - Subject
   * @param {string} predicate - Predicate
   * @param {string} object - Object
   * @returns {SPARQLBuilder} This builder instance
   */
  where(subject, predicate, object) {
    this.whereClauses.push(`  ${subject} ${predicate} ${object} .`);
    return this;
  }

  /**
   * Add an OPTIONAL clause
   * @param {string} subject - Subject
   * @param {string} predicate - Predicate
   * @param {string} object - Object
   * @returns {SPARQLBuilder} This builder instance
   */
  optional(subject, predicate, object) {
    this.optionalClauses.push(`  OPTIONAL { ${subject} ${predicate} ${object} . }`);
    return this;
  }

  /**
   * Add a FILTER clause
   * @param {string} filter - Filter expression
   * @returns {SPARQLBuilder} This builder instance
   */
  filter(filter) {
    this.filters.push(`  FILTER(${filter})`);
    return this;
  }

  /**
   * Add GROUP BY clause
   * @param {...string} vars - Variables to group by
   * @returns {SPARQLBuilder} This builder instance
   */
  groupBy(...vars) {
    this.groupByVars.push(...vars);
    return this;
  }

  /**
   * Add ORDER BY clause
   * @param {string} expression - Order expression
   * @param {string} [direction='ASC'] - Order direction (ASC/DESC)
   * @returns {SPARQLBuilder} This builder instance
   */
  orderBy(expression, direction = 'ASC') {
    this.orderByVars.push(`${expression} ${direction.toUpperCase()}`);
    return this;
  }

  /**
   * Set LIMIT
   * @param {number} limit - Limit value
   * @returns {SPARQLBuilder} This builder instance
   */
  setLimit(limit) {
    this.limit = limit;
    return this;
  }

  /**
   * Set OFFSET
   * @param {number} offset - Offset value
   * @returns {SPARQLBuilder} This builder instance
   */
  setOffset(offset) {
    this.offset = offset;
    return this;
  }

  /**
   * Set DISTINCT
   * @param {boolean} distinct - Whether to use DISTINCT
   * @returns {SPARQLBuilder} This builder instance
   */
  setDistinct(distinct = true) {
    this.distinct = distinct;
    return this;
  }

  /**
   * Build the SPARQL query
   * @returns {string} The complete SPARQL query
   */
  build() {
    let query = '';

    // Add prefixes
    if (this.prefixes.size > 0) {
      const prefixObj = Object.fromEntries(this.prefixes);
      query += generateSPARQLPrefixes(prefixObj) + '\n';
    }

    // Add query type and variables
    switch (this.queryType) {
      case 'SELECT': {
        const distinctStr = this.distinct ? 'DISTINCT ' : '';
        const selectVars = this.selectVars.length > 0 ? this.selectVars.join(' ') : '*';
        query += `SELECT ${distinctStr}${selectVars}\n`;

        break;
      }
      case 'CONSTRUCT': {
        query += 'CONSTRUCT {\n';
        // Add construct template here if needed
        query += '}\n';

        break;
      }
      case 'ASK': {
        query += 'ASK\n';

        break;
      }
      case 'DESCRIBE': {
        const describeVars = this.selectVars.length > 0 ? this.selectVars.join(' ') : '*';
        query += `DESCRIBE ${describeVars}\n`;

        break;
      }
      // No default
    }

    // Add WHERE clause
    if (
      this.whereClauses.length > 0 ||
      this.optionalClauses.length > 0 ||
      this.filters.length > 0
    ) {
      query += 'WHERE {\n';
      query += this.whereClauses.join('\n') + '\n';
      query += this.optionalClauses.join('\n') + '\n';
      query += this.filters.join('\n') + '\n';
      query += '}\n';
    }

    // Add GROUP BY
    if (this.groupByVars.length > 0) {
      query += `GROUP BY ${this.groupByVars.join(' ')}\n`;
    }

    // Add ORDER BY
    if (this.orderByVars.length > 0) {
      query += `ORDER BY ${this.orderByVars.join(' ')}\n`;
    }

    // Add LIMIT
    if (this.limit !== null) {
      query += `LIMIT ${this.limit}\n`;
    }

    // Add OFFSET
    if (this.offset !== null) {
      query += `OFFSET ${this.offset}\n`;
    }

    return query.trim();
  }

  /**
   * Reset the builder
   * @returns {SPARQLBuilder} This builder instance
   */
  reset() {
    this.prefixes.clear();
    this.selectVars = [];
    this.whereClauses = [];
    this.filters = [];
    this.optionalClauses = [];
    this.groupByVars = [];
    this.orderByVars = [];
    this.limit = null;
    this.offset = null;
    this.distinct = false;
    this.queryType = 'SELECT';
    return this;
  }
}

/**
 * Create a new SPARQL query builder
 * @returns {SPARQLBuilder} New builder instance
 */
export const createSPARQLBuilder = () => new SPARQLBuilder();

/**
 * Build a simple SELECT query
 * @param {string[]} variables - Variables to select
 * @param {Object} patterns - Triple patterns
 * @param {Object} [options] - Query options
 * @returns {string} SPARQL query
 */
export const buildSelectQuery = (variables, patterns, options = {}) => {
  const builder = createSPARQLBuilder();

  if (options.prefixes) {
    builder.addPrefixes(options.prefixes);
  }

  builder.select(...variables);

  for (const [subject, predicates] of Object.entries(patterns)) {
    for (const [predicate, objects] of Object.entries(predicates)) {
      if (Array.isArray(objects)) {
        for (const object of objects) {
          builder.where(subject, predicate, object);
        }
      } else {
        builder.where(subject, predicate, objects);
      }
    }
  }

  if (options.filters) {
    for (const filter of options.filters) {
      builder.filter(filter);
    }
  }

  if (options.limit) {
    builder.setLimit(options.limit);
  }

  if (options.orderBy) {
    builder.orderBy(options.orderBy.expression, options.orderBy.direction);
  }

  return builder.build();
};

/**
 * Build a CONSTRUCT query
 * @param {Object} constructTemplate - Construct template patterns
 * @param {Object} wherePatterns - WHERE clause patterns
 * @param {Object} [options] - Query options
 * @returns {string} SPARQL query
 */
export const buildConstructQuery = (constructTemplate, wherePatterns, options = {}) => {
  const builder = createSPARQLBuilder();

  if (options.prefixes) {
    builder.addPrefixes(options.prefixes);
  }

  builder.setType('CONSTRUCT');

  // Add WHERE patterns
  for (const [subject, predicates] of Object.entries(wherePatterns)) {
    for (const [predicate, objects] of Object.entries(predicates)) {
      if (Array.isArray(objects)) {
        for (const object of objects) {
          builder.where(subject, predicate, object);
        }
      } else {
        builder.where(subject, predicate, objects);
      }
    }
  }

  if (options.filters) {
    for (const filter of options.filters) {
      builder.filter(filter);
    }
  }

  return builder.build();
};

/**
 * Build an ASK query
 * @param {Object} patterns - Triple patterns
 * @param {Object} [options] - Query options
 * @returns {string} SPARQL query
 */
export const buildAskQuery = (patterns, options = {}) => {
  const builder = createSPARQLBuilder();

  if (options.prefixes) {
    builder.addPrefixes(options.prefixes);
  }

  builder.setType('ASK');

  for (const [subject, predicates] of Object.entries(patterns)) {
    for (const [predicate, objects] of Object.entries(predicates)) {
      if (Array.isArray(objects)) {
        for (const object of objects) {
          builder.where(subject, predicate, object);
        }
      } else {
        builder.where(subject, predicate, objects);
      }
    }
  }

  if (options.filters) {
    for (const filter of options.filters) {
      builder.filter(filter);
    }
  }

  return builder.build();
};

/**
 * Common SPARQL query patterns
 */
export const COMMON_PATTERNS = {
  /**
   * Get all types for a subject
   * @param {string} subject - Subject variable or IRI
   * @returns {Object} Query patterns
   */
  getTypes: subject => ({
    [subject]: {
      'rdf:type': '?type',
    },
  }),

  /**
   * Get all properties for a subject
   * @param {string} subject - Subject variable or IRI
   * @returns {Object} Query patterns
   */
  getProperties: subject => ({
    [subject]: {
      '?property': '?value',
    },
  }),

  /**
   * Find subjects of a specific type
   * @param {string} type - Type IRI
   * @returns {Object} Query patterns
   */
  findSubjectsOfType: type => ({
    '?subject': {
      'rdf:type': type,
    },
  }),

  /**
   * Find subjects with a specific property value
   * @param {string} property - Property IRI
   * @param {string} value - Value
   * @returns {Object} Query patterns
   */
  findSubjectsWithProperty: (property, value) => ({
    '?subject': {
      [property]: value,
    },
  }),

  /**
   * Get all labels for a subject
   * @param {string} subject - Subject variable or IRI
   * @returns {Object} Query patterns
   */
  getLabels: subject => ({
    [subject]: {
      'rdfs:label': '?label',
    },
  }),

  /**
   * Get all comments for a subject
   * @param {string} subject - Subject variable or IRI
   * @returns {Object} Query patterns
   */
  getComments: subject => ({
    [subject]: {
      'rdfs:comment': '?comment',
    },
  }),
};

/**
 * Analyze a SPARQL query
 * @param {string} query - SPARQL query string
 * @returns {Object} Query analysis
 */
export const analyzeSPARQLQuery = query => {
  const analysis = {
    type: 'UNKNOWN',
    prefixes: [],
    variables: [],
    patterns: [],
    filters: [],
    hasLimit: false,
    hasOffset: false,
    hasOrderBy: false,
    hasGroupBy: false,
    hasDistinct: false,
  };

  const lines = query.split('\n').map(line => line.trim());

  for (const line of lines) {
    // Detect query type
    if (line.startsWith('SELECT')) {
      analysis.type = 'SELECT';
      analysis.hasDistinct = line.includes('DISTINCT');
    } else if (line.startsWith('CONSTRUCT')) {
      analysis.type = 'CONSTRUCT';
    } else if (line.startsWith('ASK')) {
      analysis.type = 'ASK';
    } else if (line.startsWith('DESCRIBE')) {
      analysis.type = 'DESCRIBE';
    }

    // Extract prefixes
    if (line.startsWith('PREFIX')) {
      const match = line.match(/PREFIX\s+(\w+):\s*<([^>]+)>/);
      if (match) {
        analysis.prefixes.push({ prefix: match[1], namespace: match[2] });
      }
    }

    // Extract variables
    if (line.includes('?')) {
      const variables = line.match(/\?(\w+)/g);
      if (variables) {
        analysis.variables.push(...variables.map(v => v.slice(1)));
      }
    }

    // Detect clauses
    if (line.includes('LIMIT')) {
      analysis.hasLimit = true;
    }
    if (line.includes('OFFSET')) {
      analysis.hasOffset = true;
    }
    if (line.includes('ORDER BY')) {
      analysis.hasOrderBy = true;
    }
    if (line.includes('GROUP BY')) {
      analysis.hasGroupBy = true;
    }
    if (line.includes('FILTER')) {
      analysis.filters.push(line);
    }
  }

  // Remove duplicates from variables
  analysis.variables = [...new Set(analysis.variables)];

  return analysis;
};

/**
 * Validate a SPARQL query syntax
 * @param {string} query - SPARQL query string
 * @returns {Object} Validation result
 */
export const validateSPARQLQuery = query => {
  const issues = [];

  // Basic syntax checks
  if (!query.trim()) {
    issues.push({ type: 'error', message: 'Query is empty' });
    return { valid: false, issues };
  }

  const lines = query.split('\n').map(line => line.trim());
  let hasQueryType = false;
  let hasWhere = false;

  for (const [i, line] of lines.entries()) {
    // Check for query type
    if (/^(SELECT|CONSTRUCT|ASK|DESCRIBE)/.test(line)) {
      hasQueryType = true;
    }

    // Check for WHERE clause
    if (line.includes('WHERE {')) {
      hasWhere = true;
    }

    // Check for balanced braces
    const openBraces = (line.match(/\{/g) || []).length;
    const closeBraces = (line.match(/\}/g) || []).length;

    if (openBraces !== closeBraces && !line.includes('WHERE')) {
      issues.push({
        type: 'warning',
        message: `Unbalanced braces on line ${i + 1}`,
        line: i + 1,
      });
    }

    // Check for common syntax errors
    if (line.includes('..') && !line.includes('...')) {
      issues.push({
        type: 'error',
        message: `Invalid syntax on line ${i + 1}: '..' should be '...'`,
        line: i + 1,
      });
    }
  }

  if (!hasQueryType) {
    issues.push({
      type: 'error',
      message: 'No query type found (SELECT, CONSTRUCT, ASK, or DESCRIBE)',
    });
  }

  if (!hasWhere && query.includes('WHERE {')) {
    issues.push({
      type: 'error',
      message: 'WHERE clause found but not properly formatted',
    });
  }

  return {
    valid: issues.filter(i => i.type === 'error').length === 0,
    issues,
    issueCount: issues.length,
    errorCount: issues.filter(i => i.type === 'error').length,
    warningCount: issues.filter(i => i.type === 'warning').length,
  };
};

/**
 * Extract variables from a SPARQL query
 * @param {string} query - SPARQL query string
 * @returns {string[]} Array of variable names
 */
export const extractVariables = query => {
  const variables = new Set();
  const matches = query.match(/\?(\w+)/g);

  if (matches) {
    for (const match of matches) {
      variables.add(match.slice(1));
    }
  }

  return [...variables];
};

/**
 * Extract IRIs from a SPARQL query
 * @param {string} query - SPARQL query string
 * @returns {string[]} Array of IRIs
 */
export const extractIRIs = query => {
  const iris = new Set();
  const matches = query.match(/<([^>]+)>/g);

  if (matches) {
    for (const match of matches) {
      iris.add(match.substring(1, match.length - 1));
    }
  }

  return [...iris];
};

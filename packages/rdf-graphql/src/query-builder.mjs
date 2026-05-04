/**
 * @file Query Builder - Translates GraphQL queries to SPARQL
 * @module @unrdf/rdf-graphql/query
 */

import { z } from 'zod';

// Zod schemas for validation
const GraphQLFieldSchema = z.object({
  name: z.string(),
  alias: z.string().optional(),
  arguments: z.record(z.any()).optional(),
  selectionSet: z.any().optional(),
});

/**
 * SPARQL Query Builder - Converts GraphQL queries to SPARQL
 */
export class SPARQLQueryBuilder {
  /**
   * @param {object} config - Configuration
   * @param {Record<string, string>} [config.namespaces] - SPARQL namespace prefixes
   */
  constructor(config = {}) {
    this.namespaces = config.namespaces || {
      rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
      rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
      owl: 'http://www.w3.org/2002/07/owl#',
      xsd: 'http://www.w3.org/2001/XMLSchema#',
    };
  }

  /**
   * Build SPARQL query from GraphQL query info
   * @param {object} info - GraphQL resolve info
   * @param {string} resourceIRI - Subject IRI
   * @param {string} typeIRI - RDF class IRI
   * @returns {string} SPARQL query
   */
  buildQueryForResource(info, resourceIRI, typeIRI) {
    const prefixes = this.buildPrefixes();
    const patterns = this.buildGraphPatterns(info, '?s', typeIRI);
    const projection = this.buildProjection(info);

    return `
${prefixes}

SELECT ${projection} WHERE {
  BIND(<${resourceIRI}> AS ?s)
  ${patterns}
}
    `.trim();
  }

  /**
   * Build SPARQL query for listing resources
   * @param {object} info - GraphQL resolve info
   * @param {string} typeIRI - RDF class IRI
   * @param {object} args - GraphQL arguments (limit, offset)
   * @returns {string} SPARQL query
   */
  buildListQuery(info, typeIRI, args = {}) {
    const { limit = 10, offset = 0 } = args;
    const prefixes = this.buildPrefixes();
    const patterns = this.buildGraphPatterns(info, '?s', typeIRI);
    const projection = this.buildProjection(info);

    return `
${prefixes}

SELECT ${projection} WHERE {
  ?s a <${typeIRI}> .
  ${patterns}
}
LIMIT ${limit}
OFFSET ${offset}
    `.trim();
  }

  /**
   * Build SPARQL query with filters
   * @param {object} info - GraphQL resolve info
   * @param {string} typeIRI - RDF class IRI
   * @param {object} filters - Filter conditions
   * @returns {string} SPARQL query
   */
  buildFilteredQuery(info, typeIRI, filters = {}) {
    const prefixes = this.buildPrefixes();
    const patterns = this.buildGraphPatterns(info, '?s', typeIRI);
    const projection = this.buildProjection(info);
    const filterClauses = this.buildFilters(filters);

    return `
${prefixes}

SELECT ${projection} WHERE {
  ?s a <${typeIRI}> .
  ${patterns}
  ${filterClauses}
}
    `.trim();
  }

  /**
   * Build namespace prefixes for SPARQL
   * @returns {string} PREFIX declarations
   * @private
   */
  buildPrefixes() {
    return Object.entries(this.namespaces)
      .map(([prefix, uri]) => `PREFIX ${prefix}: <${uri}>`)
      .join('\n');
  }

  /**
   * Build SPARQL graph patterns from GraphQL field selection
   * @param {object} info - GraphQL resolve info
   * @param {string} subject - Subject variable
   * @param {string} typeIRI - RDF class IRI
   * @returns {string} Graph patterns
   * @private
   */
  buildGraphPatterns(info, subject, typeIRI) {
    const patterns = [];
    const fields = this.extractFields(info);

    for (const field of fields) {
      if (field.name === 'id') continue; // ID is the subject IRI

      const propertyIRI = this.fieldNameToPropertyIRI(field.name, typeIRI);
      const varName = `?${field.alias || field.name}`;

      if (field.selectionSet) {
        // Nested object - follow the relationship
        patterns.push(`OPTIONAL { ${subject} <${propertyIRI}> ${varName} . }`);
        // Could recursively build nested patterns here
      } else {
        // Simple property value
        patterns.push(`OPTIONAL { ${subject} <${propertyIRI}> ${varName} . }`);
      }
    }

    return patterns.join('\n  ');
  }

  /**
   * Build SPARQL projection (SELECT clause variables)
   * @param {object} info - GraphQL resolve info
   * @returns {string} Projection variables
   * @private
   */
  buildProjection(info) {
    const fields = this.extractFields(info);
    const vars = fields
      .filter(f => f.name !== 'id')
      .map(f => `?${f.alias || f.name}`);

    return `?s ${vars.join(' ')}`;
  }

  /**
   * Build FILTER clauses from arguments
   * @param {object} filters - Filter conditions
   * @returns {string} FILTER clauses
   * @private
   */
  buildFilters(filters) {
    const clauses = [];

    for (const [field, value] of Object.entries(filters)) {
      const varName = `?${field}`;

      if (typeof value === 'string') {
        clauses.push(`FILTER(${varName} = "${this.escapeSPARQL(value)}")`);
      } else if (typeof value === 'number') {
        clauses.push(`FILTER(${varName} = ${value})`);
      } else if (typeof value === 'boolean') {
        clauses.push(`FILTER(${varName} = ${value})`);
      } else if (value && typeof value === 'object') {
        // Handle complex filters (gt, lt, contains, etc.)
        if (value.eq) clauses.push(`FILTER(${varName} = "${this.escapeSPARQL(value.eq)}")`);
        if (value.ne) clauses.push(`FILTER(${varName} != "${this.escapeSPARQL(value.ne)}")`);
        if (value.gt) clauses.push(`FILTER(${varName} > ${value.gt})`);
        if (value.lt) clauses.push(`FILTER(${varName} < ${value.lt})`);
        if (value.contains) {
          clauses.push(`FILTER(CONTAINS(LCASE(STR(${varName})), LCASE("${this.escapeSPARQL(value.contains)}")))`);
        }
      }
    }

    return clauses.join('\n  ');
  }

  /**
   * Extract field selections from GraphQL info
   * @param {object} info - GraphQL resolve info
   * @returns {Array<{name: string, alias?: string, selectionSet?: any}>}
   * @private
   */
  extractFields(info) {
    if (!info.fieldNodes || !info.fieldNodes[0]?.selectionSet) {
      return [];
    }

    const selections = info.fieldNodes[0].selectionSet.selections;
    return selections
      .filter(s => s.kind === 'Field')
      .map(s => ({
        name: s.name.value,
        alias: s.alias?.value,
        arguments: this.extractArguments(s.arguments),
        selectionSet: s.selectionSet,
      }));
  }

  /**
   * Extract arguments from GraphQL field
   * @param {any} args - GraphQL arguments AST
   * @returns {Record<string, any>}
   * @private
   */
  extractArguments(args) {
    if (!args) return {};

    const result = {};
    for (const arg of args) {
      result[arg.name.value] = this.extractValue(arg.value);
    }
    return result;
  }

  /**
   * Extract value from GraphQL value AST
   * @param {any} valueNode - GraphQL value node
   * @returns {any}
   * @private
   */
  extractValue(valueNode) {
    switch (valueNode.kind) {
      case 'StringValue':
        return valueNode.value;
      case 'IntValue':
        return parseInt(valueNode.value, 10);
      case 'FloatValue':
        return parseFloat(valueNode.value);
      case 'BooleanValue':
        return valueNode.value;
      case 'ListValue':
        return valueNode.values.map(v => this.extractValue(v));
      case 'ObjectValue':
        return valueNode.fields.reduce((obj, field) => {
          obj[field.name.value] = this.extractValue(field.value);
          return obj;
        }, {});
      default:
        return null;
    }
  }

  /**
   * Convert GraphQL field name to RDF property IRI
   * @param {string} fieldName - GraphQL field name
   * @param {string} typeIRI - Class IRI for context
   * @returns {string} Property IRI
   * @private
   */
  fieldNameToPropertyIRI(fieldName, typeIRI) {
    // Extract namespace from type IRI
    const match = typeIRI.match(/^(.+[#/])[^#/]+$/);
    const namespace = match ? match[1] : typeIRI + '#';

    // Construct property IRI (assumes same namespace as class)
    return namespace + fieldName;
  }

  /**
   * Escape string for SPARQL
   * @param {string} str - Input string
   * @returns {string} Escaped string
   * @private
   */
  escapeSPARQL(str) {
    return str
      .replace(/\\/g, '\\\\')
      .replace(/"/g, '\\"')
      .replace(/\n/g, '\\n')
      .replace(/\r/g, '\\r');
  }

  /**
   * Add custom namespace
   * @param {string} prefix - Namespace prefix
   * @param {string} uri - Namespace URI
   */
  addNamespace(prefix, uri) {
    this.namespaces[prefix] = uri;
  }
}

/**
 * Build a simple SPARQL SELECT query
 * @param {object} options - Query options
 * @param {string} options.subject - Subject variable or IRI
 * @param {string} options.predicate - Predicate IRI
 * @param {string} [options.object] - Object variable or value
 * @param {number} [options.limit] - Result limit
 * @returns {string} SPARQL query
 */
export function buildSimpleQuery(options) {
  const { subject, predicate, object = '?o', limit } = options;
  const limitClause = limit ? `LIMIT ${limit}` : '';

  return `
SELECT * WHERE {
  ${subject} <${predicate}> ${object} .
}
${limitClause}
  `.trim();
}

/**
 * Build SPARQL CONSTRUCT query
 * @param {object} options - Query options
 * @param {string} options.template - CONSTRUCT template
 * @param {string} options.where - WHERE clause
 * @returns {string} SPARQL CONSTRUCT query
 */
export function buildConstructQuery(options) {
  const { template, where } = options;

  return `
CONSTRUCT {
  ${template}
}
WHERE {
  ${where}
}
  `.trim();
}

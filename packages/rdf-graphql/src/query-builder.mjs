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

// =============================================================================
// Injection-safe term helpers (see .claude/rules/scoped/p1-sparql-injection.md)
// =============================================================================

const VAR_NAME_RE = /^[A-Za-z_][A-Za-z0-9_]*$/;
const PREFIX_RE = /^[A-Za-z][A-Za-z0-9_-]*$|^$/;
// Characters that may never appear inside an IRIREF (SPARQL 1.1 grammar rule [139])
// eslint-disable-next-line no-control-regex
const IRI_FORBIDDEN_RE = /[\u0000- <>"{}|^`\\]/;

/**
 * Validate an IRI so it is safe inside <...>.
 * @param {string} iri - Candidate IRI
 * @returns {string} The same IRI
 * @throws {Error} If the IRI could break out of the IRIREF
 */
export function assertSafeIRI(iri) {
  if (typeof iri !== 'string' || iri.length === 0 || IRI_FORBIDDEN_RE.test(iri)) {
    throw new Error(`Invalid IRI: ${JSON.stringify(iri)}`);
  }
  return iri;
}

/**
 * Validate a SPARQL variable name (without the leading ?).
 * @param {string} name - Candidate variable name
 * @returns {string} The same name
 * @throws {Error} If the name is not a plain identifier
 */
export function assertSafeVarName(name) {
  if (typeof name !== 'string' || !VAR_NAME_RE.test(name)) {
    throw new Error(`Invalid variable name: ${JSON.stringify(name)}`);
  }
  return name;
}

/**
 * Coerce a value to a non-negative integer for LIMIT/OFFSET.
 * @param {unknown} value - Candidate value
 * @param {string} label - Name used in the error message
 * @returns {number} Integer
 * @throws {Error} If the value is not a non-negative integer
 */
export function assertNonNegativeInteger(value, label) {
  const n = typeof value === 'string' && /^\d+$/.test(value) ? Number(value) : value;
  if (typeof n !== 'number' || !Number.isSafeInteger(n) || n < 0) {
    throw new Error(`${label} must be a non-negative integer`);
  }
  return n;
}

/**
 * Coerce a value to a numeric SPARQL literal (rejects expressions).
 * @param {unknown} value - Candidate value
 * @returns {string} Canonical numeric lexical form
 * @throws {Error} If the value is not a finite number
 */
export function toNumericLiteral(value) {
  const n =
    typeof value === 'string' && /^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$/.test(value.trim())
      ? Number(value)
      : value;
  if (typeof n !== 'number' || !Number.isFinite(n)) {
    throw new Error(`Filter operand must be a finite numeric value, got ${JSON.stringify(value)}`);
  }
  return String(n);
}

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
    assertSafeIRI(resourceIRI);
    assertSafeIRI(typeIRI);
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
    const limit = assertNonNegativeInteger(args.limit ?? 10, 'limit');
    const offset = assertNonNegativeInteger(args.offset ?? 0, 'offset');
    assertSafeIRI(typeIRI);
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
    assertSafeIRI(typeIRI);
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
      .map(([prefix, uri]) => {
        if (!PREFIX_RE.test(prefix)) {
          throw new Error(`Invalid prefix name: ${JSON.stringify(prefix)}`);
        }
        return `PREFIX ${prefix}: <${assertSafeIRI(uri)}>`;
      })
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
      const varName = `?${assertSafeVarName(field.alias || field.name)}`;

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
      .map(f => `?${assertSafeVarName(f.alias || f.name)}`);

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
      const varName = `?${assertSafeVarName(field)}`;

      if (typeof value === 'string') {
        clauses.push(`FILTER(${varName} = "${this.escapeSPARQL(value)}")`);
      } else if (typeof value === 'number') {
        clauses.push(`FILTER(${varName} = ${toNumericLiteral(value)})`);
      } else if (typeof value === 'boolean') {
        clauses.push(`FILTER(${varName} = ${value})`);
      } else if (value && typeof value === 'object') {
        // Handle complex filters (gt, lt, contains, etc.)
        if (value.eq) clauses.push(`FILTER(${varName} = "${this.escapeSPARQL(String(value.eq))}")`);
        if (value.ne) clauses.push(`FILTER(${varName} != "${this.escapeSPARQL(String(value.ne))}")`);
        if (value.gt !== undefined && value.gt !== null) {
          clauses.push(`FILTER(${varName} > ${toNumericLiteral(value.gt)})`);
        }
        if (value.lt !== undefined && value.lt !== null) {
          clauses.push(`FILTER(${varName} < ${toNumericLiteral(value.lt)})`);
        }
        if (value.contains) {
          clauses.push(
            `FILTER(CONTAINS(LCASE(STR(${varName})), LCASE("${this.escapeSPARQL(String(value.contains))}")))`
          );
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
    return assertSafeIRI(namespace + fieldName);
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
      .replace(/\r/g, '\\r')
      .replace(/\t/g, '\\t');
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
  const limitClause =
    limit === undefined || limit === null || limit === ''
      ? ''
      : `LIMIT ${assertNonNegativeInteger(limit, 'limit')}`;

  const term = (value, position) => {
    if (typeof value !== 'string') throw new Error(`Invalid ${position}`);
    if (/^[?$]/.test(value)) return `?${assertSafeVarName(value.slice(1))}`;
    if (/^<.*>$/.test(value)) return `<${assertSafeIRI(value.slice(1, -1))}>`;
    if (position === 'object') {
      return `"${new SPARQLQueryBuilder().escapeSPARQL(value)}"`; // plain literal
    }
    return `<${assertSafeIRI(value)}>`;
  };

  return `
SELECT * WHERE {
  ${term(subject, 'subject')} <${assertSafeIRI(predicate)}> ${term(object, 'object')} .
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

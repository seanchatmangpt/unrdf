/**
 * @file RDF Template Filters - @unrdf/core Integration
 * @module @unrdf/kgn/rdf/filters
 * @description Custom Nunjucks filters for RDF operations using @unrdf/core
 */

import { z } from 'zod';
import {
  namedNode,
  literal as createRdfLiteral,
  blankNode as createRdfBlankNode,
  quad,
  COMMON_PREFIXES,
} from '@unrdf/core';

/**
 * Schema for toTurtle filter options
 */
const ToTurtleOptionsSchema = z.object({
  prefixes: z.record(z.string()).optional(),
  indent: z.string().optional(),
  includeComments: z.boolean().optional(),
});

/**
 * Schema for toSparql filter options
 */
const ToSparqlOptionsSchema = z.object({
  prefixes: z.record(z.string()).optional(),
  type: z.enum(['select', 'construct', 'ask', 'describe']).optional(),
  limit: z.number().int().positive().optional(),
});

/**
 * Schema for RDF prefix configuration
 */
const RdfPrefixSchema = z.object({
  uri: z.string().url(),
  prefix: z.string().min(1),
});

/**
 * Schema for blank node ID
 */
const BlankNodeIdSchema = z.string().regex(/^[a-zA-Z0-9_]+$/, 'Blank node ID must be alphanumeric with underscores');

/**
 * Schema for literal options
 */
const LiteralOptionsSchema = z.object({
  value: z.any(),
  lang: z.string().regex(/^[a-z]{2,3}(-[A-Z]{2})?$/).optional(),
  datatype: z.string().optional(),
});

/**
 * Convert RDF data to Turtle format
 * @param {Object|Array} data - RDF quads or triples
 * @param {Object} options - Formatting options
 * @returns {string} Turtle-formatted RDF
 * @example
 * {{ rdfData | toTurtle }}
 * {{ rdfData | toTurtle({ prefixes: { ex: 'http://example.org/' } }) }}
 */
export function toTurtle(data, options = {}) {
  const validated = ToTurtleOptionsSchema.parse(options);
  const {
    prefixes = {},
    indent = '  ',
    includeComments = false
  } = validated;

  if (!data) {
    return '';
  }

  // Merge with common prefixes
  const allPrefixes = { ...COMMON_PREFIXES, ...prefixes };

  const lines = [];

  // Add prefix declarations
  if (includeComments) {
    lines.push('# RDF Turtle Format');
    lines.push('');
  }

  for (const [prefix, uri] of Object.entries(allPrefixes)) {
    lines.push(`@prefix ${prefix}: <${uri}> .`);
  }

  if (Object.keys(allPrefixes).length > 0) {
    lines.push('');
  }

  // Process quads/triples
  const quads = Array.isArray(data) ? data : [data];

  for (const item of quads) {
    if (!item || typeof item !== 'object') {
      continue;
    }

    const subject = formatTerm(item.subject || item.s, allPrefixes);
    const predicate = formatTerm(item.predicate || item.p, allPrefixes);
    const object = formatTerm(item.object || item.o, allPrefixes);

    lines.push(`${subject} ${predicate} ${object} .`);
  }

  return lines.join('\n');
}

/**
 * Generate SPARQL query from template pattern
 * @param {Object|string} pattern - Query pattern or template
 * @param {Object} options - Query options
 * @returns {string} SPARQL query string
 * @example
 * {{ { s: '?s', p: 'rdf:type', o: '?type' } | toSparql }}
 * {{ pattern | toSparql({ type: 'select', limit: 10 }) }}
 */
export function toSparql(pattern, options = {}) {
  const validated = ToSparqlOptionsSchema.parse(options);
  const {
    prefixes = {},
    type = 'select',
    limit
  } = validated;

  // Merge with common prefixes
  const allPrefixes = { ...COMMON_PREFIXES, ...prefixes };

  const lines = [];

  // Add prefix declarations
  for (const [prefix, uri] of Object.entries(allPrefixes)) {
    lines.push(`PREFIX ${prefix}: <${uri}>`);
  }

  if (Object.keys(allPrefixes).length > 0) {
    lines.push('');
  }

  // Handle string pattern (raw SPARQL)
  if (typeof pattern === 'string') {
    lines.push(pattern);
    return lines.join('\n');
  }

  // Build query from pattern object
  if (type === 'select') {
    const vars = extractVariables(pattern);
    lines.push(`SELECT ${vars.join(' ')}`);
  } else if (type === 'construct') {
    lines.push('CONSTRUCT {');
    lines.push(`  ${formatTriplePattern(pattern, allPrefixes)}`);
    lines.push('}');
  } else if (type === 'ask') {
    lines.push('ASK');
  } else if (type === 'describe') {
    const subject = pattern.subject || pattern.s || '?s';
    lines.push(`DESCRIBE ${subject}`);
  }

  lines.push('WHERE {');

  if (Array.isArray(pattern)) {
    for (const triple of pattern) {
      lines.push(`  ${formatTriplePattern(triple, allPrefixes)} .`);
    }
  } else {
    lines.push(`  ${formatTriplePattern(pattern, allPrefixes)} .`);
  }

  lines.push('}');

  if (limit) {
    lines.push(`LIMIT ${limit}`);
  }

  return lines.join('\n');
}

/**
 * Manage RDF prefix mappings
 * @param {string} uri - Full URI to convert
 * @param {string} prefix - Prefix to use
 * @returns {string} Prefixed URI (CURIE)
 * @example
 * {{ 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' | rdfPrefix('rdf') }}
 * // Returns: "rdf:type"
 */
export function rdfPrefix(uri, prefix) {
  const validated = RdfPrefixSchema.parse({ uri, prefix });

  const allPrefixes = { ...COMMON_PREFIXES };

  // Check if prefix is known
  if (allPrefixes[validated.prefix]) {
    const namespace = allPrefixes[validated.prefix];
    if (validated.uri.startsWith(namespace)) {
      const localName = validated.uri.substring(namespace.length);
      return `${validated.prefix}:${localName}`;
    }
  }

  // If URI doesn't match known prefix, return as-is
  return validated.uri;
}

/**
 * Generate blank node identifier
 * @param {string} id - Optional identifier
 * @returns {string} Blank node with _: prefix
 * @example
 * {{ 'person1' | blankNode }}
 * // Returns: "_:person1"
 */
export function blankNode(id) {
  if (!id) {
    // Generate a deterministic blank node ID based on timestamp
    const timestamp = Date.now();
    return `_:blank${timestamp % 100000}`;
  }

  const validated = BlankNodeIdSchema.parse(id);
  return `_:${validated}`;
}

/**
 * Create RDF literal with language tag or datatype
 * @param {any} value - Literal value
 * @param {string} lang - Language tag (e.g., 'en', 'fr')
 * @param {string} datatype - Datatype URI (e.g., 'xsd:integer')
 * @returns {string} RDF literal string
 * @example
 * {{ "Hello" | literal('en') }}
 * // Returns: '"Hello"@en'
 * {{ 42 | literal(null, 'xsd:integer') }}
 * // Returns: '"42"^^xsd:integer'
 */
export function literal(value, lang = null, datatype = null) {
  const validated = LiteralOptionsSchema.parse({
    value,
    lang: lang || undefined,
    datatype: datatype || undefined,
  });

  if (validated.value === null || validated.value === undefined) {
    return '""';
  }

  // Escape quotes in value
  const stringValue = String(validated.value).replace(/"/g, '\\"');
  let literalStr = `"${stringValue}"`;

  if (validated.lang) {
    literalStr += `@${validated.lang}`;
  } else if (validated.datatype) {
    literalStr += `^^${validated.datatype}`;
  }

  return literalStr;
}

/**
 * Format RDF term for Turtle output
 * @private
 * @param {Object|string} term - RDF term
 * @param {Object} prefixes - Prefix mappings
 * @returns {string} Formatted term
 */
function formatTerm(term, prefixes = {}) {
  if (!term) {
    return '""';
  }

  if (typeof term === 'string') {
    // Variable
    if (term.startsWith('?') || term.startsWith('$')) {
      return term;
    }
    // Blank node
    if (term.startsWith('_:')) {
      return term;
    }
    // CURIE
    if (term.includes(':') && !term.startsWith('http')) {
      return term;
    }
    // URI
    return `<${term}>`;
  }

  if (typeof term === 'object') {
    // Named Node
    if (term.termType === 'NamedNode' || term.type === 'NamedNode') {
      const uri = term.value || term.uri;
      return contractUri(uri, prefixes);
    }
    // Literal
    if (term.termType === 'Literal' || term.type === 'Literal') {
      let lit = `"${(term.value || '').replace(/"/g, '\\"')}"`;
      if (term.language) {
        lit += `@${term.language}`;
      } else if (term.datatype && term.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string') {
        lit += `^^${contractUri(term.datatype.value, prefixes)}`;
      }
      return lit;
    }
    // Blank Node
    if (term.termType === 'BlankNode' || term.type === 'BlankNode') {
      return `_:${term.value || 'blank'}`;
    }
  }

  return String(term);
}

/**
 * Contract URI to CURIE if possible
 * @private
 * @param {string} uri - Full URI
 * @param {Object} prefixes - Prefix mappings
 * @returns {string} CURIE or original URI
 */
function contractUri(uri, prefixes = {}) {
  for (const [prefix, namespace] of Object.entries(prefixes)) {
    if (uri.startsWith(namespace)) {
      const localName = uri.substring(namespace.length);
      return `${prefix}:${localName}`;
    }
  }
  return `<${uri}>`;
}

/**
 * Format triple pattern for SPARQL
 * @private
 * @param {Object} pattern - Triple pattern
 * @param {Object} prefixes - Prefix mappings
 * @returns {string} Formatted triple pattern
 */
function formatTriplePattern(pattern, prefixes = {}) {
  const subject = formatTerm(pattern.subject || pattern.s, prefixes);
  const predicate = formatTerm(pattern.predicate || pattern.p, prefixes);
  const object = formatTerm(pattern.object || pattern.o, prefixes);

  return `${subject} ${predicate} ${object}`;
}

/**
 * Extract SPARQL variables from pattern
 * @private
 * @param {Object|Array} pattern - Query pattern
 * @returns {Array<string>} Variable names
 */
function extractVariables(pattern) {
  const vars = new Set();
  const patterns = Array.isArray(pattern) ? pattern : [pattern];

  for (const p of patterns) {
    if (!p || typeof p !== 'object') continue;

    const terms = [
      p.subject || p.s,
      p.predicate || p.p,
      p.object || p.o,
    ];

    for (const term of terms) {
      if (typeof term === 'string' && (term.startsWith('?') || term.startsWith('$'))) {
        vars.add(term);
      }
    }
  }

  return vars.size > 0 ? Array.from(vars) : ['*'];
}

/**
 * Export all RDF filters
 */
export const rdfTemplateFilters = {
  toTurtle,
  toSparql,
  rdfPrefix,
  blankNode,
  literal,
};

export default rdfTemplateFilters;

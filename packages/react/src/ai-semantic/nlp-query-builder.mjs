/**
 * @file Natural Language to SPARQL Query Builder
 * @module ai-semantic/nlp-query-builder
 *
 * @description
 * Converts natural language queries into SPARQL queries using pattern matching
 * and template-based approach. Supports ontology context for improved query
 * generation and provides query completion suggestions.
 *
 * Integrates with UNRDF's Knowledge Hook system and provides OTEL observability.
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import LRUCache from 'lru-cache';
import { z } from 'zod';

const tracer = trace.getTracer('unrdf-ai-semantic');

// ─── Zod Schemas ────────────────────────────────────────────────────────────

/**
 * NLP Query Builder configuration schema
 */
const NLPQueryBuilderConfigSchema = z.object({
  cacheSize: z.number().int().min(0).default(500),
  enableCache: z.boolean().default(true),
  defaultLimit: z.number().int().min(1).default(100),
  caseSensitive: z.boolean().default(false),
});

/**
 * Ontology context schema
 */
const OntologyContextSchema = z.object({
  prefixes: z.record(z.string(), z.string()).default({}),
  classes: z.array(z.string()).default([]),
  properties: z.array(z.string()).default([]),
});

/**
 * Parsed query intermediate representation schema
 */
const ParsedQuerySchema = z.object({
  intent: z.enum([
    'find_all',
    'find_property',
    'count',
    'filter',
    'describe',
    'ask',
    'custom',
    'unknown',
  ]),
  entityType: z.string().optional(),
  property: z.string().optional(),
  value: z.string().optional(),
  subject: z.string().optional(),
  filters: z.array(z.object({
    property: z.string(),
    operator: z.enum(['=', '!=', '<', '>', '<=', '>=', 'contains']),
    value: z.string(),
  })).default([]),
  originalQuery: z.string(),
  confidence: z.number().min(0).max(1),
});

/**
 * Custom mapping schema
 */
const CustomMappingSchema = z.object({
  pattern: z.instanceof(RegExp).or(z.string()),
  template: z.string(),
  intent: z.enum([
    'find_all', 'find_property', 'count', 'filter',
    'describe', 'ask', 'custom', 'unknown',
  ]).default('custom'),
});

/**
 * Query suggestion schema
 */
const SuggestionSchema = z.object({
  text: z.string(),
  sparql: z.string(),
  confidence: z.number().min(0).max(1),
});

// ─── Built-in Patterns ──────────────────────────────────────────────────────

/**
 * @typedef {Object} PatternDef
 * @property {RegExp} regex - Pattern to match
 * @property {string} intent - Query intent
 * @property {function(RegExpMatchArray): Object} extract - Extract params from match
 */

/** @type {PatternDef[]} */
const BUILTIN_PATTERNS = [
  // "find all X" / "list all X" / "show all X" / "get all X"
  {
    regex: /^(?:find|list|show|get)\s+all\s+(.+?)$/i,
    intent: 'find_all',
    extract: (m) => ({ entityType: m[1].trim() }),
  },
  // "find X" / "list X" / "show X" / "get X" (without "all")
  {
    regex: /^(?:find|list|show|get)\s+(.+?)$/i,
    intent: 'find_all',
    extract: (m) => ({ entityType: m[1].trim() }),
  },
  // "what is the Y of X" / "what are the Y of X"
  {
    regex: /^what\s+(?:is|are)\s+the\s+(.+?)\s+of\s+(.+?)$/i,
    intent: 'find_property',
    extract: (m) => ({ property: m[1].trim(), subject: m[2].trim() }),
  },
  // "how many X" / "count X" / "count all X"
  {
    regex: /^(?:how\s+many|count(?:\s+all)?)\s+(.+?)$/i,
    intent: 'count',
    extract: (m) => ({ entityType: m[1].trim() }),
  },
  // "list X with Y = Z" / "find X where Y = Z" / "show X with Y = Z"
  {
    regex: /^(?:find|list|show|get)\s+(.+?)\s+(?:with|where)\s+(.+?)\s*=\s*(.+?)$/i,
    intent: 'filter',
    extract: (m) => ({
      entityType: m[1].trim(),
      filters: [{
        property: m[2].trim(),
        operator: '=',
        value: m[3].trim(),
      }],
    }),
  },
  // "list X with Y containing Z"
  {
    regex: /^(?:find|list|show|get)\s+(.+?)\s+(?:with|where)\s+(.+?)\s+containing\s+(.+?)$/i,
    intent: 'filter',
    extract: (m) => ({
      entityType: m[1].trim(),
      filters: [{
        property: m[2].trim(),
        operator: 'contains',
        value: m[3].trim(),
      }],
    }),
  },
  // "describe X" / "what is X"
  {
    regex: /^(?:describe|what\s+is)\s+(.+?)$/i,
    intent: 'describe',
    extract: (m) => ({ subject: m[1].trim() }),
  },
  // "is X a Y" / "does X have Y"
  {
    regex: /^(?:is|does)\s+(.+?)\s+(?:a|an|have)\s+(.+?)$/i,
    intent: 'ask',
    extract: (m) => ({ subject: m[1].trim(), entityType: m[2].trim() }),
  },
];

// ─── Utility Functions ──────────────────────────────────────────────────────

/**
 * Resolve a term to an IRI using ontology context
 * @param {string} term - Term to resolve
 * @param {Object} ontology - Ontology context
 * @param {string} kind - 'class' or 'property'
 * @returns {string} Resolved IRI or original term
 */
function resolveToIRI(term, ontology, kind) {
  if (!ontology) return term;

  // Check if it already looks like a URI
  if (term.startsWith('http://') || term.startsWith('https://')) {
    return term;
  }

  // Check if it's a prefixed name
  if (term.includes(':')) {
    const [prefix, local] = term.split(':', 2);
    const ns = ontology.prefixes[prefix];
    if (ns) return `${ns}${local}`;
  }

  // Try to find a matching class or property
  const normalizedTerm = term.toLowerCase().replace(/\s+/g, '');
  const candidates = kind === 'class' ? ontology.classes : ontology.properties;

  for (const candidate of candidates) {
    const localName = candidate.includes('#')
      ? candidate.split('#').pop()
      : candidate.split('/').pop();
    if (localName && localName.toLowerCase() === normalizedTerm) {
      return candidate;
    }
  }

  // Try with common prefixes
  for (const [prefix, ns] of Object.entries(ontology.prefixes)) {
    const candidate = `${ns}${term.charAt(0).toUpperCase()}${term.slice(1)}`;
    if (kind === 'class' && ontology.classes.includes(candidate)) {
      return candidate;
    }
    if (kind === 'property' && ontology.properties.includes(candidate)) {
      return candidate;
    }
  }

  return term;
}

/**
 * Build SPARQL prefix declarations
 * @param {Record<string, string>} prefixes - Prefix map
 * @returns {string} PREFIX declarations
 */
function buildPrefixBlock(prefixes) {
  if (!prefixes || Object.keys(prefixes).length === 0) return '';
  const lines = Object.entries(prefixes)
    .map(([prefix, ns]) => `PREFIX ${prefix}: <${ns}>`)
    .join('\n');
  return lines + '\n';
}

/**
 * Wrap a value as a SPARQL term (IRI or literal)
 * @param {string} value - Value to wrap
 * @returns {string} SPARQL term
 */
function sparqlTerm(value) {
  if (value.startsWith('http://') || value.startsWith('https://')) {
    return `<${value}>`;
  }
  if (value.includes(':') && !value.includes(' ')) {
    return value; // Prefixed name
  }
  return `"${value.replace(/"/g, '\\"')}"`;
}

// ─── NLPQueryBuilder Class ──────────────────────────────────────────────────

/**
 * Natural language to SPARQL query builder
 *
 * Converts natural language queries into SPARQL using pattern matching
 * and template-based transformation. Supports ontology context for
 * better IRI resolution and query suggestions.
 *
 * @example
 * const builder = createNLPQueryBuilder();
 * builder.setOntology(
 *   { foaf: 'http://xmlns.com/foaf/0.1/' },
 *   ['http://xmlns.com/foaf/0.1/Person'],
 *   ['http://xmlns.com/foaf/0.1/name']
 * );
 * const result = builder.buildQuery('find all Person');
 * console.log(result.sparql);
 */
export class NLPQueryBuilder {
  /**
   * Create a new NLP query builder
   * @param {Object} [config] - Builder configuration
   * @param {number} [config.cacheSize=500] - LRU cache size
   * @param {boolean} [config.enableCache=true] - Enable query cache
   * @param {number} [config.defaultLimit=100] - Default LIMIT for queries
   * @param {boolean} [config.caseSensitive=false] - Case-sensitive matching
   */
  constructor(config = {}) {
    this.config = NLPQueryBuilderConfigSchema.parse(config);

    /** @type {Object} Ontology context */
    this.ontology = { prefixes: {}, classes: [], properties: [] };

    /** @type {Array<{pattern: RegExp, template: string, intent: string}>} */
    this.customMappings = [];

    this.cache = this.config.enableCache
      ? new LRUCache({ max: this.config.cacheSize })
      : null;

    this.stats = {
      queriesBuilt: 0,
      cacheHits: 0,
      customMappingsUsed: 0,
      parseFailures: 0,
    };
  }

  /**
   * Parse a natural language query into an intermediate representation
   * @param {string} naturalLanguageQuery - The natural language query
   * @returns {Object} Parsed query with intent, entities, and confidence
   * @throws {Error} If query is empty
   */
  parse(naturalLanguageQuery) {
    z.string().min(1, 'Query cannot be empty').parse(naturalLanguageQuery);

    return tracer.startActiveSpan('nlp.parse', (span) => {
      try {
        const query = naturalLanguageQuery.trim();
        span.setAttribute('nlp.query_length', query.length);

        // Try custom mappings first
        for (const mapping of this.customMappings) {
          const regex = mapping.pattern instanceof RegExp
            ? mapping.pattern
            : new RegExp(mapping.pattern, 'i');
          const match = query.match(regex);
          if (match) {
            this.stats.customMappingsUsed++;
            const result = ParsedQuerySchema.parse({
              intent: mapping.intent,
              originalQuery: query,
              confidence: 0.9,
              subject: match[1] || undefined,
              entityType: match[2] || match[1] || undefined,
            });
            span.setStatus({ code: SpanStatusCode.OK });
            return result;
          }
        }

        // Try built-in patterns
        for (const pattern of BUILTIN_PATTERNS) {
          const match = query.match(pattern.regex);
          if (match) {
            const extracted = pattern.extract(match);
            const result = ParsedQuerySchema.parse({
              intent: pattern.intent,
              originalQuery: query,
              confidence: 0.8,
              ...extracted,
            });
            span.setStatus({ code: SpanStatusCode.OK });
            return result;
          }
        }

        // Fallback: unknown intent
        this.stats.parseFailures++;
        const result = ParsedQuerySchema.parse({
          intent: 'unknown',
          originalQuery: query,
          confidence: 0.1,
        });

        span.setAttribute('nlp.parse_result', 'unknown');
        span.setStatus({ code: SpanStatusCode.OK });
        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Convert a parsed intermediate representation to a SPARQL query
   * @param {Object} parsed - Parsed query from parse()
   * @returns {string} SPARQL query string
   * @throws {Error} If parsed query is invalid
   */
  toSPARQL(parsed) {
    const validated = ParsedQuerySchema.parse(parsed);

    return tracer.startActiveSpan('nlp.to_sparql', (span) => {
      try {
        const prefixes = buildPrefixBlock(this.ontology.prefixes);
        let sparql = '';

        switch (validated.intent) {
          case 'find_all':
            sparql = this._buildFindAll(validated, prefixes);
            break;
          case 'find_property':
            sparql = this._buildFindProperty(validated, prefixes);
            break;
          case 'count':
            sparql = this._buildCount(validated, prefixes);
            break;
          case 'filter':
            sparql = this._buildFilter(validated, prefixes);
            break;
          case 'describe':
            sparql = this._buildDescribe(validated, prefixes);
            break;
          case 'ask':
            sparql = this._buildAsk(validated, prefixes);
            break;
          case 'custom':
            sparql = this._buildCustom(validated, prefixes);
            break;
          default:
            sparql = this._buildFallback(validated, prefixes);
        }

        span.setAttribute('nlp.sparql_length', sparql.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return sparql;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * End-to-end natural language to SPARQL conversion
   * @param {string} naturalLanguageQuery - The natural language query
   * @returns {Object} Result with parsed representation and SPARQL
   * @throws {Error} If query is empty or cannot be processed
   */
  buildQuery(naturalLanguageQuery) {
    z.string().min(1, 'Query cannot be empty').parse(naturalLanguageQuery);

    return tracer.startActiveSpan('nlp.build_query', (span) => {
      try {
        // Check cache
        const cacheKey = `query:${naturalLanguageQuery}`;
        if (this.cache) {
          const cached = this.cache.get(cacheKey);
          if (cached) {
            this.stats.cacheHits++;
            span.setStatus({ code: SpanStatusCode.OK });
            return cached;
          }
        }

        const parsed = this.parse(naturalLanguageQuery);
        const sparql = this.toSPARQL(parsed);

        const result = {
          parsed,
          sparql,
          naturalLanguageQuery,
        };

        if (this.cache) {
          this.cache.set(cacheKey, result);
        }

        this.stats.queriesBuilt++;
        span.setAttributes({
          'nlp.intent': parsed.intent,
          'nlp.confidence': parsed.confidence,
        });
        span.setStatus({ code: SpanStatusCode.OK });
        return result;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Add a custom natural language to SPARQL mapping
   * @param {RegExp|string} pattern - Pattern to match (regex or string)
   * @param {string} sparqlTemplate - SPARQL template (use $1, $2 for captures)
   * @param {string} [intent='custom'] - Query intent for the mapping
   * @throws {Error} If pattern or template is invalid
   * @example
   * builder.addMapping(
   *   /^who\s+knows\s+(.+)$/i,
   *   'SELECT ?person WHERE { ?person <http://xmlns.com/foaf/0.1/knows> <$1> }'
   * );
   */
  addMapping(pattern, sparqlTemplate, intent = 'custom') {
    z.string().min(1, 'SPARQL template cannot be empty').parse(sparqlTemplate);

    const mapping = {
      pattern: pattern instanceof RegExp ? pattern : new RegExp(pattern, 'i'),
      template: sparqlTemplate,
      intent,
    };

    this.customMappings.push(mapping);
  }

  /**
   * Get query completion suggestions for a partial query
   * @param {string} partialQuery - Partial natural language query
   * @returns {Array<{text: string, sparql: string, confidence: number}>} Suggestions
   */
  getSuggestions(partialQuery) {
    z.string().parse(partialQuery);

    return tracer.startActiveSpan('nlp.get_suggestions', (span) => {
      try {
        const suggestions = [];
        const partial = partialQuery.trim().toLowerCase();

        if (partial.length === 0) {
          span.setStatus({ code: SpanStatusCode.OK });
          return this._getDefaultSuggestions();
        }

        // Suggest based on partial matching
        const templates = this._getSuggestionTemplates();

        for (const template of templates) {
          if (template.prefix.toLowerCase().startsWith(partial) ||
              partial.startsWith(template.prefix.toLowerCase())) {
            for (const cls of this.ontology.classes.slice(0, 5)) {
              const localName = cls.includes('#')
                ? cls.split('#').pop()
                : cls.split('/').pop();
              const text = template.template.replace('{class}', localName);
              const built = this.buildQuery(text);
              suggestions.push(SuggestionSchema.parse({
                text,
                sparql: built.sparql,
                confidence: 0.7,
              }));
            }
          }
        }

        // If no suggestions from templates, try to build as-is
        if (suggestions.length === 0 && partial.length > 3) {
          try {
            const built = this.buildQuery(partialQuery);
            if (built.parsed.intent !== 'unknown') {
              suggestions.push(SuggestionSchema.parse({
                text: partialQuery,
                sparql: built.sparql,
                confidence: built.parsed.confidence,
              }));
            }
          } catch (_) {
            // Ignore parse failures for suggestions
          }
        }

        span.setAttribute('nlp.suggestions_count', suggestions.length);
        span.setStatus({ code: SpanStatusCode.OK });
        return suggestions;
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      }
    });
  }

  /**
   * Set the ontology context for better query generation
   * @param {Record<string, string>} prefixes - Namespace prefixes (e.g. { foaf: 'http://xmlns.com/foaf/0.1/' })
   * @param {string[]} classes - Known class URIs
   * @param {string[]} properties - Known property URIs
   */
  setOntology(prefixes, classes, properties) {
    this.ontology = OntologyContextSchema.parse({
      prefixes: prefixes || {},
      classes: classes || [],
      properties: properties || [],
    });

    // Clear cache since ontology changed
    if (this.cache) this.cache.clear();
  }

  /**
   * Get builder statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      customMappingCount: this.customMappings.length,
      ontologyClasses: this.ontology.classes.length,
      ontologyProperties: this.ontology.properties.length,
      cacheSize: this.cache ? this.cache.size : 0,
    };
  }

  /**
   * Clear the query cache
   */
  clearCache() {
    if (this.cache) this.cache.clear();
  }

  // ─── Private Methods ──────────────────────────────────────────────────────

  /**
   * Build SPARQL for "find all X" intent
   * @param {Object} parsed - Parsed query
   * @param {string} prefixes - PREFIX block
   * @returns {string} SPARQL query
   * @private
   */
  _buildFindAll(parsed, prefixes) {
    const entityType = resolveToIRI(
      parsed.entityType || 'Thing',
      this.ontology,
      'class'
    );
    const typeRef = sparqlTerm(entityType);
    return `${prefixes}SELECT ?x WHERE {\n  ?x a ${typeRef} .\n}\nLIMIT ${this.config.defaultLimit}`;
  }

  /**
   * Build SPARQL for "what is the Y of X" intent
   * @param {Object} parsed - Parsed query
   * @param {string} prefixes - PREFIX block
   * @returns {string} SPARQL query
   * @private
   */
  _buildFindProperty(parsed, prefixes) {
    const property = resolveToIRI(
      parsed.property || 'label',
      this.ontology,
      'property'
    );
    const subject = resolveToIRI(
      parsed.subject || 'unknown',
      this.ontology,
      'class'
    );
    const propRef = sparqlTerm(property);
    const subjRef = sparqlTerm(subject);
    return `${prefixes}SELECT ?value WHERE {\n  ${subjRef} ${propRef} ?value .\n}\nLIMIT ${this.config.defaultLimit}`;
  }

  /**
   * Build SPARQL for "how many X" intent
   * @param {Object} parsed - Parsed query
   * @param {string} prefixes - PREFIX block
   * @returns {string} SPARQL query
   * @private
   */
  _buildCount(parsed, prefixes) {
    const entityType = resolveToIRI(
      parsed.entityType || 'Thing',
      this.ontology,
      'class'
    );
    const typeRef = sparqlTerm(entityType);
    return `${prefixes}SELECT (COUNT(?x) AS ?count) WHERE {\n  ?x a ${typeRef} .\n}`;
  }

  /**
   * Build SPARQL for filtered queries
   * @param {Object} parsed - Parsed query
   * @param {string} prefixes - PREFIX block
   * @returns {string} SPARQL query
   * @private
   */
  _buildFilter(parsed, prefixes) {
    const entityType = resolveToIRI(
      parsed.entityType || 'Thing',
      this.ontology,
      'class'
    );
    const typeRef = sparqlTerm(entityType);
    let body = `  ?x a ${typeRef} .\n`;

    for (let i = 0; i < parsed.filters.length; i++) {
      const filter = parsed.filters[i];
      const prop = resolveToIRI(filter.property, this.ontology, 'property');
      const propRef = sparqlTerm(prop);
      const varName = `?val${i}`;

      body += `  ?x ${propRef} ${varName} .\n`;

      if (filter.operator === 'contains') {
        body += `  FILTER(CONTAINS(STR(${varName}), "${filter.value.replace(/"/g, '\\"')}"))\n`;
      } else if (filter.operator === '=') {
        body += `  FILTER(${varName} = ${sparqlTerm(filter.value)})\n`;
      } else {
        body += `  FILTER(${varName} ${filter.operator} ${sparqlTerm(filter.value)})\n`;
      }
    }

    return `${prefixes}SELECT ?x WHERE {\n${body}}\nLIMIT ${this.config.defaultLimit}`;
  }

  /**
   * Build SPARQL DESCRIBE query
   * @param {Object} parsed - Parsed query
   * @param {string} prefixes - PREFIX block
   * @returns {string} SPARQL query
   * @private
   */
  _buildDescribe(parsed, prefixes) {
    const subject = resolveToIRI(
      parsed.subject || 'unknown',
      this.ontology,
      'class'
    );
    const subjRef = sparqlTerm(subject);
    return `${prefixes}SELECT ?p ?o WHERE {\n  ${subjRef} ?p ?o .\n}\nLIMIT ${this.config.defaultLimit}`;
  }

  /**
   * Build SPARQL ASK query
   * @param {Object} parsed - Parsed query
   * @param {string} prefixes - PREFIX block
   * @returns {string} SPARQL query
   * @private
   */
  _buildAsk(parsed, prefixes) {
    const subject = resolveToIRI(
      parsed.subject || 'unknown',
      this.ontology,
      'class'
    );
    const entityType = resolveToIRI(
      parsed.entityType || 'Thing',
      this.ontology,
      'class'
    );
    const subjRef = sparqlTerm(subject);
    const typeRef = sparqlTerm(entityType);
    return `${prefixes}ASK {\n  ${subjRef} a ${typeRef} .\n}`;
  }

  /**
   * Build SPARQL from custom mapping template
   * @param {Object} parsed - Parsed query
   * @param {string} prefixes - PREFIX block
   * @returns {string} SPARQL query
   * @private
   */
  _buildCustom(parsed, prefixes) {
    // Try to match against custom mappings for the template
    for (const mapping of this.customMappings) {
      const regex = mapping.pattern instanceof RegExp
        ? mapping.pattern
        : new RegExp(mapping.pattern, 'i');
      const match = parsed.originalQuery.match(regex);
      if (match) {
        let sparql = mapping.template;
        for (let i = 1; i < match.length; i++) {
          sparql = sparql.replace(new RegExp(`\\$${i}`, 'g'), match[i]);
        }
        return `${prefixes}${sparql}`;
      }
    }
    return this._buildFallback(parsed, prefixes);
  }

  /**
   * Build fallback SPARQL for unknown queries
   * @param {Object} parsed - Parsed query
   * @param {string} prefixes - PREFIX block
   * @returns {string} SPARQL query
   * @private
   */
  _buildFallback(parsed, prefixes) {
    // Generic triple pattern match using keywords from the query
    const words = parsed.originalQuery.split(/\s+/).filter((w) => w.length > 2);
    const filterParts = words
      .map((w) => `CONTAINS(LCASE(STR(?s)), "${w.toLowerCase()}")`)
      .join(' || ');
    const filter = filterParts ? `\n  FILTER(${filterParts})` : '';
    return `${prefixes}SELECT ?s ?p ?o WHERE {\n  ?s ?p ?o .${filter}\n}\nLIMIT ${this.config.defaultLimit}`;
  }

  /**
   * Get default suggestions when no partial query is provided
   * @returns {Array} Default suggestions
   * @private
   */
  _getDefaultSuggestions() {
    const suggestions = [];
    for (const cls of this.ontology.classes.slice(0, 3)) {
      const localName = cls.includes('#')
        ? cls.split('#').pop()
        : cls.split('/').pop();
      suggestions.push(SuggestionSchema.parse({
        text: `find all ${localName}`,
        sparql: `SELECT ?x WHERE { ?x a <${cls}> . } LIMIT 100`,
        confidence: 0.5,
      }));
    }
    return suggestions;
  }

  /**
   * Get suggestion templates for auto-completion
   * @returns {Array<{prefix: string, template: string}>} Templates
   * @private
   */
  _getSuggestionTemplates() {
    return [
      { prefix: 'find', template: 'find all {class}' },
      { prefix: 'list', template: 'list all {class}' },
      { prefix: 'how', template: 'how many {class}' },
      { prefix: 'count', template: 'count {class}' },
      { prefix: 'show', template: 'show all {class}' },
      { prefix: 'describe', template: 'describe {class}' },
    ];
  }
}

/**
 * Create an NLP query builder instance
 * @param {Object} [config] - Configuration
 * @returns {NLPQueryBuilder} NLP query builder
 * @example
 * const builder = createNLPQueryBuilder({ defaultLimit: 50 });
 */
export function createNLPQueryBuilder(config = {}) {
  return new NLPQueryBuilder(config);
}

/**
 * Default NLP query builder instance
 */
export const defaultNLPQueryBuilder = createNLPQueryBuilder();

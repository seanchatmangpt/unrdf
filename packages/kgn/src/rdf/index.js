/**
 * @file RDF-Aware Template System Integration
 * @module @unrdf/kgn/rdf
 * @description Integration layer between @unrdf/kgn and @unrdf/core for RDF-aware templating
 */

import { z } from 'zod';
import nunjucks from 'nunjucks';
import {
  createStore,
  namedNode,
  literal as createLiteral,
  blankNode as createBlankNode,
  quad,
  executeQuery,
  COMMON_PREFIXES,
} from '@unrdf/core';
import { rdfTemplateFilters } from './filters.js';

/**
 * Schema for RDF template engine configuration
 */
const RdfTemplateEngineConfigSchema = z.object({
  autoescape: z.boolean().optional(),
  throwOnUndefined: z.boolean().optional(),
  trimBlocks: z.boolean().optional(),
  lstripBlocks: z.boolean().optional(),
  prefixes: z.record(z.string()).optional(),
  enableStore: z.boolean().optional(),
  storeOptions: z.any().optional(),
});

/**
 * Schema for RDF template rendering context
 */
const RdfRenderContextSchema = z.object({
  data: z.any(),
  prefixes: z.record(z.string()).optional(),
  store: z.any().optional(),
  baseUri: z.string().url().optional(),
});

/**
 * Schema for SPARQL template generation
 */
const SparqlTemplateSchema = z.object({
  prefixes: z.record(z.string()).optional(),
  where: z.array(z.object({
    subject: z.string(),
    predicate: z.string(),
    object: z.string(),
  })).min(1),
  select: z.array(z.string()).optional(),
  distinct: z.boolean().optional(),
  limit: z.number().int().positive().optional(),
  offset: z.number().int().nonnegative().optional(),
  orderBy: z.array(z.object({
    variable: z.string(),
    direction: z.enum(['ASC', 'DESC']).optional(),
  })).optional(),
});

/**
 * RDF-aware template engine
 * Extends Nunjucks with RDF-specific filters and functions
 */
export class RdfTemplateEngine {
  /**
   * Create RDF template engine
   * @param {Object} config - Engine configuration
   */
  constructor(config = {}) {
    this.config = RdfTemplateEngineConfigSchema.parse(config);

    // Apply defaults
    const {
      autoescape = false,
      throwOnUndefined = false,
      trimBlocks = true,
      lstripBlocks = true,
      prefixes = {},
      enableStore = true,
      storeOptions,
    } = this.config;

    // Create Nunjucks environment
    this.env = new nunjucks.Environment(null, {
      autoescape,
      throwOnUndefined,
      trimBlocks,
      lstripBlocks,
    });

    // Initialize RDF store if enabled
    this.store = enableStore ? createStore(storeOptions) : null;

    // Merge common prefixes with custom prefixes
    this.prefixes = { ...COMMON_PREFIXES, ...prefixes };

    // Register RDF filters
    this._registerFilters();

    // Register RDF functions
    this._registerGlobals();
  }

  /**
   * Register RDF template filters
   * @private
   */
  _registerFilters() {
    // Register all RDF template filters
    for (const [name, filter] of Object.entries(rdfTemplateFilters)) {
      this.env.addFilter(name, filter);
    }

    // Additional utility filters
    this.env.addFilter('expandUri', (curie) => this._expandUri(curie));
    this.env.addFilter('contractUri', (uri) => this._contractUri(uri));
    this.env.addFilter('toQuad', (triple) => this._toQuad(triple));
  }

  /**
   * Register global RDF functions
   * @private
   */
  _registerGlobals() {
    // Add RDF term constructors as globals
    this.env.addGlobal('namedNode', (uri) => namedNode(uri));
    this.env.addGlobal('literal', (value, langOrDatatype) => {
      if (!langOrDatatype) {
        return createLiteral(value);
      }
      // Check if it's a language tag
      if (/^[a-z]{2,3}(-[A-Z]{2})?$/.test(langOrDatatype)) {
        return createLiteral(value, langOrDatatype);
      }
      // Treat as datatype
      return createLiteral(value, null, namedNode(langOrDatatype));
    });
    this.env.addGlobal('blankNode', (id) => createBlankNode(id));
    this.env.addGlobal('quad', (s, p, o, g) => quad(s, p, o, g));

    // Add prefix expansion
    this.env.addGlobal('expand', (curie) => this._expandUri(curie));
    this.env.addGlobal('contract', (uri) => this._contractUri(uri));

    // Add SPARQL query builder
    this.env.addGlobal('sparqlQuery', (spec) => this._buildSparqlQuery(spec));

    // Add store access (if enabled)
    if (this.store) {
      this.env.addGlobal('queryStore', async (query) => {
        return await executeQuery(this.store, query);
      });
    }
  }

  /**
   * Expand CURIE to full URI
   * @private
   * @param {string} curie - CURIE (prefix:localName)
   * @returns {string} Full URI
   */
  _expandUri(curie) {
    if (!curie || typeof curie !== 'string') {
      return '';
    }

    const colonIndex = curie.indexOf(':');
    if (colonIndex === -1) {
      return curie;
    }

    const prefix = curie.substring(0, colonIndex);
    const localName = curie.substring(colonIndex + 1);

    if (this.prefixes[prefix]) {
      return this.prefixes[prefix] + localName;
    }

    return curie;
  }

  /**
   * Contract URI to CURIE
   * @private
   * @param {string} uri - Full URI
   * @returns {string} CURIE or original URI
   */
  _contractUri(uri) {
    if (!uri || typeof uri !== 'string') {
      return '';
    }

    for (const [prefix, namespace] of Object.entries(this.prefixes)) {
      if (uri.startsWith(namespace)) {
        const localName = uri.substring(namespace.length);
        return `${prefix}:${localName}`;
      }
    }

    return uri;
  }

  /**
   * Convert triple object to quad
   * @private
   * @param {Object} triple - Triple object
   * @returns {Object} Quad object
   */
  _toQuad(triple) {
    if (!triple || typeof triple !== 'object') {
      return null;
    }

    const subject = this._createTerm(triple.subject || triple.s);
    const predicate = this._createTerm(triple.predicate || triple.p);
    const object = this._createTerm(triple.object || triple.o);

    return quad(subject, predicate, object);
  }

  /**
   * Create RDF term from string or object
   * @private
   * @param {string|Object} term - Term specification
   * @returns {Object} RDF term
   */
  _createTerm(term) {
    if (!term) {
      return null;
    }

    if (typeof term === 'object' && term.termType) {
      return term;
    }

    if (typeof term === 'string') {
      // Blank node
      if (term.startsWith('_:')) {
        return createBlankNode(term.substring(2));
      }
      // URI
      if (term.startsWith('http://') || term.startsWith('https://')) {
        return namedNode(term);
      }
      // CURIE
      if (term.includes(':')) {
        return namedNode(this._expandUri(term));
      }
      // Literal
      return createLiteral(term);
    }

    return createLiteral(String(term));
  }

  /**
   * Build SPARQL query from template specification
   * @private
   * @param {Object} spec - Query specification
   * @returns {string} SPARQL query
   */
  _buildSparqlQuery(spec) {
    const validated = SparqlTemplateSchema.parse(spec);

    // Apply defaults
    const {
      prefixes = {},
      where,
      select,
      distinct = false,
      limit,
      offset = 0,
      orderBy,
    } = validated;

    const lines = [];

    // Prefixes
    const queryPrefixes = { ...this.prefixes, ...prefixes };
    for (const [prefix, uri] of Object.entries(queryPrefixes)) {
      lines.push(`PREFIX ${prefix}: <${uri}>`);
    }
    lines.push('');

    // SELECT clause
    const selectVars = select || ['*'];
    const distinctStr = distinct ? 'DISTINCT ' : '';
    lines.push(`SELECT ${distinctStr}${selectVars.join(' ')}`);

    // WHERE clause
    lines.push('WHERE {');
    for (const pattern of where) {
      lines.push(`  ${pattern.subject} ${pattern.predicate} ${pattern.object} .`);
    }
    lines.push('}');

    // ORDER BY
    if (orderBy && orderBy.length > 0) {
      const orderClauses = orderBy.map(
        (o) => `${o.direction || 'ASC'}(${o.variable})`
      );
      lines.push(`ORDER BY ${orderClauses.join(' ')}`);
    }

    // LIMIT and OFFSET
    if (limit) {
      lines.push(`LIMIT ${limit}`);
    }
    if (offset > 0) {
      lines.push(`OFFSET ${offset}`);
    }

    return lines.join('\n');
  }

  /**
   * Render template with RDF context
   * @param {string} template - Template string
   * @param {Object} context - Rendering context
   * @returns {string} Rendered output
   */
  render(template, context = {}) {
    const validated = RdfRenderContextSchema.parse({
      data: context,
      prefixes: context.prefixes,
      store: context.store || this.store,
      baseUri: context.baseUri,
    });

    // Merge context with RDF utilities
    const renderContext = {
      ...validated.data,
      prefixes: { ...this.prefixes, ...validated.prefixes },
      store: validated.store,
      baseUri: validated.baseUri,
    };

    return this.env.renderString(template, renderContext);
  }

  /**
   * Render template from file
   * @param {string} templatePath - Path to template file
   * @param {Object} context - Rendering context
   * @returns {string} Rendered output
   */
  renderFile(templatePath, context = {}) {
    const validated = RdfRenderContextSchema.parse({
      data: context,
      prefixes: context.prefixes,
      store: context.store || this.store,
      baseUri: context.baseUri,
    });

    const renderContext = {
      ...validated.data,
      prefixes: { ...this.prefixes, ...validated.prefixes },
      store: validated.store,
      baseUri: validated.baseUri,
    };

    return this.env.render(templatePath, renderContext);
  }

  /**
   * Add custom prefix mapping
   * @param {string} prefix - Prefix name
   * @param {string} uri - Namespace URI
   */
  addPrefix(prefix, uri) {
    z.string().min(1).parse(prefix);
    z.string().url().parse(uri);

    this.prefixes[prefix] = uri;
  }

  /**
   * Get RDF store instance
   * @returns {Object|null} RDF store or null if disabled
   */
  getStore() {
    return this.store;
  }

  /**
   * Get Nunjucks environment
   * @returns {Object} Nunjucks environment
   */
  getEnvironment() {
    return this.env;
  }
}

/**
 * Create RDF template engine instance
 * @param {Object} config - Engine configuration
 * @returns {RdfTemplateEngine} RDF template engine
 */
export function createRdfTemplateEngine(config = {}) {
  return new RdfTemplateEngine(config);
}

/**
 * Render RDF template (convenience function)
 * @param {string} template - Template string
 * @param {Object} context - Rendering context
 * @param {Object} config - Engine configuration
 * @returns {string} Rendered output
 */
export function renderRdfTemplate(template, context = {}, config = {}) {
  const engine = new RdfTemplateEngine(config);
  return engine.render(template, context);
}

/**
 * Export RDF template filters
 */
export {
  rdfTemplateFilters,
  toTurtle,
  toSparql,
  rdfPrefix,
  blankNode,
  literal,
} from './filters.js';

/**
 * Export schemas for external validation
 */
export {
  RdfTemplateEngineConfigSchema,
  RdfRenderContextSchema,
  SparqlTemplateSchema,
};

export default {
  RdfTemplateEngine,
  createRdfTemplateEngine,
  renderRdfTemplate,
  rdfTemplateFilters,
};

/**
 * @file RDF/Turtle template loading integration layer
 * @module @unrdf/cli/lib/rdf-template-loader
 *
 * Loads Turtle files into UnrdfStore (Oxigraph), executes SPARQL SELECT
 * queries, and creates Nunjucks-ready context objects from the results.
 *
 * CONSTRAINT: Never import from 'n3' directly. Use @unrdf/core only.
 */

import { readFile } from 'node:fs/promises';
import { createStore } from '@unrdf/core';
import { executeSelectSync } from '@unrdf/core/sparql/executor-sync';
import { Parser } from '@unrdf/core/rdf/n3-justified-only';
import { COMMON_PREFIXES } from '@unrdf/core';
import { detectRdfParserFormat } from './rdf-format-detect.mjs';

/**
 * Extract prefix declarations from raw Turtle text using regex.
 * Does NOT use N3. Handles both @prefix and PREFIX directive forms.
 * @param {string} turtleText
 * @returns {Record<string, string>} Map of prefix → IRI
 */
export function extractPrefixesFromTurtle(turtleText) {
  const prefixes = {};
  const lines = turtleText.split('\n');

  for (const line of lines) {
    // Match @prefix foaf: <http://...> . or PREFIX foaf: <http://...>
    const match = line.match(/^(?:@)?[Pp][Rr][Ee][Ff][Ii][Xx]\s+(\w+):\s*<([^>]+)>/);
    if (match) {
      prefixes[match[1]] = match[2];
    }
  }

  return prefixes;
}

/**
 * Convert an xsd datatype to native JS value.
 * @param {string} datatype - Full IRI or xsd: prefixed
 * @param {string} value - Literal value
 * @returns {any}
 */
function coerceXsdType(datatype, value) {
  if (!datatype) return value;
  const xsdTypes = {
    'http://www.w3.org/2001/XMLSchema#integer': () => parseInt(value, 10),
    'http://www.w3.org/2001/XMLSchema#decimal': () => parseFloat(value),
    'http://www.w3.org/2001/XMLSchema#double': () => parseFloat(value),
    'http://www.w3.org/2001/XMLSchema#float': () => parseFloat(value),
    'http://www.w3.org/2001/XMLSchema#boolean': () => value === 'true',
    'http://www.w3.org/2001/XMLSchema#date': () => new Date(value),
    'http://www.w3.org/2001/XMLSchema#dateTime': () => new Date(value),
  };

  const converter = xsdTypes[datatype];
  return converter ? converter() : value;
}

/**
 * Convert a SPARQL SELECT binding row to flat JS values.
 * Coerces xsd datatypes to native JS types.
 * @param {Record<string, {type: string, value: string, datatype?: string, language?: string}>} binding
 * @returns {Record<string, any>}
 */
export function bindingRowToContext(binding) {
  const result = {};

  for (const [varName, term] of Object.entries(binding)) {
    if (term.language) {
      result[varName] = { value: term.value, lang: term.language };
    } else if (term.datatype) {
      result[varName] = coerceXsdType(term.datatype, term.value);
    } else {
      result[varName] = term.value;
    }
  }

  return result;
}

/**
 * Get the local name from a full URI.
 * Splits on '#' first, then last '/'.
 * @param {string} uri
 * @returns {string}
 */
export function localName(uri) {
  if (!uri) return '';
  const hash = uri.lastIndexOf('#');
  if (hash !== -1) return uri.substring(hash + 1);
  const slash = uri.lastIndexOf('/');
  return slash !== -1 ? uri.substring(slash + 1) : uri;
}

/**
 * Convert a URI local name to camelCase key suitable for JS object props.
 * @param {string} name
 * @returns {string}
 */
function toCamelCase(name) {
  return name.replace(/-([a-z])/g, (_, char) => char.toUpperCase());
}

/**
 * Build a Nunjucks-safe key from a full predicate URI.
 * Returns the camelCased local name.
 * @param {string} predicateUri
 * @returns {string}
 */
export function predicateToKey(predicateUri) {
  return toCamelCase(localName(predicateUri));
}

/**
 * Expand a prefixed URI using a prefix map.
 * Falls back to COMMON_PREFIXES from @unrdf/core.
 * @param {string} prefixedUri - e.g. 'foaf:Person'
 * @param {Record<string, string>} [extraPrefixes]
 * @returns {string} Full IRI
 */
export function expandPrefixedUri(prefixedUri, extraPrefixes = {}) {
  if (!prefixedUri || !prefixedUri.includes(':')) return prefixedUri;

  const [prefix, localPart] = prefixedUri.split(':', 2);
  const allPrefixes = { ...COMMON_PREFIXES, ...extraPrefixes };
  const ns = allPrefixes[prefix];

  return ns ? ns + localPart : prefixedUri;
}

/**
 * RDF/Turtle template context loader.
 * Loads TTL files, executes SPARQL queries, builds Nunjucks context.
 */
export class RdfTemplateLoader {
  /**
   * @param {Object} [options]
   * @param {string} [options.baseIri='http://example.org/']
   * @param {boolean} [options.cacheEnabled=true]
   * @param {number} [options.maxCacheSize=50]
   */
  constructor(options = {}) {
    this.baseIri = options.baseIri || 'http://example.org/';
    this.cacheEnabled = options.cacheEnabled !== false;
    this.maxCacheSize = options.maxCacheSize || 50;
    this._storeCache = new Map();
  }

  /**
   * Load RDF from file path (format from extension) → store
   * @param {string} filePath
   * @returns {Promise<Object>}
   */
  async loadFromFile(filePath) {
    const text = await readFile(filePath, 'utf8');
    const format = detectRdfParserFormat(filePath);
    return this.loadFromText(text, undefined, format);
  }

  /**
   * Load RDF from inline string → store
   * @param {string} rdfText
   * @param {string} [baseIri]
   * @param {string} [format='Turtle'] - N3 Parser format name
   * @returns {Object}
   */
  loadFromText(rdfText, baseIri, format = 'Turtle') {
    const store = createStore();
    const parser = new Parser({ format });

    return new Promise((resolve, reject) => {
      parser.parse(rdfText, (error, quad) => {
        if (error) {
          reject(error);
          return;
        }
        if (quad) {
          store.add(quad);
        } else {
          // Parsing complete
          resolve(store);
        }
      });
    });
  }

  /**
   * Load from { type: 'file'|'inline'|'uri', path?, content?, uri? }
   * @param {Object} source
   * @returns {Promise<Object>}
   */
  async loadFromSource(source) {
    if (!source || typeof source !== 'object') {
      throw new Error('loadFromSource requires an object with type, and path/content/uri');
    }

    if (source.type === 'file') {
      if (!source.path) throw new Error('file source requires path');
      return this.loadFromFile(source.path);
    }

    if (source.type === 'inline') {
      if (!source.content) throw new Error('inline source requires content');
      return this.loadFromText(source.content, source.baseUri);
    }

    if (source.type === 'uri') {
      if (!source.uri) throw new Error('uri source requires uri');
      throw new Error('URI loading not yet implemented');
    }

    throw new Error(`Unknown RDF source type: ${source.type}`);
  }

  /**
   * Load sources described in frontmatter → merged UnrdfStore
   * Handles frontmatter.rdf, frontmatter.turtle, frontmatter.sparql
   * @param {Object} frontmatter
   * @returns {Promise<{store: UnrdfStore|null, context: Object|null, pendingSparql: string|null}>}
   */
  async loadFromFrontmatter(frontmatter) {
    if (!frontmatter) return { store: null, context: null, pendingSparql: null };

    const rdfConfig = frontmatter.rdf || frontmatter.turtle || frontmatter.turtleData;
    const sparqlQuery = frontmatter.sparql;

    let store = null;
    let context = null;

    // Load RDF if present
    if (rdfConfig) {
      if (typeof rdfConfig === 'string') {
        // Simple string path
        store = await this.loadFromFile(rdfConfig);
      } else if (typeof rdfConfig === 'object') {
        // Full source config { type, path?, content?, uri? }
        store = await this.loadFromSource(rdfConfig);
      }

      // Execute SPARQL if both RDF and SPARQL present
      if (store && sparqlQuery) {
        context = this.queryToContext(store, sparqlQuery);
      }
    }

    return {
      store,
      context,
      pendingSparql: sparqlQuery && !store ? sparqlQuery : null,
    };
  }

  /**
   * Execute SPARQL SELECT on loaded store → flat context vars
   * Returns { flat vars from first row, $rdf: { subjects, raw } }
   * @param {UnrdfStore} store
   * @param {string} sparqlQuery
   * @returns {Object}
   */
  queryToContext(store, sparqlQuery) {
    const results = executeSelectSync(store, sparqlQuery);

    if (!results || results.length === 0) {
      return { $rdf: { subjects: {}, raw: [], getByType: () => [] } };
    }

    // Track multi-value vars
    const varCounts = {};
    for (const row of results) {
      for (const varName of Object.keys(row)) {
        varCounts[varName] = (varCounts[varName] || 0) + 1;
      }
    }

    // Flat vars from all rows
    const flatVars = {};
    for (const row of results) {
      for (const [varName, term] of Object.entries(row)) {
        const jsValue = term.language
          ? { value: term.value, lang: term.language }
          : coerceXsdType(term.datatype, term.value);

        if (varCounts[varName] === 1) {
          flatVars[varName] = jsValue;
        } else {
          if (!Array.isArray(flatVars[varName])) {
            flatVars[varName] = [];
          }
          flatVars[varName].push(jsValue);
        }
      }
    }

    // Raw bindings
    const raw = results.map(row => bindingRowToContext(row));

    return {
      ...flatVars,
      $rdf: {
        subjects: {},
        raw,
        getByType: () => [],
      },
    };
  }

  /**
   * Build full Nunjucks context from store
   * @param {UnrdfStore} store
   * @param {string} [sparqlQuery] - Optional SPARQL to extract specific vars
   * @param {string} [subjectUri] - Optional subject to focus on
   * @returns {Object}
   */
  createTemplateContext(store, sparqlQuery, subjectUri) {
    if (!sparqlQuery) {
      // Return generic context with all subjects
      return { $rdf: { subjects: {}, raw: [] } };
    }

    // Run SPARQL and return context
    return this.queryToContext(store, sparqlQuery);
  }

  /**
   * Find all instances of a class for batch generation
   * @param {UnrdfStore} store
   * @param {string} classUri - Full IRI or prefixed form
   * @returns {string[]} Array of subject URIs
   */
  findInstancesOfClass(store, classUri) {
    const expandedClass = expandPrefixedUri(classUri);
    const sparql = `SELECT DISTINCT ?subject WHERE { ?subject <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <${expandedClass}> } ORDER BY ?subject`;

    const results = executeSelectSync(store, sparql);
    return results.map(row => row.subject.value);
  }

  /**
   * Build per-instance context for batch mode
   * @param {UnrdfStore} store
   * @param {string} subjectUri
   * @param {string} [templateSparql] - Optional SPARQL with ?subject placeholder
   * @returns {Object}
   */
  createInstanceContext(store, subjectUri, templateSparql) {
    if (!templateSparql) {
      // Generic: fetch all properties of subject
      const sparql = `SELECT ?predicate ?object WHERE { <${subjectUri}> ?predicate ?object }`;
      return this.queryToContext(store, sparql);
    }

    // Substitute ?subject placeholder
    const query = templateSparql.replace(/\?subject/g, `<${subjectUri}>`);
    return this.queryToContext(store, query);
  }

  /**
   * Find and build per-instance contexts for batch generation
   * @param {UnrdfStore} store
   * @param {string} classUri - Class IRI
   * @param {string} [templateSparql] - SPARQL with ?subject placeholder
   * @returns {Array<{subjectUri: string, context: Object}>}
   */
  findAndBuildBatchContexts(store, classUri, templateSparql) {
    const subjects = this.findInstancesOfClass(store, classUri);
    return subjects.map(subjectUri => ({
      subjectUri,
      context: this.createInstanceContext(store, subjectUri, templateSparql),
    }));
  }

  /**
   *
   */
  destroy() {
    this._storeCache.clear();
  }
}

/**
 * Factory function
 * @param {Object} [options]
 * @returns {RdfTemplateLoader}
 */
export function createRdfTemplateLoader(options) {
  return new RdfTemplateLoader(options);
}

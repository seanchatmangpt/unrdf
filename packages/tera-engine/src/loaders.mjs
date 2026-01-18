/**
 * @file TOML and RDF Data Loaders
 * @module @unrdf/tera-engine/loaders
 * @description Loaders for TOML configuration and RDF/Turtle data
 */

import { readFile } from 'fs/promises';
import { parse as parseToml } from 'toml';
import { validateTomlOptions, validateRdfOptions, validateSparqlOptions } from './schemas.mjs';

/**
 * Loads TOML file or content
 * @param {Object} options - Loader options
 * @param {string} [options.path] - Path to TOML file
 * @param {string} [options.content] - TOML content string
 * @returns {Promise<Record<string, any>>} Parsed TOML data
 * @throws {Error} If TOML parsing fails
 * @example
 * const data = await loadToml({ path: 'config.toml' });
 * const data2 = await loadToml({ content: 'key = "value"' });
 */
export async function loadToml(options) {
  const validOptions = validateTomlOptions(options);

  try {
    let content;

    if (validOptions.path) {
      content = await readFile(validOptions.path, 'utf-8');
    } else {
      content = validOptions.content;
    }

    return parseToml(content);
  } catch (error) {
    throw new Error(`Failed to parse TOML: ${error.message}`);
  }
}

/**
 * Loads TOML synchronously
 * @param {string} content - TOML content string
 * @returns {Record<string, any>} Parsed TOML data
 * @throws {Error} If TOML parsing fails
 * @example
 * const data = loadTomlSync('[section]\nkey = "value"');
 */
export function loadTomlSync(content) {
  try {
    return parseToml(content);
  } catch (error) {
    throw new Error(`Failed to parse TOML: ${error.message}`);
  }
}

/**
 * Loads RDF/Turtle file or content into a store
 * @param {Object} options - Loader options
 * @param {string} [options.path] - Path to RDF file
 * @param {string} [options.content] - RDF content string
 * @param {string} [options.format='turtle'] - RDF format
 * @param {string} [options.baseIRI] - Base IRI for relative URIs
 * @returns {Promise<Object>} Object with store and triples
 * @throws {Error} If RDF parsing fails
 * @example
 * const { store, triples } = await loadRdf({
 *   path: 'data.ttl',
 *   format: 'turtle'
 * });
 */
export async function loadRdf(options) {
  const validOptions = validateRdfOptions(options);

  try {
    // Dynamic import to avoid hard dependency on @unrdf/oxigraph
    const { createStore } = await import('@unrdf/oxigraph');

    let content;

    if (validOptions.path) {
      content = await readFile(validOptions.path, 'utf-8');
    } else {
      content = validOptions.content;
    }

    const store = createStore();

    // Load into store (Oxigraph supports multiple formats)
    store.load(content, {
      format: validOptions.format,
      baseIRI: validOptions.baseIRI,
    });

    // Extract triples from store
    const triples = [];
    for (const quad of store.match(null, null, null)) {
      triples.push({
        subject: quad.subject,
        predicate: quad.predicate,
        object: quad.object,
        graph: quad.graph,
      });
    }

    return { store, triples };
  } catch (error) {
    throw new Error(`Failed to load RDF: ${error.message}`);
  }
}

/**
 * Executes SPARQL query on RDF store
 * @param {Object} store - Oxigraph store instance
 * @param {Object} options - Query options
 * @param {string} options.query - SPARQL query string
 * @param {string} [options.format='simple'] - Result format
 * @returns {Array|Object} Query results
 * @throws {Error} If query execution fails
 * @example
 * const results = querySparql(store, {
 *   query: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10'
 * });
 */
export function querySparql(store, options) {
  const validOptions = validateSparqlOptions(options);

  try {
    const results = store.query(validOptions.query);

    if (validOptions.format === 'simple') {
      // Convert to simple array of objects
      if (Array.isArray(results)) {
        return results.map(binding => {
          const obj = {};
          for (const [key, value] of Object.entries(binding)) {
            obj[key] = value.value;
          }
          return obj;
        });
      }
      return results;
    }

    if (validOptions.format === 'bindings') {
      // Return raw bindings
      return results;
    }

    // JSON format
    return results;
  } catch (error) {
    throw new Error(`SPARQL query failed: ${error.message}`);
  }
}

/**
 * Loads RDF and converts to simple context object
 * @param {Object} options - Loader options
 * @returns {Promise<Object>} Context object with triples and query helper
 * @example
 * const context = await loadRdfContext({ path: 'data.ttl' });
 * // context.triples - array of triples
 * // context.query(sparql) - run SPARQL query
 */
export async function loadRdfContext(options) {
  const { store, triples } = await loadRdf(options);

  return {
    triples,
    store,
    query: (sparqlQuery, format = 'simple') => {
      return querySparql(store, { query: sparqlQuery, format });
    },
    subjects: () => {
      const subjects = new Set();
      for (const triple of triples) {
        subjects.add(triple.subject.value);
      }
      return Array.from(subjects);
    },
    predicates: () => {
      const predicates = new Set();
      for (const triple of triples) {
        predicates.add(triple.predicate.value);
      }
      return Array.from(predicates);
    },
    objects: () => {
      const objects = new Set();
      for (const triple of triples) {
        objects.add(triple.object.value);
      }
      return Array.from(objects);
    },
  };
}

/**
 * Merges TOML and RDF contexts for templates
 * @param {Object} options - Merge options
 * @param {Object} [options.toml] - TOML loader options
 * @param {Object} [options.rdf] - RDF loader options
 * @returns {Promise<Object>} Merged context
 * @example
 * const context = await mergeContexts({
 *   toml: { path: 'config.toml' },
 *   rdf: { path: 'data.ttl' }
 * });
 */
export async function mergeContexts(options = {}) {
  const context = {};

  if (options.toml) {
    const tomlData = await loadToml(options.toml);
    Object.assign(context, tomlData);
  }

  if (options.rdf) {
    const rdfData = await loadRdfContext(options.rdf);
    context.rdf = rdfData;
  }

  return context;
}

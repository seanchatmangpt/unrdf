/**
 * @file knowledge-engine/lite.mjs
 * @description Minimal RDF functionality entry point for bundle size optimization
 *
 * This lite bundle exports ONLY the core RDF primitives from n3:
 * - Store: In-memory quad storage
 * - Parser: Turtle/N-Triples/N-Quads parsing
 * - Writer: RDF serialization
 * - DataFactory: RDF term creation
 *
 * EXCLUDES (for 60%+ bundle reduction):
 * - @comunica/query-sparql (~2.5MB)
 * - isolated-vm (~5MB native bindings)
 * - testcontainers (dev-only)
 * - eyereasoner
 * - rdf-validate-shacl
 * - All hook/policy infrastructure
 *
 * @example
 * // Minimal import for basic RDF operations
 * import { Store, Parser, Writer, DataFactory, parseTurtle, toTurtle } from 'unrdf/knowledge-engine/lite';
 *
 * const store = new Store();
 * const { namedNode, literal, quad } = DataFactory;
 *
 * store.addQuad(quad(
 *   namedNode('http://example.org/alice'),
 *   namedNode('http://xmlns.com/foaf/0.1/name'),
 *   literal('Alice')
 * ));
 *
 * @module knowledge-engine/lite
 * @see {@link https://github.com/rdfjs/N3.js} for N3.js documentation
 */

/**
 * JUSTIFIED N3 USAGE (μ(O) Compliance):
 * This lite.mjs module is a justified boundary for N3 exports because:
 * 1. It provides streaming parsing with backpressure control (N3.Parser)
 * 2. It's explicitly designed for lightweight RDF operations without Oxigraph
 * 3. Users opting into lite.mjs accept N3-based implementation
 *
 * For full Oxigraph-based functionality, use the main knowledge-engine exports.
 */

// Core N3 exports - the essential RDF primitives
export { Store, Parser, Writer, DataFactory } from 'n3';

/**
 * Parse a Turtle string into a Store (lite version - no OTEL tracing)
 * @param {string} ttl - The Turtle string to parse
 * @param {string} [baseIRI='http://example.org/'] - Base IRI for resolving relative URIs
 * @returns {Promise<import('n3').Store>} Promise resolving to a Store containing the parsed quads
 * @throws {TypeError} If ttl is not a string
 * @throws {Error} If parsing fails
 *
 * @example
 * const ttl = `
 *   @prefix ex: <http://example.org/> .
 *   ex:alice ex:knows ex:bob .
 * `;
 * const store = await parseTurtle(ttl);
 */
export async function parseTurtle(ttl, baseIRI = 'http://example.org/') {
  if (typeof ttl !== 'string') {
    throw new TypeError('parseTurtle: ttl must be a string');
  }
  if (typeof baseIRI !== 'string') {
    throw new TypeError('parseTurtle: baseIRI must be a string');
  }

  const { Parser, Store } = await import('n3');
  const parser = new Parser({ baseIRI });
  const quads = parser.parse(ttl);
  return new Store(quads);
}

/**
 * Serialize a Store to Turtle format (lite version - no OTEL tracing)
 * @param {import('n3').Store} store - The store to serialize
 * @param {Object} [options={}] - Serialization options
 * @param {Object} [options.prefixes] - Prefix mappings for compact output
 * @param {string} [options.baseIRI] - Base IRI for the output
 * @returns {Promise<string>} Promise resolving to the Turtle string
 * @throws {TypeError} If store is not a valid Store instance
 * @throws {Error} If serialization fails
 *
 * @example
 * const turtle = await toTurtle(store, {
 *   prefixes: { ex: 'http://example.org/' }
 * });
 */
export async function toTurtle(store, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('toTurtle: store must be a valid Store instance');
  }

  const { Writer } = await import('n3');
  const writer = new Writer({
    format: 'Turtle',
    prefixes: options.prefixes || {},
  });

  const quads = store.getQuads();
  writer.addQuads(quads);

  return new Promise((resolve, reject) => {
    writer.end((error, result) => {
      if (error) {
        reject(new Error(`Failed to serialize to Turtle: ${error.message}`));
      } else {
        if (options.baseIRI) {
          result = `@base <${options.baseIRI}> .\n\n${result}`;
        }
        resolve(result);
      }
    });
  });
}

/**
 * Serialize a Store to N-Quads format (lite version - no OTEL tracing)
 * @param {import('n3').Store} store - The store to serialize
 * @param {Object} [options={}] - Serialization options
 * @returns {Promise<string>} Promise resolving to the N-Quads string
 * @throws {TypeError} If store is not a valid Store instance
 * @throws {Error} If serialization fails
 *
 * @example
 * const nquads = await toNQuads(store);
 */
export async function toNQuads(store, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('toNQuads: store must be a valid Store instance');
  }

  const { Writer } = await import('n3');
  const writer = new Writer({
    format: 'N-Quads',
    ...options,
  });

  const quads = store.getQuads();
  writer.addQuads(quads);

  return new Promise((resolve, reject) => {
    writer.end((error, result) => {
      if (error) {
        reject(new Error(`Failed to serialize to N-Quads: ${error.message}`));
      } else {
        resolve(result);
      }
    });
  });
}

/**
 * Create a quad using DataFactory (convenience function)
 * @param {string} subject - Subject IRI
 * @param {string} predicate - Predicate IRI
 * @param {string|{value: string, language?: string, datatype?: string}} object - Object value
 * @param {string} [graph] - Optional graph IRI
 * @returns {import('n3').Quad} The created quad
 *
 * @example
 * const q = createQuad(
 *   'http://example.org/alice',
 *   'http://xmlns.com/foaf/0.1/name',
 *   { value: 'Alice', language: 'en' }
 * );
 */
export function createQuad(subject, predicate, object, graph) {
  const { DataFactory } = require('n3');
  const { namedNode, literal, quad, defaultGraph } = DataFactory;

  const subjectNode = namedNode(subject);
  const predicateNode = namedNode(predicate);

  let objectNode;
  if (typeof object === 'string') {
    // Check if it looks like a URI
    if (
      object.startsWith('http://') ||
      object.startsWith('https://') ||
      object.startsWith('urn:')
    ) {
      objectNode = namedNode(object);
    } else {
      objectNode = literal(object);
    }
  } else if (typeof object === 'object') {
    if (object.language) {
      objectNode = literal(object.value, object.language);
    } else if (object.datatype) {
      objectNode = literal(object.value, namedNode(object.datatype));
    } else {
      objectNode = literal(object.value);
    }
  }

  const graphNode = graph ? namedNode(graph) : defaultGraph();
  return quad(subjectNode, predicateNode, objectNode, graphNode);
}

/**
 * Bundle size comparison (approximate):
 *
 * Full knowledge-engine: ~8-10MB
 * - @comunica/query-sparql: ~2.5MB
 * - isolated-vm: ~5MB (native)
 * - eyereasoner: ~500KB
 * - rdf-validate-shacl: ~200KB
 * - n3: ~150KB
 * - hooks/policy: ~100KB
 *
 * Lite knowledge-engine: ~150KB
 * - n3 only: ~150KB
 *
 * Savings: ~98% for basic RDF operations
 */

/**
 * @file rdf-utils.mjs
 * @description RDF utility functions for loading TTL, parsing quads, validating IRIs, and deterministic hashing
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { sha256 } from 'hash-wasm';
import { z } from 'zod';

/**
 * IRI validation schema (RFC 3987 compatible)
 * @type {z.ZodString}
 */
export const IriSchema = z
  .string()
  .min(1)
  .regex(
    /^[a-z][a-z0-9+.-]*:/i,
    'IRI must start with a valid scheme (e.g., http:, https:, urn:)'
  );

/**
 * Validate an IRI according to RFC 3987
 *
 * @param {string} iri - IRI to validate
 * @returns {boolean} True if valid IRI
 *
 * @example
 * ```javascript
 * validateIri('http://example.org/resource') // => true
 * validateIri('not-a-valid-iri') // => false
 * ```
 */
export function validateIri(iri) {
  try {
    IriSchema.parse(iri);
    return true;
  } catch {
    return false;
  }
}

/**
 * Load Turtle/TTL content into an RDF store
 *
 * @param {string} turtleContent - Turtle format RDF content
 * @param {string} [baseIri='http://example.org/'] - Base IRI for resolving relative IRIs
 * @returns {import('@unrdf/oxigraph').OxigraphStore} Store with loaded quads
 * @throws {Error} If TTL parsing fails
 *
 * @example
 * ```javascript
 * const store = loadTurtle(`
 *   @prefix foaf: <http://xmlns.com/foaf/0.1/> .
 *   <http://example.org/alice> foaf:name "Alice" .
 * `);
 * console.log('Loaded', store.size, 'triples');
 * ```
 */
export function loadTurtle(turtleContent, baseIri = 'http://example.org/') {
  if (typeof turtleContent !== 'string' || !turtleContent.trim()) {
    throw new Error('Turtle content must be a non-empty string');
  }

  if (!validateIri(baseIri)) {
    throw new Error(`Invalid base IRI: ${baseIri}`);
  }

  const store = createStore();

  try {
    store.load(turtleContent, { format: 'text/turtle', baseIRI: baseIri });
    return store;
  } catch (error) {
    throw new Error(`Failed to load Turtle content: ${error.message}`);
  }
}

/**
 * Parse quads from a Turtle string
 *
 * @param {string} turtleContent - Turtle format RDF content
 * @param {string} [baseIri='http://example.org/'] - Base IRI for resolving relative IRIs
 * @returns {Array<import('@unrdf/oxigraph').Quad>} Array of parsed quads
 * @throws {Error} If parsing fails
 *
 * @example
 * ```javascript
 * const quads = parseQuads(`
 *   @prefix foaf: <http://xmlns.com/foaf/0.1/> .
 *   <http://example.org/alice> foaf:name "Alice" .
 * `);
 * console.log('Parsed', quads.length, 'quads');
 * ```
 */
export function parseQuads(turtleContent, baseIri = 'http://example.org/') {
  const store = loadTurtle(turtleContent, baseIri);
  return store.match(); // Returns all quads
}

/**
 * Canonicalize and sort quads for deterministic ordering
 *
 * Quads are sorted by:
 * 1. Subject value
 * 2. Predicate value
 * 3. Object value
 * 4. Graph value
 *
 * @param {Array<import('@unrdf/oxigraph').Quad>} quads - Quads to sort
 * @returns {Array<import('@unrdf/oxigraph').Quad>} Sorted quads
 *
 * @example
 * ```javascript
 * const sorted = canonicalizeQuads(store.match());
 * ```
 */
export function canonicalizeQuads(quads) {
  return [...quads].sort((a, b) => {
    // Sort by subject
    const subjCmp = a.subject.value.localeCompare(b.subject.value);
    if (subjCmp !== 0) return subjCmp;

    // Sort by predicate
    const predCmp = a.predicate.value.localeCompare(b.predicate.value);
    if (predCmp !== 0) return predCmp;

    // Sort by object
    const objCmp = a.object.value.localeCompare(b.object.value);
    if (objCmp !== 0) return objCmp;

    // Sort by graph
    return a.graph.value.localeCompare(b.graph.value);
  });
}

/**
 * Serialize quads to N-Triples format (deterministic)
 *
 * @param {Array<import('@unrdf/oxigraph').Quad>} quads - Quads to serialize
 * @returns {string} N-Triples serialization
 *
 * @example
 * ```javascript
 * const ntriples = serializeQuadsToNTriples(quads);
 * ```
 */
export function serializeQuadsToNTriples(quads) {
  const sortedQuads = canonicalizeQuads(quads);

  return sortedQuads
    .map((quad) => {
      const subject = formatTerm(quad.subject);
      const predicate = formatTerm(quad.predicate);
      const object = formatTerm(quad.object);

      return `${subject} ${predicate} ${object} .`;
    })
    .join('\n');
}

/**
 * Format an RDF term to N-Triples format
 *
 * @param {import('@unrdf/oxigraph').Term} term - RDF term
 * @returns {string} Formatted term
 * @private
 */
function formatTerm(term) {
  switch (term.termType) {
    case 'NamedNode':
      return `<${term.value}>`;
    case 'BlankNode':
      return `_:${term.value}`;
    case 'Literal':
      if (term.language) {
        return `"${escapeLiteral(term.value)}"@${term.language}`;
      }
      if (
        term.datatype &&
        term.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string'
      ) {
        return `"${escapeLiteral(term.value)}"^^<${term.datatype.value}>`;
      }
      return `"${escapeLiteral(term.value)}"`;
    case 'DefaultGraph':
      return '';
    default:
      return term.value;
  }
}

/**
 * Escape special characters in literal values
 *
 * @param {string} str - String to escape
 * @returns {string} Escaped string
 * @private
 */
function escapeLiteral(str) {
  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');
}

/**
 * Compute deterministic SHA256 hash of RDF content
 *
 * Quads are canonicalized (sorted) before hashing to ensure deterministic results.
 *
 * @param {Array<import('@unrdf/oxigraph').Quad>} quads - Quads to hash
 * @returns {Promise<string>} Hexadecimal SHA256 hash
 *
 * @example
 * ```javascript
 * const hash = await computeContentHash(store.match());
 * console.log('Content hash:', hash);
 * ```
 */
export async function computeContentHash(quads) {
  if (!Array.isArray(quads)) {
    throw new Error('Quads must be an array');
  }

  const ntriples = serializeQuadsToNTriples(quads);
  const hash = await sha256(ntriples);
  return hash;
}

/**
 * Compute SHA256 hash of a string
 *
 * @param {string} content - String content to hash
 * @returns {Promise<string>} Hexadecimal SHA256 hash
 *
 * @example
 * ```javascript
 * const hash = await hashString('hello world');
 * ```
 */
export async function hashString(content) {
  if (typeof content !== 'string') {
    throw new Error('Content must be a string');
  }

  return await sha256(content);
}

/**
 * Extract all namespace IRIs from quads
 *
 * @param {Array<import('@unrdf/oxigraph').Quad>} quads - Quads to analyze
 * @returns {Set<string>} Set of unique namespace IRIs
 *
 * @example
 * ```javascript
 * const namespaces = extractNamespaces(quads);
 * console.log('Found namespaces:', Array.from(namespaces));
 * ```
 */
export function extractNamespaces(quads) {
  const namespaces = new Set();

  for (const quad of quads) {
    // Extract namespace from named nodes
    if (quad.subject.termType === 'NamedNode') {
      namespaces.add(extractNamespaceFromIri(quad.subject.value));
    }
    if (quad.predicate.termType === 'NamedNode') {
      namespaces.add(extractNamespaceFromIri(quad.predicate.value));
    }
    if (quad.object.termType === 'NamedNode') {
      namespaces.add(extractNamespaceFromIri(quad.object.value));
    }
  }

  return namespaces;
}

/**
 * Extract namespace from an IRI (everything up to # or last /)
 *
 * @param {string} iri - IRI to extract namespace from
 * @returns {string} Namespace IRI
 *
 * @example
 * ```javascript
 * extractNamespaceFromIri('http://xmlns.com/foaf/0.1/name')
 * // => 'http://xmlns.com/foaf/0.1/'
 * ```
 */
export function extractNamespaceFromIri(iri) {
  // Check for fragment identifier (#)
  const hashIndex = iri.lastIndexOf('#');
  if (hashIndex !== -1) {
    return iri.substring(0, hashIndex + 1);
  }

  // Otherwise, use last slash
  const slashIndex = iri.lastIndexOf('/');
  if (slashIndex !== -1) {
    return iri.substring(0, slashIndex + 1);
  }

  // If no separator found, return the full IRI
  return iri;
}

/**
 * Merge multiple stores into one
 *
 * @param {Array<import('@unrdf/oxigraph').OxigraphStore>} stores - Stores to merge
 * @returns {import('@unrdf/oxigraph').OxigraphStore} Merged store
 *
 * @example
 * ```javascript
 * const merged = mergeStores([store1, store2, store3]);
 * ```
 */
export function mergeStores(stores) {
  const merged = createStore();

  for (const store of stores) {
    const quads = store.match();
    for (const quad of quads) {
      merged.add(quad);
    }
  }

  return merged;
}

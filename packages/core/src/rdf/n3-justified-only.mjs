/**
 * @fileoverview N3 Justified-Only Module - The ONLY module allowed to import N3 directly
 *
 * **100% COMPLIANCE ENFORCEMENT**:
 * - This module is the SINGLE SOURCE for all N3 functionality
 * - All other modules MUST import from this module, NOT from 'n3' directly
 * - Justification: Streaming RDF parsing/writing requires N3's SAX-like API
 *
 * **ALLOWED N3 FEATURES** (Streaming & Performance):
 * 1. Parser.parse() - Streaming RDF parsing (SAX-like, not DOM-based)
 * 2. Writer - Streaming RDF serialization (no memory buffering)
 * 3. StreamParser/StreamWriter - Backpressure-aware streaming
 *
 * **FORBIDDEN** (Use Oxigraph instead):
 * - Store - Use Oxigraph Store
 * - DataFactory - Use Oxigraph NamedNode, Literal, etc
 * - Quad operations - Use Oxigraph Quad
 *
 * @module @unrdf/core/rdf/n3-justified-only
 */

import { Parser, Writer, StreamParser, StreamWriter, DataFactory, Store } from 'n3';

/**
 * Streaming RDF parser using N3's SAX-like API
 *
 * @param {string} input - RDF content to parse
 * @param {object} options - Parser options
 * @param {string} [options.format='text/turtle'] - RDF format
 * @param {string} [options.baseIRI] - Base IRI for resolving relative URIs
 * @returns {Promise<import('oxigraph').Quad[]>} Parsed quads
 */
export async function streamingParse(input, options = {}) {
  const parser = new Parser({
    format: options.format || 'text/turtle',
    baseIRI: options.baseIRI,
  });

  return new Promise((resolve, reject) => {
    const quads = [];
    parser.parse(input, (error, quad, _prefixes) => {
      if (error) {
        reject(error);
      } else if (quad) {
        quads.push(quad);
      } else {
        // End of stream
        resolve(quads);
      }
    });
  });
}

/**
 * Streaming RDF writer using N3's Writer
 *
 * @param {import('oxigraph').Quad[]} quads - Quads to serialize
 * @param {object} options - Writer options
 * @param {string} [options.format='text/turtle'] - RDF format
 * @param {object} [options.prefixes] - Namespace prefixes
 * @returns {Promise<string>} Serialized RDF
 */
export async function streamingWrite(quads, options = {}) {
  return new Promise((resolve, reject) => {
    const writer = new Writer({
      format: options.format || 'text/turtle',
      prefixes: options.prefixes,
    });

    quads.forEach(quad => writer.addQuad(quad));

    writer.end((error, result) => {
      if (error) {
        reject(error);
      } else {
        resolve(result);
      }
    });
  });
}

/**
 * Create a streaming parser for large files
 *
 * @param {object} options - Parser options
 * @returns {StreamParser} N3 stream parser
 */
export function createStreamParser(options = {}) {
  return new StreamParser(options);
}

/**
 * Create a streaming writer for large files
 *
 * @param {object} options - Writer options
 * @returns {StreamWriter} N3 stream writer
 */
export function createStreamWriter(options = {}) {
  return new StreamWriter(options);
}

/**
 * DataFactory facade - provides RDF/JS DataFactory interface
 * Uses N3's implementation but isolated to this module
 *
 * **FOR NEW CODE**: Consider using Oxigraph's term constructors directly
 * **FOR EXISTING CODE**: Import from this module instead of 'n3'
 */
export const UnrdfDataFactory = {
  namedNode: DataFactory.namedNode.bind(DataFactory),
  blankNode: DataFactory.blankNode.bind(DataFactory),
  literal: DataFactory.literal.bind(DataFactory),
  variable: DataFactory.variable.bind(DataFactory),
  defaultGraph: DataFactory.defaultGraph.bind(DataFactory),
  quad: DataFactory.quad.bind(DataFactory),
  triple: DataFactory.triple?.bind(DataFactory),
};

/**
 * N3 DataFactory for backward compatibility ONLY
 * **PREFER UnrdfDataFactory or Oxigraph DataFactory for new code**
 *
 * @deprecated Use UnrdfDataFactory instead
 */
export { DataFactory as N3DataFactory };

/**
 * N3 Store - ONLY for backward compatibility detection
 *
 * **CRITICAL**: DO NOT use Store for new code!
 * - Use createStore() from @unrdf/oxigraph instead
 * - This export exists ONLY for n3-migration.mjs to detect N3.Store instances
 * - Required for instanceof checks in backward compatibility layer
 *
 * @deprecated Use @unrdf/oxigraph createStore() for all new code
 */
export { Store as N3Store };

/**
 * Re-export Parser and Writer for advanced streaming use cases
 */
export { Parser, Writer, StreamParser, StreamWriter };

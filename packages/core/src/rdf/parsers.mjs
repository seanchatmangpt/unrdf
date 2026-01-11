/**
 * @file Optimized RDF Parsers
 * @module @unrdf/core/rdf/parsers
 * @description Fast, streaming RDF parsers with error recovery and validation
 */

import { createReadStream } from 'node:fs';
import { createGunzip } from 'node:zlib';
import { Transform } from 'node:stream';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  BaseParserOptionsSchema,
  StreamParserOptionsSchema,
  ErrorRecoveryOptionsSchema,
} from './parsers.schema.mjs';

const { namedNode, literal, blankNode, defaultGraph, quad } = dataFactory;

/**
 * @typedef {import('@unrdf/oxigraph').Store} Store
 * @typedef {import('@unrdf/oxigraph').Quad} Quad
 */

/**
 * Parse RDF string to quads (streaming)
 * @param {string} rdfString - RDF content to parse
 * @param {Object} [options] - Parser options
 * @returns {AsyncGenerator<Quad>} Parsed quads
 *
 * @throws {Error} If parsing fails and strict mode is enabled
 *
 * @example
 * const turtle = `
 *   @prefix foaf: <http://xmlns.com/foaf/0.1/> .
 *   <http://example.org/alice> foaf:name "Alice" .
 * `;
 * for await (const quad of parseRdf(turtle, { format: 'turtle' })) {
 *   console.log(quad);
 * }
 */
export async function* parseRdf(rdfString, options = {}) {
  const opts = BaseParserOptionsSchema.parse(options);

  try {
    const parser = getParser(opts.format);
    yield* parser(rdfString, opts);
  } catch (error) {
    if (opts.strict) {
      throw new Error(`Parse error: ${error.message}`);
    }
    // In permissive mode, yield nothing on error
  }
}

/**
 * Parse RDF from file (streaming with optional compression detection)
 * @param {string} filePath - Path to RDF file
 * @param {Object} [options] - Parser options
 * @returns {AsyncGenerator<Quad>} Parsed quads
 *
 * @throws {TypeError} If filePath is invalid
 * @throws {Error} If file cannot be read
 *
 * @example
 * for await (const quad of parseFile('data.ttl.gz', { format: 'turtle' })) {
 *   console.log(quad);
 * }
 */
export async function* parseFile(filePath, options = {}) {
  if (!filePath) {
    throw new TypeError('filePath is required');
  }

  const opts = StreamParserOptionsSchema.parse(options);
  const isCompressed = filePath.endsWith('.gz');

  let readStream = createReadStream(filePath, { encoding: opts.encoding });

  if (isCompressed) {
    readStream = readStream.pipe(createGunzip());
  }

  let buffer = '';
  let quadsParsed = 0;
  let bytesRead = 0;

  for await (const chunk of readStream) {
    buffer += chunk;
    bytesRead += chunk.length;

    // Parse complete statements
    const lines = buffer.split('\n');
    buffer = lines.pop() || ''; // Keep incomplete line

    for (const line of lines) {
      if (line.trim() === '' || line.startsWith('#')) continue;

      try {
        const parser = getParser(opts.format);
        for await (const quad of parser(line + '\n', opts)) {
          yield quad;
          quadsParsed++;

          if (quadsParsed % opts.progressInterval === 0 && opts.onProgress) {
            opts.onProgress({ quadsParsed, bytesRead });
          }
        }
      } catch (error) {
        if (opts.strict) {
          throw new Error(`Parse error at line: ${error.message}`);
        }
      }
    }

    if (buffer.length > opts.maxBufferSize) {
      throw new Error(`Parser buffer exceeded maximum size: ${opts.maxBufferSize}`);
    }
  }

  // Parse remaining buffer
  if (buffer.trim()) {
    try {
      const parser = getParser(opts.format);
      for await (const quad of parser(buffer, opts)) {
        yield quad;
      }
    } catch (error) {
      if (opts.strict) {
        throw error;
      }
    }
  }
}

/**
 * Parse RDF to Oxigraph store
 * @param {string} rdfString - RDF content
 * @param {Object} [options] - Parser options
 * @returns {Promise<Store>} Populated Oxigraph store
 *
 * @example
 * const store = await parseToStore(turtle, { format: 'turtle' });
 * console.log(`Parsed ${store.size} quads`);
 */
export async function parseToStore(rdfString, options = {}) {
  const opts = BaseParserOptionsSchema.parse(options);
  const store = createStore();

  for await (const quad of parseRdf(rdfString, opts)) {
    store.add(quad);
  }

  return store;
}

/**
 * Parse RDF file to Oxigraph store
 * @param {string} filePath - Path to RDF file
 * @param {Object} [options] - Parser options
 * @returns {Promise<Store>} Populated Oxigraph store
 *
 * @example
 * const store = await parseFileToStore('data.ttl');
 * console.log(`Loaded ${store.size} quads from file`);
 */
export async function parseFileToStore(filePath, options = {}) {
  const store = createStore();

  for await (const quad of parseFile(filePath, options)) {
    store.add(quad);
  }

  return store;
}

/**
 * Parse with error recovery
 * @param {string} rdfString - RDF content (potentially malformed)
 * @param {Object} [options] - Parser and error recovery options
 * @returns {Promise<{quads: Quad[], errors: Error[]}>} Parsed quads and errors
 *
 * @example
 * const { quads, errors } = await parseWithRecovery(dirtyRdf, {
 *   format: 'turtle',
 *   skipInvalid: true,
 *   collectErrors: true
 * });
 * console.log(`Parsed ${quads.length} quads with ${errors.length} errors`);
 */
export async function parseWithRecovery(rdfString, options = {}) {
  const parserOpts = BaseParserOptionsSchema.parse(options);
  const recoveryOpts = ErrorRecoveryOptionsSchema.parse(options);

  const quads = [];
  const errors = [];

  const lines = rdfString.split('\n');

  for (const line of lines) {
    if (line.trim() === '' || line.startsWith('#')) continue;

    try {
      const parser = getParser(parserOpts.format);
      for await (const quad of parser(line + '\n', { ...parserOpts, strict: false })) {
        if (parserOpts.validate) {
          validateQuad(quad);
        }
        quads.push(quad);
      }
    } catch (error) {
      if (recoveryOpts.collectErrors) {
        errors.push(error);
      }
      if (recoveryOpts.onError) {
        recoveryOpts.onError(error);
      }
      if (!recoveryOpts.skipInvalid || errors.length >= recoveryOpts.maxErrors) {
        throw error;
      }
    }
  }

  return { quads, errors };
}

/**
 * Create streaming parser transform
 * @param {Object} [options] - Stream parser options
 * @returns {Transform} Transform stream that parses RDF
 *
 * @example
 * const parser = createParserStream({ format: 'ntriples' });
 * fs.createReadStream('data.nt').pipe(parser).on('data', quad => {
 *   console.log(quad);
 * });
 */
export function createParserStream(options = {}) {
  const opts = StreamParserOptionsSchema.parse(options);
  const parser = getParser(opts.format);

  let buffer = '';

  return new Transform({
    objectMode: true,
    highWaterMark: opts.highWaterMark,

    async transform(chunk, encoding, callback) {
      buffer += chunk.toString();

      const lines = buffer.split('\n');
      buffer = lines.pop() || '';

      try {
        for (const line of lines) {
          if (line.trim() === '' || line.startsWith('#')) continue;

          for await (const quad of parser(line + '\n', opts)) {
            this.push(quad);
          }
        }
        callback();
      } catch (error) {
        if (opts.strict) {
          callback(error);
        } else {
          callback();
        }
      }
    },

    async flush(callback) {
      if (buffer.trim()) {
        try {
          for await (const quad of parser(buffer, opts)) {
            this.push(quad);
          }
          callback();
        } catch (error) {
          if (opts.strict) {
            callback(error);
          } else {
            callback();
          }
        }
      } else {
        callback();
      }
    },
  });
}

/**
 * Batch parse multiple RDF strings
 * @param {string[]} rdfStrings - Array of RDF strings to parse
 * @param {Object} [options] - Parser options
 * @returns {AsyncGenerator<{index: number, quad: Quad}>} Parsed quads with source index
 *
 * @example
 * const inputs = [turtle1, turtle2, turtle3];
 * for await (const { index, quad } of batchParse(inputs, { format: 'turtle' })) {
 *   console.log(`Quad from input ${index}:`, quad);
 * }
 */
export async function* batchParse(rdfStrings, options = {}) {
  const opts = BaseParserOptionsSchema.parse(options);

  for (let i = 0; i < rdfStrings.length; i++) {
    try {
      for await (const quad of parseRdf(rdfStrings[i], opts)) {
        yield { index: i, quad };
      }
    } catch (error) {
      if (opts.strict) {
        throw new Error(`Batch parse error at index ${i}: ${error.message}`);
      }
    }
  }
}

// ============================================================================
// Format-Specific Parsers
// ============================================================================

/**
 * Parse N-Triples format
 * @private
 * @param {string} ntriples - N-Triples content
 * @param {Object} opts - Parser options
 * @yields {Quad} Parsed quads
 */
async function* parseNTriples(ntriples, opts) {
  const lines = ntriples.split('\n');

  for (const line of lines) {
    const trimmed = line.trim();
    if (trimmed === '' || trimmed.startsWith('#')) continue;

    try {
      const quad = parseNTripleLine(trimmed);
      if (quad) yield quad;
    } catch (error) {
      if (opts.strict) {
        throw error;
      }
    }
  }
}

/**
 * Parse Turtle format
 * @private
 * @param {string} turtle - Turtle content
 * @param {Object} opts - Parser options
 * @yields {Quad} Parsed quads
 */
async function* parseTurtle(turtle, opts) {
  // Use N3 parser for Turtle (justified case: complex format)
  const { Parser } = await import('@unrdf/core/rdf/n3-justified-only');
  const parser = new Parser({ format: 'Turtle', baseIRI: opts.baseIRI });

  try {
    const quads = parser.parse(turtle);
    for (const quad of quads) {
      yield quad;
    }
  } catch (error) {
    if (opts.strict) {
      throw error;
    }
  }
}

/**
 * Parse JSON-LD format
 * @private
 * @param {string} jsonld - JSON-LD content
 * @param {Object} opts - Parser options
 * @yields {Quad} Parsed quads
 */
async function* parseJsonLd(jsonld, opts) {
  try {
    const doc = JSON.parse(jsonld);
    yield* jsonLdToQuads(doc, opts.baseIRI);
  } catch (error) {
    if (opts.strict) {
      throw new Error(`JSON-LD parse error: ${error.message}`);
    }
  }
}

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Parse a single N-Triple line
 * @private
 * @param {string} line - N-Triple line
 * @returns {Quad|null} Parsed quad or null
 */
function parseNTripleLine(line) {
  if (!line.endsWith('.')) return null;

  const parts = line.slice(0, -1).trim().match(/(\S+)\s+(\S+)\s+(.+)/);
  if (!parts) return null;

  const [, s, p, o] = parts;

  const subject = parseTerm(s.trim());
  const predicate = parseTerm(p.trim());
  const object = parseTerm(o.trim());

  if (!subject || !predicate || !object) return null;

  return quad(subject, predicate, object, defaultGraph());
}

/**
 * Parse RDF term from N-Triples format
 * @private
 * @param {string} term - Term string
 * @returns {*} Parsed term
 */
function parseTerm(term) {
  if (term.startsWith('<') && term.endsWith('>')) {
    return namedNode(term.slice(1, -1));
  } else if (term.startsWith('_:')) {
    return blankNode(term.slice(2));
  } else if (term.startsWith('"')) {
    return parseLiteral(term);
  }
  return null;
}

/**
 * Parse literal from N-Triples format
 * @private
 * @param {string} literalStr - Literal string
 * @returns {*} Parsed literal
 */
function parseLiteral(literalStr) {
  const match = literalStr.match(/^"((?:[^"\\]|\\.)*)"\s*(?:@(\w+)|\^\^<([^>]+)>)?$/);
  if (!match) return null;

  const [, value, language, datatypeIri] = match;
  const unescaped = value.replace(/\\(.)/g, '$1');

  if (language) {
    return literal(unescaped, language);
  } else if (datatypeIri) {
    return literal(unescaped, namedNode(datatypeIri));
  } else {
    return literal(unescaped);
  }
}

/**
 * Convert JSON-LD to quads
 * @private
 * @param {Object} doc - JSON-LD document
 * @param {string} [_baseIRI] - Base IRI
 * @yields {Quad} Generated quads
 */
async function* jsonLdToQuads(doc, _baseIRI) {
  const graph = doc['@graph'] || [doc];

  for (const node of graph) {
    if (!node['@id']) continue;

    const subject = namedNode(node['@id']);

    for (const [key, value] of Object.entries(node)) {
      if (key.startsWith('@')) continue;

      const predicate = namedNode(key);

      const values = Array.isArray(value) ? value : [value];

      for (const val of values) {
        let object;

        if (typeof val === 'object' && val['@id']) {
          object = namedNode(val['@id']);
        } else if (typeof val === 'object' && val['@value'] !== undefined) {
          if (val['@language']) {
            object = literal(val['@value'], val['@language']);
          } else if (val['@type']) {
            object = literal(val['@value'], namedNode(val['@type']));
          } else {
            object = literal(val['@value']);
          }
        } else {
          object = literal(String(val));
        }

        yield quad(subject, predicate, object, defaultGraph());
      }
    }
  }
}

/**
 * Validate quad structure
 * @private
 * @param {Quad} quad - Quad to validate
 * @throws {Error} If quad is invalid
 */
function validateQuad(quad) {
  if (!quad.subject || !quad.predicate || !quad.object) {
    throw new Error('Invalid quad: missing subject, predicate, or object');
  }

  if (quad.subject.termType !== 'NamedNode' && quad.subject.termType !== 'BlankNode') {
    throw new Error('Invalid quad: subject must be NamedNode or BlankNode');
  }

  if (quad.predicate.termType !== 'NamedNode') {
    throw new Error('Invalid quad: predicate must be NamedNode');
  }
}

/**
 * Get parser function for format
 * @private
 * @param {string} format - RDF format
 * @returns {Function} Parser function
 */
function getParser(format) {
  switch (format) {
    case 'ntriples':
    case 'nquads':
      return parseNTriples;
    case 'turtle':
    case 'trig':
      return parseTurtle;
    case 'jsonld':
      return parseJsonLd;
    default:
      throw new Error(`Unsupported format: ${format}`);
  }
}

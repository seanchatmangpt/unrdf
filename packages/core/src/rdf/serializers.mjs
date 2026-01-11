/**
 * @file Optimized RDF Serializers
 * @module @unrdf/core/rdf/serializers
 * @description Fast, streaming RDF serializers with compression and batch processing
 */

import { createWriteStream } from 'node:fs';
import { pipeline } from 'node:stream/promises';
import { createGzip } from 'node:zlib';
import { Transform } from 'node:stream';
import {
  BaseSerializerOptionsSchema,
  TurtleSerializerOptionsSchema,
  JsonLdSerializerOptionsSchema,
  NTriplesSerializerOptionsSchema,
  StreamSerializerOptionsSchema,
} from './serializers.schema.mjs';

/**
 * @typedef {import('@unrdf/oxigraph').Store} Store
 * @typedef {import('@unrdf/oxigraph').Quad} Quad
 */

/**
 * Serialize quads to N-Triples format (streaming)
 * @param {AsyncIterable<Quad>|Iterable<Quad>} quads - Quads to serialize
 * @param {Object} [options] - Serializer options
 * @returns {AsyncGenerator<string>} N-Triples lines
 *
 * @example
 * const store = createStore();
 * for await (const line of serializeNTriples(store.match())) {
 *   console.log(line);
 * }
 */
export async function* serializeNTriples(quads, options = {}) {
  const opts = NTriplesSerializerOptionsSchema.parse({ ...options, format: 'ntriples' });

  const buffer = [];

  for await (const quad of quads) {
    buffer.push(quadToNTriple(quad));

    if (buffer.length >= opts.chunkSize) {
      if (opts.sorted) {
        buffer.sort();
      }
      yield buffer.join('\n') + '\n';
      buffer.length = 0;
    }
  }

  // Flush remaining
  if (buffer.length > 0) {
    if (opts.sorted) {
      buffer.sort();
    }
    yield buffer.join('\n') + '\n';
  }
}

/**
 * Serialize quads to Turtle format (streaming)
 * @param {AsyncIterable<Quad>|Iterable<Quad>} quads - Quads to serialize
 * @param {Object} [options] - Serializer options
 * @returns {AsyncGenerator<string>} Turtle chunks
 *
 * @example
 * const store = createStore();
 * for await (const chunk of serializeTurtle(store.match(), { prefixes: { foaf: 'http://xmlns.com/foaf/0.1/' } })) {
 *   console.log(chunk);
 * }
 */
export async function* serializeTurtle(quads, options = {}) {
  const opts = TurtleSerializerOptionsSchema.parse({ ...options, format: 'turtle' });

  // Write prefixes
  if (opts.prefixes) {
    const prefixLines = Object.entries(opts.prefixes)
      .map(([prefix, uri]) => `@prefix ${prefix}: <${uri}> .`)
      .join('\n');
    yield prefixLines + '\n\n';
  }

  // Group by subject for compact representation
  const subjects = new Map();
  let count = 0;

  for await (const quad of quads) {
    const subjectKey = termToString(quad.subject);

    if (!subjects.has(subjectKey)) {
      subjects.set(subjectKey, []);
    }
    subjects.get(subjectKey).push(quad);
    count++;

    // Flush when chunk size reached
    if (count % opts.chunkSize === 0) {
      yield* flushTurtleSubjects(subjects, opts);
      subjects.clear();
    }
  }

  // Flush remaining
  if (subjects.size > 0) {
    yield* flushTurtleSubjects(subjects, opts);
  }
}

/**
 * Serialize quads to JSON-LD format (streaming)
 * @param {AsyncIterable<Quad>|Iterable<Quad>} quads - Quads to serialize
 * @param {Object} [options] - Serializer options
 * @returns {AsyncGenerator<string>} JSON-LD chunks
 *
 * @example
 * const store = createStore();
 * for await (const chunk of serializeJsonLd(store.match())) {
 *   console.log(chunk);
 * }
 */
export async function* serializeJsonLd(quads, options = {}) {
  const opts = JsonLdSerializerOptionsSchema.parse({ ...options, format: 'jsonld' });

  // Build JSON-LD graph
  const graph = [];
  const nodeMap = new Map();

  for await (const quad of quads) {
    const subjectId = termToJsonLdId(quad.subject);

    if (!nodeMap.has(subjectId)) {
      const node = { '@id': subjectId };
      nodeMap.set(subjectId, node);
      graph.push(node);
    }

    const node = nodeMap.get(subjectId);
    const predicate = termToJsonLdPredicate(quad.predicate);
    const object = termToJsonLdValue(quad.object);

    if (!node[predicate]) {
      node[predicate] = [];
    }
    node[predicate].push(object);
  }

  // Create JSON-LD document
  const doc = {
    '@context': opts.context || {},
    '@graph': graph,
  };

  yield JSON.stringify(doc, null, 2);
}

/**
 * Serialize store to file with optional compression
 * @param {Store} store - Oxigraph store to serialize
 * @param {string} outputPath - Output file path
 * @param {Object} [options] - Serializer options
 * @returns {Promise<{bytesWritten: number, quadsWritten: number}>} Serialization stats
 *
 * @throws {TypeError} If store or outputPath is invalid
 * @throws {Error} If serialization fails
 *
 * @example
 * const stats = await serializeToFile(store, 'output.ttl.gz', {
 *   format: 'turtle',
 *   compress: true
 * });
 * console.log(`Wrote ${stats.quadsWritten} quads, ${stats.bytesWritten} bytes`);
 */
export async function serializeToFile(store, outputPath, options = {}) {
  if (!store) {
    throw new TypeError('store is required');
  }
  if (!outputPath) {
    throw new TypeError('outputPath is required');
  }

  const opts = BaseSerializerOptionsSchema.parse(options);
  const quads = store.match();

  let bytesWritten = 0;
  let quadsWritten = 0;

  // Select serializer based on format
  const serializer = getSerializer(opts.format);

  // Create write stream
  const fileStream = createWriteStream(outputPath);

  // Track bytes
  const trackingStream = new Transform({
    transform(chunk, encoding, callback) {
      bytesWritten += chunk.length;
      callback(null, chunk);
    },
  });

  try {
    // Build pipeline based on compression setting
    const streams = [
      async function* () {
        for await (const chunk of serializer(quads, opts)) {
          quadsWritten += opts.chunkSize;
          yield chunk;
        }
      },
      trackingStream,
    ];

    if (opts.compress) {
      streams.push(createGzip());
    }

    streams.push(fileStream);

    await pipeline(...streams);

    return { bytesWritten, quadsWritten };
  } catch (error) {
    throw new Error(`Serialization failed: ${error.message}`);
  }
}

/**
 * Batch serialize multiple stores with chunking
 * @param {Store[]} stores - Array of stores to serialize
 * @param {Object} [options] - Batch serializer options
 * @returns {AsyncGenerator<{storeIndex: number, chunk: string}>} Serialized chunks
 *
 * @example
 * const stores = [store1, store2, store3];
 * for await (const { storeIndex, chunk } of batchSerialize(stores, { format: 'ntriples' })) {
 *   console.log(`Store ${storeIndex}: ${chunk.length} bytes`);
 * }
 */
export async function* batchSerialize(stores, options = {}) {
  const opts = BaseSerializerOptionsSchema.parse(options);
  const serializer = getSerializer(opts.format);

  for (let i = 0; i < stores.length; i++) {
    const store = stores[i];
    const quads = store.match();

    for await (const chunk of serializer(quads, opts)) {
      yield { storeIndex: i, chunk };
    }
  }
}

/**
 * Create streaming serializer transform
 * @param {Object} [options] - Stream serializer options
 * @returns {Transform} Transform stream that serializes quads
 *
 * @example
 * const serializer = createSerializerStream({ format: 'turtle' });
 * quadStream.pipe(serializer).pipe(process.stdout);
 */
export function createSerializerStream(options = {}) {
  const opts = StreamSerializerOptionsSchema.parse(options);
  const serializer = getSerializer(opts.format);

  const buffer = [];
  let processing = false;

  return new Transform({
    objectMode: true,
    highWaterMark: opts.highWaterMark,

    async transform(quad, encoding, callback) {
      buffer.push(quad);

      if (buffer.length >= opts.chunkSize && !processing) {
        processing = true;
        const quadsToProcess = buffer.splice(0, opts.chunkSize);

        try {
          for await (const chunk of serializer(quadsToProcess, opts)) {
            this.push(chunk);
          }
          callback();
        } catch (error) {
          callback(error);
        } finally {
          processing = false;
        }
      } else {
        callback();
      }
    },

    async flush(callback) {
      if (buffer.length > 0) {
        try {
          for await (const chunk of serializer(buffer, opts)) {
            this.push(chunk);
          }
          callback();
        } catch (error) {
          callback(error);
        }
      } else {
        callback();
      }
    },
  });
}

// ============================================================================
// Helper Functions
// ============================================================================

/**
 * Convert quad to N-Triple string
 * @private
 * @param {Quad} quad - Quad to convert
 * @returns {string} N-Triple string
 */
function quadToNTriple(quad) {
  const s = termToNTriples(quad.subject);
  const p = termToNTriples(quad.predicate);
  const o = termToNTriples(quad.object);
  return `${s} ${p} ${o} .`;
}

/**
 * Convert term to N-Triples format
 * @private
 * @param {*} term - RDF term
 * @returns {string} N-Triples representation
 */
function termToNTriples(term) {
  if (term.termType === 'NamedNode') {
    return `<${term.value}>`;
  } else if (term.termType === 'BlankNode') {
    return `_:${term.value}`;
  } else if (term.termType === 'Literal') {
    let result = `"${escapeString(term.value)}"`;
    if (term.language) {
      result += `@${term.language}`;
    } else if (term.datatype && term.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string') {
      result += `^^<${term.datatype.value}>`;
    }
    return result;
  }
  return term.value;
}

/**
 * Convert term to string representation
 * @private
 * @param {*} term - RDF term
 * @returns {string} String representation
 */
function termToString(term) {
  return termToNTriples(term);
}

/**
 * Convert term to JSON-LD ID
 * @private
 * @param {*} term - RDF term
 * @returns {string} JSON-LD ID
 */
function termToJsonLdId(term) {
  if (term.termType === 'NamedNode') {
    return term.value;
  } else if (term.termType === 'BlankNode') {
    return `_:${term.value}`;
  }
  return term.value;
}

/**
 * Convert predicate to JSON-LD property
 * @private
 * @param {*} term - Predicate term
 * @returns {string} JSON-LD property
 */
function termToJsonLdPredicate(term) {
  return term.value;
}

/**
 * Convert term to JSON-LD value
 * @private
 * @param {*} term - Object term
 * @returns {Object|string} JSON-LD value
 */
function termToJsonLdValue(term) {
  if (term.termType === 'NamedNode') {
    return { '@id': term.value };
  } else if (term.termType === 'Literal') {
    const value = { '@value': term.value };
    if (term.language) {
      value['@language'] = term.language;
    } else if (term.datatype && term.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string') {
      value['@type'] = term.datatype.value;
    }
    return value;
  } else if (term.termType === 'BlankNode') {
    return { '@id': `_:${term.value}` };
  }
  return term.value;
}

/**
 * Escape string for N-Triples/Turtle
 * @private
 * @param {string} str - String to escape
 * @returns {string} Escaped string
 */
function escapeString(str) {
  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');
}

/**
 * Flush Turtle subjects to output
 * @private
 * @param {Map} subjects - Map of subjects to quads
 * @param {Object} _opts - Serializer options
 * @yields {string} Turtle chunks
 */
async function* flushTurtleSubjects(subjects, _opts) {
  const lines = [];

  for (const [subjectKey, quads] of subjects) {
    if (quads.length === 0) continue;

    lines.push(`${subjectKey}`);

    // Group by predicate
    const predicates = new Map();
    for (const quad of quads) {
      const predKey = termToString(quad.predicate);
      if (!predicates.has(predKey)) {
        predicates.set(predKey, []);
      }
      predicates.get(predKey).push(quad.object);
    }

    const predicateEntries = Array.from(predicates.entries());
    for (let i = 0; i < predicateEntries.length; i++) {
      const [predKey, objects] = predicateEntries[i];
      const objectStrs = objects.map(o => termToNTriples(o)).join(' , ');
      const sep = i === predicateEntries.length - 1 ? ' .' : ' ;';
      lines.push(`  ${predKey} ${objectStrs}${sep}`);
    }

    lines.push('');
  }

  yield lines.join('\n');
}

/**
 * Get serializer function for format
 * @private
 * @param {string} format - RDF format
 * @returns {Function} Serializer function
 */
function getSerializer(format) {
  switch (format) {
    case 'turtle':
    case 'trig':
      return serializeTurtle;
    case 'ntriples':
    case 'nquads':
      return serializeNTriples;
    case 'jsonld':
      return serializeJsonLd;
    default:
      throw new Error(`Unsupported format: ${format}`);
  }
}

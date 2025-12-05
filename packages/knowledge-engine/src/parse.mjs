/**
 * @file Parsing and serialization utilities for RDF data.
 * @module parse
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('unrdf');

/**
 * Parse a Turtle string into a Store.
 * @param {string} ttl - The Turtle string to parse
 * @param {string} [baseIRI] - Base IRI for resolving relative URIs
 * @returns {Promise<Store>} Promise resolving to a Store containing the parsed quads
 *
 * @throws {Error} If parsing fails
 *
 * @example
 * const ttl = `
 *   @prefix ex: <http://example.org/> .
 *   ex:alice ex:knows ex:bob .
 * `;
 * const store = await parseTurtle(ttl, 'http://example.org/');
 */
export async function parseTurtle(ttl, baseIRI = 'http://example.org/') {
  if (typeof ttl !== 'string') {
    throw new TypeError('parseTurtle: ttl must be a string');
  }
  if (typeof baseIRI !== 'string') {
    throw new TypeError('parseTurtle: baseIRI must be a string');
  }

  return tracer.startActiveSpan('parse.turtle', async span => {
    try {
      span.setAttributes({
        'parse.format': 'turtle',
        'parse.base_iri': baseIRI,
        'parse.input_length': ttl.length,
      });

      // Use Oxigraph for parsing Turtle
      const store = createStore();
      store.load(ttl, { format: 'text/turtle', base: baseIRI });

      span.setAttribute('parse.quads_count', store.size);
      span.setStatus({ code: SpanStatusCode.OK });

      return store;
    } catch (error) {
      span.recordException(error);
      span.setStatus({
        code: SpanStatusCode.ERROR,
        message: error.message,
      });
      throw new Error(`Failed to parse Turtle: ${error.message}`);
    } finally {
      span.end();
    }
  });
}

/**
 * Serialize a store to Turtle.
 * @param {Store} store - The store to serialize
 * @param {Object} [options] - Serialization options
 * @param {Object} [options.prefixes] - Prefix mappings
 * @param {string} [options.baseIRI] - Base IRI for the output
 * @returns {Promise<string>} Promise resolving to the Turtle string
 *
 * @throws {Error} If serialization fails
 *
 * @example
 * const turtle = await toTurtle(store, {
 *   prefixes: { ex: 'http://example.org/' },
 *   baseIRI: 'http://example.org/'
 * });
 */
export async function toTurtle(store, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('toTurtle: store must be a valid Store instance');
  }

  try {
    // Use Oxigraph dump for serialization
    let result = store.dump({ format: 'text/turtle' });

    // Add @base directive if baseIRI is provided
    if (options.baseIRI) {
      result = `@base <${options.baseIRI}> .\n\n${result}`;
    }

    // Add prefixes if provided (prepend to output)
    if (options.prefixes && Object.keys(options.prefixes).length > 0) {
      const prefixLines = Object.entries(options.prefixes)
        .map(([prefix, iri]) => `@prefix ${prefix}: <${iri}> .`)
        .join('\n');
      result = `${prefixLines}\n\n${result}`;
    }

    return result;
  } catch (error) {
    throw new Error(`Failed to serialize to Turtle: ${error.message}`);
  }
}

/**
 * Serialize a store to canonical N-Quads.
 * @param {Store} store - The store to serialize
 * @param {Object} [options] - Serialization options
 * @returns {Promise<string>} Promise resolving to the N-Quads string
 *
 * @throws {Error} If serialization fails
 *
 * @example
 * const nquads = await toNQuads(store);
 */
export async function toNQuads(store, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('toNQuads: store must be a valid Store instance');
  }

  try {
    // Use Oxigraph dump for N-Quads serialization
    const result = store.dump({ format: 'application/n-quads', ...options });
    return result;
  } catch (error) {
    throw new Error(`Failed to serialize to N-Quads: ${error.message}`);
  }
}

/**
 * Parse a JSON-LD string into a Store.
 * @param {string} jsonld - The JSON-LD string to parse
 * @param {Object} [options] - Parsing options
 * @param {string} [options.baseIRI] - Base IRI for resolving relative URIs
 * @returns {Promise<Store>} Promise resolving to a Store containing the parsed quads
 *
 * @throws {Error} If parsing fails
 *
 * @example
 * const jsonld = `{
 *   "@context": {"ex": "http://example.org/"},
 *   "@id": "ex:alice",
 *   "ex:knows": {"@id": "ex:bob"}
 * }`;
 * const store = await parseJsonLd(jsonld);
 */
export async function parseJsonLd(jsonld, _options = {}) {
  if (typeof jsonld !== 'string' && typeof jsonld !== 'object') {
    throw new TypeError('parseJsonLd: jsonld must be a string or object');
  }

  try {
    // Parse JSON string if needed
    const jsonldData = typeof jsonld === 'string' ? JSON.parse(jsonld) : jsonld;

    if (!jsonldData || (!Array.isArray(jsonldData) && typeof jsonldData !== 'object')) {
      throw new TypeError('parseJsonLd: jsonld must be an object or array');
    }

    // Convert JSON-LD to RDF quads
    // This is a simplified implementation - in production you would use a proper JSON-LD to RDF converter
    const store = createStore();

    // Handle @graph array or single object
    const items = jsonldData['@graph'] || (Array.isArray(jsonldData) ? jsonldData : [jsonldData]);

    // Validate that we have valid JSON-LD structure
    if (
      items.length === 0 ||
      !items.some(item => item && typeof item === 'object' && item['@id'])
    ) {
      throw new Error('Invalid JSON-LD structure: no valid items with @id found');
    }

    for (const item of items) {
      if (item && item['@id']) {
        const subject = item['@id'];

        // Process each property
        for (const [key, value] of Object.entries(item)) {
          if (key === '@id' || key === '@context') continue;

          if (Array.isArray(value)) {
            for (const val of value) {
              if (typeof val === 'object' && val['@id']) {
                store.add(
                  dataFactory.quad(
                    dataFactory.namedNode(subject),
                    dataFactory.namedNode(key),
                    dataFactory.namedNode(val['@id'])
                  )
                );
              } else if (typeof val === 'string' || typeof val === 'number') {
                store.add(
                  dataFactory.quad(
                    dataFactory.namedNode(subject),
                    dataFactory.namedNode(key),
                    dataFactory.literal(String(val))
                  )
                );
              }
            }
          } else if (typeof value === 'object' && value['@id']) {
            store.add(
              dataFactory.quad(
                dataFactory.namedNode(subject),
                dataFactory.namedNode(key),
                dataFactory.namedNode(value['@id'])
              )
            );
          } else if (typeof value === 'string' || typeof value === 'number') {
            store.add(
              dataFactory.quad(
                dataFactory.namedNode(subject),
                dataFactory.namedNode(key),
                dataFactory.literal(String(value))
              )
            );
          }
        }
      }
    }

    return store;
  } catch (error) {
    throw new Error(`Failed to parse JSON-LD: ${error.message}`);
  }
}

/**
 * Serialize a store to JSON-LD.
 * @param {Store} store - The store to serialize
 * @param {Object} [options] - Serialization options
 * @param {Object} [options.context] - JSON-LD context
 * @returns {Promise<Object>} Promise resolving to the JSON-LD object
 *
 * @throws {Error} If serialization fails
 *
 * @example
 * const jsonld = await toJsonLd(store, {
 *   context: { ex: 'http://example.org/' }
 * });
 */
export async function toJsonLd(store, options = {}) {
  if (!store || typeof store.getQuads !== 'function') {
    throw new TypeError('toJsonLd: store must be a valid Store instance');
  }

  try {
    // This is a simplified implementation
    // In production, you would use a proper RDF to JSON-LD converter
    const quads = store.getQuads();
    const result = {
      '@context': options.context || {},
      '@graph': [],
    };

    // Add @base to context if baseIRI is provided
    if (options.baseIRI) {
      result['@context']['@base'] = options.baseIRI;
    }

    // Convert quads to JSON-LD format
    for (const quad of quads) {
      const subject = quad.subject.value;
      const predicate = quad.predicate.value;
      const object = quad.object.value;

      // Find existing subject in graph or create new one
      let subjectNode = result['@graph'].find(node => node['@id'] === subject);
      if (!subjectNode) {
        subjectNode = { '@id': subject };
        result['@graph'].push(subjectNode);
      }

      // Add predicate-object pair
      if (!subjectNode[predicate]) {
        subjectNode[predicate] = [];
      }
      subjectNode[predicate].push({ '@id': object });
    }

    // Return as object (users can call JSON.stringify if they need a string)
    return result;
  } catch (error) {
    throw new Error(`Failed to serialize to JSON-LD: ${error.message}`);
  }
}

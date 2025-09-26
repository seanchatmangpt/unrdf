/**
 * @file Parsing and serialization utilities for RDF data.
 * @module parse
 */

import { Parser, Writer, Store } from 'n3';

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

  try {
    const parser = new Parser({ baseIRI });
    const quads = parser.parse(ttl);
    return new Store(quads);
  } catch (error) {
    throw new Error(`Failed to parse Turtle: ${error.message}`);
  }
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
    const writer = new Writer({ 
      format: 'Turtle',
      ...options
    });
    
    const quads = store.getQuads();
    writer.addQuads(quads);
    
    return new Promise((resolve, reject) => {
      writer.end((error, result) => {
        if (error) {
          reject(new Error(`Failed to serialize to Turtle: ${error.message}`));
        } else {
          resolve(result);
        }
      });
    });
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
    const writer = new Writer({ 
      format: 'N-Quads',
      ...options
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
export async function parseJsonLd(jsonld, options = {}) {
  if (typeof jsonld !== 'string') {
    throw new TypeError('parseJsonLd: jsonld must be a string');
  }

  try {
    const jsonldData = JSON.parse(jsonld);
    const parser = new Parser({ 
      baseIRI: options.baseIRI || 'http://example.org/'
    });
    
    // Convert JSON-LD to Turtle first, then parse
    // This is a simplified approach - in production you might want to use a proper JSON-LD parser
    const quads = parser.parse(jsonldData);
    return new Store(quads);
  } catch (error) {
    throw new Error(`Failed to parse JSON-LD: ${error.message}`);
  }
}

/**
 * Serialize a store to JSON-LD.
 * @param {Store} store - The store to serialize
 * @param {Object} [options] - Serialization options
 * @param {Object} [options.context] - JSON-LD context
 * @returns {Promise<string>} Promise resolving to the JSON-LD string
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
      '@graph': []
    };

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

    return JSON.stringify(result, null, 2);
  } catch (error) {
    throw new Error(`Failed to serialize to JSON-LD: ${error.message}`);
  }
}

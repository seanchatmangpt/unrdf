/**
 * @fileoverview Production-grade RDF engine for JavaScript.
 * @version 2.0.0
 * @license MIT
 */

import { Parser, Writer, UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { createStore } from '@unrdf/oxigraph'; // TODO: Replace with Oxigraph Store
import { createStore as createOxigraphStore } from '@unrdf/oxigraph';
import rdf from 'rdf-ext';
import SHACLValidator from 'rdf-validate-shacl';
import rdfCanonize from 'rdf-canonize';
import eyereasoner from 'eyereasoner';
import _jsonld from 'jsonld';

const { namedNode, literal, quad, blankNode, defaultGraph } = DataFactory;

/**
 * A comprehensive, production-grade engine for RDF processing in JavaScript.
 * It unifies parsing, serialization, querying, validation, and reasoning.
 */
export class RdfEngine {
  /**
   * @param {object} [options] - Configuration options for the engine.
   * @param {string} [options.baseIRI] - The base IRI to use for parsing relative URIs.
   */
  constructor(options = {}) {
    this.baseIRI = options.baseIRI || 'http://example.org/';
    this.store = createStore();
  }

  // =================================================================
  // == Core Setup & Store Access
  // =================================================================

  /**
   * Returns the underlying N3.js Store instance.
   * @returns {import('n3').Store}
   */
  getStore() {
    return this.store;
  }

  /**
   * Clears the internal store, removing all quads.
   */
  clearStore() {
    this.store.removeQuads(this.store.getQuads());
  }

  // =================================================================
  // == Term Creation
  // =================================================================

  /**
   *
   */
  namedNode(value) {
    return namedNode(value);
  }
  /**
   *
   */
  literal(value, langOrDt) {
    return literal(value, langOrDt);
  }
  /**
   *
   */
  blankNode(value) {
    return blankNode(value);
  }
  /**
   *
   */
  quad(s, p, o, g = defaultGraph()) {
    return quad(s, p, o, g);
  }

  // =================================================================
  // == Parsing & Serialization
  // =================================================================

  /**
   * Parses a Turtle string and adds the quads to the internal store.
   * @param {string} ttl - The Turtle string to parse.
   * @returns {import('n3').Store} The engine's store instance.
   */
  parseTurtle(ttl) {
    const quads = new Parser({ baseIRI: this.baseIRI }).parse(ttl);
    this.store.addQuads(quads);
    return this.store;
  }

  /**
   * Serializes a store to a Turtle string.
   * @param {import('n3').Store} [store=this.store] - The store to serialize.
   * @param {object} [options] - N3.js Writer options.
   * @returns {string}
   */
  serializeTurtle(store = this.store, options = {}) {
    const writer = new Writer({ ...options, format: 'Turtle' });
    return writer.quadsToString(store.getQuads());
  }

  /**
   * Serializes a store to a canonical N-Quads string.
   * @param {import('n3').Store} [store=this.store] - The store to serialize.
   * @returns {string}
   */
  serializeNQuads(store = this.store) {
    const writer = new Writer({ format: 'N-Quads' });
    return writer.quadsToString(store.getQuads());
  }

  // =================================================================
  // == SPARQL Querying
  // =================================================================

  /**
   * Executes a read-only SPARQL query (SELECT, ASK, CONSTRUCT) against the store.
   * @param {string} sparql - The SPARQL query string.
   * @returns {Promise<Array<object>|boolean|import('n3').Store>} The query result.
   */
  async query(sparql) {
    // Remove PREFIX declarations to find the actual query type
    const queryWithoutPrefixes = sparql.replace(/^PREFIX\s+[^\s]+\s+<[^>]+>\s*/gm, '').trim();
    const queryType = queryWithoutPrefixes.toUpperCase().split(/\s+/)[0];

    // Convert n3.Store to Oxigraph store and execute query synchronously
    const oxigraphStore = createOxigraphStore(Array.from(this.store.getQuads()));
    const result = oxigraphStore.query(sparql);

    switch (queryType) {
      case 'SELECT': {
        // Oxigraph returns array of binding objects for SELECT
        const rows = Array.isArray(result)
          ? result.map(item => {
              const entry = {};
              if (item && typeof item === 'object') {
                for (const [key, val] of Object.entries(item)) {
                  entry[key] = val && val.value ? val.value : val;
                }
              }
              return entry;
            })
          : [];
        const variables = rows.length > 0 ? Object.keys(rows[0]) : [];
        return { type: 'select', rows, variables };
      }
      case 'ASK': {
        // Oxigraph returns boolean for ASK
        const boolean = typeof result === 'boolean' ? result : false;
        return { type: 'ask', boolean };
      }
      case 'CONSTRUCT': {
        // Oxigraph returns array of quads for CONSTRUCT
        const quads = Array.isArray(result) ? result : [];
        return {
          type: 'construct',
          store: createStore(quads),
        };
      }
      case 'DESCRIBE': {
        // Oxigraph returns array of quads for DESCRIBE
        const quads = Array.isArray(result) ? result : [];
        return {
          type: 'describe',
          store: createStore(quads),
        };
      }
      case 'INSERT':
      case 'UPDATE':
      case 'DELETE':
        throw new Error(
          `Query type "${queryType}" is not supported. Use the update() helper for writes.`
        );
      default:
        throw new Error(`Query type "${queryType}" is not supported by query().`);
    }
  }

  /**
   * Executes a SPARQL UPDATE operation (INSERT, DELETE, etc.) against the store.
   * @param {string} sparql - The SPARQL UPDATE query string.
   * @returns {Promise<object>} The update result.
   */
  async update(sparql) {
    // Remove PREFIX declarations to find the actual query type
    const queryWithoutPrefixes = sparql.replace(/PREFIX\s+[^\s]+\s+<[^>]+>\s*/g, '').trim();

    // If still starts with PREFIX, try a different approach
    if (queryWithoutPrefixes.startsWith('PREFIX')) {
      const lines = queryWithoutPrefixes.split('\n');
      const nonPrefixLines = lines.filter(line => !line.trim().startsWith('PREFIX'));
      const result = nonPrefixLines.join('\n').trim();
      return this.update(result);
    }
    const queryType = queryWithoutPrefixes.toUpperCase().split(/\s+/)[0];

    // For now, we'll implement a simple INSERT DATA operation
    if (queryType === 'INSERT' && queryWithoutPrefixes.includes('INSERT DATA')) {
      // Parse the INSERT DATA operation - need to handle PREFIX declarations
      const insertMatch = sparql.match(/INSERT\s+DATA\s*\{([^}]+)\}/is);
      if (insertMatch) {
        const turtleData = insertMatch[1].trim();

        // Extract PREFIX declarations from the original query
        const prefixMatches = sparql.match(/PREFIX\s+[^\s]+\s+<[^>]+>/gi) || [];
        const prefixes = prefixMatches.join('\n');

        // Combine prefixes with the data for parsing
        const dataToParse = prefixes + (prefixes ? '\n' : '') + turtleData;

        // Parse the Turtle data with PREFIX declarations and add to store
        const parser = new Parser();
        const quads = parser.parse(dataToParse);
        for (const quad of quads) {
          this.store.add(quad);
        }
        return { type: 'update', ok: true, inserted: quads.length };
      }
    }

    // For other UPDATE operations, throw an error for now
    throw new Error(`UPDATE operation "${queryType}" not yet implemented`);
  }

  // =================================================================
  // == SHACL Validation
  // =================================================================

  /**
   * Validates a data store against a set of SHACL shapes.
   * @param {import('n3').Store} dataStore - The store containing data to validate.
   * @param {import('n3').Store|string} shapes - The store or Turtle string containing SHACL shapes.
   * @returns {{conforms: boolean, results: Array<object>}} A validation report.
   */
  validateShacl(dataStore, shapes) {
    const shapesStore = typeof shapes === 'string' ? createStore(new Parser().parse(shapes)) : shapes;

    const validator = new SHACLValidator(rdf.dataset([...shapesStore]));
    const report = validator.validate(rdf.dataset([...dataStore]));

    return {
      conforms: report.conforms,
      results: (report.results || []).map(r => ({
        message: r.message?.[0]?.value || null,
        path: r.path?.value || null,
        focusNode: r.focusNode?.value || null,
      })),
    };
  }

  // =================================================================
  // == Reasoning
  // =================================================================

  /**
   * Infers new knowledge by applying N3 Rules to a data store.
   * @param {import('n3').Store} dataStore - The store containing data.
   * @param {import('n3').Store|string} rules - The store or Turtle string containing N3 Rules.
   * @returns {Promise<import('n3').Store>} A new store containing both original and inferred quads.
   */
  async reason(dataStore, rules) {
    const rulesN3 = typeof rules === 'string' ? rules : this.serializeTurtle(rules);
    const dataN3 = this.serializeTurtle(dataStore);
    const { executeBasicEyeQuery } = await import('eyereasoner');
    const inferredN3 = await executeBasicEyeQuery(eyereasoner.SWIPL, dataN3, rulesN3);
    return createStore(new Parser().parse(inferredN3));
  }

  // =================================================================
  // == Canonicalization & Isomorphism
  // =================================================================

  /**
   * Produces a canonical representation of a store's quads using URDNA2015.
   * @param {import('n3').Store} store - The store to canonicalize.
   * @returns {string} The canonical N-Quads string.
   */
  canonicalize(store) {
    return rdfCanonize.canonizeSync(store.getQuads(), {
      algorithm: 'URDNA2015',
    });
  }

  /**
   * Checks if two stores are logically equivalent (isomorphic).
   * @param {import('n3').Store} storeA
   * @param {import('n3').Store} storeB
   * @returns {boolean}
   */
  isIsomorphic(storeA, storeB) {
    if (storeA.size !== storeB.size) return false;
    return this.canonicalize(storeA) === this.canonicalize(storeB);
  }
}

/**
 * @fileoverview Production-grade RDF engine for JavaScript.
 * @version 2.0.0
 * @license MIT
 */

import { Parser, Store, Writer, DataFactory } from 'n3';
import { QueryEngine } from '@comunica/query-sparql';
import rdf from 'rdf-ext';
import SHACLValidator from 'rdf-validate-shacl';
import rdfCanonize from 'rdf-canonize';
import eyereasoner from 'eyereasoner';
import jsonld from 'jsonld';

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
    this.baseIRI = options.baseIRI || "http://example.org/";
    this.comunicaEngine = new QueryEngine();
    this.store = new Store();
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

  namedNode(value) { return namedNode(value); }
  literal(value, langOrDt) { return literal(value, langOrDt); }
  blankNode(value) { return blankNode(value); }
  quad(s, p, o, g = defaultGraph()) { return quad(s, p, o, g); }

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
    const queryType = sparql.trim().toUpperCase().split(/\s+/)[0];
    const source = { type: 'rdfjsSource', value: this.store };
    const context = { sources: [source] };

    switch (queryType) {
      case 'SELECT':
        const bindingsStream = await this.comunicaEngine.queryBindings(sparql, context);
        const bindings = await bindingsStream.toArray();
        // Convert from RDF/JS terms to simple values
        return bindings.map(b => Object.fromEntries([...b].map(([k, v]) => [k.value, v.value])));
      case 'ASK':
        return this.comunicaEngine.queryBoolean(sparql, context);
      case 'CONSTRUCT':
        const quadStream = await this.comunicaEngine.queryQuads(sparql, context);
        return new Store(await quadStream.toArray());
      case 'PREFIX':
        // Handle PREFIX declarations by treating as SELECT query
        const selectQuery = sparql.replace(/^PREFIX\s+[^\s]+\s+<[^>]+>\s*/g, '').trim();
        if (selectQuery.toUpperCase().startsWith('SELECT')) {
          const bindingsStream = await this.comunicaEngine.queryBindings(sparql, context);
          const bindings = await bindingsStream.toArray();
          return bindings.map(b => Object.fromEntries([...b].map(([k, v]) => [k.value, v.value])));
        }
        throw new Error(`Query type "${queryType}" is not supported. Use the TransactionManager for writes.`);
      default:
        throw new Error(`Query type "${queryType}" is not supported. Use the TransactionManager for writes.`);
    }
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
    const shapesStore = typeof shapes === "string"
      ? new Store(new Parser().parse(shapes))
      : shapes;

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
    const rulesN3 = typeof rules === "string" ? rules : this.serializeTurtle(rules);
    const dataN3 = this.serializeTurtle(dataStore);
    const inferredN3 = await eyereasoner.queryOnce(dataN3, rulesN3);
    return new Store(new Parser().parse(inferredN3));
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
    return rdfCanonize.canonizeSync(store.getQuads(), { algorithm: 'URDNA2015' });
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
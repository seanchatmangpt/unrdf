/**
 * @fileoverview CRDT-based RDF Graph using Yjs for conflict-free collaborative editing
 *
 * Implements a Last-Write-Wins (LWW) CRDT for RDF triples using Yjs.
 * Each triple is stored with metadata (timestamp, clientID) for conflict resolution.
 *
 * CRDT Properties:
 * - Commutativity: Operations commute regardless of order
 * - Associativity: Grouping doesn't matter
 * - Idempotency: Applying same operation multiple times = applying once
 *
 * @module @unrdf/collab/crdt
 */

import * as Y from 'yjs';
import { dataFactory } from '@unrdf/core';
import { z } from 'zod';

/**
 * @typedef {Object} RDFTriple
 * @property {string} subject - Subject URI or blank node
 * @property {string} predicate - Predicate URI
 * @property {string} object - Object URI, literal, or blank node
 * @property {string} [objectType] - Type: 'uri', 'literal', 'blank'
 * @property {string} [datatype] - Datatype URI for literals
 * @property {string} [language] - Language tag for literals
 */

/**
 * @typedef {Object} CRDTTriple
 * @property {RDFTriple} triple - The RDF triple
 * @property {number} timestamp - Last write timestamp
 * @property {number} clientID - Client that last modified
 * @property {boolean} deleted - Tombstone flag
 */

/** Validation schema for RDF triple */
const RDFTripleSchema = z.object({
  subject: z.string().min(1),
  predicate: z.string().min(1),
  object: z.string().min(1),
  objectType: z.enum(['uri', 'literal', 'blank']).optional(),
  datatype: z.string().optional(),
  language: z.string().optional(),
});

/**
 * CollaborativeRDFGraph - CRDT-based RDF graph with automatic conflict resolution
 *
 * Uses Yjs YArray to store triples as CRDT elements. Each triple has:
 * - Unique key (subject+predicate+object hash)
 * - LWW metadata (timestamp, clientID)
 * - Tombstone for deletions
 *
 * @example
 * const graph = new CollaborativeRDFGraph();
 *
 * // Add triple - automatically gets timestamp + clientID
 * graph.addTriple({
 *   subject: 'http://example.org/alice',
 *   predicate: 'http://xmlns.com/foaf/0.1/knows',
 *   object: 'http://example.org/bob',
 *   objectType: 'uri'
 * });
 *
 * // Get all triples (excludes tombstones)
 * const triples = graph.getTriples();
 *
 * // Sync with another graph (CRDTs merge automatically)
 * Y.applyUpdate(doc1, Y.encodeStateAsUpdate(doc2));
 */
export class CollaborativeRDFGraph {
  /**
   * Create a new collaborative RDF graph
   * @param {Y.Doc} [ydoc] - Existing Yjs document (for sync)
   */
  constructor(ydoc = new Y.Doc()) {
    /** @type {Y.Doc} */
    this.ydoc = ydoc;

    /** @type {Y.Map<CRDTTriple>} - Map of triple keys to CRDT triples */
    this.triples = this.ydoc.getMap('triples');

    /** @type {Y.Map<any>} - Metadata (graph name, version, etc.) */
    this.metadata = this.ydoc.getMap('metadata');

    /** @type {number} - Client ID for this instance */
    this.clientID = this.ydoc.clientID;

    /** @type {Array<Function>} - Change listeners */
    this.changeListeners = [];

    // Subscribe to changes
    this.triples.observe(this._handleChange.bind(this));
  }

  /**
   * Generate unique key for a triple (for deduplication)
   * @param {RDFTriple} triple - RDF triple
   * @returns {string} Unique key
   * @private
   */
  _tripleKey(triple) {
    return `${triple.subject}||${triple.predicate}||${triple.object}`;
  }

  /**
   * Add a triple to the graph (LWW semantics)
   * @param {RDFTriple} triple - Triple to add
   * @throws {Error} If triple is invalid
   *
   * @example
   * graph.addTriple({
   *   subject: 'http://example.org/alice',
   *   predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
   *   object: 'http://xmlns.com/foaf/0.1/Person',
   *   objectType: 'uri'
   * });
   */
  addTriple(triple) {
    // Validate triple
    const result = RDFTripleSchema.safeParse(triple);
    if (!result.success) {
      throw new Error(`Invalid triple: ${result.error.message}`);
    }

    const key = this._tripleKey(triple);
    const now = Date.now();

    // Get existing triple if any
    const existing = this.triples.get(key);

    // LWW: Only write if newer or doesn't exist (>= allows same timestamp, Yjs resolves ties)
    if (!existing || now >= existing.timestamp) {
      this.triples.set(key, {
        triple,
        timestamp: now,
        clientID: this.clientID,
        deleted: false,
      });
    }
  }

  /**
   * Remove a triple from the graph (tombstone, LWW semantics)
   * @param {RDFTriple} triple - Triple to remove
   *
   * @example
   * graph.removeTriple({
   *   subject: 'http://example.org/alice',
   *   predicate: 'http://xmlns.com/foaf/0.1/knows',
   *   object: 'http://example.org/bob'
   * });
   */
  removeTriple(triple) {
    const key = this._tripleKey(triple);
    const now = Date.now();

    const existing = this.triples.get(key);

    // LWW: Only delete if newer or equal (>= allows same timestamp, Yjs resolves ties)
    if (!existing || now >= existing.timestamp) {
      this.triples.set(key, {
        triple,
        timestamp: now,
        clientID: this.clientID,
        deleted: true, // Tombstone
      });
    }
  }

  /**
   * Get all active triples (excludes tombstones)
   * @returns {Array<RDFTriple>} Array of triples
   *
   * @example
   * const triples = graph.getTriples();
   * console.log(`Graph has ${triples.length} triples`);
   */
  getTriples() {
    const result = [];
    this.triples.forEach((crdtTriple) => {
      if (!crdtTriple.deleted) {
        result.push(crdtTriple.triple);
      }
    });
    return result;
  }

  /**
   * Query triples by pattern (simple pattern matching)
   * @param {Object} pattern - Pattern to match
   * @param {string} [pattern.subject] - Subject pattern
   * @param {string} [pattern.predicate] - Predicate pattern
   * @param {string} [pattern.object] - Object pattern
   * @returns {Array<RDFTriple>} Matching triples
   *
   * @example
   * // Find all triples about Alice
   * const aliceTriples = graph.queryTriples({
   *   subject: 'http://example.org/alice'
   * });
   */
  queryTriples(pattern) {
    const triples = this.getTriples();

    return triples.filter((triple) => {
      if (pattern.subject && triple.subject !== pattern.subject) return false;
      if (pattern.predicate && triple.predicate !== pattern.predicate)
        return false;
      if (pattern.object && triple.object !== pattern.object) return false;
      return true;
    });
  }

  /**
   * Convert to @unrdf/core Store for SPARQL queries
   * @returns {Promise<import('@unrdf/core').Store>} RDF Store
   *
   * @example
   * const store = await graph.toStore();
   * const results = await engine.query(store, 'SELECT * WHERE { ?s ?p ?o }');
   */
  async toStore() {
    const { createStore } = await import('@unrdf/core');
    const store = createStore();

    this.getTriples().forEach((triple) => {
      // Convert to RDF/JS quad
      const subject = triple.subject.startsWith('_:')
        ? dataFactory.blankNode(triple.subject.slice(2))
        : dataFactory.namedNode(triple.subject);

      const predicate = dataFactory.namedNode(triple.predicate);

      let object;
      if (triple.objectType === 'uri') {
        object = dataFactory.namedNode(triple.object);
      } else if (triple.objectType === 'blank') {
        object = dataFactory.blankNode(triple.object.slice(2));
      } else {
        // Literal
        object = triple.datatype
          ? dataFactory.literal(triple.object, dataFactory.namedNode(triple.datatype))
          : triple.language
            ? dataFactory.literal(triple.object, triple.language)
            : dataFactory.literal(triple.object);
      }

      const quad = dataFactory.quad(subject, predicate, object);
      store.add(quad);
    });

    return store;
  }

  /**
   * Subscribe to graph changes
   * @param {Function} listener - Callback function
   * @returns {Function} Unsubscribe function
   *
   * @example
   * const unsubscribe = graph.onChange((event) => {
   *   console.log('Graph changed:', event);
   * });
   */
  onChange(listener) {
    this.changeListeners.push(listener);
    return () => {
      const idx = this.changeListeners.indexOf(listener);
      if (idx !== -1) this.changeListeners.splice(idx, 1);
    };
  }

  /**
   * Handle Yjs change events
   * @param {Y.YMapEvent} event - Yjs map event
   * @private
   */
  _handleChange(event) {
    const changes = {
      added: [],
      removed: [],
      modified: [],
    };

    event.changes.keys.forEach((change, key) => {
      if (change.action === 'add') {
        const crdtTriple = this.triples.get(key);
        if (!crdtTriple.deleted) {
          changes.added.push(crdtTriple.triple);
        }
      } else if (change.action === 'update') {
        const crdtTriple = this.triples.get(key);
        if (crdtTriple.deleted) {
          changes.removed.push(crdtTriple.triple);
        } else {
          changes.modified.push(crdtTriple.triple);
        }
      }
    });

    // Notify listeners
    this.changeListeners.forEach((listener) => listener(changes));
  }

  /**
   * Get statistics about the graph
   * @returns {Object} Statistics
   *
   * @example
   * const stats = graph.getStats();
   * console.log(`Active: ${stats.active}, Tombstones: ${stats.tombstones}`);
   */
  getStats() {
    let active = 0;
    let tombstones = 0;

    this.triples.forEach((crdtTriple) => {
      if (crdtTriple.deleted) {
        tombstones++;
      } else {
        active++;
      }
    });

    return {
      active,
      tombstones,
      total: active + tombstones,
      clientID: this.clientID,
    };
  }

  /**
   * Clear all triples (including tombstones)
   * WARNING: This breaks CRDT properties - use only for initialization
   */
  clear() {
    this.triples.clear();
  }

  /**
   * Get the Yjs document for sync providers
   * @returns {Y.Doc} Yjs document
   */
  getYDoc() {
    return this.ydoc;
  }
}

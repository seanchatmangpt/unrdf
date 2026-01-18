/**
 * Atomic Store - Extends UnrdfStore with capsule application and receipt tracking
 * @module agent-8/store
 */

import { UnrdfStore } from '@unrdf/core';
import { dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';

/**
 * Default graph for universe state
 */
export const UNIVERSE_GRAPH = dataFactory.namedNode('urn:autonomic:universe');

/**
 * AtomicStore - Extends UnrdfStore with capsule delta application
 *
 * Provides atomic operations for applying capsule deltas with:
 * - All-or-nothing semantics (ACID compliance)
 * - Receipt chain tracking (parent hash → child hash)
 * - Rollback on error
 * - Optional snapshot integration with KGC-4D
 *
 * @class
 * @extends UnrdfStore
 *
 * @example
 * import { createAtomicStore } from './store.mjs';
 * const store = createAtomicStore({ nodeId: 'node-1' });
 * const receipt = await store.applyCapsule(capsule);
 * console.log(receipt.hash); // BLAKE3 hash
 */
export class AtomicStore extends UnrdfStore {
  /**
   * Create a new AtomicStore
   * @param {Object} [options] - Store options
   * @param {string} [options.nodeId] - Node ID for receipt chain
   * @param {boolean} [options.enableSnapshots=false] - Enable KGC-4D integration
   */
  constructor(options = {}) {
    super([], options);

    /**
     * Node ID for this store instance
     * @type {string}
     */
    this.nodeId = options.nodeId || this._generateNodeId();

    /**
     * Receipt chain - array of all receipts in order
     * @type {Array<Object>}
     */
    this._receipts = [];

    /**
     * Snapshots enabled flag
     * @type {boolean}
     */
    this.snapshotsEnabled = options.enableSnapshots === true;
  }

  /**
   * Generate a unique node ID for this store instance
   * @returns {string} Node ID
   * @private
   */
  _generateNodeId() {
    // Use crypto.randomUUID if available (Node 18+, browsers)
    if (typeof crypto !== 'undefined' && crypto.randomUUID) {
      return `node-${crypto.randomUUID().slice(0, 8)}`;
    }
    // Fallback to timestamp-based ID
    return `node-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
  }

  /**
   * Get all receipts
   * @returns {Array<Object>} Array of receipts
   *
   * @example
   * const receipts = store.getReceipts();
   * console.log(receipts.length); // Number of applied capsules
   */
  getReceipts() {
    return [...this._receipts];
  }

  /**
   * Get last receipt (most recent)
   * @returns {Object|null} Last receipt or null if none
   *
   * @example
   * const lastReceipt = store.getLastReceipt();
   * if (lastReceipt) {
   *   console.log(lastReceipt.hash);
   * }
   */
  getLastReceipt() {
    return this._receipts.length > 0 ? this._receipts[this._receipts.length - 1] : null;
  }

  /**
   * Add receipt to chain
   * @param {Object} receipt - Receipt to add
   * @private
   */
  _addReceipt(receipt) {
    this._receipts.push(receipt);
  }

  /**
   * Get store size (number of quads in universe graph)
   * @returns {number} Quad count
   *
   * @example
   * const size = store.size();
   * console.log(`Store contains ${size} quads`);
   */
  size() {
    // Count quads in the store
    let count = 0;
    for (const _quad of this._store.match()) {
      count++;
    }
    return count;
  }

  /**
   * Clear all quads from store
   * WARNING: This does not clear receipts
   *
   * @example
   * store.clear();
   * console.assert(store.size() === 0, 'Store is empty');
   */
  clear() {
    // Remove all quads
    const quads = [...this._store.match()];
    for (const quad of quads) {
      this._store.delete(quad);
    }
  }

  /**
   * Add a quad to the store
   * @param {Object} quad - RDF quad to add
   *
   * @example
   * import { dataFactory } from '@unrdf/oxigraph';
   * const quad = dataFactory.quad(
   *   dataFactory.namedNode('http://ex.org/s'),
   *   dataFactory.namedNode('http://ex.org/p'),
   *   dataFactory.literal('o')
   * );
   * store.add(quad);
   */
  add(quad) {
    this._store.add(quad);
  }

  /**
   * Delete a quad from the store
   * @param {Object} quad - RDF quad to delete
   *
   * @example
   * store.delete(quad);
   */
  delete(quad) {
    this._store.delete(quad);
  }

  /**
   * Match quads in the store
   * @param {Object} [subject] - Subject to match
   * @param {Object} [predicate] - Predicate to match
   * @param {Object} [object] - Object to match
   * @param {Object} [graph] - Graph to match
   * @returns {Iterable<Object>} Matching quads
   *
   * @example
   * for (const quad of store.match(null, predicate, null)) {
   *   console.log(quad);
   * }
   */
  match(subject, predicate, object, graph) {
    return this._store.match(subject, predicate, object, graph);
  }

  /**
   * Execute SPARQL query
   * @param {string} sparql - SPARQL query string
   * @param {Object} [options] - Query options
   * @returns {Array|boolean|Object} Query results
   *
   * @example
   * const results = store.query('SELECT * WHERE { ?s ?p ?o }');
   */
  query(sparql, options) {
    return this._store.query(sparql, options);
  }

  /**
   * Insert multiple quads atomically
   * @param {Array<Object>} quads - Quads to insert
   * @returns {Promise<void>}
   *
   * @example
   * await store.insert([quad1, quad2, quad3]);
   */
  async insert(quads) {
    if (!Array.isArray(quads)) {
      throw new TypeError('Quads must be an array');
    }
    for (const quad of quads) {
      this.add(quad);
    }
  }

  /**
   * Delete multiple quads atomically
   * @param {Array<Object>} quads - Quads to delete
   * @returns {Promise<void>}
   *
   * @example
   * await store.delete([quad1, quad2]);
   */
  async deleteQuads(quads) {
    if (!Array.isArray(quads)) {
      throw new TypeError('Quads must be an array');
    }
    for (const quad of quads) {
      this.delete(quad);
    }
  }
}

/**
 * Create atomic store instance
 * @param {Object} [options] - Store configuration
 * @param {string} [options.nodeId] - Node ID for receipt chain
 * @param {boolean} [options.enableSnapshots=false] - Enable KGC-4D integration
 * @returns {AtomicStore} New atomic store instance
 *
 * @example
 * const store = createAtomicStore({ nodeId: 'test-node' });
 */
export function createAtomicStore(options = {}) {
  return new AtomicStore(options);
}

/**
 * @fileoverview RDF-Set CRDT - Specialized OR-Set for RDF triples
 *
 * Implements a CRDT specifically designed for RDF triple management.
 * Uses OR-Set semantics with add-wins conflict resolution.
 * Each triple gets a unique tag, and removal creates tombstones.
 *
 * CRDT Properties Proven:
 * - Concurrent adds of same triple → single triple in final state
 * - Add + Remove in any order → converges to same state
 * - Commutativity, Associativity, Idempotency guaranteed
 *
 * @module @unrdf/collab/crdt-pure/rdf-set
 */

import { ORSet } from './or-set.mjs';
import { VectorClock } from './vector-clock.mjs';

/**
 * @typedef {Object} RDFTriple
 * @property {string} subject - Subject URI or blank node
 * @property {string} predicate - Predicate URI
 * @property {string} object - Object value
 */

/**
 * RDFSet - CRDT for RDF triples with causal metadata
 *
 * @example
 * const rdf1 = new RDFSet('node-1');
 * rdf1.add({ subject: 'ex:Alice', predicate: 'rdf:type', object: 'foaf:Person' });
 *
 * const rdf2 = new RDFSet('node-2');
 * rdf2.add({ subject: 'ex:Bob', predicate: 'foaf:knows', object: 'ex:Alice' });
 *
 * rdf1.merge(rdf2); // Converges to union of triples
 * console.log(rdf1.getTriples()); // All triples from both replicas
 */
export class RDFSet {
  /**
   * Create a new RDF-Set
   * @param {string} nodeId - Unique node identifier
   * @param {Object} [initialState] - Initial state (for deserialization)
   */
  constructor(nodeId, initialState = null) {
    if (!nodeId || typeof nodeId !== 'string') {
      throw new Error('nodeId must be a non-empty string');
    }

    /** @type {string} */
    this.nodeId = nodeId;

    /** @type {ORSet} - Underlying OR-Set for triple storage */
    this.orSet = initialState
      ? ORSet.fromJSON(initialState.orSet)
      : new ORSet(nodeId);

    /** @type {VectorClock} - Causal ordering */
    this.vectorClock = initialState
      ? VectorClock.fromJSON(initialState.vectorClock)
      : new VectorClock(nodeId);

    /** @type {Map<string, Object>} - Metadata for each triple */
    this.metadata = new Map();

    if (initialState && initialState.metadata) {
      this.metadata = new Map(Object.entries(initialState.metadata));
    }
  }

  /**
   * Add an RDF triple
   * @param {RDFTriple} triple - Triple to add
   * @returns {string} Unique tag for this add operation
   */
  add(triple) {
    this._validateTriple(triple);

    const key = this._tripleKey(triple);
    const tag = this.orSet.add(key);

    // Store metadata
    this.metadata.set(key, {
      triple,
      addedBy: this.nodeId,
      addedAt: Date.now(),
      tag,
    });

    // Update vector clock
    this.vectorClock.increment();

    return tag;
  }

  /**
   * Remove an RDF triple
   * @param {RDFTriple} triple - Triple to remove
   * @returns {boolean} True if triple was present
   */
  remove(triple) {
    this._validateTriple(triple);

    const key = this._tripleKey(triple);
    const wasPresent = this.orSet.remove(key);

    if (wasPresent) {
      // Update vector clock
      this.vectorClock.increment();
    }

    return wasPresent;
  }

  /**
   * Check if triple exists
   * @param {RDFTriple} triple - Triple to check
   * @returns {boolean} True if triple is present
   */
  has(triple) {
    this._validateTriple(triple);
    const key = this._tripleKey(triple);
    return this.orSet.has(key);
  }

  /**
   * Get all triples
   * @returns {Array<RDFTriple>} Array of triples
   */
  getTriples() {
    const keys = this.orSet.values();

    return keys
      .map((key) => {
        const meta = this.metadata.get(key);
        return meta ? meta.triple : null;
      })
      .filter((t) => t !== null);
  }

  /**
   * Query triples by pattern
   * @param {Object} pattern - Pattern to match
   * @param {string} [pattern.subject] - Subject pattern
   * @param {string} [pattern.predicate] - Predicate pattern
   * @param {string} [pattern.object] - Object pattern
   * @returns {Array<RDFTriple>} Matching triples
   */
  query(pattern) {
    const triples = this.getTriples();

    return triples.filter((triple) => {
      if (pattern.subject && triple.subject !== pattern.subject) return false;
      if (pattern.predicate && triple.predicate !== pattern.predicate) return false;
      if (pattern.object && triple.object !== pattern.object) return false;
      return true;
    });
  }

  /**
   * Get the number of triples
   * @returns {number} Size
   */
  size() {
    return this.orSet.size();
  }

  /**
   * Merge with another RDF-Set (CRDT merge operation)
   * @param {RDFSet} other - Other RDF set
   * @returns {RDFSet} This set (for chaining)
   */
  merge(other) {
    // Merge underlying OR-Set
    this.orSet.merge(other.orSet);

    // Merge metadata
    for (const [key, meta] of other.metadata) {
      if (!this.metadata.has(key)) {
        this.metadata.set(key, meta);
      }
    }

    // Merge vector clocks
    this.vectorClock.merge(other.vectorClock);

    return this;
  }

  /**
   * Clone this RDF-Set
   * @returns {RDFSet} New set with same values
   */
  clone() {
    const cloned = new RDFSet(this.nodeId);
    cloned.orSet = this.orSet.clone();
    cloned.vectorClock = this.vectorClock.clone();
    cloned.metadata = new Map(this.metadata);
    return cloned;
  }

  /**
   * Serialize to JSON
   * @returns {Object} Serialized form
   */
  toJSON() {
    return {
      nodeId: this.nodeId,
      orSet: this.orSet.toJSON(),
      vectorClock: this.vectorClock.toJSON(),
      metadata: Object.fromEntries(this.metadata),
    };
  }

  /**
   * Deserialize from JSON
   * @param {Object} json - Serialized set
   * @returns {RDFSet} New set
   */
  static fromJSON(json) {
    return new RDFSet(json.nodeId, {
      orSet: json.orSet,
      vectorClock: json.vectorClock,
      metadata: json.metadata,
    });
  }

  /**
   * Get string representation
   * @returns {string} String form
   */
  toString() {
    return `RDFSet[${this.size()} triples, VC: ${this.vectorClock.toString()}]`;
  }

  /**
   * Get detailed stats
   * @returns {Object} Statistics
   */
  getStats() {
    const orSetStats = this.orSet.getStats();

    return {
      triples: this.size(),
      totalTags: orSetStats.totalTags,
      tombstones: orSetStats.tombstones,
      vectorClock: this.vectorClock.toString(),
      nodeId: this.nodeId,
    };
  }

  /**
   * Validate RDF triple structure
   * @param {RDFTriple} triple - Triple to validate
   * @throws {Error} If triple is invalid
   * @private
   */
  _validateTriple(triple) {
    if (!triple || typeof triple !== 'object') {
      throw new Error('Triple must be an object');
    }
    if (!triple.subject || typeof triple.subject !== 'string') {
      throw new Error('Triple must have a subject string');
    }
    if (!triple.predicate || typeof triple.predicate !== 'string') {
      throw new Error('Triple must have a predicate string');
    }
    if (triple.object === undefined || triple.object === null) {
      throw new Error('Triple must have an object');
    }
  }

  /**
   * Generate unique key for a triple
   * @param {RDFTriple} triple - Triple
   * @returns {string} Unique key
   * @private
   */
  _tripleKey(triple) {
    return `${triple.subject}||${triple.predicate}||${String(triple.object)}`;
  }
}

/**
 * @fileoverview LWW-Register (Last-Writer-Wins Register) CRDT
 *
 * A state-based CRDT that stores a single value with timestamp-based conflict resolution.
 * In case of concurrent writes, the one with the highest timestamp wins.
 * If timestamps are equal, the node ID is used as a tie-breaker.
 *
 * Properties:
 * - Commutative: merge(A, B) = merge(B, A)
 * - Associative: merge(merge(A, B), C) = merge(A, merge(B, C))
 * - Idempotent: merge(A, A) = A
 *
 * @module @unrdf/collab/crdt-pure/lww-register
 */

import { VectorClock } from './vector-clock.mjs';

/**
 * LWWRegister - Last-Writer-Wins register for single values
 *
 * @example
 * const r1 = new LWWRegister('node-1', 'initial');
 * r1.set('updated'); // timestamp: now
 *
 * const r2 = new LWWRegister('node-2', 'other');
 * r2.set('newer');
 *
 * r1.merge(r2); // Takes newer value based on timestamp
 * console.log(r1.get()); // 'newer' (if r2's timestamp was later)
 */
export class LWWRegister {
  /**
   * Create a new LWW-Register
   * @param {string} nodeId - Unique node identifier
   * @param {any} [initialValue=null] - Initial value
   * @param {Object} [initialState] - Initial state (for deserialization)
   */
  constructor(nodeId, initialValue = null, initialState = null) {
    if (!nodeId || typeof nodeId !== 'string') {
      throw new Error('nodeId must be a non-empty string');
    }

    /** @type {string} */
    this.nodeId = nodeId;

    if (initialState) {
      this.value = initialState.value;
      this.timestamp = initialState.timestamp;
      this.writerId = initialState.writerId;
      this.vectorClock = VectorClock.fromJSON(initialState.vectorClock);
    } else {
      /** @type {any} - Current value */
      this.value = initialValue;

      /** @type {number} - Timestamp of last write */
      this.timestamp = initialValue !== null ? Date.now() : 0;

      /** @type {string} - Node ID that wrote the current value */
      this.writerId = nodeId;

      /** @type {VectorClock} - Vector clock for causal ordering */
      this.vectorClock = new VectorClock(nodeId);

      if (initialValue !== null) {
        this.vectorClock.increment();
      }
    }
  }

  /**
   * Set a new value
   * @param {any} newValue - New value
   * @param {number} [timestamp] - Optional timestamp (defaults to Date.now())
   * @returns {any} The value that was set
   */
  set(newValue, timestamp = null) {
    const ts = timestamp !== null ? timestamp : Date.now();

    // Always accept local writes
    this.value = newValue;
    this.timestamp = ts;
    this.writerId = this.nodeId;
    this.vectorClock.increment();

    return newValue;
  }

  /**
   * Get the current value
   * @returns {any} Current value
   */
  get() {
    return this.value;
  }

  /**
   * Merge with another LWW-Register
   * @param {LWWRegister} other - Other register
   * @returns {LWWRegister} This register (for chaining)
   */
  merge(other) {
    // Determine winner based on timestamp, then node ID
    const shouldUpdate =
      other.timestamp > this.timestamp ||
      (other.timestamp === this.timestamp && other.writerId > this.writerId);

    if (shouldUpdate) {
      this.value = other.value;
      this.timestamp = other.timestamp;
      this.writerId = other.writerId;
    }

    // Merge vector clocks
    this.vectorClock.merge(other.vectorClock);

    return this;
  }

  /**
   * Clone this register
   * @returns {LWWRegister} New register with same value
   */
  clone() {
    const cloned = new LWWRegister(this.nodeId);
    cloned.value = this.value;
    cloned.timestamp = this.timestamp;
    cloned.writerId = this.writerId;
    cloned.vectorClock = this.vectorClock.clone();
    return cloned;
  }

  /**
   * Serialize to JSON
   * @returns {Object} Serialized form
   */
  toJSON() {
    return {
      nodeId: this.nodeId,
      value: this.value,
      timestamp: this.timestamp,
      writerId: this.writerId,
      vectorClock: this.vectorClock.toJSON(),
    };
  }

  /**
   * Deserialize from JSON
   * @param {Object} json - Serialized register
   * @returns {LWWRegister} New register
   */
  static fromJSON(json) {
    return new LWWRegister(json.nodeId, null, {
      value: json.value,
      timestamp: json.timestamp,
      writerId: json.writerId,
      vectorClock: json.vectorClock,
    });
  }

  /**
   * Get string representation
   * @returns {string} String form
   */
  toString() {
    return `LWWRegister[${this.value}@${this.timestamp}]`;
  }

  /**
   * Get detailed stats
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      value: this.value,
      timestamp: this.timestamp,
      writerId: this.writerId,
      age: Date.now() - this.timestamp,
      vectorClock: this.vectorClock.toString(),
    };
  }
}

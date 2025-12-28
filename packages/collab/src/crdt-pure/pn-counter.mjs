/**
 * @fileoverview PN-Counter (Positive-Negative Counter) CRDT
 *
 * A state-based CRDT that supports both increments and decrements.
 * Implemented as two G-Counters: one for increments, one for decrements.
 * Value = increments - decrements.
 *
 * Properties:
 * - Commutative: merge(A, B) = merge(B, A)
 * - Associative: merge(merge(A, B), C) = merge(A, merge(B, C))
 * - Idempotent: merge(A, A) = A
 *
 * @module @unrdf/collab/crdt-pure/pn-counter
 */

import { GCounter } from './g-counter.mjs';

/**
 * PNCounter - Counter supporting increments and decrements
 *
 * @example
 * const c1 = new PNCounter('node-1');
 * c1.increment(10); // +10
 * c1.decrement(3);  // -3
 * console.log(c1.value()); // 7
 *
 * const c2 = new PNCounter('node-2');
 * c2.increment(5); // +5
 *
 * c1.merge(c2);
 * console.log(c1.value()); // 12 (10 - 3 + 5)
 */
export class PNCounter {
  /**
   * Create a new PN-Counter
   * @param {string} nodeId - Unique node identifier
   * @param {Object} [initialState] - Initial state (for deserialization)
   */
  constructor(nodeId, initialState = null) {
    if (!nodeId || typeof nodeId !== 'string') {
      throw new Error('nodeId must be a non-empty string');
    }

    /** @type {string} */
    this.nodeId = nodeId;

    if (initialState) {
      this.increments = new GCounter(nodeId, initialState.increments);
      this.decrements = new GCounter(nodeId, initialState.decrements);
    } else {
      /** @type {GCounter} - Tracks increments */
      this.increments = new GCounter(nodeId);

      /** @type {GCounter} - Tracks decrements */
      this.decrements = new GCounter(nodeId);
    }
  }

  /**
   * Increment the counter
   * @param {number} [amount=1] - Amount to increment (must be positive)
   * @returns {number} New total value
   */
  increment(amount = 1) {
    if (amount < 0) {
      throw new Error('Increment amount must be positive');
    }

    this.increments.increment(amount);
    return this.value();
  }

  /**
   * Decrement the counter
   * @param {number} [amount=1] - Amount to decrement (must be positive)
   * @returns {number} New total value
   */
  decrement(amount = 1) {
    if (amount < 0) {
      throw new Error('Decrement amount must be positive');
    }

    this.decrements.increment(amount);
    return this.value();
  }

  /**
   * Get the current value (increments - decrements)
   * @returns {number} Total value
   */
  value() {
    return this.increments.value() - this.decrements.value();
  }

  /**
   * Merge with another PN-Counter
   * @param {PNCounter} other - Other counter
   * @returns {PNCounter} This counter (for chaining)
   */
  merge(other) {
    this.increments.merge(other.increments);
    this.decrements.merge(other.decrements);
    return this;
  }

  /**
   * Clone this counter
   * @returns {PNCounter} New counter with same values
   */
  clone() {
    const cloned = new PNCounter(this.nodeId);
    cloned.increments = this.increments.clone();
    cloned.decrements = this.decrements.clone();
    return cloned;
  }

  /**
   * Serialize to JSON
   * @returns {Object} Serialized form
   */
  toJSON() {
    return {
      nodeId: this.nodeId,
      increments: this.increments.toJSON().counters,
      decrements: this.decrements.toJSON().counters,
    };
  }

  /**
   * Deserialize from JSON
   * @param {Object} json - Serialized counter
   * @returns {PNCounter} New counter
   */
  static fromJSON(json) {
    const increments = new Map(Object.entries(json.increments).map(([k, v]) => [k, Number(v)]));
    const decrements = new Map(Object.entries(json.decrements).map(([k, v]) => [k, Number(v)]));

    return new PNCounter(json.nodeId, { increments, decrements });
  }

  /**
   * Get string representation
   * @returns {string} String form
   */
  toString() {
    return `PNCounter[${this.value()}]`;
  }

  /**
   * Get detailed stats
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      value: this.value(),
      increments: this.increments.value(),
      decrements: this.decrements.value(),
      nodeId: this.nodeId,
    };
  }
}

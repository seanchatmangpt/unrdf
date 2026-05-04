/**
 * @fileoverview G-Counter (Grow-only Counter) CRDT
 *
 * A simple state-based CRDT that only supports increments.
 * Each node maintains its own counter; the total is the sum of all counters.
 *
 * Properties:
 * - Commutative: merge(A, B) = merge(B, A)
 * - Associative: merge(merge(A, B), C) = merge(A, merge(B, C))
 * - Idempotent: merge(A, A) = A
 *
 * @module @unrdf/collab/crdt-pure/g-counter
 */

/**
 * GCounter - Grow-only counter (increment-only)
 *
 * @example
 * const c1 = new GCounter('node-1');
 * c1.increment(5); // node-1: 5
 *
 * const c2 = new GCounter('node-2');
 * c2.increment(3); // node-2: 3
 *
 * c1.merge(c2); // node-1: 5, node-2: 3
 * console.log(c1.value()); // 8
 */
export class GCounter {
  /**
   * Create a new G-Counter
   * @param {string} nodeId - Unique node identifier
   * @param {Map<string, number>} [initialState] - Initial state (for deserialization)
   */
  constructor(nodeId, initialState = null) {
    if (!nodeId || typeof nodeId !== 'string') {
      throw new Error('nodeId must be a non-empty string');
    }

    /** @type {string} */
    this.nodeId = nodeId;

    /** @type {Map<string, number>} - Map of nodeId -> counter value */
    this.counters = new Map();

    if (initialState) {
      this.counters = new Map(initialState);
    } else {
      this.counters.set(nodeId, 0);
    }
  }

  /**
   * Increment this node's counter
   * @param {number} [amount=1] - Amount to increment (must be positive)
   * @returns {number} New counter value for this node
   */
  increment(amount = 1) {
    if (amount < 0) {
      throw new Error('G-Counter only supports positive increments');
    }

    const current = this.counters.get(this.nodeId) || 0;
    const newValue = current + amount;
    this.counters.set(this.nodeId, newValue);

    return newValue;
  }

  /**
   * Get the current total value (sum of all node counters)
   * @returns {number} Total value
   */
  value() {
    let total = 0;
    for (const value of this.counters.values()) {
      total += value;
    }
    return total;
  }

  /**
   * Merge with another G-Counter (take max of each node)
   * @param {GCounter} other - Other counter
   * @returns {GCounter} This counter (for chaining)
   */
  merge(other) {
    for (const [nodeId, otherValue] of other.counters) {
      const currentValue = this.counters.get(nodeId) || 0;
      this.counters.set(nodeId, Math.max(currentValue, otherValue));
    }

    return this;
  }

  /**
   * Compare with another G-Counter
   * @param {GCounter} other - Other counter
   * @returns {string} 'LESS_THAN' | 'GREATER_THAN' | 'EQUAL' | 'CONCURRENT'
   */
  compare(other) {
    const allNodes = new Set([...this.counters.keys(), ...other.counters.keys()]);

    let hasGreater = false;
    let hasLess = false;

    for (const nodeId of allNodes) {
      const thisValue = this.counters.get(nodeId) || 0;
      const otherValue = other.counters.get(nodeId) || 0;

      if (thisValue > otherValue) {
        hasGreater = true;
      } else if (thisValue < otherValue) {
        hasLess = true;
      }
    }

    if (hasGreater && hasLess) return 'CONCURRENT';
    if (hasGreater) return 'GREATER_THAN';
    if (hasLess) return 'LESS_THAN';
    return 'EQUAL';
  }

  /**
   * Clone this counter
   * @returns {GCounter} New counter with same values
   */
  clone() {
    return new GCounter(this.nodeId, new Map(this.counters));
  }

  /**
   * Serialize to JSON
   * @returns {Object} Serialized form
   */
  toJSON() {
    return {
      nodeId: this.nodeId,
      counters: Object.fromEntries(this.counters),
    };
  }

  /**
   * Deserialize from JSON
   * @param {Object} json - Serialized counter
   * @returns {GCounter} New counter
   */
  static fromJSON(json) {
    const counters = new Map(Object.entries(json.counters).map(([k, v]) => [k, Number(v)]));
    return new GCounter(json.nodeId, counters);
  }

  /**
   * Get string representation
   * @returns {string} String form
   */
  toString() {
    return `GCounter[${this.value()}]`;
  }
}

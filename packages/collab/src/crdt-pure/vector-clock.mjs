/**
 * @fileoverview Vector Clock for causal ordering in distributed systems
 *
 * Vector clocks track causality between events in a distributed system.
 * Each node maintains a vector of logical clocks for all nodes in the system.
 *
 * Properties:
 * - Happens-before relationship: a -> b iff VC(a) < VC(b)
 * - Concurrent events: VC(a) || VC(b) iff neither < nor >
 * - Transitivity: a -> b and b -> c implies a -> c
 *
 * @module @unrdf/collab/crdt-pure/vector-clock
 */

/**
 * VectorClock - Lamport vector clock for causal ordering
 *
 * @example
 * const vc1 = new VectorClock('node-1');
 * vc1.increment(); // node-1: 1
 * vc1.increment(); // node-1: 2
 *
 * const vc2 = new VectorClock('node-2');
 * vc2.merge(vc1); // node-1: 2, node-2: 0
 * vc2.increment(); // node-1: 2, node-2: 1
 *
 * console.log(vc1.isBefore(vc2)); // true (causally before)
 */
export class VectorClock {
  /**
   * Create a new vector clock
   * @param {string} nodeId - Unique node identifier
   */
  constructor(nodeId) {
    if (!nodeId || typeof nodeId !== 'string') {
      throw new Error('nodeId must be a non-empty string');
    }

    /** @type {string} */
    this.nodeId = nodeId;

    /** @type {Map<string, number>} - Map of nodeId -> clock value */
    this.clocks = new Map([[nodeId, 0]]);
  }

  /**
   * Increment this node's clock
   * @returns {number} New clock value
   */
  increment() {
    const current = this.clocks.get(this.nodeId) || 0;
    const newValue = current + 1;
    this.clocks.set(this.nodeId, newValue);
    return newValue;
  }

  /**
   * Get clock value for a node
   * @param {string} nodeId - Node to query
   * @returns {number} Clock value (0 if not present)
   */
  get(nodeId) {
    return this.clocks.get(nodeId) || 0;
  }

  /**
   * Set clock value for a node
   * @param {string} nodeId - Node to update
   * @param {number} value - New clock value
   */
  set(nodeId, value) {
    if (value < 0) {
      throw new Error('Clock value cannot be negative');
    }
    this.clocks.set(nodeId, value);
  }

  /**
   * Merge with another vector clock (take max of each component)
   * @param {VectorClock} other - Other vector clock
   * @returns {VectorClock} This clock (for chaining)
   */
  merge(other) {
    // Merge all clocks from other
    for (const [nodeId, otherValue] of other.clocks) {
      const currentValue = this.clocks.get(nodeId) || 0;
      this.clocks.set(nodeId, Math.max(currentValue, otherValue));
    }

    return this;
  }

  /**
   * Check if this clock is before another (happens-before relationship)
   * @param {VectorClock} other - Other vector clock
   * @returns {boolean} True if this < other
   */
  isBefore(other) {
    let anyLess = false;

    // Get all nodes from both clocks
    const allNodes = new Set([...this.clocks.keys(), ...other.clocks.keys()]);

    for (const nodeId of allNodes) {
      const thisValue = this.get(nodeId);
      const otherValue = other.get(nodeId);

      if (thisValue > otherValue) {
        return false; // Not before
      }
      if (thisValue < otherValue) {
        anyLess = true;
      }
    }

    return anyLess;
  }

  /**
   * Check if this clock is after another
   * @param {VectorClock} other - Other vector clock
   * @returns {boolean} True if this > other
   */
  isAfter(other) {
    return other.isBefore(this);
  }

  /**
   * Check if clocks are concurrent (neither before nor after)
   * @param {VectorClock} other - Other vector clock
   * @returns {boolean} True if concurrent
   */
  isConcurrent(other) {
    return !this.isBefore(other) && !other.isBefore(this) && !this.equals(other);
  }

  /**
   * Check if clocks are equal
   * @param {VectorClock} other - Other vector clock
   * @returns {boolean} True if equal
   */
  equals(other) {
    const allNodes = new Set([...this.clocks.keys(), ...other.clocks.keys()]);

    for (const nodeId of allNodes) {
      if (this.get(nodeId) !== other.get(nodeId)) {
        return false;
      }
    }

    return true;
  }

  /**
   * Clone this vector clock
   * @returns {VectorClock} New vector clock with same values
   */
  clone() {
    const cloned = new VectorClock(this.nodeId);
    cloned.clocks = new Map(this.clocks);
    return cloned;
  }

  /**
   * Serialize to JSON-compatible object
   * @returns {Object} Serialized form
   */
  toJSON() {
    return {
      nodeId: this.nodeId,
      clocks: Object.fromEntries(this.clocks),
    };
  }

  /**
   * Deserialize from JSON
   * @param {Object} json - Serialized vector clock
   * @returns {VectorClock} New vector clock
   */
  static fromJSON(json) {
    const vc = new VectorClock(json.nodeId);
    vc.clocks = new Map(Object.entries(json.clocks).map(([k, v]) => [k, Number(v)]));
    return vc;
  }

  /**
   * Get string representation
   * @returns {string} String form
   */
  toString() {
    const entries = Array.from(this.clocks.entries())
      .sort((a, b) => a[0].localeCompare(b[0]))
      .map(([node, value]) => `${node}:${value}`)
      .join(', ');
    return `VC[${entries}]`;
  }
}

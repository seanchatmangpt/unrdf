/**
 * @file crdt.mjs
 * @description Conflict-Free Replicated Data Types for distributed synchronization
 *
 * CRDT Properties:
 * - Commutative: merge(A, B) = merge(B, A)
 * - Associative: merge(merge(A, B), C) = merge(A, merge(B, C))
 * - Idempotent: merge(A, A) = A
 *
 * Types Implemented:
 * - G-Set: Grow-only set (add-only)
 * - 2P-Set: Two-phase set (add + remove)
 * - LWW-Element-Set: Last-Write-Wins element set
 * - OR-Set: Observed-Remove set
 *
 * Use for artifact archive A_τ synchronization across swarm nodes
 *
 * @module @unrdf/kgc-swarm/consensus/crdt
 */

import { z } from 'zod';

/**
 * Vector clock for causality tracking
 * @typedef {Object} VectorClock
 * @property {Map<string, number>} clock - Node ID -> logical timestamp
 */
const VectorClockSchema = z.object({
  clock: z.map(z.string(), z.number().int().nonnegative()),
});

/**
 * Timestamped element for LWW-Set
 * @typedef {Object} TimestampedElement
 * @property {any} element - The element value
 * @property {number} timestamp - Logical timestamp
 * @property {string} nodeId - Node that added this element
 */
const TimestampedElementSchema = z.object({
  element: z.any(),
  timestamp: z.number(),
  nodeId: z.string(),
});

/**
 * Tagged element for OR-Set
 * @typedef {Object} TaggedElement
 * @property {any} element - The element value
 * @property {string[]} tags - Unique tags (UUIDs)
 */
const TaggedElementSchema = z.object({
  element: z.any(),
  tags: z.array(z.string()),
});

/**
 * VectorClock: Tracks causality in distributed system
 *
 * Properties:
 * - Monotonic: Timestamps only increase
 * - Causal ordering: A < B iff A causally precedes B
 * - Concurrent detection: A || B iff neither A < B nor B < A
 *
 * @class VectorClock
 * @example
 * ```javascript
 * const clock1 = new VectorClock('node-1');
 * const clock2 = new VectorClock('node-2');
 *
 * clock1.increment(); // node-1: 1
 * clock2.increment(); // node-2: 1
 *
 * clock1.merge(clock2); // node-1: 1, node-2: 1
 * console.log(clock1.compare(clock2)); // 'concurrent'
 * ```
 */
export class VectorClock {
  /**
   * @param {string} nodeId - This node's identifier
   */
  constructor(nodeId) {
    /** @type {string} */
    this.nodeId = nodeId;

    /** @type {Map<string, number>} */
    this.clock = new Map();
    this.clock.set(nodeId, 0);
  }

  /**
   * Increment this node's clock
   * @returns {number} New timestamp
   */
  increment() {
    const current = this.clock.get(this.nodeId) || 0;
    this.clock.set(this.nodeId, current + 1);
    return current + 1;
  }

  /**
   * Get timestamp for a node
   * @param {string} nodeId - Node identifier
   * @returns {number} Timestamp
   */
  get(nodeId) {
    return this.clock.get(nodeId) || 0;
  }

  /**
   * Update timestamp for a node
   * @param {string} nodeId - Node identifier
   * @param {number} timestamp - New timestamp
   */
  set(nodeId, timestamp) {
    this.clock.set(nodeId, Math.max(this.get(nodeId), timestamp));
  }

  /**
   * Merge another vector clock (take maximum of each component)
   * @param {VectorClock} other - Other vector clock
   */
  merge(other) {
    for (const [nodeId, timestamp] of other.clock) {
      this.set(nodeId, timestamp);
    }
  }

  /**
   * Compare this clock with another
   * @param {VectorClock} other - Other vector clock
   * @returns {'before' | 'after' | 'concurrent' | 'equal'} Comparison result
   */
  compare(other) {
    let before = false;
    let after = false;

    const allNodes = new Set([...this.clock.keys(), ...other.clock.keys()]);

    for (const nodeId of allNodes) {
      const thisTime = this.get(nodeId);
      const otherTime = other.get(nodeId);

      if (thisTime < otherTime) before = true;
      if (thisTime > otherTime) after = true;
    }

    if (!before && !after) return 'equal';
    if (before && !after) return 'before';
    if (!before && after) return 'after';
    return 'concurrent';
  }

  /**
   * Clone this vector clock
   * @returns {VectorClock} Cloned clock
   */
  clone() {
    const cloned = new VectorClock(this.nodeId);
    cloned.clock = new Map(this.clock);
    return cloned;
  }

  /**
   * Serialize to JSON
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      nodeId: this.nodeId,
      clock: Object.fromEntries(this.clock),
    };
  }

  /**
   * Deserialize from JSON
   * @param {Object} json - JSON representation
   * @returns {VectorClock} Vector clock
   */
  static fromJSON(json) {
    const clock = new VectorClock(json.nodeId);
    clock.clock = new Map(Object.entries(json.clock));
    return clock;
  }
}

/**
 * GSet: Grow-only set (add-only, no removes)
 *
 * Properties:
 * - Commutative: merge(A, B) = merge(B, A)
 * - Idempotent: merge(A, A) = A
 * - Monotonic: Elements only added, never removed
 *
 * @class GSet
 * @example
 * ```javascript
 * const set1 = new GSet();
 * const set2 = new GSet();
 *
 * set1.add('a');
 * set2.add('b');
 *
 * set1.merge(set2);
 * console.log(set1.has('b')); // true
 * ```
 */
export class GSet {
  constructor() {
    /** @type {Set<any>} */
    this.elements = new Set();
  }

  /**
   * Add element to set
   * @param {any} element - Element to add
   * @returns {boolean} True if element was added
   */
  add(element) {
    const sizeBefore = this.elements.size;
    this.elements.add(JSON.stringify(element)); // Serialize for set equality
    return this.elements.size > sizeBefore;
  }

  /**
   * Check if set contains element
   * @param {any} element - Element to check
   * @returns {boolean} True if element exists
   */
  has(element) {
    return this.elements.has(JSON.stringify(element));
  }

  /**
   * Get all elements
   * @returns {any[]} Array of elements
   */
  values() {
    return Array.from(this.elements).map(e => JSON.parse(e));
  }

  /**
   * Merge another GSet (union)
   * @param {GSet} other - Other GSet
   */
  merge(other) {
    for (const element of other.elements) {
      this.elements.add(element);
    }
  }

  /**
   * Get set size
   * @returns {number} Number of elements
   */
  size() {
    return this.elements.size;
  }

  /**
   * Clone this set
   * @returns {GSet} Cloned set
   */
  clone() {
    const cloned = new GSet();
    cloned.elements = new Set(this.elements);
    return cloned;
  }
}

/**
 * TwoPhaseSet: Add and remove elements (remove wins)
 *
 * Properties:
 * - Elements can be added and removed
 * - Remove is permanent (element cannot be re-added)
 * - Convergent: All replicas converge to same state
 *
 * @class TwoPhaseSet
 * @example
 * ```javascript
 * const set1 = new TwoPhaseSet();
 * const set2 = new TwoPhaseSet();
 *
 * set1.add('a');
 * set2.remove('a'); // Concurrent with add
 *
 * set1.merge(set2);
 * console.log(set1.has('a')); // false - remove wins
 * ```
 */
export class TwoPhaseSet {
  constructor() {
    /** @type {GSet} - Added elements */
    this.added = new GSet();

    /** @type {GSet} - Removed elements */
    this.removed = new GSet();
  }

  /**
   * Add element to set
   * @param {any} element - Element to add
   * @returns {boolean} True if added (not previously removed)
   */
  add(element) {
    if (this.removed.has(element)) {
      return false; // Cannot re-add removed element
    }
    return this.added.add(element);
  }

  /**
   * Remove element from set
   * @param {any} element - Element to remove
   * @returns {boolean} True if removed
   */
  remove(element) {
    if (!this.added.has(element)) {
      return false; // Cannot remove non-existent element
    }
    return this.removed.add(element);
  }

  /**
   * Check if set contains element
   * @param {any} element - Element to check
   * @returns {boolean} True if in added set and not in removed set
   */
  has(element) {
    return this.added.has(element) && !this.removed.has(element);
  }

  /**
   * Get all elements
   * @returns {any[]} Array of elements
   */
  values() {
    const addedElements = this.added.values();
    return addedElements.filter(e => !this.removed.has(e));
  }

  /**
   * Merge another 2P-Set
   * @param {TwoPhaseSet} other - Other 2P-Set
   */
  merge(other) {
    this.added.merge(other.added);
    this.removed.merge(other.removed);
  }

  /**
   * Get set size
   * @returns {number} Number of elements
   */
  size() {
    return this.values().length;
  }

  /**
   * Clone this set
   * @returns {TwoPhaseSet} Cloned set
   */
  clone() {
    const cloned = new TwoPhaseSet();
    cloned.added = this.added.clone();
    cloned.removed = this.removed.clone();
    return cloned;
  }
}

/**
 * LWWElementSet: Last-Write-Wins element set
 *
 * Properties:
 * - Elements have timestamps
 * - Concurrent add/remove: higher timestamp wins
 * - Same timestamp: bias towards add or remove (configurable)
 *
 * @class LWWElementSet
 * @example
 * ```javascript
 * const set1 = new LWWElementSet('node-1');
 * const set2 = new LWWElementSet('node-2');
 *
 * set1.add('a'); // timestamp: 100
 * set2.remove('a'); // timestamp: 200
 *
 * set1.merge(set2);
 * console.log(set1.has('a')); // false - remove has higher timestamp
 * ```
 */
export class LWWElementSet {
  /**
   * @param {string} nodeId - This node's identifier
   * @param {Object} [options] - Options
   * @param {boolean} [options.biasAdd=true] - On tie, bias towards add (else remove)
   */
  constructor(nodeId, options = {}) {
    /** @type {string} */
    this.nodeId = nodeId;

    /** @type {boolean} */
    this.biasAdd = options.biasAdd !== false;

    /** @type {Map<string, TimestampedElement>} - Added elements */
    this.added = new Map();

    /** @type {Map<string, TimestampedElement>} - Removed elements */
    this.removed = new Map();

    /** @type {VectorClock} */
    this.clock = new VectorClock(nodeId);
  }

  /**
   * Add element to set
   * @param {any} element - Element to add
   * @returns {TimestampedElement} Timestamped element
   */
  add(element) {
    const timestamp = this.clock.increment();
    const key = JSON.stringify(element);

    const timestampedElement = {
      element,
      timestamp,
      nodeId: this.nodeId,
    };

    this.added.set(key, timestampedElement);
    return timestampedElement;
  }

  /**
   * Remove element from set
   * @param {any} element - Element to remove
   * @returns {TimestampedElement} Timestamped element
   */
  remove(element) {
    const timestamp = this.clock.increment();
    const key = JSON.stringify(element);

    const timestampedElement = {
      element,
      timestamp,
      nodeId: this.nodeId,
    };

    this.removed.set(key, timestampedElement);
    return timestampedElement;
  }

  /**
   * Check if set contains element
   * @param {any} element - Element to check
   * @returns {boolean} True if element exists
   */
  has(element) {
    const key = JSON.stringify(element);

    const addEntry = this.added.get(key);
    const removeEntry = this.removed.get(key);

    if (!addEntry) return false;
    if (!removeEntry) return true;

    // Compare timestamps
    if (addEntry.timestamp > removeEntry.timestamp) return true;
    if (addEntry.timestamp < removeEntry.timestamp) return false;

    // Tie - use bias
    return this.biasAdd;
  }

  /**
   * Get all elements
   * @returns {any[]} Array of elements
   */
  values() {
    const elements = [];

    for (const [key, addEntry] of this.added) {
      const removeEntry = this.removed.get(key);

      let include = true;
      if (removeEntry) {
        if (addEntry.timestamp < removeEntry.timestamp) {
          include = false;
        } else if (addEntry.timestamp === removeEntry.timestamp && !this.biasAdd) {
          include = false;
        }
      }

      if (include) {
        elements.push(addEntry.element);
      }
    }

    return elements;
  }

  /**
   * Merge another LWW-Element-Set
   * @param {LWWElementSet} other - Other LWW-Element-Set
   */
  merge(other) {
    // Merge vector clocks
    this.clock.merge(other.clock);

    // Merge added sets (keep higher timestamp)
    for (const [key, otherEntry] of other.added) {
      const thisEntry = this.added.get(key);

      if (!thisEntry || otherEntry.timestamp > thisEntry.timestamp) {
        this.added.set(key, otherEntry);
      }
    }

    // Merge removed sets (keep higher timestamp)
    for (const [key, otherEntry] of other.removed) {
      const thisEntry = this.removed.get(key);

      if (!thisEntry || otherEntry.timestamp > thisEntry.timestamp) {
        this.removed.set(key, otherEntry);
      }
    }
  }

  /**
   * Get set size
   * @returns {number} Number of elements
   */
  size() {
    return this.values().length;
  }

  /**
   * Clone this set
   * @returns {LWWElementSet} Cloned set
   */
  clone() {
    const cloned = new LWWElementSet(this.nodeId, { biasAdd: this.biasAdd });
    cloned.added = new Map(this.added);
    cloned.removed = new Map(this.removed);
    cloned.clock = this.clock.clone();
    return cloned;
  }
}

/**
 * ORSet: Observed-Remove set (most general add/remove CRDT)
 *
 * Properties:
 * - Elements tagged with unique identifiers
 * - Remove only removes observed tags
 * - Concurrent add wins over remove
 * - Can re-add removed elements
 *
 * @class ORSet
 * @example
 * ```javascript
 * const set1 = new ORSet();
 * const set2 = new ORSet();
 *
 * const tag1 = set1.add('a');
 * set2.remove('a', [tag1]); // Remove observed tag
 * set1.add('a'); // Re-add with new tag
 *
 * set1.merge(set2);
 * console.log(set1.has('a')); // true - new add wins
 * ```
 */
export class ORSet {
  constructor() {
    /** @type {Map<string, Set<string>>} - element -> tags */
    this.elements = new Map();

    /** @type {Set<string>} - Tombstone tags (removed) */
    this.tombstones = new Set();
  }

  /**
   * Add element to set
   * @param {any} element - Element to add
   * @returns {string} Unique tag for this add
   */
  add(element) {
    const key = JSON.stringify(element);
    const tag = crypto.randomUUID();

    if (!this.elements.has(key)) {
      this.elements.set(key, new Set());
    }

    this.elements.get(key).add(tag);
    return tag;
  }

  /**
   * Remove element from set (removes all observed tags)
   * @param {any} element - Element to remove
   * @param {string[]} [observedTags] - Tags to remove (default: all current tags)
   * @returns {string[]} Removed tags
   */
  remove(element, observedTags) {
    const key = JSON.stringify(element);
    const tags = this.elements.get(key);

    if (!tags) return [];

    const tagsToRemove = observedTags || Array.from(tags);

    for (const tag of tagsToRemove) {
      if (tags.has(tag)) {
        this.tombstones.add(tag);
        tags.delete(tag);
      }
    }

    if (tags.size === 0) {
      this.elements.delete(key);
    }

    return tagsToRemove;
  }

  /**
   * Check if set contains element
   * @param {any} element - Element to check
   * @returns {boolean} True if element has live tags
   */
  has(element) {
    const key = JSON.stringify(element);
    const tags = this.elements.get(key);

    if (!tags || tags.size === 0) return false;

    // Check if any tag is not tombstoned
    for (const tag of tags) {
      if (!this.tombstones.has(tag)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Get all elements
   * @returns {any[]} Array of elements
   */
  values() {
    const elements = [];

    for (const [key, tags] of this.elements) {
      // Check if element has any live tags
      for (const tag of tags) {
        if (!this.tombstones.has(tag)) {
          elements.push(JSON.parse(key));
          break;
        }
      }
    }

    return elements;
  }

  /**
   * Merge another OR-Set
   * @param {ORSet} other - Other OR-Set
   */
  merge(other) {
    // Merge elements (union of tags)
    for (const [key, otherTags] of other.elements) {
      if (!this.elements.has(key)) {
        this.elements.set(key, new Set());
      }

      const thisTags = this.elements.get(key);
      for (const tag of otherTags) {
        thisTags.add(tag);
      }
    }

    // Merge tombstones (union)
    for (const tag of other.tombstones) {
      this.tombstones.add(tag);
    }

    // Clean up tombstoned elements
    for (const [key, tags] of this.elements) {
      for (const tag of tags) {
        if (this.tombstones.has(tag)) {
          tags.delete(tag);
        }
      }

      if (tags.size === 0) {
        this.elements.delete(key);
      }
    }
  }

  /**
   * Get set size
   * @returns {number} Number of elements
   */
  size() {
    return this.values().length;
  }

  /**
   * Clone this set
   * @returns {ORSet} Cloned set
   */
  clone() {
    const cloned = new ORSet();

    for (const [key, tags] of this.elements) {
      cloned.elements.set(key, new Set(tags));
    }

    cloned.tombstones = new Set(this.tombstones);
    return cloned;
  }
}

/**
 * Create CRDT for artifact archive synchronization
 * @param {string} type - CRDT type: 'gset', '2pset', 'lwwset', 'orset'
 * @param {string} [nodeId] - Node identifier (required for lwwset)
 * @param {Object} [options] - Additional options
 * @returns {GSet | TwoPhaseSet | LWWElementSet | ORSet} CRDT instance
 */
export function createCRDT(type, nodeId, options = {}) {
  switch (type) {
    case 'gset':
      return new GSet();

    case '2pset':
      return new TwoPhaseSet();

    case 'lwwset':
      if (!nodeId) throw new Error('nodeId required for LWW-Element-Set');
      return new LWWElementSet(nodeId, options);

    case 'orset':
      return new ORSet();

    default:
      throw new Error(`Unknown CRDT type: ${type}`);
  }
}

// Export all CRDTs
export {
  VectorClockSchema,
  TimestampedElementSchema,
  TaggedElementSchema,
};

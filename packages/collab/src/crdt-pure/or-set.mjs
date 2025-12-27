/**
 * @fileoverview OR-Set (Observed-Remove Set) CRDT
 *
 * A state-based CRDT set that supports add and remove operations.
 * Resolves add/remove conflicts by using unique tags for each add.
 * An element is present if it has at least one non-removed tag.
 *
 * Properties:
 * - Add-wins semantics: concurrent add/remove → add wins
 * - Commutative: merge(A, B) = merge(B, A)
 * - Associative: merge(merge(A, B), C) = merge(A, merge(B, C))
 * - Idempotent: merge(A, A) = A
 *
 * @module @unrdf/collab/crdt-pure/or-set
 */

/**
 * ORSet - Observed-Remove Set with add-wins semantics
 *
 * @example
 * const s1 = new ORSet('node-1');
 * s1.add('apple');
 * s1.add('banana');
 *
 * const s2 = new ORSet('node-2');
 * s2.add('cherry');
 *
 * s1.merge(s2);
 * console.log(s1.values()); // ['apple', 'banana', 'cherry']
 *
 * s1.remove('banana');
 * console.log(s1.has('banana')); // false
 */
export class ORSet {
  /**
   * Create a new OR-Set
   * @param {string} nodeId - Unique node identifier
   * @param {Object} [initialState] - Initial state (for deserialization)
   */
  constructor(nodeId, initialState = null) {
    if (!nodeId || typeof nodeId !== 'string') {
      throw new Error('nodeId must be a non-empty string');
    }

    /** @type {string} */
    this.nodeId = nodeId;

    /** @type {Map<any, Set<string>>} - Map of element -> Set of unique tags */
    this.elements = new Map();

    /** @type {Set<string>} - Set of removed tags (tombstones) */
    this.tombstones = new Set();

    /** @type {number} - Tag counter for unique tags */
    this.tagCounter = 0;

    if (initialState) {
      this.elements = new Map(
        Object.entries(initialState.elements).map(([k, tags]) => [
          k,
          new Set(tags),
        ])
      );
      this.tombstones = new Set(initialState.tombstones);
      this.tagCounter = initialState.tagCounter || 0;
    }
  }

  /**
   * Add an element to the set
   * @param {any} element - Element to add
   * @returns {string} Unique tag for this add operation
   */
  add(element) {
    const tag = this._generateTag();

    if (!this.elements.has(element)) {
      this.elements.set(element, new Set());
    }

    this.elements.get(element).add(tag);

    return tag;
  }

  /**
   * Remove an element from the set
   * @param {any} element - Element to remove
   * @returns {boolean} True if element was present
   */
  remove(element) {
    if (!this.elements.has(element)) {
      return false; // Element not present
    }

    const tags = this.elements.get(element);

    // Add all tags to tombstones
    for (const tag of tags) {
      this.tombstones.add(tag);
    }

    return true;
  }

  /**
   * Check if element is in the set
   * @param {any} element - Element to check
   * @returns {boolean} True if element is present
   */
  has(element) {
    if (!this.elements.has(element)) {
      return false;
    }

    const tags = this.elements.get(element);

    // Element is present if it has at least one non-tombstoned tag
    for (const tag of tags) {
      if (!this.tombstones.has(tag)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Get all elements in the set
   * @returns {Array<any>} Array of elements
   */
  values() {
    const result = [];

    for (const [element, tags] of this.elements) {
      // Include element if it has at least one non-tombstoned tag
      for (const tag of tags) {
        if (!this.tombstones.has(tag)) {
          result.push(element);
          break;
        }
      }
    }

    return result;
  }

  /**
   * Get the number of elements in the set
   * @returns {number} Size
   */
  size() {
    return this.values().length;
  }

  /**
   * Clear the set (WARNING: breaks CRDT properties)
   */
  clear() {
    this.elements.clear();
    this.tombstones.clear();
  }

  /**
   * Merge with another OR-Set
   * @param {ORSet} other - Other set
   * @returns {ORSet} This set (for chaining)
   */
  merge(other) {
    // Merge elements and their tags
    for (const [element, otherTags] of other.elements) {
      if (!this.elements.has(element)) {
        this.elements.set(element, new Set());
      }

      const currentTags = this.elements.get(element);

      for (const tag of otherTags) {
        currentTags.add(tag);
      }
    }

    // Merge tombstones
    for (const tombstone of other.tombstones) {
      this.tombstones.add(tombstone);
    }

    // Update tag counter to avoid conflicts
    this.tagCounter = Math.max(this.tagCounter, other.tagCounter);

    return this;
  }

  /**
   * Clone this set
   * @returns {ORSet} New set with same values
   */
  clone() {
    const cloned = new ORSet(this.nodeId);

    // Deep copy elements
    for (const [element, tags] of this.elements) {
      cloned.elements.set(element, new Set(tags));
    }

    cloned.tombstones = new Set(this.tombstones);
    cloned.tagCounter = this.tagCounter;

    return cloned;
  }

  /**
   * Serialize to JSON
   * @returns {Object} Serialized form
   */
  toJSON() {
    const elementsObj = {};

    for (const [element, tags] of this.elements) {
      elementsObj[element] = Array.from(tags);
    }

    return {
      nodeId: this.nodeId,
      elements: elementsObj,
      tombstones: Array.from(this.tombstones),
      tagCounter: this.tagCounter,
    };
  }

  /**
   * Deserialize from JSON
   * @param {Object} json - Serialized set
   * @returns {ORSet} New set
   */
  static fromJSON(json) {
    return new ORSet(json.nodeId, {
      elements: json.elements,
      tombstones: json.tombstones,
      tagCounter: json.tagCounter,
    });
  }

  /**
   * Get string representation
   * @returns {string} String form
   */
  toString() {
    return `ORSet{${this.values().join(', ')}}`;
  }

  /**
   * Get detailed stats
   * @returns {Object} Statistics
   */
  getStats() {
    let totalTags = 0;
    for (const tags of this.elements.values()) {
      totalTags += tags.size;
    }

    return {
      size: this.size(),
      totalTags,
      tombstones: this.tombstones.size,
      uniqueElements: this.elements.size,
    };
  }

  /**
   * Generate unique tag for this node
   * @returns {string} Unique tag
   * @private
   */
  _generateTag() {
    return `${this.nodeId}-${Date.now()}-${++this.tagCounter}`;
  }
}

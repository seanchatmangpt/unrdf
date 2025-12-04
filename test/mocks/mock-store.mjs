/**
 * @file Mock N3 Store Implementation
 * @description Lightweight mock store for testing without full N3 dependency
 */

/**
 * Mock N3 Store for testing
 */
export class MockStore {
  constructor() {
    this._quads = new Map()
    this._size = 0
  }

  get size() {
    return this._size
  }

  /**
   * Add a quad to the store
   * @param {Object} quad - Quad to add
   */
  addQuad(quad) {
    const key = this._quadKey(quad)
    if (!this._quads.has(key)) {
      this._quads.set(key, quad)
      this._size++
    }
  }

  /**
   * Add multiple quads
   * @param {Object[]} quads - Quads to add
   */
  addQuads(quads) {
    for (const quad of quads) {
      this.addQuad(quad)
    }
  }

  /**
   * Remove a quad from the store
   * @param {Object} quad - Quad to remove
   */
  removeQuad(quad) {
    const key = this._quadKey(quad)
    if (this._quads.has(key)) {
      this._quads.delete(key)
      this._size--
    }
  }

  /**
   * Get matching quads
   * @param {any} subject - Subject pattern
   * @param {any} predicate - Predicate pattern
   * @param {any} object - Object pattern
   * @param {any} graph - Graph pattern
   * @returns {Object[]}
   */
  getQuads(subject, predicate, object, graph) {
    const results = []

    for (const quad of this._quads.values()) {
      if (this._matches(quad, subject, predicate, object, graph)) {
        results.push(quad)
      }
    }

    return results
  }

  /**
   * Check if store contains a quad
   * @param {Object} quad - Quad to check
   * @returns {boolean}
   */
  has(quad) {
    const key = this._quadKey(quad)
    return this._quads.has(key)
  }

  /**
   * Clear all quads
   */
  clear() {
    this._quads.clear()
    this._size = 0
  }

  /**
   * Get all quads as array
   * @returns {Object[]}
   */
  toArray() {
    return Array.from(this._quads.values())
  }

  /**
   * Generate unique key for quad
   * @private
   */
  _quadKey(quad) {
    return JSON.stringify({
      s: this._termValue(quad.subject),
      p: this._termValue(quad.predicate),
      o: this._termValue(quad.object),
      g: this._termValue(quad.graph)
    })
  }

  /**
   * Get term value
   * @private
   */
  _termValue(term) {
    if (!term) return null
    if (term.value) return term.value
    return String(term)
  }

  /**
   * Check if quad matches pattern
   * @private
   */
  _matches(quad, subject, predicate, object, graph) {
    return (
      this._termMatches(quad.subject, subject) &&
      this._termMatches(quad.predicate, predicate) &&
      this._termMatches(quad.object, object) &&
      this._termMatches(quad.graph, graph)
    )
  }

  /**
   * Check if term matches pattern
   * @private
   */
  _termMatches(term, pattern) {
    if (!pattern) return true
    if (!term) return false

    const termValue = this._termValue(term)
    const patternValue = this._termValue(pattern)

    return termValue === patternValue
  }
}

/**
 * Create a mock store with sample data
 * @param {Object[]} [quads=[]] - Initial quads
 * @returns {MockStore}
 */
export function createMockStore(quads = []) {
  const store = new MockStore()
  store.addQuads(quads)
  return store
}

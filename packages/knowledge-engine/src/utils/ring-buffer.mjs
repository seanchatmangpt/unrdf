/**
 * @file ring-buffer.mjs
 * @description TRIZ #10 Prior Action - Ring buffer for efficient streaming operations
 * Pre-allocates fixed memory to avoid array reallocation overhead during high-throughput streaming
 */

/**
 * @typedef {Object} RingBufferOptions
 * @property {number} [maxSize=100] - Maximum number of items in buffer
 */

/**
 * High-performance circular buffer for streaming data
 * O(1) push operations with automatic wraparound, avoiding array slice/shift overhead
 *
 * @template T
 * @example
 * const buffer = new RingBuffer(100);
 * buffer.push({ event: 'added', timestamp: Date.now() });
 * const events = buffer.toArray(); // Returns items in insertion order
 */
export class RingBuffer {
  /**
   * @param {number} [maxSize=100] - Maximum buffer capacity
   */
  constructor(maxSize = 100) {
    /** @type {Array<T|undefined>} */
    this.buffer = new Array(maxSize);
    /** @type {number} - Index where next item will be written */
    this.head = 0;
    /** @type {number} - Index of oldest item */
    this.tail = 0;
    /** @type {number} - Current number of items */
    this.size = 0;
    /** @type {number} - Maximum capacity */
    this.maxSize = maxSize;
  }

  /**
   * Add item to buffer, overwriting oldest if full
   * @param {T} item - Item to add
   * @returns {T|undefined} - Overwritten item if buffer was full, undefined otherwise
   */
  push(item) {
    /** @type {T|undefined} */
    let overwritten;

    if (this.size === this.maxSize) {
      // Buffer full - overwrite oldest item
      overwritten = this.buffer[this.tail];
      this.tail = (this.tail + 1) % this.maxSize;
    } else {
      this.size++;
    }

    this.buffer[this.head] = item;
    this.head = (this.head + 1) % this.maxSize;

    return overwritten;
  }

  /**
   * Remove and return oldest item from buffer
   * @returns {T|undefined} - Oldest item or undefined if empty
   */
  shift() {
    if (this.size === 0) {
      return undefined;
    }

    const item = this.buffer[this.tail];
    this.buffer[this.tail] = undefined; // Allow GC
    this.tail = (this.tail + 1) % this.maxSize;
    this.size--;

    return item;
  }

  /**
   * Get item at index (0 = oldest)
   * @param {number} index - Index from oldest item
   * @returns {T|undefined} - Item at index or undefined
   */
  get(index) {
    if (index < 0 || index >= this.size) {
      return undefined;
    }
    const actualIndex = (this.tail + index) % this.maxSize;
    return this.buffer[actualIndex];
  }

  /**
   * Get most recent item without removing
   * @returns {T|undefined} - Most recent item or undefined if empty
   */
  peek() {
    if (this.size === 0) {
      return undefined;
    }
    const lastIndex = (this.head - 1 + this.maxSize) % this.maxSize;
    return this.buffer[lastIndex];
  }

  /**
   * Get oldest item without removing
   * @returns {T|undefined} - Oldest item or undefined if empty
   */
  peekOldest() {
    if (this.size === 0) {
      return undefined;
    }
    return this.buffer[this.tail];
  }

  /**
   * Convert buffer to array in insertion order (oldest first)
   * @returns {Array<T>} - Array of items
   */
  toArray() {
    if (this.size === 0) {
      return [];
    }

    /** @type {Array<T>} */
    const result = new Array(this.size);

    for (let i = 0; i < this.size; i++) {
      const index = (this.tail + i) % this.maxSize;
      result[i] = /** @type {T} */ (this.buffer[index]);
    }

    return result;
  }

  /**
   * Clear all items from buffer
   */
  clear() {
    // Clear references for GC
    for (let i = 0; i < this.maxSize; i++) {
      this.buffer[i] = undefined;
    }
    this.head = 0;
    this.tail = 0;
    this.size = 0;
  }

  /**
   * Check if buffer is empty
   * @returns {boolean}
   */
  isEmpty() {
    return this.size === 0;
  }

  /**
   * Check if buffer is full
   * @returns {boolean}
   */
  isFull() {
    return this.size === this.maxSize;
  }

  /**
   * Get current number of items
   * @returns {number}
   */
  get length() {
    return this.size;
  }

  /**
   * Get maximum capacity
   * @returns {number}
   */
  get capacity() {
    return this.maxSize;
  }

  /**
   * Get available space
   * @returns {number}
   */
  get available() {
    return this.maxSize - this.size;
  }

  /**
   * Iterate over items (oldest to newest)
   * @yields {T}
   */
  *[Symbol.iterator]() {
    for (let i = 0; i < this.size; i++) {
      const index = (this.tail + i) % this.maxSize;
      yield /** @type {T} */ (this.buffer[index]);
    }
  }

  /**
   * Apply function to each item
   * @param {(item: T, index: number) => void} fn - Function to apply
   */
  forEach(fn) {
    for (let i = 0; i < this.size; i++) {
      const index = (this.tail + i) % this.maxSize;
      fn(/** @type {T} */ (this.buffer[index]), i);
    }
  }

  /**
   * Filter items and return new array
   * @param {(item: T, index: number) => boolean} predicate - Filter function
   * @returns {Array<T>}
   */
  filter(predicate) {
    /** @type {Array<T>} */
    const result = [];
    for (let i = 0; i < this.size; i++) {
      const index = (this.tail + i) % this.maxSize;
      const item = /** @type {T} */ (this.buffer[index]);
      if (predicate(item, i)) {
        result.push(item);
      }
    }
    return result;
  }

  /**
   * Find first item matching predicate
   * @param {(item: T, index: number) => boolean} predicate - Search function
   * @returns {T|undefined}
   */
  find(predicate) {
    for (let i = 0; i < this.size; i++) {
      const index = (this.tail + i) % this.maxSize;
      const item = /** @type {T} */ (this.buffer[index]);
      if (predicate(item, i)) {
        return item;
      }
    }
    return undefined;
  }

  /**
   * Get last N items (most recent)
   * @param {number} n - Number of items
   * @returns {Array<T>}
   */
  last(n) {
    const count = Math.min(n, this.size);
    /** @type {Array<T>} */
    const result = new Array(count);
    const startOffset = this.size - count;

    for (let i = 0; i < count; i++) {
      const index = (this.tail + startOffset + i) % this.maxSize;
      result[i] = /** @type {T} */ (this.buffer[index]);
    }

    return result;
  }

  /**
   * Get first N items (oldest)
   * @param {number} n - Number of items
   * @returns {Array<T>}
   */
  first(n) {
    const count = Math.min(n, this.size);
    /** @type {Array<T>} */
    const result = new Array(count);

    for (let i = 0; i < count; i++) {
      const index = (this.tail + i) % this.maxSize;
      result[i] = /** @type {T} */ (this.buffer[index]);
    }

    return result;
  }
}

export default RingBuffer;

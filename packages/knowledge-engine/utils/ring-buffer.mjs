/**
 * @fileoverview Ring Buffer - Fixed-size circular buffer implementation
 * @module ring-buffer
 * @description Efficient circular buffer for fixed-size data storage with automatic wrapping
 */

/**
 * Ring Buffer implementation
 * @class
 * @description Circular buffer that overwrites oldest items when full
 */
export class RingBuffer {
  /**
   * Creates a new RingBuffer instance
   * @param {number} capacity - Maximum capacity of the buffer
   * @example
   * const buffer = new RingBuffer(5);
   * buffer.push(1);
   * buffer.push(2);
   */
  constructor(capacity) {
    if (!Number.isInteger(capacity) || capacity <= 0) {
      throw new Error('Capacity must be a positive integer');
    }
    this.capacity = capacity;
    this.buffer = new Array(capacity);
    this.head = 0;
    this.tail = 0;
    this._length = 0;
  }

  /**
   * Pushes an item to the buffer
   * @param {*} item - Item to push
   * @returns {*} Item that was overwritten (if full), undefined otherwise
   */
  push(item) {
    let overwritten;

    if (this._length === this.capacity) {
      // Buffer is full, overwrite oldest item
      overwritten = this.buffer[this.head];
      this.head = (this.head + 1) % this.capacity;
    } else {
      this._length++;
    }

    this.buffer[this.tail] = item;
    this.tail = (this.tail + 1) % this.capacity;

    return overwritten;
  }

  /**
   * Gets item at specified index
   * @param {number} index - Index within buffer (0-based from oldest)
   * @returns {*} Item at index
   */
  get(index) {
    if (index < 0 || index >= this._length) {
      throw new Error(`Index ${index} out of bounds [0, ${this._length})`);
    }
    const actualIndex = (this.head + index) % this.capacity;
    return this.buffer[actualIndex];
  }

  /**
   * Gets the current length of the buffer
   * @returns {number} Number of items in buffer
   */
  get length() {
    return this._length;
  }

  /**
   * Checks if buffer is full
   * @returns {boolean} True if buffer is at capacity
   */
  isFull() {
    return this._length === this.capacity;
  }

  /**
   * Clears all items from the buffer
   */
  clear() {
    this.buffer = new Array(this.capacity);
    this.head = 0;
    this.tail = 0;
    this._length = 0;
  }

  /**
   * Converts buffer to array (in order from oldest to newest)
   * @returns {Array} Array of items
   */
  toArray() {
    const result = [];
    for (let i = 0; i < this._length; i++) {
      result.push(this.get(i));
    }
    return result;
  }

  /**
   * Iterates over buffer items from oldest to newest
   */
  *[Symbol.iterator]() {
    for (let i = 0; i < this._length; i++) {
      yield this.get(i);
    }
  }
}

export default { RingBuffer };

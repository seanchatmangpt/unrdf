/**
 * @file ring-buffer.test.mjs
 * @description Tests for TRIZ #10 Prior Action - RingBuffer implementation
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { RingBuffer } from '../../../src/knowledge-engine/utils/ring-buffer.mjs';

describe('RingBuffer (TRIZ #10 Prior Action)', () => {
  /** @type {RingBuffer<number>} */
  let buffer;

  beforeEach(() => {
    buffer = new RingBuffer(5);
  });

  describe('constructor', () => {
    it('should create buffer with default size', () => {
      const defaultBuffer = new RingBuffer();
      expect(defaultBuffer.capacity).toBe(100);
      expect(defaultBuffer.length).toBe(0);
    });

    it('should create buffer with custom size', () => {
      expect(buffer.capacity).toBe(5);
      expect(buffer.length).toBe(0);
    });
  });

  describe('push', () => {
    it('should add items to buffer', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);

      expect(buffer.length).toBe(3);
      expect(buffer.toArray()).toEqual([1, 2, 3]);
    });

    it('should overwrite oldest items when full', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);
      buffer.push(4);
      buffer.push(5);

      // Buffer is now full
      expect(buffer.length).toBe(5);
      expect(buffer.isFull()).toBe(true);

      // Push one more - should overwrite oldest (1)
      const overwritten = buffer.push(6);

      expect(overwritten).toBe(1);
      expect(buffer.length).toBe(5);
      expect(buffer.toArray()).toEqual([2, 3, 4, 5, 6]);
    });

    it('should return undefined when not overwriting', () => {
      const result = buffer.push(1);
      expect(result).toBeUndefined();
    });
  });

  describe('shift', () => {
    it('should remove and return oldest item', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);

      expect(buffer.shift()).toBe(1);
      expect(buffer.length).toBe(2);
      expect(buffer.toArray()).toEqual([2, 3]);
    });

    it('should return undefined for empty buffer', () => {
      expect(buffer.shift()).toBeUndefined();
    });
  });

  describe('get', () => {
    it('should get item at index (0 = oldest)', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);

      expect(buffer.get(0)).toBe(1);
      expect(buffer.get(1)).toBe(2);
      expect(buffer.get(2)).toBe(3);
    });

    it('should return undefined for invalid index', () => {
      buffer.push(1);
      expect(buffer.get(-1)).toBeUndefined();
      expect(buffer.get(1)).toBeUndefined();
      expect(buffer.get(100)).toBeUndefined();
    });
  });

  describe('peek and peekOldest', () => {
    it('should peek at most recent item', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);

      expect(buffer.peek()).toBe(3);
      expect(buffer.length).toBe(3); // Unchanged
    });

    it('should peek at oldest item', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);

      expect(buffer.peekOldest()).toBe(1);
      expect(buffer.length).toBe(3); // Unchanged
    });

    it('should return undefined for empty buffer', () => {
      expect(buffer.peek()).toBeUndefined();
      expect(buffer.peekOldest()).toBeUndefined();
    });
  });

  describe('toArray', () => {
    it('should return items in insertion order', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);

      expect(buffer.toArray()).toEqual([1, 2, 3]);
    });

    it('should return empty array for empty buffer', () => {
      expect(buffer.toArray()).toEqual([]);
    });

    it('should handle wraparound correctly', () => {
      // Fill buffer
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);
      buffer.push(4);
      buffer.push(5);

      // Cause wraparound
      buffer.push(6);
      buffer.push(7);

      expect(buffer.toArray()).toEqual([3, 4, 5, 6, 7]);
    });
  });

  describe('clear', () => {
    it('should reset buffer to empty state', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);

      buffer.clear();

      expect(buffer.length).toBe(0);
      expect(buffer.isEmpty()).toBe(true);
      expect(buffer.toArray()).toEqual([]);
    });
  });

  describe('isEmpty and isFull', () => {
    it('should report empty state', () => {
      expect(buffer.isEmpty()).toBe(true);
      expect(buffer.isFull()).toBe(false);

      buffer.push(1);

      expect(buffer.isEmpty()).toBe(false);
      expect(buffer.isFull()).toBe(false);
    });

    it('should report full state', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);
      buffer.push(4);
      buffer.push(5);

      expect(buffer.isEmpty()).toBe(false);
      expect(buffer.isFull()).toBe(true);
    });
  });

  describe('length, capacity, available', () => {
    it('should report correct values', () => {
      expect(buffer.length).toBe(0);
      expect(buffer.capacity).toBe(5);
      expect(buffer.available).toBe(5);

      buffer.push(1);
      buffer.push(2);

      expect(buffer.length).toBe(2);
      expect(buffer.capacity).toBe(5);
      expect(buffer.available).toBe(3);
    });
  });

  describe('iteration', () => {
    it('should be iterable', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);

      const result = [...buffer];
      expect(result).toEqual([1, 2, 3]);
    });

    it('should work with for...of', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);

      const result = [];
      for (const item of buffer) {
        result.push(item);
      }
      expect(result).toEqual([1, 2, 3]);
    });
  });

  describe('forEach', () => {
    it('should apply function to each item', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);

      const result = [];
      buffer.forEach((item, index) => {
        result.push({ item, index });
      });

      expect(result).toEqual([
        { item: 1, index: 0 },
        { item: 2, index: 1 },
        { item: 3, index: 2 },
      ]);
    });
  });

  describe('filter', () => {
    it('should filter items', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);
      buffer.push(4);
      buffer.push(5);

      const result = buffer.filter(x => x % 2 === 0);
      expect(result).toEqual([2, 4]);
    });
  });

  describe('find', () => {
    it('should find first matching item', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);
      buffer.push(4);

      expect(buffer.find(x => x > 2)).toBe(3);
    });

    it('should return undefined if not found', () => {
      buffer.push(1);
      buffer.push(2);

      expect(buffer.find(x => x > 10)).toBeUndefined();
    });
  });

  describe('first and last', () => {
    it('should get first N items (oldest)', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);
      buffer.push(4);
      buffer.push(5);

      expect(buffer.first(3)).toEqual([1, 2, 3]);
      expect(buffer.first(10)).toEqual([1, 2, 3, 4, 5]); // Capped at size
    });

    it('should get last N items (newest)', () => {
      buffer.push(1);
      buffer.push(2);
      buffer.push(3);
      buffer.push(4);
      buffer.push(5);

      expect(buffer.last(3)).toEqual([3, 4, 5]);
      expect(buffer.last(10)).toEqual([1, 2, 3, 4, 5]); // Capped at size
    });
  });

  describe('performance characteristics', () => {
    it('should handle high-throughput streaming scenario', () => {
      const largeBuffer = new RingBuffer(1000);

      // Simulate high-throughput streaming: push 10,000 events
      for (let i = 0; i < 10000; i++) {
        largeBuffer.push({ id: i, timestamp: Date.now() });
      }

      // Buffer should only contain last 1000 items
      expect(largeBuffer.length).toBe(1000);
      expect(largeBuffer.isFull()).toBe(true);

      // Oldest item should be id: 9000
      const oldest = largeBuffer.peekOldest();
      expect(oldest.id).toBe(9000);

      // Newest item should be id: 9999
      const newest = largeBuffer.peek();
      expect(newest.id).toBe(9999);
    });

    it('should maintain O(1) push performance', () => {
      const largeBuffer = new RingBuffer(10000);

      const start = performance.now();
      for (let i = 0; i < 100000; i++) {
        largeBuffer.push(i);
      }
      const elapsed = performance.now() - start;

      // Should complete in reasonable time (< 100ms for 100k operations)
      expect(elapsed).toBeLessThan(100);
      expect(largeBuffer.length).toBe(10000);
    });
  });

  describe('object storage', () => {
    it('should store complex objects', () => {
      const objBuffer = new RingBuffer(3);

      objBuffer.push({ event: 'added', data: { value: 1 } });
      objBuffer.push({ event: 'updated', data: { value: 2 } });
      objBuffer.push({ event: 'removed', data: { value: 3 } });

      const items = objBuffer.toArray();
      expect(items).toHaveLength(3);
      expect(items[0].event).toBe('added');
      expect(items[2].event).toBe('removed');
    });
  });
});

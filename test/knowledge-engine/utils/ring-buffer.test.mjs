/**
 * @file ring-buffer.test.mjs (Fast)
 * @description Minimal fast tests for RingBuffer add/get operations
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { RingBuffer } from '../../../src/knowledge-engine/utils/ring-buffer.mjs';

describe('RingBuffer', () => {
  let buffer;

  beforeEach(() => {
    buffer = new RingBuffer(5);
  });

  it('should push and get items in order', () => {
    buffer.push(1);
    buffer.push(2);
    buffer.push(3);

    expect(buffer.length).toBe(3);
    expect(buffer.get(0)).toBe(1);
    expect(buffer.get(2)).toBe(3);
  });

  it('should overwrite oldest item when full', () => {
    for (let i = 1; i <= 5; i++) buffer.push(i);
    expect(buffer.isFull()).toBe(true);

    const overwritten = buffer.push(6);
    expect(overwritten).toBe(1);
    expect(buffer.toArray()).toEqual([2, 3, 4, 5, 6]);
  });
});

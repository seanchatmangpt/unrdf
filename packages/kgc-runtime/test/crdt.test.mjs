/**
 * @fileoverview Tests for CRDT (Conflict-free Replicated Data Types)
 */

import { describe, it, expect } from 'vitest';
import {
  createTimestamp,
  compareTimestamps,
  createLWWRegister,
  updateLWWRegister,
  mergeLWWRegisters,
  threeWayMergeLWW,
  createORSet,
  addToORSet,
  removeFromORSet,
  getORSetValues,
  mergeORSets,
  threeWayMergeORSet,
} from '../src/crdt.mjs';

describe('CRDT - Timestamps', () => {
  it('should create and compare timestamps correctly', () => {
    const t1 = createTimestamp(1, 'actor1');
    const t2 = createTimestamp(2, 'actor1');
    const t3 = createTimestamp(1, 'actor2');

    expect(compareTimestamps(t1, t2)).toBeLessThan(0); // t1 < t2
    expect(compareTimestamps(t2, t1)).toBeGreaterThan(0); // t2 > t1
    expect(compareTimestamps(t1, t1)).toBe(0); // t1 == t1

    // Same counter, different actor - lexicographic comparison
    const cmp = compareTimestamps(t1, t3);
    expect(cmp).toBe('actor1'.localeCompare('actor2'));
  });
});

describe('CRDT - LWW-Register', () => {
  it('should create and update LWW-Register', () => {
    const t1 = createTimestamp(1, 'agent1');
    const reg1 = createLWWRegister('initial value', t1);

    expect(reg1.value).toBe('initial value');
    expect(reg1.timestamp).toEqual(t1);

    // Update with later timestamp
    const t2 = createTimestamp(2, 'agent1');
    const reg2 = updateLWWRegister(reg1, 'updated value', t2);

    expect(reg2.value).toBe('updated value');
    expect(reg2.timestamp).toEqual(t2);

    // Update with earlier timestamp (should be ignored)
    const t0 = createTimestamp(0, 'agent1');
    const reg3 = updateLWWRegister(reg2, 'old value', t0);

    expect(reg3.value).toBe('updated value'); // Not changed
    expect(reg3.timestamp).toEqual(t2);
  });

  it('should merge LWW-Registers with last-write-wins semantics', () => {
    const t1 = createTimestamp(1, 'agent1');
    const t2 = createTimestamp(2, 'agent2');

    const reg1 = createLWWRegister('value from agent1', t1);
    const reg2 = createLWWRegister('value from agent2', t2);

    // Merge - later timestamp wins
    const merged = mergeLWWRegisters(reg1, reg2);

    expect(merged.value).toBe('value from agent2');
    expect(merged.timestamp).toEqual(t2);

    // Merge in opposite order - result should be same
    const merged2 = mergeLWWRegisters(reg2, reg1);

    expect(merged2.value).toBe('value from agent2');
    expect(merged2.timestamp).toEqual(t2);
  });

  it('should perform three-way merge with common ancestor', () => {
    const t0 = createTimestamp(0, 'agent0');
    const t1 = createTimestamp(1, 'agent1');
    const t2 = createTimestamp(2, 'agent2');

    const ancestor = createLWWRegister('original', t0);
    const left = createLWWRegister('left edit', t1);
    const right = createLWWRegister('right edit', t2);

    // Both branches modified - use LWW
    const merged = threeWayMergeLWW(ancestor, left, right);

    expect(merged.value).toBe('right edit'); // Later timestamp wins

    // Only one branch modified
    const merged2 = threeWayMergeLWW(ancestor, left, ancestor);
    expect(merged2.value).toBe('left edit'); // Left branch wins

    const merged3 = threeWayMergeLWW(ancestor, ancestor, right);
    expect(merged3.value).toBe('right edit'); // Right branch wins

    // No changes
    const merged4 = threeWayMergeLWW(ancestor, ancestor, ancestor);
    expect(merged4.value).toBe('original'); // No changes
  });
});

describe('CRDT - OR-Set', () => {
  it('should add and retrieve elements from OR-Set', () => {
    let set = createORSet();
    expect(getORSetValues(set)).toHaveLength(0);

    // Add elements
    const t1 = createTimestamp(1, 'agent1');
    set = addToORSet(set, 'apple', t1);

    const t2 = createTimestamp(2, 'agent1');
    set = addToORSet(set, 'banana', t2);

    const values = getORSetValues(set);
    expect(values).toHaveLength(2);
    expect(values).toContain('apple');
    expect(values).toContain('banana');
  });

  it('should remove elements from OR-Set', () => {
    let set = createORSet();

    const t1 = createTimestamp(1, 'agent1');
    set = addToORSet(set, 'item1', t1);

    const t2 = createTimestamp(2, 'agent1');
    set = addToORSet(set, 'item2', t2);

    expect(getORSetValues(set)).toHaveLength(2);

    // Remove item1
    const t3 = createTimestamp(3, 'agent1');
    set = removeFromORSet(set, 'item1', t3);

    const values = getORSetValues(set);
    expect(values).toHaveLength(1);
    expect(values).toContain('item2');
    expect(values).not.toContain('item1');
  });

  it('should merge OR-Sets correctly', () => {
    // Agent1's set
    let set1 = createORSet();
    const t1 = createTimestamp(1, 'agent1');
    set1 = addToORSet(set1, 'apple', t1);
    const t2 = createTimestamp(2, 'agent1');
    set1 = addToORSet(set1, 'banana', t2);

    // Agent2's set
    let set2 = createORSet();
    const t3 = createTimestamp(1, 'agent2');
    set2 = addToORSet(set2, 'cherry', t3);
    const t4 = createTimestamp(2, 'agent2');
    set2 = addToORSet(set2, 'banana', t4); // Also adds banana

    // Merge sets
    const merged = mergeORSets(set1, set2);
    const values = getORSetValues(merged);

    expect(values).toHaveLength(3);
    expect(values).toContain('apple');
    expect(values).toContain('banana');
    expect(values).toContain('cherry');

    // Test concurrent add/remove
    let set3 = createORSet();
    const t5 = createTimestamp(1, 'agent3');
    set3 = addToORSet(set3, 'item', t5);
    const t6 = createTimestamp(2, 'agent3');
    set3 = removeFromORSet(set3, 'item', t6);

    let set4 = createORSet();
    const t7 = createTimestamp(3, 'agent4');
    set4 = addToORSet(set4, 'item', t7); // Re-add after removal

    const merged2 = mergeORSets(set3, set4);
    const values2 = getORSetValues(merged2);

    // Item should be present (re-added after remove)
    expect(values2).toContain('item');
  });

  it('should perform three-way merge for OR-Sets', () => {
    // Ancestor
    let ancestor = createORSet();
    const t0 = createTimestamp(0, 'agent0');
    ancestor = addToORSet(ancestor, 'original', t0);

    // Left branch - adds 'left'
    let left = createORSet();
    left = addToORSet(left, 'original', t0);
    const t1 = createTimestamp(1, 'agent1');
    left = addToORSet(left, 'left', t1);

    // Right branch - adds 'right'
    let right = createORSet();
    right = addToORSet(right, 'original', t0);
    const t2 = createTimestamp(1, 'agent2');
    right = addToORSet(right, 'right', t2);

    // Three-way merge
    const merged = threeWayMergeORSet(ancestor, left, right);
    const values = getORSetValues(merged);

    expect(values).toHaveLength(3);
    expect(values).toContain('original');
    expect(values).toContain('left');
    expect(values).toContain('right');
  });
});

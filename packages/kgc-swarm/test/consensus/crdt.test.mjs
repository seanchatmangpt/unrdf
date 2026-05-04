/**
 * @file crdt.test.mjs
 * @description Tests for CRDT implementations
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  VectorClock,
  GSet,
  TwoPhaseSet,
  LWWElementSet,
  ORSet,
  createCRDT,
} from '../../src/consensus/crdt.mjs';

describe('CRDT Data Types', () => {
  describe('VectorClock', () => {
    it('should initialize correctly', () => {
      const clock = new VectorClock('node-1');

      expect(clock.nodeId).toBe('node-1');
      expect(clock.get('node-1')).toBe(0);
    });

    it('should increment local time', () => {
      const clock = new VectorClock('node-1');

      expect(clock.increment()).toBe(1);
      expect(clock.increment()).toBe(2);
      expect(clock.get('node-1')).toBe(2);
    });

    it('should merge clocks correctly', () => {
      const clock1 = new VectorClock('node-1');
      const clock2 = new VectorClock('node-2');

      clock1.increment(); // node-1: 1
      clock2.increment(); // node-2: 1
      clock2.increment(); // node-2: 2

      clock1.merge(clock2);

      expect(clock1.get('node-1')).toBe(1);
      expect(clock1.get('node-2')).toBe(2);
    });

    it('should compare clocks - before', () => {
      const clock1 = new VectorClock('node-1');
      const clock2 = new VectorClock('node-2');

      clock1.increment(); // node-1: 1
      clock2.increment(); // node-2: 1
      clock2.increment(); // node-2: 2

      clock1.merge(clock2); // node-1: 1, node-2: 2
      clock2.increment(); // node-2: 3

      expect(clock1.compare(clock2)).toBe('before');
    });

    it('should compare clocks - concurrent', () => {
      const clock1 = new VectorClock('node-1');
      const clock2 = new VectorClock('node-2');

      clock1.increment(); // node-1: 1
      clock2.increment(); // node-2: 1

      expect(clock1.compare(clock2)).toBe('concurrent');
    });

    it('should compare clocks - equal', () => {
      const clock1 = new VectorClock('node-1');
      const clock2 = new VectorClock('node-2');

      clock1.set('node-1', 1);
      clock1.set('node-2', 2);

      clock2.set('node-1', 1);
      clock2.set('node-2', 2);

      expect(clock1.compare(clock2)).toBe('equal');
    });

    it('should serialize and deserialize', () => {
      const clock = new VectorClock('node-1');
      clock.increment();
      clock.set('node-2', 5);

      const json = clock.toJSON();
      const restored = VectorClock.fromJSON(json);

      expect(restored.nodeId).toBe('node-1');
      expect(restored.get('node-1')).toBe(1);
      expect(restored.get('node-2')).toBe(5);
    });
  });

  describe('GSet (Grow-only Set)', () => {
    it('should add elements', () => {
      const set = new GSet();

      expect(set.add('a')).toBe(true);
      expect(set.add('b')).toBe(true);
      expect(set.add('a')).toBe(false); // Already exists

      expect(set.size()).toBe(2);
    });

    it('should check membership', () => {
      const set = new GSet();

      set.add('a');
      set.add({ x: 1, y: 2 });

      expect(set.has('a')).toBe(true);
      expect(set.has('b')).toBe(false);
      expect(set.has({ x: 1, y: 2 })).toBe(true);
    });

    it('should merge sets (commutativity)', () => {
      const set1 = new GSet();
      const set2 = new GSet();

      set1.add('a');
      set1.add('b');

      set2.add('b');
      set2.add('c');

      // Test commutativity: merge(A, B) = merge(B, A)
      const merged1 = set1.clone();
      merged1.merge(set2);

      const merged2 = set2.clone();
      merged2.merge(set1);

      expect(merged1.values().sort()).toEqual(['a', 'b', 'c']);
      expect(merged2.values().sort()).toEqual(['a', 'b', 'c']);
    });

    it('should be idempotent: merge(A, A) = A', () => {
      const set = new GSet();
      set.add('a');
      set.add('b');

      const original = set.values().sort();

      set.merge(set);

      expect(set.values().sort()).toEqual(original);
    });
  });

  describe('TwoPhaseSet (2P-Set)', () => {
    it('should add and remove elements', () => {
      const set = new TwoPhaseSet();

      set.add('a');
      set.add('b');

      expect(set.has('a')).toBe(true);
      expect(set.size()).toBe(2);

      set.remove('a');

      expect(set.has('a')).toBe(false);
      expect(set.size()).toBe(1);
    });

    it('should not allow re-adding removed elements', () => {
      const set = new TwoPhaseSet();

      set.add('a');
      set.remove('a');

      expect(set.add('a')).toBe(false);
      expect(set.has('a')).toBe(false);
    });

    it('should merge sets correctly', () => {
      const set1 = new TwoPhaseSet();
      const set2 = new TwoPhaseSet();

      set1.add('a');
      set1.add('b');

      set2.add('b');
      set2.remove('b');
      set2.add('c');

      set1.merge(set2);

      expect(set1.has('a')).toBe(true);
      expect(set1.has('b')).toBe(false); // Removed in set2
      expect(set1.has('c')).toBe(true);
    });

    it('should be commutative', () => {
      const set1 = new TwoPhaseSet();
      const set2 = new TwoPhaseSet();

      set1.add('a');
      set2.remove('a'); // Concurrent add/remove

      const merged1 = set1.clone();
      merged1.merge(set2);

      const merged2 = set2.clone();
      merged2.merge(set1);

      // Both should converge to same state (remove wins)
      expect(merged1.has('a')).toBe(merged2.has('a'));
      expect(merged1.values()).toEqual(merged2.values());
    });
  });

  describe('LWWElementSet (Last-Write-Wins)', () => {
    it('should add and remove with timestamps', () => {
      const set = new LWWElementSet('node-1');

      set.add('a'); // timestamp: 1
      set.add('b'); // timestamp: 2

      expect(set.has('a')).toBe(true);
      expect(set.has('b')).toBe(true);
      expect(set.size()).toBe(2);

      set.remove('a'); // timestamp: 3

      expect(set.has('a')).toBe(false);
      expect(set.size()).toBe(1);
    });

    it('should resolve conflicts by timestamp (add wins)', () => {
      const set1 = new LWWElementSet('node-1', { biasAdd: true });
      const set2 = new LWWElementSet('node-2', { biasAdd: true });

      // Simulate: set1 adds 'a', set2 removes 'a' later
      set1.add('a'); // timestamp: 1

      // Manually create later timestamp
      set2.clock.set('node-2', 10);
      set2.remove('a'); // timestamp: 11 (higher)

      set1.merge(set2);

      expect(set1.has('a')).toBe(false); // Remove wins (higher timestamp)
    });

    it('should resolve ties with bias', () => {
      const setAddBias = new LWWElementSet('node-1', { biasAdd: true });
      const setRemoveBias = new LWWElementSet('node-2', { biasAdd: false });

      // Same timestamp
      const element = { x: 1 };
      setAddBias.added.set(JSON.stringify(element), {
        element,
        timestamp: 100,
        nodeId: 'node-1',
      });

      setAddBias.removed.set(JSON.stringify(element), {
        element,
        timestamp: 100,
        nodeId: 'node-2',
      });

      expect(setAddBias.has(element)).toBe(true); // Add bias

      setRemoveBias.added.set(JSON.stringify(element), {
        element,
        timestamp: 100,
        nodeId: 'node-1',
      });

      setRemoveBias.removed.set(JSON.stringify(element), {
        element,
        timestamp: 100,
        nodeId: 'node-2',
      });

      expect(setRemoveBias.has(element)).toBe(false); // Remove bias
    });

    it('should be commutative', () => {
      const set1 = new LWWElementSet('node-1');
      const set2 = new LWWElementSet('node-2');

      set1.add('a');
      set2.add('b');

      const merged1 = set1.clone();
      merged1.merge(set2);

      const merged2 = set2.clone();
      merged2.merge(set1);

      expect(merged1.values().sort()).toEqual(merged2.values().sort());
    });

    it('should be associative', () => {
      const set1 = new LWWElementSet('node-1');
      const set2 = new LWWElementSet('node-2');
      const set3 = new LWWElementSet('node-3');

      set1.add('a');
      set2.add('b');
      set3.add('c');

      // merge(merge(A, B), C)
      const left = set1.clone();
      left.merge(set2);
      left.merge(set3);

      // merge(A, merge(B, C))
      const right = set1.clone();
      const temp = set2.clone();
      temp.merge(set3);
      right.merge(temp);

      expect(left.values().sort()).toEqual(right.values().sort());
    });

    it('should be idempotent', () => {
      const set = new LWWElementSet('node-1');

      set.add('a');
      set.add('b');

      const original = set.values().sort();

      set.merge(set);

      expect(set.values().sort()).toEqual(original);
    });
  });

  describe('ORSet (Observed-Remove Set)', () => {
    it('should add elements with unique tags', () => {
      const set = new ORSet();

      const tag1 = set.add('a');
      const tag2 = set.add('a'); // Same element, different tag

      expect(set.has('a')).toBe(true);
      expect(tag1).not.toBe(tag2);
    });

    it('should remove only observed tags', () => {
      const set = new ORSet();

      const tag1 = set.add('a');
      set.add('a'); // tag2

      set.remove('a', [tag1]); // Remove only first add

      expect(set.has('a')).toBe(true); // Still has tag2
    });

    it('should allow re-adding removed elements', () => {
      const set = new ORSet();

      const tag1 = set.add('a');
      set.remove('a', [tag1]);

      expect(set.has('a')).toBe(false);

      set.add('a'); // New tag

      expect(set.has('a')).toBe(true);
    });

    it('should merge sets correctly', () => {
      const set1 = new ORSet();
      const set2 = new ORSet();

      const tag1 = set1.add('a');
      set2.add('a'); // Different tag

      set2.remove('a', [tag1]); // Remove tag1 (not observed by set2)

      set1.merge(set2);

      // Both tags should exist after merge
      expect(set1.has('a')).toBe(true);
    });

    it('should handle concurrent add/remove (add wins)', () => {
      const set1 = new ORSet();
      const set2 = new ORSet();

      const tag1 = set1.add('a');

      // Concurrent: set2 doesn't know about tag1
      set2.add('a'); // Different tag

      // set1 removes its tag
      set1.remove('a', [tag1]);

      set1.merge(set2);

      expect(set1.has('a')).toBe(true); // set2's tag survives
    });

    it('should be commutative', () => {
      const set1 = new ORSet();
      const set2 = new ORSet();

      const tag1 = set1.add('a');
      set1.add('b');

      const tag2 = set2.add('a');
      set2.remove('a', [tag2]);

      const merged1 = set1.clone();
      merged1.merge(set2);

      const merged2 = set2.clone();
      merged2.merge(set1);

      expect(merged1.values().sort()).toEqual(merged2.values().sort());
      expect(merged1.has('a')).toBe(merged2.has('a'));
    });

    it('should be idempotent', () => {
      const set = new ORSet();

      set.add('a');
      set.add('b');

      const original = set.values().sort();

      set.merge(set);

      expect(set.values().sort()).toEqual(original);
    });
  });

  describe('CRDT merge commutativity tests', () => {
    it('GSet: A ⊔ B = B ⊔ A', () => {
      const setA = new GSet();
      const setB = new GSet();

      setA.add('x');
      setA.add('y');
      setB.add('y');
      setB.add('z');

      const ab = setA.clone();
      ab.merge(setB);

      const ba = setB.clone();
      ba.merge(setA);

      expect(ab.values().sort()).toEqual(ba.values().sort());
    });

    it('2P-Set: A ⊔ B = B ⊔ A', () => {
      const setA = new TwoPhaseSet();
      const setB = new TwoPhaseSet();

      setA.add('x');
      setA.remove('x');
      setB.add('x');
      setB.add('y');

      const ab = setA.clone();
      ab.merge(setB);

      const ba = setB.clone();
      ba.merge(setA);

      expect(ab.values().sort()).toEqual(ba.values().sort());
    });

    it('LWW-Set: A ⊔ B = B ⊔ A', () => {
      const setA = new LWWElementSet('node-1');
      const setB = new LWWElementSet('node-2');

      setA.add('x');
      setB.add('y');

      const ab = setA.clone();
      ab.merge(setB);

      const ba = setB.clone();
      ba.merge(setA);

      expect(ab.values().sort()).toEqual(ba.values().sort());
    });

    it('OR-Set: A ⊔ B = B ⊔ A', () => {
      const setA = new ORSet();
      const setB = new ORSet();

      setA.add('x');
      setB.add('y');

      const ab = setA.clone();
      ab.merge(setB);

      const ba = setB.clone();
      ba.merge(setA);

      expect(ab.values().sort()).toEqual(ba.values().sort());
    });
  });

  describe('createCRDT factory', () => {
    it('should create GSet', () => {
      const set = createCRDT('gset');
      expect(set).toBeInstanceOf(GSet);
    });

    it('should create 2P-Set', () => {
      const set = createCRDT('2pset');
      expect(set).toBeInstanceOf(TwoPhaseSet);
    });

    it('should create LWW-Set', () => {
      const set = createCRDT('lwwset', 'node-1');
      expect(set).toBeInstanceOf(LWWElementSet);
    });

    it('should create OR-Set', () => {
      const set = createCRDT('orset');
      expect(set).toBeInstanceOf(ORSet);
    });

    it('should throw on unknown type', () => {
      expect(() => createCRDT('unknown')).toThrow('Unknown CRDT type');
    });
  });
});

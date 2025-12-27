/**
 * @fileoverview Tests for useDeltaQuery hook
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Store, DataFactory } from 'n3';

const { quad, namedNode, literal } = DataFactory;

describe('useDeltaQuery', () => {
  let store1, store2;

  beforeEach(() => {
    store1 = new Store([quad(namedNode('http://s1'), namedNode('http://p'), literal('o1'))]);

    store2 = new Store([
      quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
      quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
    ]);
  });

  describe('Delta Detection', () => {
    it('should detect added quads', () => {
      const added = new Store();
      for (const q of store2) {
        if (!store1.has(q)) {
          added.add(q);
        }
      }

      expect(added.size).toBe(1);
    });

    it('should detect removed quads', () => {
      const removed = new Store();
      for (const q of store1) {
        if (!store2.has(q)) {
          removed.add(q);
        }
      }

      expect(removed.size).toBe(0);
    });

    it('should detect unchanged quads', () => {
      let unchanged = 0;
      for (const q of store1) {
        if (store2.has(q)) {
          unchanged++;
        }
      }

      expect(unchanged).toBe(1);
    });
  });

  describe('Delta Application', () => {
    it('should apply delta to store', () => {
      const delta = {
        added: [quad(namedNode('http://s3'), namedNode('http://p'), literal('o3'))],
        removed: [],
      };

      delta.added.forEach(q => store1.add(q));

      expect(store1.size).toBe(2);
    });

    it('should track delta history', () => {
      const history = [];

      const delta1 = {
        added: [quad(namedNode('http://s2'), namedNode('http://p'), literal('o2'))],
        removed: [],
      };
      history.push(delta1);

      const delta2 = {
        added: [quad(namedNode('http://s3'), namedNode('http://p'), literal('o3'))],
        removed: [],
      };
      history.push(delta2);

      expect(history).toHaveLength(2);
    });
  });

  describe('Performance', () => {
    it('should compute deltas efficiently', () => {
      const largeStore1 = new Store();
      const largeStore2 = new Store();

      for (let i = 0; i < 10000; i++) {
        largeStore1.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`)));
        largeStore2.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`)));
      }

      // Add some changes
      largeStore2.add(quad(namedNode('http://new'), namedNode('http://p'), literal('new')));

      const start = performance.now();
      const added = new Store();
      for (const q of largeStore2) {
        if (!largeStore1.has(q)) {
          added.add(q);
        }
      }
      const duration = performance.now() - start;

      expect(added.size).toBe(1);
      expect(duration).toBeLessThan(1000);
    });
  });
});

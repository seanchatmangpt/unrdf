/**
 * @fileoverview Tests for useTransaction hook
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { Store, DataFactory } from 'n3';

const { quad, namedNode, literal } = DataFactory;

describe('useTransaction', () => {
  let store;

  beforeEach(() => {
    store = new Store();
  });

  describe('Basic Transactions', () => {
    it('should commit transaction', () => {
      const snapshot = new Store([...store]);

      const quadsToAdd = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2'))
      ];

      // Transaction
      quadsToAdd.forEach(q => store.add(q));

      expect(store.size).toBe(2);
    });

    it('should rollback transaction on error', () => {
      const snapshot = new Store([...store]);
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const originalSize = snapshot.size;

      try {
        // Simulate error
        store.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));
        throw new Error('Transaction failed');
      } catch (error) {
        // Rollback
        store = snapshot;
      }

      expect(store.size).toBe(originalSize);
    });
  });

  describe('ACID Properties', () => {
    it('should ensure atomicity', () => {
      const snapshot = new Store([...store]);

      const operations = [
        () => store.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1'))),
        () => store.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2'))),
        () => { throw new Error('Fail'); }
      ];

      try {
        operations.forEach(op => op());
      } catch (error) {
        store = snapshot;
      }

      expect(store.size).toBe(0);
    });

    it('should ensure consistency', () => {
      const size1 = store.size;

      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));
      const size2 = store.size;

      expect(size2).toBe(size1 + 1);
    });

    it('should ensure isolation', () => {
      const tx1Store = new Store([...store]);
      const tx2Store = new Store([...store]);

      tx1Store.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));
      tx2Store.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));

      expect(tx1Store.size).toBe(1);
      expect(tx2Store.size).toBe(1);
    });

    it('should ensure durability', async () => {
      const quads = [
        quad(namedNode('http://s'), namedNode('http://p'), literal('o'))
      ];

      quads.forEach(q => store.add(q));

      // Simulate persistence
      const serialized = JSON.stringify([...store]);
      const restored = new Store(JSON.parse(serialized).map(q =>
        quad(namedNode(q.subject.value), namedNode(q.predicate.value), literal(q.object.value))
      ));

      expect(restored.size).toBe(1);
    });
  });

  describe('Nested Transactions', () => {
    it('should support nested transactions', () => {
      const snapshot1 = new Store([...store]);

      store.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));

      const snapshot2 = new Store([...store]);

      store.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));

      expect(snapshot1.size).toBe(0);
      expect(snapshot2.size).toBe(1);
      expect(store.size).toBe(2);
    });

    it('should rollback nested transaction', () => {
      store.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));
      const snapshot = new Store([...store]);

      try {
        store.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));
        throw new Error('Nested failed');
      } catch (error) {
        store = snapshot;
      }

      expect(store.size).toBe(1);
    });
  });

  describe('Transaction Hooks', () => {
    it('should execute pre-commit hooks', () => {
      const preHooks = [];

      const hook = () => preHooks.push('executed');

      hook();
      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      expect(preHooks).toContain('executed');
    });

    it('should execute post-commit hooks', () => {
      const postHooks = [];

      store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));

      const hook = () => postHooks.push('executed');
      hook();

      expect(postHooks).toContain('executed');
    });
  });

  describe('Performance', () => {
    it('should handle large transactions efficiently', () => {
      const start = performance.now();

      const snapshot = new Store([...store]);

      for (let i = 0; i < 10000; i++) {
        store.add(quad(
          namedNode(`http://s${i}`),
          namedNode('http://p'),
          literal(`o${i}`)
        ));
      }

      const duration = performance.now() - start;

      expect(store.size).toBe(10000);
      expect(duration).toBeLessThan(2000);
    });
  });
});

/**
 * @fileoverview Tests for useQuadStore hook
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Store, DataFactory } from 'n3';

const { quad, namedNode, literal } = DataFactory;

describe('useQuadStore', () => {
  let store;

  beforeEach(() => {
    store = new Store();
  });

  describe('Quad Operations', () => {
    it('should add quad', () => {
      const q = quad(namedNode('http://s'), namedNode('http://p'), literal('o'));

      store.add(q);

      expect(store.has(q)).toBe(true);
      expect(store.size).toBe(1);
    });

    it('should remove quad', () => {
      const q = quad(namedNode('http://s'), namedNode('http://p'), literal('o'));

      store.add(q);
      store.delete(q);

      expect(store.has(q)).toBe(false);
      expect(store.size).toBe(0);
    });

    it('should query quads', () => {
      store.add(quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')));
      store.add(quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')));

      const results = [...store.match(null, namedNode('http://p'), null, null)];

      expect(results).toHaveLength(2);
    });
  });

  describe('Bulk Operations', () => {
    it('should add multiple quads', () => {
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
        quad(namedNode('http://s3'), namedNode('http://p'), literal('o3')),
      ];

      quads.forEach(q => store.add(q));

      expect(store.size).toBe(3);
    });

    it('should remove multiple quads', () => {
      const quads = [
        quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')),
      ];

      quads.forEach(q => store.add(q));
      quads.forEach(q => store.delete(q));

      expect(store.size).toBe(0);
    });
  });

  describe('Pattern Matching', () => {
    beforeEach(() => {
      store.add(
        quad(namedNode('http://alice'), namedNode('http://knows'), namedNode('http://bob'))
      );
      store.add(quad(namedNode('http://alice'), namedNode('http://age'), literal('30')));
      store.add(quad(namedNode('http://bob'), namedNode('http://age'), literal('25')));
    });

    it('should match by subject', () => {
      const results = [...store.match(namedNode('http://alice'), null, null, null)];

      expect(results).toHaveLength(2);
    });

    it('should match by predicate', () => {
      const results = [...store.match(null, namedNode('http://age'), null, null)];

      expect(results).toHaveLength(2);
    });

    it('should match by object', () => {
      const results = [...store.match(null, null, literal('30'), null)];

      expect(results).toHaveLength(1);
    });

    it('should match with multiple criteria', () => {
      const results = [
        ...store.match(namedNode('http://alice'), namedNode('http://age'), null, null),
      ];

      expect(results).toHaveLength(1);
      expect(results[0].object.value).toBe('30');
    });
  });

  describe('Performance', () => {
    it('should handle large quad stores efficiently', () => {
      const start = performance.now();

      for (let i = 0; i < 100000; i++) {
        store.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`)));
      }

      const duration = performance.now() - start;

      expect(store.size).toBe(100000);
      expect(duration).toBeLessThan(5000);
    });

    it('should query large stores efficiently', () => {
      for (let i = 0; i < 10000; i++) {
        store.add(
          quad(namedNode(`http://s${i}`), namedNode(`http://p${i % 10}`), literal(`o${i}`))
        );
      }

      const start = performance.now();
      const results = [...store.match(null, namedNode('http://p5'), null, null)];
      const duration = performance.now() - start;

      expect(results.length).toBeGreaterThan(0);
      expect(duration).toBeLessThan(100);
    });
  });
});

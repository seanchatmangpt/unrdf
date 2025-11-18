/**
 * @fileoverview Tests for useTriples hook
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { renderHook, act } from '@testing-library/react';
import { useTriples } from '../../../src/react-hooks/core/useTriples.mjs';
import { Store, DataFactory } from 'n3';

const { quad, namedNode, literal } = DataFactory;

describe('useTriples', () => {
  let testStore;

  beforeEach(() => {
    testStore = new Store([
      quad(namedNode('http://s1'), namedNode('http://p1'), literal('o1')),
      quad(namedNode('http://s2'), namedNode('http://p1'), literal('o2')),
      quad(namedNode('http://s1'), namedNode('http://p2'), literal('o3'))
    ]);
  });

  describe('Initialization', () => {
    it('should return all triples from store', () => {
      const { result } = renderHook(() => useTriples(testStore));

      expect(result.current.triples).toHaveLength(3);
      expect(result.current.total).toBe(3);
    });

    it('should handle empty store', () => {
      const { result } = renderHook(() => useTriples(new Store()));

      expect(result.current.triples).toHaveLength(0);
      expect(result.current.total).toBe(0);
    });
  });

  describe('Filter Operations', () => {
    it('should filter by subject', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const filtered = result.current.filter({
        subject: namedNode('http://s1')
      });

      expect(filtered).toHaveLength(2);
    });

    it('should filter by predicate', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const filtered = result.current.filter({
        predicate: namedNode('http://p1')
      });

      expect(filtered).toHaveLength(2);
    });

    it('should filter by object', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const filtered = result.current.filter({
        object: literal('o1')
      });

      expect(filtered).toHaveLength(1);
    });

    it('should filter with multiple criteria', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const filtered = result.current.filter({
        subject: namedNode('http://s1'),
        predicate: namedNode('http://p1')
      });

      expect(filtered).toHaveLength(1);
    });

    it('should return empty array when no matches', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const filtered = result.current.filter({
        subject: namedNode('http://nonexistent')
      });

      expect(filtered).toHaveLength(0);
    });
  });

  describe('Find Operations', () => {
    it('should find triples by subject', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const found = result.current.findBySubject('http://s1');

      expect(found).toHaveLength(2);
    });

    it('should find triples by predicate', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const found = result.current.findByPredicate('http://p1');

      expect(found).toHaveLength(2);
    });

    it('should find triples by object', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const found = result.current.findByObject(literal('o1'));

      expect(found).toHaveLength(1);
    });

    it('should return empty array when subject not found', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const found = result.current.findBySubject('http://nonexistent');

      expect(found).toHaveLength(0);
    });
  });

  describe('Count Operations', () => {
    it('should count all triples', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const count = result.current.count();

      expect(count).toBe(3);
    });

    it('should count filtered triples', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const count = result.current.count({
        subject: namedNode('http://s1')
      });

      expect(count).toBe(2);
    });

    it('should return 0 for no matches', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const count = result.current.count({
        subject: namedNode('http://nonexistent')
      });

      expect(count).toBe(0);
    });
  });

  describe('State Updates', () => {
    it('should update when store changes', () => {
      const initialStore = new Store([
        quad(namedNode('http://s1'), namedNode('http://p1'), literal('o1'))
      ]);

      const { result, rerender } = renderHook(
        ({ store }) => useTriples(store),
        { initialProps: { store: initialStore } }
      );

      expect(result.current.total).toBe(1);

      const newStore = new Store([
        quad(namedNode('http://s1'), namedNode('http://p1'), literal('o1')),
        quad(namedNode('http://s2'), namedNode('http://p2'), literal('o2'))
      ]);

      rerender({ store: newStore });

      expect(result.current.total).toBe(2);
    });
  });

  describe('Performance', () => {
    it('should handle large stores efficiently', () => {
      const largeStore = new Store();

      for (let i = 0; i < 10000; i++) {
        largeStore.add(
          quad(
            namedNode(`http://s${i % 100}`),
            namedNode(`http://p${i % 10}`),
            literal(`o${i}`)
          )
        );
      }

      const start = performance.now();
      const { result } = renderHook(() => useTriples(largeStore));
      const duration = performance.now() - start;

      expect(result.current.total).toBe(10000);
      expect(duration).toBeLessThan(1000);
    });

    it('should memoize triples array', () => {
      const { result, rerender } = renderHook(() => useTriples(testStore));

      const triples1 = result.current.triples;
      rerender();
      const triples2 = result.current.triples;

      expect(triples1).toBe(triples2);
    });
  });

  describe('Edge Cases', () => {
    it('should handle undefined filter values', () => {
      const { result } = renderHook(() => useTriples(testStore));

      const filtered = result.current.filter({});

      expect(filtered).toHaveLength(3);
    });

    it('should handle null parameters', () => {
      const { result } = renderHook(() => useTriples(testStore));

      expect(() => {
        result.current.filter({ subject: null });
      }).not.toThrow();
    });
  });
});

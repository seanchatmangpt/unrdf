/**
 * @fileoverview Tests for useStore hook
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { renderHook, act } from '@testing-library/react';
import { useStore } from '../src/core/useStore.mjs';
import { Store, DataFactory } from 'n3';

const { quad, namedNode, literal } = DataFactory;

describe('useStore', () => {
  let testQuad;

  beforeEach(() => {
    testQuad = quad(
      namedNode('http://example.org/subject'),
      namedNode('http://example.org/predicate'),
      literal('object')
    );
  });

  describe('Initialization', () => {
    it('should initialize with empty store by default', () => {
      const { result } = renderHook(() => useStore());

      expect(result.current.store).toBeInstanceOf(Store);
      expect(result.current.size).toBe(0);
    });

    it('should initialize with provided store', () => {
      const initialStore = new Store([testQuad]);
      const { result } = renderHook(() => useStore(initialStore));

      expect(result.current.size).toBe(1);
    });
  });

  describe('Add Operations', () => {
    it('should add a single quad', () => {
      const { result } = renderHook(() => useStore());

      act(() => {
        result.current.add(testQuad);
      });

      expect(result.current.size).toBe(1);
      expect(result.current.has(testQuad)).toBe(true);
    });

    it('should add multiple quads', () => {
      const { result } = renderHook(() => useStore());

      const quads = [testQuad, quad(namedNode('http://s2'), namedNode('http://p2'), literal('o2'))];

      act(() => {
        result.current.addQuads(quads);
      });

      expect(result.current.size).toBe(2);
    });

    it('should not add duplicate quads', () => {
      const { result } = renderHook(() => useStore());

      act(() => {
        result.current.add(testQuad);
        result.current.add(testQuad);
      });

      expect(result.current.size).toBe(1);
    });

    it('should return new store after add', () => {
      const { result } = renderHook(() => useStore());

      let newStore;
      act(() => {
        newStore = result.current.add(testQuad);
      });

      expect(newStore).toBeInstanceOf(Store);
      expect(newStore.size).toBe(1);
    });
  });

  describe('Remove Operations', () => {
    it('should remove a quad', () => {
      const initialStore = new Store([testQuad]);
      const { result } = renderHook(() => useStore(initialStore));

      expect(result.current.size).toBe(1);

      act(() => {
        result.current.remove(testQuad);
      });

      expect(result.current.size).toBe(0);
      expect(result.current.has(testQuad)).toBe(false);
    });

    it('should handle removing non-existent quad', () => {
      const { result } = renderHook(() => useStore());

      act(() => {
        result.current.remove(testQuad);
      });

      expect(result.current.size).toBe(0);
    });
  });

  describe('Clear Operation', () => {
    it('should clear all quads', () => {
      const initialStore = new Store([testQuad]);
      const { result } = renderHook(() => useStore(initialStore));

      expect(result.current.size).toBe(1);

      act(() => {
        result.current.clear();
      });

      expect(result.current.size).toBe(0);
    });

    it('should return empty store after clear', () => {
      const initialStore = new Store([testQuad]);
      const { result } = renderHook(() => useStore(initialStore));

      let clearedStore;
      act(() => {
        clearedStore = result.current.clear();
      });

      expect(clearedStore.size).toBe(0);
    });
  });

  describe('Query Operations', () => {
    it('should check if store has quad', () => {
      const initialStore = new Store([testQuad]);
      const { result } = renderHook(() => useStore(initialStore));

      expect(result.current.has(testQuad)).toBe(true);
    });

    it('should match quads with pattern', () => {
      const { result } = renderHook(() => useStore());

      const quad1 = quad(namedNode('http://s'), namedNode('http://p'), literal('o1'));
      const quad2 = quad(namedNode('http://s'), namedNode('http://p'), literal('o2'));

      act(() => {
        result.current.addQuads([quad1, quad2]);
      });

      const matches = result.current.match(namedNode('http://s'), null, null, null);
      expect([...matches]).toHaveLength(2);
    });

    it('should match quads by predicate', () => {
      const { result } = renderHook(() => useStore());

      const p1 = namedNode('http://p1');
      const quad1 = quad(namedNode('http://s1'), p1, literal('o1'));
      const quad2 = quad(namedNode('http://s2'), namedNode('http://p2'), literal('o2'));

      act(() => {
        result.current.addQuads([quad1, quad2]);
      });

      const matches = result.current.match(null, p1, null, null);
      expect([...matches]).toHaveLength(1);
    });
  });

  describe('State Updates', () => {
    it('should trigger re-render on add', () => {
      const { result } = renderHook(() => useStore());

      const initialSize = result.current.size;

      act(() => {
        result.current.add(testQuad);
      });

      expect(result.current.size).toBe(initialSize + 1);
    });

    it('should maintain immutability', () => {
      const { result } = renderHook(() => useStore());

      const store1 = result.current.store;

      act(() => {
        result.current.add(testQuad);
      });

      const store2 = result.current.store;

      expect(store1).not.toBe(store2);
    });
  });

  describe('Performance', () => {
    it('should handle large number of quads efficiently', () => {
      const { result } = renderHook(() => useStore());

      const quads = Array.from({ length: 1000 }, (_, i) =>
        quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`))
      );

      const start = performance.now();

      act(() => {
        result.current.addQuads(quads);
      });

      const duration = performance.now() - start;

      expect(result.current.size).toBe(1000);
      expect(duration).toBeLessThan(1000); // Should complete in less than 1 second
    });
  });

  describe('Edge Cases', () => {
    it('should handle null and undefined gracefully', () => {
      const { result } = renderHook(() => useStore());

      expect(() => {
        act(() => {
          result.current.match(null, null, null, null);
        });
      }).not.toThrow();
    });

    it('should handle empty match results', () => {
      const { result } = renderHook(() => useStore());

      const matches = result.current.match(namedNode('http://nonexistent'), null, null, null);

      expect([...matches]).toHaveLength(0);
    });
  });
});

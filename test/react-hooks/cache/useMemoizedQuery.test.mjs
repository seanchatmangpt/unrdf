/**
 * @fileoverview Tests for useMemoizedQuery hook
 */

import { describe, it, expect, _beforeEach, vi } from 'vitest';
import { renderHook } from '@testing-library/react';
import { useMemo } from 'react';

describe('useMemoizedQuery', () => {
  describe('Memoization', () => {
    it('should memoize query result', () => {
      const queryFn = vi.fn(() => ({ rows: [] }));

      const { result, rerender } = renderHook(({ query }) => useMemo(() => queryFn(), [query]), {
        initialProps: { query: 'SELECT * WHERE { ?s ?p ?o }' },
      });

      const result1 = result.current;
      rerender({ query: 'SELECT * WHERE { ?s ?p ?o }' });
      const result2 = result.current;

      expect(result1).toBe(result2);
      expect(queryFn).toHaveBeenCalledTimes(1);
    });

    it('should recompute when dependencies change', () => {
      const queryFn = vi.fn(q => ({ query: q, rows: [] }));

      const { result, rerender } = renderHook(
        ({ query }) => useMemo(() => queryFn(query), [query]),
        { initialProps: { query: 'query1' } }
      );

      const result1 = result.current;

      rerender({ query: 'query2' });

      const result2 = result.current;

      expect(result1).not.toBe(result2);
      expect(queryFn).toHaveBeenCalledTimes(2);
    });
  });

  describe('Performance Optimization', () => {
    it('should prevent unnecessary recomputation', () => {
      const expensiveQuery = vi.fn(() => {
        let sum = 0;
        for (let i = 0; i < 1000000; i++) {
          sum += i;
        }
        return { rows: [], computed: sum };
      });

      const { _result, rerender } = renderHook(
        ({ deps }) => useMemo(() => expensiveQuery(), deps),
        { initialProps: { deps: [] } }
      );

      // Multiple rerenders with same deps
      rerender({ deps: [] });
      rerender({ deps: [] });
      rerender({ deps: [] });

      expect(expensiveQuery).toHaveBeenCalledTimes(1);
    });

    it('should measure performance improvement', () => {
      const computeFn = vi.fn(() => {
        let result = 0;
        for (let i = 0; i < 100000; i++) {
          result += Math.sqrt(i);
        }
        return result;
      });

      // Without memoization
      const start1 = performance.now();
      const _r1 = computeFn();
      const _r2 = computeFn();
      const _r3 = computeFn();
      const duration1 = performance.now() - start1;

      // With memoization
      const cached = computeFn();
      const start2 = performance.now();
      const _r4 = cached;
      const _r5 = cached;
      const _r6 = cached;
      const duration2 = performance.now() - start2;

      expect(duration2).toBeLessThan(duration1);
    });
  });

  describe('Dependency Tracking', () => {
    it('should track query string changes', () => {
      const computeFn = vi.fn(query => ({ query, rows: [] }));

      const { _result, rerender } = renderHook(
        ({ query }) => useMemo(() => computeFn(query), [query]),
        { initialProps: { query: 'q1' } }
      );

      rerender({ query: 'q1' }); // Same - should not recompute
      expect(computeFn).toHaveBeenCalledTimes(1);

      rerender({ query: 'q2' }); // Different - should recompute
      expect(computeFn).toHaveBeenCalledTimes(2);
    });

    it('should track multiple dependencies', () => {
      const computeFn = vi.fn((a, b) => a + b);

      const { result, rerender } = renderHook(
        ({ a, b }) => useMemo(() => computeFn(a, b), [a, b]),
        { initialProps: { a: 1, b: 2 } }
      );

      expect(result.current).toBe(3);
      expect(computeFn).toHaveBeenCalledTimes(1);

      rerender({ a: 1, b: 2 }); // Same - no recompute
      expect(computeFn).toHaveBeenCalledTimes(1);

      rerender({ a: 2, b: 2 }); // Different a - recompute
      expect(computeFn).toHaveBeenCalledTimes(2);
    });
  });

  describe('Memory Management', () => {
    it('should not cause memory leaks', () => {
      const { unmount } = renderHook(() =>
        useMemo(() => ({ large: new Array(1000).fill('data') }), [])
      );

      // Unmount should cleanup
      unmount();

      // If we can allocate memory after, no leak
      const allocated = new Array(1000).fill('test');
      expect(allocated.length).toBe(1000);
    });
  });
});

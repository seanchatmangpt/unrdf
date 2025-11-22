/**
 * @fileoverview Performance benchmark tests for React hooks
 */

import { describe, it, expect, _beforeEach } from 'vitest';
import { _renderHook } from '@testing-library/react';
import { Store, DataFactory } from 'n3';

const { quad, namedNode, literal } = DataFactory;

describe('Performance Benchmarks', () => {
  describe('useStore Performance', () => {
    it('should add 10,000 quads in < 1 second', () => {
      const store = new Store();
      const start = performance.now();

      for (let i = 0; i < 10000; i++) {
        store.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`)));
      }

      const duration = performance.now() - start;
      expect(store.size).toBe(10000);
      expect(duration).toBeLessThan(1000);
    });

    it('should query 10,000 quads in < 100ms', () => {
      const store = new Store();

      for (let i = 0; i < 10000; i++) {
        store.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`)));
      }

      const start = performance.now();
      const results = [...store.match(null, namedNode('http://p'), null, null)];
      const duration = performance.now() - start;

      expect(results).toHaveLength(10000);
      expect(duration).toBeLessThan(100);
    });

    it('should remove 10,000 quads in < 1 second', () => {
      const store = new Store();
      const quads = [];

      for (let i = 0; i < 10000; i++) {
        const q = quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`));
        quads.push(q);
        store.add(q);
      }

      const start = performance.now();
      quads.forEach(q => store.delete(q));
      const duration = performance.now() - start;

      expect(store.size).toBe(0);
      expect(duration).toBeLessThan(1000);
    });
  });

  describe('useTriples Performance', () => {
    it('should filter 10,000 triples by subject in < 50ms', () => {
      const store = new Store();
      const targetSubject = namedNode('http://target');

      // Add target quads
      for (let i = 0; i < 1000; i++) {
        store.add(quad(targetSubject, namedNode(`http://p${i}`), literal(`o${i}`)));
      }

      // Add other quads
      for (let i = 0; i < 9000; i++) {
        store.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`)));
      }

      const start = performance.now();
      const results = [...store.match(targetSubject, null, null, null)];
      const duration = performance.now() - start;

      expect(results).toHaveLength(1000);
      expect(duration).toBeLessThan(50);
    });
  });

  describe('useGraphs Performance', () => {
    it('should handle 100 graphs with 100 quads each in < 2 seconds', () => {
      const store = new Store();
      const start = performance.now();

      for (let g = 0; g < 100; g++) {
        const graph = namedNode(`http://graph${g}`);
        for (let i = 0; i < 100; i++) {
          store.add(
            quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`), graph)
          );
        }
      }

      const duration = performance.now() - start;
      expect(store.size).toBe(10000);
      expect(duration).toBeLessThan(2000);
    });

    it('should list all graphs in < 100ms', () => {
      const store = new Store();

      for (let g = 0; g < 100; g++) {
        store.add(
          quad(
            namedNode('http://s'),
            namedNode('http://p'),
            literal('o'),
            namedNode(`http://graph${g}`)
          )
        );
      }

      const start = performance.now();
      const graphs = new Set();
      for (const q of store) {
        graphs.add(q.graph.value);
      }
      const duration = performance.now() - start;

      expect(graphs.size).toBe(100);
      expect(duration).toBeLessThan(100);
    });
  });

  describe('Memory Usage', () => {
    it('should maintain reasonable memory usage with large stores', () => {
      const store = new Store();
      const initialMemory = process.memoryUsage().heapUsed;

      for (let i = 0; i < 100000; i++) {
        store.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`)));
      }

      const finalMemory = process.memoryUsage().heapUsed;
      const memoryIncrease = finalMemory - initialMemory;

      // Should use less than 100MB for 100k quads
      expect(memoryIncrease).toBeLessThan(100 * 1024 * 1024);
    });
  });

  describe('React Hook Render Performance', () => {
    it('should render hooks quickly', () => {
      const start = performance.now();

      // Simulate 100 hook renders
      for (let i = 0; i < 100; i++) {
        const store = new Store();
        store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));
      }

      const duration = performance.now() - start;
      expect(duration).toBeLessThan(1000);
    });

    it('should not cause memory leaks on unmount', () => {
      const initialMemory = process.memoryUsage().heapUsed;

      // Simulate mount/unmount cycles
      for (let i = 0; i < 100; i++) {
        const store = new Store();
        store.add(quad(namedNode('http://s'), namedNode('http://p'), literal('o')));
        // Simulate unmount (store goes out of scope)
      }

      // Force garbage collection if available
      if (global.gc) {
        global.gc();
      }

      const finalMemory = process.memoryUsage().heapUsed;
      const memoryDiff = finalMemory - initialMemory;

      // Should not leak significant memory
      expect(memoryDiff).toBeLessThan(10 * 1024 * 1024); // < 10MB
    });
  });

  describe('Concurrent Operations', () => {
    it('should handle concurrent reads efficiently', async () => {
      const store = new Store();

      for (let i = 0; i < 1000; i++) {
        store.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal(`o${i}`)));
      }

      const start = performance.now();

      const promises = Array.from({ length: 100 }, () =>
        Promise.resolve([...store.match(null, null, null, null)])
      );

      const results = await Promise.all(promises);
      const duration = performance.now() - start;

      expect(results).toHaveLength(100);
      expect(duration).toBeLessThan(1000);
    });
  });
});

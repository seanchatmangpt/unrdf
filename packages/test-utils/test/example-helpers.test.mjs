/**
 * Example tests demonstrating new test utilities
 * These serve as both tests and documentation
 */

import { test, expect, describe } from 'vitest';
import {
  createTestStore,
  createTestWorkflow,
  mockOTEL,
  waitForCondition,
  createQuad,
  measureTime,
  testBatch,
  snapshotStore,
  assertSnapshotsEqual,
  sampleRDF,
  sampleWorkflows,
  performanceFixtures,
} from '../src/index.mjs';

describe('Test Helper Examples', () => {
  describe('createTestStore', () => {
    test('creates empty store', () => {
      const store = createTestStore();
      expect(store).toBeDefined();
      expect(store.size).toBe(0);
    });

    test('creates store with initial quads', () => {
      const quad = createQuad(
        'http://example.org/subject',
        'http://example.org/predicate',
        'http://example.org/object'
      );

      const store = createTestStore({ quads: [quad] });
      expect(store.size).toBe(1);
    });

    test('tracks metrics when enabled', () => {
      const store = createTestStore({ enableMetrics: true });
      const quad = createQuad('http://ex.org/s', 'http://ex.org/p', 'http://ex.org/o');

      store.add(quad);
      store.add(quad); // Duplicate

      const matches = Array.from(store.match());

      const metrics = store.getMetrics();
      expect(metrics.addCount).toBe(2);
      expect(metrics.matchCount).toBe(1);
    });
  });

  describe('createTestWorkflow', () => {
    test('creates default workflow', () => {
      const workflow = createTestWorkflow();

      expect(workflow.id).toBe('test-workflow');
      expect(workflow.name).toBe('Test Workflow');
      expect(workflow.tasks).toHaveLength(3);
      expect(workflow.flows).toHaveLength(2);
    });

    test('creates custom workflow', () => {
      const workflow = createTestWorkflow({
        id: 'custom-flow',
        name: 'Custom Flow',
        tasks: [
          { id: 'task1', type: 'atomic', name: 'Task 1' },
          { id: 'task2', type: 'atomic', name: 'Task 2' },
        ],
        flows: [{ from: 'task1', to: 'task2' }],
      });

      expect(workflow.id).toBe('custom-flow');
      expect(workflow.tasks).toHaveLength(2);
    });
  });

  describe('mockOTEL', () => {
    test('creates mock tracer', () => {
      const otel = mockOTEL();
      const tracer = otel.getTracer('test');

      expect(tracer).toBeDefined();
      expect(typeof tracer.startSpan).toBe('function');
    });

    test('captures spans when enabled', () => {
      const otel = mockOTEL({ capture: true });
      const tracer = otel.getTracer('test');

      tracer.startActiveSpan('operation', (span) => {
        span.setAttribute('key', 'value');
        span.end();
      });

      const spans = otel.getSpans();
      expect(spans).toHaveLength(1);
      expect(spans[0].name).toBe('operation');
    });

    test('captures attributes', () => {
      const otel = mockOTEL({ capture: true });
      const tracer = otel.getTracer('test');

      const span = tracer.startSpan('test-span');
      span.setAttribute('key1', 'value1');
      span.setAttribute('key2', 'value2');
      span.end();

      const attributes = otel.getAttributes();
      expect(attributes).toHaveLength(2);
      expect(attributes[0]).toEqual({ key: 'key1', value: 'value1' });
    });
  });

  describe('waitForCondition', () => {
    test('resolves when condition becomes true', async () => {
      let value = false;
      setTimeout(() => (value = true), 100);

      await waitForCondition(() => value, { timeout: 1000 });

      expect(value).toBe(true);
    });

    test('throws on timeout', async () => {
      await expect(
        waitForCondition(() => false, {
          timeout: 100,
          message: 'Should timeout',
        })
      ).rejects.toThrow('Should timeout');
    });

    test('uses custom interval', async () => {
      let callCount = 0;
      let value = false;

      setTimeout(() => (value = true), 250);

      await waitForCondition(
        () => {
          callCount++;
          return value;
        },
        { timeout: 1000, interval: 50 }
      );

      // Should have polled ~5 times (250ms / 50ms interval)
      expect(callCount).toBeGreaterThanOrEqual(4);
      expect(callCount).toBeLessThanOrEqual(6);
    });
  });

  describe('measureTime', () => {
    test('measures execution time', async () => {
      const { result, duration } = await measureTime(async () => {
        await new Promise((resolve) => setTimeout(resolve, 100));
        return 'done';
      });

      expect(result).toBe('done');
      expect(duration).toBeGreaterThanOrEqual(95); // ~100ms (with margin)
      expect(duration).toBeLessThan(150);
    });

    test('measures synchronous operations', async () => {
      const { result, duration } = await measureTime(() => {
        return 42;
      });

      expect(result).toBe(42);
      expect(duration).toBeLessThan(10); // Should be very fast
    });
  });

  describe('testBatch', () => {
    test('runs operations in parallel', async () => {
      const results = await testBatch(
        [
          async () => 'result1',
          async () => 'result2',
          async () => 'result3',
        ],
        { parallel: true }
      );

      expect(results).toEqual(['result1', 'result2', 'result3']);
    });

    test('runs operations sequentially', async () => {
      const order = [];
      const results = await testBatch(
        [
          async () => {
            order.push(1);
            return 1;
          },
          async () => {
            order.push(2);
            return 2;
          },
          async () => {
            order.push(3);
            return 3;
          },
        ],
        { parallel: false }
      );

      expect(results).toEqual([1, 2, 3]);
      expect(order).toEqual([1, 2, 3]);
    });

    test('respects timeout', async () => {
      await expect(
        testBatch(
          [
            async () => {
              await new Promise((resolve) => setTimeout(resolve, 200));
              return 'done';
            },
          ],
          { timeout: 100 }
        )
      ).rejects.toThrow('Operation timeout');
    });
  });

  describe('snapshotStore', () => {
    test('captures store state', () => {
      const store = createTestStore();
      const quad = createQuad('http://ex.org/s', 'http://ex.org/p', 'http://ex.org/o');
      store.add(quad);

      const snapshot = snapshotStore(store);

      expect(snapshot.size).toBe(1);
      expect(snapshot.quads).toHaveLength(1);
      expect(snapshot.timestamp).toBeDefined();
    });

    test('asserts equal snapshots', () => {
      const store1 = createTestStore();
      const store2 = createTestStore();
      const quad = createQuad('http://ex.org/s', 'http://ex.org/p', 'http://ex.org/o');

      store1.add(quad);
      store2.add(quad);

      const snapshot1 = snapshotStore(store1);
      const snapshot2 = snapshotStore(store2);

      expect(() => assertSnapshotsEqual(snapshot1, snapshot2)).not.toThrow();
    });

    test('throws on different snapshots', () => {
      const store1 = createTestStore();
      const store2 = createTestStore();

      store1.add(createQuad('http://ex.org/s1', 'http://ex.org/p', 'http://ex.org/o'));
      store2.add(createQuad('http://ex.org/s2', 'http://ex.org/p', 'http://ex.org/o'));

      const snapshot1 = snapshotStore(store1);
      const snapshot2 = snapshotStore(store2);

      expect(() => assertSnapshotsEqual(snapshot1, snapshot2)).toThrow();
    });
  });

  describe('Fixtures', () => {
    test('uses sample RDF data', () => {
      const store = createTestStore({ quads: sampleRDF.person.quads });
      expect(store.size).toBe(3);
    });

    test('uses sample workflows', () => {
      const workflow = sampleWorkflows.linear;
      expect(workflow.id).toBe('linear-workflow');
      expect(workflow.tasks).toHaveLength(3);
    });

    test('generates performance test data', () => {
      const quads = performanceFixtures.generateQuads(100);
      expect(quads).toHaveLength(100);

      const store = createTestStore({ quads });
      expect(store.size).toBe(100);
    });

    test('generates large workflows', () => {
      const workflow = performanceFixtures.generateLargeWorkflow(50);
      expect(workflow.tasks).toHaveLength(50);
      expect(workflow.flows).toHaveLength(49); // N-1 flows for linear workflow
    });
  });
});

describe('Performance Examples', () => {
  test('processes 1000 quads quickly', async () => {
    const quads = performanceFixtures.generateQuads(1000);
    const store = createTestStore({ enableMetrics: true });

    const { duration } = await measureTime(async () => {
      for (const quad of quads) {
        store.add(quad);
      }
    });

    expect(store.size).toBe(1000);
    expect(duration).toBeLessThan(100); // Should complete in <100ms
  });

  test('handles concurrent store operations', async () => {
    const store = createTestStore();
    const quads = Array(50)
      .fill(null)
      .map((_, i) =>
        createQuad(`http://ex.org/s${i}`, 'http://ex.org/p', 'http://ex.org/o')
      );

    const results = await testBatch(
      quads.map((quad) => () => store.add(quad)),
      { parallel: true }
    );

    expect(results).toHaveLength(50);
    expect(store.size).toBe(50);
  });
});

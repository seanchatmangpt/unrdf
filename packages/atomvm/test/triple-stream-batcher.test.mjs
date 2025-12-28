/**
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  TripleStreamBatcher,
  createTripleStreamBatcher,
} from '../src/triple-stream-batcher.mjs';

/**
 * Create a mock triple for testing
 *
 * @param {number} id - Triple ID
 * @returns {Object} Mock triple
 */
function createMockTriple(id) {
  return {
    subject: { termType: 'NamedNode', value: `http://example.org/s${id}` },
    predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
    object: { termType: 'Literal', value: `value${id}` },
  };
}

/**
 * Create an async iterable from an array
 *
 * @param {Array} items - Items to iterate
 * @param {number} [delay=0] - Delay between items in ms
 * @returns {AsyncIterable} Async iterable
 */
async function* createAsyncIterable(items, delay = 0) {
  for (const item of items) {
    if (delay > 0) {
      await new Promise((resolve) => setTimeout(resolve, delay));
    }
    yield item;
  }
}

describe('TripleStreamBatcher', () => {
  let batcher;

  beforeEach(() => {
    batcher = new TripleStreamBatcher({ batchSize: 100, timeout: 50 });
  });

  afterEach(() => {
    if (batcher && batcher.getState() !== 'Destroyed') {
      batcher.destroy();
    }
  });

  describe('constructor', () => {
    it('should create batcher with default options', () => {
      const defaultBatcher = new TripleStreamBatcher();
      expect(defaultBatcher.batchSize).toBe(100);
      expect(defaultBatcher.timeout).toBe(50);
      expect(defaultBatcher.getState()).toBe('Idle');
      defaultBatcher.destroy();
    });

    it('should create batcher with custom options', () => {
      const customBatcher = new TripleStreamBatcher({
        batchSize: 50,
        timeout: 100,
      });
      expect(customBatcher.batchSize).toBe(50);
      expect(customBatcher.timeout).toBe(100);
      customBatcher.destroy();
    });
  });

  describe('addTriple', () => {
    it('should accumulate triples until threshold', async () => {
      const batches = [];
      batcher.onBatch(async (batch) => {
        batches.push(batch);
        return { success: true };
      });

      // Add 99 triples - should not trigger batch
      for (let i = 0; i < 99; i++) {
        batcher.addTriple(createMockTriple(i));
      }

      expect(batcher.getQueueSize()).toBe(99);
      expect(batches.length).toBe(0);
      expect(batcher.getState()).toBe('Accumulating');
    });

    it('should validate triple structure', () => {
      expect(() => batcher.addTriple(null)).toThrow('Triple must be a non-null object');
      expect(() => batcher.addTriple({})).toThrow('Triple must have subject, predicate, and object');
      expect(() => batcher.addTriple({ subject: {} })).toThrow(
        'Triple must have subject, predicate, and object'
      );
    });

    it('should throw when destroyed', () => {
      batcher.destroy();
      expect(() => batcher.addTriple(createMockTriple(0))).toThrow('batcher has been destroyed');
    });
  });

  describe('addTriples', () => {
    it('should add multiple triples at once', () => {
      const triples = Array.from({ length: 50 }, (_, i) => createMockTriple(i));
      batcher.addTriples(triples);
      expect(batcher.getQueueSize()).toBe(50);
    });

    it('should validate array input', () => {
      expect(() => batcher.addTriples('not an array')).toThrow(
        'addTriples requires an array of triples'
      );
    });
  });

  describe('batch size limit trigger', () => {
    it('should send batch immediately when reaching threshold', async () => {
      const batches = [];
      batcher.onBatch(async (batch) => {
        batches.push([...batch]);
        return { success: true };
      });

      // Add exactly 100 triples
      for (let i = 0; i < 100; i++) {
        batcher.addTriple(createMockTriple(i));
      }

      // Allow async batch processing
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(batches.length).toBe(1);
      expect(batches[0].length).toBe(100);
      expect(batcher.getQueueSize()).toBe(0);
    });

    it('should send multiple batches for large inputs', async () => {
      const batches = [];
      batcher.onBatch(async (batch) => {
        batches.push([...batch]);
        return { success: true };
      });

      // Add 250 triples
      for (let i = 0; i < 250; i++) {
        batcher.addTriple(createMockTriple(i));
      }

      // Allow async batch processing plus timeout for partial batch
      await new Promise((resolve) => setTimeout(resolve, 100));

      // Should have 3 batches: 100 + 100 + 50 (after timeout)
      expect(batches.length).toBe(3);
      expect(batches[0].length).toBe(100);
      expect(batches[1].length).toBe(100);
      expect(batches[2].length).toBe(50);
      expect(batcher.getQueueSize()).toBe(0);
    });
  });

  describe('timeout trigger', () => {
    it('should send partial batch after timeout', async () => {
      const batches = [];
      batcher.onBatch(async (batch) => {
        batches.push([...batch]);
        return { success: true };
      });

      // Add 50 triples - below threshold
      for (let i = 0; i < 50; i++) {
        batcher.addTriple(createMockTriple(i));
      }

      expect(batches.length).toBe(0);

      // Wait for timeout
      await new Promise((resolve) => setTimeout(resolve, 100));

      expect(batches.length).toBe(1);
      expect(batches[0].length).toBe(50);
    });
  });

  describe('flush', () => {
    it('should force send pending batch', async () => {
      const batches = [];
      batcher.onBatch(async (batch) => {
        batches.push([...batch]);
        return { success: true };
      });

      // Add 30 triples
      for (let i = 0; i < 30; i++) {
        batcher.addTriple(createMockTriple(i));
      }

      await batcher.flush();

      expect(batches.length).toBe(1);
      expect(batches[0].length).toBe(30);
      expect(batcher.getQueueSize()).toBe(0);
    });

    it('should handle empty flush', async () => {
      const batches = [];
      batcher.onBatch(async (batch) => {
        batches.push(batch);
        return { success: true };
      });

      await batcher.flush();
      expect(batches.length).toBe(0);
    });
  });

  describe('streamTriples', () => {
    it('should process async iterable of triples', async () => {
      const batches = [];
      batcher.onBatch(async (batch) => {
        batches.push([...batch]);
        return { success: true };
      });

      const triples = Array.from({ length: 150 }, (_, i) => createMockTriple(i));
      const metrics = await batcher.streamTriples(createAsyncIterable(triples));

      expect(metrics.totalTriples).toBe(150);
      expect(metrics.totalBatches).toBe(2); // 100 + 50
      expect(batches.length).toBe(2);
      expect(batches[0].length).toBe(100);
      expect(batches[1].length).toBe(50);
    });

    it('should handle empty async iterable', async () => {
      const batches = [];
      batcher.onBatch(async (batch) => {
        batches.push(batch);
        return { success: true };
      });

      const metrics = await batcher.streamTriples(createAsyncIterable([]));

      expect(metrics.totalTriples).toBe(0);
      expect(metrics.totalBatches).toBe(0);
    });
  });

  describe('backpressure', () => {
    it('should handle slow consumers', async () => {
      let processedBatches = 0;

      batcher.onBatch(async (batch) => {
        processedBatches++;
        // Simulate slow consumer
        await new Promise((resolve) => setTimeout(resolve, 10));
        return { success: true, slow: processedBatches === 1 };
      });

      // Add 100 triples to trigger first batch
      for (let i = 0; i < 100; i++) {
        batcher.addTriple(createMockTriple(i));
      }

      // Wait for batch to be processed
      await new Promise((resolve) => setTimeout(resolve, 50));

      expect(batcher.getState()).toBe('Paused');

      // Resume and add more
      batcher.resume();
      expect(batcher.getState()).toBe('Idle');
    });

    it('should apply backpressure when queue is full', () => {
      const smallQueueBatcher = new TripleStreamBatcher({
        batchSize: 100,
        timeout: 50,
        maxQueueSize: 50,
      });

      // No callback registered, so queue will fill
      for (let i = 0; i < 50; i++) {
        smallQueueBatcher.addTriple(createMockTriple(i));
      }

      expect(smallQueueBatcher.getState()).toBe('Paused');
      expect(smallQueueBatcher.getMetrics().backpressureEvents).toBe(1);

      smallQueueBatcher.destroy();
    });
  });

  describe('metrics', () => {
    it('should track batch metrics', async () => {
      batcher.onBatch(async (batch) => {
        return { success: true };
      });

      // Add 200 triples
      for (let i = 0; i < 200; i++) {
        batcher.addTriple(createMockTriple(i));
      }

      await batcher.flush();

      const metrics = batcher.getMetrics();
      expect(metrics.totalTriples).toBe(200);
      expect(metrics.totalBatches).toBe(2);
      expect(metrics.avgBatchSize).toBe(100);
      expect(metrics.avgLatencyMs).toBeGreaterThanOrEqual(0);
    });
  });

  describe('throughput benchmark', () => {
    it('should achieve 10,000+ triples/second', async () => {
      const tripleCount = 10000;
      let processedCount = 0;

      batcher = new TripleStreamBatcher({ batchSize: 100, timeout: 10 });
      batcher.onBatch(async (batch) => {
        processedCount += batch.length;
        return { success: true };
      });

      const triples = Array.from({ length: tripleCount }, (_, i) => createMockTriple(i));
      const startTime = Date.now();

      await batcher.streamTriples(createAsyncIterable(triples));

      const duration = Date.now() - startTime;
      const throughput = (processedCount / duration) * 1000;

      console.log(`Throughput benchmark: ${processedCount} triples in ${duration}ms`);
      console.log(`Throughput: ${Math.round(throughput)} triples/second`);

      expect(processedCount).toBe(tripleCount);
      expect(throughput).toBeGreaterThanOrEqual(10000);
    });
  });

  describe('roundtrip integration', () => {
    it('should batch 1000 triples into 10 batches of 100', async () => {
      const batches = [];
      let totalLatency = 0;

      batcher.onBatch(async (batch) => {
        const start = performance.now();
        // Simulate Oxigraph insert
        await new Promise((resolve) => setTimeout(resolve, 1));
        totalLatency += performance.now() - start;
        batches.push([...batch]);
        return { success: true };
      });

      const startTime = Date.now();

      // Add 1000 triples
      for (let i = 0; i < 1000; i++) {
        batcher.addTriple(createMockTriple(i));
      }

      await batcher.flush();

      const totalTime = Date.now() - startTime;

      expect(batches.length).toBe(10);
      expect(batches.every((b) => b.length === 100)).toBe(true);

      const totalTriples = batches.reduce((sum, b) => sum + b.length, 0);
      expect(totalTriples).toBe(1000);

      console.log(`Roundtrip test: 1000 triples in ${totalTime}ms (${batches.length} batches)`);
      console.log(`Average batch latency: ${totalLatency / batches.length}ms`);

      // Total latency should be <200ms for bulk operation
      expect(totalTime).toBeLessThan(200);
    });
  });

  describe('onBatch callback', () => {
    it('should validate callback is a function', () => {
      expect(() => batcher.onBatch('not a function')).toThrow(
        'onBatch callback must be a function'
      );
    });

    it('should fire callback with correct batch', async () => {
      const receivedBatches = [];

      batcher.onBatch(async (batch) => {
        receivedBatches.push({
          size: batch.length,
          firstSubject: batch[0].subject.value,
          lastSubject: batch[batch.length - 1].subject.value,
        });
        return { success: true };
      });

      for (let i = 0; i < 100; i++) {
        batcher.addTriple(createMockTriple(i));
      }

      await batcher.flush();

      expect(receivedBatches.length).toBe(1);
      expect(receivedBatches[0].size).toBe(100);
      expect(receivedBatches[0].firstSubject).toBe('http://example.org/s0');
      expect(receivedBatches[0].lastSubject).toBe('http://example.org/s99');
    });
  });

  describe('destroy', () => {
    it('should clean up resources', () => {
      for (let i = 0; i < 50; i++) {
        batcher.addTriple(createMockTriple(i));
      }

      batcher.destroy();

      expect(batcher.getState()).toBe('Destroyed');
      expect(batcher.getQueueSize()).toBe(0);
    });
  });

  describe('createTripleStreamBatcher factory', () => {
    it('should create batcher instance', () => {
      const factoryBatcher = createTripleStreamBatcher({ batchSize: 50 });
      expect(factoryBatcher).toBeInstanceOf(TripleStreamBatcher);
      expect(factoryBatcher.batchSize).toBe(50);
      factoryBatcher.destroy();
    });
  });
});

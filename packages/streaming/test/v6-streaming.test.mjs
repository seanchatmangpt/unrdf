/**
 * @vitest-environment node
 * @file V6 Streaming Features Test Suite
 */
import { describe, it, expect, beforeEach, vi } from 'vitest';
import { Readable } from 'stream';
import { namedNode, literal } from '@rdfjs/data-model';
import {
  createRDFStreamParser,
  parseRDFStream,
  createPerformanceMonitor,
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
  mergeSyncMessages,
} from '../src/index.mjs';
import {
  generateSyntheticRDF,
  createReadableStreamFromString,
} from '../src/benchmarks.mjs';

describe('V6 Streaming Features', () => {
  describe('RDF Stream Parser', () => {
    it('should parse turtle stream with backpressure', async () => {
      const rdfData = `
        @prefix ex: <http://example.org/> .
        ex:Alice ex:name "Alice" .
        ex:Bob ex:name "Bob" .
        ex:Charlie ex:name "Charlie" .
      `;

      const stream = createReadableStreamFromString(rdfData);
      const parser = createRDFStreamParser({ format: 'turtle', chunkSize: 2 });

      const chunks = [];
      parser.on('data', (chunk) => {
        if (chunk.type === 'quads') {
          chunks.push(chunk);
        }
      });

      await new Promise((resolve, reject) => {
        stream.pipe(parser);
        parser.on('end', resolve);
        parser.on('error', reject);
      });

      expect(chunks.length).toBeGreaterThan(0);
      expect(chunks.every(c => c.type === 'quads')).toBe(true);
    });

    it('should handle large datasets efficiently', async () => {
      const rdfData = generateSyntheticRDF(10000, 'n-triples');
      const stream = createReadableStreamFromString(rdfData);

      const quads = await parseRDFStream(stream, {
        format: 'n-triples',
        chunkSize: 1000,
      });

      expect(quads.length).toBe(10000);
    });

    it('should track metrics during parsing', async () => {
      const rdfData = generateSyntheticRDF(1000, 'n-triples');
      const stream = createReadableStreamFromString(rdfData);
      const parser = createRDFStreamParser({ format: 'n-triples' });

      await new Promise((resolve, reject) => {
        stream.pipe(parser);
        parser.on('end', resolve);
        parser.on('error', reject);
      });

      const metrics = parser.getMetrics();
      expect(metrics.quadsProcessed).toBe(1000);
      expect(metrics.chunksEmitted).toBeGreaterThan(0);
      expect(metrics.throughput).toBeGreaterThan(0);
    });

    it('should handle backpressure events', async () => {
      const rdfData = generateSyntheticRDF(5000, 'n-triples');
      const stream = createReadableStreamFromString(rdfData, { chunkSize: 256 });
      const parser = createRDFStreamParser({
        format: 'n-triples',
        enableBackpressure: true,
        chunkSize: 500,
      });

      await new Promise((resolve, reject) => {
        stream.pipe(parser);
        parser.on('end', resolve);
        parser.on('error', reject);
      });

      const metrics = parser.getMetrics();
      expect(metrics.quadsProcessed).toBe(5000);
      expect(metrics.backpressureRate).toBeGreaterThanOrEqual(0);
    });

    it('should call onQuad callback for each quad', async () => {
      const rdfData = generateSyntheticRDF(100, 'n-triples');
      const stream = createReadableStreamFromString(rdfData);
      const onQuad = vi.fn();

      const parser = createRDFStreamParser({
        format: 'n-triples',
        onQuad,
      });

      await new Promise((resolve, reject) => {
        stream.pipe(parser);
        parser.on('end', resolve);
        parser.on('error', reject);
      });

      expect(onQuad).toHaveBeenCalledTimes(100);
    });
  });

  describe('Performance Monitor', () => {
    it('should track quad processing metrics', () => {
      const monitor = createPerformanceMonitor({
        sampleInterval: 100,
        windowSize: 10,
      });

      monitor.start();

      // Simulate quad processing
      for (let i = 0; i < 1000; i++) {
        monitor.recordQuad(Math.random() * 10);
        monitor.recordBytes(50);
      }

      const current = monitor.getCurrentMetrics();
      expect(current.quadsProcessed).toBe(1000);
      expect(current.bytesProcessed).toBe(50000);

      monitor.stop();
    });

    it('should calculate throughput correctly', (done) => {
      const monitor = createPerformanceMonitor({
        sampleInterval: 50,
        enableThroughputTracking: true,
      });

      monitor.start();

      // Process quads over time
      let processed = 0;
      const interval = setInterval(() => {
        for (let i = 0; i < 100; i++) {
          monitor.recordQuad();
        }
        processed += 100;

        if (processed >= 500) {
          clearInterval(interval);
          monitor.stop();

          const report = monitor.getReport();
          expect(report.throughput.mean).toBeGreaterThan(0);
          expect(report.summary.quadsProcessed).toBe(500);
          done();
        }
      }, 10);
    });

    it('should track latency statistics', () => {
      const monitor = createPerformanceMonitor({
        enableLatencyTracking: true,
      });

      monitor.start();

      // Record varying latencies
      const latencies = [1, 2, 3, 10, 15, 100, 200];
      latencies.forEach(l => monitor.recordQuad(l));

      monitor.stop();

      const report = monitor.getReport();
      expect(report.latency.mean).toBeGreaterThan(0);
      expect(report.latency.p95).toBeGreaterThan(report.latency.p50);
    });

    it('should track memory usage', (done) => {
      const monitor = createPerformanceMonitor({
        sampleInterval: 50,
        enableMemoryTracking: true,
      });

      monitor.start();

      setTimeout(() => {
        monitor.stop();
        const report = monitor.getReport();

        expect(report.memory.heapUsed).toBeDefined();
        expect(report.memory.heapUsed.mean).toBeGreaterThan(0);
        done();
      }, 150);
    });

    it('should record backpressure events', () => {
      const monitor = createPerformanceMonitor();

      monitor.start();
      monitor.recordBackpressure();
      monitor.recordBackpressure();
      monitor.recordBackpressure();
      monitor.stop();

      const report = monitor.getReport();
      expect(report.backpressure.events).toBe(3);
    });

    it('should emit threshold violations', (done) => {
      const monitor = createPerformanceMonitor({
        sampleInterval: 50,
        thresholds: {
          throughputMin: 10000, // Very high threshold
        },
      });

      monitor.on('threshold-violation', (violation) => {
        expect(violation.metric).toBe('throughput');
        expect(violation.value).toBeLessThan(violation.threshold);
        monitor.stop();
        done();
      });

      monitor.start();

      // Process slowly
      setTimeout(() => {
        monitor.recordQuad();
      }, 60);
    });

    it('should reset metrics', () => {
      const monitor = createPerformanceMonitor();

      monitor.start();
      monitor.recordQuad();
      monitor.recordQuad();
      monitor.stop();

      expect(monitor.getCurrentMetrics().quadsProcessed).toBe(2);

      monitor.reset();

      expect(monitor.getCurrentMetrics().quadsProcessed).toBe(0);
    });
  });

  describe('Sync Protocol', () => {
    it('should create sync message with checksum', () => {
      const changes = [
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal('value'),
          },
          timestamp: 12345,
        },
      ];

      const message = createSyncMessage(changes);

      expect(message.version).toBe('1.0');
      expect(message.changes).toEqual(changes);
      expect(message.checksum).toBeDefined();
      expect(typeof message.checksum).toBe('string');
      expect(message.checksum.length).toBe(64); // SHA-256
    });

    it('should parse and validate sync message', () => {
      const changes = [
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal('value'),
          },
          timestamp: 12345,
        },
      ];

      const message = createSyncMessage(changes);
      const parsed = parseSyncMessage(message);

      expect(parsed).toEqual(message);
    });

    it('should detect checksum mismatch', () => {
      const message = createSyncMessage([
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal('value'),
          },
          timestamp: 12345,
        },
      ]);

      message.checksum = 'invalid_checksum';

      expect(() => parseSyncMessage(message)).toThrow('Checksum mismatch');
    });

    it('should calculate consistent checksums', () => {
      const changes = [
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s1'),
            predicate: namedNode('http://example.org/p'),
            object: literal('1'),
          },
          timestamp: 1000,
        },
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s2'),
            predicate: namedNode('http://example.org/p'),
            object: literal('2'),
          },
          timestamp: 2000,
        },
      ];

      const checksum1 = calculateChecksum(changes);
      const checksum2 = calculateChecksum(changes);

      expect(checksum1).toBe(checksum2);
    });

    it('should merge sync messages correctly', () => {
      const message1 = createSyncMessage([
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s1'),
            predicate: namedNode('http://example.org/p'),
            object: literal('1'),
          },
          timestamp: 1000,
        },
      ]);

      const message2 = createSyncMessage([
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s2'),
            predicate: namedNode('http://example.org/p'),
            object: literal('2'),
          },
          timestamp: 2000,
        },
      ]);

      const merged = mergeSyncMessages([message1, message2]);

      expect(merged.changes).toHaveLength(2);
      expect(merged.changes[0].timestamp).toBe(1000);
      expect(merged.changes[1].timestamp).toBe(2000);
    });

    it('should deduplicate merged messages', () => {
      const change = {
        type: 'add',
        quad: {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('value'),
        },
        timestamp: 1000,
      };

      const message1 = createSyncMessage([change]);
      const message2 = createSyncMessage([change]);

      const merged = mergeSyncMessages([message1, message2]);

      expect(merged.changes).toHaveLength(1);
    });
  });

  describe('Integration: Streaming Pipeline', () => {
    it('should process RDF stream with monitoring', async () => {
      const rdfData = generateSyntheticRDF(1000, 'n-triples');
      const stream = createReadableStreamFromString(rdfData);

      const monitor = createPerformanceMonitor({
        sampleInterval: 50,
        enableThroughputTracking: true,
        enableLatencyTracking: true,
      });

      const parser = createRDFStreamParser({
        format: 'n-triples',
        chunkSize: 100,
      });

      monitor.start();

      parser.on('data', (chunk) => {
        if (chunk.type === 'quads') {
          chunk.data.forEach(() => {
            monitor.recordQuad(Math.random() * 5);
          });
          monitor.recordChunk();
        }
      });

      await new Promise((resolve, reject) => {
        stream.pipe(parser);
        parser.on('end', resolve);
        parser.on('error', reject);
      });

      monitor.stop();

      const parserMetrics = parser.getMetrics();
      const monitorReport = monitor.getReport();

      expect(parserMetrics.quadsProcessed).toBe(1000);
      expect(monitorReport.summary.quadsProcessed).toBe(1000);
      expect(monitorReport.throughput.mean).toBeGreaterThan(0);
    });
  });
});

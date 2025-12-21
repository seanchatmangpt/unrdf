#!/usr/bin/env node
/**
 * @fileoverview Streaming Package Performance Analysis
 *
 * Specialized benchmarks for streaming operations:
 * - Stream throughput
 * - Backpressure handling
 * - Memory usage during streaming
 * - Transform stream performance
 *
 * @version 1.0.0
 */

import { performance } from 'node:perf_hooks';
import { Readable, Transform, pipeline } from 'node:stream';
import { promisify } from 'node:util';

const pipelineAsync = promisify(pipeline);

/**
 * Get memory usage
 */
function getMemoryUsage() {
  const usage = process.memoryUsage();
  return {
    rss: usage.rss,
    heapUsed: usage.heapUsed,
    heapTotal: usage.heapTotal,
  };
}

/**
 * Format bytes
 */
function formatBytes(bytes) {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return `${(bytes / k ** i).toFixed(2)} ${sizes[i]}`;
}

/**
 * Create test data stream
 * @param {number} count - Number of items
 * @returns {Readable} Readable stream
 */
function createTestStream(count) {
  let i = 0;
  return new Readable({
    objectMode: true,
    read() {
      if (i < count) {
        this.push({
          id: i,
          subject: `http://example.org/subject${i}`,
          predicate: 'http://example.org/predicate',
          object: `Object ${i}`,
        });
        i++;
      } else {
        this.push(null);
      }
    },
  });
}

/**
 * Create transform stream
 * @returns {Transform} Transform stream
 */
function createTransformStream() {
  return new Transform({
    objectMode: true,
    transform(chunk, _encoding, callback) {
      // Simulate transformation work
      const transformed = {
        ...chunk,
        processed: true,
        timestamp: Date.now(),
      };
      callback(null, transformed);
    },
  });
}

/**
 * Benchmark stream throughput
 * @param {number} itemCount - Number of items to stream
 * @returns {Promise<object>} Performance metrics
 */
async function benchmarkStreamThroughput(itemCount) {
  const memBefore = getMemoryUsage();
  const start = performance.now();

  let processedCount = 0;
  const stream = createTestStream(itemCount);

  await new Promise((resolve, reject) => {
    stream.on('data', () => {
      processedCount++;
    });
    stream.on('end', resolve);
    stream.on('error', reject);
  });

  const duration = performance.now() - start;
  const memAfter = getMemoryUsage();

  return {
    itemCount,
    processedCount,
    duration: duration.toFixed(2),
    throughput: (itemCount / (duration / 1000)).toFixed(2),
    memoryGrowth: formatBytes(memAfter.heapUsed - memBefore.heapUsed),
    avgTimePerItem: (duration / itemCount).toFixed(4),
  };
}

/**
 * Benchmark transform stream
 * @param {number} itemCount - Number of items
 * @returns {Promise<object>} Performance metrics
 */
async function benchmarkTransformStream(itemCount) {
  const memBefore = getMemoryUsage();
  const start = performance.now();

  let processedCount = 0;
  const source = createTestStream(itemCount);
  const transform = createTransformStream();

  await pipelineAsync(
    source,
    transform,
    new Transform({
      objectMode: true,
      transform(chunk, _encoding, callback) {
        processedCount++;
        callback();
      },
    })
  );

  const duration = performance.now() - start;
  const memAfter = getMemoryUsage();

  return {
    itemCount,
    processedCount,
    duration: duration.toFixed(2),
    throughput: (itemCount / (duration / 1000)).toFixed(2),
    memoryGrowth: formatBytes(memAfter.heapUsed - memBefore.heapUsed),
    avgTimePerItem: (duration / itemCount).toFixed(4),
  };
}

/**
 * Benchmark backpressure handling
 * @param {number} itemCount - Number of items
 * @returns {Promise<object>} Performance metrics
 */
async function benchmarkBackpressure(itemCount) {
  const memBefore = getMemoryUsage();
  const start = performance.now();
  const memorySnapshots = [];

  let processedCount = 0;
  const source = createTestStream(itemCount);

  // Slow consumer to create backpressure
  const slowConsumer = new Transform({
    objectMode: true,
    async transform(chunk, _encoding, callback) {
      // Simulate slow processing
      await new Promise(resolve => setImmediate(resolve));
      processedCount++;

      // Sample memory periodically
      if (processedCount % 100 === 0) {
        memorySnapshots.push(getMemoryUsage().heapUsed);
      }

      callback();
    },
  });

  await pipelineAsync(source, slowConsumer);

  const duration = performance.now() - start;
  const memAfter = getMemoryUsage();
  const peakMemory = memorySnapshots.length > 0 ? Math.max(...memorySnapshots) : 0;

  return {
    itemCount,
    processedCount,
    duration: duration.toFixed(2),
    throughput: (itemCount / (duration / 1000)).toFixed(2),
    memoryGrowth: formatBytes(memAfter.heapUsed - memBefore.heapUsed),
    peakMemory: formatBytes(peakMemory),
    backpressureHandled: memAfter.heapUsed < memBefore.heapUsed + 10 * 1024 * 1024,
  };
}

/**
 * Benchmark parallel streams
 * @param {number} streamCount - Number of parallel streams
 * @param {number} itemsPerStream - Items per stream
 * @returns {Promise<object>} Performance metrics
 */
async function benchmarkParallelStreams(streamCount, itemsPerStream) {
  const memBefore = getMemoryUsage();
  const start = performance.now();

  const streams = Array.from({ length: streamCount }, () => {
    return new Promise((resolve, reject) => {
      const stream = createTestStream(itemsPerStream);
      let count = 0;
      stream.on('data', () => count++);
      stream.on('end', () => resolve(count));
      stream.on('error', reject);
    });
  });

  const results = await Promise.all(streams);
  const totalProcessed = results.reduce((sum, count) => sum + count, 0);

  const duration = performance.now() - start;
  const memAfter = getMemoryUsage();

  return {
    streamCount,
    itemsPerStream,
    totalItems: streamCount * itemsPerStream,
    totalProcessed,
    duration: duration.toFixed(2),
    throughput: (totalProcessed / (duration / 1000)).toFixed(2),
    memoryGrowth: formatBytes(memAfter.heapUsed - memBefore.heapUsed),
  };
}

/**
 * Run all streaming benchmarks
 */
async function runStreamingBenchmarks() {
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║   STREAMING PERFORMANCE ANALYSIS                         ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  const results = {
    timestamp: new Date().toISOString(),
    nodeVersion: process.version,
  };

  // Throughput tests
  console.log('Benchmarking stream throughput...');
  results.throughput_1k = await benchmarkStreamThroughput(1000);
  results.throughput_10k = await benchmarkStreamThroughput(10000);
  results.throughput_100k = await benchmarkStreamThroughput(100000);

  // Transform tests
  console.log('Benchmarking transform streams...');
  results.transform_1k = await benchmarkTransformStream(1000);
  results.transform_10k = await benchmarkTransformStream(10000);

  // Backpressure tests
  console.log('Benchmarking backpressure handling...');
  results.backpressure_1k = await benchmarkBackpressure(1000);

  // Parallel streams
  console.log('Benchmarking parallel streams...');
  results.parallel_10_streams = await benchmarkParallelStreams(10, 1000);
  results.parallel_100_streams = await benchmarkParallelStreams(100, 100);

  // Summary
  console.log('\n╔═══════════════════════════════════════════════════════════╗');
  console.log('║   STREAMING SUMMARY                                       ║');
  console.log('╚═══════════════════════════════════════════════════════════╝\n');

  console.log('THROUGHPUT:');
  console.log(`  1K items:    ${results.throughput_1k.throughput} items/sec`);
  console.log(`  10K items:   ${results.throughput_10k.throughput} items/sec`);
  console.log(`  100K items:  ${results.throughput_100k.throughput} items/sec`);

  console.log('\nTRANSFORM:');
  console.log(`  1K items:    ${results.transform_1k.throughput} items/sec`);
  console.log(`  10K items:   ${results.transform_10k.throughput} items/sec`);

  console.log('\nBACKPRESSURE:');
  console.log(`  Handled:     ${results.backpressure_1k.backpressureHandled ? 'YES ✅' : 'NO ⚠️'}`);
  console.log(`  Peak Memory: ${results.backpressure_1k.peakMemory}`);

  console.log('\nPARALLEL STREAMS:');
  console.log(`  10 streams:  ${results.parallel_10_streams.throughput} items/sec`);
  console.log(`  100 streams: ${results.parallel_100_streams.throughput} items/sec`);

  // Write results
  const fs = await import('node:fs/promises');
  await fs.writeFile(
    '/Users/sac/unrdf/packages/atomvm/benchmarks/streaming-results.json',
    JSON.stringify(results, null, 2)
  );

  console.log('\n✅ Streaming benchmarks complete!');
  console.log('   Results saved to: packages/atomvm/benchmarks/streaming-results.json\n');

  return results;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runStreamingBenchmarks()
    .then(() => process.exit(0))
    .catch(error => {
      console.error('❌ Streaming benchmark failed:', error);
      console.error(error);
      process.exit(1);
    });
}

export { runStreamingBenchmarks };

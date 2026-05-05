#!/usr/bin/env node
/**
 * @file V6 Streaming Validation Script
 * @description
 * Manual validation of all v6 streaming features with comprehensive tests
 */

import { Readable } from 'stream';
import { dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal } = dataFactory;
import {
  createRDFStreamParser,
  parseRDFStream,
  createPerformanceMonitor,
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
  mergeSyncMessages,
  createChangeFeed,
} from './src/index.mjs';
import {
  generateSyntheticRDF,
  createReadableStreamFromString,
  runComprehensiveBenchmarks,
  saveBenchmarkResults,
} from './src/benchmarks.mjs';

console.log('🧪 V6 Streaming Features Validation\n');
console.log('=' .repeat(60));

let passed = 0;
let failed = 0;

function test(name, fn) {
  try {
    fn();
    console.log(`✅ ${name}`);
    passed++;
  } catch (error) {
    console.log(`❌ ${name}`);
    console.log(`   Error: ${error.message}`);
    failed++;
  }
}

async function testAsync(name, fn) {
  try {
    await fn();
    console.log(`✅ ${name}`);
    passed++;
  } catch (error) {
    console.log(`❌ ${name}`);
    console.log(`   Error: ${error.message}`);
    failed++;
  }
}

// Test 1: RDF Stream Parser Creation
test('RDFStreamParser creation', () => {
  const parser = createRDFStreamParser({ format: 'turtle' });
  if (!parser) throw new Error('Parser not created');
  if (typeof parser._transform !== 'function') throw new Error('Not a Transform stream');
});

// Test 2: Sync Protocol - Create Message
test('Sync Protocol: createSyncMessage', () => {
  const changes = [{
    type: 'add',
    quad: {
      subject: namedNode('http://example.org/s'),
      predicate: namedNode('http://example.org/p'),
      object: literal('value'),
    },
    timestamp: Date.now(),
  }];

  const message = createSyncMessage(changes);
  if (!message.checksum) throw new Error('No checksum');
  if (message.checksum.length !== 64) throw new Error('Invalid checksum length');
  if (message.version !== '1.0') throw new Error('Invalid version');
});

// Test 3: Sync Protocol - Parse Message
test('Sync Protocol: parseSyncMessage', () => {
  const changes = [{
    type: 'add',
    quad: {
      subject: namedNode('http://example.org/s'),
      predicate: namedNode('http://example.org/p'),
      object: literal('value'),
    },
    timestamp: 12345,
  }];

  const message = createSyncMessage(changes);
  const parsed = parseSyncMessage(message);

  if (parsed.checksum !== message.checksum) throw new Error('Checksum mismatch');
});

// Test 4: Sync Protocol - Checksum Consistency
test('Sync Protocol: calculateChecksum consistency', () => {
  const changes = [{
    type: 'add',
    quad: {
      subject: namedNode('http://example.org/s'),
      predicate: namedNode('http://example.org/p'),
      object: literal('value'),
    },
    timestamp: 12345,
  }];

  const checksum1 = calculateChecksum(changes);
  const checksum2 = calculateChecksum(changes);

  if (checksum1 !== checksum2) throw new Error('Checksums not consistent');
});

// Test 5: Sync Protocol - Merge Messages
test('Sync Protocol: mergeSyncMessages', () => {
  const message1 = createSyncMessage([{
    type: 'add',
    quad: {
      subject: namedNode('http://example.org/s1'),
      predicate: namedNode('http://example.org/p'),
      object: literal('1'),
    },
    timestamp: 1000,
  }]);

  const message2 = createSyncMessage([{
    type: 'add',
    quad: {
      subject: namedNode('http://example.org/s2'),
      predicate: namedNode('http://example.org/p'),
      object: literal('2'),
    },
    timestamp: 2000,
  }]);

  const merged = mergeSyncMessages([message1, message2]);

  if (merged.changes.length !== 2) throw new Error('Merge failed');
  if (merged.changes[0].timestamp !== 1000) throw new Error('Wrong order');
});

// Test 6: Performance Monitor Creation
test('Performance Monitor: creation', () => {
  const monitor = createPerformanceMonitor({
    sampleInterval: 1000,
    windowSize: 60,
  });

  if (!monitor) throw new Error('Monitor not created');
  if (typeof monitor.start !== 'function') throw new Error('Missing start method');
  if (typeof monitor.recordQuad !== 'function') throw new Error('Missing recordQuad method');
});

// Test 7: Performance Monitor - Record Metrics
test('Performance Monitor: record metrics', () => {
  const monitor = createPerformanceMonitor();

  monitor.start();
  monitor.recordQuad(5);
  monitor.recordQuad(10);
  monitor.recordBytes(100);
  monitor.recordChunk();
  monitor.stop();

  const metrics = monitor.getCurrentMetrics();

  if (metrics.quadsProcessed !== 2) throw new Error('Quad count wrong');
  if (metrics.bytesProcessed !== 100) throw new Error('Bytes count wrong');
  if (metrics.chunksProcessed !== 1) throw new Error('Chunk count wrong');
});

// Test 8: RDF Stream Parser - Parse Synthetic Data
await testAsync('RDF Stream Parser: parse synthetic data', async () => {
  const rdfData = generateSyntheticRDF(100, 'n-triples');
  const stream = createReadableStreamFromString(rdfData);

  const quads = await parseRDFStream(stream, {
    format: 'n-triples',
    chunkSize: 50,
  });

  if (quads.length !== 100) throw new Error(`Expected 100 quads, got ${quads.length}`);
});

// Test 9: RDF Stream Parser - Track Metrics
await testAsync('RDF Stream Parser: track metrics', async () => {
  const rdfData = generateSyntheticRDF(500, 'n-triples');
  const stream = createReadableStreamFromString(rdfData);
  const parser = createRDFStreamParser({ format: 'n-triples' });

  await new Promise((resolve, reject) => {
    stream.pipe(parser);
    parser.on('end', resolve);
    parser.on('error', reject);
  });

  const metrics = parser.getMetrics();

  if (metrics.quadsProcessed !== 500) throw new Error('Wrong quad count');
  if (metrics.throughput <= 0) throw new Error('Invalid throughput');
  if (metrics.chunksEmitted <= 0) throw new Error('No chunks emitted');
});

// Test 10: RDF Stream Parser - Backpressure Handling
await testAsync('RDF Stream Parser: backpressure handling', async () => {
  const rdfData = generateSyntheticRDF(1000, 'n-triples');
  const stream = createReadableStreamFromString(rdfData, { chunkSize: 128 });
  const parser = createRDFStreamParser({
    format: 'n-triples',
    enableBackpressure: true,
    chunkSize: 100,
  });

  await new Promise((resolve, reject) => {
    stream.pipe(parser);
    parser.on('end', resolve);
    parser.on('error', reject);
  });

  const metrics = parser.getMetrics();

  if (metrics.quadsProcessed !== 1000) throw new Error('Not all quads processed');
  if (metrics.backpressureRate < 0) throw new Error('Invalid backpressure rate');
});

// Test 11: Integration - Parser + Monitor
await testAsync('Integration: RDF Parser + Performance Monitor', async () => {
  const rdfData = generateSyntheticRDF(500, 'n-triples');
  const stream = createReadableStreamFromString(rdfData);

  const monitor = createPerformanceMonitor({
    enableThroughputTracking: true,
    enableLatencyTracking: true,
  });

  const parser = createRDFStreamParser({
    format: 'n-triples',
    onQuad: () => monitor.recordQuad(Math.random() * 5),
  });

  monitor.start();

  await new Promise((resolve, reject) => {
    stream.pipe(parser);
    parser.on('end', resolve);
    parser.on('error', reject);
  });

  monitor.stop();

  const parserMetrics = parser.getMetrics();
  const monitorMetrics = monitor.getCurrentMetrics();

  if (parserMetrics.quadsProcessed !== 500) throw new Error('Parser: wrong quad count');
  if (monitorMetrics.quadsProcessed !== 500) throw new Error('Monitor: wrong quad count');
});

// Test 12: Synthetic Data Generation
test('Benchmark Utils: generateSyntheticRDF', () => {
  const rdf = generateSyntheticRDF(100, 'turtle');

  if (!rdf) throw new Error('No data generated');
  if (rdf.length === 0) throw new Error('Empty data');
  if (!rdf.includes('http://example.org')) throw new Error('Invalid format');
});

// Test 13: Stream Creation Utility
test('Benchmark Utils: createReadableStreamFromString', () => {
  const stream = createReadableStreamFromString('test data', { chunkSize: 5 });

  if (!stream) throw new Error('Stream not created');
  if (typeof stream.pipe !== 'function') throw new Error('Not a stream');
});

console.log('\n' + '='.repeat(60));
console.log(`\n📊 Test Results:`);
console.log(`   ✅ Passed: ${passed}`);
console.log(`   ❌ Failed: ${failed}`);
console.log(`   📈 Success Rate: ${((passed / (passed + failed)) * 100).toFixed(1)}%\n`);

if (failed === 0) {
  console.log('🎉 All V6 streaming features validated successfully!\n');

  // Run comprehensive benchmarks
  console.log('🚀 Running Comprehensive Benchmarks...\n');

  try {
    const benchmarkResults = await runComprehensiveBenchmarks({
      iterations: 2,
      warmupIterations: 1,
      datasetSizes: [1000, 5000],
      formats: ['turtle', 'n-triples'],
      chunkSizes: [100, 1000],
    });

    console.log('\n📊 Benchmark Summary:');
    console.log(`   Duration: ${benchmarkResults.duration}ms`);
    console.log(`   Average Throughput: ${benchmarkResults.summary.tests.parsingThroughput.avgThroughput.toFixed(0)} quads/sec`);
    console.log(`   Max Throughput: ${benchmarkResults.summary.tests.parsingThroughput.maxThroughput.toFixed(0)} quads/sec`);
    console.log(`   Average Latency: ${benchmarkResults.summary.tests.changeFeedLatency.avgLatency.toFixed(2)}ms`);
    console.log(`   P99 Latency: ${benchmarkResults.summary.tests.changeFeedLatency.p99Latency.toFixed(2)}ms`);
    console.log(`   Avg Memory/Quad: ${benchmarkResults.summary.tests.memoryEfficiency.avgMemoryPerQuad.toFixed(2)} bytes`);
    console.log(`   Avg Backpressure Rate: ${(benchmarkResults.summary.tests.backpressure.avgBackpressureRate * 100).toFixed(2)}%\n`);

    // Save results
    saveBenchmarkResults(benchmarkResults, './benchmark-results.json');

    process.exit(0);
  } catch (error) {
    console.error('❌ Benchmark failed:', error.message);
    process.exit(1);
  }
} else {
  console.log('❌ Some tests failed. Fix issues before running benchmarks.\n');
  process.exit(1);
}

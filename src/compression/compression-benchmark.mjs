/**
 * @fileoverview Compression Benchmark Suite
 *
 * Benchmarks all compression algorithms on:
 * - Compression ratio
 * - Compression speed
 * - Decompression speed
 * - Memory usage
 *
 * Run: node src/compression/compression-benchmark.mjs
 */

import {
  compressLZ4,
  decompressLZ4,
  compressBrotli,
  decompressBrotli,
  compressDelta,
  decompressDelta,
  compressColumnar,
  decompressColumnar,
  compressSemantic,
  decompressSemantic,
  compressAdaptive,
  decompressAdaptive
} from './index.mjs';

// ============================================================================
// Test Data Generators
// ============================================================================

function generateTextData(size = 10000) {
  const words = ['the', 'quick', 'brown', 'fox', 'jumps', 'over', 'lazy', 'dog', 'and', 'the', 'cat', 'runs'];
  const result = [];
  for (let i = 0; i < size / 5; i++) {
    result.push(words[i % words.length]);
  }
  return result.join(' ');
}

function generateTemporalData(count = 1000) {
  const sequence = [];
  let value = { counter: 0, status: 'active', metadata: { id: 1, timestamp: Date.now() } };

  for (let i = 0; i < count; i++) {
    sequence.push({ ...value, counter: i, metadata: { ...value.metadata, id: Math.floor(i / 10) } });
  }

  return sequence;
}

function generateStructuredData(count = 1000) {
  const data = [];
  for (let i = 0; i < count; i++) {
    data.push({
      id: i,
      name: `Item_${i % 20}`,
      category: ['Electronics', 'Clothing', 'Food', 'Books'][i % 4],
      price: (i % 100) + 9.99,
      inStock: i % 3 !== 0,
      tags: ['tag1', 'tag2', 'tag3'].slice(0, (i % 3) + 1)
    });
  }
  return data;
}

function generateRDFQuads(count = 1000) {
  const data = [];
  for (let i = 0; i < count; i++) {
    data.push({
      subject: `http://example.org/subject/${i % 50}`,
      predicate: `http://example.org/predicate/${i % 10}`,
      object: `"value_${i}"`,
      graph: 'http://example.org/graph/default'
    });
  }
  return data;
}

// ============================================================================
// Benchmark Runner
// ============================================================================

class CompressionBenchmark {
  constructor() {
    this.results = [];
  }

  async benchmark(name, data, compressFn, decompressFn, iterations = 10) {
    const measurements = {
      name,
      dataSize: JSON.stringify(data).length,
      iterations,
      compressionTimes: [],
      decompressionTimes: [],
      compressionRatios: [],
      compressedSizes: []
    };

    for (let i = 0; i < iterations; i++) {
      // Compression
      const compressStart = performance.now();
      const compressed = await compressFn(data);
      const compressEnd = performance.now();

      measurements.compressionTimes.push(compressEnd - compressStart);
      measurements.compressionRatios.push(compressed.ratio);
      measurements.compressedSizes.push(compressed.compressedSize);

      // Decompression
      const decompressStart = performance.now();
      await decompressFn(compressed);
      const decompressEnd = performance.now();

      measurements.decompressionTimes.push(decompressEnd - decompressStart);
    }

    const stats = {
      name,
      dataType: typeof data === 'string' ? 'text' : (Array.isArray(data) ? 'array' : 'object'),
      originalSize: measurements.dataSize,
      avgCompressedSize: this._avg(measurements.compressedSizes),
      avgCompressionRatio: this._avg(measurements.compressionRatios),
      avgCompressionTime: this._avg(measurements.compressionTimes),
      avgDecompressionTime: this._avg(measurements.decompressionTimes),
      compressionThroughput: measurements.dataSize / this._avg(measurements.compressionTimes) / 1024, // KB/ms
      decompressionThroughput: measurements.dataSize / this._avg(measurements.decompressionTimes) / 1024,
      bytesSaved: measurements.dataSize - this._avg(measurements.compressedSizes),
      savingsPercent: ((measurements.dataSize - this._avg(measurements.compressedSizes)) / measurements.dataSize) * 100
    };

    this.results.push(stats);
    return stats;
  }

  _avg(arr) {
    return arr.reduce((a, b) => a + b, 0) / arr.length;
  }

  printResults() {
    console.log('\n============================================================');
    console.log('COMPRESSION BENCHMARK RESULTS');
    console.log('============================================================\n');

    for (const result of this.results) {
      console.log(`Algorithm: ${result.name}`);
      console.log(`  Data Type: ${result.dataType}`);
      console.log(`  Original Size: ${(result.originalSize / 1024).toFixed(2)} KB`);
      console.log(`  Compressed Size: ${(result.avgCompressedSize / 1024).toFixed(2)} KB`);
      console.log(`  Compression Ratio: ${result.avgCompressionRatio.toFixed(2)}x`);
      console.log(`  Savings: ${result.savingsPercent.toFixed(1)}% (${(result.bytesSaved / 1024).toFixed(2)} KB)`);
      console.log(`  Compression Time: ${result.avgCompressionTime.toFixed(3)} ms`);
      console.log(`  Decompression Time: ${result.avgDecompressionTime.toFixed(3)} ms`);
      console.log(`  Compression Throughput: ${result.compressionThroughput.toFixed(2)} KB/ms`);
      console.log(`  Decompression Throughput: ${result.decompressionThroughput.toFixed(2)} KB/ms`);
      console.log('');
    }

    this._printComparison();
  }

  _printComparison() {
    console.log('============================================================');
    console.log('ALGORITHM COMPARISON');
    console.log('============================================================\n');

    // Best compression ratio
    const bestRatio = this.results.reduce((best, current) =>
      current.avgCompressionRatio > best.avgCompressionRatio ? current : best
    );
    console.log(`Best Compression Ratio: ${bestRatio.name} (${bestRatio.avgCompressionRatio.toFixed(2)}x)`);

    // Fastest compression
    const fastest = this.results.reduce((best, current) =>
      current.avgCompressionTime < best.avgCompressionTime ? current : best
    );
    console.log(`Fastest Compression: ${fastest.name} (${fastest.avgCompressionTime.toFixed(3)} ms)`);

    // Fastest decompression
    const fastestDecomp = this.results.reduce((best, current) =>
      current.avgDecompressionTime < best.avgDecompressionTime ? current : best
    );
    console.log(`Fastest Decompression: ${fastestDecomp.name} (${fastestDecomp.avgDecompressionTime.toFixed(3)} ms)`);

    // Best throughput
    const bestThroughput = this.results.reduce((best, current) =>
      current.compressionThroughput > best.compressionThroughput ? current : best
    );
    console.log(`Best Throughput: ${bestThroughput.name} (${bestThroughput.compressionThroughput.toFixed(2)} KB/ms)`);

    console.log('');
  }

  exportJSON() {
    return {
      timestamp: new Date().toISOString(),
      results: this.results,
      summary: {
        totalAlgorithms: this.results.length,
        bestRatio: this.results.reduce((best, current) =>
          current.avgCompressionRatio > best.avgCompressionRatio ? current : best
        ).name,
        fastest: this.results.reduce((best, current) =>
          current.avgCompressionTime < best.avgCompressionTime ? current : best
        ).name
      }
    };
  }
}

// ============================================================================
// Run Benchmarks
// ============================================================================

async function runBenchmarks() {
  console.log('Starting compression benchmarks...\n');

  const benchmark = new CompressionBenchmark();

  // Text data benchmarks
  console.log('Benchmarking text data (10KB)...');
  const textData = generateTextData(10000);

  await benchmark.benchmark('LZ4 (Text)', textData, compressLZ4, decompressLZ4, 10);
  await benchmark.benchmark('Brotli (Text)', textData, compressBrotli, decompressBrotli, 10);

  // Temporal data benchmarks
  console.log('Benchmarking temporal data (1000 items)...');
  const temporalData = generateTemporalData(1000);

  await benchmark.benchmark('Delta (Temporal)', temporalData, compressDelta, decompressDelta, 10);
  await benchmark.benchmark('LZ4 (Temporal)', temporalData, compressLZ4, decompressLZ4, 10);

  // Structured data benchmarks
  console.log('Benchmarking structured data (1000 rows)...');
  const structuredData = generateStructuredData(1000);

  await benchmark.benchmark('Columnar (Structured)', structuredData, compressColumnar, decompressColumnar, 10);
  await benchmark.benchmark('Brotli (Structured)', structuredData, compressBrotli, decompressBrotli, 10);

  // RDF quads benchmarks
  console.log('Benchmarking RDF quads (1000 quads)...');
  const rdfData = generateRDFQuads(1000);

  await benchmark.benchmark('Columnar (RDF)', rdfData, compressColumnar, decompressColumnar, 10);
  await benchmark.benchmark('Semantic (RDF)', rdfData, compressSemantic, decompressSemantic, 10);

  // Adaptive benchmarks
  console.log('Benchmarking adaptive selection...');
  await benchmark.benchmark('Adaptive (Text)', textData, compressAdaptive, decompressAdaptive, 10);
  await benchmark.benchmark('Adaptive (Structured)', structuredData, compressAdaptive, decompressAdaptive, 10);

  // Print results
  benchmark.printResults();

  // Export JSON
  const json = benchmark.exportJSON();
  console.log('============================================================');
  console.log('Benchmark results exported to JSON format');
  console.log('============================================================\n');

  return json;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runBenchmarks()
    .then((results) => {
      console.log('Benchmarks completed successfully!');
      process.exit(0);
    })
    .catch((error) => {
      console.error('Benchmark failed:', error);
      process.exit(1);
    });
}

export { runBenchmarks, CompressionBenchmark };
export default runBenchmarks;
